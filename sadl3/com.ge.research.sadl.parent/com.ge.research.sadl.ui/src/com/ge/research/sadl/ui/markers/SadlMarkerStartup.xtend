/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/
package com.ge.research.sadl.ui.markers

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory
import com.ge.research.sadl.markers.SadlMarker
import com.ge.research.sadl.markers.SadlMarkerDeserializerService
import com.ge.research.sadl.markers.SadlMarkerLocationProvider
import com.ge.research.sadl.markers.SadlMarkerLocationProvider.Location
import com.ge.research.sadl.markers.SadlMarkerRefType
import com.ge.research.sadl.markers.SadlMarkerSeverityMapper
import com.ge.research.sadl.model.SadlSerializationFormat
import com.ge.research.sadl.reasoner.utils.SadlUtils
import com.ge.research.sadl.sADL.SadlModel
import com.google.common.collect.Iterables
import com.google.inject.Inject
import java.io.File
import java.nio.file.Paths
import java.util.Collection
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IMarker
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.resources.WorkspaceJob
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Status
import org.eclipse.core.runtime.SubMonitor
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.ui.IStartup
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.resource.IResourceSetProvider
import org.eclipse.xtext.util.Strings
import org.eclipse.xtext.validation.Issue

import static com.ge.research.sadl.jena.UtilsForJena.*
import static com.ge.research.sadl.markers.SadlMarkerConstants.*

/**
 * Contribution that registers a resource change listener for tracking all the {@code .err} 
 * file changes.
 * 
 * @author akos.kitta
 */
class SadlMarkerStartup implements IStartup {

	@Inject
	IResourceSetProvider resourceSetProvider;

	@Inject
	SadlMarkerDeserializerService deserializerService;

	@Inject
	SadlMarkerSeverityMapper severityMapper;

	override earlyStartup() {
		val ws = ResourcesPlugin.workspace;
		ws.addResourceChangeListener([ event |
			val Collection<()=>void> modifications = newArrayList();
			event?.delta?.accept([
				if (resource instanceof IFile && resource.fileExtension == FILE_EXTENSION) {
					val markerInfos = deserializerService.deserialize(Paths.get(resource.locationURI));
					val origin = markerInfos.origin;
					val project = resource.project;
					if (project.accessible) {
						modifications.add([project.deleteExistingMarkersWithOrigin(origin)]);
						markerInfos.map[mapModelUri].map[mapRefs(project)].groupBy[modelUri].forEach [ modelUri, entries |
							val resource = modelUri.getResource(project);
							if (resource !== null) {
								val locationProvider = resource.locationProvider;
								val member = ws.root.findMember(resource.URI.toPlatformString(true));
								if (member !== null && member.accessible) {
									entries.forEach [ marker |
										val segments = marker.astNodeName.split('/');
										val name = if(segments.length > 0) segments.last else marker.astNodeName;
										val location = locationProvider.getLocation(name, resource);
										modifications.add([member.createMarker(marker, location, origin)]);
									];
								}
							}
						];
					}
					return false; // No need to visit any delta children.
				}
				return true; // Visit child resources (if any).
			]);
			if (!modifications.empty) {
				new WorkspaceJob("External SADL marker job") {

					override runInWorkspace(IProgressMonitor monitor) throws CoreException {
						monitor.beginTask("Updating external SADL markers...", modifications.size);
						val subMonitor = SubMonitor.convert(monitor, modifications.size);
						modifications.forEach [
							it.apply();
							subMonitor.worked(1);
						];
						monitor.done();
						return Status.OK_STATUS;
					}

				}.schedule;
			}
		]);
	}

	protected def mapModelUri(SadlMarker it) {
		// If the model URI is set, then it returns with the identical instance.
		if (modelUriSet) {
			return it;
		}
		// AATIM-2050 Get rid of the translator dependency and truncate the fully qualified URI to given from RAE to short name
		val newModelUri = if (astNodeName === null || !astNodeName.contains("/")) {
				null;
			} else {
				astNodeName.substring(0, (astNodeName.lastIndexOf("/")));
			};
		return SadlMarker.copyWithModelUri(it, newModelUri);
	}

	protected def mapRefs(SadlMarker it, IProject project) {
		val itr = references.iterator;
		while (itr.hasNext) {
			val ref = itr.next;
			if (!ref.resolved) {
				if (ref.type === SadlMarkerRefType.File) {
					val projectPath = Paths.get(project.locationURI);
					val file = project.getFile(ref.referencedId);
					if (file.accessible) {
						val refPath = Paths.get(project.getFile(ref.referencedId).locationURI);
						val projectRelativePath = '''«project.name»«IPath.SEPARATOR»«projectPath.relativize(refPath)»''';
						resolveRef(ref, projectRelativePath);
					}
				} else if (ref.type === SadlMarkerRefType.ModelElement) {
					val lastIndexOfSlash = ref.referencedId.lastIndexOf('/');
					val modelUri = if (lastIndexOfSlash != 0) ref.referencedId.substring(0,lastIndexOfSlash) else null;
					val astNodeName = if (lastIndexOfSlash != 0) ref.referencedId.substring(lastIndexOfSlash + 1) else null;
					val resource = modelUri?.getResource(project);
					if (resource !== null) {
						val astNode = if (astNodeName === null) {
							resource.sadlModel;
						} else  {
							val object = resource.locationProvider.getEObjectByName(astNodeName, resource);
							if (object === null) resource.sadlModel else object;
						}
						val astNodeUri = EcoreUtil.getURI(astNode);
						val model = resource.sadlModel;
						val resourceName = '''«IF model.alias.nullOrEmpty»«model.baseUri»«ELSE»«model.alias»«ENDIF»''';
						val resolvedRefId = '''«IF !astNodeName.nullOrEmpty»«astNodeName»«ENDIF»«SADL_REFS_SEPARATOR»«resourceName»«SADL_REFS_SEPARATOR»«astNodeUri»''';
						resolveRef(ref, resolvedRefId);
					}
				} else {
					throw new IllegalStateException('''Unexpected SADL marker reference type: «ref.type»''');
				}
			}
		}
		return it;
	}

	private def getSadlModel(Resource it) {
		return contents.head as SadlModel;
	}
	
	private def getResource(String modelUri, IProject project) {
		val resourceUri = modelUri.getResourceUri(project);
		if (resourceUri === null) {
			return null;
		}
		return resourceUri.getResource(project);
	} 
	
	private def getResource(URI resourceUri, IProject project) {
		return resourceSetProvider.get(project).getResource(resourceUri, true);
	}

	private def getLocationProvider(Resource resource) {
		return (resource as XtextResource).resourceServiceProvider.get(SadlMarkerLocationProvider);
	}

	private def getConfigurationManager(IProject it) {
		val modelFolder = '''«Paths.get(locationURI).resolve(OWL_MODELS_FOLDER_NAME)»''';
		return ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolder, SadlSerializationFormat.RDF_XML_FORMAT);
	}

	private def getResourceUri(String modelUri, IProject project) {
		val configurationManager = project.configurationManager;
		val owlFilePath = configurationManager.mappings.get(modelUri);
		if (owlFilePath === null) {
			return null;
		}
		val fileName = new SadlUtils().fileUrlToFileName(owlFilePath);
		val file = new File(fileName);
		if (!file.exists()) {
			return null;
		}
		val normalizedFilePath = file.toPath.fileName.toString;
		val owlFileName = Paths.get(normalizedFilePath).toFile.name;
		var resourceName = owlFileName.substring(0, owlFileName.lastIndexOf("."));
		if (resourceName.indexOf('.') < 0) {
			resourceName = resourceName + ".sadl";
		}
		val rn = resourceName;
		val resourceUri = <URI>newArrayList();
		project.accept([
			if (it instanceof IFile) {
				if (name == rn) {
					resourceUri.add(URI.createPlatformResourceURI('''«fullPath»''', true));
				}
				return false;
			} else {
				return true;
			};
		], IResource.DEPTH_INFINITE, false);
		return resourceUri.head;
	}

	private def deleteExistingMarkersWithOrigin(IProject project, String origin) {
		val markersToDelete = <IMarker>newArrayList();
		project.accept([
			markersToDelete += findMarkers(SADL_PROBLEM_MARKER, true, IResource.DEPTH_INFINITE).filter [
				origin == getAttribute(ORIGIN_KEY);
			]
		], IResource.DEPTH_INFINITE, false);
		markersToDelete.forEach[delete];
	}

	private def void createMarker(IResource resource, SadlMarker marker, Location location, String origin) {
		resource.createMarker(SADL_PROBLEM_MARKER).setAttributes(resource, marker, location, origin);
	}

	private def setAttributes(IMarker it, IResource resource, SadlMarker marker, Location location, String origin) {
		setAttribute(IMarker.MESSAGE, marker.message);
		setAttribute(IMarker.SEVERITY, severityMapper.map(marker.severity));
		setAttribute(ORIGIN_KEY, origin);
		if (!marker.references.nullOrEmpty) {
			// That has to be attached to the marker to have the quick fixes enabled.
			setAttribute(Issue.CODE_KEY, SADL_REFS);
			val refs = Iterables.toArray(marker.references.filter[resolved].map['''«type»«SADL_REFS_SEPARATOR»«referencedId»'''],
				String);
			setAttribute(Issue.DATA_KEY, Strings.pack(refs));
		}
		return setLocation(location);
	}

	private def setLocation(IMarker it, Location location) {
		val locationStr = if(location == Location.UNKNOWN) "unknown location" else '''line: «location.lineNumber»''';
		setAttribute(IMarker.LOCATION, locationStr);
		setAttribute(IMarker.CHAR_START, location.start);
		setAttribute(IMarker.CHAR_END, location.end);
		setAttribute(IMarker.LINE_NUMBER, location.lineNumber);
		return it;
	}

}
