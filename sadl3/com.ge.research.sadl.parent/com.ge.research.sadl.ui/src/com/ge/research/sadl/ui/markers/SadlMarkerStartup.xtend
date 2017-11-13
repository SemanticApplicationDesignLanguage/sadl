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
import com.ge.research.sadl.markers.SadlMarkerConstants
import com.ge.research.sadl.markers.SadlMarkerDeserializerService
import com.ge.research.sadl.markers.SadlMarkerLocationProvider
import com.ge.research.sadl.markers.SadlMarkerLocationProvider.Location
import com.ge.research.sadl.markers.SadlMarkerSeverityMapper
import com.ge.research.sadl.reasoner.utils.SadlUtils
import com.google.common.base.Suppliers
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
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Status
import org.eclipse.core.runtime.SubMonitor
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.ui.IStartup
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.resource.IResourceSetProvider

import static com.ge.research.sadl.jena.UtilsForJena.*
import static com.ge.research.sadl.reasoner.IConfigurationManager.*

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
				if (resource instanceof IFile && resource.fileExtension == SadlMarkerConstants.FILE_EXTENSION) {
					val markerInfos = deserializerService.deserialize(Paths.get(resource.locationURI));
					val origin = markerInfos.origin;
					val project = resource.project;
					if (project.accessible) {
						modifications.add([project.deleteExistingMarkersWithOrigin(origin)]);
						val translator = Suppliers.memoize([getConfigurationManager(project).translator]);
						markerInfos.map[if (isModelUriSet) {
							// If the model URI is set, then it returns with the identical instance.
							it 
						} else {
							// Otherwise let's create a new copy of the original marker with the model URI from the translator.
							//AATIM-2050 Get rid of the translator dependency and truncate the fully qualified URI to given from RAE to short name
							var nodeName = astNodeName
							if(nodeName === null || !nodeName.contains("/")){
								SadlMarker.copyWithModelUri(it, null);
							}else{
								SadlMarker.copyWithModelUri(it, astNodeName.substring(0, (astNodeName.lastIndexOf("/"))));
							}						
						}].groupBy[modelUri].forEach [ modelUri, entries |
							val resourceUri = modelUri.getResourceUri(project);
							if (resourceUri !== null) {
								val resource = resourceSetProvider.get(project).getResource(resourceUri, true);
								val locationProvider = resource.locationProvider;
								val member = ws.root.findMember(resourceUri.toPlatformString(true));
								if (member !== null && member.accessible) {
									val projectLocation = Paths.get(project.locationURI);
									entries.forEach [ marker |
										val location = locationProvider.getLocation(marker, resource, projectLocation);
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

				}.schedule
			}
		]);
	}

	private def getLocationProvider(Resource resource) {
		return (resource as XtextResource).resourceServiceProvider.get(SadlMarkerLocationProvider);
	}

	private def getConfigurationManager(IProject it) {
		val modelFolder = '''«Paths.get(locationURI).resolve(OWL_MODELS_FOLDER_NAME)»''';
		return ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolder, RDF_XML_FORMAT);
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
		val owlFileName = Paths.get(normalizedFilePath).toFile.name
		var resourceName = owlFileName.substring(0, owlFileName.lastIndexOf("."));
		if (resourceName.indexOf('.') < 0) {
			resourceName = resourceName + ".sadl"
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
			}
		], IResource.DEPTH_INFINITE, false);
		return resourceUri.head;
	}

	private def deleteExistingMarkersWithOrigin(IProject project, String origin) {
		val markersToDelete = <IMarker>newArrayList();
		project.accept([
			markersToDelete +=
				findMarkers(SadlMarkerConstants.SADL_PROBLEM_MARKER, true, IResource.DEPTH_INFINITE).filter [
					origin == getAttribute(SadlMarkerConstants.ORIGIN_KEY);
				]
		], IResource.DEPTH_INFINITE, false);
		markersToDelete.forEach[delete];
	}

	private def void createMarker(IResource resource, SadlMarker marker, Location location, String origin) {
		resource.createMarker(SadlMarkerConstants.SADL_PROBLEM_MARKER).setAttributes(resource, marker, location,
			origin);
	}

	private def setAttributes(IMarker it, IResource resource, SadlMarker marker, Location location, String origin) {
		setAttribute(IMarker.MESSAGE, marker.message);
		setAttribute(IMarker.SEVERITY, severityMapper.map(marker.severity));
		setAttribute(SadlMarkerConstants.ORIGIN_KEY, origin);
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
