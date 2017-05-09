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

import com.ge.research.sadl.markers.SadlMarker
import com.ge.research.sadl.markers.SadlMarkerConstants
import com.ge.research.sadl.markers.SadlMarkerDeserializerService
import com.ge.research.sadl.markers.SadlMarkerLocationProvider
import com.ge.research.sadl.markers.SadlMarkerLocationProvider.Location
import com.ge.research.sadl.markers.SadlMarkerSeverityMapper
import com.google.inject.Inject
import java.nio.file.Paths
import java.util.Collection
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IMarker
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.resources.WorkspaceJob
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Status
import org.eclipse.core.runtime.SubMonitor
import org.eclipse.emf.common.util.URI
import org.eclipse.ui.IStartup
import org.eclipse.xtext.ui.resource.IResourceSetProvider

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
	SadlMarkerLocationProvider locationProvider;

	@Inject
	SadlMarkerSeverityMapper severityMapper;

	@Override
	override earlyStartup() {
		val ws = ResourcesPlugin.workspace;
		ws.addResourceChangeListener([ event |
			val Collection<()=>void> modifications = newArrayList();
			event?.delta.accept([
				if (resource instanceof IFile && resource.fileExtension == SadlMarkerConstants.FILE_EXTENSION) {
					val markers = deserializerService.deserialize(Paths.get(resource.locationURI));
					val project = resource.project;
					if (project.accessible) {
						markers.groupBy[filePath].forEach [ filePath, entries |
							val uri = URI.createPlatformResourceURI('''«project.name»/«filePath»''', true);
							if (uri.platformResource) {
								val path = uri.toPlatformString(true);
								val member = ws.root.findMember(path);
								if (member !== null && member.accessible) {
									val projectLocation = Paths.get(project.locationURI);
									val resourceSet = resourceSetProvider.get(project);
									modifications.add([member.deleteExistingMarkersWithOrigin(entries.map[origin])]);
									entries.forEach [ entry |
										val location = locationProvider.getLocation(resourceSet, entry,
											projectLocation);
										modifications.add([member.createMarker(entry, location)]);
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

					@Override
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

	def void deleteExistingMarkersWithOrigin(IResource resource, Collection<String> origins) {
		resource.findMarkers(SadlMarkerConstants.SADL_PROBLEM_MARKER, true, IResource.DEPTH_INFINITE).filter [
			origins.contains(getAttribute(SadlMarkerConstants.ORIGIN_KEY));
		].forEach [
			delete();
		];
	}

	private def void createMarker(IResource resource, SadlMarker marker, Location location) {
		resource.createMarker(SadlMarkerConstants.SADL_PROBLEM_MARKER).setAttributes(resource, marker, location);
	}

	private def setAttributes(IMarker it, IResource resource, SadlMarker marker, Location location) {
		setAttribute(IMarker.MESSAGE, marker.message);
		setAttribute(IMarker.SEVERITY, severityMapper.map(marker.severity));
		setAttribute(SadlMarkerConstants.ORIGIN_KEY, marker.origin);
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
