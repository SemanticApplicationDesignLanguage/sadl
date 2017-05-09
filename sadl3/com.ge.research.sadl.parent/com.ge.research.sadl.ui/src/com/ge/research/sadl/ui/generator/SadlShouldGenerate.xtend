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
package com.ge.research.sadl.ui.generator

import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.Path
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.generator.IShouldGenerate
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.workspace.ProjectConfigAdapter

import static com.ge.research.sadl.markers.SadlMarkerConstants.*
import static org.eclipse.core.resources.IMarker.*
import static org.eclipse.core.resources.IResource.*

/**
 * Shamelessly copied from {@code EclipseBasedShouldGenerate}.
 * 
 * This implementation allows generation even if the Eclipse has attached external 
 * SADL markers.
 * 
 * @author akos.kitta
 */
class SadlShouldGenerate implements IShouldGenerate {

	@Override
	override shouldGenerate(Resource resource, CancelIndicator cancelIndicator) {
		val uri = resource.URI;
		if (uri === null || !uri.isPlatformResource) {
			return false
		}

		val member = ResourcesPlugin.workspace.root.findMember(new Path(uri.toPlatformString(true)));
		if (member !== null && member.type === FILE) {
			val projectConfig = ProjectConfigAdapter.findInEmfObject(resource.resourceSet)?.projectConfig;
			if (member.project.name == projectConfig?.name) {
				return member.hasErrors();
			}
		}

		return false
	}

	def boolean hasErrors(IResource resource) {
		val noErrors = resource.findMaxProblemSeverity(null, true, DEPTH_INFINITE) !== SEVERITY_ERROR;
		if (noErrors) {
			return false;
		} else {
			return resource.findMarkers(PROBLEM, true, DEPTH_INFINITE).exists[type !== SADL_PROBLEM_MARKER];
		}
	}

}
