/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.utils

import com.ge.research.sadl.utils.SadlProjectHelper
import com.google.common.base.Preconditions
import java.net.URI
import java.nio.file.Path
import java.nio.file.Paths
import org.eclipse.core.resources.ResourcesPlugin

/**
 * Project helper for the Eclipse case.
 * 
 * @author akos.kitta
 */
class EclipseSadlProjectHelper implements SadlProjectHelper {
	
	override isRoot(URI uri) {
		uri.checkUri;
		val emfUri = uri.toEmfUri;
		if (emfUri.segmentCount === 2) {
			val project = emfUri.project;
			if (project !== null) {
				return true;
			}
		}
		return false;
	}

	override getRoot(URI nested) {
		nested.checkUri;
		val emfUri = nested.toEmfUri;
		if (nested.isRoot) {
			return getProject(emfUri).locationURI;
		}
		if (emfUri.segmentCount < 3) {
			return null;
		}
		return getRoot(new URI(emfUri.trimSegments(1).toString));
	}

	override toUri(Path path) {
		val project = ResourcesPlugin.workspace.root.projects.map[location.toFile.toPath -> it].findFirst[path.startsWith(key)];
		if (project === null) {
			throw new RuntimeException('''Cannot convert path to URI. Container project does not exist for path: «path».''')
		}
		if (!project.value.accessible) {
			throw new RuntimeException('''Cannot convert path to URI. Container project is not accessible: «path».''')
		}
		val file = project.value.findMember(project.key.relativize(path).toString);
		if (file === null || !file.accessible) {
			throw new RuntimeException('''Cannot convert path to URI. File does no exist under: «path».''')
		}
		return new URI(org.eclipse.emf.common.util.URI.createPlatformResourceURI(file.fullPath.toString, true).toString);
	}

	override toPath(URI uri) {
		uri.checkUri;
		val path = org.eclipse.emf.common.util.URI.createURI(uri.toString(), false).toPlatformString(false);
		val resource = ResourcesPlugin.workspace.root.findMember(path);
		if (!resource.exists) {
			throw new IllegalArgumentException('''The resource does not exits for the «uri» URI. Converted path was: «path».''');
		}
		return Paths.get(resource.locationURI);
	}

	protected def checkUri(URI uri) {
		Preconditions.checkNotNull(uri, 'uri');
		if (!uri.scheme.equals("synthetic")) {
			Preconditions.checkArgument(uri.toEmfUri.platformResource, '''Expected a `platform:/resource/` URI. Got instead: «uri».''')
		}
		return uri;
	}

	protected def getProject(org.eclipse.emf.common.util.URI platformResourceUri) {
		Preconditions.checkArgument(platformResourceUri.segmentCount === 2, '''Expected a `platform:/resource/ URI with two segments. URI: «platformResourceUri».''');
		val root = ResourcesPlugin.workspace.root;
		val project = root.getProject(platformResourceUri.lastSegment);
		if (project.accessible) {
			return project;
		}
		return null;
	}

	protected def toEmfUri(URI uri) {
		return org.eclipse.emf.common.util.URI.createURI(uri.toString);
	}

}
