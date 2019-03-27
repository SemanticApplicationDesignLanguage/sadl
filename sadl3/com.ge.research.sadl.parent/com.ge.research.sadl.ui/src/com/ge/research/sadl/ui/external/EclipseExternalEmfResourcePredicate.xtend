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
package com.ge.research.sadl.ui.external

import com.ge.research.sadl.external.ExternalEmfResourcePredicate
import com.google.common.base.Preconditions
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.emf.common.util.URI
import com.google.inject.Singleton

/**
 * External EMF resource that uses the Eclipse workspace.
 * 
 * @author akos.kitta
 */
@Singleton
class EclipseExternalEmfResourcePredicate extends ExternalEmfResourcePredicate.Default {

	override apply(URI it) {
		Preconditions.checkArgument(platformResource, '''Implementation error. Expected a `file` URI. Was: «it»''');
		if (!super.apply(it)) {
			return false;
		}
		val file = ResourcesPlugin.workspace.root.findMember(toPlatformString(true));
		if (file === null || !file.accessible) {
			return false;
		}
		val project = file.project;
		if (!project.accessible) {
			return false;
		}
		val externalDefinitions = newArrayList;
		project.accept([
			if (it instanceof IFile) {
				if (name.endsWith(EXTERNAL_EXTENSION)) {
					externalDefinitions.add(it.location.removeFileExtension)
				}
				return false;
			} else {
				return true;
			};
		], IResource.DEPTH_INFINITE, false);
		return externalDefinitions.exists[folderPath | file.location.toString.startsWith(folderPath.toString)];
	}

}
