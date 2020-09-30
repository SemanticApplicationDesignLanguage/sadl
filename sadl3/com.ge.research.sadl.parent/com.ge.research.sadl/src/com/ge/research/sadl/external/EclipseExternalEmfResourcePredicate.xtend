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
package com.ge.research.sadl.external

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory
import com.ge.research.sadl.reasoner.utils.SadlUtils
import com.google.common.base.Preconditions
import com.google.inject.Singleton
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.emf.common.util.URI

/**
 * External EMF resource that uses the Eclipse workspace.
 * 
 * @author akos.kitta
 */
@Singleton
class EclipseExternalEmfResourcePredicate extends ExternalEmfResourcePredicate.Default {
	
	/**
	 * TODO: this should not be here. Move this to the downstream grammar.
	 */
	static val EXTRACTED_MODELS = 'ExtractedModels';
	static val CG_MODELS = 'ComputationalGraphModels';

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
		
		// This is a hack here.
		// it considers all {@code .owl|nt|n3} files inside the {@code YOUR_PROJECT/ExtractedModels} folder as an external EMF resource.
		val extractedModelFolder = project.findMember(EXTRACTED_MODELS);
		if (extractedModelFolder !== null && extractedModelFolder.accessible) {
			// The current file is an external file and is inside the `ExtractedModels` folder.
			if (extractedModelFolder.getFullPath().isPrefixOf(file.getFullPath())) {
				return true;
			}
		}
		val cgModelsFolder = project.findMember(EclipseExternalEmfResourcePredicate.CG_MODELS);
		if (cgModelsFolder !== null && cgModelsFolder.accessible) {
			// The current file is an external file and is inside the `ExtractedModels` folder.
			if (cgModelsFolder.getFullPath().isPrefixOf(file.getFullPath())) {
				return true;
			}
		}
		
		if (segments.size < 3 || 
			(!segments.get(2).equals("OwlModels") &&
				!segments.get(2).equals(EXTRACTED_MODELS) &&
				!segments.get(2).equals(EclipseExternalEmfResourcePredicate.CG_MODELS))) {
			// this isn't something in the OwlModels folder or one of the exempted folders so apply
			return true;
//			val prjuri = project.locationURI
//			if (prjuri !== null) {
//				val prjpath = prjuri.path
//				val owlmodelfolder = prjpath + "/OwlModels"
//				val cmgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(owlmodelfolder, null);
//				if (cmgr !== null) {
//					var cntr = 0;
//					var sb = new StringBuilder()
//					for (seg : segments) {
//						if (cntr > 1) {
//							sb.append("/")
//							sb.append(seg)
//						}
//						cntr++
//					}
//					val su = new SadlUtils()
//					val fnuri = su.fileNameToFileUrl(prjpath + "/" + sb.toString)
//					try {
//						// if we succeed in getting a public URI then there is a mapping and we should import the OWL file
//						val puri = cmgr.getPublicUriFromActualUrl(fnuri)
////						println("Index non-SADL-generated OWL model '" + puri + "' located at '" + fnuri + "' returning true");
//						return true;
//					}
//					catch (Throwable t) {
//						// this just means that there's no mapping provided so don't process the file.
//						// well, it isn't in the OwlModels folder so why not process?
//						return true;
//					}
//				}
//			}
		}
		
//		val externalDefinitions = newArrayList;
//		project.accept([
//			if (it instanceof IFile) {
//				if (name.endsWith(EXTERNAL_EXTENSION)) {
//					externalDefinitions.add(it.location.removeFileExtension)
//				}
//				return false;
//			} else {
//				return true;
//			};
//		], IResource.DEPTH_INFINITE, false);
//		return externalDefinitions.exists[folderPath | file.location.toString.startsWith(folderPath.toString)];
		return false;
	}

}
