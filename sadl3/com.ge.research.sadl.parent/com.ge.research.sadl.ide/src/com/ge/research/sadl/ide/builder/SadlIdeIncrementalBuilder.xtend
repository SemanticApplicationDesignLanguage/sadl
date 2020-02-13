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
package com.ge.research.sadl.ide.builder

import com.ge.research.sadl.utils.SadlProjectHelper
import com.google.inject.Inject
import java.net.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.build.BuildContext
import org.eclipse.xtext.build.BuildRequest
import org.eclipse.xtext.build.IncrementalBuilder
import org.eclipse.xtext.build.Source2GeneratedMapping
import org.eclipse.xtext.generator.GeneratorContext
import org.eclipse.xtext.generator.GeneratorDelegate
import org.eclipse.xtext.generator.IContextualOutputConfigurationProvider
import org.eclipse.xtext.generator.IFilePostProcessor
import org.eclipse.xtext.generator.URIBasedFileSystemAccess
import org.eclipse.xtext.generator.trace.TraceFileNameProvider
import org.eclipse.xtext.generator.trace.TraceRegionSerializer
import org.eclipse.xtext.parser.IEncodingProvider
import org.eclipse.xtext.resource.IResourceServiceProvider
import org.eclipse.xtext.resource.persistence.StorageAwareResource
import org.eclipse.xtext.workspace.IProjectConfigProvider

/**
 * Customized incremental builder that does two things differently:
 * <ul>
 * <li>Does not delete the generated {@code SadlBaseModel.}[{@code owl|n3|nt}] file from the {@code OwlModels} folder.</li>
 * <li>Dynamically calculates the based directory URI per resource, so that the generation location can be dynamic too.</li> 
 * </ul>
 */
class SadlIdeIncrementalBuilder extends IncrementalBuilder {

	static class SadlInternalStatefulIncrementalBuilder extends InternalStatefulIncrementalBuilder {

		@Inject
		SadlProjectHelper projectHelper;

		override createFileSystemAccess(IResourceServiceProvider serviceProvider, Resource resource) {
			if (resource.URI.file) {
				val root = projectHelper.getRoot(new URI(resource.URI.toString));
				if (root !== null) {
					val projectConfigProvider = serviceProvider.get(IProjectConfigProvider)
					val projectConfig = projectConfigProvider?.getProjectConfig(resource.resourceSet)
					val sourceFolder = projectConfig?.findSourceFolderContaining(resource.getURI)
					return new URIBasedFileSystemAccess() => [
						val outputConfigProvider = serviceProvider.get(IContextualOutputConfigurationProvider)
						outputConfigurations = outputConfigProvider.getOutputConfigurations(resource).toMap[name]

						postProcessor = serviceProvider.get(IFilePostProcessor)
						val newEncodingProvider = serviceProvider.get(IEncodingProvider)
						if (newEncodingProvider !== null)
							encodingProvider = newEncodingProvider
						traceFileNameProvider = serviceProvider.get(TraceFileNameProvider)
						traceRegionSerializer = serviceProvider.get(TraceRegionSerializer)
						generateTraces = true

						baseDir = org.eclipse.emf.common.util.URI.createURI(root.toString);
						currentSource = sourceFolder?.name
						converter = resource.resourceSet.getURIConverter
					]
				}
			}
			return super.createFileSystemAccess(serviceProvider, resource);
		}

		override void generate(Resource resource, BuildRequest request, Source2GeneratedMapping newMappings) {
			val serviceProvider = context.getResourceServiceProvider(resource.URI)
			val generator = serviceProvider.get(GeneratorDelegate)
			if (generator === null) {
				return;
			}
			val previous = newMappings.deleteSource(resource.getURI)
			val fileSystemAccess = createFileSystemAccess(serviceProvider, resource) => [
				beforeWrite = [ uri, outputCfgName, contents |
					newMappings.addSource2Generated(resource.getURI, uri, outputCfgName)
					previous.remove(uri)
					request.afterGenerateFile.apply(resource.getURI, uri)
					return contents
				]
				beforeDelete = [ uri |
					newMappings.deleteGenerated(uri)
					request.afterDeleteFile.apply(uri)
					return true
				]
			]
			fileSystemAccess.context = resource
			if (request.isWriteStorageResources) {
				switch resource {
					StorageAwareResource case resource.resourceStorageFacade !== null: {
						resource.resourceStorageFacade.saveResource(resource, fileSystemAccess)
					}
				}
			}
			val generatorContext = new GeneratorContext
			generatorContext.cancelIndicator = request.cancelIndicator
			generator.generate(resource, fileSystemAccess, generatorContext)
			// delete everything that was previously generated, but not this time
			previous.filter [ uri |
				val uriStr = uri.toString;
				return !(uriStr.endsWith('OwlModels/SadlBaseModel.owl') ||
					uriStr.endsWith('OwlModels/SadlBaseModel.nt') || uriStr.endsWith('OwlModels/SadlBaseModel.n3'))
			].forEach [
				context.resourceSet.getURIConverter.delete(it, emptyMap)
				request.getAfterDeleteFile.apply(it)
			]
		}

	}

}
