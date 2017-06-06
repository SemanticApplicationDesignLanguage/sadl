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
package com.ge.research.sadl.ui.external

import com.ge.research.sadl.external.ExternalEmfResourceGenerator
import com.ge.research.sadl.external.ExternalEmfResourceServiceProvider
import com.google.inject.Inject
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.OperationCanceledException
import org.eclipse.xtend.lib.annotations.Delegate
import org.eclipse.xtext.builder.BuilderParticipant
import org.eclipse.xtext.builder.EclipseResourceFileSystemAccess2
import org.eclipse.xtext.builder.MonitorBasedCancelIndicator
import org.eclipse.xtext.generator.GeneratorContext
import org.eclipse.xtext.resource.IResourceDescription.Delta
import org.eclipse.xtext.resource.IResourceServiceProvider

/**
 * Builder participant that will take care of the of the external SADL resources.
 * 
 * @author akos.kitta
 */
class ExternalEmfBuilderParticipant extends BuilderParticipant implements IResourceServiceProvider {

	@Inject
	@Delegate
	ExternalEmfResourceServiceProvider delegate;

	@Inject
	ExternalEmfResourceGenerator generator;

	override getResourceServiceProvider() {
		return this;
	}

	override protected getRelevantDeltas(IBuildContext context) {
		return context.deltas.filter[canHandle(uri) && context.isSourceLevelURI(uri)].toList;
	}

	override handleChangedContents(Delta delta, IBuildContext context,
		EclipseResourceFileSystemAccess2 fileSystemAccess) throws CoreException {

		if (!getResourceServiceProvider().canHandle(delta.getUri())) {
			return;
		}
		val resource = context.getResourceSet().getResource(delta.getUri(), true);
		registerCurrentSourceFolder(context, delta, fileSystemAccess);
		saveResourceStorage(resource, fileSystemAccess);
		if (shouldGenerate(resource, context)) {
			try {
				val cancelIndicator = new MonitorBasedCancelIndicator(new NullProgressMonitor());
				val generatorContext = new GeneratorContext();
				generatorContext.setCancelIndicator(cancelIndicator);
				generator.generate(resource, fileSystemAccess, generatorContext);
			} catch (OperationCanceledException e) {
				// don't look into the cause for OCE
				throw e;
			} catch (RuntimeException e) {
				if (e.getCause() instanceof CoreException) {
					throw e.getCause() as CoreException;
				}
				throw e;
			}
		}
	}

}
