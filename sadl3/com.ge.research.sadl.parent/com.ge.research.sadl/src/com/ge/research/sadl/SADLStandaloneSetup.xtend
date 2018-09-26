/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl

import com.ge.research.sadl.external.ExternalEmfResourceFactory
import com.ge.research.sadl.external.ExternalEmfResourceServiceProvider
import com.google.inject.Injector
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.Resource.Factory
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.resource.IResourceServiceProvider

/**
 * Initialization support for running Xtext languages without Equinox extension registry.
 */
class SADLStandaloneSetup extends SADLStandaloneSetupGenerated {

	def static void doSetup() {
		new SADLStandaloneSetup().createInjectorAndDoEMFRegistration()
	}

	override register(Injector injector) {
		super.register(injector)
		injector.createExternalResourceHelper.registerExternalEmfResourceSupport;
	}
	
	override createInjectorAndDoEMFRegistration() {
		return super.createInjectorAndDoEMFRegistration()
	}

	protected def ExternalEmfResourceHelper createExternalResourceHelper(Injector injector) {
		return new ExternalEmfResourceHelper(injector);
	}

	@Data
	protected static class ExternalEmfResourceHelper {

		@Accessors(PROTECTED_GETTER)
		val Injector injector;

		protected def registerExternalEmfResourceSupport() {
			val resourceFactory = resourceFactory;
			val serviceProvider = resourceServiceProvider;
			ExternalEmfResourceFactory.EXTERNAL_EXTENSIONS.forEach [ ext |
				Resource.Factory.Registry.INSTANCE.getExtensionToFactoryMap().put(ext, resourceFactory);
				IResourceServiceProvider.Registry.INSTANCE.getExtensionToFactoryMap().put(ext, serviceProvider);
			];
		}

		protected def Factory getResourceFactory() {
			return injector.getInstance(ExternalEmfResourceFactory);
		}

		protected def IResourceServiceProvider getResourceServiceProvider() {
			return injector.getInstance(ExternalEmfResourceServiceProvider);
		}

	}

}