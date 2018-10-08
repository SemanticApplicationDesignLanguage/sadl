/************************************************************************
 * Copyright 2007-2018 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.tests.lsp

import com.ge.research.sadl.SADLRuntimeModule
import com.ge.research.sadl.SADLStandaloneSetup
import com.ge.research.sadl.ide.SADLIdeModule
import com.ge.research.sadl.scoping.GlobalScopeProviderFilterProvider
import com.google.inject.Guice
import org.eclipse.xtext.util.Modules2

/**
 * Standalone setup for SADL IDE tests with customized "project helper".
 * Model files do not have to physically belong to the same container project.  
 */
class SadlIdeTestSetup extends SADLStandaloneSetup {

	override createInjector() {
		Guice.createInjector(Modules2.mixin(new SADLRuntimeModule, new SADLIdeModule, [
			// Re-re-bind the global scope provider filer. That does not expect container projects during the tests.
			bind(GlobalScopeProviderFilterProvider)
		]))
	}

}
