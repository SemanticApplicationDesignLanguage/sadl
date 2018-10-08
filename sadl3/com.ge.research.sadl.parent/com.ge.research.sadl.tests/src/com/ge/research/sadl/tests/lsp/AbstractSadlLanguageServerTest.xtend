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
package com.ge.research.sadl.tests.lsp

import org.eclipse.emf.common.EMFPlugin
import org.eclipse.xtext.resource.IResourceServiceProvider
import org.eclipse.xtext.testing.AbstractLanguageServerTest
import org.eclipse.xtext.util.Modules2
import org.junit.Assume

/**
 * Base class for all language server tests for the {@code SADL} language.
 * 
 * @author akos.kitta
 */
class AbstractSadlLanguageServerTest extends AbstractLanguageServerTest {
	
	/**
	 * The desired file extension for the {@code SADL} language. 
	 */
	protected static val FILE_EXTENSION = 'sadl';
	
	override setup() {
		Assume.assumeTrue(!EMFPlugin.IS_ECLIPSE_RUNNING);
		super.setup()
	}
	
	/**
	 * Sole constructor. Defines the proper file extension.
	 */
	new() {
		super(FILE_EXTENSION)
	}
	
	override protected getServerModule() {
		return Modules2.mixin(super.serverModule, [
			bind(IResourceServiceProvider.Registry).toProvider(SadlIdeTestIResourceServiceProviderRegistry)
		]);
	}
	
}