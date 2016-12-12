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

import org.eclipse.xtext.testing.AbstractLanguageServerTest
import org.junit.Assume
import org.eclipse.core.runtime.Platform

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
	
	@Override
	override setup() {
		Assume.assumeFalse(Platform.running);
		super.setup()
	}
	
	/**
	 * Sole constructor. Defines the proper file extension.
	 */
	new() {
		super(FILE_EXTENSION)
	}
	
}