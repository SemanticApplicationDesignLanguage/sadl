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
package com.ge.research.sadl.tests

import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.runner.RunWith
import com.ge.research.sadl.reasoner.ITranslator
import com.ge.research.sadl.jena.IJenaBasedModelProcessor
import java.lang.reflect.Method
import com.ge.research.sadl.builder.ConfigurationManagerForIDE

/**
 * Use this class as the super class of your test classes if you would like to run the model processors.
 */
@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
abstract class AbstractSADLModelProcessorTest extends AbstractSADLParsingTest {
		
	def ITranslator getTranslator(IJenaBasedModelProcessor processor) {
		val gcm = processor.getClass.getDeclaredMethod("getConfigMgr") as Method
		gcm.accessible = true
		val cm = gcm.invoke(processor, null) as ConfigurationManagerForIDE;
		val trans =cm.translator as ITranslator
		trans
	}
	
	
}