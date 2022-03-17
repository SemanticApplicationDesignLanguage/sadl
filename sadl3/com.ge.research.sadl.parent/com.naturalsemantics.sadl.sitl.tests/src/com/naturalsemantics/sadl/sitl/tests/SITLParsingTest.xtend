/************************************************************************
 * 
 * Project: SADL
 * Copyright 2007-2022 - General Electric Company, All Rights Reserved
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
/*
 * SADL Extension for SADL Import Template Language (SITL)
 * Copyright 2022 - Natural Semantics, LLC, All Rights Reserved
 */
package com.naturalsemantics.sadl.sitl.tests

import com.ge.research.sadl.sADL.SadlModel
import com.google.inject.Inject
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.util.ParseHelper
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(XtextRunner)
@InjectWith(SITLInjectorProvider)
class SITLParsingTest {
	@Inject
	ParseHelper<SadlModel> parseHelper
	
	@Test
	def void loadModel() {
		val result = parseHelper.parse('''
			uri "http://sadl.org/TGTemplate.sitl" alias tgt.
			
			import "http://sadl.org/TGModel.sadl".
			
			validate: ^A is a string.
			validate: B is a decimal.
			validate: C is a decimal.
			validate: D is a dateTime.
			
			Transform: TID = strConcat("T",^A).
			Transform: GID = strConcat("Gen", ^A).
			
			TID is a Turbine with speed 
				(a Speed with ^value B, with unit "RPM", with timestamp D).
			TID drives 
				(a Generator GID with power 
					(a Power with ^value C, with unit "MW", with timestamp D)
				).
			
			Incremental.	// add the generated triples to the existing model if such exists
			
			Infer: chunk 20 parallel 9.	// do inference over 20 rows of data at a time, with up to 9 parallel threads
			
			Log: "ImportLog.log".	// generate a log file containing an ordered record of each triple generated, along with validation information
		''')
		Assert.assertNotNull(result)
		val errors = result.eResource.errors
		Assert.assertTrue('''Unexpected errors: «errors.join(", ")»''', errors.isEmpty)
	}
}
