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
package com.ge.research.sadl.tests.validation

import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.ge.research.sadl.validation.SADLValidator
import com.google.inject.Inject
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith
import com.ge.research.sadl.scoping.TestScopeProvider

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class ValidationTest {
	
	@Test def void testDuplicateAlias() {
		'''
			uri "http://my/uri" alias foo.
		'''.sadl
		'''
		    uri "http://other/uri" alias foo.
		'''.sadl => [
			validationTestHelper.assertError(it, SADLPackage.Literals.SADL_MODEL, SADLValidator.INVALID_MODEL_ALIAS)
		]
	}
	
	@Test def void testDuplicateSimpleFileName() {
		val uri = '''
			uri "http://my/uri1" alias foo.
		'''.sadl.URI
		parseHelper.parse('''
		    uri "http://my/uri2" alias bar.
		''', uri.trimSegments(1).appendSegment("subpath").appendSegment(uri.lastSegment), currentResourceSet) => [
			validationTestHelper.assertError(it, SADLPackage.Literals.SADL_MODEL, SADLValidator.INVALID_MODEL_FILENAME)
		]
	}
	
	@Test def void testLinkingAmbiguousElements() {
		'''
			uri "http://sadl.org/NS1.sadl" alias ns1.
			
			Car is a class.
		'''.sadl
		'''
			uri "http://sadl.org/NS2.sadl" alias ns2.
			
			Car is a class.
		'''.sadl
		val model = '''
			uri "http://sadl.org/NS3.sadl" alias ns3.
			
			import "http://sadl.org/NS1.sadl".
			import "http://sadl.org/NS2.sadl".
			
			MyCar is a Car.
			
		'''.sadl
		TestScopeProvider.registerResource(model, true)
		val issues = validationTestHelper.validate(model)
		Assert.assertEquals("Ambiguously imported name 'Car' from 'http://sadl.org/NS2.sadl', 'http://sadl.org/NS1.sadl'. Please use an alias or choose different names.", issues.head.message)
	}
	
	@Test def void testLinkingAmbiguousElementsIndirect() {
		'''
			uri "http://sadl.org/NS1.sadl" alias ns1.
			
			Car is a class.
		'''.sadl
		'''
			uri "http://sadl.org/NS2.sadl" alias ns2.
			
			Car is a class.
		'''.sadl
		'''
			uri "http://sadl.org/NS3.sadl" alias ns3.
			
			import "http://sadl.org/NS1.sadl".
			import "http://sadl.org/NS2.sadl".
						
		'''.sadl
		val model = '''
			uri "http://sadl.org/NS4.sadl" alias ns4.
			
			import "http://sadl.org/NS3.sadl".
			MyCar is a Car.
		'''.sadl
		TestScopeProvider.registerResource(model, true)
		val issues = validationTestHelper.validate(model)
		Assert.assertEquals("Ambiguously imported name 'Car' from 'http://sadl.org/NS2.sadl', 'http://sadl.org/NS1.sadl'. Please use an alias or choose different names.", issues.head.message)
	}
	
	@Test def void testLinkingAmbiguousElementsBigger() {
		'''
			uri "http://assert/Properties" alias Properties.
			//import "http://sadl.org/sadlimplicitmodel".
			reference_class is a type of annotation.
			reference_property is a type of annotation.
			reference_range is a type of annotation.
			reference_instance is a type of annotation.
			//impliedProperty is a type of annotation.
			INTERFACE_DEFINITION is a class.
			  functional_max describes INTERFACE_DEFINITION with values of type decimal.
			  functional_min describes INTERFACE_DEFINITION with values of type decimal.
			  physical_max describes INTERFACE_DEFINITION with values of type decimal.
			  physical_min describes INTERFACE_DEFINITION with values of type decimal.
			  tolerance describes INTERFACE_DEFINITION with values of type decimal.
			  resolution describes INTERFACE_DEFINITION with values of type decimal.
			  physical_mapping describes INTERFACE_DEFINITION with values of type PHYSICAL_MAPPING.
			  PHYSICAL_MAPPING is a class.
			    physical_value describes PHYSICAL_MAPPING with values of type decimal.
			
			
			//====================================================
			//All user defined Object Properties are recorded below.
			//====================================================
			
			  EVENT is a class.
			
			  DATA is a class.
			    ^data describes DATA with values of type decimal.
			    DATA has impliedProperty ^data.
			//====================================================
			
			
			//ObjectId=ASSERT_DM-94
			X is a property.
			
			//ObjectId=ASSERT_DM-213, ASSERT_DM-205, ASSERT_DM-209
			_string is a property.
			
			//ObjectId=ASSERT_DM-46
			_value is a property.
			
			//ObjectId=ASSERT_DM-135
			active_frequency is a property.
			
			//ObjectId=ASSERT_DM-137
			active_status is a property.
			
			//ObjectId=ASSERT_DM-111
			alpha_numeric_characters is a property.
			
			//ObjectId=ASSERT_DM-207
			characters is a property.
			
			//ObjectId=ASSERT_DM-128
			default_position is a property.
			
			//ObjectId=ASSERT_DM-172
			error_code is a property.
			
			//ObjectId=ASSERT_DM-27, ASSERT_DM-30, ASSERT_DM-32, ASSERT_DM-73, ASSERT_DM-75
			event_data_1 is a property.
			
			//ObjectId=ASSERT_DM-33, ASSERT_DM-76
			event_data_2 is a property.
			
			//ObjectId=ASSERT_DM-34
			event_data_3 is a property.
			
			//ObjectId=ASSERT_DM-14
			example_11 is a property.
			
			//ObjectId=ASSERT_DM-51
			example_12 is a property.
			
			//ObjectId=ASSERT_DM-52
			example_13 is a property.
			
			//ObjectId=ASSERT_DM-3
			example_2 is a property.
			
			//ObjectId=ASSERT_DM-82
			example_21 is a property.
			
			//ObjectId=ASSERT_DM-84
			example_23 is a property.
			
			//ObjectId=ASSERT_DM-217
			example_28 is a property.
			
			//ObjectId=ASSERT_DM-4
			example_3 is a property.
			
			//ObjectId=ASSERT_DM-244
			example_31 is a property.
			
			//ObjectId=ASSERT_DM-5
			example_4 is a property.
			
			//ObjectId=ASSERT_DM-11
			example_8 is a property.
			
			//ObjectId=ASSERT_DM-12
			example_9 is a property.
			
			//ObjectId=ASSERT_DM-185
			herons_input is a property.
			
			//ObjectId=ASSERT_DM-191
			increase_multiplier is a property.
			
			//ObjectId=ASSERT_DM-190
			increased_value is a property.
			
			//ObjectId=ASSERT_DM-189, ASSERT_DM-194
			initial_multiplier is a property.
			
			//ObjectId=ASSERT_DM-188, ASSERT_DM-193
			initial_value is a property.
			
			//ObjectId=ASSERT_DM-80, ASSERT_DM-103
			input is a property.
			
			//ObjectId=ASSERT_DM-15, ASSERT_DM-225
			input_1 is a property.
			
			//ObjectId=ASSERT_DM-24
			input_10 is a property.
			
			//ObjectId=ASSERT_DM-25
			input_11 is a property.
			
			//ObjectId=ASSERT_DM-16
			input_2 is a property.
			
			//ObjectId=ASSERT_DM-226
			input_2_1 is a property.
			
			//ObjectId=ASSERT_DM-227
			input_2_2 is a property.
			
			//ObjectId=ASSERT_DM-17, ASSERT_DM-229
			input_3 is a property.
			
			//ObjectId=ASSERT_DM-18
			input_4 is a property.
			
			//ObjectId=ASSERT_DM-19
			input_5 is a property.
			
			//ObjectId=ASSERT_DM-20
			input_6 is a property.
			
			//ObjectId=ASSERT_DM-21
			input_7 is a property.
			
			//ObjectId=ASSERT_DM-22
			input_8 is a property.
			
			//ObjectId=ASSERT_DM-23
			input_9 is a property.
			
			//ObjectId=ASSERT_DM-238
			intermediate_value_2_result is a property.
			
			//ObjectId=ASSERT_DM-86, ASSERT_DM-106, ASSERT_DM-141, ASSERT_DM-148, ASSERT_DM-155, ASSERT_DM-167, ASSERT_DM-175
			invoked is a property.
			
			//ObjectId=ASSERT_DM-219
			maximum is a property.
			
			//ObjectId=ASSERT_DM-218
			minimum is a property.
			
			//ObjectId=ASSERT_DM-120
			mode is a property.
			
			//ObjectId=ASSERT_DM-230
			output_1 is a property.
			
			//ObjectId=ASSERT_DM-231
			output_2 is a property.
			
			//ObjectId=ASSERT_DM-232
			output_3 is a property.
			
			//ObjectId=ASSERT_DM-115, ASSERT_DM-117, ASSERT_DM-119
			position is a property.
			
			//ObjectId=ASSERT_DM-40
			property_1 is a property.
			
			//ObjectId=ASSERT_DM-42
			property_2 is a property.
			
			//ObjectId=ASSERT_DM-90
			rad is a property.
			
			//ObjectId=ASSERT_DM-145
			radio_number is a property.
			
			//ObjectId=ASSERT_DM-8
			received is a property.
			
			//ObjectId=ASSERT_DM-196
			reduce_multiplier is a property.
			
			//ObjectId=ASSERT_DM-195
			reduced_value is a property.
			
			//ObjectId=ASSERT_DM-234, ASSERT_DM-236, ASSERT_DM-239, ASSERT_DM-92, ASSERT_DM-89, ASSERT_DM-102
			result is a property.
			
			//ObjectId=ASSERT_DM-214, ASSERT_DM-108, ASSERT_DM-204, ASSERT_DM-208
			return is a property.
			
			//ObjectId=ASSERT_DM-126
			sensor is a property.
			
			//ObjectId=ASSERT_DM-7
			sent is a property.
			
			//ObjectId=ASSERT_DM-109
			source is a property.
			
			//ObjectId=ASSERT_DM-136
			standby_frequency is a property.
			
			//ObjectId=ASSERT_DM-139
			standby_status is a property.
			
			//ObjectId=ASSERT_DM-241
			value_1 is a property.
			
			//ObjectId=ASSERT_DM-242
			value_2 is a property.
			
			//ObjectId=ASSERT_DM-243
			value_3 is a property.
			
			//ObjectId=ASSERT_DM-132
			vor_1_data is a property.
			
			//ObjectId=ASSERT_DM-133
			vor_2_data is a property.
			
			//ObjectId=ASSERT_DM-144
			vor_control_topic is a property.
			
			//ObjectId=ASSERT_DM-184
			y_herons is a property.
		'''.sadl
		'''
			uri "http://ont.sample/BASICS" alias BASICS.
			import "http://assert/Properties".
			//SectionObjectId=ASSERT_DM-123
			
			//ObjectId=ASSERT_DM-123
			BASICS is a class.
			  //ObjectId=ASSERT_DM-127
			  SENSOR is a class.
			  
			  //ObjectId=ASSERT_DM-206
			  STRING is a class.
			    //ObjectId=ASSERT_DM-207
			    characters describes STRING with values of type CHARACTER List.
			    characters of STRING only has values of type CHARACTER List.
			
			    ASSERT_DM-207 is a INTERFACE_DEFINITION,
			      with reference_class STRING,
			      with reference_property characters .
			
			  
			  //ObjectId=ASSERT_DM-112
			  CHARACTER is a class.
			  
			  //ObjectId=ASSERT_DM-216
			  BOOLEAN is a class, must be one of {True,False}.
			  
			  //ObjectId=ASSERT_DM-203
			  CSC is a class.
			  
			  //ObjectId=ASSERT_DM-202
			  INTERFACE is a type of EVENT.
			  
			  //ObjectId=ASSERT_DM-138
			  STATUS is a class, must be one of {Valid,Invalid}.
			  
			  //ObjectId=ASSERT_DM-100
			  description is a type of annotation.
			  
			  //ObjectId=ASSERT_DM-199
			  accuracy is a type of annotation.
			  
			
			//EndSectionObject
		'''.sadl
		val model = '''
			uri "http://ont.sample/C_SYSTEM" alias C_SYSTEM.
			import "http://assert/Properties".
			//SectionObjectId=ASSERT_DM-224
			import "http://ont.sample/BASICS".
			//ObjectId=ASSERT_DM-224
			C_SYSTEM is a class.
			  //ObjectId=ASSERT_DM-225
			  input_1 describes C_SYSTEM with values of type DATA.
			  input_1 of C_SYSTEM only has values of type DATA.
			  input_1 of C_SYSTEM has exactly 1 values.
			
			  ASSERT_DM-225 is a INTERFACE_DEFINITION,
			    with reference_class C_SYSTEM,
			    with reference_property input_1 .
			
			  //ObjectId=ASSERT_DM-226
			  input_2_1 describes C_SYSTEM with values of type DATA.
			  input_2_1 of C_SYSTEM only has values of type DATA.
			  input_2_1 of C_SYSTEM has exactly 1 values.
			
			  ASSERT_DM-226 is a INTERFACE_DEFINITION,
			    with reference_class C_SYSTEM,
			    with reference_property input_2_1,
			    with functional_max 10,
			    with functional_min 0,
			    with physical_max 10,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-227
			  input_2_2 describes C_SYSTEM with values of type DATA.
			  input_2_2 of C_SYSTEM only has values of type DATA.
			  input_2_2 of C_SYSTEM has exactly 1 values.
			
			  ASSERT_DM-227 is a INTERFACE_DEFINITION,
			    with reference_class C_SYSTEM,
			    with reference_property input_2_2,
			    with functional_max 10,
			    with functional_min 0,
			    with physical_max 10,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-228
			  input_2_c describes C_SYSTEM with values of type boolean.
			  input_2_c of C_SYSTEM has exactly 1 values.
			
			  ASSERT_DM-228 is a INTERFACE_DEFINITION,
			    with reference_class C_SYSTEM,
			    with reference_property input_2_c,
			    with functional_max 10,
			    with functional_min 0,
			    with physical_max 10,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-229
			  input_3 describes C_SYSTEM with values of type DATA.
			  input_3 of C_SYSTEM only has values of type DATA.
			  input_3 of C_SYSTEM has exactly 1 values.
			
			  ASSERT_DM-229 is a INTERFACE_DEFINITION,
			    with reference_class C_SYSTEM,
			    with reference_property input_3,
			    with functional_max 10,
			    with functional_min 0,
			    with physical_max 10,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-230
			  output_1 describes C_SYSTEM with values of type DATA.
			  output_1 of C_SYSTEM only has values of type DATA.
			  output_1 of C_SYSTEM has exactly 1 values.
			
			  ASSERT_DM-230 is a INTERFACE_DEFINITION,
			    with reference_class C_SYSTEM,
			    with reference_property output_1,
			    with functional_max 10,
			    with functional_min 0,
			    with physical_max 10,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-231
			  output_2 describes C_SYSTEM with values of type DATA.
			  output_2 of C_SYSTEM only has values of type DATA.
			  output_2 of C_SYSTEM has exactly 1 values.
			
			  ASSERT_DM-231 is a INTERFACE_DEFINITION,
			    with reference_class C_SYSTEM,
			    with reference_property output_2,
			    with functional_max 10,
			    with functional_min 0,
			    with physical_max 10,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-232
			  output_3 describes C_SYSTEM with values of type RECORD.
			  output_3 of C_SYSTEM only has values of type RECORD.
			  output_3 of C_SYSTEM has exactly 3 values.
			
			  ASSERT_DM-232 is a INTERFACE_DEFINITION,
			    with reference_class C_SYSTEM,
			    with reference_property output_3,
			    with functional_max 10,
			    with functional_min 0,
			    with physical_max 10,
			    with physical_min 0,
			    with tolerance 1,
			    with resolution 1 .
			
			  //ObjectId=ASSERT_DM-240
			  RECORD is a class.
			    //ObjectId=ASSERT_DM-241
			    value_1 describes RECORD with values of type DATA.
			    value_1 of RECORD only has values of type DATA.
			    value_1 of RECORD has exactly 1 values.
			
			    ASSERT_DM-241 is a INTERFACE_DEFINITION,
			      with reference_class RECORD,
			      with reference_property value_1,
			      with tolerance .1,
			      with resolution .1 .
			
			    //ObjectId=ASSERT_DM-242
			    value_2 describes RECORD with values of type DATA.
			    value_2 of RECORD only has values of type DATA.
			    value_2 of RECORD has exactly 1 values.
			
			    ASSERT_DM-242 is a INTERFACE_DEFINITION,
			      with reference_class RECORD,
			      with reference_property value_2,
			      with tolerance 1,
			      with resolution 1 .
			
			    //ObjectId=ASSERT_DM-243
			    value_3 describes RECORD with values of type DATA.
			    value_3 of RECORD only has values of type DATA.
			    value_3 of RECORD has exactly 1 values.
			
			    ASSERT_DM-243 is a INTERFACE_DEFINITION,
			      with reference_class RECORD,
			      with reference_property value_3,
			      with tolerance 1,
			      with resolution 1 .
			
			  
			  //ObjectId=ASSERT_DM-233
			  INTERMEDIATE_VALUE_1 is a class.
			    //ObjectId=ASSERT_DM-234
			    result describes INTERMEDIATE_VALUE_1 with values of type DATA.
			    result of INTERMEDIATE_VALUE_1 only has values of type DATA.
			    result of INTERMEDIATE_VALUE_1 has exactly 1 values.
			
			    ASSERT_DM-234 is a INTERFACE_DEFINITION,
			      with reference_class INTERMEDIATE_VALUE_1,
			      with reference_property result,
			      with tolerance .1,
			      with resolution .1 .
			
			  
			  //ObjectId=ASSERT_DM-235
			  INTERMEDIATE_VALUE_2 is a class.
			    //ObjectId=ASSERT_DM-236
			    result describes INTERMEDIATE_VALUE_2 with values of type DATA.
			    result of INTERMEDIATE_VALUE_2 only has values of type DATA.
			    result of INTERMEDIATE_VALUE_2 has exactly 1 values.
			
			    ASSERT_DM-236 is a INTERFACE_DEFINITION,
			      with reference_class INTERMEDIATE_VALUE_2,
			      with reference_property result,
			      with tolerance 1,
			      with resolution 1 .
			
			  
			  //ObjectId=ASSERT_DM-237
			  INTERMEDIATE_VALUE_3 is a class.
			    //ObjectId=ASSERT_DM-238
			    intermediate_value_2_result describes INTERMEDIATE_VALUE_3 with values of type DATA.
			    intermediate_value_2_result of INTERMEDIATE_VALUE_3 only has values of type DATA.
			    intermediate_value_2_result of INTERMEDIATE_VALUE_3 has exactly 1 values.
			
			    ASSERT_DM-238 is a INTERFACE_DEFINITION,
			      with reference_class INTERMEDIATE_VALUE_3,
			      with reference_property intermediate_value_2_result,
			      with tolerance 1,
			      with resolution 1 .
			
			    //ObjectId=ASSERT_DM-239
			    result describes INTERMEDIATE_VALUE_3 with values of type DATA.
			    result of INTERMEDIATE_VALUE_3 only has values of type DATA.
			    result of INTERMEDIATE_VALUE_3 has exactly 1 values.
			
			    ASSERT_DM-239 is a INTERFACE_DEFINITION,
			      with reference_class INTERMEDIATE_VALUE_3,
			      with reference_property result,
			      with tolerance 1,
			      with resolution 1 .
			
			  
			
			//EndSectionObject
			
		'''.sadl
		validationTestHelper.assertNoErrors(model)
	}
	
	@Test def void testLinkingAmbiguousElements_01() {
		'''
			uri "http://sadl.org/NS1.sadl" alias ns1.
			
			Car is a class.
		'''.sadl
		'''
			uri "http://sadl.org/NS2.sadl" alias ns2.
			import "http://sadl.org/NS1.sadl".
		'''.sadl
		val model = '''
			uri "http://sadl.org/NS3.sadl" alias ns3.
			
			import "http://sadl.org/NS1.sadl".
			import "http://sadl.org/NS2.sadl".
			
			MyCar is a Car.
			
		'''.sadl
		validationTestHelper.assertNoErrors(model)
	}

	@Inject ValidationTestHelper validationTestHelper
	@Inject ParseHelper<SadlModel> parseHelper
	XtextResourceSet currentResourceSet

	protected def XtextResource sadl(CharSequence contents) {
		val resource = if (currentResourceSet === null) {
				parseHelper.parse(contents).eResource as XtextResource
			} else {
				parseHelper.parse(contents, currentResourceSet).eResource as XtextResource
			}
		currentResourceSet = resource.resourceSet as XtextResourceSet
		return resource
	}
	
	
}
