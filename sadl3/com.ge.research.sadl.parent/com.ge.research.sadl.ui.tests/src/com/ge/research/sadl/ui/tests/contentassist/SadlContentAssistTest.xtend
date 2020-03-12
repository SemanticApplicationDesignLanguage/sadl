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
package com.ge.research.sadl.ui.tests.contentassist

import com.ge.research.sadl.preferences.SadlPreferences
import org.eclipse.xtext.preferences.PreferenceKey
import org.junit.Test

/**
 * Plug-in tests for the SADL content assist.
 * 
 * @author akos.kitta
 */
class SadlContentAssistTest extends AbstractSadlContentAssistTest {

	/** Primitive primary type reference. */
	@Test
	def void checkCA_01_PrimaryType_Negative() {
		newBuilder('''uri "http://myUri". Foo is a class. myFoo is a ''').assertProposalIsNot('integer');
	}

	/** Primary type reference. */
	@Test
	def void checkCA_02_PrimaryType_Positive() {
		newBuilder('''uri "http://myUri". Foo is a class. myFoo is a ''').assertProposal('Foo');
	}

	/** Imported primary type reference. */
	@Test
	def void checkCA_03_PrimaryType_Positive() {
		newBuilder('''uri "http://myUri". import "http://barUri". Foo is a class. myFoo is a ''').assertProposal('Bar');
	}

	/** Self primary type reference with imports. */
	@Test
	def void checkCA_04_PrimaryType_Positive() {
		newBuilder('''uri "http://myUri". import "http://barUri". Foo is a class. myFoo is a ''').assertProposal('Foo');
	}

	/** Not visible primary type reference with imports. */
	@Test
	def void checkCA_05_PrimaryType_Negative() {
		newBuilder('''uri "http://myUri". import "http://barUri". Foo is a class. myFoo is a ''').
			assertProposalIsNot('NotVisible');
	}

	/** Property check with imports. */
	@Test
	def void checkCA_06_Property_Positive() {
		newBuilder('''uri "http://myUri". import "http://circle". import "http://rectangle". C is a Circle with ''').
			assertProposal('radius');
	}

	/** Property check with imports. */
	@Test
	def void checkCA_06b_Property_Positive() {
		newBuilder('''uri "http://myUri". import "http://circle". import "http://rectangle". C is a Circle. C has ''').
			assertProposal('radius');
	}

	/** Negative property check with imports. */
	@Test
	def void checkCA_07_Property_Negative() {
		newBuilder('''uri "http://myUri". import "http://circle". import "http://rectangle". C is a Circle with ''').
			assertProposalIsNot('width');
	}

	/** Super type element. */
	@Test
	def void checkCA_08_SuperElment_Positive() {
		newBuilder('''uri "http://myUri". Person is a class. {Man, Woman} are types of ''').assertProposal('Person');
	}

	/** Super type element. */
	@Test
	def void checkCA_09_SuperElment_Negative() {
		newBuilder('''uri "http://myUri". Person is a class. {Man, Woman} are types of ''').assertProposalIsNot('Man');
	}

	/** Subject of property in test statement. */
	@Test
	def void checkCA_10_SubjectOfProperty_Positive() {
		newBuilder('''uri "http://myUri". import "http://circle". import "http://rectangle". Test: width of ''').
			assertProposal('Rectangle');
	}

	/** Subject of property in test statement. */
	@Test
	def void checkCA_11_SubjectOfProperty_Negative() {
		// Instead of importing `Shape`, `Circle`, and `Rectangle`, this model defines them in place: https://github.com/crapo/sadlos2/issues/407
		newBuilder('''uri "http://myUri". Shape is a class described by area with values of type float. Rectangle is a type of Shape, described by height with values of type float, described by width with values of type float. Circle is a type of Shape described by radius with values of type float. Test: width of ''').
			assertProposalIsNot('Circle');
	}

	/** Subject of property in test statement. */
	@Test
	def void checkCA_12_PropertyInTestStatement_Positive() {
		newBuilder('''uri "http://myUri". import "http://circle". import "http://rectangle". Test: ''').
			assertProposal('width');
	}

	/** Subject of property in test statement. */
	@Test
	def void checkCA_13_PropertyInTestStatement_Negative() {
		newBuilder('''uri "http://myUri". import "http://circle". import "http://rectangle". Test: ''').
			assertProposalIsNot('Circle');
	}

	/** Property initializer value. */
	@Test
	def void checCA_14_PropertyInitializerValue_Positive() {
		newBuilder('''uri "http://myUri". Foo is a class described by p1 with values of type Foo. myFoo is a Foo with p1 ''').
			assertProposal('myFoo');
	}

	/** Property initializer value. */
	@Test
	def void checCA_14b_PropertyInitializerValue_Positive() {
		newBuilder('''uri "http://myUri". 
					  Bar is a class.
					  MyBar is a Bar.
					  Foo is a class described by p1 with values of type Bar. 
					  MyFoo is a Foo with p1 ''').
			assertProposal('MyBar');
	}

	/** Property initializer value. */

	@Test
	def void checCA_14c_PropertyInitializerValue_Positive() {
		newBuilder('''uri "http://myUri". 
					  Bar is a class.
					  MyBar is a Bar.
					  Foo is a class described by p1 with values of type Bar. 
					  MyFoo is a Foo with p1 M''').
			assertProposal('MyBar');
	}
	/** Super element with `type of`. */
	@Test
	def void checkCA_15_SuperElement_IsATypeOf_Positive() {
		newBuilder('''uri "http://myUri". Foo is a class. Bar is a type of ''').assertProposal('Foo');
	}

	/** Super element with `type of`. */
	@Test
	def void checkCA_16_SuperElement_IsATypeOf_Negative() {
		newBuilder('''uri "http://myUri". Foo is a class. Bar is a type of ''').assertProposalIsNot('Bar');
	}

	/** Instance declaration. */
	@Test
	def void checkCA_17_InstanceDeclaration1() {
		newBuilder(''' uri "http://sadl.org/classes.sadl" alias clses.
					   Shape is a class described by area with values of type float .
					   MyShape is a  ''').assertProposal('Shape');
	}
	
	/** Instance declaration. */
	@Test
	def void checkCA_17_InstanceDeclaration2() {
		newBuilder(''' uri "http://sadl.org/classes.sadl" alias clses.
					   Shape is a class described by area with values of type float .
					   MyShape is a  ''').assertProposalIsNot('float');
	}
	
	/** Keywords in range definition */
	@Test
	def void checkCA_18_RangeKeyword1() {
		newBuilder(''' uri "http://sadl.org/classes.sadl" alias clses.
					   Shape is a class described by area ''').assertProposal('with');
	}

	/** Keywords in range definition */
	@Test
	def void checkCA_19_RangeKeyword2() {
		newBuilder(''' uri "http://sadl.org/classes.sadl" alias clses.
					   Shape is a class described by area with ''').assertProposal('values');
	}
	
	/** Keywords in range definition */
	@Test
	def void checkCA_20_RangeKeyword3() {
		newBuilder(''' uri "http://sadl.org/classes.sadl" alias clses.
					   Shape is a class described by area with values ''').assertProposal('of');
	}
	
	/** Keywords in range definition */
	@Test
	def void checkCA_21_RangeKeyword4() {
		newBuilder(''' uri "http://sadl.org/classes.sadl" alias clses.
					   Shape is a class described by area with values of ''').assertProposal('type');
	}
	
	/** Keywords in range definition */
	@Test
	def void checkCA_22_Range1() {
		newBuilder(''' uri "http://sadl.org/classes.sadl" alias clses.
					   Shape is a class described by area with values of type ''').assertProposal('Shape');
	}
	
	/** Keywords in range definition */
	@Test
	def void checkCA_23_Range2() {
		newBuilder(''' uri "http://sadl.org/classes.sadl" alias clses.
					   Shape is a class described by area with values of type ''').assertProposal('float');
	}
	
	/** Keywords NOT in declaration */
	@Test
	def void checkCA_24_DeclarationCompletion() {
		newBuilder(''' uri "http://sadl.org/classes.sadl" alias clses.
					   Shape is a class described by area with values of type float.
					   MyShape is a ''').assertProposal('Shape');
	}
	
	/** Completion after start of input */
	@Test
	def void checkCA_25_DeclarationCompletion() {
		newBuilder(''' uri "http://sadl.org/classes.sadl" alias clses.
					   Shape is a class described by area with values of type float.
					   MyShape is a Sh''').assertProposal('Shape');
	}

	@Test
	def void checkCA_26_ImplicitModelIsNotFiltered() {
		newBuilder('''uri "http://myUri". Foo is a class. myFoo describes ''').assertProposal('ScientificConcept');
	}

	@Test
	def void checkCA_27_ImplicitModelCanBeFiltered() {
		val key = new PreferenceKey(SadlPreferences.CONTENT_ASSIST__FILTER_IMPLICIT_MODEL.id, Boolean.TRUE.toString);
		updatePreference(key);
		newBuilder('''uri "http://myUri". Foo is a class. myFoo describes ''').assertProposalIsNot('ScientificConcept');
	}

	@Test
	def void checkCA_28_NoPrimitivesAtdDomainOfAnyProperty() {
		// https://github.com/crapo/sadlos2/issues/406
		val builder = newBuilder('''uri "http://myUri". Engine is a class. engine describes ''');
		builder.assertProposalIsNot('double');
		builder.assertProposal('Engine');
	}

	@Test
	def void checkCA_29_PrimitivesInRanges() {
		// https://github.com/crapo/sadlos2/issues/406
		val builder = newBuilder('''uri "http://myUri". Aircraft is a class. engine describes Aircraft with values of type ''');
		builder.assertProposal('Aircraft');
		builder.assertProposal('double');
	}

	@Test
	def void checkCA_30_CanProposeIsAForClassOrPropertyDeclaration() {
		// https://github.com/crapo/sadlos2/issues/406#issuecomment-597610402
		newBuilder('''uri "http://myUri". Foo is ''').assertProposal('a');
	}

	@Test
	def void checkCA_31_CanProposeSadlResourcesOnTopLevel() {
		val builder = newBuilder('''
			uri "http://sadl.org/x.sadl".
			Artefact is a class.
			part describes Artefact with values of type Artefact.
			Aircraft is a type of Artefact.
			Engine is a type of Artefact.
		''');
		builder.assertProposal('Artefact');
		builder.assertProposal('part');
		builder.assertProposal('Aircraft');
		builder.assertProposal('Engine');
		// And we have the keywords too.
		builder.assertProposal('Ask');
		builder.assertProposal('Equation');
		// etc.
	}

	@Test
	def void checkCA_32_CanProposeCardinalityValue() {
		val builder = newBuilder('''
			uri "http://sadl.org/x.sadl".
			Artefact is a class.
			part describes Artefact with values of type Artefact.
			Aircraft is a type of Artefact.
			Engine is a type of Artefact.
			part of Aircraft has at least 
		''');
		builder.assertProposal('one');
		builder.assertProposal('2');
		builder.assertProposal('CARDINALITY');
	}

	@Test
	def void checkCA_33_AlwaysRunContextWithPrefix() {
		// https://github.com/crapo/sadlos2/issues/406#issuecomment-597622176
		val builder = newBuilder('''
			uri "http://sadl.org/x.sadl".
			Artefact is a class.
			Aircraft is a type of Artefact.
			W_IsSomethingWithW is a class.
			IsSomethingWithoutW is a class.
			MyAircraft is an Aircraft w
		''');
		builder.assertProposal('W_IsSomethingWithW');
		builder.assertProposal('with');
		builder.assertProposalIsNot('IsSomethingWithoutW');
	}

}
