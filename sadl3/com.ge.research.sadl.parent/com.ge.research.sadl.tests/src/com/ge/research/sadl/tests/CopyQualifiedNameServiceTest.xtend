/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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

import com.ge.research.sadl.utils.SadlQualifiedNameToStringService
import com.google.common.base.Supplier
import com.google.common.base.Suppliers
import com.google.inject.Inject
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.resource.EObjectAtOffsetHelper
import org.eclipse.xtext.resource.XtextResource
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

/**
 * Test for the SADL specific qualified name to string converter service.
 * 
 * @author akos.kitta
 * @see https://github.com/crapo/sadlos2/issues/134
 */
@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class CopyQualifiedNameServiceTest extends AbstractSADLParsingTest {

	@Inject
	SadlQualifiedNameToStringService qualifiedNameToStringService;
	
	@Inject
	IQualifiedNameProvider qualifiedNameProvider;

	@Inject
	EObjectAtOffsetHelper objectAtOffsetHelper;
	
	Supplier<XtextResource> resourceSupplier = Suppliers.memoize([
		'''
			uri "http://sadl.org/Shapes.sadl" alias Shapes.
			
			Shape is a class described by area with values of type float.
		'''.sadl;
		
		'''
			uri "http://sadl.org/Circle.sadl" alias Circle.
			
			import "http://sadl.org/Shapes.sadl".
			
			Circle is a type of Shape described by radius with values of type float.
			AnotherCircle is a type of Shape described by radius with values of type float.
		'''.sadl as XtextResource;
	]);

	@Test
	def void testShape() {
		val offset = '''
			uri "http://sadl.org/Circle.sadl" alias Circle.
			
			import "http://sadl.org/Shapes.sadl".
			
			Circle is a type of Sh
		'''.length;
		val selectedElement = objectAtOffsetHelper.resolveElementAt(resourceSupplier.get, offset);
		val qn = qualifiedNameProvider.getFullyQualifiedName(selectedElement);
		assertEquals('http://sadl.org/Shapes.sadl#Shape', qualifiedNameToStringService.toString(qn));
	}

	@Test
	def void testCircle() {
		val offset = '''
			uri "http://sadl.org/Circle.sadl" alias Circle.
			
			import "http://sadl.org/Shapes.sadl".
			
			Circl
		'''.length;
		val selectedElement = objectAtOffsetHelper.resolveElementAt(resourceSupplier.get, offset);
		val qn = qualifiedNameProvider.getFullyQualifiedName(selectedElement);
		assertEquals('http://sadl.org/Circle.sadl#Circle', qualifiedNameToStringService.toString(qn));
	}

	@Test
	def void testAnotherCircle() {
		val offset = '''
			uri "http://sadl.org/Circle.sadl" alias Circle.
			
			import "http://sadl.org/Shapes.sadl".
			
			Circle is a type of Shape described by radius with values of type float.
			AnotherCir
		'''.length;
		val selectedElement = objectAtOffsetHelper.resolveElementAt(resourceSupplier.get, offset);
		val qn = qualifiedNameProvider.getFullyQualifiedName(selectedElement);
		assertEquals('http://sadl.org/Circle.sadl#AnotherCircle', qualifiedNameToStringService.toString(qn));
	}

	@Test
	def void testRadius() {
		val offset = '''
			uri "http://sadl.org/Circle.sadl" alias Circle.

			import "http://sadl.org/Shapes.sadl".

			Circle is a type of Shape described by rad
		'''.length;
		val selectedElement = objectAtOffsetHelper.resolveElementAt(resourceSupplier.get, offset);
		val qn = qualifiedNameProvider.getFullyQualifiedName(selectedElement);
		assertEquals('http://sadl.org/Circle.sadl#radius', qualifiedNameToStringService.toString(qn));
	}

}
