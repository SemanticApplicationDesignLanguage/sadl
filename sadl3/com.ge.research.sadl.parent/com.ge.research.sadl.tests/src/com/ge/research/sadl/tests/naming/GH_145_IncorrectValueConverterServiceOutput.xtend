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
package com.ge.research.sadl.tests.naming

import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.conversion.IValueConverter
import org.eclipse.xtext.conversion.IValueConverterService
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.QualifiedName
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.InjectWith

/**
 * Test for checking the human readable output of the value converter service.
 * 
 * @author akos.kitta
 */
@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class GH_145_IncorrectValueConverterServiceOutput {

	@Inject
	IQualifiedNameConverter qualifiedNameConverter;

	@Inject
	IValueConverterService valueConverterService;

	@Test
	def void check_QNAME_1() {
		val name = qualifiedNameConverter.toString(QualifiedName.create('foo'));
		Assert.assertEquals('foo', getConverter('QNAME').toString(name));
	}

	@Test
	def void check_QNAME_2() {
		val name = qualifiedNameConverter.toString(QualifiedName.create('foo:bar'));
		Assert.assertEquals('foo:bar', getConverter('QNAME').toString(name));
	}
	
	@Test
	def void check_QNAME_3() {
		val name = qualifiedNameConverter.toString(QualifiedName.create('^foo:bar'));
		Assert.assertEquals('^foo:bar', getConverter('QNAME').toString(name));
	}

	@Test
	def void check_QNAME_4() {
		val name = qualifiedNameConverter.toString(QualifiedName.create('foo:^bar'));
		Assert.assertEquals('foo:^bar', getConverter('QNAME').toString(name));
	}

	@Test
	def void check_QNAME_5() {
		val name = qualifiedNameConverter.toString(QualifiedName.create('^foo:^bar'));
		Assert.assertEquals('^foo:^bar', getConverter('QNAME').toString(name));
	}

	@Test
	def void check_ID() {
		val name = qualifiedNameConverter.toString(QualifiedName.create('foo'));
		Assert.assertEquals('foo', getConverter('ID').toString(name));
	}

	@Test
	def void check_DNAME() {
		val name = qualifiedNameConverter.toString(QualifiedName.create('foo'));
		Assert.assertEquals('foo', getConverter('DNAME').toString(name));
	}

	private def IValueConverter<Object> getConverter(String lexerRule) {
		return (valueConverterService as IValueConverterService.Introspectable).getConverter(lexerRule);
	}

}
