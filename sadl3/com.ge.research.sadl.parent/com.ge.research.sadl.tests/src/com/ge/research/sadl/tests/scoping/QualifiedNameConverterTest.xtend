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
package com.ge.research.sadl.tests.scoping

import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.QualifiedName
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import org.junit.Ignore

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class QualifiedNameConverterTest {
	
	@Inject IQualifiedNameConverter converter
	
	@Test def void testConverter_01() {
		assertEquals(QualifiedName.create('foo','bar'), converter.toQualifiedName('foo:bar'))
		assertEquals(QualifiedName.create('http://foo/bar','bar'), converter.toQualifiedName('http://foo/bar:bar'))
		assertEquals(QualifiedName.create('foo'), converter.toQualifiedName('foo'))
		assertEquals(QualifiedName.create('http://foo/bar'), converter.toQualifiedName('http://foo/bar'))
	}
	
	@Ignore
	@Test def void testConverter_02() {
		assertEquals(QualifiedName.create('^value'), converter.toQualifiedName('^value'))
		assertEquals(QualifiedName.create('http://foo/bar','^value'), converter.toQualifiedName('http://foo/bar:value'))
	}
	
}