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
}