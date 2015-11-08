package com.ge.research.sadl.tests

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration
import com.ge.research.sadl.sADL.SadlModel
import com.google.inject.Inject
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import com.ge.research.sadl.sADL.SadlInstance
import com.ge.research.sadl.sADL.SadlResource

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlLinkingTests {
	
	@Inject ParseHelper<SadlModel> parseHelper
	
	@Inject extension DeclarationExtensions 
	
	@Test def void testImportsLink() {
		val first = parseHelper.parse('''
			uri "http://sadl.org.Tests/ModelName" alias foo.
		''')
		val second = parseHelper.parse('''
			uri "http://sadl.org/Tests/Import" alias imp.
			import "http://sadl.org.Tests/ModelName".
		''', first.eResource.resourceSet)
		assertSame(first, second.imports.head.importedResource)
	}
	
	@Test def void testClassesLink() {
		val first = parseHelper.parse('''
			uri "http://sadl.org.Tests/ModelName" alias foo.
			
			Foo is a class
			Bar is a type of Foo
		''')
		val foo = first.elements.get(0) as SadlClassOrPropertyDeclaration
		val bar = first.elements.get(1) as SadlClassOrPropertyDeclaration
		val referencedSuperType = bar.superElement.referencedSadlResources.head
		assertFalse(referencedSuperType.eIsProxy)
		assertSame(foo, referencedSuperType.eContainer)
	}
	
	@Test def void testResourceLinking_02() {
		val first = parseHelper.parse('''
			uri "http://com.ge.research.sadlGeorgeAndMarthaErr".
			
			SomeClass is a class.
			SomeClass2 is a type of SomeClass.
			SomeClass3 is a type of {SomeClass and SomeClass2}.
			{^A,B,C} are classes.
			{A1,B1,C1} are types of {SomeClass or ^A}.
		''')
		val someClass = first.elements.get(0) as SadlClassOrPropertyDeclaration
		val someClass2 = first.elements.get(1) as SadlClassOrPropertyDeclaration
		val someClass3 = first.elements.get(2) as SadlClassOrPropertyDeclaration
		val abc_are_classes = first.elements.get(3) as SadlClassOrPropertyDeclaration
		val a1b1c1_are_classes = first.elements.get(4) as SadlClassOrPropertyDeclaration
		
		assertSame(someClass, someClass2.superElement.referencedSadlResources.head.eContainer)
		assertEquals(#{someClass, someClass2}, 
			someClass3.superElement.referencedSadlResources.map[eContainer].toSet)
		assertEquals(3, abc_are_classes.classOrProperty.size)
		assertEquals(3, a1b1c1_are_classes.classOrProperty.size)
		assertEquals(#{someClass.classOrProperty.head, abc_are_classes.classOrProperty.head}, 
			a1b1c1_are_classes.superElement.referencedSadlResources.toSet)
	}
	
	@Test def void testInstanceAndPropertyLinking_01() {
		val first = parseHelper.parse('''
			uri "http://com.ge.research.sadl/NotEqualRule2". 
			
			Thingy is a class described by connectedTo with values of type Thingy.
			
			T1 is a Thingy.
			T2 is a Thingy.
			T3 is a Thingy, with connectedTo T1, with connectedTo T2.
		''')
		val thingy = first.elements.get(0) as SadlClassOrPropertyDeclaration
		val t1 = first.elements.get(1) as SadlInstance
		val t2 = first.elements.get(2) as SadlInstance
		val t3 = first.elements.get(3) as SadlInstance
		
		assertSame(thingy.classOrProperty.head, t1.type.referencedSadlResources.head)
		assertSame(thingy.classOrProperty.head, t2.type.referencedSadlResources.head)
		assertSame(thingy.classOrProperty.head, t3.type.referencedSadlResources.head)
		assertSame(thingy.describedBy.head.nameDeclarations.head, t3.propertyInitializers.head.property)
		assertSame(thingy.describedBy.head.nameDeclarations.head, t3.propertyInitializers.get(1).property)
		assertSame(t1, (t3.propertyInitializers.get(0).value as SadlResource).declaration.eContainer)
		assertSame(t2, (t3.propertyInitializers.get(1).value as SadlResource).declaration.eContainer)
	}
}