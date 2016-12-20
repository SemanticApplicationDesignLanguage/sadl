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
package com.ge.research.sadl.ui.tests.editor

import com.ge.research.sadl.ui.tests.SADLUiInjectorProvider
import com.google.common.base.Throwables
import com.google.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.CoreException
import org.eclipse.emf.common.EMFPlugin
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.ui.XtextProjectHelper
import org.eclipse.xtext.ui.editor.copyqualifiedname.CopyQualifiedNameService
import org.junit.AfterClass
import org.junit.Assert
import org.junit.BeforeClass
import org.junit.runner.RunWith

import static org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil.*
import org.junit.Test
import org.eclipse.xtext.ui.resource.IResourceSetProvider
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.impl.BinaryResourceImpl.EObjectOutputStream
import org.eclipse.xtext.resource.EObjectAtOffsetHelper
import org.eclipse.xtext.resource.XtextResource

/**
 * Test for the SADL specific qualified name copier service.
 * 
 * @author akos.kitta
 * @see https://github.com/crapo/sadlos2/issues/134
 */
@RunWith(XtextRunner)
@InjectWith(SADLUiInjectorProvider)
class CopyQualifiedNameServiceTest extends Assert {

	static val PROJECT_NAME = 'SADLTestProject';

	@Inject
	IResourceSetProvider resourceSetProvider;

	@Inject
	CopyQualifiedNameService copyQualifiedNameService;

	@Inject
	EObjectAtOffsetHelper objectAtOffsetHelper;

	@BeforeClass
	def static void createProject() {
		assertTrue('Expected running platform for content assist test.', EMFPlugin.IS_ECLIPSE_RUNNING);
		getOrCreateProject();
		createFile('''«PROJECT_NAME»/Shapes.sadl''', '''
			uri "http://sadl.org/Shapes.sadl" alias Shapes.
			
			Shape is a class described by area with values of type float.
		''');
		createFile('''«PROJECT_NAME»/Circle.sadl''', '''
			uri "http://sadl.org/Circle.sadl" alias Circle.
			
			import "http://sadl.org/Shapes.sadl".
			
			Circle is a type of Shape described by radius with values of type float.
			AnotherCircle is a type of Shape described by radius with values of type float.
		''');
		reallyWaitForAutoBuild;
	}

	@AfterClass
	def static void deleteProject() {
		val project = findProject(PROJECT_NAME);
		if (project.accessible) {
			project.delete(true, true, monitor);
			assertFalse('Error while cleaning up test project.', project.accessible);
		}
	}

	private static def IProject getOrCreateProject() {
		return getOrCreateProject(PROJECT_NAME);
	}

	private static def IProject getOrCreateProject(String projectName) {
		val project = findProject(projectName);
		if (!project.accessible) {
			try {
				createProject(projectName);
				addNature(project, XtextProjectHelper.NATURE_ID);
				addBuilder(project, XtextProjectHelper.BUILDER_ID);
				project.open(monitor);
				reallyWaitForAutoBuild;
			} catch (CoreException e) {
				fail('''Error occurred while creating project: «project».
				«Throwables.getStackTraceAsString(e)»''');
			}
		}
		return project;
	}

	private static def IProject findProject(String projectName) {
		return root().getProject(projectName);
	}

	@Test
	def void testShape() {
		val uri = URI.createPlatformResourceURI('''«PROJECT_NAME»/Circle.sadl''', true);
		val resource = resourceSetProvider.get(getOrCreateProject()).getResource(uri, true) as XtextResource;
		val offset = '''
			uri "http://sadl.org/Circle.sadl" alias Circle.
			
			import "http://sadl.org/Shapes.sadl".
			
			Circle is a type of Sh
		'''.length;
		val selectedElement = objectAtOffsetHelper.resolveElementAt(resource, offset);
		val context = objectAtOffsetHelper.resolveContainedElementAt(resource, offset);
		assertEquals('http://sadl.org/Shapes.sadl#Shape', copyQualifiedNameService.getQualifiedName(selectedElement, context));
	}

	@Test
	def void testCircle() {
		val uri = URI.createPlatformResourceURI('''«PROJECT_NAME»/Circle.sadl''', true);
		val resource = resourceSetProvider.get(getOrCreateProject()).getResource(uri, true) as XtextResource;
		val offset = '''
			uri "http://sadl.org/Circle.sadl" alias Circle.
			
			import "http://sadl.org/Shapes.sadl".
			
			Circl
		'''.length;
		val selectedElement = objectAtOffsetHelper.resolveElementAt(resource, offset);
		val context = objectAtOffsetHelper.resolveContainedElementAt(resource, offset);
		assertEquals('http://sadl.org/Circle.sadl#Circle', copyQualifiedNameService.getQualifiedName(selectedElement, context));
	}

	@Test
	def void testAnotherCircle() {
		val uri = URI.createPlatformResourceURI('''«PROJECT_NAME»/Circle.sadl''', true);
		val resource = resourceSetProvider.get(getOrCreateProject()).getResource(uri, true) as XtextResource;
		val offset = '''
			uri "http://sadl.org/Circle.sadl" alias Circle.
			
			import "http://sadl.org/Shapes.sadl".
			
			Circle is a type of Shape described by radius with values of type float.
			AnotherCir
		'''.length;
		val selectedElement = objectAtOffsetHelper.resolveElementAt(resource, offset);
		val context = objectAtOffsetHelper.resolveContainedElementAt(resource, offset);
		assertEquals('http://sadl.org/Circle.sadl#AnotherCircle', copyQualifiedNameService.getQualifiedName(selectedElement, context));
	}

	@Test
	def void testRadius() {
		val uri = URI.createPlatformResourceURI('''«PROJECT_NAME»/Circle.sadl''', true);
		val resource = resourceSetProvider.get(getOrCreateProject()).getResource(uri, true) as XtextResource;
		val offset = '''
			uri "http://sadl.org/Circle.sadl" alias Circle.

			import "http://sadl.org/Shapes.sadl".

			Circle is a type of Shape described by rad
		'''.length;
		val selectedElement = objectAtOffsetHelper.resolveElementAt(resource, offset);
		val context = objectAtOffsetHelper.resolveContainedElementAt(resource, offset);
		assertEquals('http://sadl.org/Circle.sadl#radius', copyQualifiedNameService.getQualifiedName(selectedElement, context));
	}

}
