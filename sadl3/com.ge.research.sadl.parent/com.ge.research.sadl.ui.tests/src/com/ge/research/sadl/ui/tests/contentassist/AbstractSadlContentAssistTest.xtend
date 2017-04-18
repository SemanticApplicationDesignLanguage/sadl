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

import com.ge.research.sadl.tests.AbstractLinkingTest
import com.ge.research.sadl.ui.OutputStreamStrategy
import com.ge.research.sadl.ui.contentassist.OntologyContextProvider
import com.ge.research.sadl.ui.tests.SADLUiInjectorProvider
import com.ge.research.sadl.ui.tests.contentassist.AbstractSadlContentAssistTest.SadlContentAssistProcessorTestBuilder
import com.google.common.collect.ImmutableMap
import com.google.inject.Inject
import com.google.inject.Injector
import java.io.InputStream
import java.util.Arrays
import org.eclipse.core.runtime.Platform
import org.eclipse.emf.common.util.URI
import org.eclipse.jface.text.contentassist.ICompletionProposal
import org.eclipse.swt.widgets.Shell
import org.eclipse.xtext.junit4.ui.ContentAssistProcessorTestBuilder
import org.eclipse.xtext.junit4.util.ResourceLoadHelper
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.ui.XtextProjectHelper
import org.eclipse.xtext.ui.editor.XtextSourceViewerConfiguration
import org.eclipse.xtext.ui.editor.contentassist.XtextContentAssistProcessor
import org.eclipse.xtext.ui.resource.IResourceSetProvider
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.validation.CheckMode
import org.eclipse.xtext.validation.IResourceValidator
import org.junit.AfterClass
import org.junit.Assert
import org.junit.Before
import org.junit.BeforeClass
import org.junit.Rule
import org.junit.rules.TestName
import org.junit.runner.RunWith

import static org.eclipse.core.runtime.IPath.SEPARATOR
import static org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil.*
import static org.junit.Assert.*

/**
 * Base content assist plug-in test for SADL.
 * 
 * @author akos.kitta 
 */
@RunWith(XtextRunner)
@InjectWith(SADLUiInjectorProvider)
abstract class AbstractSadlContentAssistTest extends AbstractLinkingTest implements ResourceLoadHelper {

	static val PROJECT_NAME = 'testProject'

	static val RESOURCES = ImmutableMap.builder.put('Bar.sadl', '''uri "http://barUri". Bar is a class.''').put(
		'NotVisible.sadl', '''uri "http://notVisibleUri". NotVisible is a class.''').put('Shape.sadl', '''uri "http://shape". 
			Shape is a class described by area with values of type float.''').put('Circle.sadl', '''uri "http://circle". import "http://shape". 
			Circle is a type of Shape described by radius with values of type float.''').put('Rectangle.sadl', '''uri "http://rectangle". import "http://shape". 
			Rectangle is a type of Shape, described by height with values of type float, described by width with values of type float.''').
		build

	@Rule
	public val name = new TestName();

	@Inject
	Injector injector;

	@BeforeClass
	static def void assertRunningPlatform() {
		assertTrue('''
		These tests require a running Eclipse platform.
		Execute them as a JUnit Plug-in Test.
		If you see this error from Maven, then please configure your POM to use Tycho Surefire correctly for test execution.''',
			Platform.isRunning);
	}

	@BeforeClass
	def static void initWorkspace() {
		// Make sure console is redirected for the tests.
		OutputStreamStrategy.STD.use;
		val project = createProject(PROJECT_NAME);
		addNature(project, XtextProjectHelper.NATURE_ID);
		addBuilder(project, XtextProjectHelper.BUILDER_ID);
		RESOURCES.forEach[fileName, content|createFile('''«PROJECT_NAME»«SEPARATOR»«fileName»''', content)];
		waitForBuild;
	}

	@AfterClass
	def static void cleanWorkspace() {
		projects.forEach[delete(true, monitor)];
		waitForBuild;
		assertTrue('''Expected empty workspace. Workspace content was: «Arrays.toString(projects)».''', projects.empty);
	}

	@Before
	def void before() {
		OutputStreamStrategy.STD.use;
	}

	@Override
	override getResourceFor(InputStream stream) {
		val uri = URI.createURI('''platform:/resource/«PROJECT_NAME»/«name.methodName».sadl''');
		val resource = resourceSet.createResource(uri) as XtextResource;
		try {
			resource.load(stream, null);
			// XXX akitta: this is bad, but the same is happening in the editor.
			// Validation runs, validator recursively resolves and validates imported 
			// resources. During the validation, the ontology model is attached to all
			// other imported resources.
			val validator = resource.resourceServiceProvider.get(IResourceValidator);
			validator.validate(resource, CheckMode.ALL, CancelIndicator.NullImpl);
		} catch (Exception e) {
			Exceptions.sneakyThrow(e);
		}
		return resource;
	}

	protected static def assertProposalIsNot(ContentAssistProcessorTestBuilder builder, String missing) {
		try {
			builder.assertProposal(missing)
			Assert.fail('''The proposal '«missing»' expected to be not present. But it was.''');
		} catch (AssertionError e) {
			// Tricky, but this is the correct way since we have negated the assertion.	
		}
	}

	protected def newBuilder(String content) {
		val builder = new SadlContentAssistProcessorTestBuilder(injector, this) {

			@Override
			override protected toString(ICompletionProposal proposal) {
				return super.toString(proposal).trim;
			}

		};
		return builder.append(content) as SadlContentAssistProcessorTestBuilder;
	}

	protected def <T> get(Class<T> clazz) {
		return injector.getInstance(clazz);
	}

	private def getResourceSet() {
		val resourceSetProvider = injector.getInstance(IResourceSetProvider);
		val resourceSet = resourceSetProvider.get(project);
		assertNotNull('''Resource set was null for project: «project».''', resourceSet);
		return resourceSet;
	}

	private static def getProject() {
		val project = root.getProject(PROJECT_NAME);
		assertTrue('''Project '«project»' is not accessible.''', project.accessible);
		return project;
	}

	private static def getProjects() {
		return root.projects;
	}

	package static class SadlContentAssistProcessorTestBuilder extends ContentAssistProcessorTestBuilder {

		new(Injector injector, ResourceLoadHelper helper) throws Exception {
			super(injector, helper)
		}

		def getOntologyContext() {
			val document = getDocument(fullTextToBeParsed);
			val Shell shell = new Shell();
			try {
				val configuration = get(XtextSourceViewerConfiguration);
				val sourceViewer = getSourceViewer(shell, document, configuration);
				val contentAssist = configuration.getContentAssistant(sourceViewer);
				val contentType = document.getContentType(cursorPosition);
				val processor = contentAssist.getContentAssistProcessor(contentType);
				if (processor instanceof XtextContentAssistProcessor) {
					val contexts = document.readOnly([
						processor.contextFactory.create(sourceViewer, document.length, it)
					]);
					for (context : contexts) {
						val ontologyContext = get(OntologyContextProvider).getOntologyContext(context);
						if (ontologyContext.present) {
							return ontologyContext.get;
						}
					}
				}
				return null;
			} finally {
				shell.dispose;
			}
		}

	}

}
