package com.ge.research.sadl.ui.tests.contentassist

import com.ge.research.sadl.tests.AbstractLinkingTest
import com.ge.research.sadl.ui.tests.SADLUiInjectorProvider
import com.google.common.collect.ImmutableMap
import com.google.inject.Inject
import com.google.inject.Injector
import java.io.InputStream
import java.util.Arrays
import org.eclipse.core.runtime.Platform
import org.eclipse.emf.common.util.URI
import org.eclipse.jface.text.contentassist.ICompletionProposal
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.ui.ContentAssistProcessorTestBuilder
import org.eclipse.xtext.junit4.util.ResourceLoadHelper
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.XtextProjectHelper
import org.eclipse.xtext.ui.resource.IResourceSetProvider
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import org.junit.runner.RunWith

import static org.eclipse.core.runtime.IPath.SEPARATOR
import static org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil.*
import static org.junit.Assert.*

@RunWith(XtextRunner)
@InjectWith(SADLUiInjectorProvider)
class SadlContentAssistTest extends AbstractLinkingTest implements ResourceLoadHelper {

	static val PROJECT_NAME = 'testProject';

	static val RESOURCES = ImmutableMap.builder
		.put('Bar.sadl', 'uri "http://barUri". Bar is a class.')
		.put('NotVisible.sadl', 'uri "http://notVisibleUri". NotVisible is a class.')
		.build;

	@Inject
	Injector injector;

	@BeforeClass
	static def void assertRunningPlatform() {
		assertTrue('These tests require a running Eclipse platform.
			Execute them as a JUnit Plug-in Test.
			If you see this error from Maven, then please configure your POM to use Tycho Surefire correctly for test execution.',
			Platform.isRunning);
	}

	@Before
	def void initWorkspace() {
		val project = createProject(PROJECT_NAME);
		addNature(project, XtextProjectHelper.NATURE_ID);
		addBuilder(project, XtextProjectHelper.BUILDER_ID);
		RESOURCES.forEach[fileName, content|createFile('''«PROJECT_NAME»«SEPARATOR»«fileName»''', content)];
		waitForBuild;
	}

	@After
	def void cleanWorkspace() {
		projects.forEach[delete(true, monitor)];
		waitForBuild;
		assertTrue('''Expected empty workspace. Workspace content was: «Arrays.toString(projects)».''', projects.empty);
	}

	/** Primitive primary type reference. */
	@Test
	def void checkCA_01_Positive() {
		newBuilder('''uri "http://myUri". Foo is a class. myFoo is a ''').assertProposal('integer');
	}

	/** Primary type reference. */
	@Test
	def void checkCA_02_Positive() {
		newBuilder('''uri "http://myUri". Foo is a class. myFoo is a ''').assertProposal('Foo');
	}

	/** Imported primary type reference. */
	@Test
	def void checkCA_03_Positive() {
		newBuilder('''uri "http://myUri". import "http://barUri". Foo is a class. myFoo is a ''').assertProposal('Bar');
	}

	/** Self primary type reference with imports. */
	@Test
	def void checkCA_04_Positive() {
		newBuilder('''uri "http://myUri". import "http://barUri". Foo is a class. myFoo is a ''').assertProposal('Foo');
	}

	/** Not visible primary type reference with imports. */
	@Test
	def void checkCA_05_Negative() {
		newBuilder('''uri "http://myUri". import "http://barUri". Foo is a class. myFoo is a ''').
			assertProposalIsNot('NotVisible');
	}

	@Override
	override getResourceFor(InputStream stream) {
		val uri = URI.createURI('''platform:/resource/«PROJECT_NAME»/TestMe.sadl''');
		val resource = resourceSet.createResource(uri) as XtextResource;
		try {
			resource.load(stream, null);
		} catch (Exception e) {
			Exceptions.sneakyThrow(e);
		}
		return resource;
	}

	private def newBuilder() {
		return new ContentAssistProcessorTestBuilder(injector, this) {

			@Override
			override protected toString(ICompletionProposal proposal) {
				return super.toString(proposal).trim;
			}

		};
	}

	private def newBuilder(String content) {
		return newBuilder.append(content);
	}

	private def getResourceSet() {
		val resourceSetProvider = injector.getInstance(IResourceSetProvider);
		val resourceSet = resourceSetProvider.get(project);
		assertNotNull('''Resource set was null for project: «project».''', resourceSet);
		return resourceSet;
	}

	private def getProject() {
		val project = root.getProject(PROJECT_NAME);
		assertTrue('''Project '«project»' is not accessible.''', project.accessible);
		return project;
	}

	private def getProjects() {
		return root.projects;
	}

	private def assertProposalIsNot(ContentAssistProcessorTestBuilder builder, String missing) {
		try {
			builder.assertProposal(missing)
			Assert.fail('''The proposal '«missing»' expected to be not present. But it was.''');
		} catch (AssertionError e) {
			// Tricky, but this is the correct way since we have negated the assertion.	
		}
	}

}
