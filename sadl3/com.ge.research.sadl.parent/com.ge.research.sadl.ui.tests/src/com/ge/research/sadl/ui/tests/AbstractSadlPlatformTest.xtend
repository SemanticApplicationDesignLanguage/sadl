/**
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
 */
package com.ge.research.sadl.ui.tests

import com.ge.research.sadl.jena.IJenaBasedModelProcessor
import com.ge.research.sadl.model.gp.Rule
import com.ge.research.sadl.model.gp.SadlCommand
import com.ge.research.sadl.tests.SadlTestAssertions
import com.ge.research.sadl.ui.OutputStreamStrategy
import com.google.common.collect.Lists
import com.google.inject.Inject
import com.hp.hpl.jena.ontology.OntModel
import java.util.Arrays
import java.util.List
import org.eclipse.core.resources.IFile
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.Platform
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.ui.actions.WorkspaceModifyOperation
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.preferences.PreferenceKey
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.ui.XtextProjectHelper
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess
import org.eclipse.xtext.ui.resource.IResourceSetProvider
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.util.StringInputStream
import org.eclipse.xtext.validation.CheckMode
import org.eclipse.xtext.validation.IResourceValidator
import org.eclipse.xtext.validation.Issue
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.BeforeClass
import org.junit.rules.TestName
import org.junit.runner.RunWith

import static org.eclipse.core.runtime.IPath.SEPARATOR
import static org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil.*
import com.google.common.collect.Iterables
import org.eclipse.xtext.diagnostics.Severity

/**
 * Base test class with a running Eclipse platform, with a workspace and a convenient way
 * to customize the preferences for the tests.
 * 
 * @author akos.kitta. 
 */
@RunWith(XtextRunner)
@InjectWith(SADLUiInjectorProvider)
abstract class AbstractSadlPlatformTest extends Assert {

	val modifiedPreferences = <String>newHashSet();

	@org.junit.Rule
	public TestName testName = new TestName;

	@Inject
	IResourceSetProvider resourceSetProvider;

	@Inject
	IPreferenceStoreAccess access;

	@Inject
	@Accessors(PROTECTED_GETTER)
	IPreferenceValuesProvider preferenceValuesProvider;

	@BeforeClass
	static def void assertRunningPlatform() {
		assertTrue('These tests require a running Eclipse platform.
			Execute them as a JUnit Plug-in Test.
			If you see this error from Maven, then please configure your POM to use Tycho Surefire correctly for test execution.',
			Platform.isRunning);
	}

	@Before
	def void before() {
		OutputStreamStrategy.STD.use; // To redirect error to the Eclipse console.
		deleteProjects();
		modifiedPreferences.clear();
		beforeProjectCreation();
		createProject();
	}

	@After
	def void after() {
		resetPreferences();
		beforeProjectDeletion();
		deleteProjects();
		afterProjectDeletion();
	}

	/**
	 * Creates the test project.
	 */
	protected def void createProject() {
		createProject(projectName);
		addNature(project, XtextProjectHelper.NATURE_ID);
		addBuilder(project, XtextProjectHelper.BUILDER_ID);
		waitForBuild;
		configurePreferences();
		// This is used to trigger the implicit model creation before the tests. 
		val file = createFile('Dummy.sadl', 'uri "http://sadl.org/Dummy.sadl."');
		fullBuild();
		file.delete(true, monitor);
		fullBuild();
		assertTrue('Dummy file should not exist.', !file.exists);
	}

	/**
	 * Hook to modify preferences before running the actual test case.
	 */
	protected def void configurePreferences() {
	}

	/**
	 * Called before creating the test project in the setup phase.
	 * Does nothing by default.
	 */
	protected def void beforeProjectCreation() {
	}

	/**
	 * Called after the test project creation. Clients could make sure
	 * that the workspace is fully built when calling this method. Does nothing
	 * by default.
	 */
	protected def void afterProjectCreation() {
	}

	/**
	 * Deletes all workspace projects.
	 */
	protected def void deleteProjects() {
		projects.forEach[delete(true, monitor)];
		waitForBuild();
		assertTrue('''Expected empty workspace. Workspace content was: «Arrays.toString(projects)».''', projects.empty);
	}

	/**
	 * Called before deleting the workspace content. Does nothing by default, clients may override.
	 */
	protected def void beforeProjectDeletion() {
	}

	/**
	 * Called when the workspace content has been cleaned up in the test tear-down phase. 
	 * Does nothing by default.
	 */
	protected def void afterProjectDeletion() {
	}

	/**
	 * Resets any modified preferences to the default.
	 */
	protected def void resetPreferences() {
		modifiedPreferences.forEach [
			access.getWritablePreferenceStore(project).setToDefault(it);
		];
	}

	/**
	 * Returns with the test project. Makes sure, that the project exists and accessible.
	 */
	protected def getProject() {
		val projectName = projectName;
		val project = root.projects.findFirst[name == projectName];
		assertNotNull('''Cannot find '<<projectName>>' project in the workspace.''', project);
		assertTrue('''Coonnot access '<<projectName>>' project in the workspace.''', project.accessible);
		return project;
	}

	/**
	 * Returns with all workspace projects.
	 */
	protected def getProjects() {
		return root.projects;
	}

	/**
	 * Creates a file with the given file name and content. 
	 */
	protected def createFile(String fileName, String content) {
		val file = IResourcesSetupUtil.createFile('''«projectName»«SEPARATOR»«fileName»''', content);
		assertNotNull('''Cannot create '«fileName»' file in the workspace.''', file);
		assertTrue('''Cannot access '«fileName»' file project in the workspace.''', file.accessible);
		waitForBuild();
		return file;
	}

	/**
	 * Set the content of an existing file. If the file does not yet exist, creates it.
	 */
	protected def setFileContent(String fileName, String content) {
		val file = project.getFile(fileName);
		if (!file.accessible) {
			return createFile(fileName, content);
		}
		new WorkspaceModifyOperation() {

			override protected execute(IProgressMonitor monitor) {
				file.setContents(new StringInputStream(content), true, false, monitor);
			}

		}.run(new NullProgressMonitor());
		waitForBuild();
	}

	/**
	 * Returns with the EMF resource for the given workspace file.
	 */
	protected def getResource(IFile file) {
		assertNotNull('File cannot be null.', file);
		assertTrue('''Cannot access '«file»' file project in the workspace.''', file.accessible);
		assertTrue('''Cannot access '«file.project»' project in the workspace.''', file.project.accessible);
		val uri = URI.createPlatformResourceURI(file.fullPath.toString, true);
		return resourceSetProvider.get(file.project).getResource(uri, true);
	}

	/**
	 * Updates the default preferences with the user provided preference keys.
	 */
	protected def updatePreferences(PreferenceKey key, PreferenceKey... rest) {
		updatePreferences(Lists.asList(key, rest));
	}

	/**
	 * Sugar for updating multiple preference values.
	 */
	protected def updatePreferences(Iterable<PreferenceKey> keys) {
		if (!keys.nullOrEmpty) {
			val store = access.getWritablePreferenceStore(project);
			keys.forEach [
				store.setValue(id, defaultValue);
				modifiedPreferences.add(id);
			];
		}
	}

	/**
	 * Validates the resource argument.
	 */
	protected def validate(Resource it) {
		if (it instanceof XtextResource) {
			return resourceServiceProvider.get(IResourceValidator).validate(it, CheckMode.FAST_ONLY, cancelIndicator);
		} else {
			return emptyList;
		}
	}
	
	/**
	 * Validates the resource, asserts the issues.
	 */
	protected def Resource assertValidatesTo(Resource resource,
		(OntModel, List<Rule>, List<SadlCommand>, List<Issue>, IJenaBasedModelProcessor)=>void assertions) {

		return SadlTestAssertions.assertValidatesTo(resource as XtextResource, assertions);
	}

	/**
	 * Returns with the cancel indicator.
	 */
	protected def cancelIndicator() {
		return CancelIndicator.NullImpl;
	}

	/**
	 * Returns with the name of the project being tested. This differs per test case.
	 */
	protected def getProjectName() {
		return '''«class.simpleName»_«testName.methodName»''';
	}

	/**
	 * Asserts no validation issues.
	 */
	static def void assertHasNoIssues(Iterable<? extends Issue> issues) {
		doAssertHasIssues(issues, [true], 0);
	}

	private static def void doAssertHasIssues(Iterable<? extends Issue> issues, (Severity)=>boolean severityPredicate,
		int expectedCount) {
		val actualIssues = issues.filter[severityPredicate.apply(severity)];
		Assert.assertEquals(
			'''Expected «expectedCount» issues. Got «actualIssues.size» instead. [«Iterables.toString(actualIssues)»]''',
			expectedCount,
			actualIssues.size
		);
	}

}
