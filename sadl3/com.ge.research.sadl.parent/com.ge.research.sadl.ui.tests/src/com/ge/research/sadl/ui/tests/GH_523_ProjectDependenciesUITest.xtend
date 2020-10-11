package com.ge.research.sadl.ui.tests

import com.ge.research.sadl.utils.SadlProjectHelper
import com.google.inject.Inject
import java.net.URI
import java.util.UUID
import org.eclipse.core.resources.IResource
import org.junit.Test

class GH_523_ProjectDependenciesUITest extends AbstractSadlPlatformTest {

	@Inject
	SadlProjectHelper testMe

	@Test
	def void testCanReadEmpty() {
		val file = createFile('Foo.sadl', 'uri "http://sadl.org/Foo.sadl".')
		assertEquals(0, testMe.getReferencedProjectURIs(file.toUri).length)
		assertEquals(0, testMe.getReferencedProjectURIs(file.project.toUri).length)
		assertEquals(0, testMe.getReferencedProjectURIs(file.resource).length)
	}

	@Test
	def void testOneProject() {
		val otherProjectName = '''sadl--gh-523--test-other-project-«UUID.randomUUID»'''
		createProject(otherProjectName)
		val otherProject = projects.findFirst[name == otherProjectName]
		assertTrue(otherProject.accessible)

		val copy = project.description
		copy.referencedProjects = #[otherProject]
		project.setDescription(copy, null)

		val file = createFile('Foo.sadl', 'uri "http://sadl.org/Foo.sadl".')
		assertEquals(1, testMe.getReferencedProjectURIs(file.toUri).length)
		assertEquals(1, testMe.getReferencedProjectURIs(file.project.toUri).length)
		assertEquals(1, testMe.getReferencedProjectURIs(file.resource).length)
	}

	@Test
	def void testMultiProject() {
		val projectOneName = '''sadl--gh-523--test-project-one-«UUID.randomUUID»'''
		createProject(projectOneName)
		val projectOne = projects.findFirst[name == projectOneName]
		assertTrue(projectOne.accessible)

		val projectTwoName = '''sadl--gh-523--test-project-two-«UUID.randomUUID»'''
		createProject(projectTwoName)
		val projectTwo = projects.findFirst[name == projectTwoName]
		assertTrue(projectTwo.accessible)

		val copy = project.description
		copy.referencedProjects = #[projectOne, projectTwo]
		project.setDescription(copy, null)

		val file = createFile('Foo.sadl', 'uri "http://sadl.org/Foo.sadl".')
		assertEquals(2, testMe.getReferencedProjectURIs(file.toUri).length)
		assertEquals(2, testMe.getReferencedProjectURIs(file.project.toUri).length)
		assertEquals(2, testMe.getReferencedProjectURIs(file.resource).length)
		assertEquals(#[projectOne.locationURI, projectTwo.locationURI].sort,
			testMe.getReferencedProjectURIs(file.resource).sort)
	}

	@Test
	def void testMultiProjectWithMissing() {
		val projectOneName = '''sadl--gh-523--test-project-one-«UUID.randomUUID»'''
		createProject(projectOneName)
		val projectOne = projects.findFirst[name == projectOneName]
		assertTrue(projectOne.accessible)

		val projectTwoName = '''sadl--gh-523--test-project-two-«UUID.randomUUID»'''
		createProject(projectTwoName)
		val projectTwo = projects.findFirst[name == projectTwoName]
		assertTrue(projectTwo.accessible)

		val copy = project.description
		copy.referencedProjects = #[projectOne, projectTwo]
		project.setDescription(copy, null)

		projectOne.delete(true, null)
		assertFalse(projectOne.accessible)

		val file = createFile('Foo.sadl', 'uri "http://sadl.org/Foo.sadl".')
		assertEquals(1, testMe.getReferencedProjectURIs(file.toUri).length)
		assertEquals(1, testMe.getReferencedProjectURIs(file.project.toUri).length)
		assertEquals(1, testMe.getReferencedProjectURIs(file.resource).length)
		assertEquals(#[projectTwo.locationURI], testMe.getReferencedProjectURIs(file.resource))
	}

	protected def URI toUri(IResource resource) {
		return testMe.toUri(resource.location.toFile.toPath)
	}

}
