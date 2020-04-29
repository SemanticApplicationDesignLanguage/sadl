package com.ge.research.sadl.ui.tests

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.ui.internal.SadlActivator
import com.google.common.base.Charsets
import com.google.common.io.Files
import com.google.inject.Inject
import com.google.inject.Provider
import java.io.File
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.ltk.core.refactoring.Change
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.XtextProjectHelper
import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.xtext.ui.refactoring.impl.RenameElementProcessor
import org.eclipse.xtext.ui.refactoring.ui.IRenameElementContext
import org.eclipse.xtext.ui.resource.IResourceSetProvider
import org.eclipse.xtext.ui.testing.AbstractEditorTest
import org.eclipse.xtext.ui.testing.util.IResourcesSetupUtil
import org.eclipse.xtext.util.concurrent.IUnitOfWork
import org.junit.Test

import static com.ge.research.sadl.sADL.SADLPackage.Literals.SADL_RESOURCE
import static org.eclipse.xtext.ui.testing.util.IResourcesSetupUtil.*

class SadlEditorTest extends AbstractEditorTest {

	@Inject
	Provider<RenameElementProcessor> processorProvider

	@Inject
	IResourceSetProvider resourceSetProvider

	@Inject
	extension DeclarationExtensions

	static final String TEST_PROJECT = "sadl.refactoring.test"
	static final String BASE_FILE_NAME = TEST_PROJECT + '/base.sadl'
	static final String SUB_FILE1_NAME = TEST_PROJECT + '/sub_1.sadl'
	static final String SUB_FILE2_NAME = TEST_PROJECT + '/sub_2.sadl'

	String baseModel
	String subModel1
	String subModel2
	IFile baseFile
	IFile subFile1
	IFile subFile2

	URI sadlResourceUri
	IProject project
	Change change

	override protected getEditorId() {
		return SadlActivator.COM_GE_RESEARCH_SADL_SADL
	}

	override void setUp() throws Exception {
		super.setUp()
		project = createProject(TEST_PROJECT)
		addNature(project, XtextProjectHelper.NATURE_ID)
		val injector = SadlActivator.getInstance().getInjector(getEditorId())
		injector.injectMembers(this)
		baseModel = 'uri "http://sadl.org/base.sadl". Base is a class.'
		baseFile = createFile(BASE_FILE_NAME, baseModel)
		subModel1 = 'uri "http://sadl.org/sub_1.sadl". import "http://sadl.org/base.sadl". MySub1 is a Base.'
		subFile1 = createFile(SUB_FILE1_NAME, subModel1)
		subModel2 = 'uri "http://sadl.org/sub_2.sadl". import "http://sadl.org/base.sadl". MySub2 is a Base.'
		subFile2 = createFile(SUB_FILE2_NAME, subModel2)
		val resourceSet = resourceSetProvider.get(project)
		val resourceUri = URI.createPlatformResourceURI(baseFile.getFullPath().toString(), true);
		val baseResource = resourceSet.getResource(resourceUri, true)
		val baseSadlResource = baseResource.allContents.filter(SadlResource).findFirst[concreteName == 'Base']
		sadlResourceUri = EcoreUtil.getURI((baseSadlResource as SadlResource).declaration)
	}

	@Test
	def void testRename_allOpened() throws Exception {
		val editorBase = openEditor(baseFile)
		val editorSub1 = openEditor(subFile1)
		val editorSub2 = openEditor(subFile2)
		doRename('NewBase')
		assertEquals(baseModel.replaceAll('Base', 'NewBase'), editorBase.getDocument().get())
		assertEquals(subModel1.replaceAll('Base', 'NewBase'), editorSub1.getDocument().get())
		assertEquals(subModel2.replaceAll('Base', 'NewBase'), editorSub2.getDocument().get())
		undoRename()
		assertEquals(baseModel, editorBase.getDocument().get())
		assertEquals(subModel1, editorSub1.getDocument().get())
		assertEquals(subModel2, editorSub2.getDocument().get())
	}

	protected def void doRename(String newName) throws Exception {
		IResourcesSetupUtil.waitForBuild()
		val change = createChange(sadlResourceUri, newName)
		new DelagatingWorkspaceModifyOperation([this.change = change.perform(it)]).run(null)
	}

	protected def void undoRename() throws Exception {
		IResourcesSetupUtil.waitForBuild()
		new DelagatingWorkspaceModifyOperation([this.change.perform(it)]).run(null)
	}

	protected def Change createChange(URI targetElementURI, String newName) throws Exception {
		val processor = processorProvider.get()
		processor.initialize(new IRenameElementContext.Impl(targetElementURI, SADL_RESOURCE, null, null, null))
		processor.setNewName(newName)
		val initialStatus = processor.checkInitialConditions(new NullProgressMonitor())
		assertTrue('Initial RefactoringStatus is OK', initialStatus.isOK())
		val finalStatus = processor.checkFinalConditions(new NullProgressMonitor(), null)
		assertTrue('Final RefactoringStatus is OK', finalStatus.isOK())
		val change = processor.createChange(new NullProgressMonitor())
		assertNotNull('RenameElementProcessor created changes', change)
		return change
	}

	protected def String readFile(IFile file) throws Exception {
		return Files.toString(new File(file.locationURI), Charsets.UTF_8)
	}

	protected def void waitForReconciler(XtextEditor editor) {
		editor.getDocument().readOnly(new IUnitOfWork.Void<XtextResource>() {
			override void process(XtextResource state) throws Exception {
				// just wait for the reconciler to announce the dirty state
			}
		})
	}

	protected def sadlResourceUri(XtextEditor editor, String sadlResourceName) {
		val sadlResource = editor.document.
			readOnly([allContents.filter(SadlResource).findFirst[concreteName == 'Base']])
		if (sadlResource !== null) {
			return EcoreUtil.getURI(sadlResource)
		}
		return null
	}

}
