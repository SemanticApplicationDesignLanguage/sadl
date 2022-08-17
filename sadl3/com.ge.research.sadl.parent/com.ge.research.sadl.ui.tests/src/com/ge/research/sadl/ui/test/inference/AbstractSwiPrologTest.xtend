package com.ge.research.sadl.ui.test.inference

import com.ge.research.sadl.ui.tests.AbstractSadlPlatformTest
import java.io.IOException
import org.eclipse.xtext.ui.testing.util.IResourcesSetupUtil
import org.eclipse.xtext.ui.XtextProjectHelper
import java.io.File
import java.util.regex.Pattern
import java.nio.file.Paths
import java.util.stream.Stream
import static java.nio.file.Files.isExecutable;
import java.nio.file.Files
import com.ge.research.sadl.reasoner.utils.SadlUtils
import com.ge.research.sadl.jena.UtilsForJena

abstract class AbstractSwiPrologTest extends AbstractSadlPlatformTest {

static val CONFIG = '''
<rdf:RDF
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns="http://com.ge.research.sadl.configuration#">
  <Category rdf:about="http://com.ge.research.sadl.configuration#ReasonerSpec">
    <reasonerClassName>com.ge.research.sadl.swi_prolog.reasoner.SWIPrologReasonerPlugin</reasonerClassName>
  </Category>
  <rdf:Description rdf:about="http://com.ge.research.sadl.configuration#SWI-Prolog-Reasoner">
    <translatorClassName>com.ge.research.sadl.swi_prolog.translator.SWIPrologTranslatorPlugin</translatorClassName>
  </rdf:Description>
</rdf:RDF>
	'''

	override void createProject(String projectName) {
		val project = IResourcesSetupUtil.createProject(projectName);
		createFile('/OwlModels/configuration.rdf', CONFIG)
		IResourcesSetupUtil.addNature(project, XtextProjectHelper.NATURE_ID);
		IResourcesSetupUtil.addBuilder(project, XtextProjectHelper.BUILDER_ID);
		IResourcesSetupUtil.waitForBuild;
		configurePreferences();
		// This is used to trigger the implicit model creation before the tests.
		val file = createFile(project.name, 'Dummy.sadl', 'uri "http://sadl.org/Dummy.sadl."');
		IResourcesSetupUtil.fullBuild();
		file.delete(true, IResourcesSetupUtil.monitor);
		IResourcesSetupUtil.fullBuild();
		assertTrue('Dummy file should not exist.', !file.exists);
	}
	
	def boolean canRunSwiProlog() {
		var ce = false;
		if (SadlUtils.isWindows()) {
			ce = SadlUtils.canExecute("swipl-win");
			System.out.println("OS: Windows, SWI-Prolog installed: " + ce);
		}
		else if (SadlUtils.isMac()) {
			ce = SadlUtils.canExecute("swipl");
			System.out.println("OS: Mac, SWI-Prolog installed: " + ce);
		}
		else if (SadlUtils.isUnix()) {
			ce = SadlUtils.canExecute("swipl");
			System.out.println("OS: unix, SWI-Prolog installed: " + ce);
		}
		return ce;
	}
	
	def String getPrologFileContent(String pfn) {
		val projectPath = Paths.get(project.locationURI);
		val modelFolderPath = projectPath.resolve(UtilsForJena.OWL_MODELS_FOLDER_NAME);
		val su = new SadlUtils
		val fn = su.fileUrlToFileName(modelFolderPath.toFile.absolutePath) + "/" + pfn
		val content = su.fileToString(new File(fn))
		return content
	}
}