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
package com.ge.research.sadl.ide.lsp.inference

import com.ge.research.sadl.model.gp.Test
import com.ge.research.sadl.model.gp.TestResult
import com.ge.research.sadl.processing.SadlInferenceProcessorProvider
import com.google.common.base.Preconditions
import com.google.common.collect.ImmutableList
import com.google.inject.Inject
import com.google.inject.Singleton
import java.nio.file.Path
import java.nio.file.Paths
import java.util.List
import java.util.Map
import org.eclipse.emf.common.util.URI
import org.eclipse.lsp4j.Range
import org.eclipse.xtext.ide.server.Document
import org.eclipse.xtext.resource.XtextResource

import static com.ge.research.sadl.ide.SadlProjectStructureInitializer.*
import static com.ge.research.sadl.ide.lsp.inference.InferenceStatus.*
import static com.ge.research.sadl.jena.UtilsForJena.*

/**
 * Headless implementation of the {@code SADL} inferencer.
 * 
 * @author akos.kitta
 */
@Singleton
class SadlInferencer {

	val static URI_PREFIX = 'file:///';

	@Inject
	SadlInferenceProcessorProvider processorProvider;

	def List<? extends InferenceResult> runTests(XtextResource resource, Document doc,
		Map<String, String> preferences) {

		val currentPath = resource.URI.toPath;
		val projectPath = currentPath.locateProjectPath;
		val modelFolderPath = projectPath.resolve(OWL_MODELS_FOLDER_NAME);
		val modelPath = modelFolderPath.resolve('''«resource.URI.trimFileExtension.lastSegment».owl''');
		val processor = processorProvider.getProcessor(resource);
		val result = processor.runInference(resource, modelPath.toString, modelFolderPath.toString, preferences);
		val commands = result.get(0);
		val testResults = result.get(1);

		val builder = ImmutableList.builder;
		if (commands instanceof List<?>) {
			Preconditions.checkState(
				testResults instanceof List<?>, '''Expected a list of inference results. Was: «testResults».''');
			for (var i = 0; i < commands.length; i++) {
				val command = commands.get(i);
				if (command instanceof Test) {
					val testResult = (testResults as List<?>).get(i);
					Preconditions.checkState(
						testResult instanceof TestResult, '''Expected a test result. Was: «testResult».''');

					builder.add(doc.createInferenceResult(command, testResult as TestResult));
				}
			}
		}

		return builder.build;
	}

	private def createInferenceResult(Document doc, Test test, TestResult testResult) {
		new InferenceResult => [
			status = if (testResult.passed) Passed else Failed;
			value = testResult.toString;
			range = getRange(doc, test);
		];
	}

	private def getRange(Document doc, Test test) {
		val start = test.offset;
		val end = start + test.length;
		new Range(doc.getPosition(start), doc.getPosition(end));
	}

	private def toPath(URI it) {
		val uri = it.toString;
		Preconditions.checkState(uri.startsWith(URI_PREFIX), '''URI must start with «URI_PREFIX». URI was: «it».''');
		val path = Paths.get(uri.substring(URI_PREFIX.length - 1));
		Preconditions.checkState(path.toFile.exists, '''Resource does not exist under «path». URI was: «it».''');
		return path;
	}

	private def locateProjectPath(Path it) {
		var path = toAbsolutePath;
		var dotProjectPath = path.resolve(DOT_PROJECT_FILENAME);
		while (!dotProjectPath.toFile.exists) {
			path = path.parent;
			Preconditions.checkState(path !== null, '''Cannot locate conatiner project path for «it».''');
			dotProjectPath = path.resolve(DOT_PROJECT_FILENAME);
		}
		return dotProjectPath;
	}

}
