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

import com.ge.research.sadl.model.gp.Query
import com.ge.research.sadl.model.gp.SadlCommand
import com.ge.research.sadl.model.gp.Test
import com.ge.research.sadl.model.gp.TestResult
import com.ge.research.sadl.processing.SadlInferenceProcessorProvider
import com.ge.research.sadl.reasoner.ResultSet
import com.ge.research.sadl.reasoner.SadlCommandResult
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
		val results = processor.runInference(resource, modelPath.toString, modelFolderPath.toString, preferences).filter(SadlCommandResult);

		val builder = ImmutableList.builder;
		results.forEach[r | 
			if (r.results instanceof TestResult) {
				builder.add(doc.createInferenceResult(r.cmd as Test, r.results as TestResult));
			} else if (r.results instanceof ResultSet) {
				builder.add(doc.createInferenceResult(r.cmd as Query, r.results as ResultSet));
			} else {
				new UnsupportedOperationException('''Unhandled inference result: «r.results». SADL Command was: «r.cmd».''').printStackTrace
			}
		];

		return builder.build;
	}

	private def createInferenceResult(Document doc, Query query, ResultSet resultSet) {
		new InferenceResult => [
			status = Passed;
			value = '''Query: «query»«'\n'»«resultSet.toStringWithIndent(5)»'''.trim;
			range = getRange(doc, query);
		];
	}

	private def createInferenceResult(Document doc, Test test, TestResult testResult) {
		val passed = testResult.passed;
		new InferenceResult => [
			status = if (passed) Passed else Failed;
			value = '''Test «IF passed»passed«ELSE»failed«ENDIF»: «test»«IF !passed» («testResult»)«ENDIF»'''.trim;
			range = getRange(doc, test);
		];
	}

	private def getRange(Document doc, SadlCommand command) {
		val start = command.offset;
		val end = start + command.length;
		new Range(doc.getPosition(start), doc.getPosition(end));
	}
	
	private def trim(CharSequence s) {
		return if (s instanceof String) s.trim else s?.toString.trim;
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
		val projectPath = dotProjectPath.parent;
		return Preconditions.checkNotNull(projectPath, '''Cannot locate conatiner project path for «it».''');
	}

}
