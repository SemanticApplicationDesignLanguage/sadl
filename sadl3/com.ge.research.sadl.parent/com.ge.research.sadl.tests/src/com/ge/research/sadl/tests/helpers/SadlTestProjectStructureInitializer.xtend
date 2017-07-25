/************************************************************************
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
package com.ge.research.sadl.tests.helpers

import com.ge.research.sadl.ide.DotProjectContentProvider
import com.ge.research.sadl.processing.ISadlImplicitModelContentProvider
import com.google.inject.Inject
import com.google.inject.Singleton
import java.io.FileWriter
import java.nio.file.Files
import java.nio.file.Path

import static com.ge.research.sadl.jena.UtilsForJena.*
import static com.ge.research.sadl.processing.SadlConstants.*

import static extension com.google.common.base.Preconditions.*

/**
 * Singleton class for initializing the SADL project structure.
 * 
 * @author akos.kitta
 */
@Singleton
class SadlTestProjectStructureInitializer {

	/**
	 * The file name of the {@code .project} file that is available in the project root.
	 */
	public static val DOT_PROJECT_FILENAME = '.project';
	
	static val SHAPES_FILENAME = 'Shapes.sadl';
	static val CIRCLE_FILENAME = 'Circle.sadl';
	static val RECTANGLE_FILENAME = 'Rectangle.sadl';
	static val TEST_FILENAME = 'Test.sadl';

	static val SHAPES_FILE_CONTENT = '''
	uri "http://sadl.org/«SHAPES_FILENAME»" alias Shapes.
	
	Shape is a class described by area with values of type float.''';
	
	static val CIRCLE_FILE_CONTENT = '''
	uri "http://sadl.org/«CIRCLE_FILENAME»" alias Circle.
	
	import "http://sadl.org/«SHAPES_FILENAME»".
	
	Circle is a type of Shape described by radius with values of type float.
	
	Rule Area: if x is a Circle then area of x is radius of x ^2 * PI.''';
	
	static val RECTANGLE_FILE_CONTENT = '''
	uri "http://sadl.org/«RECTANGLE_FILENAME»" alias Rectangle.
	
	import "http://sadl.org/«SHAPES_FILENAME»".
	
	Rectangle is a type of Shape,
		described by height with values of type float,
		described by width with values of type float.
	
	Rule AreaOfRect: if x is a Rectangle then area of x is height of x * width of x.''';
	
	static val TEST_FILE_CONTENT = '''
	uri "http://sadl.org/«TEST_FILENAME»".
	
	import "http://sadl.org/«CIRCLE_FILENAME»".
	import "http://sadl.org/Rectangle.sadl".
	
	MyCircle is a Circle with radius 4.5.
	MyRect is a Rectangle with height 2.5, with width 5.5.
	
	Ask: "select ?sh ?ar where {?sh <area> ?ar}".
	
	Test: area of MyCircle is 63.61.
	
	Test: area of MyCircle is 53.51.''';

	@Inject
	DotProjectContentProvider dotProjectContentProvider;
	
	@Inject
	ISadlImplicitModelContentProvider modelContentProvider;
	

	/**
	 * Initializes the SADL project structure on demand.
	 * 
	 * @param projectRoot the path to the project root.
	 */
	def void initialize(Path projectRoot) {
		// This is just a hack to be able to locate the project root and 
		// the implicit model folder in a web project as well. 
		projectRoot.createDotProjectFileIfMissing;

		projectRoot.createShapesFileIfMissing;
		projectRoot.createCircleFileIfMissing;
		projectRoot.createRectangleFileIfMissing;
		projectRoot.createTestFileIfMissing;

		val implicitModelRoot = projectRoot.resolve(SADL_IMPLICIT_MODEL_FOLDER);
		if (!implicitModelRoot.toFile.exists) {
			Files.createDirectory(implicitModelRoot);
		}
		implicitModelRoot.createImplicitModelFileIfMissing;

		val owlModelsRoot = projectRoot.resolve(OWL_MODELS_FOLDER_NAME);
		if (!owlModelsRoot.toFile.exists) {
			Files.createDirectories(owlModelsRoot);
		}
	}

	private def void createDotProjectFileIfMissing(Path projectRoot) {
		val projectName = projectRoot.toFile.name;
		projectRoot.createFileIfMissing(DOT_PROJECT_FILENAME, dotProjectContentProvider.getContent(projectName));
	}
	
	private def void createShapesFileIfMissing(Path projectRoot) {
		projectRoot.createFileIfMissing(SHAPES_FILENAME, SHAPES_FILE_CONTENT);
	}
	
	private def void createCircleFileIfMissing(Path projectRoot) {
		projectRoot.createFileIfMissing(CIRCLE_FILENAME, CIRCLE_FILE_CONTENT);
	}

	private def void createRectangleFileIfMissing(Path projectRoot) {
		projectRoot.createFileIfMissing(RECTANGLE_FILENAME, RECTANGLE_FILE_CONTENT);
	}
	
	private def void createTestFileIfMissing(Path projectRoot) {
		projectRoot.createFileIfMissing(TEST_FILENAME, TEST_FILE_CONTENT);	
	}

	private def createImplicitModelFileIfMissing(Path implicitModelRoot) {
		implicitModelRoot.createFileIfMissing(SADL_IMPLICIT_MODEL_FILENAME, modelContentProvider.content);
	}

	private def createFileIfMissing(Path rootFolder, String fileName, String content) {
		rootFolder.toFile.directory.
			checkArgument('''Cannot find root directory for file «fileName» under: «rootFolder».''');
		val filePath = rootFolder.resolve(fileName);
		val file = filePath.toFile;
		if (!file.exists) {
			Files.createFile(filePath);
			val writer = new FileWriter(file);
			writer.write(content);
			writer.flush();
			writer.close();
		}
	}

}
