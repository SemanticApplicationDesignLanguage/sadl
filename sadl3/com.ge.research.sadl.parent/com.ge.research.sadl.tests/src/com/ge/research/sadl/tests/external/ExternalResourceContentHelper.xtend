package com.ge.research.sadl.tests.external

import org.eclipse.xtext.util.Files
import java.net.URL

/**
 * Helper class that reads the content of an external resource into a string.
 * <p>
 * This helper assumes that the resource are located under the {@code src/resources/} folder in this project.
 * 
 * @author akos.kitta
 */
final class ExternalResourceContentHelper {

	/**
	 * Should be added on the class-path.
	 */
	static val RESOURCES_FOLDER = 'resources';

	/**
	 * Gets the content of the file for the given file name.
	 * <p>
	 * No absolute paths are required, just the name of the file with the extension.
	 * If you are referencing a resource in a sub-folder, use {@code /folder/file.extension} construct.
	 */
	static def String getContent(String fileName) {
		return Files.readStreamIntoString(ExternalResourceContentHelper.getResourceAsStream('''/«RESOURCES_FOLDER»/«fileName»'''));
	}
	
	static def URL getURL(String fileName) {
		return ExternalResourceContentHelper.getResource('''/«RESOURCES_FOLDER»/«fileName»''')
	}

}