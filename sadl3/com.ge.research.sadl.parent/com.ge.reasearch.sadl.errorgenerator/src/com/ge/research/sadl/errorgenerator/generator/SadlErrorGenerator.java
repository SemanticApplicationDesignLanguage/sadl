package com.ge.research.sadl.errorgenerator.generator;

import java.io.IOException;

import com.ge.research.sadl.errorgeneratorbase.ErrorGeneratorBase;

/**
 * 
 * Class used to generate the list of possible errors associated with sadl 
 * repository files
 * <p>
 * Run this file as a Java application to call the main() function to generate 
 * the list of parameterized error strings (SadlErrorMessages.java) and the 
 * html page listing all implemented error messages(html/errorMessages.html)
 * <p>
 * <i> Right-click SadlErrorGenerator.java file -> Run As -> Java Application </i>
 * 
 * @author Brett VanDam
 * @author Tyler Dicks
 */
public class SadlErrorGenerator extends ErrorGeneratorBase{
	private final static String PROPERTIES_FILE = "sadlMessages";
	private final static String HTML_FILE_PATH = "html/SadlErrorMessages.html";
	private final static String SOURCE_FILE_PATH = "src/com/ge/research/sadl/errorgenerator/generator/SadlErrorMessages.java";

	@Override
	public
	String getProjectPrefix() {
		return SADL_PREFIX;
	}

	@Override
	public
	String getPropertiesFileName() {
		return PROPERTIES_FILE;
	}

	@Override
	public
	String getDocumentationFilePath() {
		return HTML_FILE_PATH;
	}

	@Override
	public
	String getSourceFilePath() {
		return SOURCE_FILE_PATH;
	}
	
	@Override
	public
	String getSourceTemplate() {
		return String.join("\n",
				"$warning-comment",
				"package com.ge.research.sadl.errorgenerator.generator;",
				"",
				"import com.ge.research.sadl.errorgenerator.messages.SadlErrorMessage;",
				"",
				"public final class $classname {",
				"$content",
				"}");
	}
	
	public static void main(String args[]) throws IllegalArgumentException, IllegalAccessException, IOException {
		String dir = System.getProperty("user.dir");
		(new SadlErrorGenerator()).generate(dir);
	}
}