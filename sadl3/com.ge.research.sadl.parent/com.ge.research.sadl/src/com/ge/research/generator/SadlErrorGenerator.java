package com.ge.research.generator;

import java.io.IOException;

public class SadlErrorGenerator extends ErrorGeneratorBase{
	private final static String PROPERTIES_FILE = "sadlMessages";
	private final static String HTML_FILE_PATH = "html/errormessages.html";
	private final static String SOURCE_FILE_PATH = "src/com/ge/research/documentation/SadlErrorMessages.java";

	@Override
	String getProjectPrefix() {
		// TODO Auto-generated method stub
		return SADL_PREFIX;
	}

	@Override
	String getPropertiesFileName() {
		// TODO Auto-generated method stub
		return PROPERTIES_FILE;
	}

	@Override
	String getDocumentationFilePath() {
		// TODO Auto-generated method stub
		return HTML_FILE_PATH;
	}

	@Override
	String getSourceFilePath() {
		// TODO Auto-generated method stub
		return SOURCE_FILE_PATH;
	}

	public static void main(String[] args) throws IllegalArgumentException, IllegalAccessException, IOException {
		(new SadlErrorGenerator()).generate();
	}
}
