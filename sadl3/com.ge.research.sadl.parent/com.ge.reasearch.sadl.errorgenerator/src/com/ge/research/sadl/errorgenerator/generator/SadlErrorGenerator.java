package com.ge.research.sadl.errorgenerator.generator;

import java.io.IOException;
import java.util.Enumeration;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

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
		String[] sa = {"$warning-comment",
				"package com.ge.research.sadl.errorgenerator.generator;",
				"",
				"import com.ge.research.sadl.errorgenerator.messages.SadlErrorMessage;",
				"",
				"public final class $classname {",
				"$content",
				"}"};
		return strJoin("\n", sa);
//		return String.join("\n",
//				"$warning-comment",
//				"package com.ge.research.sadl.errorgenerator.generator;",
//				"",
//				"import com.ge.research.sadl.errorgenerator.messages.SadlErrorMessage;",
//				"",
//				"public final class $classname {",
//				"$content",
//				"}");
	}
	
	public static String getSadlErrorHtmlFile() {
		String prefix = "Sadl";
		
		String htmlString = getHtmlTemplate();//FileUtils.readWholeFileAsUTF8("templates/htmlTemplate.tmpl");
		htmlString = htmlString.replace("$title", prefix + " Error Messages");
		String tableString = "<table><tr><th>Error Message</th><th>Description</th></tr>";
		
		//Read the error messages from the .properties file
		ResourceBundle msgBundle = ResourceBundle.getBundle("sadlMessages");
		for(Enumeration<String> keys = msgBundle.getKeys(); keys.hasMoreElements();) {
			String key = keys.nextElement();
			if(!key.contains(".")){
				//Add content to HTML file
				String msg = msgBundle.getString(key);
				String description = "";
				try {
					description = msgBundle.getString(key + ".description");
				} catch(MissingResourceException e) {
					description = "<em>No description provided</em>";
				}
				tableString += "<tr><td class=\"absorbing-column\">" + msg + "</td><td>" + description + "</td></tr>";
			}
		}
		tableString += "</table>";
		htmlString = htmlString.replace("$body", tableString);
		
		return htmlString;
	}
	
	
	public static void main(String args[]) throws IllegalArgumentException, IllegalAccessException, IOException {
		String dir = System.getProperty("user.dir");
		(new SadlErrorGenerator()).generate(dir);
	}
}