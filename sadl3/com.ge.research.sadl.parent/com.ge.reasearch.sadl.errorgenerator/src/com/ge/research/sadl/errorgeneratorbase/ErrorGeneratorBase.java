package com.ge.research.sadl.errorgeneratorbase;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Enumeration;
import java.util.MissingResourceException;
import java.util.ResourceBundle;



/**
 * Error Generator Base class which auto-generates the 
 * Sadl/RequirementsErrorMessages.java files and associated
 * html files.
 * 
 * 
 * @author Brett VanDam
 * @author Tyler Dicks
 *
 */
public abstract class ErrorGeneratorBase {
//	private final static String warningComment = 
//			String.join("\n", 
//			"/**WARNING: This document is auto-generated. DO NOT manually edit this file!",
//			"*   ==Generated based on appropriate .properties file==",
//			"*",
//			"* To add new errors to this list, use the following process:",
//			"*",
//			"* 1. Add new error to associated sadlMessages/requirementsMessages.properties file",
//			"*      -File is found in either com.ge.research.sadl.errorgenerator/src or ",
//			"*       com.ge.research.sadl.requirements.errorgenerator/src",
//			"* 2. Select respective ErrorGenerator class (SadlErrorGenerator/RequirmentsErrorGenerator)",
//			"* 3. Right click -> Run As -> Java Application",
//			"*",
//			"* This process should generate a new Sadl/Req ErrorMessages class and updated html table",
//			"*",
//			"*/",
//			"");	
	private static String[] strArray =	{ 
			"/**WARNING: This document is auto-generated. DO NOT manually edit this file!",
			"*   ==Generated based on appropriate .properties file==",
			"*",
			"* To add new errors to this list, use the following process:",
			"*",
			"* 1. Add new error to associated sadlMessages/requirementsMessages.properties file",
			"*      -File is found in either com.ge.research.sadl.errorgenerator/src or ",
			"*       com.ge.research.sadl.requirements.errorgenerator/src",
			"* 2. Select respective ErrorGenerator class (SadlErrorGenerator/RequirmentsErrorGenerator)",
			"* 3. Right click -> Run As -> Java Application",
			"*",
			"* This process should generate a new Sadl/Req ErrorMessages class and updated html table",
			"*",
			"*/",
			"" };
	private final static String warningComment = strJoin("\n", strArray);

	private final static String indent = "    ";
	private final static String varDeclaration1 = "public static final ";
	private final static String varDeclaration2 = "ErrorMessage ";
	private final static String newErrorMsg1 = "new ";
	private final static String newErrorMsg2 = "ErrorMessage(";
	private final static String endErrorMsg = ");";
	private final static String newline = "\n";
	
	protected final static String REQUIREMENTS_PREFIX = "Requirements";
	protected final static String SADL_PREFIX = "Sadl";
	
	/**
	 * This method is used for Java 1.7 compatibility
	 * @param aSep
	 * @param aArray
	 * @return
	 */
	public static String strJoin(String aSep, String[] aArray) {
		StringBuilder sb = new StringBuilder();
		for (int i =0; i < aArray.length; i++) {
			if (i > 0) sb.append(aSep);
			sb.append(aArray[i]);
		}
		return sb.toString();
	}
	
	/**
	 * Must be implemented
	 * @return either REQUIREMENTS_PREFIX or SADL_PREFIX
	 */
	abstract public String getProjectPrefix();
	
	/**
	 * Defines what the properties file to parse is called
	 * @return the name of the properties file (WITHOUT ".properties")
	 */
	abstract public String getPropertiesFileName();
	
	/**
	 * Defines the location the HTML documentation will be written
	 * @return file path for HTML documentation, INCLUDING file name
	 */
	abstract public String getDocumentationFilePath();
	
	/**
	 * Defines the location the Java source will be written
	 * @return file path for Java source code, INCLUDING file name
	 */
	abstract public String getSourceFilePath();
	
	
	/**
	 * Defines the generated file package and imports
	 * @return string containing correct package and import statements
	 */
	abstract public String getSourceTemplate();
	
	
	public void generate(String projectBase) throws IllegalArgumentException, IllegalAccessException, IOException {
		System.out.println("--------Starting Source Code and Document Generation---------");
		
		String projectFileBase = projectBase.replace('\\', '/') + '/';
		String prefix = getProjectPrefix();
		
		//Code generation
		String sourceCodeString = getSourceTemplate();//FileUtils.readWholeFileAsUTF8("templates/ErrorMessages.tmpl");
		
		sourceCodeString = sourceCodeString.replace("$warning-comment", warningComment);
		sourceCodeString = sourceCodeString.replace("$classname", prefix + "ErrorMessages");
		
		String sourceContent = "";
		
		//Documentation generation
		String htmlString = getHtmlTemplate();//FileUtils.readWholeFileAsUTF8("templates/htmlTemplate.tmpl");
		htmlString = htmlString.replace("$title", prefix + " Error Messages");
		String tableString = "<table><tr><th>Error Message</th><th>Description</th></tr>";
		
		//Read the error messages from the .properties file
		ResourceBundle msgBundle = ResourceBundle.getBundle(getPropertiesFileName());
		for(Enumeration<String> keys = msgBundle.getKeys(); keys.hasMoreElements();) {
			String key = keys.nextElement();
			if(!key.contains(".")){
				//Add content to source file
				String msg = msgBundle.getString(key);
				String[] sa = {"/**",
						"* " + msg,
						"**/",
						indent + varDeclaration1 + getProjectPrefix() + varDeclaration2 + key.toUpperCase() + " = " + newErrorMsg1 + getProjectPrefix() + newErrorMsg2 + "\"" + key 
								+ "\"" + endErrorMsg,
						""};
				sourceContent += strJoin(newline, sa);
//				sourceContent += String.join(newline, 
//								"/**",
//								"* " + msg,
//								"**/",
//								indent + varDeclaration1 + getProjectPrefix() + varDeclaration2 + key.toUpperCase() + " = " + newErrorMsg1 + getProjectPrefix() + newErrorMsg2 + "\"" + key 
//										+ "\"" + endErrorMsg,
//								"");
				//Add content to HTML file
				String description = "";
				try {
					description = msgBundle.getString(key + ".description");
				} catch(MissingResourceException e) {
					System.out.println("  WARNING: No description provided for key '" + key + "'");
					description = "<em>No description provided</em>";
				}
				tableString += "<tr><td class=\"absorbing-column\">" + msg + "</td><td>" + description + "</td></tr>";
			}
		}
		
		sourceCodeString = sourceCodeString.replace("$content", sourceContent);
		
		System.out.println("    Writing to source files");
		writeToFile(projectFileBase +  getSourceFilePath(), sourceCodeString);

		//Finish off documentation and write to html file
		tableString += "</table>";
		htmlString = htmlString.replace("$body", tableString);
		
		System.out.println("    Writing to HTML file");
		writeToFile(projectFileBase + getDocumentationFilePath(), htmlString);
		
		System.out.println("  COMPLETE");
	}
	
	private static void writeToFile(String fileName, String content) throws IOException {
		File newFile = new File(fileName);
		newFile.getParentFile().mkdirs();
		if(!newFile.exists()) {
			newFile.createNewFile();
		}
		FileWriter fw = new FileWriter(newFile);
		fw.write(content);
		fw.close();
	}
	
	
	
	private static String getHtmlTemplate() {
		return "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"" +
		"\"http://www.w3.org/TR/html4/loose.dtd\">" +
		"<html>" +
		"<head>" +
		"<style>" +
		  "h1 { font-family: verdana, arial, sans-serif; }" +
		  "table, th, td { border: 1px solid black; }" +
		  "table { border-collapse: collapse; table-layout: auto; width: 100%; }" +
		  "table td.absorbing-column { width: 40%; }" +
		  "table td { padding: 5px; }" +
		  "table {" +
				"font-family: verdana, arial, sans-serif;" +
				"font-size: 12px;" +
				"color: #333333;" +
				"border-width: 1px;" +
				"border-color: #3A3A3A;" +
				"border-collapse: collapse;" +
			"}" +
			"table th {" +
				"border-width: 1px;" +
				"padding: 8px;" +
				"border-style: solid;" +
				"border-color: #517994;" +
				"background-color: #B2CFD8;" +
			"}" +
			"table tr:hover td {" +
				"background-color: #DFEBF1;" +
			"}" +
			"table td {" +
				"border-width: 1px;" +
				"padding: 8px;" +
				"border-style: solid;" +
				"border-color: #517994;" +
				"background-color: #ffffff;" +
			"}" +
		"</style>" +
		"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">" +
		"<title>$title</title>" +
		"</head>" +
		"<body>" +
		"<h1>$title</h1>" +
		"$body" +
		"</body>" +
		"</html>";
	}
}
