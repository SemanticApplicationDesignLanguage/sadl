package com.ge.research.documentation;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;

import com.ge.research.messages.ErrorMessage;
import com.hp.hpl.jena.util.FileUtils;


public class GenerateDocumentation {
	public static void main(String[] args) throws IllegalArgumentException, IllegalAccessException, IOException {
		System.out.println("----------Starting Document Generation----------");
		
		String htmlString = FileUtils.readWholeFileAsUTF8("html/template.html");
		htmlString = htmlString.replace("$title", "Error Messages");
		String tableString = "<table><tr><th>Error Message</th><th>Description</th></tr>";
		
		System.out.println("    Generating SADL Error Messages");
		Field[] fields = SadlErrorMessages.class.getDeclaredFields();
		for(Field f : fields) {
			ErrorMessage em = (ErrorMessage) f.get(null);
			tableString += "<tr><td class=\"absorbing-column\">" + em.toString() + "</td><td>" + em.getDescription() + "</td></tr>";
		}
		
		System.out.println("    Generating Requirements Error Messages");
		fields = RequirementsErrorMessages.class.getDeclaredFields();
		for(Field f : fields) {
			ErrorMessage em = (ErrorMessage) f.get(null);
			tableString += "<tr><td class=\"absorbing-column\">" + em.toString() + "</td><td>" + em.getDescription() + "</td></tr>";
		}
		
		tableString += "</table>";
		htmlString = htmlString.replace("$body", tableString);
		File newFile = new File("html/errormessages.html");
		
		System.out.println("    Writing to HTML file");
		FileWriter fileWriter = new FileWriter(newFile);
		fileWriter.write(htmlString);
		fileWriter.close();
		
		System.out.println("  COMPLETE");
	}
}
