package com.ge.research.documentation;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;

import com.ge.research.messages.ErrorMessage;
import com.ge.research.sadl.reasoner.utils.SadlErrorMessages;
import com.hp.hpl.jena.util.FileUtils;


public class GenerateDocumentation {
	public static void main(String[] args) throws IllegalArgumentException, IllegalAccessException, IOException {
		//System.out.println(ErrorMessages.TEST.get("THIS IS YOUR MOTHER SPEAKING!"));
		
		String htmlString = FileUtils.readWholeFileAsUTF8("html/template.html");
		htmlString = htmlString.replace("$title", "Error Messages");
		String tableString = "<table><tr><th>Error Message</th><th>Description</th></tr>";
		
		Field[] fields = SadlErrorMessages.class.getDeclaredFields();
		for(Field f : fields) {
			ErrorMessage em = (ErrorMessage) f.get(null);
			tableString += "<tr><td class=\"absorbing-column\">" + em.toString() + "</td><td>" + em.getDescription() + "</td></tr>";
		}
		
		fields = RequirementsErrorMessages.class.getDeclaredFields();
		for(Field f : fields) {
			ErrorMessage em = (ErrorMessage) f.get(null);
			tableString += "<tr><td class=\"absorbing-column\">" + em.toString() + "</td><td>" + em.getDescription() + "</td></tr>";
		}
		
		tableString += "</table>";
		htmlString = htmlString.replace("$body", tableString);
		File newFile = new File("html/errormessages.html");
		FileWriter fileWriter = new FileWriter(newFile);
		fileWriter.write(htmlString);
		fileWriter.close();
	}
}
