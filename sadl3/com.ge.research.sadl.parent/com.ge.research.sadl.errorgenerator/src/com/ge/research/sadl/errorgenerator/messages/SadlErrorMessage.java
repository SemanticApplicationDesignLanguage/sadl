package com.ge.research.sadl.errorgenerator.messages;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

public class SadlErrorMessage {
	private String msg;
	private String description;
	
	public SadlErrorMessage(String msg) {
		ResourceBundle msgBundle = ResourceBundle.getBundle("sadlMessages");
		
		this.msg = msgBundle.getString(msg);
		try {
			this.description = msgBundle.getString(msg + ".description");
		} catch(MissingResourceException e) {
			this.description = "<em>No description provided</em>";
		}
	}
	
	public SadlErrorMessage(String msg, String desc) {
		this.msg = msg;
		this.description = desc;
	}
	
	public String get(String... args) {
		MessageFormat m = new MessageFormat(msg);
		return m.format(args);
	}
	
	public String getDescription() {
		MessageFormat m = new MessageFormat(description);
		return m.format(null);
	}
	
	public String toString() {
		//return msg.replaceAll("\\{.*?\\}", "<>");
		return msg;
	}
}