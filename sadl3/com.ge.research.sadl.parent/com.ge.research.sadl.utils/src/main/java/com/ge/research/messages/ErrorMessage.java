package com.ge.research.messages;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

public class ErrorMessage {
	private String msg;
	private String description;
	public final static String SADL_MSG = "sadl";
	public final static String REQ_MSG = "req";
	
	public ErrorMessage(String msg) {
		ResourceBundle bundle = ResourceBundle.getBundle(SADL_MSG + "Messages");
		
		try {
			this.msg = bundle.getString(msg);
		} catch(MissingResourceException e) {
			bundle = ResourceBundle.getBundle(REQ_MSG + "Messages");
			this.msg = bundle.getString(msg);
		} finally {
			getDescriptionFromBundle(bundle, msg);
		}
	}
	
	public String get(String... args) {
		MessageFormat m = new MessageFormat(msg);
		return m.format(args);
	}
	
	private void getDescriptionFromBundle(ResourceBundle bundle, String msg) {
		try {
			this.description = bundle.getString(msg + ".description");
		} catch(MissingResourceException e) {
			System.out.println("  WARNING: Description missing for message '" + msg + "'");
			this.description = "<em>No description provided</em>";
		}
	}
	
	public String getDescription() {
		MessageFormat m = new MessageFormat(description);
		return m.format(null);
	}
	
	public String toString() {
		return msg.replaceAll("\\{.*?\\}", "<>");
	}
}
