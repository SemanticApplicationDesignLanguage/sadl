package com.ge.research.messages;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.ResourceBundle;

public class ErrorMessage {
	private String msg;
	private String description;
	
	public ErrorMessage(String msg) {
		ResourceBundle msgBundle = ResourceBundle.getBundle("messages");
		this.msg = msgBundle.getString(msg);
		ResourceBundle descBundle = ResourceBundle.getBundle("descriptions");
		this.description = descBundle.getString(msg);
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
		return msg.replaceAll("\\{.*?\\}", "<>");
	}
}
