package com.ge.research.sadl.model.gp;

public class Print extends SadlCommand {
	private String displayString;
	private String model;
	
	public Print(String displayStr) {
		displayString = displayStr;
	}

	public void setDisplayString(String displayString) {
		this.displayString = displayString;
	}

	public String getDisplayString() {
		return displayString;
	}

	public void setModel(String model) {
		this.model = model;
	}

	public String getModel() {
		return model;
	}
}
