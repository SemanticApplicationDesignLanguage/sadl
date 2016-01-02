package com.ge.research.sadl.model.gp;

import java.util.ArrayList;
import java.util.List;

public class StartWrite extends SadlCommand {
	private boolean dataOnly = false;
	
	public StartWrite(boolean bData) {
		setDataOnly(bData);
	}
	
	public boolean isDataOnly() {
		return dataOnly;
	}
	
	private void setDataOnly(boolean dataOnly) {
		this.dataOnly = dataOnly;
	}
	
}
