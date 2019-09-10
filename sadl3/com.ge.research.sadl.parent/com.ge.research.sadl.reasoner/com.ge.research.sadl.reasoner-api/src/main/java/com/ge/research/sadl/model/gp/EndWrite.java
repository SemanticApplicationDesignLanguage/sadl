package com.ge.research.sadl.model.gp;

import java.util.ArrayList;
import java.util.List;

public class EndWrite extends SadlCommand {
	private String outputFilename = null;
	
	public EndWrite(String outFN) {
		setOutputFilename(outFN);
	}
	
	public String getOutputFilename() {
		return outputFilename;
	}
	
	private void setOutputFilename(String outputFilename) {
		this.outputFilename = outputFilename;
	}
	

}
