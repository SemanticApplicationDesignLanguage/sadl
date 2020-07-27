package com.ge.research.sadl.model.gp;

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
