package com.ge.research.sadl.model.gp;

public class Read extends SadlCommand {
	private String inputFilename = null;
	private String templateFilename = null;
	private String format = null;
	
	public Read(String inFN) {
		setInputFilename(inFN);
	}
	
	public Read(String inFN, String tmpFN) {
		setInputFilename(inFN);
		setTemplateFilename(tmpFN);
	}
	
	public Read(String inFN, String tmpFN, String fmt) {
		setInputFilename(inFN);
		setTemplateFilename(tmpFN);
		setFormat(fmt);
	}
	
	public String getInputFilename() {
		return inputFilename;
	}
	
	private void setInputFilename(String inputFilename) {
		this.inputFilename = inputFilename;
	}
	
	public String getTemplateFilename() {
		return templateFilename;
	}
	
	private void setTemplateFilename(String templateFilename) {
		this.templateFilename = templateFilename;
	}
	
	public String getFormat() {
		return format;
	}
	
	private void setFormat(String format) {
		this.format = format;
	}

}