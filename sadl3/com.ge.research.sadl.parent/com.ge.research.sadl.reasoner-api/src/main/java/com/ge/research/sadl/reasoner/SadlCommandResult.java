package com.ge.research.sadl.reasoner;

import java.util.List;

import com.ge.research.sadl.model.gp.SadlCommand;

public class SadlCommandResult {
	private SadlCommand cmd;
	private Object results;
	private List<ModelError> errors;
	
	public SadlCommandResult(SadlCommand cmd) {
		this.cmd = cmd;
	}
	
	public SadlCommand getCmd() {
		return cmd;
	}
	
	public void setCmd(SadlCommand cmd) {
		this.cmd = cmd;
	}
	
	public Object getResults() {
		return results;
	}
	
	public void setResults(Object results) {
		this.results = results;
	}
	
	public List<ModelError> getErrors() {
		return errors;
	}
	
	public void setErrors(List<ModelError> errors) {
		this.errors = errors;
	}
}
