package com.ge.research.sadl.reasoner;

import java.util.List;

import jakarta.activation.DataSource;

import com.ge.research.sadl.model.gp.SadlCommand;

public class SadlCommandResult {
	private SadlCommand cmd;
	private Object results;
	private List<ModelError> errors;
	private List<ReasonerTiming> timingInfo;
	private DataSource derivations;
	
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
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("SADL Command Result:\n");
		sb.append("  ");
		sb.append(getCmd().toString());
		if (getResults() instanceof ResultSet) {
			sb.append("\n");
			sb.append(((ResultSet)getResults()).toStringWithIndent(2));
		}
		else {
			sb.append("\n  ");
			sb.append(getResults().toString());
		}
		if (getErrors() != null) {
			sb.append("\n  Errors:");
			for (ModelError err: getErrors()) {
				sb.append("\n  ");
				sb.append(err.toString());
			}
		}
		return sb.toString();
	}

	public List<ReasonerTiming> getTimingInfo() {
		return timingInfo;
	}

	public void setTimingInfo(List<ReasonerTiming> timingInfo) {
		this.timingInfo = timingInfo;
	}

	public DataSource getDerivations() {
		return derivations;
	}

	public void setDerivations(DataSource derivations) {
		this.derivations = derivations;
	}
}
