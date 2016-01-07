package com.ge.research.sadl.model.gp;

import java.util.List;

public class NegatedExistentialQuantifier extends GraphPatternElement {
	
	private List<String> variables = null;
	
	private GraphPatternElement quantified = null;
	
	public NegatedExistentialQuantifier(List<String> vars, GraphPatternElement _quantified) {
		setVariables(vars);
		setQuantified(_quantified);
	}
	
	@Override
	public String toFullyQualifiedString() {
		StringBuilder sb = new StringBuilder();
		sb.append("there is no ");
		for (int i = 0; variables != null && i < variables.size(); i++) {
			if (i > 0) sb.append(", ");
			sb.append(variables.get(i));
		}
		sb.append(" such that ");
		sb.append(getQuantified().toFullyQualifiedString());
		return sb.toString();
	}
	
	public String toString() {
		return toFullyQualifiedString();
	}

	public void setVariables(List<String> variables) {
		this.variables = variables;
	}
	
	public List<String> getVariables() {
		return variables;
	}

	public GraphPatternElement getQuantified() {
		return quantified;
	}

	private void setQuantified(GraphPatternElement quantified) {
		this.quantified = quantified;
	}
	
}
