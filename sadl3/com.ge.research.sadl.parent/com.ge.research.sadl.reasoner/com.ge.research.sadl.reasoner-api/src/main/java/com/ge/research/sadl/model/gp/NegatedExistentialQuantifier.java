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
		StringBuilder sb = new StringBuilder();
		sb.append("there is no ");
		for (int i = 0; variables != null && i < variables.size(); i++) {
			if (i > 0) sb.append(", ");
			sb.append(variables.get(i));
		}
		sb.append(" such that ");
		sb.append(getQuantified().toString());
		return sb.toString();
	}

	@Override
	public String toDescriptiveString() {
		StringBuilder sb = new StringBuilder();
		sb.append("there is no ");
		for (int i = 0; variables != null && i < variables.size(); i++) {
			if (i > 0) sb.append(", ");
			sb.append(variables.get(i));
		}
		sb.append(" such that ");
		sb.append(getQuantified().toDescriptiveString());
		return sb.toString();
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((quantified == null) ? 0 : quantified.hashCode());
		result = prime * result + ((variables == null) ? 0 : variables.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		NegatedExistentialQuantifier other = (NegatedExistentialQuantifier) obj;
		if (quantified == null) {
			if (other.quantified != null)
				return false;
		} else if (!quantified.equals(other.quantified))
			return false;
		if (variables == null) {
			if (other.variables != null)
				return false;
		} else if (!variables.equals(other.variables))
			return false;
		return true;
	}

}
