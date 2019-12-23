/************************************************************************
 * Copyright \u00a9 2007-2010 - General Electric Company, All Rights Reserved
 *
 * Project: SADL
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/

package com.ge.research.sadl.model.gp;

import java.util.ArrayList;
import java.util.List;


public class Rule {
	private String ruleName = null;
	private String ruleNamespace = null;
	private int editorLine;
	private int editorLength;
	private int editorOffset;

	private List<String[]> annotations = null;
	private List<GraphPatternElement> givens = null;
	private List<GraphPatternElement> ifs = null;
	private List<GraphPatternElement> thens = null;
	private List<VariableNode> ruleVariables = null;
	private int stage = 1;	// default
	private boolean missingPatternsAdded = false;
	
	public Rule(String name) {
		setRuleName(name);
	}
	
	public Rule(String name, List<GraphPatternElement> _givens, 
			List<GraphPatternElement> _ifs, List<GraphPatternElement> _thens) {
		setRuleName(name);
		setGivens(_givens);
		setIfs(_ifs);
		setThens(_thens);
	}

	public void setGivens(List<GraphPatternElement> givens) {
		this.givens = givens;
	}
	
	public void addGiven(GraphPatternElement agiven) {
		if (givens == null) givens = new ArrayList<GraphPatternElement>();
		givens.add(agiven);
	}

	public List<GraphPatternElement> getGivens() {
		return givens;
	}
	
	public void addIf(GraphPatternElement anif) {
		if (ifs == null) ifs = new ArrayList<GraphPatternElement>();
		ifs.add(anif);
	}

	public void setIfs(List<GraphPatternElement> ifs) {
		this.ifs = ifs;
	}

	public List<GraphPatternElement> getIfs() {
		return ifs;
	}

	public void setThens(List<GraphPatternElement> thens) {
		this.thens = thens;
	}
	
	public void addThen(GraphPatternElement athen) {
		if (thens == null) thens = new ArrayList<GraphPatternElement>();
		thens.add(athen);
	}

	public List<GraphPatternElement> getThens() {
		return thens;
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder("Rule ");
		sb.append(getRuleName());
		sb.append(": ");
		for (int i = 0; givens != null && i < givens.size(); i++) {
			if (i == 0) {
				sb.append("given ");				
			}
			if (i > 0) sb.append(" and ");
			sb.append(givens.get(i).toString());
		}
		for (int i = 0; ifs != null && i < ifs.size(); i++) {
			if (i == 0) {
				sb.append(" if ");				
			}
			if (i > 0) sb.append(" and ");
			sb.append(ifs.get(i).toString());
		}
		for (int i = 0; thens != null && i < thens.size(); i++) {
			if (i == 0) {
				sb.append(" then ");				
			}
			if (i > 0) sb.append(" and ");
			sb.append(thens.get(i).toString());
		}
		sb.append(".");
		return sb.toString();
	}

	public String toFullyQualifiedString() {
		StringBuilder sb = new StringBuilder("Rule ");
		sb.append(getRuleName());
		sb.append(": ");
		for (int i = 0; givens != null && i < givens.size(); i++) {
			if (i == 0) {
				sb.append("given ");				
			}
			if (i > 0) sb.append(" and ");
			sb.append(givens.get(i).toFullyQualifiedString());
		}
		for (int i = 0; ifs != null && i < ifs.size(); i++) {
			if (i == 0) {
				sb.append(" if ");				
			}
			if (i > 0) sb.append(" and ");
			sb.append(ifs.get(i).toFullyQualifiedString());
		}
		for (int i = 0; thens != null && i < thens.size(); i++) {
			if (i == 0) {
				sb.append(" then ");				
			}
			if (i > 0) sb.append(" and ");
			sb.append(thens.get(i).toFullyQualifiedString());
		}
		sb.append(".");
		return sb.toString();
	}
	
	public String toDescriptiveString() {
		StringBuilder sb = new StringBuilder("Rule ");
		sb.append(getRuleName());
		sb.append(": ");
		for (int i = 0; givens != null && i < givens.size(); i++) {
			if (i == 0) {
				sb.append("given ");				
			}
			if (i > 0) sb.append(" and ");
			sb.append(givens.get(i).toDescriptiveString());
		}
		for (int i = 0; ifs != null && i < ifs.size(); i++) {
			if (i == 0) {
				sb.append(" if ");				
			}
			if (i > 0) sb.append(" and ");
			sb.append(ifs.get(i).toDescriptiveString());
		}
		for (int i = 0; thens != null && i < thens.size(); i++) {
			if (i == 0) {
				sb.append(" then ");				
			}
			if (i > 0) sb.append(" and ");
			sb.append(thens.get(i).toDescriptiveString());
		}
		sb.append(".");
		return sb.toString();
	}

	private void setRuleName(String ruleName) {
		this.ruleName = ruleName;
	}

	public String getRuleName() {
		return ruleName;
	}

	public List<String[]> getAnnotations() {
		return annotations;
	}

	public void setAnnotations(List<String[]> annotations) {
		this.annotations = annotations;
	}

	public String getRuleNamespace() {
		return ruleNamespace;
	}

	public void setRuleNamespace(String ruleNamespace) {
		this.ruleNamespace = ruleNamespace;
	}

	public int getEditorLine() {
		return editorLine;
	}

	public void setEditorLine(int editorLine) {
		this.editorLine = editorLine;
	}

	public int getEditorLength() {
		return editorLength;
	}

	public void setEditorLength(int editorLength) {
		this.editorLength = editorLength;
	}

	public int getEditorOffset() {
		return editorOffset;
	}

	public void setEditorOffset(int editorOffset) {
		this.editorOffset = editorOffset;
	}

	public List<VariableNode> getRuleVariables() {
		return ruleVariables;
	}

	public boolean addRuleVariable(VariableNode ruleVariable) {
		if (ruleVariables == null) {
			ruleVariables = new ArrayList<VariableNode>();
		}
		ruleVariables.add(ruleVariable);
		return true;
	}
	
	public VariableNode getVariable(String uri) {
		if (ruleVariables != null) {
			for (int i = 0; i < ruleVariables.size(); i++) {
				if (ruleVariables.get(i).toFullyQualifiedString().equals(uri)) {
					return ruleVariables.get(i);
				}
			}
		}
		return null;
	}

	public int getStage() {
		return stage;
	}

	public void setStage(int stage) {
		this.stage = stage;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((annotations == null) ? 0 : annotations.hashCode());
		result = prime * result + ((givens == null) ? 0 : givens.hashCode());
		result = prime * result + ((ifs == null) ? 0 : ifs.hashCode());
		result = prime * result + ((ruleName == null) ? 0 : ruleName.hashCode());
		result = prime * result + ((ruleNamespace == null) ? 0 : ruleNamespace.hashCode());
		result = prime * result + ((ruleVariables == null) ? 0 : ruleVariables.hashCode());
		result = prime * result + ((thens == null) ? 0 : thens.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Rule other = (Rule) obj;
		if (annotations == null) {
			if (other.annotations != null)
				return false;
		} else if (!annotations.equals(other.annotations))
			return false;
		if (givens == null) {
			if (other.givens != null)
				return false;
		} else if (!givens.equals(other.givens))
			return false;
		if (ifs == null) {
			if (other.ifs != null)
				return false;
		} else if (!ifs.equals(other.ifs))
			return false;
		if (ruleName == null) {
			if (other.ruleName != null)
				return false;
		} else if (!ruleName.equals(other.ruleName))
			return false;
		if (ruleNamespace == null) {
			if (other.ruleNamespace != null)
				return false;
		} else if (!ruleNamespace.equals(other.ruleNamespace))
			return false;
		if (ruleVariables == null) {
			if (other.ruleVariables != null)
				return false;
		} else if (!ruleVariables.equals(other.ruleVariables))
			return false;
		if (thens == null) {
			if (other.thens != null)
				return false;
		} else if (!thens.equals(other.thens))
			return false;
		return true;
	}
	
	/**
	 * Method to find out if missing patterns (if needed) have been added to this Rule
	 * @return
	 */
	public boolean isMissingPatternsAdded() {
		return missingPatternsAdded;
	}

	/**
	 * Method to set flag that missing patterns have been added (if needed) to this Rule
	 * @param missingPatternsAdded
	 */
	public void setMissingPatternsAdded(boolean missingPatternsAdded) {
		this.missingPatternsAdded = missingPatternsAdded;
	}
	
	/**
	 * Get CtxRequrement's URI
	 * @return
	 */
	public String getUri() {
		return getRuleNamespace() + getRuleName();
	}
	
}
