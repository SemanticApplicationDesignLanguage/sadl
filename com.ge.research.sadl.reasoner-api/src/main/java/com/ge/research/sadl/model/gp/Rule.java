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
	private List<String[]> annotations = null;
	private List<GraphPatternElement> givens = null;
	private List<GraphPatternElement> ifs = null;
	private List<GraphPatternElement> thens = null;
	
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
}
