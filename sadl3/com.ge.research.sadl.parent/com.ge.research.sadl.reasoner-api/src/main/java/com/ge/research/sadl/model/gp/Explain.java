/************************************************************************
 * Copyright � 2007-2010 - General Electric Company, All Rights Reserved
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

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/26 18:52:09 $
 ***********************************************************************/

package com.ge.research.sadl.model.gp;

import java.util.ArrayList;
import java.util.List;

public class Explain extends SadlCommand {
	private List<GraphPatternElement> patterns;
	private String ruleName;
	
	public Explain(List<GraphPatternElement> gpes) {
		patterns = gpes;
	}
	
	public Explain(GraphPatternElement gpe) {
		patterns = new ArrayList<GraphPatternElement>(1);
		patterns.add(gpe);
	}
	
	public Explain(String rname) {
		ruleName = rname;
	}

	public void setPatterns(List<GraphPatternElement> patterns) {
		this.patterns = patterns;
	}

	public List<GraphPatternElement> getPatterns() {
		return patterns;
	}

	public void setRuleName(String ruleName) {
		this.ruleName = ruleName;
	}

	public String getRuleName() {
		return ruleName;
	}
	
	public String toString() {
		if (ruleName != null) {
			return "Explanation of Rule " + ruleName;
		}
		else if (patterns != null) {
			StringBuilder sb = new StringBuilder("Explanation of '");
			for (int i = 0; i < patterns.size(); i++) {
				if (i > 0) {
					sb.append(" . ");
				}
				sb.append(patterns.get(i).toString());
			}
			sb.append("'");
			return sb.toString();
		}
		return "Explain ill-defined:";
	}
}
