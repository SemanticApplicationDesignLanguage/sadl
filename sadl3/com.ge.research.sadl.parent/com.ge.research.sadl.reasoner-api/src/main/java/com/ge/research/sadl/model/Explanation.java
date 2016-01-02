/************************************************************************
 * Copyright 2007-2010 - General Electric Company, All Rights Reserved
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
 * $Revision: 1.2 $ Last modified on   $Date: 2013/08/27 18:05:13 $
 ***********************************************************************/

package com.ge.research.sadl.model;

import java.util.ArrayList;
import java.util.List;

import com.ge.research.sadl.model.gp.GraphPatternElement;

public class Explanation {
	private String patternPrefix;
	private GraphPatternElement pattern;
	private List<String> explanations;
	
	public Explanation(GraphPatternElement gpe) {
		pattern = gpe;
	}
	
	public Explanation(GraphPatternElement gpe, String expl) {
		pattern = gpe;
		explanations = new ArrayList<String>(1);
		explanations.add(expl);
	}
	
	public void setGraphPatternElement(GraphPatternElement gpePattern) {
		this.pattern = gpePattern;
	}
	public GraphPatternElement getGrpahPatternElement() {
		return pattern;
	}
	public void setExplanations(List<String> explanations) {
		this.explanations = explanations;
	}
	public List<String> getExplanations() {
		return explanations;
	}

	public void setPatternPrefix(String patternPrefix) {
		this.patternPrefix = patternPrefix;
	}

	public String getPatternPrefix() {
		return patternPrefix;
	}
}
