/************************************************************************
 * Copyright © 2023 - Natural Semantics, LLC. All Rights Reserved.
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
package com.naturalsemantics.sadl.jena.reasoner.builtin;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.reasoner.InfGraph;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.Rule;
import org.apache.jena.util.iterator.ClosableIterator;

public class RuleContextForEvaluation implements org.apache.jena.reasoner.rulesys.RuleContext {

	private BindingEnvironmentForEvaluation bindingEnv = null;
	
	public RuleContextForEvaluation(BindingEnvironmentForEvaluation befe) {
		bindingEnv = befe;
	}
	
	@Override
	public BindingEnvironment getEnv() {
		return bindingEnv;
	}

	@Override
	public InfGraph getGraph() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Rule getRule() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setRule(Rule rule) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean contains(Triple t) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean contains(Node s, Node p, Node o) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public ClosableIterator<Triple> find(Node s, Node p, Node o) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void silentAdd(Triple t) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void add(Triple t) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void remove(Triple t) {
		// TODO Auto-generated method stub
		
	}

}
