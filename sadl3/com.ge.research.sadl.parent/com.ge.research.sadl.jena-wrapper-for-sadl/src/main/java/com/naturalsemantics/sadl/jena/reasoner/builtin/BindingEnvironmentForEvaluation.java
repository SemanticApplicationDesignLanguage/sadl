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

import java.util.HashMap;
import java.util.Map;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.reasoner.TriplePattern;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;

public class BindingEnvironmentForEvaluation implements BindingEnvironment {
	
	private Map<Node, Node> bindings = null;

	@Override
	public Node getGroundVersion(Node node) {
		return node;
	}

	@Override
	public boolean bind(Node var, Node value) {
		if (bindings == null) {
			bindings = new HashMap<Node, Node>();
		}
		bindings.put(var, value);
		return true;
	}

	@Override
	public Triple instantiate(TriplePattern pattern) {
		// TODO Auto-generated method stub
		return null;
	}

	public Object getBoundValue(Node node) {
		return bindings.get(node);
	}

}
