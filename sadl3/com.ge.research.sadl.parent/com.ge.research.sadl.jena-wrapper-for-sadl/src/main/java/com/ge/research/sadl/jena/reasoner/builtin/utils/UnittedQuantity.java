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
package com.ge.research.sadl.jena.reasoner.builtin.utils;

import org.apache.jena.graph.Node;

/**
 * This class encapsulated the value Node and the unit Node of a UnittedQuantity instance.
 * It is only intended for use within ITypedBaseBuiltin implementations and in Utils.
 */
public class UnittedQuantity {
	private Node value;
	private Node unit;
	
	public UnittedQuantity(Node value, Node unit) {
		setValue(value);
		setUnit(unit);
	}
	
	public Node getUnit() {
		return unit;
	}
	
	public void setUnit(Node unit) {
		this.unit = unit;
	}
	
	public Node getValue() {
		return value;
	}
	
	public void setValue(Node value) {
		this.value = value;
	}
}

