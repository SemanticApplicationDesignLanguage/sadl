package com.ge.research.sadl.model.gp;

/**
 * This class is a special subtype of Node used as an Equation argument, 
 * indicating that the parameter declaration is "unknown" ("--").
 * 
 * @author Natural Semantics, LLC
 *
 */
public class UnknownNode extends Node {

	public UnknownNode() {
		name = this.getClass().getName();
		namespace = "http://" + this.getClass().getPackage().getName() + "#";
	}

	@Override
	public String toString() {
		return "--";
	}

	@Override
	public String toFullyQualifiedString() {
		return "--";
	}

	@Override
	public String toDescriptiveString() {
		return "--";
	}

}
