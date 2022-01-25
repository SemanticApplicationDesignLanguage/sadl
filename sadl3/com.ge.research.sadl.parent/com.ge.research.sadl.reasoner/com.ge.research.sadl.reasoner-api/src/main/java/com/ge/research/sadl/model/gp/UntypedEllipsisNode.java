package com.ge.research.sadl.model.gp;

/**
 * This class is a special subtype of Node used as an Equation argument, 
 * indicating that the parameter declaration is unnamed, untyped, but has an ellipsis, so is VarArgs parameter.
 * 
 * @author Natural Semantics, LLC
 *
 */
public class UntypedEllipsisNode extends Node {

	@Override
	public String toString() {
		return "...";
	}

	@Override
	public String toFullyQualifiedString() {
		return "...";
	}

	@Override
	public String toDescriptiveString() {
		return "...";
	}

}
