package com.ge.research.sadl.model.gp;

/**
 * This class is a special subtype of NamedNode used as an Equation argument, 
 * indicating that the parameter declaration has an ellipsis, so is VarArgs parameter.
 * 
 * @author Natural Semantics, LLC
 *
 */
public class TypedEllipsisNode extends NamedNode {
	
	public TypedEllipsisNode(String name) {
		super(name);
	}

	@Override
	public String toString() {
		return super.toString() + " ...";
	}
	
	@Override
	public String toDescriptiveString() {
		return super.toDescriptiveString() + " ...";
	}

	@Override
	public String toFullyQualifiedString() {
		return super.toFullyQualifiedString() + " ...";
	}
}
