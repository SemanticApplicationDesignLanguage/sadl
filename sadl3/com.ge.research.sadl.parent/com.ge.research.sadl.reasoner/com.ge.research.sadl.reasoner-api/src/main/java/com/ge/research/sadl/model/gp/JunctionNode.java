package com.ge.research.sadl.model.gp;

import com.ge.research.sadl.model.gp.Junction.JunctionType;

/**
 * Class to hold a conjunction or disjunction in a Node subclass
 * @author 200005201
 *
 */

public class JunctionNode extends Node {
	private Node lhs;
	private Node rhs;
	private JunctionType type;

	public JunctionNode(Node lhs, Node rhs, JunctionType type) {
		setLhs(lhs);
		setRhs(rhs);
		setType(type);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(JunctionType.toString(getType()));
		sb.append("(");
		sb.append(getLhs().toString());
		sb.append(",");
		sb.append(getRhs().toString());
		sb.append(")");
		return sb.toString();
	}

	@Override
	public String toFullyQualifiedString() {
		StringBuilder sb = new StringBuilder(JunctionType.toString(getType()));
		
		sb.append(getLhs().toFullyQualifiedString());
		sb.append("(");
		sb.append(getLhs().toFullyQualifiedString());
		sb.append(",");
		sb.append(getRhs().toFullyQualifiedString());
		sb.append(")");
		return sb.toString();
	}

	@Override
	public String toDescriptiveString() {
		StringBuilder sb = new StringBuilder(JunctionType.toString(getType()));
		sb.append("(");
		sb.append(getLhs().toDescriptiveString());
		sb.append(",");
		sb.append(getRhs().toDescriptiveString());
		sb.append(")");
		return sb.toString();
	}
	
	public Node getLhs() {
		return lhs;
	}

	public void setLhs(Node lhs) {
		this.lhs = lhs;
	}

	public Node getRhs() {
		return rhs;
	}

	public void setRhs(Node rhs) {
		this.rhs = rhs;
	}

	public JunctionType getType() {
		return type;
	}

	public void setType(JunctionType type) {
		this.type = type;
	}

}
