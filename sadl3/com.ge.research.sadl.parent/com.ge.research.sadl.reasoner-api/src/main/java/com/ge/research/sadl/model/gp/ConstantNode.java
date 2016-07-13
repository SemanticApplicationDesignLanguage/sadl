package com.ge.research.sadl.model.gp;

import com.ge.research.sadl.reasoner.InvalidNameException;

public class ConstantNode extends Node {

	private String name = null;
	
	public ConstantNode(String _name) throws InvalidNameException {
		if (_name == null) {
			throw new InvalidNameException("A constant must have a name.");
		}
		name = _name;
	}
	
	@Override
	public String toFullyQualifiedString() {
		return name;
	}
	
	public String toString() {
		return name;
	}
	
	public String getName() {
		return name;
	}

}
