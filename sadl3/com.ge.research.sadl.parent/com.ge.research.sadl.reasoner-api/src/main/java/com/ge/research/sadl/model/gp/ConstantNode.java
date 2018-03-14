package com.ge.research.sadl.model.gp;

import com.ge.research.sadl.reasoner.InvalidNameException;

public class ConstantNode extends Node {

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

	@Override
	public String toDescriptiveString() {
		return name + " (constant)";
	}
	
}
