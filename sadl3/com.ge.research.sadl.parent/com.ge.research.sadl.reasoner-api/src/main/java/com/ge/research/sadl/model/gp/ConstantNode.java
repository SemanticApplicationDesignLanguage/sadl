package com.ge.research.sadl.model.gp;

import com.ge.research.sadl.reasoner.InvalidNameException;

public class ConstantNode extends NamedNode {

	public ConstantNode(String _name) throws InvalidNameException {
		super(_name);
		if (_name == null) {
			throw new InvalidNameException("A constant must have a name.");
		}
	}
	
}
