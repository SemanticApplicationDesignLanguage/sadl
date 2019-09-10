package com.ge.research.sadl.model.gp;

import com.ge.research.sadl.reasoner.InvalidNameException;

/**
 * Class capturing a Node which is a constant in the SADL grammar
 * @author 200005201
 *
 */
public class ConstantNode extends Node {

	/**
	 * Constructor taking name of the Constant, e.g., "PI"
	 * @param _name
	 * @throws InvalidNameException
	 */
	public ConstantNode(String _name) throws InvalidNameException {
		if (_name == null) {
			throw new InvalidNameException("A constant must have a name.");
		}
		name = _name;
	}
	
	/**
	 * Get the name of this ConstantNode
	 */
	public String getName() {
		return name;
	}

	@Override
	public String toFullyQualifiedString() {
		return name;
	}
	
	@Override
	public String toString() {
		return name;
	}
	
	@Override
	public String toDescriptiveString() {
		return name + " (constant)";
	}
	
}
