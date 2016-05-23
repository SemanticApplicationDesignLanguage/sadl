package com.ge.research.sadl.model;

@SuppressWarnings("serial")
public class CircularDefinitionException extends Exception {
	private OntConceptType definitionType;
	
	public CircularDefinitionException(String msg, OntConceptType type) {
		super(msg);
		setDefinitionType(type);
	}

	public OntConceptType getDefinitionType() {
		return definitionType;
	}

	private void setDefinitionType(OntConceptType definitionType) {
		this.definitionType = definitionType;
	}
	
}
