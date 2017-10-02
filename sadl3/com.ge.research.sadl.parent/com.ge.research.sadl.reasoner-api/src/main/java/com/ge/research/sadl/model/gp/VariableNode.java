/************************************************************************
 * Copyright \u00a9 2007-2010 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.model.gp;

import java.util.List;

import com.ge.research.sadl.reasoner.TranslationException;

/**
 * Keeps track of whether a connective variable (explicitly specified or 
 * generated automatically) has been used in another triple element. 
 */
public class VariableNode extends NamedNode {
	private int references = 0;
	private NamedNode type = null;
	private List<GraphPatternElement> definition = null;

	public VariableNode(String name) {
		super(name, NamedNode.NodeType.VariableNode);
	}

	public void incrementReferences() {
		references += 1;
	}
	
	public int getNumReferences() {
		return references;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + references;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
//		VariableNode other = (VariableNode) obj;
//		if (references != other.references)
//			return false;
		return true;
	}

	public NamedNode getType() {
		return type;
	}

	public void setType(NamedNode type) throws TranslationException {
		if (this.type != null && 
				!this.type.equals(type)) {
			throw new TranslationException("Cannot change the type of a variable. (Attempted change from '" + 
				this.type.toFullyQualifiedString() + "' to '" + type.toFullyQualifiedString() + "'");
		}
		this.type = type;
	}

	public List<GraphPatternElement> getDefinition() {
		return definition;
	}

	public void setDefinition(List<GraphPatternElement> definition) {
		this.definition = definition;
	}

}
