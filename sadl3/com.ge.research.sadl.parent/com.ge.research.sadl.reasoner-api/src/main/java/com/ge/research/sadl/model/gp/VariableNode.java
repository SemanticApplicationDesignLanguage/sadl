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

import java.util.ArrayList;
import java.util.List;

import com.ge.research.sadl.reasoner.TranslationException;

/**
 * Class to represent a variable in the SADL grammar (explicitly specified) or 
 * a connective variable (implicit, generated automatically when nested GraphPatternElements 
 * are expanded). 
 */
public class VariableNode extends NamedNode {
	private int references = 0;
	private Object hostObject = null;
	private Node type = null;			// the type of the variable or if a list the type of the elements of the list 
	
	private List<GraphPatternElement> definition = null;	// definition(s) of the variable
	private boolean isCRulesVariable = false;		// generated from a Declaration (article before class name)

	/**
	 * Constructor taking variable name as argument
	 * @param name
	 */
	public VariableNode(String name) {
		super(name, NodeType.VariableNode);
	}

	/**
	 * {@link Deprecated}
	 */
	public void incrementReferences() {
		references += 1;
	}
	
	/**
	 * {@link Deprecated}
	 * @return
	 */
	public int getNumReferences() {
		return references;
	}

	/**
	 * The EObject in the SADL grammar from which this VariableNode was created
	 * @return
	 */
	public Object getHostObject() {
		return hostObject;
	}

	/**
	 * Set the EObject in the SADL grammar from which this VariableNode was created
	 * @param host
	 */
	public void setHostObject(Object host) {
		hostObject = host;
	}
	
	@Override
	public boolean isList() {
		if (type != null && type instanceof NamedNode) {
			return ((NamedNode)type).isList();
		}
		return super.isList();
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

	/**
	 * Get the type of the VariableNode (will be a class node)
	 * @return
	 */
	public Node getType() {
		return type;
	}

	/**
	 * Set the type of the VariableNode (should be a class node)
	 * @param type
	 * @throws TranslationException
	 */
	public void setType(Node type) throws TranslationException {
		if (this.type != null && 
				!this.type.equals(type)) {
			throw new TranslationException("Cannot change the type of a variable. (Attempted change from '" + 
				this.type.toFullyQualifiedString() + "' to '" + type.toFullyQualifiedString() + "'");
		}
		this.type = type;
	}

	/**
	 * Get the definition(s) of the VariableNode
	 * @return
	 */
	public List<GraphPatternElement> getDefinition() {
		return definition;
	}

	/**
	 * Set the definition(s) of the VariableNode
	 * @param definition
	 */
	public void setDefinition(List<GraphPatternElement> definition) {
		this.definition = definition;
	}
	
	public void addDefinition(GraphPatternElement definition) {
		if (this.definition == null) {
			this.definition = new ArrayList<GraphPatternElement>();
		}
		this.definition.add(definition);
	}

	/**
	 * Is this VariableNode created from a Declaration (article followed by class name)?
	 * @return
	 */
	public boolean isCRulesVariable() {
		return isCRulesVariable;
	}

	/**
	 * Set true if this VariableNode created from a Declaration (article followed by class name)
	 * @param isCRulesVariable
	 */
	public void setCRulesVariable(boolean isCRulesVariable) {
		this.isCRulesVariable = isCRulesVariable;
	}

	@Override
	public String toString() {
		// a variable need only have the name as the scope is this namespace, often this named structure--it can't be from another namespace.
		return getName();
	}
	
	@Override
	public String toDescriptiveString() {
		StringBuilder sb = new StringBuilder();
		sb.append(toString());
		sb.append(" (a variable of type ");
		if (isList()) {
			sb.append(getType() != null ? getType().toDescriptiveString() : "<unknown>");
			sb.append(" List");
			if (getListLength() >= 0) {
				sb.append(" length ");
				sb.append(getListLength());
			}
			else if (getMinListLength() >= 0) {
				sb.append(" length ");
				sb.append(getMinListLength());
				sb.append("-");
				if (getMaxListLength() >= 0) {
					sb.append(getMaxListLength());
				}
				else {
					sb.append("*");
				}
			}
		}
		else {
			sb.append(getType() != null ? getType().toDescriptiveString() : "<unknown>");
		}
		sb.append(")");
		return sb.toString();
	}

	@Override
	public String toFullyQualifiedString() {
		StringBuilder sb = new StringBuilder();
		sb.append(toString());
		sb.append(" (a variable of type ");
		if (isList()) {
			sb.append(getType() != null ? getType().toFullyQualifiedString() : "<unknown>");
			sb.append(" List");
			if (getListLength() >= 0) {
				sb.append(" length ");
				sb.append(getListLength());
			}
			else if (getMinListLength() >= 0) {
				sb.append(" length ");
				sb.append(getMinListLength());
				sb.append("-");
				if (getMaxListLength() >= 0) {
					sb.append(getMaxListLength());
				}
				else {
					sb.append("*");
				}
			}
		}
		else {
			sb.append(getType() != null ? getType().toFullyQualifiedString() : "<unknown>");
		}
		sb.append(")");
		return sb.toString();
	}


}
