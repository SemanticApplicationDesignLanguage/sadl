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

/**
 * Class to represent a Node in a graph model.
 * @author 200005201
 *
 */
public abstract class Node implements Cloneable{
	// The value types of a variable node will be determined by its use as subject or object
	//	of a predicate in RDF triples and come from the domain or range. As such, the value
	//	types could be a union of classes (for an ObjectProperty)--hence the list.
	// For a NamedNode, the value type is the class to which the node belongs if an instance.
	protected String name = null;
	protected String prefix = null;
	protected String namespace = null;

	/**
	 * Default method to convert the GraphPatternElement to a string
	 */
	public abstract String toString();

	/**
	 * Convert this Node to a string in which each named concept from the ontology 
	 * is identified by a complete URI
	 * @return
	 */
	public abstract String toFullyQualifiedString();

	/**
	 * Convert this Node to the most descriptive string available
	 * @return
	 */
	abstract public String toDescriptiveString();
	
	/**
	 * Method to get the local name, if any of the node.
	 * @return
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Method to get the namespace, if any, of the node.
	 * @return
	 */
	public String getNamespace() {
		return namespace;
	}
	
	/**
	 * Method to get the prefix, if any, to be used in the qualified name of the node.
	 * @return
	 */
	public String getPrefix() {
		return prefix;
	}
	
	/**
	 * Method to the the URI (Universal Resource Identifier), if any, which is the unique 
	 * identifier of this node.
	 * @return
	 */
	public String getURI() {
		if (namespace != null) {
			return namespace + name;
		}
		return null;
	}
	
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	public Object clone() throws CloneNotSupportedException {
	    return super.clone();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((namespace == null) ? 0 : namespace.hashCode());
		result = prime * result + ((prefix == null) ? 0 : prefix.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Node other = (Node) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (namespace == null) {
			if (other.namespace != null)
				return false;
		} else if (!namespace.equals(other.namespace))
			return false;
		if (prefix == null) {
			if (other.prefix != null)
				return false;
		} else if (!prefix.equals(other.prefix))
			return false;
		return true;
	}

}
