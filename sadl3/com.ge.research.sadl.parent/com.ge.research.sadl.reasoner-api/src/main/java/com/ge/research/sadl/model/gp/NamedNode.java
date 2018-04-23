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
 * Class to capture a named concept (SadlResource) in the SADL grammar
 * @author 200005201
 *
 */
public class NamedNode extends Node {
	// Possible types that a NamedNode can be.
	public static enum NodeType {ClassNode, ClassListNode, InstanceNode, DataTypeNode, DataTypeListNode, 
		PropertyNode, DataTypeProperty, ObjectProperty, AnnotationProperty, 
		FunctionNode, VariableNode}

	private boolean validated = false;
	private NodeType nodeType = null;
	// if the Node represents a List (ClassListNode or DataTypeListNode) then the following may apply: 
	private int listLength = -1;						// the length restriction, if any (-1 => none)
	private int minListLength = -1;						// the minimum length restriction, if any (-1 => none)
	private int maxListLength = -1;						// the maximum length restriction, if any (-1 => none)
	
	/**
	 * Null argument constructor
	 */
	public NamedNode() {
		super();
	}
	
	/**
	 * Constructor taking name of new NamedNode (preferably a URI)
	 * @param name
	 */
	public NamedNode(String name) {
		int hash = name.indexOf('#');
		if (hash > 0 && hash < name.length() - 1) {
			setNamespace(name.substring(0, hash + 1));
			setName(name.substring(hash + 1));
		}
		else {
		    int colon = name.indexOf(':');
		    if (colon > 0 && colon < name.length() - 1) {
		        setPrefix(name.substring(0, colon));
		        setName(name.substring(colon + 1));
		    }
		    else {
		        setPrefix(null);
		        setName(name);
		    }
		}
	}
	
	/**
	 * Constructor taking both name and NodeType of new NamedNode
	 * @param name
	 * @param type
	 */
	public NamedNode(String name, NodeType type) {
		this(name);
		setNodeType(type);
	}

	/**
	 * Set the name of the NamedNode
	 * @param name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Set the NodeType of the NamedNode
	 * @param nodeType
	 */
	public void setNodeType(NodeType nodeType) {
		this.nodeType = nodeType;
	}

	/**
	 * Get the NodeType of the NamedNode
	 * @return
	 */
	public NodeType getNodeType() {
		return nodeType;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((nodeType == null) ? 0 : nodeType.hashCode());
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
		NamedNode other = (NamedNode) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (nodeType != other.nodeType)
			return false;
		if(isList() && !other.isList())
			return false;
		if(!isList() && other.isList())
			return false;
		if(isList() && other.isList()) {
			if(getListLength() != other.getListLength())
				return false;
			if(getMaxListLength() != other.getMaxListLength())
				return false;
			if(getMinListLength() != other.getMinListLength())
				return false;
		}
		return true;
	}

	@Override
	public String toString() {
		if (prefix != null && prefix.length() > 0) {
			return prefix + ":" + name;
		}
		return name;
	}
	
	@Override
	public String toFullyQualifiedString() {
		if (namespace != null) {
			return namespace + name;
		}
		return toString();
	}

	@Override
	public String toDescriptiveString() {
		if (getNodeType().equals(NodeType.ClassListNode) || getNodeType().equals(NodeType.DataTypeListNode)) {
			return toString() + " List";
		}
		String ts = toString();
		if (getMissingTripleReplacement() != null) {
			return ts + missingTripleReplacementToDescriptiveString();
		}
		return ts;
	}

	/**
	 * Set the namespace of the NamedNode
	 * @param namespace
	 */
	public void setNamespace(String namespace) {
		this.namespace = namespace;
	}

	/**
	 * Get the QName prefix of the NamedNode
	 * @param prefix
	 */
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

	/**
	 * Set validated for NamedNode
	 * @param validated
	 */
	public void setValidated(boolean validated) {
		this.validated = validated;
	}

	/**
	 * Is NamedNode validated?
	 * @return
	 */
	public boolean isValidated() {
		return validated;
	}

	/**
	 * If NamedNode is a List, does it have a list length restriction?
	 * @return
	 */
	public int getListLength() {
		return listLength;
	}

	/**
	 * If NamedNode is a List, set the list length restriction
	 * @param listLength
	 */
	public void setListLength(int listLength) {
		this.listLength = listLength;
	}

	/**
	 * If NamedNode is a List, does it have a min list length restriction?
	 * @return
	 */
	public int getMinListLength() {
		return minListLength;
	}

	/**
	 * If NamedNode is a List, set the min list length restriction
	 * @param minListLength
	 */
	public void setMinListLength(int minListLength) {
		this.minListLength = minListLength;
	}

	/**
	 * If NamedNode is a List, does it have a max list length restriction?
	 * @return
	 */
	public int getMaxListLength() {
		return maxListLength;
	}

	/**
	 * If NamedNode is a List, set the max list length restriction
	 * @param maxListLength
	 */
	public void setMaxListLength(int maxListLength) {
		this.maxListLength = maxListLength;
	}

	/**
	 * Is the NamedNode a list?
	 * @return
	 */
	public boolean isList() {
		return nodeType != null && (nodeType.equals(NodeType.ClassListNode) || nodeType.equals(NodeType.DataTypeListNode));
	}

	/**
	 * Set the NamedNode to be a list
	 * @param list
	 */
	public void setList(boolean list) {
		if (nodeType != null) {
			if (list) {
				if (nodeType.equals(NodeType.ClassNode)) {
					nodeType = NodeType.ClassListNode;
				}
				else if (nodeType.equals(NodeType.DataTypeNode)) {
					nodeType = NodeType.DataTypeListNode;
				}
			}
			else {
				if (nodeType.equals(NodeType.ClassListNode)) {
					nodeType = NodeType.ClassNode;
				}
				else if (nodeType.equals(NodeType.DataTypeListNode)) {
					nodeType = NodeType.DataTypeNode;
				}
			}
		}
	}

}
