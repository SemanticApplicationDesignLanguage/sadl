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

public class NamedNode extends Node {
	private boolean validated = false;
	private NodeType nodeType = null;
	// if the Node represents a List (ClassListNode or DataTypeListNode) then the following may apply: 
	private int listLength = -1;						// the length restriction, if any (-1 => none)
	private int minListLength = -1;						// the minimum length restriction, if any (-1 => none)
	private int maxListLength = -1;						// the maximum length restriction, if any (-1 => none)
	
	public static enum NodeType {ClassNode, ClassListNode, InstanceNode, DataTypeNode, DataTypeListNode, 
									PropertyNode, DataTypeProperty, ObjectProperty, AnnotationProperty, 
									FunctionNode, VariableNode}
	
	public NamedNode() {
		super();
	}
	
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
	
	public NamedNode(String name, NodeType type) {
		this(name);
		setNodeType(type);
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setNodeType(NodeType nodeType) {
		this.nodeType = nodeType;
	}

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
		return true;
	}

	@Override
	public String toString() {
		if (prefix != null && prefix.length() > 0) {
			return prefix + ":" + name;
		}
		return name;
	}
	
	public String toFullyQualifiedString() {
		if (namespace != null) {
			return namespace + name;
		}
		return toString();
	}

	@Override
	public String toDescriptiveString() {
		return toString();
	}

	public void setNamespace(String namespace) {
		this.namespace = namespace;
	}

	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

	public void setValidated(boolean validated) {
		this.validated = validated;
	}

	public boolean isValidated() {
		return validated;
	}

	public int getListLength() {
		return listLength;
	}

	public void setListLength(int listLength) {
		this.listLength = listLength;
	}

	public int getMinListLength() {
		return minListLength;
	}

	public void setMinListLength(int minListLength) {
		this.minListLength = minListLength;
	}

	public int getMaxListLength() {
		return maxListLength;
	}

	public void setMaxListLength(int maxListLength) {
		this.maxListLength = maxListLength;
	}

	public boolean isList() {
		return nodeType != null && (nodeType.equals(NodeType.ClassListNode) || nodeType.equals(NodeType.DataTypeListNode));
	}

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
