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

import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.TripleElement.TripleSourceType;

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

	// editor object for marker addition to editor
	private Object context;

	private Node mLocalizedType = null;
	
	private boolean validated = false;
	private NodeType nodeType = null;
	// if the Node represents a List (ClassListNode or DataTypeListNode) then the following may apply: 
	private int listLength = -1;						// the length restriction, if any (-1 => none)
	private int minListLength = -1;						// the minimum length restriction, if any (-1 => none)
	private int maxListLength = -1;						// the maximum length restriction, if any (-1 => none)
	private List<Object> listLiterals = null;			// a collection of literals that are included in this list
	
	private ProxyNode missingTripleReplacement = null;
	private List<TripleElement> missingPatterns = null;  // a list of patterns found to be missing from the higher-level structure
														 // e.g, Rule, and which should be added before the GraphPatternElement
														 // containing this Node
	
	private NamedNode mImpliedPropertyNode = null;		// contains the implied property information of the named node
	

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

	/*
	 * Sets locally restricted type of Node, usually range
	 * @param Node
	 */
	public void setLocalizedType(Node aNode) {
		this.mLocalizedType = aNode;
	}
	
	/*
	 * Gets locally restricted type
	 */
	public Node getLocalizedType() {
		return this.mLocalizedType;
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
		StringBuilder sb = new StringBuilder();
		if (prefix != null && prefix.length() > 0) {
			sb.append(prefix);
			sb.append(":");
		}
		sb.append(name);
		if (getNodeType() != null && 
				(getNodeType().equals(NodeType.ClassListNode) || getNodeType().equals(NodeType.DataTypeListNode))) {
			sb.append(" List");
			if(listLiterals != null) {
				sb.append(" [");
				for(int i = 0; i < listLiterals.size(); i++) {
					if(i > 0) {
						sb.append(",");
					}
					sb.append(listLiterals.get(i));
				}
				sb.append("]");
			}
		}
		return sb.toString();
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
		StringBuilder sb = new StringBuilder();
		if (prefix != null && prefix.length() > 0) {
			sb.append(prefix);
			sb.append(":");
		}
		sb.append(name);
		if (getNodeType() != null && 
				(getNodeType().equals(NodeType.ClassListNode) || getNodeType().equals(NodeType.DataTypeListNode))) {
			sb.append(" List");
			if(listLiterals != null) {
				sb.append(" [");
				for(int i = 0; i < listLiterals.size(); i++) {
					if(i > 0) {
						sb.append(",");
					}
					sb.append(listLiterals.get(i));
				}
				sb.append("]");
			}
		}
		if (getMissingTripleReplacement() != null) {
			sb.append(missingTripleReplacementToDescriptiveString());
		}
		if (getMissingPatterns() != null) {
			sb.append(missingPatternsToDescriptiveString());
		}
		if(getImpliedPropertyNode() != null) {
			sb.append(impliedPropertyToDescriptiveString());
		}
		return sb.toString();
	}

	private String impliedPropertyToDescriptiveString() {
		StringBuilder sb = new StringBuilder(" (has implied property '");
		Object ip = getImpliedPropertyNode();
		if (ip instanceof NamedNode) {
			sb.append(((NamedNode)ip).toDescriptiveString());
			sb.append("')");
		}
		return sb.toString();
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
	 * Add literal value for list to the collection of List Literals
	 * @param aObject
	 */
	public void addListLiteral(Object aObject) {
		if(this.listLiterals == null) {
			this.listLiterals = new ArrayList<Object>();
		}
		
		this.listLiterals.add(aObject);
	}
	
	/**
	 * Sets the literals collection for NamedNode List
	 * @param alistLiterals
	 */
	public void setListLiterals(List<Object> alistLiterals) {
		this.listLiterals = alistLiterals;
	}
	
	/**
	 * Literals in the NamedNode List
	 * @return a collection of literals in this list
	 */
	public List<Object> getListLiterals(){
		return this.listLiterals;
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

	/**
	 * Get the ProxyNode representing a replacement (if any) for this Node
	 * @return
	 */
	public ProxyNode getMissingTripleReplacement() {
		return missingTripleReplacement;
	}
	
	/**
	 * Set the ProxyNode representing a replacement for this Node
	 * (Occurs when Node is a lone property in an incomplete property chain) 
	 * @param missingTripleReplacement
	 */
	public void setMissingTripleReplacement(ProxyNode missingTripleReplacement) {
		this.missingTripleReplacement = missingTripleReplacement;
	}
	
	protected String missingTripleReplacementToDescriptiveString() {
		StringBuilder sb = new StringBuilder(" (has missing triple replacement '");
		Object pf = getMissingTripleReplacement().getProxyFor();
		if (pf instanceof TripleElement) {
			sb.append(((TripleElement)pf).toDescriptiveString());
			sb.append("')");
		}
		return sb.toString();
	}

	/**
	 * Get the list of missing TripleElements to be applied to this Node
	 * Note that the list is ordered top-down, in the direction of the directed graph
	 * @return
	 */
	public List<TripleElement> getMissingPatterns() {
		return missingPatterns;
	}

	/**
	 * Set the list of missing TripleElements to be applied to this Node
	 * @param missingPatterns
	 */
	public void setMissingPatterns(List<TripleElement> missingPatterns) {
		this.missingPatterns = missingPatterns;
	}

	/**
	 * Add a GrpahPatternElement to the list of missing TripleElements to be applied to this Node
	 * @param missingPattern
	 */
	public boolean addMissingPattern(TripleElement missingPattern) {
		if (this.missingPatterns == null) {
			missingPatterns = new ArrayList<TripleElement>();
		}
		if (!missingPatterns.contains(missingPattern)) {
			boolean added = false;
			if (missingPatterns.size() > 0) {
				int idx = 0;
				for (TripleElement tr : missingPatterns) {
					if (tr.getSubject().equals(missingPattern.getObject())) {
						missingPatterns.add(idx, missingPattern);
						added = true;
						break;
					}
					idx++;
				}
			}
			if (!added) {
				missingPatterns.add(missingPattern);
			}
			return true;
		}
		return false;
	}

	/**
	 * Add a GrpahPatternElement to the list of missing TripleElements to be applied to this Node
	 * at the indicated location in the list
	 * @param idx
	 * @param missingPattern
	 */
	public boolean addMissingPattern(int idx, TripleElement missingPattern) {
		if (this.missingPatterns == null) {
			missingPatterns = new ArrayList<TripleElement>();
		}
		if (!missingPatterns.contains(missingPattern)) {
			missingPatterns.add(idx, missingPattern);
			return true;
		}
		return false;
	}

	/**
	 * Method to serialize missing patterns descriptively
	 * @return
	 */
	protected String missingPatternsToDescriptiveString() {
		StringBuilder sb = new StringBuilder(" (has missing triple patterns: [");
		List<TripleElement> mps = getMissingPatterns();
		int cntr = 0;
		for (GraphPatternElement mp : mps) {
			if (cntr > 0) sb.append(", ");
			sb.append("'");
			if (mp instanceof TripleElement && 
					(((TripleElement)mp).getSubject() == null || !((TripleElement)mp).getSubject().equals(this)) &&
					(((TripleElement)mp).getPredicate() == null || !((TripleElement)mp).getPredicate().equals(this)) &&
					(((TripleElement)mp).getObject() == null || !((TripleElement)mp).getObject().equals(this))) {
				sb.append(mp.toDescriptiveString());
			}
			else {
				sb.append(mp.toString());
			}
			sb.append("'");
			cntr++;
		}
		sb.append("])");
		return sb.toString();
	}
	
	/**
	 * Get the node's implied property
	 * @return NamedNode mImpliedPropertyNode
	 */
	public NamedNode getImpliedPropertyNode() {
		return mImpliedPropertyNode;
	}
	
	/**
	 * set the node's implied property
	 * @param NamedNode mImpliedPropertyNode
	 */
	public void setImpliedPropertyNode(NamedNode aImpliedPropertyNode) {
		this.mImpliedPropertyNode = aImpliedPropertyNode;
	}

	public Object getContext() {
		return context;
	}

	public void setContext(Object context) {
		this.context = context;
	}
	
	

}
