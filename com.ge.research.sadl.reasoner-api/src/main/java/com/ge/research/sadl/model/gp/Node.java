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

public abstract class Node {
	// The value types of a variable node will be determined by its use as subject or object
	//	of a predicate in RDF triples and come from the domain or range. As such, the value
	//	types could be a union of classes (for an ObjectProperty)--hence the list.
	// For a NamedNode, the value type is the class to which the node belongs if an instance.
	private List<Node> nodeValueTypes;
	public abstract String toFullyQualifiedString();
	
	public List<Node> getNodeValueTypes() {
		return nodeValueTypes;
	}
	public void setNodeValueTypes(List<Node> nodeValueTypes) {
		this.nodeValueTypes = nodeValueTypes;
	}
	
	public void addNodeValueType(Node nodeValueType) {
		if (nodeValueTypes == null) {
			nodeValueTypes = new ArrayList<Node>();
		}
		if (!nodeValueTypes.contains(nodeValueType)) {
			nodeValueTypes.add(nodeValueType);
		}
	}
	
	public boolean hasCompatibleValueType(Node type) {
		if (nodeValueTypes != null && nodeValueTypes.contains(type)) {
			return true;
		}
		return false;
	}
}
