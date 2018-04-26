/************************************************************************
 * Copyright \u00a9 2007-2017 - General Electric Company, All Rights Reserved
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

/**
 * Top-level abstract class which is the super class of all graph pattern constructs. 
 * @author 200005201
 *
 */
public abstract class GraphPatternElement {
	enum PatternFunction {BindsVariable, UsesBoundVariables}
	private PatternFunction patternFunction;
	private Object locationType;
	private int lineNo;
	private int length;
	private int offset;
	
	// These only apply to BuiltinElement and TripleElement
	private NamedNode leftImpliedPropertyUsed = null;		// an impliedProperty to be used on the left side of a binary operation
	private NamedNode rightImpliedPropertyUsed = null;		// am impliedProperty to be used on the right side of a binary operation
	private List<NamedNode> expandedPropertiesToBeUsed = null;	// a list of expandedProperties to be used on both sides of a binary operation

	private GraphPatternElement next = null;
	
	private boolean isEmbedded = false;	// this will be replaced with use of PatternFunction 

	/**
	 * Constructor
	 */
	public GraphPatternElement() {
	}

	/**
	 * {@link Deprecated}
	 * Set the next in a linked list of GraphPatternElements (linked lists no longer used)
	 * @param next
	 */
	public void setNext(GraphPatternElement next) {
		this.next = next;
	}

	/**
	 * {@link Deprecated}
	 * Get the next in a linked list of GraphPatternElements (linked lists no longer used)
	 * @return - next GraphPatternElement
	 */
	public GraphPatternElement getNext() {
		return next;
	}

	/**
	 * Set line number of this GraphPatternElement in the original SADL file
	 * @param lineNo
	 */
	public void setLineNo(int lineNo) {
		this.lineNo = lineNo;
	}

	/**
	 * Get line number of this GraphPatternElement in the original SADL file
	 * @return
	 */
	public int getLineNo() {
		return lineNo;
	}

	/**
	 * Set the length of this GraphPatternElement in the original SADL file
	 * @param length
	 */
	public void setLength(int length) {
		this.length = length;
	}

	/**
	 * Get the length of this GraphPatternElement in the original SADL file
	 * @return
	 */
	public int getLength() {
		return length;
	}

	/**
	 * Set the offset of this GraphPatternElement in the original SADL file
	 * @param offset
	 */
	public void setOffset(int offset) {
		this.offset = offset;
	}

	/**
	 * Get the offset of this GraphPatternElement in the original SADL file
	 * @return
	 */
	public int getOffset() {
		return offset;
	}

	/**
	 * {@link Deprecated}
	 * @param isEmbedded
	 */
	public void setEmbedded(boolean isEmbedded) {
		this.isEmbedded = isEmbedded;
	}

	/**
	 * {@link Deprecated}
	 * @return
	 */
	public boolean isEmbedded() {
		return isEmbedded;
	}

	/**
	 * Default method to convert the GraphPatternElement to a string
	 */
	public abstract String toString();
	
	/**
	 * Convert this GraphPatternElement to a string in which each named concept from the ontology 
	 * is identified by a complete URI
	 * @return
	 */
	public abstract String toFullyQualifiedString();
	
	/**
	 * Convert this GraphPatternElement to the most descriptive string available
	 * @return
	 */
	public abstract String toDescriptiveString();
	
	/**
	 * {@link Deprecated}
	 * @return
	 */
	public PatternFunction getPatternFunction() {
		return patternFunction;
	}

	/**
	 * {@link Deprecated}
	 * @param patternFunction
	 */
	public void setPatternFunction(PatternFunction patternFunction) {
		this.patternFunction = patternFunction;
	}

	/**
	 * {@link Deprecated}
	 * @return
	 */
	public Object getLocationType() {
		return locationType;
	}

	/**
	 * {@link Deprecated}
	 * @param locationType
	 */
	public void setLocationType(Object locationType) {
		this.locationType = locationType;
	}
	
	/**
	 * Get the expanded properties to be used to augment the property chain of this GraphPatternElement
	 * @return
	 */
	public List<NamedNode> getExpandedPropertiesToBeUsed() {
		return expandedPropertiesToBeUsed;
	}

	/**
	 * Set the expanded properties to be used to augment the property chain of this GraphPatternElement
	 * @param expandedPropertiesToBeUsed
	 */
	public void setExpandedPropertiesToBeUsed(List<NamedNode> expandedPropertiesToBeUsed) {
		this.expandedPropertiesToBeUsed = expandedPropertiesToBeUsed;
	}
	
	/**
	 * Add a property to the list of expanded properties to be used to augment the property chain
	 * of this GraphPatternElement
	 * @param expProp
	 * @return
	 */
	public boolean addExpandedPropertyToBeUsed(NamedNode expProp) {
		if (expandedPropertiesToBeUsed == null) {
			expandedPropertiesToBeUsed = new ArrayList<NamedNode>();
		}
		if (!expandedPropertiesToBeUsed.contains(expProp)) {
			expandedPropertiesToBeUsed.add(expProp);
			return true;
		}
		return false;
	}

	/**
	 * Get the implied property (if any) to be used on the right of this GraphPatternElement
	 * @return
	 */
	public NamedNode getRightImpliedPropertyUsed() {
		return rightImpliedPropertyUsed;
	}

	/**
	 * Set the implied property (if any) to be used on the right of this GraphPatternElement
	 * @return
	 */
	public void setRightImpliedPropertyUsed(NamedNode rightImpliedPropertyUsed) {
		this.rightImpliedPropertyUsed = rightImpliedPropertyUsed;
	}

	/**
	 * Get the implied property (if any) to be used on the left of this GraphPatternElement
	 * @return
	 */
	public NamedNode getLeftImpliedPropertyUsed() {
		return leftImpliedPropertyUsed;
	}

	/**
	 * Set the implied property (if any) to be used on the left of this GraphPatternElement
	 * @return
	 */
	public void setLeftImpliedPropertyUsed(NamedNode leftImpliedPropertyUsed) {
		this.leftImpliedPropertyUsed = leftImpliedPropertyUsed;
	}

}
