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

public abstract class GraphPatternElement {
	enum PatternFunction {BindsVariable, UsesBoundVariables}
	private PatternFunction patternFunction;
	private Object locationType;
	private int lineNo;
	private int length;
	private int offset;
	
	private GraphPatternElement next = null;
	
	private boolean isEmbedded = false;	// this will be replaced with use of PatternFunction 

	public GraphPatternElement() {
	}

	public void setNext(GraphPatternElement next) {
		this.next = next;
	}

	public GraphPatternElement getNext() {
		return next;
	}

	public void setLineNo(int lineNo) {
		this.lineNo = lineNo;
	}

	public int getLineNo() {
		return lineNo;
	}

	public void setLength(int length) {
		this.length = length;
	}

	public int getLength() {
		return length;
	}

	public void setOffset(int offset) {
		this.offset = offset;
	}

	public int getOffset() {
		return offset;
	}

	public void setEmbedded(boolean isEmbedded) {
		this.isEmbedded = isEmbedded;
	}

	public boolean isEmbedded() {
		return isEmbedded;
	}

	public abstract String toString();
	
	public abstract String toFullyQualifiedString();
	
	public abstract String toDescriptiveString();

	public PatternFunction getPatternFunction() {
		return patternFunction;
	}

	public void setPatternFunction(PatternFunction patternFunction) {
		this.patternFunction = patternFunction;
	}

	public Object getLocationType() {
		return locationType;
	}

	public void setLocationType(Object locationType) {
		this.locationType = locationType;
	}
	
}
