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

import com.ge.research.sadl.reasoner.InvalidTypeException;

public class TripleElement extends GraphPatternElement {
	private Node subject = null;
	private Node predicate = null;
	private Node object = null;
	
	public enum TripleModifierType {None, Not, Only, NotOnly}
	private TripleModifierType modifierType = TripleModifierType.None;		// default

	public enum TripleSourceType {SPV, PSnewV, PSV, VPS, ITC, SCofC}
	private TripleSourceType sourceType;	// what kind of triple was this translated from?
	
	public TripleElement() {
		super();
	}
	
	public TripleElement(Node subj, Node pred, Node obj) {
		super();
		setSubject(subj);
		setPredicate(pred);
		setObject(obj);
	}
	
	public void setSubject(Node subject) {
		this.subject = subject;
//		if (subject instanceof VariableNode) {
//			((VariableNode)subject).incrementReferences();
//		}
	}
	public Node getSubject() {
		return subject;
	}

	public void setPredicate(Node predicate) {
		this.predicate = predicate;
	}
	public Node getPredicate() {
		return predicate;
	}

	public void setObject(Node object) {
		this.object = object;
	}
	public Node getObject() {
		return object;
	}
	
	public void setType(TripleModifierType _type) throws InvalidTypeException {
		if (_type == null) {
			throw new InvalidTypeException("Triple modifier type cannot be null!");
		}
		this.modifierType = _type;
	}

	public TripleModifierType getModifierType() {
		return modifierType;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder("rdf(");
		sb.append(subject != null ? subject.toString() : "null");
		sb.append(", ");
		sb.append(predicate != null ? predicate.toString() : "null");
		sb.append(", ");
		sb.append(object != null ? object.toString() : "null");
		if (getNext() != null) {
			sb.append(" . ");
			sb.append(getNext().toString());
		}
		if (!getModifierType().equals(TripleModifierType.None)) {
			sb.insert(0, "(");
			sb.insert(0, getModifierType().toString());
			sb.append(")");
		}
		sb.append(")");
		return sb.toString();
	}
	
	public String toFullyQualifiedString() {
		StringBuilder sb = new StringBuilder("rdf(");
		sb.append(subject != null ? subject.toFullyQualifiedString() : "null");
		sb.append(", ");
		sb.append(predicate != null ? predicate.toFullyQualifiedString() : "null");
		sb.append(", ");
		sb.append(object != null ? object.toFullyQualifiedString() : "null");
		if (getNext() != null) {
			sb.append(" . ");
			sb.append(getNext().toFullyQualifiedString());
		}
		if (!getModifierType().equals(TripleModifierType.None)) {
			sb.insert(0, "(");
			sb.insert(0, getModifierType().toString());
			sb.append(")");
		}
		sb.append(")");
		return sb.toString();
	}
	
	@Override
	public String toDescriptiveString() {
		StringBuilder sb = new StringBuilder("rdf(");
		sb.append(subject != null ? subject.toDescriptiveString() : "null");
		sb.append(", ");
		sb.append(predicate != null ? predicate.toDescriptiveString() : "null");
		sb.append(", ");
		sb.append(object != null ? object.toDescriptiveString() : "null");
		if (getNext() != null) {
			sb.append(" . ");
			sb.append(getNext().toDescriptiveString());
		}
		if (!getModifierType().equals(TripleModifierType.None)) {
			sb.insert(0, "(");
			sb.insert(0, getModifierType().toString());
			sb.append(")");
		}
		sb.append(")");
		return sb.toString();
	}
	
	@Override
	public boolean equals(Object other) {
		if (other == null || !(other instanceof TripleElement)) {
			return false;
		}
		Node s = getSubject();
		Node so = ((TripleElement) other).getSubject();
		if (s != null && (so == null || !s.equals(so))) {
			return false;
		}
		Node p = getPredicate();
		Node po = ((TripleElement) other).getPredicate();
		if (p != null && (po == null || !p.equals(po))) {
			return false;
		}
		Node o = getObject();
		Node oo = ((TripleElement) other).getObject();
		if (o != null && (oo == null || !o.equals(oo))) {
			return false;
		}
		return true;
	}

	public void setSourceType(TripleSourceType sourceType) {
		this.sourceType = sourceType;
	}

	public TripleSourceType getSourceType() {
		return sourceType;
	}
}
