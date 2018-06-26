/************************************************************************
 * Copyright \u00a9 2007-2018 - General Electric Company, All Rights Reserved
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

import java.util.Iterator;

import com.ge.research.sadl.reasoner.InvalidTypeException;

/**
 * Class to capture the content of an RDF triple
 * @author 200005201
 *
 */
public class TripleElement extends GraphPatternElement {
	private Node subject = null;
	private Node predicate = null;
	private Node object = null;
	
	public enum TripleModifierType {None, Not, Only, NotOnly, Assignment}	// special kinds of triples
	private TripleModifierType modifierType = TripleModifierType.None;		// default

	public enum TripleSourceType {SPV, PSnewV, PSV, VPS, ITC, SCofC}	// type of source
	private TripleSourceType sourceType;	// what was the source of this TripleElement?
	
	/**
	 * Null argument constructor
	 */
	public TripleElement() {
		super();
	}
	
	/**
	 * Constructor with subject, predicate, and object as arguments
	 * @param subj
	 * @param pred
	 * @param obj
	 */
	public TripleElement(Node subj, Node pred, Node obj) {
		super();
		setSubject(subj);
		setPredicate(pred);
		setObject(obj);
	}
	
	/**
	 * Constructor with subject, predicate, and object as arguments
	 * @param subj
	 * @param pred
	 * @param obj
	 * @throws InvalidTypeException 
	 */
	public TripleElement(Node subj, Node pred, Node obj, TripleSourceType type) throws InvalidTypeException {
		super();
		setSubject(subj);
		setPredicate(pred);
		setObject(obj);
		setSourceType(type);;
	}
	
	/**
	 * Set the TripleElement's subject
	 * @param subject
	 */
	public void setSubject(Node subject) {
		this.subject = subject;
	}
	
	/**
	 * Get the TripleElement's subject
	 * @return
	 */
	public Node getSubject() {
		return subject;
	}

	/**
	 * Set the TripleElement's predicate
	 * @param predicate
	 */
	public void setPredicate(Node predicate) {
		this.predicate = predicate;
	}
	
	/**
	 * Get the TripleElement's predicate
	 * @return
	 */
	public Node getPredicate() {
		return predicate;
	}

	/**
	 * Set the TripleElement's object
	 * @param object
	 */
	public void setObject(Node object) {
		this.object = object;
	}
	
	/**
	 * Get the TripleElement's object
	 * @return
	 */
	public Node getObject() {
		return object;
	}
	
	/**
	 * Set the TripleModifierType for a special TripleElement type
	 * @param _type
	 * @throws InvalidTypeException
	 */
	public void setType(TripleModifierType _type) throws InvalidTypeException {
		if (_type == null) {
			throw new InvalidTypeException("Triple modifier type cannot be null!");
		}
		this.modifierType = _type;
	}

	/**
	 * Get the TripleModifierType for the TripleElement
	 * @return
	 */
	public TripleModifierType getModifierType() {
		return modifierType;
	}

	@Override
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
		if (!getModifierType().equals(TripleModifierType.None) && !getModifierType().equals(TripleModifierType.Assignment)) {
			sb.insert(0, "(");
			if(getModifierType().equals(TripleModifierType.Not)) {
				sb.insert(0, getModifierType().toString().toLowerCase());
			}else {
				sb.insert(0, getModifierType().toString());
			}
			sb.append(")");
		}
		sb.append(")");
		return sb.toString();
	}
	
	@Override
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
		if (!getModifierType().equals(TripleModifierType.None) && !getModifierType().equals(TripleModifierType.Assignment)) {
			sb.insert(0, "(");
			if(getModifierType().equals(TripleModifierType.Not)) {
				sb.insert(0, getModifierType().toString().toLowerCase());
			}else {
				sb.insert(0, getModifierType().toString());
			}
			sb.append(")");
		}
		sb.append(")");
		return sb.toString();
	}
	
	@Override
	public String toDescriptiveString() {
		StringBuilder sb = new StringBuilder("rdf(");
		if (getLeftImpliedPropertyUsed() != null || getRightImpliedPropertyUsed() != null || getExpandedPropertiesToBeUsed() != null) {
			sb.append("(");
			boolean needComma = false;
			if (getLeftImpliedPropertyUsed() != null) {
				sb.append("leftImpliedProperty ");
				sb.append(getLeftImpliedPropertyUsed().toDescriptiveString());
				needComma = true;
			}
			else if (getRightImpliedPropertyUsed() != null) { // only left or right should exist (at most)
				sb.append("rightImpliedProperty ");
				sb.append(getRightImpliedPropertyUsed().toDescriptiveString());
				needComma = true;
			}
			if (getExpandedPropertiesToBeUsed() != null) {
				if (needComma) sb.append(",");
				needComma = false;
				sb.append("expandedProperties [");
				Iterator<NamedNode> epitr = getExpandedPropertiesToBeUsed().iterator();
				while (epitr.hasNext()) {
					if (needComma) sb.append(",");
					sb.append(epitr.next().toDescriptiveString());
					needComma = true;
				}
				sb.append("]");
			}
			sb.append(")");
		}
		if (subject instanceof NamedNode && ((NamedNode)subject).getMissingTripleReplacement() != null) {
			sb.append(subject.toString());
		}
		else {
			sb.append(subject != null ? subject.toDescriptiveString() : "null");
		}
		sb.append(", ");
		if (predicate instanceof NamedNode && ((NamedNode)predicate).getMissingTripleReplacement() != null) {
			sb.append(predicate.toString());
		}
		else {
			sb.append(predicate != null ? predicate.toDescriptiveString() : "null");
		}
		sb.append(", ");
		if (object instanceof NamedNode && ((NamedNode)object).getMissingTripleReplacement() != null) {
			sb.append(object.toString());
		}
		else {
			sb.append(object != null ? object.toDescriptiveString() : "null");
		}
		if (getNext() != null) {
			sb.append(" . ");
			sb.append(getNext().toDescriptiveString());
		}
		if (!getModifierType().equals(TripleModifierType.None)) {
			sb.insert(0, "(");
			if(getModifierType().equals(TripleModifierType.Not)) {
				sb.insert(0, getModifierType().toString().toLowerCase());
			}else {
				sb.insert(0, getModifierType().toString());
			}
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
