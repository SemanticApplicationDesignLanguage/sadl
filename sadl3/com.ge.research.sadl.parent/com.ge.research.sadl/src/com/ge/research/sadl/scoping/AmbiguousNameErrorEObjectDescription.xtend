/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.scoping

import com.google.common.collect.ImmutableSet
import com.google.common.collect.Sets
import java.util.Collection
import org.eclipse.xtext.resource.ForwardingEObjectDescription
import org.eclipse.xtext.resource.IEObjectDescription

import static extension com.google.common.collect.Iterables.concat
import com.ge.research.sadl.sADL.SadlModel

/**
 * EObject description that represents an ambiguous name error with alternatives.
 */
class AmbiguousNameErrorEObjectDescription extends ForwardingEObjectDescription {

	/**
	 * The issue code. Used for the quick fixes too. 
	 */
	public static val AMBIGUOUS_NAME_ISSUE_CODE = "ambiguous_import";

	/**
	 * User data key for the error.
	 */
	public static val AMBIGUOUS_NAME_ERROR = "AMBIGUOUS_NAME_ERROR";

	/**
	 * User data key for the alternatives that can be used for the quick fix provider.
	 */
	public static val AMBIGUOUS_NAME_ALTERNATIVES = "ALTERNATIVES";

	/**
	 * Wraps the ambiguous name description into another by appending the {@code conflicting} argument to
	 * the {@code alternative}s of the original. Keeps the order.
	 * Does nothing but returns with the {@code it} argument if the {@code conflicting} is {@code null}.
	 */
	static def addConflicting(AmbiguousNameErrorEObjectDescription it, IEObjectDescription conflicting) {
		if (conflicting === null) {
			return it;
		}
		val copy = new AmbiguousNameErrorEObjectDescription(it);
		copy.alternatives.addAll(alternatives);
		copy.alternatives.add(conflicting);
		return copy;
	}

	val Collection<IEObjectDescription> alternatives;

	private new(IEObjectDescription delegate) {
		super(delegate);
		alternatives = Sets.newLinkedHashSet;
	}

	new(IEObjectDescription delegate, IEObjectDescription conflicting) {
		this(delegate);
		alternatives.add(conflicting);
	}

	override getUserData(String key) {
		return switch (key) {
			case AMBIGUOUS_NAME_ERROR: getMessage()
			case AMBIGUOUS_NAME_ALTERNATIVES: getAlternatives()
			default: super.getUserData(key)
		}
	}

	protected def getAlternatives() {
		return allDescriptions.map[name.toString].join(',');
	}

	protected def getMessage() {
		return '''Ambiguously imported name '«name»' from «allDescriptions.map["'" + baseUri + "'"].join(', ')». Please use a prefix to disambiguate name.''';
	}

	protected def getAllDescriptions() {
		return ImmutableSet.of(this).concat(alternatives);
	}
	
	protected def getBaseUri(IEObjectDescription it) {
		return EObjectOrProxy.eResource.allContents.filter(SadlModel).head.baseUri;
	}

}
