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

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlResource
import com.google.common.collect.ImmutableSet
import com.google.common.collect.Sets
import java.util.Collection
import org.eclipse.xtext.resource.ForwardingEObjectDescription
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.resource.XtextResource

import static extension com.google.common.collect.Iterables.concat

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

	/**
	 * Registers the {@code conflicting} argument among the alternatives. Keeps the order.
	 * Does nothing but returns with {@code this} argument if the {@code conflicting} is {@code null}. 
	 */
	def IEObjectDescription addConflicting(IEObjectDescription conflicting) {
		if (conflicting !== null) {
			alternatives.add(conflicting);
		}
		return this;
	}

	protected def getAlternatives() {
		// Here, we are in the middle of scoping and linking, hence we cannot resolve the `alias` but the base URI.
		return allDescriptions.map['''«baseUri» «name»'''].join(',');
	}
	
	def getEObjDescAlternatives() {
		return alternatives;
	}

	protected def getMessage() {
		return '''Ambiguously imported name '«name»' from «allDescriptions.map["'" + baseUri + "'"].join(', ')». Please use a prefix to disambiguate name.''';
	}

	def getAllDescriptions() {
		return ImmutableSet.of(this).concat(alternatives);
	}

	protected def getBaseUri(IEObjectDescription it) {
		return model.baseUri;
	}

	protected def getConcreteName(IEObjectDescription it) {
		val object = EObjectOrProxy;
		if (!object.eIsProxy && SADLPackage.Literals.SADL_RESOURCE.isSuperTypeOf(object.eClass)) {
			return it.declarationExtensions.getConcreteName(object as SadlResource, false);
		}
		return name;
	}

	protected def getDeclarationExtensions(IEObjectDescription it) {
		val resource = EObjectOrProxy.eResource;
		if (resource instanceof XtextResource) {
			return resource.resourceServiceProvider.get(DeclarationExtensions);
		}
		return new DeclarationExtensions();
	}

	protected def getModel(IEObjectDescription it) {
		return EObjectOrProxy.eResource.allContents.filter(SadlModel).head;
	}

}
