/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.query

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlResource
import com.google.inject.Inject
import com.google.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.scoping.IScopeProvider

import static com.ge.research.sadl.sADL.SADLPackage.Literals.*
import static com.ge.research.sadl.scoping.ErrorAddingLinkingService.ERROR

import static extension org.eclipse.xtext.EcoreUtil2.getContainerOfType

/**
 * Helper for finding and retrieving a {@link QueryStatement SADL query} by its name.
 * 
 * @author akos.kitta 
 */
@Singleton
class SadlQueryHelper {

	@Inject
	extension IScopeProvider;

	@Inject
	extension IQualifiedNameConverter;

	@Inject
	extension DeclarationExtensions;

	/**
	 * Returns the name declaration (SADL resource) of the query statement argument.
	 * Returns {@code null} if the argument is {@code null}, or when the query does not
	 * have a name. Also returns with {@code null} if the underlying AST is broken. 
	 */
	def dispatch SadlResource getQueryName(QueryStatement it) {
		return name.declaration;
	}

	def dispatch SadlResource getQueryName(Void it) {
		return null;
	}

	/**
	 * Returns the SADL query statement identified by its name. Returns {@code null}
	 * if the query does not exist with the given name argument, or it is ambiguous and a
	 * prefix alias has to be used. For instance, when a model imports two other models
	 * and both imported models has the same named query. 
	 */
	def dispatch QueryStatement findQueryByName(Resource resource, String name) {
		return resource.contents.head.findQueryByName(name);
	}

	def dispatch QueryStatement findQueryByName(SadlModel model, String name) {
		return model.doFindQueryByName(name);
	}

	def dispatch QueryStatement findQueryByName(EObject object, String name) {
		return object.getContainerOfType(SadlModel).findQueryByName(name);
	}

	def dispatch QueryStatement findQueryByName(Void any, String name) {
		return null;
	}

	private def QueryStatement doFindQueryByName(SadlModel it, String name) {
		return getScope(SADL_RESOURCE__NAME).getElements(name.toQualifiedName).filter[getUserData(ERROR).nullOrEmpty].
			map[EObjectOrProxy].map[getContainerOfType(QueryStatement)].filterNull.head;
	}

}
