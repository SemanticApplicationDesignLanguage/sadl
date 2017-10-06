/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.resource

import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlProperty
import com.ge.research.sadl.sADL.SadlResource
import com.google.common.base.Optional
import com.google.common.collect.ImmutableMap
import com.google.inject.Singleton
import java.util.Collections
import java.util.Map
import org.eclipse.emf.ecore.EClass
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.IEObjectDescription

import static com.ge.research.sadl.sADL.SADLPackage.Literals.*

/**
 * Helper for creating and accessing user data on the EObject descriptions.
 * 
 * @author akos.kitta
 */
@Singleton
class UserDataHelper {

	public static val USER_DATA_ALIAS = 'alias';
	public static val USER_DATE_PROPERTY_RESTRICTION = 'property_restriction';

	def dispatch Map<String, String> createUserData(EObject it) {
		return Collections.emptyMap;
	}

	def dispatch Map<String, String> createUserData(SadlResource it) {
		if (eContainer instanceof SadlProperty) {
			return (eContainer as SadlProperty).createUserData;
		}
		return Collections.emptyMap;
	}

	def dispatch Map<String, String> createUserData(SadlProperty it) {
		val node = NodeModelUtils.findActualNodeFor(it);
		if (node !== null && !node.text.nullOrEmpty) {
			val rawText = node.text
			// Replace all non-single whitespaces and EOL with a single space, so that when the user
			// performs any whitespce changes, the underlying EObjectDescription will be marked as unchanged. 
			val text = rawText.trim.replaceAll("\r?\n", " ").replaceAll(" +", " ");
			return Collections.singletonMap(USER_DATE_PROPERTY_RESTRICTION, text);
		}
		return Collections.emptyMap;
	}

	def dispatch Map<String, String> createUserData(SadlModel it) {
		val builder = ImmutableMap.builder;
		if (!alias.nullOrEmpty) {
			builder.put(USER_DATA_ALIAS, alias);
		}
		return builder.build;
	}

	def Optional<String> getAlias(IEObjectDescription it) {
		return getUserData(USER_DATA_ALIAS, SADL_MODEL);
	}

	def Optional<String> getPropertyRestriction(IEObjectDescription it) {
		return getUserData(USER_DATE_PROPERTY_RESTRICTION, null);
	}

	/**
	 * Tries to extract a desired given user object from the EObject description. EClass argument is just a minor performance tweak.
	 * If you would like to avoid that predicate evaluation, pass in {@code null}.
	 */
	private def Optional<String> getUserData(IEObjectDescription desc, String key, /*nullable*/ EClass expectedClass) {
		if (expectedClass !== null && expectedClass !== desc.EClass) {
			return Optional.absent;
		}
		return Optional.fromNullable(desc.getUserData(key));
	}

}
