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
import com.google.common.base.Optional
import com.google.common.collect.ImmutableMap
import com.google.inject.Singleton
import java.util.Collections
import java.util.Map
import org.eclipse.emf.ecore.EObject
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

	def dispatch Map<String, String> createUserData(EObject it) {
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
		return getSadlModelData(USER_DATA_ALIAS);
	}

	private def Optional<String> getSadlModelData(IEObjectDescription desc, String key) {
		if (SADL_MODEL === desc.EClass) {
			return Optional.fromNullable(desc.getUserData(key));
		}
		return Optional.absent;
	}

}
