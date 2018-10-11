/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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

import com.google.common.base.Predicate
import com.google.inject.Singleton
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.resource.IEObjectDescription

import static com.google.common.base.Predicates.alwaysTrue

/**
 * Provides a filter predicate to restrict the global scope.
 * Accepts everything.
 */
@Singleton
class GlobalScopeProviderFilterProvider {
	
	def Predicate<IEObjectDescription> getPredicate(Resource it) {
		return alwaysTrue;
	}
	
}
