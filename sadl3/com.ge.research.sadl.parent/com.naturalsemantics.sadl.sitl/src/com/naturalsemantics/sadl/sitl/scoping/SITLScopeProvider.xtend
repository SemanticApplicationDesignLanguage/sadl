/************************************************************************
 * 
 * Project: SADL
 * Copyright 2007-2022 - General Electric Company, All Rights Reserved
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
/*
 * SADL Extension for SADL Import Template Language (SITL)
 * Copyright 2022 - Natural Semantics, LLC, All Rights Reserved
 */
package com.naturalsemantics.sadl.sitl.scoping;

import com.ge.research.sadl.scoping.SADLScopeProvider
import com.google.common.base.Predicate
import com.google.inject.Provider
import java.util.Map
import java.util.Set
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.scoping.IScope

/**
 * This class contains custom scoping description.
 * 
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#scoping
 * on how and when to use it.
 */
class SITLScopeProvider extends SADLScopeProvider {

	protected override IScope createResourceScope(Resource resource, String alias, Set<Resource> importedResources, Predicate<EObject> isIncluded) {
		val provider = [
			val shouldWrap = importedResources.empty
			if (!importedResources.add(resource)) {
				return IScope.NULLSCOPE
			}
			
			var newParent = createImportScope(resource, importedResources)
			if (shouldWrap) {
				newParent = wrap(newParent)
			}
			val importScope = newParent;
			val aliasToUse = alias ?: resource.getAlias
			val namespace = if(aliasToUse !== null) QualifiedName.create(aliasToUse) else null
						
			// finally all the rest
			newParent = internalGetLocalResourceScope(resource, namespace, newParent, importScope, isIncluded)
			return newParent
		] as Provider<IScope>;
		val key = 'resource_scope' -> alias;
		return cache.get(key, resource, provider);
	}

	protected override boolean canAddToScope(Map<QualifiedName, IEObjectDescription> scope, QualifiedName qn, EObject obj) {
		if (!obj.eResource.URI.fileExtension.equals("sitl")) {
			return super.canAddToScope(scope, qn, obj);
		}
		return !scope.containsKey(qn);
	}
	
}
