/************************************************************************
 * Copyright © 2007-2014 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.scoping;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.impl.AliasedEObjectDescription;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.scoping.impl.SimpleLocalScopeProvider;
import org.eclipse.xtext.scoping.impl.SimpleScope;

import com.ge.research.sadl.sadl.SadlPackage;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;


/**
 * Creates aliased object descriptions for ResourceName instances in the global scope: All ResourceNames are available with their simple name also.
 * @author thoms
 */
public class SadlLocalScopeProvider extends SimpleLocalScopeProvider {
	@Override
	protected IScope getGlobalScope(Resource context, EReference reference) {
		IScope globalScope = super.getGlobalScope(context, reference);

		// Create a filtered view on all ResourceNames from the global scope that have a 2 segment qualified name (namespace:name)
		Iterable<IEObjectDescription> resourceNameDescriptions = Iterables.filter(globalScope.getAllElements(), new Predicate<IEObjectDescription>() {
			@Override
			public boolean apply(IEObjectDescription input) {
				return input.getEClass()==SadlPackage.Literals.RESOURCE_NAME && input.getQualifiedName().getSegmentCount()==2;
			}
		});
		// Map these object descriptions to their simple name
		Iterable<IEObjectDescription> aliasedResourceNameDescriptions = Iterables.transform(resourceNameDescriptions, new Function<IEObjectDescription,IEObjectDescription>(){
			@Override
			public IEObjectDescription apply(IEObjectDescription input) {
				return new AliasedEObjectDescription(QualifiedName.create(input.getQualifiedName().getLastSegment()), input);
			}});

		return new SimpleScope(globalScope, aliasedResourceNameDescriptions);
	}
}
