/************************************************************************
 * Copyright © 2007-2015 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.resource;

import java.util.List;
import java.util.Set;

import javax.inject.Inject;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.DerivedStateAwareResource;
import org.eclipse.xtext.resource.IDerivedStateComputer;

import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlFactory;
import com.ge.research.sadl.sadl.SadlPackage;
import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;


/**
 * In SADL a ResourceByName may link to a ResourceName defined elsewhere, but it is also possible that it
 * refers to a <i>variable</i> in a Rule, Test or Query. Such a variable does not need to be declared, thus 
 * there is no ResourceName declared.
 * Therefore this class will install virtual ResourceNames in the resource's contents. The names of these
 * ResourceNames are prefixed by the name of its container.
 * @author thoms
 *
 */
public class SadlDerivedStateComputer implements IDerivedStateComputer {
	@Inject
	private IQualifiedNameProvider qnProvider;

	@Override
	public void installDerivedState(DerivedStateAwareResource resource, boolean preLinkingPhase) {
		installResourceNamesForVariables(resource);
	}

	@Override
	public void discardDerivedState(DerivedStateAwareResource resource) {
		if (resource.getContents().isEmpty()) return;
		List<ResourceName> toRemove = Lists.newArrayList(Iterables.filter(resource.getContents(), ResourceName.class));
		resource.getContents().removeAll(toRemove);
	}

	/**
	 * This method will create a ResourceName instance for each ResourceByName element found within the context of a Rule, Query or Test.
	 * The ResourceName will have a name which is prefixed by the name of its context container and are installed to the root of the
	 * resource's contents.
	 */
	private void installResourceNamesForVariables (DerivedStateAwareResource resource) {
		Model m = (Model) resource.getContents().get(0);
		/** Filter for type Rule,Query or Test*/
		final Predicate<EObject> filter = new Predicate<EObject>() {
			private final Set<EClass> filteredEClasses = Sets.newHashSet(SadlPackage.Literals.RULE, SadlPackage.Literals.TEST, SadlPackage.Literals.QUERY);
			@Override
			public boolean apply(EObject input) {
				return filteredEClasses.contains(input.eClass());
			}
		};
		List<ResourceName> result = Lists.newArrayList();
		// Iterate over elements of type Rule,Test,Query
		for (EObject ctx: Iterables.filter(m.getElements(), filter)) {
			QualifiedName qn = qnProvider.getFullyQualifiedName(ctx);
			
			for (ResourceByName rbn: EcoreUtil2.eAllOfType(ctx, ResourceByName.class)) {
				ResourceName rn = SadlFactory.eINSTANCE.createResourceName();
				String rbnName = NodeModelUtils.getTokenText(NodeModelUtils.getNode(rbn));
				String name = qn.toString()+"_"+rbnName;
				rn.setName(name);
				result.add(rn);
			}
		}
		// now install ResourceNames into resource
		m.eResource().getContents().addAll(result);
	}
	
}
