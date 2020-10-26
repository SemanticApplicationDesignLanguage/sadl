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
package com.ge.research.sadl.naming;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.util.SimpleAttributeResolver;

import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.sadl.Query;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.Test;
import com.google.inject.Inject;

public class SadlSimpleNameProvider extends IQualifiedNameProvider.AbstractImpl {
	@Inject
	private IQualifiedNameConverter qualifiedNameConverter;

	public QualifiedName getFullyQualifiedName(EObject obj) {
		String name = null;

		if (obj instanceof Model) {
			Model m = (Model) obj;
			if (m.getModelName()!=null && m.getModelName().getBaseUri()!=null) {
				return QualifiedName.create(m.getModelName().getBaseUri());
			}
		}
		else if (obj instanceof ResourceName) {
			Model m = EcoreUtil2.getContainerOfType(obj, Model.class);
			// ResourceNames derived by SadlLinkingService for variables do not have a Model container, but are directly in the root
			// of the resource
			if (m == null) {
				String nm = ((ResourceName)obj).getName();
				if (nm.contains(":")) {
					// remove context_ here? or should the name not ever have gotten created?
				}
				return null;
			}
			if (m.getModelName()!=null && m.getModelName().getAlias()!=null) {

				return QualifiedName.create(m.getModelName().getAlias(), ((ResourceName)obj).getName());
			} else {
				return QualifiedName.create(((ResourceName)obj).getName());
			}
		} else if (obj instanceof Test || obj instanceof Query) {
			return QualifiedName.create(obj.eClass().getName()+getIndexInContainer(obj));
		} else {
			name = SimpleAttributeResolver.NAME_RESOLVER.apply(obj);
		}
		if (name == null)
			return null;
		return qualifiedNameConverter.toQualifiedName(name);
	}

	private int getIndexInContainer (EObject ctx) {
		int index = -1;
		for (EObject obj: ctx.eContainer().eContents()) {
			if (obj.getClass().equals(ctx.getClass())) {
				index ++;
				if (obj == ctx) break;
			}
		}
		return index;
	}
	

}