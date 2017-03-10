/************************************************************************
 * Copyright © 2007-2010 - General Electric Company, All Rights Reserved
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

/***********************************************************************
 * $Last revised by: crapo $
 * $Revision: 1.1 $ Last modified on   $Date: 2014/05/05 15:09:43 $
 ***********************************************************************/

package com.ge.research.sadl.scoping;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.EObjectDescription;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.scoping.impl.ImportUriGlobalScopeProvider;
import org.eclipse.xtext.scoping.impl.SimpleScope;

import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.model.ImportMapping;
import com.google.common.base.Function;
import com.google.common.base.Predicates;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.inject.Inject;

public class SadlGlobalScopeProvider extends ImportUriGlobalScopeProvider {

    @Inject
    private IQualifiedNameProvider nameProvider;

    @Inject
    private SadlModelManager visitor;

    public IScope getScope(EObject context, EReference reference) {
    	Collection<ImportMapping> imports = visitor.getModelImportMappings();
    	Iterator<ImportMapping> mappingsItr = imports != null ? imports.iterator() : null;
        IScope scope = IScope.NULLSCOPE;
        if (mappingsItr != null) {
	    	while (mappingsItr.hasNext()) {
	            // expose the names in the Jena model for this import.
	    		ImportMapping mapping = mappingsItr.next();
	            scope = createJenaScope(scope, context, URI.createURI(mapping.getActualURL()), mapping.getPrefix());
	    	}
        }
        return scope;
    }

    protected IScope createJenaScope(IScope scope, EObject context, URI uri, String prefix) {
        Resource res = context.eResource().getResourceSet()
                .getResource(uri, true);
        return new SimpleScope(scope, computeExportedObjects(res, prefix));
    }

    // see
    // org.eclipse.xtext.resource.impl.DefaultResourceDescription.computeExportedObjects()
    protected List<IEObjectDescription> computeExportedObjects(
            final Resource res, final String prefix) {
        Iterable<EObject> contents1 = new Iterable<EObject>() {
            public Iterator<EObject> iterator() {
                return EcoreUtil.getAllProperContents(res, true);
            }
        };
        Iterable<EObject> contents2 = new Iterable<EObject>() {
            public Iterator<EObject> iterator() {
                return EcoreUtil.getAllProperContents(res, true);
            }
        };
        Iterable<IEObjectDescription> pass1 = Iterables.transform(contents1,
                new Function<EObject, IEObjectDescription>() {
                    public IEObjectDescription apply(EObject from) {
                        return createIEObjectDescription(from);
                    }
                });
        Iterable<IEObjectDescription> pass2 = Iterables.transform(contents2,
                new Function<EObject, IEObjectDescription>() {
                    public IEObjectDescription apply(EObject from) {
                        return createIEObjectDescription(from, prefix);
                    }
                });
        Iterable<IEObjectDescription> result = Iterables.concat(pass1, pass2);
        Iterable<IEObjectDescription> filter = Iterables.filter(result, Predicates.notNull());
        return Lists.newArrayList(filter);
    }

    // see
    // org.eclipse.xtext.resource.impl.DefaultResourceDescription.createIEObjectDescription(EObject)
    protected IEObjectDescription createIEObjectDescription(EObject from) {
        if (nameProvider == null)
            return null;
        QualifiedName qualifiedName = nameProvider.getFullyQualifiedName(from);
        if (qualifiedName != null) {
            return EObjectDescription.create(qualifiedName, from);
        }
        return null;
    }

    protected IEObjectDescription createIEObjectDescription(EObject from, String prefix) {
        if (nameProvider == null)
            return null;
        QualifiedName qualifiedName = nameProvider.getFullyQualifiedName(from);
        if (qualifiedName != null && prefix != null) {
        	if (qualifiedName.startsWith(QualifiedName.create(prefix + ":"))) {
        		qualifiedName = QualifiedName.create(qualifiedName.toString().substring(prefix.length() + 1));
                return EObjectDescription.create(qualifiedName, from);
        	}
//            qualifiedName = prefix + ":" + qualifiedName;
        }
        return null;
    }

}
