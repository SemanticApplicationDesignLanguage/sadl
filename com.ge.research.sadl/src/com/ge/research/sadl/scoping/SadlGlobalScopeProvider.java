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
import java.util.LinkedHashSet;
import java.util.List;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.EObjectDescription;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.scoping.impl.DefaultGlobalScopeProvider;
import org.eclipse.xtext.scoping.impl.ImportUriGlobalScopeProvider;
import org.eclipse.xtext.scoping.impl.ImportUriResolver;
import org.eclipse.xtext.scoping.impl.SimpleScope;
import org.eclipse.xtext.util.IResourceScopeCache;

import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.model.ImportMapping;
import com.ge.research.sadl.resource.SadlEObjectDescription;
import com.ge.research.sadl.resource.SadlResourceDescription;
import com.ge.research.sadl.sadl.SadlPackage;
import com.google.common.base.Function;
import com.google.common.base.Predicates;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class SadlGlobalScopeProvider extends ImportUriGlobalScopeProvider {

    @Inject
    private IQualifiedNameProvider nameProvider;

    @Inject
    private SadlModelManager visitor;
    
	@Inject
	private ImportUriResolver importResolver;
	@Inject
	private IResourceScopeCache cache;
    @Inject DefaultGlobalScopeProvider defaultGlobalScopeProvider;
    @Inject IResourceDescription.Manager resourceDescriptionManager;

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

    
    /**
     * For each imported owl resource, import also the accoring sadl resource to have the elements on the global scope
     */
    @Override
    protected LinkedHashSet<URI> getImportedUris(final Resource resource) {
    	// copied from super method
    	// access ConfigurationManager to get all accessed URIs
//    	visitor.getConfigurationMgr(resource.getURI())
    	
    	IResourceDescription description = resourceDescriptionManager.getResourceDescription(resource);
    	if (description!=null && description instanceof SadlResourceDescription) {
    		LinkedHashSet<URI> uris = ((SadlResourceDescription)description).getImportedURIs();
    		return uris;
    	} else {
    		return super.getImportedUris(resource);
    	}
    	
//		return cache.get(ImportUriGlobalScopeProvider.class.getName(), resource, new Provider<LinkedHashSet<URI>>(){
//			public LinkedHashSet<URI> get() {
//				TreeIterator<EObject> iterator = resource.getAllContents();
//				final LinkedHashSet<URI> uniqueImportURIs = new LinkedHashSet<URI>(10);
//				while (iterator.hasNext()) {
//					EObject object = iterator.next();
//					String uri = importResolver.apply(object);
//					if (uri != null) {
//						URI importUri = URI.createURI(uri);
//						uniqueImportURIs.add(importUri);
//					}
//				}
//				Iterator<URI> uriIter = uniqueImportURIs.iterator();
//				while(uriIter.hasNext()) {
//					if (!EcoreUtil2.isValidUri(resource, uriIter.next()))
//						uriIter.remove();
//				}
//				// start customizing
//				uriIter = uniqueImportURIs.iterator();
//				while(uriIter.hasNext()) {
//					URI uri = uriIter.next();
//					if ("owl".equals(uri.fileExtension())) {
//						String resourceName = uri.trimFileExtension().appendFileExtension("sadl").lastSegment();
//						URI sadlUri = uri.trimSegments(2).appendSegment(resourceName);
//						if (EcoreUtil2.isValidUri(resource, sadlUri)) {
//							uniqueImportURIs.add(sadlUri);
//						}
//					}
//				}
//				
//				return uniqueImportURIs;
//			}
//		});
    }
}
