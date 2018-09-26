/************************************************************************
 * Copyright 2007-2018 - General Electric Company, All Rights Reserved
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

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.eclipse.emf.common.util.URI;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IContainer;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.IResourceDescription.Delta;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.impl.DefaultResourceDescriptionManager;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.inject.Inject;

/**
 * The basic logic was copied as is from the {@link DefaultResourceDescriptionManager}.
 * 
 * This is customized to handle container projects gracefully.
 * When deciding whether a candidate is affected by a delta, we <b>must</b> consider the container project.
 */
public class SadlResourceDescriptionManager extends DefaultResourceDescriptionManager {
	
	@Inject
	private UserDataHelper userDataHelper;

    @Override
	public boolean isAffected(Collection<Delta> deltas, IResourceDescription candidate, IResourceDescriptions context) {
        Set<URI> outgoingReferences = getDescriptionUtils().collectOutgoingReferences(candidate);
        if (!outgoingReferences.isEmpty()) {
	        for (IResourceDescription.Delta delta : deltas)
	            if (hasChanges(delta, candidate) && outgoingReferences.contains(delta.getUri()))
	                return true;
        }
        // this is a tradeoff - we could either check whether a given delta uri is contained
        // in a reachable container and check for intersecting names afterwards, or we can do
        // the other way round
        // unfortunately there is no way to decide reliably which algorithm scales better
        // note that this method is called for each description so we have something like a 
        // number of deltas x number of resources which is not really nice
        List<IContainer> containers = null;
        Collection<QualifiedName> importedNames = getImportedNames(candidate);
        Supplier<String> candidateProjectUriSupplier = Suppliers.memoize(() -> getContainerProjectUri(candidate));
        for (IResourceDescription.Delta delta : deltas) {
			if (hasChanges(delta, candidate)) {
				// not a java resource - delta's resource should be contained in a visible container
				// as long as we did not delete the resource
				URI uri = delta.getUri();
				if ((uri.isPlatform() || uri.isArchive()) && delta.getNew() != null) { 
					if (containers == null)
						containers = getContainerManager().getVisibleContainers(candidate, context);
					boolean descriptionIsContained = false;
					for(int i = 0; i < containers.size() && !descriptionIsContained; i++) {
						descriptionIsContained = containers.get(i).hasResourceDescription(uri);
					}
					if (!descriptionIsContained)
						return false;
				}
				// START - This is the difference compared to the `DefaultResourceDescriptionManager` logic.
				String deltaProjectUri = getContainerProjectUri(delta);
				String currentProjectUri = candidateProjectUriSupplier.get();
				if (Objects.equals(deltaProjectUri, currentProjectUri)) {
					// END
					if (isAffected(importedNames, delta.getNew()) || isAffected(importedNames, delta.getOld())) {
						return true;
					}					
				}
			}
        }
        return false;
    }

    protected String getContainerProjectUri(IResourceDescription description) {
    	return userDataHelper.getContainerProjectUri(description).orNull();
    }
    
    protected String getContainerProjectUri(IResourceDescription.Delta delta) {
    	String uri = userDataHelper.getContainerProjectUri(delta.getNew()).orNull();
    	if (uri == null) {
    		uri = userDataHelper.getContainerProjectUri(delta.getOld()).orNull();
    	}
    	return uri;
    }
	
}
