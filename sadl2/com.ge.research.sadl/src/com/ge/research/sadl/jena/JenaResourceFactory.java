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

package com.ge.research.sadl.jena;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.IResourceFactory;

import com.google.inject.Inject;
import com.google.inject.Provider;

public class JenaResourceFactory implements IResourceFactory {

    private Provider<JenaResource> provider;
    
    @Inject
    public JenaResourceFactory(Provider<JenaResource> provider) {
        this.provider = provider;
    }

    public Resource createResource(URI uri) {
        JenaResource resource = provider.get();
        resource.setURI(uri);
        return resource;
    }
}
