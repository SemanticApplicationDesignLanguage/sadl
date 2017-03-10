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

import java.net.MalformedURLException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.scoping.impl.ImportUriResolver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.builder.ResourceManager;
import com.google.inject.Inject;

/**
 * Overrides Xtext's ImportUriResolver so that we can remove
 * file:// from URI strings.
 */
public class SadlImportUriResolver extends ImportUriResolver {

    private static final Logger logger = LoggerFactory.getLogger(ImportUriResolver.class);

    @Inject
    private SadlModelManager visitor;
	
    /**
     * When we resolve an import URI, transform the URI to the absolute
     * file URI of the corresponding OWL file.  We need to handle many
     * variations of import URIs: relative filenames without any file:/
     * or http:/, file:/ urls, and http:/ urls.
     *
     * @param object Object that contains an import URI.
     * @return Import URI resolved to file URI of OWL file.
     */
    public String resolve(EObject object) {
        String importUri = super.resolve(object);
        String owlUri = importUri;

        if (importUri != null) {
            URI resourceURI = object.eResource().getURI();
            URI uri = URI.createURI(importUri);

            // check for a V1 compatibility issue
            if (uri.isFile()) {
            	String rawSadlFilename = uri.toString();
            	if (rawSadlFilename.startsWith(ResourceManager.FILE_URL_PREFIX) &&
            			rawSadlFilename.endsWith(ResourceManager.SADLEXT)) {
            		rawSadlFilename = rawSadlFilename.substring(ResourceManager.FILE_URL_PREFIX.length());
            		int lastSegmentDivider = rawSadlFilename.lastIndexOf("/");
            		if (lastSegmentDivider > 0) {
            			rawSadlFilename = rawSadlFilename.substring(lastSegmentDivider + 1);
            		}
            		uri = URI.createFileURI(rawSadlFilename);
            	}
                // Convert the file URI to a platform URI.
                uri = uri.resolve(resourceURI);
                // Convert the platform URI to an absolute URI.
                uri = ResourceManager.convertPlatformUriToAbsoluteUri(uri);
                // Convert any SADL URI to an OWL URI.
        	    if (ResourceManager.SADLEXT.equalsIgnoreCase(uri.fileExtension())) {
        	        try {
                        uri = ResourceManager.validateAndReturnOwlUrlOfSadlUri(uri);
                    }
        	        catch (CoreException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
        	    }
                // Convert the OWL URI back to a string.
                owlUri = uri.toString();
            }
            else {
                // Look up the public URI in the policy file and get the OWL URI.
            	try {
					owlUri = visitor.getAltUrl(importUri, resourceURI);
				} catch (MalformedURLException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
            }
            logger.debug("Resolved import '{}' to '{}'", importUri, owlUri);
        }

        return owlUri;
    }
}
