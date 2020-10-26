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

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.scoping.impl.ImportUriResolver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.ResourceManager;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.builder.SadlModelManagerProvider;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.sadl.Import;
import com.google.inject.Inject;

/**
 * Overrides Xtext's ImportUriResolver so that we can remove
 * file:// from URI strings.
 */
public class SadlImportUriResolver extends ImportUriResolver {

    private static final Logger logger = LoggerFactory.getLogger(ImportUriResolver.class);

    @Inject
	private SadlModelManagerProvider sadlModelManagerProvider;
	
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
    	if (!(object instanceof Import)) {
    		return null;
    	}
    	String importUriStr = super.resolve(object);
    	URI importedURI = URI.createURI(importUriStr);
    	if (importedURI.scheme()==null) {
    		importedURI = importedURI.resolve(object.eResource().getURI());
    	}

    	Resource importingResource = object.eResource();

    	if (importedURI.isPlatformResource()) {
    		String rawSadlFilename = importedURI.toString();
    		if (rawSadlFilename.startsWith(ResourceManager.FILE_URL_PREFIX) &&
    				rawSadlFilename.endsWith(ResourceManager.SADLEXT)) {
    			rawSadlFilename = rawSadlFilename.substring(ResourceManager.FILE_URL_PREFIX.length());
    			int lastSegmentDivider = rawSadlFilename.lastIndexOf("/");
    			if (lastSegmentDivider > 0) {
    				rawSadlFilename = rawSadlFilename.substring(lastSegmentDivider + 1);
    			}
    			importedURI = URI.createFileURI(rawSadlFilename);
    		}
    		importedURI = ResourceManager.convertPlatformUriToAbsoluteUri(importedURI);
    		// Convert any SADL URI to an OWL URI.
    		try {
    			if (ResourceManager.SADLEXT.equalsIgnoreCase(importedURI.fileExtension())) {
    				importedURI = ResourceManager.validateAndReturnOwlUrlOfSadlUri(importedURI);
    			}
    			if (ResourceManager.OWLFILEEXT.equals(importedURI.fileExtension())) {
    				SadlModelManager smMgr = sadlModelManagerProvider.get(importingResource);
    				IConfigurationManagerForIDE configMgr = smMgr.getConfigurationMgr(importingResource.getURI());
    				importedURI = URI.createURI(configMgr.getPublicUriFromActualUrl(importedURI.toString()));
    			}
    		}
    		catch (CoreException e) {
    			// TODO Auto-generated catch block
    			e.printStackTrace();
    		} catch (Exception e) {
    			SadlModelManager smMgr;
				try {
					smMgr = sadlModelManagerProvider.get(importingResource);
	    			smMgr.getMessageManager().error(e.getMessage());
				} catch (MalformedURLException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
    		} 
    	}
    	else {
    		// Look up the public URI in the policy file and get the OWL URI.
    		try {
    			SadlModelManager visitor = sadlModelManagerProvider.get(importingResource);
    			importedURI = URI.createURI(visitor.getAltUrl(importedURI.toString(), importingResource.getURI()));
    		} catch (MalformedURLException e) {
    			// TODO Auto-generated catch block
    			e.printStackTrace();
    		}
    	}
    	logger.debug("Resolved import '{}' to '{}'", importUriStr, importedURI);


    	return importedURI.toString();
    }
}
