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
import java.util.LinkedHashSet;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.scoping.impl.DefaultGlobalScopeProvider;
import org.eclipse.xtext.scoping.impl.ImportUriGlobalScopeProvider;

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.sadl.Model;
import com.google.common.collect.Sets;
import com.google.inject.Inject;

public class SadlGlobalScopeProvider extends ImportUriGlobalScopeProvider {
	@Inject
	private SadlModelManager visitor;

	@Inject
	DefaultGlobalScopeProvider defaultGlobalScopeProvider;
	@Inject
	IResourceDescription.Manager resourceDescriptionManager;

	/**
	 * For each imported owl resource, import also the accoring sadl resource to
	 * have the elements on the global scope
	 */
	@Override
	protected LinkedHashSet<URI> getImportedUris(final Resource resource) {
		// access ConfigurationManager to get all accessed URIs
		ConfigurationManagerForIDE cmgr = null;
		try {
			cmgr = visitor.getConfigurationMgr(resource.getURI());
			if (cmgr==null) {
				return super.getImportedUris(resource);
			}
			String publicUri = null;
			if (!resource.getContents().isEmpty()) {
				Model model = (Model) resource.getContents().get(0);
				publicUri = model.getModelName().getBaseUri();
			}
			
			LinkedHashSet<URI> uriSet = Sets.newLinkedHashSet();
			uriSet.addAll(super.getImportedUris(resource));
			collectImportedURIs(URI.createURI(publicUri), uriSet, cmgr);
			
			return uriSet;
		} catch (Exception e) {
			return super.getImportedUris(resource);
		}
	}
	
	
	private void collectImportedURIs (URI publicURI, LinkedHashSet<URI> uriSet, IConfigurationManagerForIDE cmgr) throws ConfigurationException {
		String altUrlFromPublicUri = cmgr.getAltUrlFromPublicUri(publicURI.toString());
		if (uriSet.add(URI.createURI(altUrlFromPublicUri))) {
			try {
				Map<String, String> imports = cmgr.getImports(publicURI.toString());
				for (String s: imports.keySet()) {
					URI uri = URI.createURI(s);
					collectImportedURIs(uri, uriSet, cmgr);
				}
			} catch (ConfigurationException e) {
				; // TODO: Handle exception
			} catch (IOException e) {
				; // TODO: Handle exception
			}
		}
	}
}
