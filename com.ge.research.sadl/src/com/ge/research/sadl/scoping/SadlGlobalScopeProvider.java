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
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.scoping.impl.ImportUriGlobalScopeProvider;
import org.eclipse.xtext.util.IResourceScopeCache;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.sadl.SadlPackage;
import com.google.common.collect.Sets;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class SadlGlobalScopeProvider extends ImportUriGlobalScopeProvider {
	@Inject
	private SadlModelManager visitor;

	@Inject
	IResourceDescription.Manager resourceDescriptionManager;

	@Inject
	private IResourceScopeCache cache;

	/**
	 * For each imported owl resource, import also the accoring sadl resource to
	 * have the elements on the global scope
	 */
	@Override
	protected LinkedHashSet<URI> getImportedUris(final Resource resource) {
		return cache.get(ImportUriGlobalScopeProvider.class.getName(), resource, new Provider<LinkedHashSet<URI>>(){
			public LinkedHashSet<URI> get() {
				// access ConfigurationManager to get all accessed URIs
				IConfigurationManagerForIDE cmgr = null;
				try {
					synchronized (resource) {
						cmgr = visitor.getConfigurationMgr(resource.getURI());
					}
					if (cmgr==null) {
						return SadlGlobalScopeProvider.super.getImportedUris(resource);
					}
					String publicUri = null;
					if (!resource.getContents().isEmpty()) {
						Model model = (Model) resource.getContents().get(0);
						publicUri = model.getModelName().getBaseUri();
					}

					LinkedHashSet<URI> uriSet = Sets.newLinkedHashSet();
					collectImportedURIs(resource, URI.createURI(publicUri), uriSet, cmgr, false);

					return uriSet;
				} catch (Exception e) {
					return SadlGlobalScopeProvider.super.getImportedUris(resource);
				}
			}
		});
	}


	/**
	 * Recursive method to resolve transitive imports.
	 * @param resource The context resource
	 * @param publicURI The public URI of the imported resource
	 * @param uriSet The result set into which the URIs are collected
	 * @param cmgr The configuration manager for the context resource
	 * @throws ConfigurationException
	 * @throws MalformedURLException
	 */
	private void collectImportedURIs (Resource resource, URI publicURI, LinkedHashSet<URI> uriSet, IConfigurationManagerForIDE cmgr, boolean getImportsFromCfgMgr) throws ConfigurationException, MalformedURLException {
		String altUrl = cmgr.getAltUrlFromPublicUri(publicURI.toString());
		if (altUrl.equals(publicURI.toString())) {
			// this might be in a different project
			Enumeration<IConfigurationManagerForIDE> configMgrs = visitor.getConfigurationManagers();
			while (configMgrs.hasMoreElements()) {
				IConfigurationManagerForIDE aconfigmgr = configMgrs.nextElement();
				String anAltUrl = aconfigmgr.getAltUrlFromPublicUri(publicURI.toString());
				if (!anAltUrl.equals(publicURI.toString())) {
					// this must be the right project
					altUrl = anAltUrl;
					cmgr = aconfigmgr;
					break;
				}
			}
		}
		URI altUrlFromPublicUri = URI.createURI(altUrl);
		// For SADL derived OWL models, resolve the SADL resource URI from the index.

		if (cmgr.isSadlDerived(publicURI.toString())) {
			// TODO: Use ResourceManager#sadlFileNameOfOwlAltUrl
			IResourceDescriptions descriptions = getResourceDescriptions(resource);
			Iterable<IEObjectDescription> matchingModels = descriptions.getExportedObjects(SadlPackage.Literals.MODEL, QualifiedName.create(publicURI.toString()), false);
			Iterator<IEObjectDescription> it = matchingModels.iterator();
			if (it.hasNext()) {
				IEObjectDescription description = it.next();
				// This will be the URI of the SADL file
				altUrlFromPublicUri = description.getEObjectURI().trimFragment();
			}
		}
		if (uriSet.add(altUrlFromPublicUri)) {
			// This URI was not collected yet, thus collect its imports again
			try {
				Set<String> imports = null;
				if (getImportsFromCfgMgr)
					imports = cmgr.getImports(publicURI.toString(), Scope.LOCALONLY).keySet();
				else {
					imports = new HashSet<>();
					Model m = (Model) resource.getContents().get(0);
					for (Import imp: m.getImports()) {
						imports.add(imp.getImportURI());
					}
				}
				for (String s: imports) {
					URI uri = URI.createURI(s);
					collectImportedURIs(resource, uri, uriSet, cmgr, true);
				}
			} catch (ConfigurationException e) {
				; // TODO: Handle exception
			} catch (IOException e) {
				; // TODO: Handle exception
			}
		}
	}
}
