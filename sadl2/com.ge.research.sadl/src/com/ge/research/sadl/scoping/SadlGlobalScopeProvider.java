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
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.IContainer;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.scoping.impl.ImportUriGlobalScopeProvider;
import org.eclipse.xtext.util.IResourceScopeCache;
import org.eclipse.xtext.xbase.lib.Pair;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.ResourceManager;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.builder.SadlModelManagerProvider;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.resource.SadlEObjectDescription;
import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.sadl.SadlPackage;
import com.google.common.base.Splitter;
import com.google.common.collect.Iterables;
import com.google.common.collect.Sets;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class SadlGlobalScopeProvider extends ImportUriGlobalScopeProvider {

	private static final Splitter SPLITTER = Splitter.on(',');
	
	private List<URI> externalURIs = null;

	@Inject
	IResourceDescription.Manager resourceDescriptionManager;
	@Inject
	private IContainer.Manager containerManager;
	@Inject
	private IResourceScopeCache cache;
	@Inject
	private SadlModelManagerProvider sadlModelManagerProvider;

	/**
	 * For each imported owl resource, import also the according sadl resource to
	 * have the elements on the global scope
	 */
	@Override
	protected LinkedHashSet<URI> getImportedUris(final Resource resource) {
		return cache.get(SadlGlobalScopeProvider.class.getName(), resource, new Provider<LinkedHashSet<URI>>(){
			public LinkedHashSet<URI> get() {
				try {
					String publicUri = null;
					if (!resource.getContents().isEmpty()) {
						Model model = (Model) resource.getContents().get(0);
						publicUri = model.getModelName().getBaseUri();
					}

					Iterable<IResourceDescription> visibleResourceDescriptions = getVisibleResourceDescriptions(resource);
					
					LinkedHashSet<URI> uriSet = Sets.newLinkedHashSet();
					collectImportedURIs(resource, URI.createURI(publicUri), uriSet, visibleResourceDescriptions);
					addImplicitUris(resource, uriSet);
					addExternalUris(resource, uriSet);
					return uriSet;
				} catch (Exception e) {
					return SadlGlobalScopeProvider.super.getImportedUris(resource);
				}
			}

			private void addExternalUris(Resource importingResource, LinkedHashSet<URI> uriSet) {
				// add a resource for this external import and add any indirect resource imports as well
				URI prjUri = SadlModelManager.getProjectFromResourceSet(importingResource.getResourceSet());
				if (prjUri != null) {
					SadlModelManager smm = sadlModelManagerProvider.get(prjUri);
					try {
						IConfigurationManagerForIDE cmgr = smm.getConfigurationMgr(prjUri.appendSegment(ResourceManager.OWLDIR).toString());
						List<URI> externalUris = cmgr.getExternalModelURIs();
						if (externalUris != null) {
							uriSet.addAll(externalUris);
						}
					} catch (ConfigurationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (URISyntaxException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
		});
	}

	private void addImplicitUris(Resource resource, LinkedHashSet<URI> uriSet) {
		uriSet.add(URI.createURI(ResourceManager.PLATFORM_RESOURCE_MODELS_RDF_SYNTAX_NS_RDF));
		uriSet.add(URI.createURI(ResourceManager.PLATFORM_RESOURCE_MODELS_RDF_SCHEMA_RDF));
//		uriSet.add(URI.createURI("http://sadl.sourceforge.net/owl/aulo.owl"));
	}

	private Iterable<IResourceDescription> getVisibleResourceDescriptions (Resource resource) {
		IResourceDescription description = resourceDescriptionManager.getResourceDescription(resource);
		IResourceDescriptions resourceDescriptions = getResourceDescriptions(resource);
		List<IContainer> visibleContainers = containerManager.getVisibleContainers(description,	resourceDescriptions);
		Set<IResourceDescription> visibleResourceDescriptions = new HashSet<>();
		for (IContainer container: visibleContainers) {
			for (IResourceDescription rd: container.getResourceDescriptions()) {
				visibleResourceDescriptions.add(rd);
			}
		}
		return visibleResourceDescriptions;
	}
	
	/**
	 * Recursive method to resolve transitive imports.
	 * @param publicURI The public URI of the imported resource
	 * @param uriSet The result set into which the URIs are collected
	 * @param resourceDescriptions The set of resource descriptions that are scanned for Models
	 */
	private void collectImportedURIs (Resource resource, URI publicURI, LinkedHashSet<URI> uriSet, Iterable<IResourceDescription> resourceDescriptions)  {
		// Here's where the imports are collected
		Pair<URI,String[]> importMapping = getImportMapping(publicURI, resourceDescriptions);
		if (importMapping==null) return;
		
		URI uri = importMapping.getKey();
		if (resource.getURI().equals(uri) || uriSet.add(uri)) {
			for (String imp: importMapping.getValue()) {
				URI importedURI = URI.createURI(imp);
				// If scheme is missing, the URI must be resolved relative to the given resource URI
				if (importedURI.scheme()==null) {
					importedURI = importedURI.resolve(resource.getURI());
				}
				if (importedURI.equals(resource.getURI())) {
					// this is a self-import
					return;
				}
				collectImportedURIs(resource, importedURI, uriSet, resourceDescriptions);
			}
		}
	}

	/**
	 * @param publicURI A public URI
	 * @param resourceDescriptions
	 * @return first: resource URI for the publicURI, second: array of imported URIs
	 */
	private Pair<URI,String[]> getImportMapping (URI publicURI, Iterable<IResourceDescription> resourceDescriptions) {
		Pair<URI, String[]> result = null;
		for (IResourceDescription resourceDescription: resourceDescriptions) {
			// TODO: Inject file extension for SADL
			for (IEObjectDescription objDesc: resourceDescription.getExportedObjectsByType(SadlPackage.Literals.MODEL)) {
				if (resourceDescription.getURI().equals(publicURI) || objDesc.getQualifiedName().toString().equals(publicURI.toString())) {
					String importsAsString = objDesc.getUserData(SadlEObjectDescription.IMPORT_KEY);
					String[] imports = !importsAsString.isEmpty() ? Iterables.toArray(SPLITTER.split(importsAsString), String.class) : new String[0];
					result = new Pair<URI, String[]>(resourceDescription.getURI(), imports);
//					if ("sadl".equals(resourceDescription.getURI().fileExtension())) {
						return result;
//					}
				}
			}
			
			// TODO add external URI imports, recurse
			if (externalURIs == null) {
				externalURIs = new ArrayList<URI>();
			}
			if (!externalURIs.contains(publicURI)) {
				externalURIs.add(publicURI);
			}
		}
		return result;
	}
}
