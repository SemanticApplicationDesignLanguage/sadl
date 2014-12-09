package com.ge.research.sadl.resource;

import java.util.LinkedHashSet;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.IDefaultResourceDescriptionStrategy;
import org.eclipse.xtext.resource.impl.DefaultResourceDescription;
import org.eclipse.xtext.util.IResourceScopeCache;

import com.ge.research.sadl.builder.SadlModelManager;
import com.google.inject.Provider;

public class SadlResourceDescription extends DefaultResourceDescription {
	private static final String IMPORTED_URIS_CACHEKEY = "IMPORTED_URIS";
	// duplicate field; super field is private
	private IResourceScopeCache cache;
	// duplicate field; super field is private
	private IDefaultResourceDescriptionStrategy strategy;
	
	public SadlResourceDescription(Resource resource,
			IDefaultResourceDescriptionStrategy strategy,
			IResourceScopeCache cache, SadlModelManager visitor) {
		super(resource, strategy, cache);
		this.cache = cache;
		this.strategy = strategy;
		// get imported URIs from ConfigurationManager and cache it
	}

	public LinkedHashSet<URI> getImportedURIs () {
		return cache.get(IMPORTED_URIS_CACHEKEY, getResource(), new Provider<LinkedHashSet<URI>>() {
			@Override
			public LinkedHashSet<URI> get() {
				return ((SadlResourceDescriptionStrategy)strategy).getImportedURIs(getResource());
			}
		});
	}
}
