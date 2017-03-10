package com.ge.research.sadl.jena;

import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.generic.GenericResourceServiceProvider;

import com.google.inject.Inject;

public class JenaResourceServiceProvider extends GenericResourceServiceProvider {
	@Inject
	private JenaResourceDescriptionManager resourceDescriptionManager;
	
	public IResourceDescription.Manager getResourceDescriptionManager() {
		return resourceDescriptionManager;
	}
}
