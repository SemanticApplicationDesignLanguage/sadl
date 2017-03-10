package com.ge.research.sadl.jena;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.IDefaultResourceDescriptionStrategy;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.impl.DefaultResourceDescriptionManager;

import com.google.inject.Inject;

public class JenaResourceDescriptionManager extends DefaultResourceDescriptionManager {
	@Inject
	private JenaResourceDescriptionStrategy strategy;
	
	@Override
	protected IResourceDescription internalGetResourceDescription(
			Resource resource, IDefaultResourceDescriptionStrategy strategy) {
		return super.internalGetResourceDescription(resource, this.strategy);
	}
}
