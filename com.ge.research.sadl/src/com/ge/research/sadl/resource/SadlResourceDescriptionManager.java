package com.ge.research.sadl.resource;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.IDefaultResourceDescriptionStrategy;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.impl.DefaultResourceDescriptionManager;

public class SadlResourceDescriptionManager extends
		DefaultResourceDescriptionManager {

	@Override
	protected IResourceDescription internalGetResourceDescription(
			Resource resource, IDefaultResourceDescriptionStrategy strategy) {
		return new SadlResourceDescription(resource, strategy, getCache());
	}
}
