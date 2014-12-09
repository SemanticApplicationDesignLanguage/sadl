package com.ge.research.sadl.resource;

import javax.inject.Inject;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.IDefaultResourceDescriptionStrategy;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.impl.DefaultResourceDescriptionManager;

import com.ge.research.sadl.builder.SadlModelManager;

public class SadlResourceDescriptionManager extends
		DefaultResourceDescriptionManager {

	@Inject
	private SadlModelManager visitor;
	
	@Override
	protected IResourceDescription internalGetResourceDescription(
			Resource resource, IDefaultResourceDescriptionStrategy strategy) {
		return new SadlResourceDescription(resource, strategy, getCache(), visitor);
	}
}
