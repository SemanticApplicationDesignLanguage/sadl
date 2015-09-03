package com.ge.research.sadl.tests;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.mwe.core.issues.Issues;
import org.eclipse.emf.mwe.core.issues.IssuesImpl;
import org.eclipse.xtext.mwe.Validator;
import org.eclipse.xtext.resource.IResourceServiceProvider;
import org.eclipse.xtext.resource.IResourceServiceProvider.Registry;
import org.junit.Assert;

import com.ge.research.sadl.SadlStandaloneSetup;
import com.google.inject.Injector;

public class SadlParseTest2 {

	public static void main(String[] args) {
		SadlStandaloneSetup.doSetup();
		
		SadlStandaloneSetup setup = new SadlStandaloneSetup();
		Injector injector = setup.createInjector();
		
		ResourceSet resourceSet = injector.getInstance(ResourceSet.class);
		resourceSet.setResourceFactoryRegistry(Resource.Factory.Registry.INSTANCE);
		URI uri = URI.createFileURI("resources/shape-rules.sadl");
		Assert.assertEquals("sadl", uri.fileExtension());
		Resource resource = resourceSet.getResource(uri, true);
		Validator validator = new Validator();
		Issues issues = new IssuesImpl();
		Registry registry = IResourceServiceProvider.Registry.INSTANCE;
		validator.validate(resourceSet, registry, issues);
		Assert.assertFalse(issues.hasErrors());
		int i=0;
	}

}
