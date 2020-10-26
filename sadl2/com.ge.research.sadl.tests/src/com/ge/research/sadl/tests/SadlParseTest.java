package com.ge.research.sadl.tests;

import javax.inject.Inject;
import javax.inject.Provider;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.ge.research.sadl.SadlInjectorProvider;

@RunWith(XtextRunner.class)
@InjectWith(SadlInjectorProvider.class)
public class SadlParseTest {
	@Inject Provider<ResourceSet> rsProvider;
	@Test
	public void test_shape_rules () {
		ResourceSet resourceSet = rsProvider.get();
		resourceSet.setResourceFactoryRegistry(Resource.Factory.Registry.INSTANCE);
		Resource resource = resourceSet.getResource(URI.createURI("file:./resources/shape-rules.sadl"), true);
		Assert.assertNotNull(resource);
		int i=0;
	}
}
