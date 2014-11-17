package com.ge.research.sadl.resource;

import javax.inject.Inject;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.resource.XtextResourceFactory;

import com.ge.research.sadl.jena.JenaResourceFactory;

public class SadlResourceFactory implements IResourceFactory {
	@Inject XtextResourceFactory xtextResourceFactory;
	@Inject JenaResourceFactory jenaResourceFactory;
	
	
	@Override
	public Resource createResource(URI uri) {

		return null;
	}
}
