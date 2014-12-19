package com.ge.research.sadl.jena;

import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.resource.IDefaultResourceDescriptionStrategy;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.resource.generic.AbstractGenericResourceRuntimeModule;

import com.ge.research.sadl.naming.SadlSimpleNameProvider;

public class JenaRuntimeModule extends AbstractGenericResourceRuntimeModule {
	@Override
	protected String getFileExtensions() {
		return "owl,nt,n3,rdf";
	}

	public Class<? extends IResourceFactory> bindIResourceFactory() {
		return JenaResourceFactory.class;
	}
	
	public Class<? extends IDefaultResourceDescriptionStrategy> bindIDefaultResourceDescriptionStrategy() {
		return JenaResourceDescriptionStrategy.class;
	}

	@Override
	protected String getLanguageName() {
		return "com.ge.research.sadl.Jena";
	}
	
	public Class<? extends IQualifiedNameProvider> bindIQualifiedNameProvider() {
		return SadlSimpleNameProvider.class;
	}

}
