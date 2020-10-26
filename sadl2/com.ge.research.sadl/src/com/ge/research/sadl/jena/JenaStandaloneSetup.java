package com.ge.research.sadl.jena;

import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.ISetup;

import com.ge.research.sadl.SadlStandaloneSetup;
import com.google.inject.Guice;
import com.google.inject.Injector;

public class JenaStandaloneSetup implements ISetup {
	public static void doSetup() {
		new JenaStandaloneSetup().createInjectorAndDoEMFRegistration();
	}
	
	public Injector createInjectorAndDoEMFRegistration() {
		org.eclipse.xtext.common.TerminalsStandaloneSetup.doSetup();

		Injector injector = createInjector();
		register(injector);
		return injector;
	}
	
	public Injector createInjector() {
		return Guice.createInjector(new com.ge.research.sadl.jena.JenaRuntimeModule());
	}
		

	public void register(Injector injector) {
		if (!EPackage.Registry.INSTANCE
				.containsKey("http://www.ge.com/research/sadl/Sadl")) {
			EPackage.Registry.INSTANCE.put(
					"http://www.ge.com/research/sadl/Sadl",
					com.ge.research.sadl.sadl.SadlPackage.eINSTANCE);
		}

		org.eclipse.xtext.resource.IResourceFactory resourceFactory = injector
				.getInstance(org.eclipse.xtext.resource.IResourceFactory.class);
		org.eclipse.xtext.resource.IResourceServiceProvider serviceProvider = injector
				.getInstance(org.eclipse.xtext.resource.IResourceServiceProvider.class);
		Resource.Factory.Registry.INSTANCE.getExtensionToFactoryMap().put(
				"owl", resourceFactory);
		org.eclipse.xtext.resource.IResourceServiceProvider.Registry.INSTANCE
				.getExtensionToFactoryMap().put("owl", serviceProvider);

	}
}
