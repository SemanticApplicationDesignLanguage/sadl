package com.ge.research.sadl.ui.jena;

import org.eclipse.xtext.ui.guice.AbstractGuiceAwareExecutableExtensionFactory;
import org.osgi.framework.Bundle;

import com.ge.research.sadl.ui.SadlActivatorExt;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.google.inject.Injector;

public class JenaExecutableExtensionFactory extends AbstractGuiceAwareExecutableExtensionFactory {

	@Override
	protected Bundle getBundle() {
		return SadlActivator.getInstance().getBundle();
	}
	
	@Override
	protected Injector getInjector() {
		return SadlActivator.getInstance().getInjector(SadlActivatorExt.COM_GE_RESEARCH_SADL_JENA);
	}
	
}
