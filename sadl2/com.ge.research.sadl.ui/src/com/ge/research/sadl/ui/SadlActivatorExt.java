package com.ge.research.sadl.ui;

import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.ResourcesPlugin;
import org.osgi.framework.BundleContext;

import com.ge.research.sadl.builder.SadlModelManagerProvider;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.google.inject.Module;

public class SadlActivatorExt extends SadlActivator {
	public static final String COM_GE_RESEARCH_SADL_JENA = "com.ge.research.sadl.Jena";
	private SadlModelManagerProvider sadlModelManagerProvider;
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		sadlModelManagerProvider = getInjector(COM_GE_RESEARCH_SADL_SADL).getInstance(SadlModelManagerProvider.class);
		ResourcesPlugin.getWorkspace().addResourceChangeListener(sadlModelManagerProvider, IResourceChangeEvent.PRE_CLOSE | IResourceChangeEvent.PRE_DELETE | IResourceChangeEvent.POST_CHANGE);
	}
	
	@Override
	public void stop(BundleContext context) throws Exception {
		// TODO Auto-generated method stub
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(sadlModelManagerProvider);
		sadlModelManagerProvider  = null;
		super.stop(context);
	}
	
	protected Module getRuntimeModule(String grammar) {
		if (COM_GE_RESEARCH_SADL_JENA.equals(grammar)) {
			return new com.ge.research.sadl.jena.JenaRuntimeModule();
		}
		
		return super.getRuntimeModule(grammar);
	}
	
	protected Module getUiModule(String grammar) {
		if (COM_GE_RESEARCH_SADL_JENA.equals(grammar)) {
			return (Module) new com.ge.research.sadl.ui.jena.JenaUiModule(this);
		}
		
		return super.getUiModule(grammar);
	}
	
}
