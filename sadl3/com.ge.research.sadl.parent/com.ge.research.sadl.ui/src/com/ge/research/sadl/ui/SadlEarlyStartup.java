package com.ge.research.sadl.ui;

import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.ui.IStartup;

import com.ge.research.sadl.ui.internal.SadlActivator;

import static com.ge.research.sadl.ui.internal.SadlActivator.COM_GE_RESEARCH_SADL_SADL;

public class SadlEarlyStartup implements IStartup {

	@Override
	public void earlyStartup() {
		CustomSadlHooks hook = SadlActivator.getInstance().getInjector(COM_GE_RESEARCH_SADL_SADL).getInstance(CustomSadlHooks.class);
		ResourcesPlugin.getWorkspace().addResourceChangeListener(hook, IResourceChangeEvent.PRE_BUILD |IResourceChangeEvent.POST_BUILD);
	}
}
