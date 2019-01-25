package com.ge.research.sadl.applications;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "com.ge.research.sadl.applications.ExecuteCommand"; //$NON-NLS-1$

	private static Activator plugin;

	private static Activator INSTANCE;

	public Activator() {

	}

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		INSTANCE = this;
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		INSTANCE = null;
	}

	public static Activator getDefault() {
		return plugin;
	}

	public static Activator getInstance() {
		return INSTANCE;
	}
}

