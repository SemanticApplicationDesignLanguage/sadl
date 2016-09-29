package com.ge.research.sadl.perspective;

import org.osgi.framework.Bundle;

import com.ge.research.sadl.ui.SADLExecutableExtensionFactory;

public class PerspectiveExecutableExtensionFactory extends SADLExecutableExtensionFactory {

	@Override
	protected Bundle getBundle() {
		return Activator.getInstance().getBundle();
	}
}
