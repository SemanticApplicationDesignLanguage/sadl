/*
* generated by Xtext
*/
package com.ge.research.sadl;

import org.eclipse.xtext.junit4.IInjectorProvider;

import com.google.inject.Injector;

public class MappingUiInjectorProvider implements IInjectorProvider {
	
	public Injector getInjector() {
		return com.ge.research.sadl.ui.internal.MappingActivator.getInstance().getInjector("com.ge.research.sadl.Mapping");
	}
	
}
