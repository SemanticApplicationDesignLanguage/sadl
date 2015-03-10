package com.ge.research.sadl.ui;

import com.ge.research.sadl.ui.internal.SadlActivator;
import com.google.inject.Module;

public class SadlActivatorExt extends SadlActivator {
	public static final String COM_GE_RESEARCH_SADL_JENA = "com.ge.research.sadl.Jena";

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
