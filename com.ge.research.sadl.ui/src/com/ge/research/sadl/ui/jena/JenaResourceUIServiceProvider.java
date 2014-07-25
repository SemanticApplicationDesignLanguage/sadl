package com.ge.research.sadl.ui.jena;

import org.eclipse.xtext.ui.resource.DefaultResourceUIServiceProvider;

import com.ge.research.sadl.jena.JenaResourceServiceProvider;
import com.google.inject.Inject;

public class JenaResourceUIServiceProvider extends DefaultResourceUIServiceProvider {

	@Inject
	public JenaResourceUIServiceProvider(JenaResourceServiceProvider runtimeDelegate) {
		super(runtimeDelegate);
	}

}
