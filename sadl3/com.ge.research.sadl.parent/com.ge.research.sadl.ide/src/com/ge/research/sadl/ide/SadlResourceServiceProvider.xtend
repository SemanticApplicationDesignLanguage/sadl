package com.ge.research.sadl.ide

import org.eclipse.xtext.resource.impl.DefaultResourceServiceProvider
import org.eclipse.emf.common.util.URI

class SadlResourceServiceProvider extends DefaultResourceServiceProvider {
	
	override isSource(URI uri) {
		if (uri.segments.contains(".ipynb_checkpoints")) {
			return false;
		}
		return super.isSource(uri)
	}
	
	override canHandle(URI uri) {
		if (!isSource(uri)) {
			return false;
		}
		super.canHandle(uri)
	}
	
}