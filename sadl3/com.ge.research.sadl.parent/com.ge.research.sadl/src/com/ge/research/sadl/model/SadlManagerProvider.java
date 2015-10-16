package com.ge.research.sadl.model;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.OnChangeEvictingCache;

import com.ge.research.sadl.jena.ModelManager;
import com.ge.research.sadl.sADL.SadlModel;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class SadlManagerProvider {

	@Inject OnChangeEvictingCache cache;
	
	public ModelManager getModelManager(final Resource resource, final CancelIndicator cancelIndicator) {
		cache.get(ModelManager.class, resource, new Provider<ModelManager>() {
			@Override
			public ModelManager get() {
				return internalGetModelManager(resource, cancelIndicator);
			}
		});
		return null;
	}

	protected ModelManager internalGetModelManager(Resource resource, CancelIndicator cancelIndicator) {
		SadlModel model = (SadlModel) resource.getContents().get(0);
		ModelManager result = new ModelManager();
		result.setModelBaseURI(model.getBaseUri());
		return result;
	}
	
}
