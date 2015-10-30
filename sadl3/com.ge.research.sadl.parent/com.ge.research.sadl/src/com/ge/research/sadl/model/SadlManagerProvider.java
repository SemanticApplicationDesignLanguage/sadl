package com.ge.research.sadl.model;

import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.OnChangeEvictingCache;

import com.ge.research.sadl.jena.ModelManager;
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration;
import com.ge.research.sadl.sADL.SadlImport;
import com.ge.research.sadl.sADL.SadlModel;
import com.ge.research.sadl.sADL.SadlModelElement;
import com.ge.research.sadl.sADL.impl.SadlClassOrPropertyDeclarationImpl;
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
		result.setModelAlias(model.getAlias());
		
		result.setModelActualURL(resource.getURI().toFileString());
		
		EList<SadlImport> implist = model.getImports();
		Iterator<SadlImport> impitr = implist.iterator();
		while (impitr.hasNext()) {
			SadlImport simport = impitr.next();
			result.addImport(simport.getImportURI(), simport.getAlias());
		}
		
		// process rest of parse tree
		List<SadlModelElement> elements = model.getElements();
		if (elements != null) {
			Iterator<SadlModelElement> elitr = elements.iterator();
			while (elitr.hasNext()) {
				SadlModelElement element = elitr.next();
				if (element instanceof SadlClassOrPropertyDeclaration) {
					
				}
			}
		}
		
		
		return result;
	}
	
}
