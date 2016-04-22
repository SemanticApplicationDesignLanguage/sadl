package com.ge.research.sadl.jena;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.resource.Resource;

import com.hp.hpl.jena.ontology.OntModel;

public class OntModelProvider {
	
	static class OntModelAdapter extends AdapterImpl  {
		OntModel model;
		@Override
		public boolean isAdapterForType(Object type) {
			return OntModel.class == type;
		}
	}
	
	public static void attach(Resource resource, OntModel model) {
		OntModelAdapter adapter = new OntModelAdapter();
		for (Adapter a : resource.eAdapters()) {
			if (a instanceof OntModelAdapter) {
				adapter = (OntModelAdapter) a;
			}
		}
		adapter.model = model;
		resource.eAdapters().add(adapter);
	}
	
	public static OntModel find(Resource resource) {
		for (Adapter a : resource.eAdapters()) {
			if (a instanceof OntModelAdapter) {
				return ((OntModelAdapter) a).model;
			}
		}
		return null;
	}
	
	
}
