package com.ge.research.sadl.jena;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.resource.Resource;

import com.hp.hpl.jena.ontology.OntModel;

public class OntModelProvider {
	
	static class OntModelAdapter extends AdapterImpl  {
		OntModel model;
		boolean isLoading = false;
		
		@Override
		public boolean isAdapterForType(Object type) {
			return OntModel.class == type;
		}
	}
	
	public static boolean checkForCircularImport(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		if (a != null) {
			if (a.isLoading) {
				return true;
			}
		}
		a = new OntModelAdapter();
		a.isLoading = true;
		resource.eAdapters().add(a);
		return false;
	}
	
	public static void attach(Resource resource, OntModel model) {
		OntModelAdapter adapter = findAdapter(resource);
		if (adapter == null) {
			adapter = new OntModelAdapter();
		}
		adapter.model = model;
		adapter.isLoading = false;  // loading is complete
		resource.eAdapters().add(adapter);
	}
	
	public static OntModelAdapter findAdapter(Resource resource) {
		for (Adapter a : resource.eAdapters()) {
			if (a instanceof OntModelAdapter) {
				return ((OntModelAdapter) a);
			}
		}
		return null;
	}
	
	public static OntModel find(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		if (a != null) {
			return a.model;
		}
		return null;
	}
	
	
}
