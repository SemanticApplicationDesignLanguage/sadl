package com.ge.research.sadl.jena;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.XtextResource;

import com.ge.research.sadl.model.gp.SadlCommand;
import com.google.inject.Provider;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.rdf.model.Property;

public class OntModelProvider {
	
	private static OntModel sadlBaseModel;
	private static OntModel sadlListModel;
	private static OntModel sadlDefaultsModel;
	
	static class OntModelAdapter extends AdapterImpl  {
		String modelName;
		String modelPrefix;
		OntModel model;
		List<Object> otherContent;
		List<SadlCommand> sadlCommands = null;
		Map<EObject, Property> impliedPropertiesUsed = null;
		boolean isLoading = true;
		boolean hasCircularImport = false;
		
		@Override
		public boolean isAdapterForType(Object type) {
			return OntModel.class == type;
		}
	}
	
	public static void registerResource(Resource resource) {
		findAdapter(resource);
	}
	
	public static boolean checkForCircularImport(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		if (a.isLoading) {
			a.hasCircularImport = true;
			return true;
		}
		return false;
	}
	
	public static boolean hasCircularImport(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		return a.hasCircularImport;
	}

	public static void attach(Resource resource, OntModel model, String modelName, String modelPrefix) {
		OntModelAdapter adapter = findAdapter(resource);
		adapter.modelName = modelName;
		adapter.modelPrefix = modelPrefix;
		adapter.model = model;
		adapter.isLoading = false;  // loading is complete
	}
	
	public static void attach(Resource resource, OntModel model, String modelName, String modelPrefix, 
			List<SadlCommand> sadlCommands) {
		OntModelAdapter adapter = findAdapter(resource);
		adapter.modelName = modelName;
		adapter.modelPrefix = modelPrefix;
		adapter.model = model;
		adapter.sadlCommands = sadlCommands;
		adapter.isLoading = false;  // loading is complete
	}
	
	public static void addOtherContent(Resource resource, Object otherContent) {
		OntModelAdapter adapter = findAdapter(resource);
		if (adapter.otherContent == null) {
			adapter.otherContent = new ArrayList<Object>();
		}
		adapter.otherContent.add(otherContent);
	}
	
	public static List<Object> getOtherContent(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		return a.otherContent;
	}
	
	public static List<SadlCommand> getSadlCommands(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		return a.sadlCommands;
	}
	
	public static void addImpliedProperties(Resource resource, Map<EObject, Property> impliedProperties) {
		if (resource != null && impliedProperties != null) {
			OntModelAdapter a = findAdapter(resource);
			if (a == null) {
				a = new OntModelAdapter();
				a.isLoading = true;
				resource.eAdapters().add(a);
			}
			a.impliedPropertiesUsed = impliedProperties;
		}
	}
	
	public static Property getImpliedProperty(Resource resource, EObject context) {
		OntModelAdapter a = findAdapter(resource);
		if (a != null && a.impliedPropertiesUsed != null) {
			return a.impliedPropertiesUsed.get(context);
		}
		return null;
	}
	
	public static OntModelAdapter findAdapter(Resource resource) {
		return findAdapter(resource, new Provider<OntModelAdapter>() {
					@Override
					public OntModelAdapter get() {
						return new OntModelAdapter();
					}
				});
	}
	
	public static OntModelAdapter findAdapter(Resource resource, Provider<OntModelAdapter> provider) {
		if (resource != null) {
			if (resource instanceof XtextResource) {
				return ((XtextResource) resource).getCache().get("ontmodel", resource, new Provider<OntModelAdapter>() {
					@Override
					public OntModelAdapter get() {
						return new OntModelAdapter();
					}
				});
			}
		}
		return new OntModelAdapter();
	}
	
	public static OntModel find(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		return a.model;
	}
	
	public static String getModelName(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		return a.modelName;
	}

	public static String getModelPrefix(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		return a.modelPrefix;
	}

	public static OntModel getSadlBaseModel() {
		return sadlBaseModel;
	}

	public static void setSadlBaseModel(OntModel model) {
		sadlBaseModel = model;
	}

	public static OntModel getSadlListModel() {
		return sadlListModel;
	}

	public static void setSadlListModel(OntModel model) {
		sadlListModel = model;
	}

	public static OntModel getSadlDefaultsModel() {
		return sadlDefaultsModel;
	}
	
	public static void setSadlDefaultsModel(OntModel model) {
		sadlDefaultsModel = model;
	}

}
