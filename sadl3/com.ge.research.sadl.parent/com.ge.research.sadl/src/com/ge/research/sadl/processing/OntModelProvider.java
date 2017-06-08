package com.ge.research.sadl.processing;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;

import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.reasoner.TranslationException;
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
		Map<EObject, List<Property>> impliedPropertiesUsed = null;
		boolean isLoading = false;
		boolean hasCircularImport = false;
		
		@Override
		public boolean isAdapterForType(Object type) {
			return OntModel.class == type;
		}
	}
	
	public static void registerResource(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		if (a == null) {
			a = new OntModelAdapter();
			a.isLoading = true;
			resource.eAdapters().add(a);
		}
	}

	/**
	 * Deprecated. As of GH-200 the cyclic dependency check is the Xtext-based SADL validator's responsibility.
	 * Will be removed with GH-202.
	 */
	@Deprecated
	public static boolean checkForCircularImport(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		if (a != null) {
			if (a.isLoading) {
				a.hasCircularImport = true;
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Deprecated. As of GH-200 the cyclic dependency check is the Xtext-based SADL validator's responsibility.
	 * Will be removed with GH-202.
	 */
	@Deprecated
	public static boolean hasCircularImport(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		if (a != null) {
			return a.hasCircularImport;
		}
		return false;
	}

	public static void attach(Resource resource, OntModel model, String modelName, String modelPrefix) {
		OntModelAdapter adapter = findAdapter(resource);
		if (adapter == null) {
			adapter = new OntModelAdapter();
			resource.eAdapters().add(adapter);
		}
		adapter.modelName = modelName;
		adapter.modelPrefix = modelPrefix;
		adapter.model = model;
		adapter.isLoading = false;  // loading is complete
	}
	
	public static void attach(Resource resource, OntModel model, String modelName, String modelPrefix, 
			List<SadlCommand> sadlCommands) {
		OntModelAdapter adapter = findAdapter(resource);
		if (adapter == null) {
			adapter = new OntModelAdapter();
			resource.eAdapters().add(adapter);
		}
		adapter.modelName = modelName;
		adapter.modelPrefix = modelPrefix;
		adapter.model = model;
		adapter.sadlCommands = sadlCommands;
		adapter.isLoading = false;  // loading is complete
	}
	
	public static void addOtherContent(Resource resource, Object otherContent) {
		OntModelAdapter adapter = findAdapter(resource);
		if (adapter == null) {
			adapter = new OntModelAdapter();
			adapter.isLoading = true;
			resource.eAdapters().add(adapter);
		}
		if (adapter.otherContent == null) {
			adapter.otherContent = new ArrayList<Object>();
		}
		adapter.otherContent.add(otherContent);
	}
	
	public static List<Object> getOtherContent(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		if (a != null) {
			return a.otherContent;
		}
		return null;
	}
	
	public static List<SadlCommand> getSadlCommands(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		if (a != null) {
			return a.sadlCommands;
		}
		return null;
	}
	
	public static void addImpliedProperty(Resource resource, EObject eobj, Property impliedProperty) {
		if (resource != null && impliedProperty != null) {
			OntModelAdapter a = findAdapter(resource);
			if (a == null) {
				a = new OntModelAdapter();
				a.isLoading = true;
				resource.eAdapters().add(a);
			}
			if (a.impliedPropertiesUsed == null) {
				a.impliedPropertiesUsed = new HashMap<EObject, List<Property>>();
			}
			List<Property> plist;
			if (!a.impliedPropertiesUsed.containsKey(eobj)) {
				plist = new ArrayList<Property>();
				a.impliedPropertiesUsed.put(eobj, plist);
			}
			else {
				plist = a.impliedPropertiesUsed.get(eobj);
			}
			plist.add(impliedProperty);
		}
	}
	
	public static List<Property> getImpliedProperties(Resource resource, EObject context) {
		OntModelAdapter a = findAdapter(resource);
		if (a != null && a.impliedPropertiesUsed != null) {
			return a.impliedPropertiesUsed.get(context);
		}
		return null;
	}
	
	public static Map<EObject, List<Property>> getAllImpliedProperties(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		if (a != null && a.impliedPropertiesUsed != null) {
			return a.impliedPropertiesUsed;
		}
		return null;
	}
	
	public static Property getImpliedProperty(Resource resource, EObject context) throws TranslationException {
		List<Property> properties = getImpliedProperties(resource, context);
		if (properties != null) {
			if (properties.size() > 1) {
				throw new TranslationException("More than one implied property for given context; use getImpliedProperties");
			}
			else {
				return properties.get(0);
			}
		}
		return null;
	}
	
	public static OntModelAdapter findAdapter(Resource resource) {
		if (resource != null) {
			for (Adapter a : resource.eAdapters()) {
				if (a instanceof OntModelAdapter) {
					return ((OntModelAdapter) a);
				}
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
	
	public static String getModelName(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		if (a != null) {
			return a.modelName;
		}
		return null;
	}

	public static String getModelPrefix(Resource resource) {
		OntModelAdapter a = findAdapter(resource);
		if (a != null) {
			return a.modelPrefix;
		}
		return null;
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
