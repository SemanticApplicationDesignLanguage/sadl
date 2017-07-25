package com.ge.research.sadl.processing;

import static org.eclipse.xtext.util.CancelIndicator.NullImpl;
import static org.eclipse.xtext.validation.CheckMode.FAST_ONLY;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.validation.IResourceValidator;

import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.reasoner.TranslationException;
import com.google.common.base.Function;
import com.google.common.base.Optional;
import com.google.common.collect.FluentIterable;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.rdf.model.Property;

public class OntModelProvider {
	
	private static Function<Resource, Optional<OntModelAdapter>> FIND_ADAPTER_FUNC = resource -> FluentIterable
			.from(resource.eAdapters()).filter(OntModelAdapter.class).first();
	
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
		
		@Override
		public boolean isAdapterForType(Object type) {
			return OntModel.class == type;
		}
	}
	
	public static void registerResource(Resource resource) {
		if (resource != null) {
			if (!FIND_ADAPTER_FUNC.apply(resource).isPresent()) {
				OntModelAdapter adapter = new OntModelAdapter();
				adapter.isLoading = true;
				resource.eAdapters().add(adapter);
			}
		}
	}

	public static void attach(Resource resource, OntModel model, String modelName, String modelPrefix) {
		OntModelAdapter adapter = FIND_ADAPTER_FUNC.apply(resource).orNull();
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
		OntModelAdapter adapter = FIND_ADAPTER_FUNC.apply(resource).orNull();
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
	
	private static OntModelAdapter findAdapter(Resource resource) {
		if (resource == null) {
			return null;
		}
		Optional<OntModelAdapter> adapter = FIND_ADAPTER_FUNC.apply(resource);
		if (adapter.isPresent()) {
			return adapter.get();
		}
		if (resource instanceof XtextResource) {
			final IResourceValidator validator = ((XtextResource) resource).getResourceServiceProvider().getResourceValidator();
			validator.validate(resource, FAST_ONLY, NullImpl);
		}
		return FIND_ADAPTER_FUNC.apply(resource).orNull();
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
