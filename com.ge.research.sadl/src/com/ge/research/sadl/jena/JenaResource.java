/************************************************************************
 * Copyright © 2007-2010 - General Electric Company, All Rights Reserved
 *
 * Project: SADL
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/

package com.ge.research.sadl.jena;

import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.ResourceManager;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.builder.SadlModelManagerProvider;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.sadl.ClassDeclaration;
import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.InstanceDeclaration;
import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.sadl.ModelName;
import com.ge.research.sadl.sadl.PropertyDeclaration;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlFactory;
import com.ge.research.sadl.utils.SadlUtils.ConceptType;
import com.google.inject.Inject;

public class JenaResource extends ResourceImpl {

	private static final Logger logger = LoggerFactory.getLogger(JenaResource.class);

	@Inject
	private SadlModelManagerProvider sadlModelManagerProvider;

	/**
	 * Loads SADL resources using Xtext but OWL and RDF resources using
	 * SadlModelManager.  Assumes that the resource's URI already has been
	 * stored in our class using the setURI method.
	 */
	@Override
	protected void doLoad(InputStream inputStream, Map<?, ?> options)
			throws IOException {
		// However, we will load OWL and RDF files by querying the SadlModelManager
		// to add the files' exported names to our resource's contents but only if
		// it isn't an OWL file generated from a SADL file (which should already have
		// its names exported).
		try {
			SadlModelManager visitor;
			URI thisUri = getURI();
			Resource context = this.getResourceSet().getResource(thisUri, false);
			if (thisUri.toString().endsWith("configuration.rdf") || thisUri.toString().endsWith("ont-policy.rdf")) {
				return;
			}
			else if (thisUri.toString().equals("platform:/resource/TestSadlIde/OwlModels/rdf-syntax-ns.rdf") ||
					thisUri.toString().equals("platform:/resource/TestSadlIde/OwlModels/rdf-schema.rdf")) {
				thisUri = ResourceManager.convertPlatformUriToAbsoluteUri(thisUri);
				visitor = sadlModelManagerProvider.getAny();
				if (visitor == null) {
					visitor = sadlModelManagerProvider.get(thisUri);
				}
				if (thisUri.toString().endsWith("rdf-syntax-ns.rdf")) {
					String bundleUrl = ResourceManager.getBundleModelFile("rdf-syntax-ns.rdf").getAbsolutePath();
					thisUri = URI.createURI(bundleUrl);
				}
				else {
					String bundleUrl = ResourceManager.getBundleModelFile("rdf-schema.rdf").getAbsolutePath();
					thisUri = URI.createURI(bundleUrl);
				}
			}
			else {
				try {
					visitor = sadlModelManagerProvider.get(thisUri);
					// TODO if sadl-derived, return without creating names.
					String owlModelsFolder = ResourceManager.getOwlModelsFolder(thisUri);
					URI absUri = ResourceManager.convertPlatformUriToAbsoluteUri(context.getURI());
					if (visitor.getConfigurationMgr(owlModelsFolder).isSadlDerived(absUri.toString())) {	
						return;
					}
				} catch (ConfigurationException e) {
					throw e;
				} catch (Exception e) {
//					sadlModelManagerProvider.setUri(thisUri);
					visitor = sadlModelManagerProvider.get();
				}
			}
			visitor.init(this);	
			List<ConceptName> names = visitor.getNamedConceptsInNamedModel(thisUri, Scope.LOCALONLY);
			if (names == null || names.isEmpty()) {
				return;
			}

			// add ModelName to the Model, set alias=prefix
			Model model = SadlFactory.eINSTANCE.createModel();
			ModelName modelName = SadlFactory.eINSTANCE.createModelName();
			String prefix = names.get(0).getPrefix();
			if (prefix == null) {
				try {
					prefix = visitor.getConfigurationMgr(thisUri).getGlobalPrefix(names.get(0).getNamespace());
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (URISyntaxException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			modelName.setAlias(names.get(0).getPrefix());
			modelName.setBaseUri(URI.createURI(names.get(0).getUri()).trimFragment().toString());
			model.setModelName(modelName);

			// pass in OWL model folder to get CfgMgr
			try {
				Map<String,String> modelImportMappings = visitor.getConfigurationMgr(thisUri.trimSegments(1)).getImports(modelName.getBaseUri(), Scope.LOCALONLY);
				for (String importedURI: modelImportMappings.keySet()) {
					Import imp = SadlFactory.eINSTANCE.createImport();
					imp.setImportURI(importedURI);
					model.getImports().add(imp);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			getContents().add(model);

			// This may not work correctly at first, but let's try it.
			for (ConceptName name : names) {
				name = visitor.validateConceptName(context, name);
				ConceptType concept = name.getType();
				switch (concept) {
				case ANNOTATIONPROPERTY:
					break;
				case DATATYPEPROPERTY:
					ResourceName rnData= SadlFactory.eINSTANCE.createResourceName();
					rnData.setName(name.getName());
					PropertyDeclaration pdData = SadlFactory.eINSTANCE.createPropertyDeclaration();
					pdData.setPropertyName(rnData);
					model.getElements().add(pdData);
					break;
				case INDIVIDUAL:
					ResourceName rnInst = SadlFactory.eINSTANCE.createResourceName();
					rnInst.setName(name.getName());
					InstanceDeclaration id = SadlFactory.eINSTANCE.createInstanceDeclaration();
					id.setInstanceName(rnInst);
					model.getElements().add(id);
					break;
				case OBJECTPROPERTY:
					ResourceName rnObj= SadlFactory.eINSTANCE.createResourceName();
					rnObj.setName(name.getName());
					PropertyDeclaration pdObj = SadlFactory.eINSTANCE.createPropertyDeclaration();
					pdObj.setPropertyName(rnObj);
					model.getElements().add(pdObj);
					break;
				case ONTCLASS:
					ResourceName rnClass = SadlFactory.eINSTANCE.createResourceName();
					rnClass.setName(name.getName());
					ClassDeclaration cd = SadlFactory.eINSTANCE.createClassDeclaration();
					cd.setClassName(rnClass);
					model.getElements().add(cd);
					break;
				default:
					break;
				}
			}
			logger.info("Loaded "+thisUri.lastSegment()+" with "+model.getElements().size()+" concepts.");
		}
		catch (InvalidNameException e) {
			logger.error("Invalid name", e);
			throw new IOException("Unable to resolve name: " + e.getMessage(), e);
		} catch (ConfigurationException e) {
			logger.error("Invalid name", e);
			throw new IOException("Configuration error, unable to resolve name: " + e.getMessage(), e);
		} catch (URISyntaxException e) {
			logger.error("Invalid name", e);
			throw new IOException("Syntax error, unable to resolve name: " + e.getMessage(), e);
		}
	}

	/**
	 * Returns ID-type URI fragments for ResourceNames, otherwise delegates to
	 * the default Xtext implementation.
	 * {@inheritDoc}
	 */
	@Override
	public String getURIFragment(final EObject object) {
		String result = super.getURIFragment(object);
		if (object instanceof ResourceName) {
			ResourceName rName = (ResourceName) object;
			result = rName.getName();
			int colon = result.indexOf(':');
			if (colon > 0 && colon < result.length() - 1) {
				result = result.substring(colon + 1);
			}
		}
		return result;
	}

	/**
	 * Resolves ID-type URI fragments by searching our resource's contents, otherwise
	 * delegates to the default Xtext implementation.
	 * {@inheritDoc}
	 */
	@Override
	public EObject getEObject(String uriFragment) {
		EObject object = super.getEObject(uriFragment);
		if (object == null) {
			List<EObject> contents = getContents();
			object = getEObjectFromContents(contents, uriFragment);
		}
		return object;
	}

	private EObject getEObjectFromContents(List<EObject> contents, String uriFragment) {
		// First check if the object is in this list.
		for (EObject element : contents) {
			if (element instanceof ResourceName) {
				ResourceName rName = (ResourceName) element;
				if (uriFragment.equals(rName.getName())) {
					return rName;
				}
			}
		}

		// Then check any sublists recursively.
		for (EObject element : contents) {
			List<EObject> eContents = element.eContents();
			EObject object = getEObjectFromContents(eContents, uriFragment);
			if (object != null) {
				return object;
			}
		}

		return null;
	}
}
