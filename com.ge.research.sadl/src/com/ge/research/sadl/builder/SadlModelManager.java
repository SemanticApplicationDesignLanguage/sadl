/************************************************************************
 * Copyright � 2007-2010 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.builder;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.inject.Inject;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.MessageManager.SadlMessage;
import com.ge.research.sadl.builder.ModelManager.ImportListType;
import com.ge.research.sadl.model.ClassRestrictionCondition;
import com.ge.research.sadl.model.ClassRestrictionCondition.RestrictionType;
import com.ge.research.sadl.model.ConceptIdentifier;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ImportMapping;
import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.PendingModelError;
import com.ge.research.sadl.model.SadlEnumeratedClass;
import com.ge.research.sadl.model.SadlIntersectionClass;
import com.ge.research.sadl.model.SadlResourceByRestriction;
import com.ge.research.sadl.model.SadlUnionClass;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.Explain;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.KnownNode;
import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.Print;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.ValueTableNode;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.SadlJenaModelGetterPutter;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.sadl.AdditionalPropertyInfo;
import com.ge.research.sadl.sadl.AddlClassInfo;
import com.ge.research.sadl.sadl.AllValuesCondition;
import com.ge.research.sadl.sadl.AllValuesFrom;
import com.ge.research.sadl.sadl.AskQueryExpression;
import com.ge.research.sadl.sadl.CardCondition;
import com.ge.research.sadl.sadl.Cardinality;
import com.ge.research.sadl.sadl.ClassDeclaration;
import com.ge.research.sadl.sadl.ComplementOfClass;
import com.ge.research.sadl.sadl.Condition;
import com.ge.research.sadl.sadl.ConstructExpression;
import com.ge.research.sadl.sadl.ContentList;
//import com.ge.research.sadl.sadl.DataTypeRestriction;
import com.ge.research.sadl.sadl.DefaultValue;
import com.ge.research.sadl.sadl.DisjointClasses;
import com.ge.research.sadl.sadl.Display;
import com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom;
import com.ge.research.sadl.sadl.EnumeratedAllValuesFrom;
import com.ge.research.sadl.sadl.EquivalentConcepts;
import com.ge.research.sadl.sadl.ExistingInstanceAttribution;
import com.ge.research.sadl.sadl.ExistingResourceList;
import com.ge.research.sadl.sadl.Explanation;
import com.ge.research.sadl.sadl.ExplicitValue;
import com.ge.research.sadl.sadl.Expr;
import com.ge.research.sadl.sadl.Expression;
//import com.ge.research.sadl.sadl.Facets;
import com.ge.research.sadl.sadl.FunctionalProperty;
import com.ge.research.sadl.sadl.GraphPattern;
import com.ge.research.sadl.sadl.HasValue;
import com.ge.research.sadl.sadl.HasValueCondition;
import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.InstanceDeclaration;
import com.ge.research.sadl.sadl.InstanceDifferentFrom;
import com.ge.research.sadl.sadl.InstancesAllDifferent;
import com.ge.research.sadl.sadl.IntersectionResource;
import com.ge.research.sadl.sadl.InverseFunctionalProperty;
import com.ge.research.sadl.sadl.InverseProperty;
import com.ge.research.sadl.sadl.LiteralList;
import com.ge.research.sadl.sadl.LiteralValue;
import com.ge.research.sadl.sadl.MaxCardCondition;
import com.ge.research.sadl.sadl.MaxCardinality;
import com.ge.research.sadl.sadl.MinCardCondition;
import com.ge.research.sadl.sadl.MinCardinality;
import com.ge.research.sadl.sadl.ModelElement;
import com.ge.research.sadl.sadl.ModelName;
import com.ge.research.sadl.sadl.NecessaryAndSufficient;
import com.ge.research.sadl.sadl.OfPhrase;
import com.ge.research.sadl.sadl.PropValPartialTriple;
import com.ge.research.sadl.sadl.PropertyDeclaration;
import com.ge.research.sadl.sadl.Range;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceByRestriction;
import com.ge.research.sadl.sadl.ResourceIdentifier;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.Rule;
import com.ge.research.sadl.sadl.SelectExpression;
import com.ge.research.sadl.sadl.SomeValuesCondition;
import com.ge.research.sadl.sadl.SomeValuesFrom;
import com.ge.research.sadl.sadl.SymmetricalProperty;
import com.ge.research.sadl.sadl.TransitiveProperty;
import com.ge.research.sadl.sadl.UnionResource;
//import com.ge.research.sadl.sadl.UserDefinedDataType;
import com.ge.research.sadl.sadl.VariableList;
import com.ge.research.sadl.sadl.util.SadlSwitch;
import com.ge.research.sadl.utils.SadlUtils;
import com.ge.research.sadl.utils.SadlUtils.ConceptType;
import com.hp.hpl.jena.vocabulary.OWL;


/**
 * Visits a SADL document's Ecore model and makes ModelManager calls
 * to turn it into a Jena model.
 * 
 * $Author: crapo $ 
 * $Revision: 1.4 $ Last modified on   $Date: 2014/11/03 19:20:22 $
 */
public class SadlModelManager extends SadlSwitch<EObject> implements IPartListener2{

    private static final Logger logger = LoggerFactory.getLogger(SadlModelManager.class);
    
    // additions for Context and Requirements:
    
    private int errorCount = 0;
    private boolean markErrors = true;
    private int maxWarnings = 100;
    private int maxErrors = 100;
    
    private boolean deepValidationOff = false;
    
    // there is one ConfigurationManager per project
    private Hashtable<URI, IConfigurationManagerForIDE> configurationMgrMap = new Hashtable<URI, IConfigurationManagerForIDE>();
    private IConfigurationManagerForIDE lastConfigMgr = null;
    
    private boolean savingModel = false;	// true if this is part of build (or save)
    private boolean editorOpen = false;
    private boolean addedAsListener = false;

    class ModelInfo {
    	private ModelManager model;
        private IntermediateFormTranslator ifTranslator;
        private boolean completed = false;
    	
		private void setModel(ModelManager model) {
			this.model = model;
		}
		private ModelManager getModel() {
			return model;
		}
		private void setIfTranslator(IntermediateFormTranslator ifTranslator) {
			this.ifTranslator = ifTranslator;
		}
		private IntermediateFormTranslator getIfTranslator() {
			return ifTranslator;
		}
    }
    
//    static ThreadLocal<ModelInfo> mData = new ThreadLocal<ModelInfo>();
    private Resource currentResource = null;
    
    private Hashtable<URI, ModelInfo> modelMgrs = new Hashtable<URI, ModelInfo>();

	private int translationErrors = 0;
    
	@Inject
	private ConfigurationManagerForIDE.Provider configurationManagerProvider;
	
    /**
     * Constructs the Ecore model visitor.
     */
    public SadlModelManager() {
//    	OntDocumentManager.getInstance().setCacheModels(false);
    }
    
    public synchronized boolean processModel(Resource resource, boolean persistModel, boolean editorOpen, SubMonitor progress) throws CoreException {
        logger.debug("SMM.processModel called from " + (editorOpen ? "Highlighting Calculator" : "Builder") + " for '" + resource.toString() + "'");
        this.editorOpen = editorOpen;
        boolean modelPreExists = hasModelManager(resource);
		ModelInfo minfo = getModelInfo(resource);
    	TreeIterator<EObject> iter = null;
		if (minfo == null || !minfo.completed || editorOpen) {
			iter = EcoreUtil.getAllContents(resource, true);
		}
        boolean returnStatus = processModel(resource, iter, persistModel, editorOpen, progress);
        return returnStatus;
    }
    
    public synchronized boolean processModel(Resource resource, TreeIterator<EObject> iter, boolean persistModel, boolean editorOpen, SubMonitor progress) throws CoreException {
//    	if (!editorOpen) {
//    		// This editor is not known to be open; however, let's see if we already have an entry for it 
//    		//	and if we do treat it as if it were open (because maybe it is)
//    		if (modelMgrs.containsKey(resource.getURI())) {
//    			editorOpen = true;
//    		}
//    	}
        savingModel = persistModel;
        if (iter != null) {
	        logger.debug("Initializing resource "+resource.toString());
	    	this.init(resource);
	        logger.debug("Done initializing resource.");
	        int iCnt = 0;
	        while(iter.hasNext()) { 
	            EObject eObject = iter.next();
	            iCnt++;
	            try {
		            if (this.doSwitch(eObject) != null) {
		                iter.prune();
		            }
		        } catch (Throwable t) {
		        	t.printStackTrace();
		        }
	        }
	        logger.debug("Done iterating thru resource. No. iteration = "+iCnt);
	        this.end();
	        logger.debug("Done ending resource.");
        }
        else {
        	this.setResource(resource);
        	logger.debug("Resource '" + resource.toString() + "' is already processed; using saved ModelInfo.");
        }

    	if (persistModel) {
	        // Save the Jena model.  The file won't exist the first time 
	        // so we have to create it before we can save the model.
	        File file = ResourceManager.getOwlFileForSadlResource(resource);
	        if (!file.exists()) {
//	            byte[] bytes = new byte[0];
//	            InputStream source = new ByteArrayInputStream(bytes);
	            try {
//					file.create(source, IResource.NONE, null);
	            	file.createNewFile();
		        }
	            catch (Throwable t) {
	        		String msg = "Unexpected error: file '" + file.getAbsolutePath() + "' doesn't exist but can't be created: " + t.getLocalizedMessage();
	        		logger.error(msg);
	        		System.err.println(msg);
	            }
	        }
	        String path = file.getAbsolutePath();
	        List<ModelError> errors = save(path);
	        if (errors != null) {
	        	for (int i = 0; i < errors.size(); i++) {
	        		ModelError error = errors.get(i);
	        		if (error.getErrorType().equals(ErrorType.ERROR)) {
	        			System.err.println(error.toString());
	        		}
	        		else {
	        			logger.error(error.toString());
	        			System.err.println(error.toString());
	        		}
	        	}
	        }
	        else {
	        	// Save was successful--update each ModelManager with the modified model so that any models using 
	        	//	this one will be updated.
	        	ModelManager savingMM = getModel();
	        	Iterator<URI> mmitr = modelMgrs.keySet().iterator();
	        	while (mmitr.hasNext()) {
	        		URI key = mmitr.next();
	        		ModelInfo minfo = modelMgrs.get(key);
	        		if (minfo.getModel().updateImport(savingMM)) {
	        			logger.debug("Updated model '" + savingMM.getModelName() + "' in imports of model '" + 
	        					minfo.getModel().getModelName() + "' after successful save.");
	        		}
	        	}
	        }
        }
        return true;
    }
    
    private void dumpMMContext() {
    	System.out.println("Dumping MM context for: " + getResource().toString());
    	System.out.println("Jena model: ");
    	getModel().dump(System.out, "N3");
    }

    /**
     * Tells ModelManager to start constructing a new Jena model.  This method
     * should be called before visiting a SADL document.
     * @param resource 
     */
    public void init(Resource resource) {
    	// remember the Resource of the SADL model
    	this.setResource(resource);
    	
    	IPreferencesService service = Platform.getPreferencesService();
    	if (service != null) {
    		deepValidationOff = service.getBoolean("com.ge.research.sadl.Sadl", "deepValidationOff", false, null);
    	}
    	
    	try {
            // What project is this? There should be one ConfigurationManager per project.
            URI projectUri = ResourceManager.getProjectUri(resource.getURI());
            logger.info("SMM called for project " + projectUri.toFileString());

            IConfigurationManagerForIDE configurationMgr = getConfigurationMgr(projectUri.toString() + "/" + ResourceManager.OWLDIR);
        	// Get a ModelManager instance associated with this thread and pass it the model Resource
            getModel().init(configurationMgr, resource.getURI());
            getModel().setDeepValidationOff(deepValidationOff);
            
		} catch (ConfigurationException e) {
			e.printStackTrace();
		}
    	catch (Throwable t) {
    		t.printStackTrace();
    	}
    	
        // delete all markers for this Resource (they will be regenerated
        //	if the causation still exists)
        deleteMarkers(resource, false);
        setTranslationErrors(0);
        
    }
    
    /**
     * Call this method to begin a new translation to Intermediate Form.
     * 
     * @param target - the Rule, Test, Query, or null if none of these
     */
    public void setTranslationTarget(Object target) {
    	getIfTranslator().resetIFTranslator();
    	getIfTranslator().setTarget(target);
    }

    /**
     * Call this method when you might be parsing an empty model, e.g., when
     * performing highlighting for a new file.  We will need this baseUri in
     * order to provide an initial model name for content assist.
     */
    public void setModelBaseUri(String modelBaseUri) {
    	getModel().setModelBaseUri(modelBaseUri);
    }

    /**
     * Call this method to get the find out the current model baseUri. This is necessary
     * to determine if it has already been set or not.
     * 
     * @return - modelBaseUri
     */
	public String getModelBaseUri() {
		return getModel().getModelBaseUri();
	}

   /**
     * Get the name of the model for which this instance is providing services
     * @return
     */
    public String getModelName() {
    	return getModel().getModelName();
    }
    
    /**
     * Call this method to validate a ConceptName by finding it in the model,
     * setting the namespace, setting the ConceptType (type), and setting the
     * prefix if not already set.
     * 
     * @param name -- the name of the concept
     * @return -- the validated, fully annotated ConceptName
     */
    public ConceptName validateConceptName(Resource resource, ConceptName name) {
    	if (name.getType() != null && name.getType() != ConceptType.CONCEPT_NOT_FOUND_IN_MODEL) {
    		return name;
    	}
    	return getModel(resource).validateConceptName(name);
    }

    private ConceptIdentifier validateConceptName(ConceptName name) {
    	Resource rsc = getResource();
    	if (rsc != null) {
    		return validateConceptName(rsc, name);
    	}
		return null;
	}

    /**
     * Gets a concept in the model matching the given name.  OWL v1 allows
     * only one concept to be mapped to a name in the same model.
     * 
     * @param name -- the name of the concept
     * @return The type of token that an identifier is mapped to else 
     * ConceptType.CONCEPT_NOT_FOUND_IN_MODEL.
     */
    public ConceptType getConceptType(String name) {
    	try {
    		return getModel().getConceptType(name);
    	}
    	catch (Throwable t) {
    		t.printStackTrace();
    		return ConceptType.CONCEPT_NOT_FOUND_IN_MODEL;
    	}
    }
    
    /**
     * Method to set the type of list of possible import models to return
     * @param importListType
     */
	public void setImportListType(ImportListType importListType) {
		getModel().setImportListType(importListType);
	}
	
    /**
     * Method to get a pending error by name only, for use in vaidation 
     * 
     * @param conceptName
     * @return
     */
	public PendingModelError getPendingError(Resource resource, String conceptName) {
		ModelManager mm = getModel(resource);
		if (mm != null) {
			return mm.getPendingError(conceptName);
		}
		return null;
	}


    
    /**
     * Call this method to retrieve the EMF Resource for this SADL model.
     * 
     * @return
     */
    public Resource getModelResource() {
    	return getResource();
    }

    /**
     * Call this method to get a list of ExistingNames in the specified namespace
     * 
     * @param uri
     * @return
     * @throws InvalidNameException 
     * @throws IOException 
     */
    public List<ConceptName> getNamedConceptsInNamedModel(URI uri) throws InvalidNameException, IOException {
    	return getNamedConceptsInNamedModel(uri, Scope.INCLUDEIMPORTS);
    }
    
    public List<ConceptName> getNamedConceptsInNamedModel(URI uri, Scope scope) throws InvalidNameException, IOException {
    	String publicUri = null;
		try {
	   		if (uri.isPlatform()) {
	   			URL fileUri = FileLocator.toFileURL(new URL(uri.toString()));
	   			uri = URI.createURI(fileUri.getFile());
	   		}
	   		IConfigurationManagerForIDE cmgr = getConfigurationMgr(uri);
			if (cmgr != null) {
				publicUri = cmgr.getPublicUriFromActualUrl(uri.toString());
			}
			else {
				throw new InvalidNameException("Unable to find a model with URL '" + uri + "'");
			}
			if (cmgr.isSadlDerived(publicUri)) {
				return getModel().getNamedConceptsInNamedModel(publicUri, null);
			}
			else {
				return cmgr.getNamedConceptsInModel(cmgr.getModelGetter().getOntModel(publicUri, uri.toString(), null), publicUri, null, scope);
			}
		} catch (ConfigurationException e) {
			return Collections.emptyList();
		} catch (MalformedURLException e) {
			e.printStackTrace();
		} catch (URISyntaxException e) {
			e.printStackTrace();
		}
		return null;
    }

    /**
     * Call this method to get a List of ExistingNames which match the 
     * conditions specified by the arguments.
     * 
     * @param cType -- the ConceptType desired (see ConceptType Enum)
     * @param scope -- the scope: LOCALONLY for this model only else INCLUDEIMPORTS
     * @return
     * @throws InvalidNameException 
     * @throws ConfigurationException 
     * @throws MalformedURLException 
     */
    public List<ConceptName> getNamedConceptsOfType(ConceptType cType, Scope scope) throws InvalidNameException, ConfigurationException, MalformedURLException {
    	if (cType != null && cType.equals(ConceptType.MODELNAME)) {
    		return getModel().getPossibleModelNames();
    	}
    	else {
    		return getModel().getNamedConceptsOfType(cType, scope);
    	}
    }

    /**
     * Method to return all of the concepts of  in the specified Scope belonging to the specified class.
     * 
     * @param cType
     * @param scope
     * @param constraint
     * @return
     */
    public List<ConceptName> getNamedConceptsOfType(ConceptType cType,
			Scope scope, ConceptName constraint) {
    	return getModel().getNamedConceptsOfType(cType, scope, constraint);
	}

    /**
     * Method to return the ImportMapping for a given public URI
     * @param alias
     * @return
     */
//    public ImportMapping getImportMappingByPublicUri(String pubUri) {
//    	return getModel().getImportMappingByPublicUri(pubUri);
//    }
    
    /**
     * Method to return a list containing the ImportMapping of each import to this model 
     * @return
     */
    public Collection<ImportMapping> getModelImportMappings() {
    	return getModel().getImportMappings();
    }

    /**
     * Tells ModelManager to finish building the Jena model and report
     * any remaining errors.
     */
    public void end() {
    	List<ModelError> remainingErrors = null;
		try {
			Map<Object,List<ModelError>> validationErrorMap = getModel().validateStatements();
			if (validationErrorMap != null) {
				Iterator<Object> keyiter = validationErrorMap.keySet().iterator();
				while (keyiter.hasNext()) {
					Object key = keyiter.next();
					if (key instanceof EObject) {
						annotateErrors((EObject) key, validationErrorMap.get(key));
					}
				}
			}
			remainingErrors = getModel().end();
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        reportErrors(remainingErrors);
		ModelInfo minfo = getModelInfo(getResource());
		if (minfo != null) {
			logger.debug("SMM end setting ModelInfo completed true for resource '" + getResource().toString() + "'");
			minfo.completed = true;
		}
    }

    /**
     * Tells ModelManager to save the Jena model and report any errors.
     */
    public List<ModelError> save(String path) {
        logger.debug("saving Jena model to {}", path);
        try {
        	List<ModelError> errors = getModel().save(path);
            return errors;
       }
        catch (Exception e) {
            logger.error(e.getMessage());
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Sets the model's base URI and version.
     */
    @Override
    public EObject caseModelName(ModelName object) {
    	String alias = object.getAlias();
    	EList<ContentList> cmtlist = object.getAnnContent();
    	List<String> comments = null;
    	if (cmtlist.size() > 0) {
    		comments = new ArrayList();
    		Iterator<ContentList> cliter = cmtlist.iterator();
    		while (cliter.hasNext()) {
    			ContentList cl = cliter.next();
    			EList<String> cl2 = cl.getAnnContent();
    			Iterator<String> inneritr = cl2.iterator();
    			while (inneritr.hasNext()) {
    				comments.add(inneritr.next().toString());
    			}
    		}
    	}
        annotateErrors(object, addModelName(object.getBaseUri(), object.getVersion(), alias, comments));   
        if (!addedAsListener) {
        	IWorkbenchWindow wndw = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
        	if (wndw != null) {
        		wndw.getPartService().addPartListener(this);
        		addedAsListener = true;
//        		System.out.println("Added part listener");
        	}
//        	else {
//        		System.out.println("Failed to add part listener");
//        	}
        }
        return object;
    }
    
    /**
     * This method allows adding a model name from other editors that just need an empty model.
     * @param _modelName
     * @param version
     * @param alias
     * @param comments 
     * @return
     */
    public List<ModelError> addModelName(String _modelName, String version, String alias, List<String> comments) {
    	return getModel().setModelName(_modelName, version, alias, comments);
    }

    /**
     * Adds an import statement to the model.
     */
    @Override
    public EObject caseImport(Import object) {
        annotateErrors(object, addImport(object.getImportURI(), object.getAlias()));
        return object;
    }
    
    /**
     * This method allows adding imports from other editors that just need a base set of imports.
     * 
     * @param importUri
     * @param alias
     * @return
     */
    public List<ModelError> addImport(String importUri, String alias) {
    	return getModel().addImports(importUri, alias);
    }

    /**
     * Adds a new class to the model.  Also adds any enumerated instances
     * of the class and any properties describing the class.
     */
    @Override
    public EObject caseClassDeclaration(ClassDeclaration object) {

        List<String> newClassNames = new ArrayList<String>(3);
        ConceptIdentifier superClsName = createConceptIdentifier(object.getClassIdentifier());
        
        if (superClsName instanceof ConceptName) {
        	// check to make sure this isn't a subproperty declaration
        	superClsName = validateConceptName((ConceptName) superClsName);
        	ConceptType sctype = ((ConceptName) superClsName).getType(); //getModel().getConceptType((ConceptName)superClsName);
        	if (sctype.equals(ConceptType.OBJECTPROPERTY) || sctype.equals(ConceptType.DATATYPEPROPERTY)) {
                boolean isObjectProperty;
                if (sctype.equals(ConceptType.OBJECTPROPERTY)) {
                	isObjectProperty = true;
                }
                else {
                	isObjectProperty = false;
                }
        		if (object.getClassName() != null) {
                    String newPropName = object.getClassName().getName();
                    if (isObjectProperty) {
                    	annotateErrors(object, getModel().addObjectProperty(newPropName, (ConceptName)superClsName, null, false));
                    }
                    else {
	                    annotateErrors(object, getModel().addDatatypeProperty(newPropName, (ConceptName)superClsName, null, false));                    	
                    }
                    addAnnotations(object.getClassName());
                    return object;
                }
        		else if (object.getClassList() != null) {
	                for (ResourceName className : object.getClassList().getNames()) {
	                    String newPropName = className.getName();
	                    if (isObjectProperty) {
	                    	annotateErrors(object, getModel().addObjectProperty(newPropName, (ConceptName)superClsName, null, false));	                    	
	                    }
	                    else {
	                    	annotateErrors(object, getModel().addDatatypeProperty(newPropName, (ConceptName)superClsName, null, false));
	                    }
                       	addAnnotations(className);
	                }
                    return object;
        		}
        	}
        }

        // Add the new class name(s) to the model.
        if (object.getClassName() != null) {
            String newClassName = object.getClassName().getName();
            newClassNames.add(newClassName);
            annotateErrors(object, getModel().addClass(newClassName, superClsName));
            addAnnotations(object.getClassName());
        }
        else if (object.getClassList() != null) {
            for (ResourceName className : object.getClassList().getNames()) {
                String newClassName = className.getName();
                newClassNames.add(newClassName);
                annotateErrors(object, getModel().addClass(newClassName, superClsName));
                addAnnotations(className);
            }
        }
        else {
            logger.error("can't happen: class declaration has null className and null classList");
        }

        // Add any enumerated instances of the class to the model as well.
        if (object.getMustBeOneOf() != null) {
            int size = object.getMustBeOneOf().getInstanceList().getNames().size();
            List<ConceptName> enumeratedInstances = new ArrayList<ConceptName>(size);
            for (ResourceName instanceName : object.getMustBeOneOf().getInstanceList().getNames()) {
                ConceptName instName = new ConceptName(instanceName.getName());
                enumeratedInstances.add(instName);
            }
            ConceptIdentifier equivalentClass = new SadlEnumeratedClass(enumeratedInstances);
            for (String newClassName : newClassNames) {
                ConceptName newClsName = new ConceptName(newClassName);
                annotateErrors(object, getModel().addEquivalentClass(newClsName, equivalentClass));
            }
            for (ResourceName instanceName : object.getMustBeOneOf().getInstanceList().getNames()) {
                addAnnotations(instanceName);
            }
        }

        // Add any properties describing the class to the model as well.
        if (object.getDescribedBy() != null) {
            for (AddlClassInfo describedBy : object.getDescribedBy()) {
            	if (describedBy.getRestriction() != null) {
            		// this is a class restriction
            		Condition cond = describedBy.getRestriction();
            		ResourceByName propName = describedBy.getPropertyByName();
            		String strName = propName.getName().getName();
            		ConceptType ptype = getModel().getConceptType(strName);
            		if (ptype.equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
                		String propStrName = null;
                		if (describedBy.getPropertyName() != null) {
                			propStrName = describedBy.getPropertyName().getName();
                		}
                		else if (describedBy.getPropertyByName() != null) {
                			propStrName = describedBy.getPropertyByName().getName().getName();
                		}           	
            			ResourceIdentifier rangeClass = null;
	            		if (cond instanceof SomeValuesCondition) {
	            			EObject restrict = ((SomeValuesCondition)cond).getRestriction();
	            			if (restrict instanceof ResourceIdentifier) {
	            				rangeClass = (ResourceIdentifier) restrict;
	            			}
	            		}
	            		else if (cond instanceof AllValuesCondition) {
	            			EObject restrict = ((AllValuesCondition)cond).getRestriction();
	            			if (restrict instanceof ResourceIdentifier) {
	            				rangeClass = (ResourceIdentifier) restrict;
	            			}
	            		}
	            		else if (cond instanceof HasValueCondition) {
	            			// this will fail in ModelManager, as it should as the range is ambiguous and the property should already exist for this restriction.
	            			rangeClass = null;
	            		}
	            		else if (cond instanceof CardCondition) {
            				rangeClass = ((CardCondition)cond).getClassQualifier();
            			}
	            		else if (cond instanceof MinCardCondition) {
	            			rangeClass = ((MinCardCondition)cond).getClassQualifier();
	            		}
	            		else if (cond instanceof MaxCardCondition) {
	            			rangeClass = ((MaxCardCondition)cond).getClassQualifier();
	            		}
            			
             			addProperty(object, newClassNames, propStrName, false, rangeClass);
            		}
                    for (String newClassName : newClassNames) {
                        ConceptName newClsName = new ConceptName(newClassName);
                		ConceptName prop = createConceptIdentifier(propName);
	            		addClassRestriction(object, newClsName, prop, cond);
                    }
            	}
            	else if (describedBy.getPropertyByName() != null && describedBy.getRange() == null) {
                    // Only the property's name was declared (so there's domain but no range).
                    ConceptName propName = new ConceptName(describedBy.getPropertyByName().getName().getName());
                    for (String newClassName : newClassNames) {
                        ConceptName newClsName = new ConceptName(newClassName);
                        annotateErrors(object, getModel().addPropertyDomain(propName, newClsName, false, null));
                    }
                }
                else {
                	// if it's a new property name then the range must be given, as required by the grammar
            		String propName = null;
            		if (describedBy.getPropertyName() != null) {
            			propName = describedBy.getPropertyName().getName();
            		}
            		else if (describedBy.getPropertyByName() != null) {
            			propName = describedBy.getPropertyByName().getName().getName();
            		}           	
            		Range range = describedBy.getRange();
            		Object objRange = null;
            		boolean singleValuedOnClass = (range != null && range.getSingle() != null);
            		if (range != null && range.getType() != null) {
            		    if (range.getType().getClassIdentifier() != null) {
            		        // An object property was declared.
            		    	objRange = createConceptIdentifier(range.getType().getClassIdentifier());
            		    }
            		    else {
////            		        // A datatype property was declared.
	           		        objRange = range.getType().getDataType().getLiteral();
////            		    	String rng = range.getType().getClassIdentifier()
////            		    	RDFDatatype dt = TypeMapper.getInstance().getTypeByName(rng);
////            		    	if (dt != null) {
////            		    		objRange = dt;
////            		    	}
            		    }
            		}
                    addProperty(object, newClassNames, propName, singleValuedOnClass, objRange);
        		    addAnnotations(describedBy.getPropertyName());
                }
            }
        }

        return object;
    }

	private void addClassRestriction(EObject object,
			ConceptIdentifier newClsName, ConceptName propName, Condition cond) {
		if (cond instanceof SomeValuesCondition) {
			addSomeValuesFromRestriction(object, newClsName, propName, (SomeValuesCondition)cond);
		}
		else if (cond instanceof AllValuesCondition) {
			addAllValuesFromRestriction(object,  newClsName, propName, (AllValuesCondition)cond);
		}
		else if (cond instanceof HasValueCondition) {
			addHasValueRestriction(object, newClsName, propName, (HasValueCondition)cond);
		}
		else if (cond instanceof CardCondition) {
			addCardinalityRestriciton(object, newClsName, propName, (CardCondition)cond);
		}
		else if (cond instanceof MinCardCondition) {
			addMinCardinalityRestriction(object, newClsName, propName, (MinCardCondition)cond);
		}
		else if (cond instanceof MaxCardCondition) {
			addMaxCardinalityRestriction(object, newClsName, propName, (MaxCardCondition)cond);
		}
	}

	private void addProperty(ClassDeclaration object, List<String> newClassNames, 
			String propName, boolean singleValuedOnClass , Object range) {
//		boolean isRDFDataType = getModel().isRDFDataType(range);
//		if (isRDFDataType) {
//			range = range.toString();
//		}
//	    if (!isRDFDataType && range instanceof ResourceIdentifier) {
//	    	range = createConceptIdentifier((ResourceIdentifier)range);
//	    }
//	    if (!isRDFDataType && range instanceof ConceptIdentifier) {
//	        // An object property was declared.
//	        for (String newClassName : newClassNames) {
//	            ConceptName superPropName = null;
//	            annotateErrors(object, getModel().addObjectProperty(propName, superPropName, (ConceptIdentifier) range, false));
//	            ConceptName pName = new ConceptName(propName);
//	            ConceptName newClsName = new ConceptName(newClassName);
//	            annotateErrors(object, getModel().addPropertyDomain(pName, newClsName, singleValuedOnClass, range));
//	        }
//	    }
	    if (range instanceof ResourceIdentifier) {
	    	range = createConceptIdentifier((ResourceIdentifier)range);
	    }
	    if (range instanceof ConceptIdentifier) {
	        // An object property was declared.
	        for (String newClassName : newClassNames) {
	            ConceptName superPropName = null;
	            annotateErrors(object, getModel().addObjectProperty(propName, superPropName, (ConceptIdentifier) range, false));
	            ConceptName pName = new ConceptName(propName);
	            ConceptName newClsName = new ConceptName(newClassName);
	            annotateErrors(object, getModel().addPropertyDomain(pName, newClsName, singleValuedOnClass, range));
	        }
	    }
	    else if (range instanceof String){
	        // A datatype property was declared.
	        String xsdRange = (String) range;
	        for (String newClassName : newClassNames) {
	            ConceptName superPropName = null; // I don't think this construct allows super property declaration, awc, 5/21/2010 // new ConceptName(newClassName);
	            annotateErrors(object, getModel().addDatatypeProperty(propName, superPropName, xsdRange, false));
	            ConceptName pName = new ConceptName(propName);
	            ConceptName newClsName = new ConceptName(newClassName);
	            annotateErrors(object, getModel().addPropertyDomain(pName, newClsName, singleValuedOnClass, xsdRange));
	        }
	    }
	}

	/**
     * Adds a new property to the model.
     */
    @Override
    public EObject casePropertyDeclaration(PropertyDeclaration object) {
    	ResourceName resourceName = object.getPropertyName();
    	if (resourceName == null) {
    		resourceName = object.getAnnotationProperty();
    		if (resourceName != null) {
	            String propName = resourceName.getName();
	            annotateErrors(object, getModel().addAnnotationProperty(propName));
    		}
    		else {
    			// shouldn't have both getPropertyName() and getAnnotationProperty() return null...
    			reportError(object, "The PropertyDeclaration object has neither a property name nor an annotation property name. This should not happen.");
    		}
        }
        else {
            String propName = resourceName.getName();
            ConceptName superPropName = null;
            ConceptIdentifier domainCls = null;
            ConceptIdentifier rangeCls = null;
            String xsdRange = null;
            Condition cond = null;
            boolean isObjProp = false;
            ConceptName inversePropertyName = null;
            boolean isSingleValuedOnClass = false;
            boolean isFunctional = false;
            boolean isInvFunctional = false;
            boolean isSymmetrical = false;
            boolean isTransitive = false;
            
     		if (object.getSuperPropName() != null) {
    			superPropName = createConceptIdentifier(object.getSuperPropName());
    		}
     		if (object.getDomain() != null) {
     			domainCls = createConceptIdentifier(object.getDomain());
     		}
     		if (object.getRangeResource() != null) {
     			isObjProp = true;
     			rangeCls = createConceptIdentifier(object.getRangeResource());
     		}
 
            EList<AdditionalPropertyInfo> addlPI = object.getAddlPropInfo();
            if (addlPI != null) {
            	Iterator<AdditionalPropertyInfo> itr = addlPI.iterator();
            	while (itr.hasNext()) {
            		AdditionalPropertyInfo api = itr.next();
            		if (api.getDomain() != null){
            			domainCls = createConceptIdentifier(api.getDomain());
            		}
            		else if (api.getRange() != null) {
            			Range range = api.getRange();
                        if (range.getSingle() != null) {
                        	isSingleValuedOnClass = true;
                        }
                        if (range.getType().getClassIdentifier() != null) {
                            // An object property was declared.
                            isObjProp = true;
                            rangeCls = createConceptIdentifier(range.getType().getClassIdentifier());
                        }
//                        boolean isRDFDataType = false;
//                        if (rangeCls != null) {
//                        	isRDFDataType = getModel().isRDFDataType(rangeCls);
//                        	if (isRDFDataType) {
//                        		xsdRange = rangeCls.toString();
//                        		isObjProp = false;
//                        	}
//                        }
            			else if (range.getType().getDataType() != null) {
                        	xsdRange = range.getType().getDataType().getLiteral();
//                        	xsdRange = range.getType().getDataType();
//                        	RDFDatatype dt = TypeMapper.getInstance().getTypeByName(xsdRange);
//            		    	if (dt != null) {
//            		    		xsdRange = dt.toString();
//            		    	}
                        }
            		}
            		else if (api.getCond() != null) {
            			cond = api.getCond();
            		}
            		else if (api.getIsfunc() != null) {
            			isFunctional = true;
            		}
            		else if (api.getIsinvfunc() != null) {
            			isInvFunctional = true;
            		}
            		else if (api.getIsInvOf() != null) {
            			inversePropertyName = createConceptIdentifier(api.getIsInvOf().getPropertyName2());
            		}
            		else if (api.getIsSym() != null) {
            			isSymmetrical = true;
            		}
            		else if (api.getIsTrans() != null) {
            			isTransitive = true;
            		}
            		else {
            			try {
            				throw new Exception("Unknown property declaration construct");
            			}
            			catch (Exception e) {
            				logger.error("", e);
            			}
            		}
            	}
            }
			if (isObjProp && rangeCls != null) {
				annotateErrors(object, getModel().addObjectProperty(propName, superPropName, rangeCls, isFunctional));
			}
			else if (cond != null) {
				addClassRestriction(object, domainCls, new ConceptName(propName), cond);
//				annotateErrors(object, getModel().addClassRestriction(domainCls, new ConceptName(propName), cond));
			}
			else if (xsdRange != null){
				// A datatype property was declared.
				annotateErrors(object, getModel().addDatatypeProperty(propName, superPropName, xsdRange, isFunctional));
			}
			else if (superPropName != null) {
				superPropName = getModel().validateConceptName(superPropName);
				ConceptType propType = superPropName.getType();
				if (propType.equals(ConceptType.OBJECTPROPERTY)) {
					annotateErrors(object, getModel().addObjectProperty(propName, superPropName, rangeCls, isFunctional));
				}
				else if (propType.equals(ConceptType.DATATYPEPROPERTY)) {
					annotateErrors(object, getModel().addDatatypeProperty(propName, superPropName, xsdRange, isFunctional));
				}
				else {
					logger.error("Property '" + propName + "' super property '" + superPropName.toString() + "' not of expected type: please fix");
				}
			}

			annotateErrors(object, getModel().addPropertyDomain(new ConceptName(propName), domainCls, isSingleValuedOnClass, rangeCls));
			if (isFunctional) {
				annotateErrors(object, getModel().addFunctionalProperty(new ConceptName(propName)));
			}
			if (isSymmetrical) {
				annotateErrors(object, getModel().addSymmetricalProperty(new ConceptName(propName)));
			}
			if (isInvFunctional) {
				annotateErrors(object, getModel().addInverseFunctionalProperty(new ConceptName(propName)));
			}
			if (isTransitive) {
				annotateErrors(object, getModel().addTransitiveProperty(new ConceptName(propName)));
			}
			if (inversePropertyName != null) {
				annotateErrors(object, getModel().addInverseProperties(new ConceptName(propName), inversePropertyName));
			}
        }
        if (resourceName != null) {
        	addAnnotations(resourceName);
        }
        return object;
    }

    private void addAnnotations(ResourceName resourceName) {
//    	if (errors == null || errors.size() == 0) {
		if (resourceName != null && resourceName.getName() != null && resourceName.getAnnType() != null) {
			EList<ContentList> contents = resourceName.getAnnContent();
			EList<String> types = resourceName.getAnnType();
			for (int i = 0; i < contents.size() && i < types.size(); i++) {
				String type = types.get(i);
				ContentList contList = contents.get(i);
				Iterator<String> itr = contList.getAnnContent().listIterator();
				while (itr.hasNext()) {
					getModel().addAnnotationToResource(resourceName.getName(), type, itr.next());
				}
			}
		}
	}

	/**
     * Adds a new instance to the model.  Also adds any triples about the
     * instance to the model as well.
     */
    @Override
    public EObject caseInstanceDeclaration(InstanceDeclaration object) {
        ResourceName instName = getInstanceName(object);
        ConceptIdentifier clsName;
        if (object.getTypeDecl() != null) {
            clsName = createConceptIdentifier(object.getTypeDecl().getType().getClassIdentifier());
        }
        else if (object.getClassName() != null) {
            clsName = createConceptIdentifier(object.getClassName());
        }
        else {
            logger.error("Invalid InstanceDeclaration object: please fix");
            reportError(object, "Invalid instance declaration object: " + object.toString());
            return object;
        }
        ConceptName name = instName != null ? new ConceptName(instName.getName()) : null;
        if (name != null) {
            annotateErrors(object, getModel().addInstance(name.toString(), clsName));
            try {
				addStatements(object, name, object.getAddlInfoItems());
			} catch (InvalidNameException e) {
				reportError(instName, e.getMessage());
			}
        }
        else {
            // a blank node must be created with its attributes in a atomic operation
            List<Object[]> attributes = null;
            try {
                attributes = getAttributeNameValuePairs(object.getAddlInfoItems());
            } catch (InvalidNameException e) {
                reportError(object, e.getMessage());
            }
            annotateErrors(object, getModel().addBNodeInstance(clsName, attributes));
            getModel().movePendingValidationStatementsToQueuedValidationStatements(object);
        }
        if (instName != null) {
           	addAnnotations(instName);
        }
        return object;
    }
    
	/**
     * Method to obtain the instance name (if given) from an InstanceDeclaration
     * 
     * @param object
     * @return - the instance ConceptName or null if none is found
     */
    private ResourceName getInstanceName(InstanceDeclaration object) {
    	ResourceName instName = null;
        if (object.getTypeDecl() != null) {
            instName = object.getTypeDecl().getInstName();
        }
        else if (object.getClassName() != null) {
            instName = object.getInstanceName();
        }
        else {
            logger.error("Invalid InstanceDeclaration object: please fix");
            reportError(object, "Invalid instance declaration object: " + object.toString());
            return null;
        }
        return instName;
    }

    /**
     * Adds any triples about an instance to the model.
     */
    @Override
    public EObject caseExistingInstanceAttribution(ExistingInstanceAttribution object) {
        if (object.getSubj() != null && object.getSubj().getName() != null && object.getSubj().getName().getName() != null) {
            ConceptName subject = new ConceptName(object.getSubj().getName().getName());
            if (object.getAddlInfoItems() != null && object.getAddlInfoItems().size() > 0) {
                try {
					addStatements(object, subject, object.getAddlInfoItems());
				} catch (InvalidNameException e) {
					reportError(object, e.getMessage());
				}
            }
        }

        else if (object.getPOfS() != null) {
        	ResourceByName subj = object.getPOfS().getSubject();
        	if (subj == null) {
        		try {
        			reportError(object, "Subject of Existing Instance is null--how did this happen?") ;
         		}
        		catch (Exception e) {
        			e.printStackTrace();
        		}
        		return object;
        	}
        	ConceptName subject = new ConceptName(subj.getName().getName());
        	// we have the subject and a list of edges. The new object values is to be assigned to the subject of the last edge.
//            	EList<ResourceByName> properties = object.getPoChain().getPropertyNames();
        	EList<OfPhrase> phrases = object.getPOfS().getOfphrs();
        	List<ConceptName> properties = new ArrayList<ConceptName>(); 
        	Iterator<OfPhrase> phritr = phrases.iterator();
        	while (phritr.hasNext()) {
        		OfPhrase phr = phritr.next();
        		ResourceByName phrname = phr.getPropertyName();
        		String pname = phrname.getName().getName();
        		if (pname != null) {
        			properties.add(new ConceptName(pname));
        		}
        		else {
        			properties.add(null);
        		}
        	}
        	ConceptName lastSubject = subject;
        	for (int i = properties.size() - 1;  i >= 0; i--) {
        		ConceptName thisProperty = properties.get(i);
        		if (thisProperty != null && i == 0) {
        			// this is the last edge, ready to make assignment of the new value to the last node
        			ConceptName instanceValue = null;
        			Object nonExistingNameValue = null;
                	EObject obj = object.getObj();
                	if (obj instanceof com.ge.research.sadl.sadl.Object) {
                		nonExistingNameValue = ((com.ge.research.sadl.sadl.Object)obj).getVal();
                		if (nonExistingNameValue instanceof ResourceByName) {
                			instanceValue = new ConceptName(((ResourceByName)nonExistingNameValue).getName().getName());
                			nonExistingNameValue = null;
                		}
                    	else if (nonExistingNameValue instanceof InstanceDeclaration) {
                    		caseInstanceDeclaration((InstanceDeclaration)nonExistingNameValue);
                    		nonExistingNameValue = getModel().getLastInstanceCreated();
                    	}                	}
                	else if (obj instanceof ResourceByName) {
                		instanceValue = new ConceptName(((ResourceByName)obj).getName().getName());
                	}
                	else if (obj instanceof ExplicitValue && ((ExplicitValue)obj).getInstName() != null) {
                		instanceValue = new ConceptName(((ExplicitValue)obj).getInstName().getName().getName());
                	}
                	else if (obj instanceof LiteralValue) {
                		nonExistingNameValue = (LiteralValue)obj;
                	}
                	else if (obj instanceof ExplicitValue && ((ExplicitValue)obj).getLitValue() != null) {
                		nonExistingNameValue = ((ExplicitValue)obj).getLitValue();
                	}
                	else if (obj instanceof ExplicitValue && ((ExplicitValue)obj).getTerm() != null) {
                		String term = ((ExplicitValue)obj).getTerm();
                		if (term.equals("PI")) {
                			nonExistingNameValue = Double.valueOf(3.141593);
                		}
                		nonExistingNameValue = term;
                	}
                	else if (obj instanceof InstanceDeclaration) {
                		caseInstanceDeclaration((InstanceDeclaration)obj);
                		nonExistingNameValue = getModel().getLastInstanceCreated();
                	}
                	else if (obj == null) {
                		reportError(object, "Value of property '" + thisProperty + "' is null.");
                	}
                	else {
                		try {
                			reportError(object, "Value of property '" + thisProperty + "' has unexpected obj type: " + obj.toString() + ", " + obj.getClass().toString());
                 		}
                		catch (Exception e) {
                			e.printStackTrace();
                		}
                	}
                	if (nonExistingNameValue != null) {
                		if (nonExistingNameValue instanceof LiteralValue) {
                			nonExistingNameValue = literalValueToObject((LiteralValue)nonExistingNameValue);
                		}
            			annotateErrors(object, getModel().addStatement(lastSubject, thisProperty, nonExistingNameValue));
            	        getModel().movePendingValidationStatementsToQueuedValidationStatements(object);

                	}
                	else if (instanceValue != null) {
                		annotateErrors(object, getModel().addStatement(lastSubject, thisProperty, instanceValue));
                		
                	}
        		}
        		else if (thisProperty != null) {
        			// move forward in the graph to the next node
        			List<Object> thisObjValue = null;
					try {
						thisObjValue = getModel().ask(lastSubject, thisProperty);
            			if (thisObjValue == null) {
            				reportError(object, "Unable to traverse graph, '" + thisProperty.toString() + "' of '" + lastSubject.toString() + "' has no value.");
            				break;
            			}
            			else if (thisObjValue.size() > 1) {
            				reportError(object, "Unable to determine which of multiple values of '" + thisProperty.toString() + "' of '" + lastSubject.toString() + "' to use for the assignment");
            				break;
            			}
            			else if (!(thisObjValue.get(0) instanceof ConceptName)) {
            				reportError(object, "Values of '" + thisProperty.toString() + "' of '" + lastSubject.toString() + "' is not a instance and cannot be subject of the next graph edge.");
            				break;
            			}
            			lastSubject = (ConceptName) thisObjValue.get(0);
					} catch (InvalidNameException e) {
						reportError(object, "Unexpected error traversing the property '" + thisProperty.toString() + "': " + e.getLocalizedMessage());
						e.printStackTrace();
						break;
					}
        		}
        	}
//            }
        }
        return object;
    }

	@Override
	public EObject caseAllValuesFrom(AllValuesFrom object) {
		// First form: <p> of <C> only has values of type <C>
		// Second form: <C> <p> only has values of type <C>
		ResourceIdentifier restrictedClass;
		ResourceByName restrictedProperty;
		if (object.getRestricted() != null) {
			restrictedClass = object.getRestricted().getClassName();
			restrictedProperty = object.getRestricted().getPropertyName();
		}
		else {
			restrictedClass = object.getClassName();			
			restrictedProperty = object.getPropertyName();
		}
		AllValuesCondition avf = object.getCond();
		ConceptIdentifier resCls = createConceptIdentifier(restrictedClass);
		ConceptName prop = createConceptIdentifier(restrictedProperty);
		addAllValuesFromRestriction(object, resCls, prop, avf);
		return object;
	}

	@Override
	public EObject caseEnumeratedAllValuesFrom(EnumeratedAllValuesFrom object) {
		if (object.getRestricted() != null) {
			ResourceIdentifier restrictedClass = object.getRestricted().getClassName();
			ResourceByName restrictedProperty = object.getRestricted().getPropertyName();
			ConceptIdentifier resCls = createConceptIdentifier(restrictedClass);
			ConceptName resProp = createConceptIdentifier(restrictedProperty);
			EObject enm = object.getEnumeration();
			List<Object> enumValues = new ArrayList<Object>();
			if (enm instanceof ExistingResourceList) {
				EList<ResourceIdentifier> enumList = ((ExistingResourceList)enm).getNames();
				Iterator<ResourceIdentifier> itr = enumList.iterator();
				while (itr.hasNext()) {
					enumValues.add(createConceptIdentifier(itr.next()));
				}
				annotateErrors(object, getModel().addEnumeratedAllValuesFromRestriction(resCls, resProp, enumValues));
			}
			else if (enm instanceof LiteralList) {
				EList<LiteralValue> enumList = ((LiteralList)enm).getLiterals();
				Iterator<LiteralValue> itr = enumList.iterator();
				while (itr.hasNext()) {
					enumValues.add(literalValueToObject(itr.next()));
				}
				annotateErrors(object, getModel().addEnumeratedAllValuesFromRestriction(resCls, resProp, enumValues));
			}
			else {
				logger.error("This shouldn't happen! Enumeration is of type {}", enm.getClass());
			}
		}
		return object;
	}

	@Override
	public EObject caseEnumeratedAllAndSomeValuesFrom(EnumeratedAllAndSomeValuesFrom object) {
		if (object.getRestricted() != null) {
			ResourceIdentifier restrictedClass = object.getRestricted().getClassName();
			ResourceByName restrictedProperty = object.getRestricted().getPropertyName();
			ConceptIdentifier resCls = createConceptIdentifier(restrictedClass);
			ConceptName resProp = createConceptIdentifier(restrictedProperty);
			EObject enm = object.getEnumeration();
			List<Object> enumValues = new ArrayList<Object>();
			if (enm instanceof ExistingResourceList) {
				EList<ResourceIdentifier> enumList = ((ExistingResourceList)enm).getNames();
				Iterator<ResourceIdentifier> itr = enumList.iterator();
				while (itr.hasNext()) {
					enumValues.add(createConceptIdentifier(itr.next()));
				}
				annotateErrors(object, getModel().addEnumeratedAllAndSomeValuesFromRestriction(resCls, resProp, enumValues));
			}
			else if (enm instanceof LiteralList) {
				EList<LiteralValue> enumList = ((LiteralList)enm).getLiterals();
				Iterator<LiteralValue> itr = enumList.iterator();
				while (itr.hasNext()) {
					enumValues.add(literalValueToObject(itr.next()));
				}
				annotateErrors(object, getModel().addEnumeratedAllAndSomeValuesFromRestriction(resCls, resProp, enumValues));
			}
			else {
				logger.error("This shouldn't happen! Enumeration is of type {}", enm.getClass());
			}
		}
		return object;
	}

	@Override
	public EObject caseCardinality(Cardinality object) {
		// First form: <p> of <C> has exactly <n> values
		// Second form: <C> <p> has exactly <n> values
		ResourceIdentifier restrictedClass;
		ResourceByName restrictedProperty;
		if (object.getRestricted() != null) {
			restrictedClass = object.getRestricted().getClassName();
			restrictedProperty = object.getRestricted().getPropertyName();
		}
		else {
			restrictedClass = object.getClassName();			
			restrictedProperty = object.getPropertyName();
		}
		ConceptIdentifier resCls = createConceptIdentifier(restrictedClass);
		CardCondition cc = object.getCond();
		ConceptName prop = createConceptIdentifier(restrictedProperty);
		addCardinalityRestriciton(object, resCls, prop, cc);
		return object;
	}

	@Override
	public EObject caseComplementOfClass(ComplementOfClass object) {
		ConceptName cl1 = createConceptIdentifier(object.getClass1());
		ConceptIdentifier cl2 = createConceptIdentifier(object.getClass2());
		annotateErrors(object, getModel().addComplementOfClass(cl1, cl2));
		return object;
	}

	@Override
	public EObject caseDefaultValue(DefaultValue object) {
		String lvlStr = object.getLevel();
		int lvl = 0;
		if (lvlStr != null) {
			lvl = Integer.parseInt(lvlStr);
		}
		ResourceIdentifier rid = object.getDefValueClass().getClassName();
		ConceptIdentifier clsname = createConceptIdentifier(rid);
		ConceptName propname = createConceptIdentifier(object.getDefValueClass().getPropertyName());
		ExplicitValue valObj = object.getDefValue();
		Object val = resolveExplicitValue(valObj);
		annotateErrors(object, getModel().addDefaultValue(clsname, propname, val, lvl));
		return object;
	}

//	private Object explicitValueToObject(ExplicitValue valObj) {
//		if (valObj.getInstName() != null) {
//			return createConceptIdentifier(valObj.getInstName());
//		}
//		else if (valObj.getLitValue() != null) {
//			if (valObj.getLitValue().getLiteralBoolean() != null) {
//				return valObj.getLitValue().getLiteralBoolean();
//			}
//			else if (valObj.getLitValue().getLiteralNumber() != null) {
//				return valObj.getLitValue().getLiteralNumber();
//			}
//			else {
//				return valObj.getLitValue().getLiteralString();
//			}
//		}
//		else if (valObj.getTerm() != null && valObj.getTerm().equals("PI")) {
//			return Math.PI;
//		}
//		return valObj.getTerm();
//	}	
	
	@Override
	public EObject caseDisjointClasses(DisjointClasses object) {
		ConceptName cl1 = createConceptIdentifier(object.getClass1());
		ConceptIdentifier cl2 = createConceptIdentifier(object.getClass2());
		if (cl1 != null && cl2 != null) {
			annotateErrors(object, getModel().addDisjointClasses(cl1, cl2));
		}
		else {
			Iterator<ResourceIdentifier> itr = object.getClasses().getNames().iterator();
			if (itr.hasNext()) {
				List<ConceptIdentifier> conceptIds = new ArrayList<ConceptIdentifier>();
				while (itr.hasNext()) {
					conceptIds.add(createConceptIdentifier(itr.next()));
				}
				annotateErrors(object, getModel().addDisjointClasses(conceptIds));
			}
		}
		return object;
	}

	@Override
	public EObject caseEquivalentConcepts(EquivalentConcepts object) {
		ConceptIdentifier cl1 = createConceptIdentifier(object.getClass1());
		ConceptIdentifier cl2 = createConceptIdentifier(object.getClass2());
		annotateErrors(object, getModel().addEquivalentConcepts(cl1, cl2));
		return object;
	}

	@Override
	public EObject caseFunctionalProperty(FunctionalProperty object) {
		ConceptName prop = createConceptIdentifier(object.getPropertyName());
		annotateErrors(object, getModel().addFunctionalProperty(prop));
		return object;
	}

	@Override
	public EObject caseHasValue(HasValue object) {
		// First form: <p> of <C> always has value <v>
		// Second form: <C> <p> always has value <v>
		ResourceIdentifier restrictedClass;
		ResourceByName restrictedProperty;
		if (object.getRestricted() != null) {
			restrictedClass = object.getRestricted().getClassName();
			restrictedProperty = object.getRestricted().getPropertyName();
		}
		else {
			restrictedClass = object.getClassName();			
			restrictedProperty = object.getPropertyName();
		}
		HasValueCondition hvc = object.getCond();
		ConceptIdentifier resCls = createConceptIdentifier(restrictedClass);
		ConceptName prop = createConceptIdentifier(restrictedProperty);
		addHasValueRestriction(object, resCls, prop, hvc);
		return object;
	}

	@Override
	public EObject caseInstanceDifferentFrom(InstanceDifferentFrom object) {
		List<ConceptName> instances = new ArrayList<ConceptName>();
		instances.add(createConceptIdentifier(object.getInstName1()));
		instances.add(createConceptIdentifier(object.getInstName2()));
		annotateErrors(object, getModel().addDifferentInstances(instances));
		return object;
	}

	@Override
	public EObject caseInstancesAllDifferent(InstancesAllDifferent object) {
		List<ConceptName> instances = new ArrayList<ConceptName>();
		ExistingResourceList erl = object.getInstances();
		if (erl != null) {
			Iterator<ResourceIdentifier> itr = erl.getNames().iterator();
			while (itr.hasNext()) {
				ResourceIdentifier rid = itr.next();
				if (rid instanceof ResourceByName) {
					instances.add(createConceptIdentifier((ResourceByName)rid));
				}
				else {
					reportError(object, "Instances all different did not have a valid resource identifier: " + rid.toString());
				}
			}
			annotateErrors(object, getModel().addDifferentInstances(instances));
		}
		return object;
	}

	@Override
	public EObject caseInverseFunctionalProperty(
			InverseFunctionalProperty object) {
		ConceptName prop = createConceptIdentifier(object.getPropertyName());
		annotateErrors(object, getModel().addInverseFunctionalProperty(prop));
		return object;
	}

	@Override
	public EObject caseInverseProperty(InverseProperty object) {
		ConceptName prop1 = createConceptIdentifier(object.getPropertyName1());
		ConceptName prop2 = createConceptIdentifier(object.getInvOf().getPropertyName2());
		annotateErrors(object, getModel().addInverseProperties(prop1, prop2));
		return object;
	}

	@Override
	public EObject caseMaxCardinality(MaxCardinality object) {
		// First form: <p> of <C> has at most <n> values
		// Second form: <C> <p> has at most <n> values
		ResourceIdentifier restrictedClass;
		ResourceByName restrictedProperty;
		if (object.getRestricted() != null) {
			restrictedClass = object.getRestricted().getClassName();
			restrictedProperty = object.getRestricted().getPropertyName();
		}
		else {
			restrictedClass = object.getClassName();			
			restrictedProperty = object.getPropertyName();
		}
		ConceptIdentifier resCls = createConceptIdentifier(restrictedClass);
		MaxCardCondition mcc = object.getCond();
		ConceptName prop = createConceptIdentifier(restrictedProperty);
		addMaxCardinalityRestriction(object, resCls, prop, mcc);
		return object;
	}

	@Override
	public EObject caseMinCardinality(MinCardinality object) {
		// First form: <p> of <C> has at least <n> values
		// Second form: <C> <p> has at least <n> values
		ResourceIdentifier restrictedClass;
		ResourceByName restrictedProperty;
		if (object.getRestricted() != null) {
			restrictedClass = object.getRestricted().getClassName();
			restrictedProperty = object.getRestricted().getPropertyName();
		}
		else {
			restrictedClass = object.getClassName();			
			restrictedProperty = object.getPropertyName();
		}
		ConceptIdentifier resCls = createConceptIdentifier(restrictedClass);
		MinCardCondition mcc = object.getCond();
		ConceptName prop = createConceptIdentifier(restrictedProperty);
		addMinCardinalityRestriction(object, resCls, prop, mcc);
		return object;
	}

	@Override
	public EObject caseNecessaryAndSufficient(NecessaryAndSufficient object) {
		ConceptName subClass = createConceptIdentifier(object.getSubClass());
		ConceptIdentifier superClass = createConceptIdentifier(object.getSuperClass().getClassIdentifier());
		EList<ResourceByName> props = object.getPropertyName();
		EList<Condition> conds = object.getCond();
		List<ConceptName> propList = new ArrayList<ConceptName>();
		List<ClassRestrictionCondition> condList = new ArrayList<ClassRestrictionCondition>();
		for (int i = 0; i < Math.max(props.size(), conds.size()); i++) {
			ConceptName prop = createConceptIdentifier((i < props.size() ? props.get(i) : null));
			propList.add(prop);
			Condition cond = i < conds.size() ? conds.get(i) : null;
			ClassRestrictionCondition rCond = createClassRestrictionCondition(object, cond);
			condList.add(rCond);
		}
		annotateErrors(object, getModel().addNecessaryAndSufficientCondition(subClass, superClass, propList, condList));
		return object;
	}
	
	private ClassRestrictionCondition createClassRestrictionCondition(EObject object, Condition cond) {
		ClassRestrictionCondition rCond = null;
		if (cond instanceof AllValuesCondition) {
			rCond = new ClassRestrictionCondition(RestrictionType.ALLVALUES, createConceptIdentifier(((AllValuesCondition)cond).getRestriction()));
		}
		else if (cond instanceof SomeValuesCondition) {
			EObject restrict = ((SomeValuesCondition)cond).getRestriction();
			if (restrict instanceof ResourceIdentifier) {
				rCond = new ClassRestrictionCondition(RestrictionType.SOMEVALUES, createConceptIdentifier((ResourceIdentifier) ((SomeValuesCondition)cond).getRestriction()));
			}
			else {
				//this should generate an error I think--necessary and sufficient on "each of..."?
				reportError(object, "Necessary and sufficient condition has invalid restriction: " + restrict.toString());
			}
		}
		else if (cond instanceof HasValueCondition) {	
			ExplicitValue value = ((HasValueCondition)cond).getRestriction();
			Object objVal = resolveExplicitValue(value);
			rCond = new ClassRestrictionCondition(RestrictionType.HASVALUE, objVal);
		}
		else {
			RestrictionType rType = null;
			int card = 0;
			if (cond instanceof CardCondition) {
				rType = RestrictionType.CARDINALITY;
				card = Integer.parseInt(((CardCondition)cond).getCard());
			}
			else if (cond instanceof MaxCardCondition) {
				rType = RestrictionType.MAXCARDINALIY;
				card = Integer.parseInt(((MaxCardCondition)cond).getCard());
			}
			else if (cond instanceof MinCardCondition) {
				rType = RestrictionType.MINCARDINALITY;
				card = Integer.parseInt(((MinCardCondition)cond).getCard());
			}
			else {
				reportError(object, "Necessary and sufficient condition has invalid restriction: " + cond.toString());
			}
			if (rType != null) {
				rCond = new ClassRestrictionCondition(rType, card);
			}
		}
		return rCond;
	}

	@Override
	public EObject caseSomeValuesFrom(SomeValuesFrom object) {
		// First form: <p> of <C> has at least one value of type <C>
		// 			   <p> of <C> has at least one value each of type {<C>, <C>, ...}
		// Second form: <C> <p> has at least one value of type <C>
		//			   <p> of <C> has at least one value each of type {<C>, <C>, ...}
		ResourceIdentifier restrictedClass;
		ResourceByName restrictedProperty;
		if (object.getRestricted() != null) {
			restrictedClass = object.getRestricted().getClassName();
			restrictedProperty = object.getRestricted().getPropertyName();
		}
		else {
			restrictedClass = object.getClassName();			
			restrictedProperty = object.getPropertyName();
		}
		SomeValuesCondition svc = object.getCond();
		ConceptIdentifier resCls = createConceptIdentifier(restrictedClass);
		ConceptName prop = createConceptIdentifier(restrictedProperty);
		addSomeValuesFromRestriction(object, resCls, prop, svc);
		
		return object;
	}

	private void addSomeValuesFromRestriction(EObject object,
			ConceptIdentifier resCls, ConceptName restrictedProperty, SomeValuesCondition svc) {
//		ConceptName prop = createConceptIdentifier(restrictedProperty);
		EObject restrict = svc.getRestriction();
		if (restrict instanceof ResourceIdentifier) {
			ClassRestrictionCondition rCond = new ClassRestrictionCondition(RestrictionType.SOMEVALUES, createConceptIdentifier((ResourceIdentifier)restrict));
			annotateErrors(object, getModel().addClassRestriction(resCls, restrictedProperty, rCond));			
		}
		else if (restrict instanceof ExistingResourceList) {
			Iterator<ResourceIdentifier> names = ((ExistingResourceList)restrict).getNames().iterator();
			while (names.hasNext()) {
				ClassRestrictionCondition rCond = new ClassRestrictionCondition(RestrictionType.SOMEVALUES, createConceptIdentifier(names.next()));
				annotateErrors(object, getModel().addClassRestriction(resCls, restrictedProperty, rCond));	
			}
		}
		else {
			logger.error("Restriction not of expected type ({})", restrict.getClass());
			reportError(object, "Some value from restriction not of expected type: " + restrict.toString());
		}
	}

	private void addAllValuesFromRestriction(EObject object, ConceptIdentifier resCls,
			ConceptName restrictedProperty, AllValuesCondition avf) {
		ResourceIdentifier valueClass = avf.getRestriction();
		ConceptIdentifier restrictedTo = createConceptIdentifier(valueClass);
		ClassRestrictionCondition crc = new ClassRestrictionCondition(RestrictionType.ALLVALUES, restrictedTo);
		annotateErrors(object, getModel().addClassRestriction(resCls, restrictedProperty, crc));
	}

	private void addHasValueRestriction(EObject object,
			ConceptIdentifier resCls, ConceptName restrictedProperty,
			HasValueCondition hvc) {
		ExplicitValue value = hvc.getRestriction();
		Object restrictedTo = resolveExplicitValue(value);
		ClassRestrictionCondition rCond = new ClassRestrictionCondition(RestrictionType.HASVALUE, restrictedTo);
		annotateErrors(object, getModel().addClassRestriction(resCls, restrictedProperty, rCond));
	}

	private void addCardinalityRestriciton(EObject object,
			ConceptIdentifier resCls, ConceptName restrictedProperty,
			CardCondition cc) {
		String cardValue = cc.getCard();
		int card = Integer.parseInt(cardValue);
		ResourceIdentifier cqr = cc.getClassQualifier();
		ConceptIdentifier clsQualifier = null;
		if (cqr != null) {
			clsQualifier = createConceptIdentifier(cqr);
		}
		annotateErrors(object, getModel().addClassRestriction(resCls, restrictedProperty, 
				new ClassRestrictionCondition(RestrictionType.CARDINALITY, card, clsQualifier)));
	}

	private void addMaxCardinalityRestriction(EObject object,
			ConceptIdentifier resCls, ConceptName restrictedProperty,
			MaxCardCondition mcc) {
		String cardValue = mcc.getCard();
		int card = Integer.parseInt(cardValue);
		ResourceIdentifier cqr = mcc.getClassQualifier();
		ConceptIdentifier clsQualifier = null;
		if (cqr != null) {
			clsQualifier = createConceptIdentifier(cqr);
		}
		ClassRestrictionCondition rCond = new ClassRestrictionCondition(RestrictionType.MAXCARDINALIY, card, clsQualifier);
		annotateErrors(object, getModel().addClassRestriction(resCls, restrictedProperty, rCond));
	}

	private void addMinCardinalityRestriction(EObject object,
			ConceptIdentifier resCls, ConceptName restrictedProperty,
			MinCardCondition mcc) {
		String cardValue = mcc.getCard();
		int card = Integer.parseInt(cardValue);
		ResourceIdentifier cqr = mcc.getClassQualifier();
		ConceptIdentifier clsQualifier = null;
		if (cqr != null) {
			clsQualifier = createConceptIdentifier(cqr);
		}
		ClassRestrictionCondition rCond = new ClassRestrictionCondition(RestrictionType.MINCARDINALITY, card, clsQualifier);
		annotateErrors(object, getModel().addClassRestriction(resCls, restrictedProperty, rCond));
	}

	@Override
	public EObject caseTransitiveProperty(TransitiveProperty object) {
		ConceptName prop = createConceptIdentifier(object.getPropertyName());
		annotateErrors(object, getModel().addTransitiveProperty(prop));
		return object;
	}

    /**
     * Returns true if there are errors
     */
    public int errors(boolean bIncludePending) {
        int cnt = getModel().errors(bIncludePending);
//        if (cnt > 0 && logger.isDebugEnabled()) {
//        	dumpMMContext();
//        }
        return cnt;
    }
    
    public int warnings() {
    	int cnt = getModel().warnings();
    	return cnt;
    }

    /**
     * Logs errors on the console.  Needs to be revamped later to insert error markers.
     */
    private void reportErrors(List<ModelError> errors) {
        if (errors != null && errors.size() > 0) {
        	int newErrorCount = errors.size();
        	int totalErrors = augmentErrorCount(newErrorCount);
        	if (markErrors) {
        		String fileStr = null;
        		if (getModel().getModelActualUrl().isFile()) {
    	        	fileStr = getModel().getModelActualUrl().toFileString();
        		} else {
    	        	fileStr = getModel().getModelActualUrl().toString();
        		}
	        	// create error or warning marker, based on error.getErrorType().
	    		IPath location = Path.fromOSString(fileStr);
	    		IWorkspaceRoot myWorkspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
	    		IFile file = myWorkspaceRoot.getFileForLocation(location);
	    		if (file != null) {
	    			int warningMarkers = 0;
	    			int errorMarkers = 0;
		            for (ModelError error : errors) {
		            	if (error.errorType.equals(ErrorType.ERROR)) {
		            		errorMarkers++;
		            		if (errorMarkers <= maxErrors) {
				           		createMarker(file, error);		            			
		            		}
		            		else if (errorMarkers == maxErrors){
			        			getModel().getMessageManager().error("Maximum number of error markers reached in model '" + file.getName() + "'. Discontinuing error marker creation afer displaying " + errorMarkers + " errors of " + totalErrors + " errors/warnings.");		            			
		            		}
		            	}
		            	else {
		            		warningMarkers++;
			        		if (warningMarkers <= maxWarnings) {
				           		createMarker(file, error);
			        		}
			        		else if (warningMarkers == maxWarnings){
			        			getModel().getMessageManager().error("Maximum number of warning markers reached in model '" + file.getName() + "'. Discontinuing warning marker creation afer displaying " + warningMarkers + " warnings of " + totalErrors + " errors/warnings.");			        			
			        		}
		            	}
		            	if (errorMarkers > maxErrors && warningMarkers > maxWarnings) {
		        			markErrors = false;
		        			break;		            		
		            	}
		            }
        		}
        	}
        }
    }

    /**
     * Converts a ResourceIdentifier to a ConceptIdentifier.
     *
     * @param classIdentifier A single class name, a list of class names, or null
     *  (null can happen when declaring a top-level class).
     * @return The same name or list of class names ready to pass to ModelManager.
     */
    private ConceptIdentifier createConceptIdentifier(ResourceIdentifier classIdentifier) {
        ConceptIdentifier conceptIdentifier = null;
        if (classIdentifier instanceof ResourceByName) {
            conceptIdentifier = createConceptIdentifier((ResourceByName)classIdentifier);
        }
        else if (classIdentifier instanceof UnionResource) {
            UnionResource unionClass = (UnionResource) classIdentifier;
            List<ConceptIdentifier> classNames = new ArrayList<ConceptIdentifier>(unionClass.getNames().size());
            for (ResourceIdentifier classByName : unionClass.getNames()) {
                // Sometimes names are used before they are defined.
            	ConceptIdentifier cident = createConceptIdentifier(classByName);
            	classNames.add(cident);
//                if (classByName.getName().getName() != null) {
//                    classNames.add(new ConceptName(classByName.getName().getName()));
//                }
            }
            conceptIdentifier = new SadlUnionClass(classNames);
        }
        else if (classIdentifier instanceof IntersectionResource) {
            IntersectionResource intersectionClass = (IntersectionResource) classIdentifier;
            List<ConceptIdentifier> classNames = new ArrayList<ConceptIdentifier>(intersectionClass.getNames().size());
            for (ResourceIdentifier classByName : intersectionClass.getNames()) {
                // Sometimes names are used before they are defined.
            	ConceptIdentifier cident = createConceptIdentifier(classByName);
            	classNames.add(cident);            	
//                if (classByName.getName().getName() != null) {
//                    classNames.add(new ConceptName(classByName.getName().getName()));
//                }
            }
            conceptIdentifier = new SadlIntersectionClass(classNames);
        }
		else if (classIdentifier instanceof ResourceByRestriction) {
			ResourceByName propName = ((ResourceByRestriction)classIdentifier).getPropName();
			ConceptName pcn = new ConceptName(propName.getName().getName());
			Condition cond = ((ResourceByRestriction)classIdentifier).getCond();
			ClassRestrictionCondition crc = createClassRestrictionCondition(classIdentifier , cond);
			conceptIdentifier = new SadlResourceByRestriction(pcn, crc);
		}
        return conceptIdentifier;
    }

    /**
     * Converts a ResourceByName to an ConceptName
     *
     * @param propName -- a property name
     *
     * @return -- a corresponding ConceptName
     */
    private ConceptName createConceptIdentifier(ResourceByName propName) {
    	ConceptName en = null;
        if (propName != null) {
            String name = null;
            ResourceName rn = propName.getName();
            if (rn != null && rn.getName() != null) {
                name = rn.getName();
            }
            else {
            	// fallback if name can't be resolved
            	name = "<unresolved_name>";
            }
            en = new ConceptName(name);
        }
        return en;
    }
    
    /**
     * Convert a ResourceName to an ConceptName
     * 
     * @param name -- the name
     * 
     * @return -- a corresponding ConceptName
     */
    private ConceptName createConceptIdentifier(ResourceName name) {
    	ConceptName en = null;
    	if (name != null && name.getName() != null) {
    		en = new ConceptName(name.getName());
    	}
    	return en;
    }

    /**
     * Adds one or more triples about a given subject to the model.
     *
     * @param instName Name of subject.
     * @param addlInfoItems List of predicates and objects for rest of triples.
     * @throws InvalidNameException 
     */
    private void addStatements(EObject object, ConceptName instName, List<PropValPartialTriple> addlInfoItems) throws InvalidNameException {
        if (addlInfoItems != null) {
        	List<Object[]>  addlItems = getAttributeNameValuePairs(addlInfoItems);
        	for (int i = 0; addlItems != null && i < addlItems.size(); i++) {
        		Object[] item = addlItems.get(i);
        		if (item[0] instanceof ConceptName) {
        			if (item[1] instanceof ConceptName) {
        				annotateErrors((EObject) item[2], getModel().addStatement(instName, (ConceptName)item[0], (ConceptName)item[1]));        				
        		        getModel().movePendingValidationStatementsToQueuedValidationStatements((EObject)item[2]);
        			}
        			else {
        				annotateErrors((EObject) item[2], getModel().addStatement(instName, (ConceptName)item[0], item[1]));
        		        getModel().movePendingValidationStatementsToQueuedValidationStatements((EObject)item[2]);
        			}
        		}
        	}
        }
    }

    /**
     * Convert a LiteralValue to a Java Object of the correct type.
     * @param value
     * @return
     */
    protected static Object literalValueToObject(LiteralValue value) {
    	Object propValue = null;
    	if (value.getLiteralBoolean() != null) {
            propValue = Boolean.valueOf(value.getLiteralBoolean());
        }
        else if (value.getLiteralNumber() != null) {
            Double dv = Double.parseDouble(value.getLiteralNumber());
            if (value.getLiteralNumber().toString().contains(".")) {
            	propValue = dv;
            }
            else if (dv.longValue() == dv.doubleValue()) {
                if (dv.longValue() > Integer.MAX_VALUE || dv.longValue() < Integer.MIN_VALUE) {
                    propValue = Long.valueOf(dv.longValue());
                }
                else {
                    propValue = Integer.valueOf(dv.intValue());
                }
            }
            else {
                propValue = dv;
            }
        }
        else if (value.getLiteralString() != null) {
            propValue = value.getLiteralString();
        }
        else {
            // this shouldn't happen
            propValue = value.toString(); // Integer.valueOf(value.getLiteralNumber().intValue());
        }
        return propValue;
    }

    /**
     * Call this method to convert an ExplicitValue into a Java Object of the appropriate type
     * @param value
     * @return
     */
	private Object resolveExplicitValue(ExplicitValue value) {
		if (value.getInstName() != null) {
			return createConceptIdentifier(value.getInstName());
		}
		else if (value.getLitValue() != null) {
			return literalValueToObject(value.getLitValue());
		}
		else {
			return value.getTerm();
		}
	}
	
	/**
     * Converts a PropValPartialTriple list into a list of Object[2], each element containing the property name and the value
     *
     * @param addlInfoItems
     * @return
     * @throws InvalidNameException
     */
    private List<Object[]> getAttributeNameValuePairs(List<PropValPartialTriple> addlInfoItems) throws InvalidNameException {
        if (addlInfoItems != null) {
            List<Object[]> nvpairs = new ArrayList<Object[]>();
            for (PropValPartialTriple item : addlInfoItems) {
                if (item.getPropertyName() != null && 
                        item.getPropertyName().getName() != null &&
                        item.getPropertyName().getName().getName() != null) {
                    ConceptName propName = new ConceptName(item.getPropertyName().getName().getName());
                    Object[] nvp = new Object[3];
                    nvp[0] = propName;
                    ExplicitValue objValue = item.getObjectValue();
                    if (objValue != null) {
                        nvp[1] = resolveExplicitValue(objValue);
                        nvpairs.add(nvp);
                        nvp[2] = objValue;
                    }
                    else {
                    	InstanceDeclaration instDecl = item.getObjectValueBNode();
                    	if (instDecl != null) {
                    		caseInstanceDeclaration(instDecl);
                    		ResourceName objName = getInstanceName(instDecl);
                    		nvp[1] = objName != null ? new ConceptName(objName.getName()) : null;
                    		nvp[2] = (objName != null) ? objName : instDecl;
                    		if (nvp[1] == null) {
                    			// TODO this will probably not handle deeply nested bnodes without names....
                    			nvp[1] = getModel().getLastInstanceCreated();
                    		}
                    		nvpairs.add(nvp);
                    	}
                    	else {
                    		reportError(item, "Unexpected condition: instance attribution item has neither an object value or a bnode object value.");
                    	}
                    }
                }
            }
            return (nvpairs.size() > 0 ? nvpairs : null);
        }
        return null;
    }

    /**
     * This method will return a relative URI for a corresponding SADL model if one exists in the project
     * @param u
     * @return
     */
	public URI equivalentSadlModelInProject(URI u) {
		return getModel().equivalentSadlModelInProject(u);
	}

    /**
     * This method is used to report errors encountered in SadlModelManager
     * 
     * @param object -- the EObject with which the error is associated
     * @param errorMsg
     */
	public void reportError(EObject object, String errorMsg) {
		setTranslationErrors(getNumTranslationErrors() + 1);
		ICompositeNode node = NodeModelUtils.findActualNodeFor(object);
		if (node != null) {
			int lnnum = node.getStartLine();
			int lnlen = node.getLength();
			int offset = node.getOffset();
				if (lnnum >= 0 && lnlen > 0 && offset >= 0) {
		        	String fileStr = getModel().getModelActualUrl().toFileString();
		        	// create error or warning marker, based on error.getErrorType().
		    		IPath location = Path.fromOSString(fileStr);
		    		IWorkspaceRoot myWorkspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		    		IFile file = myWorkspaceRoot.getFileForLocation(location);
		    		if (file != null) {
		    			ModelError error = new ModelError(errorMsg, ModelError.ErrorType.ERROR);
		    			error.setLineLength(lnlen);
		    			error.setLineNumber(lnnum);
		    			error.setOffset(offset);
			            createMarker(file, error);
		            }
		    		else {
		    			getModel().getMessageManager().error(errorMsg + "\n",
		    					getModel().getMessageManager().new HyperlinkInfo(getModel().getModelActualUrl().toFileString(), lnnum, offset, lnlen));
		    		}
				}
				else {
					getModel().getMessageManager().error(errorMsg + "\n");
				}
			}
			else {
				getModel().getMessageManager().error(errorMsg + "\n");
			}
		if (logger.isDebugEnabled()) {
			logger.error(errorMsg + "(in model " + getModel().getModelName() + ")");
		}
	}
	
	/**
	 * This method is used to report warnings encountered in SadlModelManager
	 * 
	 * @param object -- the Eobject with which the warning is associated
	 * @param warningMsg
	 */
	@SuppressWarnings("unused")
    private void reportWarning(EObject object, String warningMsg) {
		logger.warn(warningMsg + "(in model " + getModel().getModelName() + ")");
	}

	@Override
	public EObject caseSymmetricalProperty(SymmetricalProperty object) {
		ConceptName prop = createConceptIdentifier(object.getPropertyName());
		annotateErrors(object, getModel().addSymmetricalProperty(prop));
		return object;
	}

	/**
	 * Converts a test's parse tree into an intermediate form.
	 */
	@Override
	public EObject caseTest(com.ge.research.sadl.sadl.Test object) {
		translateTest(object);
		List<IFTranslationError> transErrors = getIfTranslator().getErrors();
		for (int i = 0; transErrors != null && i < transErrors.size(); i++) {
			IFTranslationError err = transErrors.get(i);
			reportError((EObject) err.getErrorLocation(), err.getLocalizedMessage());
		}
		return object;
	}

	/**
	 * Converts a query's parse tree into an intermediate form.
	 */
	@Override
	public EObject caseQuery(com.ge.research.sadl.sadl.Query object) {
		Query query = translateQuery(object);
		ICompositeNode node = NodeModelUtils.findActualNodeFor(object);
		if (node != null)  {
			query.setLineNo(node.getStartLine());
			query.setLength(node.getLength());
			query.setOffset(node.getOffset());
//				query.setOffset(((NodeAdapter)adapter).getParserNode().getTotalOffset());
		}
		
		// now add the query to the model
		annotateErrors(object, getModel().addQuery(query));
		return object.getExpr();
	}

	/**
	 * This method converts a parse tree Query to an IntermediateForm Query.
	 * It does not add it to the model.
	 * 
	 * @param expr
	 * @return
	 */
	public Query translateQuery(com.ge.research.sadl.sadl.Query object) {
		Expression expr = object.getExpr();
		Query query = translateQuery(expr, null);
		validateQuery(object, query);
		return query;
	}
	
	private Query translateQuery(Expression expr, Test parent) {
		Query query = new Query();

		setTranslationTarget(query);
		if (parent != null) {
			getIfTranslator().setEncapsulatingTarget(parent);
		}
		
		// get variables and other information from the SelectExpression
		VariableList varList = null;
		if (expr instanceof SelectExpression) {
			query.setKeyword("select");
			if (((SelectExpression)expr).getDistinct() != null) {
				query.setDistinct(true);
			}
			varList = ((SelectExpression)expr).getVarList();
			if (varList != null) {
				EList<ResourceName> rnames = varList.getNames();
				List<String> names = new ArrayList<String>();
				for (int i = 0; i < rnames.size(); i++) {
					names.add(rnames.get(i).getName());
				}
				query.setVariables(names);
			}
		}
		else if (expr instanceof ConstructExpression) {
			query.setKeyword("construct");
			List<String> names = new ArrayList<String>();
			names.add(((ConstructExpression)expr).getSubj().getName());
			names.add(((ConstructExpression)expr).getPred().getName());
			names.add(((ConstructExpression)expr).getObj().getName());
			query.setVariables(names);
		}
		else if (expr instanceof AskQueryExpression) {
			query.setKeyword("ask");
		}

		// Translate the query to the resulting intermediate form.
		Object pattern = translate(expr);
		
		Object expandedPattern = null;
		try {
			expandedPattern = getIfTranslator().expandProxyNodes(pattern, false, true);
		} catch (InvalidNameException e) {
			reportError(expr, "Invalid name in query: " + pattern.toString());
			e.printStackTrace();
		} catch (InvalidTypeException e) {
			reportError(expr, "Invalid type in query: " + pattern.toString());
			e.printStackTrace();
		} catch (TranslationException e) {
			reportError(expr, "Translation error in query: " + pattern.toString());
			e.printStackTrace();
		}
		if (expandedPattern != null && expandedPattern instanceof List<?> && ((List<?>)expandedPattern).size() > 0) {
			pattern = expandedPattern;
		}
		
		if (pattern instanceof List<?>) {
			if (query.getVariables() == null) {
				Set<VariableNode> nodes = getIfTranslator().getSelectVariables((List<GraphPatternElement>)pattern);
				if (nodes != null && nodes.size() > 0) {
					List<String> names = new ArrayList<String>(1);
					for (VariableNode node : nodes) {
						names.add(node.getName());
					}
					query.setVariables(names);
					if (query.getKeyword() == null) {
						query.setKeyword("select");
					}
				}
				else {
					// no variables, assume an ask
					if (query.getKeyword() == null) {
						query.setKeyword("ask");
					}
				}
			}
			query.setPatterns((List<GraphPatternElement>) pattern);
		}
		else if (pattern instanceof Literal) {
			// this must be a SPARQL query
			query.setSparqlQueryString(((Literal)pattern).getValue().toString());
		}
		logger.info("Ask translation: {}", query);
		return query;
	}

	@Override
	public EObject caseDisplay(Display object) {
		String displayString = object.getDisplayString();
		String model = object.getModel();
		if (displayString!= null || model != null) {
			Print prnt = new Print(displayString);
			final ICompositeNode node = NodeModelUtils.findActualNodeFor(object);
			if (node != null)  {
				prnt.setLineNo(node.getStartLine());
				prnt.setLength(node.getLength());
				prnt.setOffset(node.getOffset());
//					prnt.setOffset(((NodeAdapter)adapter).getParserNode().getTotalOffset());
			}
			if (model != null) {
				prnt.setModel(model);
			}
			annotateErrors(object, getModel().addPrint(prnt));
		}
		return object;
	}

	@Override
	public EObject caseExplanation(Explanation object) {
		Explain explain = null;
		EObject expr = object.getExpr();
		if (expr != null) {
			if (expr instanceof InstanceDeclaration) {
		        ResourceName instName = getInstanceName(((InstanceDeclaration)expr));
		        ConceptIdentifier clsName;
		        if (((InstanceDeclaration)expr).getTypeDecl() != null) {
		            clsName = createConceptIdentifier(((InstanceDeclaration)expr).getTypeDecl().getType().getClassIdentifier());
		        }
		        else if (((InstanceDeclaration)expr).getClassName() != null) {
		            clsName = createConceptIdentifier(((InstanceDeclaration)expr).getClassName());
		        }
		        else {
		            logger.error("Invalid InstanceDeclaration in Explanation: please fix");
					reportError(expr, "Invalid instance declaration in Explanation: " + expr.toString());
		            return expr;
		        }
		        ConceptName name = instName != null ? new ConceptName(instName.getName()) : null;
		        if (name != null) {
					try {
						if (((InstanceDeclaration)expr).getAddlInfoItems() != null) {
							explain = createExplainFromStatements(name, ((InstanceDeclaration)expr).getAddlInfoItems());
						}
						if (clsName instanceof ConceptName) {
							TripleElement typeTriple = new TripleElement();
							Node instNode = new NamedNode(name.toString());
							((NamedNode)instNode).setNamespace(name.getNamespace());
							instNode = getIfTranslator().validateNode(instNode);
							typeTriple.setSubject(instNode);
							Node predNode = new RDFTypeNode();
							predNode = getIfTranslator().validateNode(predNode);
							typeTriple.setPredicate(predNode);
							Node clsNode = new NamedNode(((ConceptName)clsName).toString());
							((NamedNode)clsNode).setNamespace(((ConceptName)clsName).getNamespace());
							clsNode = getIfTranslator().validateNode(clsNode);
							typeTriple.setObject(clsNode);
							if (explain != null) {
								List<GraphPatternElement>patterns = explain.getPatterns();
								patterns.add(0, typeTriple);
								explain.setPatterns(patterns);
							}
							else {
								explain = new Explain(typeTriple);
							}
						}
					} catch (InvalidNameException e) {
						e.printStackTrace();
						logger.equals("Explanation failed: " + e.getLocalizedMessage());
					}
		        }
				
			}
			else if (expr instanceof ExistingInstanceAttribution) {
		        if (((ExistingInstanceAttribution)expr).getSubj() != null && 
		        		((ExistingInstanceAttribution)expr).getSubj().getName() != null && 
		        		((ExistingInstanceAttribution)expr).getSubj().getName().getName() != null) {
		            ConceptName subject = new ConceptName(((ExistingInstanceAttribution)expr).getSubj().getName().getName());
		            if (((ExistingInstanceAttribution)expr).getAddlInfoItems() != null && ((ExistingInstanceAttribution)expr).getAddlInfoItems().size() > 0) {
		            	List<PropValPartialTriple> addlInfoItems = ((ExistingInstanceAttribution)expr).getAddlInfoItems();
		            	if (addlInfoItems != null) {
		            		try {
								explain = createExplainFromStatements(subject, addlInfoItems);
							} catch (InvalidNameException e) {
								e.printStackTrace();
								logger.equals("Explanation failed: " + e.getLocalizedMessage());
							}
		            	}
		            }
		        }
			}
			else if (expr instanceof Expression) {
				setTranslationTarget(null);
				Object pattern = translate((Expression)expr);
				logger.info("Explanation translation: {}", pattern);
				List<GraphPatternElement> patterns = new ArrayList<GraphPatternElement>();
				while (pattern instanceof GraphPatternElement) {
					GraphPatternElement element = (GraphPatternElement) pattern;
					patterns.add(element);
					pattern = element.getNext();
					element.setNext(null);
				}
				getIfTranslator().flattenLinkedList(patterns);
				if (patterns.size() > 0) {
					explain = new Explain(patterns);
//					getModel().explain(patterns);
					
				}
				else {
					reportError(expr, "Explain statement is not a GraphPatternElement; unsupported statement.");
				}
			}
			else {
				logger.info("Explanation type (" + expr.getClass().getCanonicalName() + ") not supported.");
			}
		}
		else {
			String rulename = object.getRulename();
			if (rulename != null) {
				explain = new Explain(rulename);
			}
		}
		final ICompositeNode node = NodeModelUtils.findActualNodeFor(object);
		if (explain != null && node != null)  {
			explain.setLineNo(node.getStartLine());
			explain.setLength(node.getLength());
			explain.setOffset(node.getOffset());
		}
		if (explain != null) {
			annotateErrors(object, getModel().addExplain(explain));
		}
		return object;
	}
	
	private Explain createExplainFromStatements(ConceptName subject, List<PropValPartialTriple> addlInfoItems) throws InvalidNameException {
		List<GraphPatternElement> patterns = new ArrayList<GraphPatternElement>();
    	List<Object[]>  addlItems = getAttributeNameValuePairs(addlInfoItems);
    	Node subjNode = new NamedNode(subject.toString());
    	((NamedNode)subjNode).setNamespace(subject.getNamespace());
    	subjNode = getIfTranslator().validateNode(subjNode);
    	for (int i = 0; addlItems != null && i < addlItems.size(); i++) {
    		Object[] item = addlItems.get(i);
    		if (item[0] instanceof ConceptName) {
    			Node predNode = new NamedNode(((ConceptName)item[0]).toString());
    			((NamedNode)predNode).setNamespace(((ConceptName)item[0]).getNamespace());
    			predNode = getIfTranslator().validateNode(predNode);
				TripleElement triple = new TripleElement();
				triple.setSubject(subjNode);
				triple.setPredicate(predNode);
    			if (item[1] instanceof ConceptName) {
    				Node objNode = new NamedNode(((ConceptName)item[1]).toString());
    				((NamedNode)objNode).setNamespace(((ConceptName)item[1]).getNamespace());
    				objNode = getIfTranslator().validateNode(objNode);
    				triple.setObject(objNode);
    			}
    			else {
    				Literal objNode = new com.ge.research.sadl.model.gp.Literal();
    				objNode.setValue(item[1]);
    				triple.setObject(objNode);
    			}
				patterns.add(triple);
    		}
    	}
    	if (patterns.size() >  0) {
    		return new Explain(patterns);
    	}
    	return null;
	}

	@Override
	public EObject caseExpr(Expr object) {
		Expression expr = object.getExpr();
		setTranslationTarget(null);
		Object pattern = translate(expr); 
		if (pattern != null) {
			logger.info("Expr translation: {}", pattern);
		}
		else {
			logger.error("Expr translation failed: " + expr.toString());
		}

		return expr;
	}
	
	public Object translateExpression(Expression object) throws InvalidNameException, InvalidTypeException, TranslationException {
		getIfTranslator().resetIFTranslator();
		Object pattern = translate(object);
		return getIfTranslator().expandProxyNodes(pattern, false, true);
	}

	@Override
	public EObject caseRule(Rule object) {
		com.ge.research.sadl.model.gp.Rule rule = translateRule(object);
		try {
			validateRule(object, rule);
		} catch (MalformedURLException | ConfigurationException e) {
			e.printStackTrace();
			reportError(object, "Unexpected exception: " + e.getMessage());
		}
		List<IFTranslationError> transErrors = getIfTranslator().getErrors();
		for (int i = 0; transErrors != null && i < transErrors.size(); i++) {
			IFTranslationError err = transErrors.get(i);
			if (err.getErrorLocation() instanceof EObject) {
				reportError((EObject) err.getErrorLocation(), err.getLocalizedMessage());
			}
			else {
				reportError((EObject)object, err.getLocalizedMessage());
			}
		}
		logger.info("Rule postprocessed to: " + rule.toString());
		annotateErrors(object, getModel().addRule(rule));
		return object;
	}
	
    /**
     * This method checks a GraphPatternElement for errors and warnings and generates the same if found.
     * 
     * @param gpe
     * @return
     */
    private int validateGraphPatternElement(EObject object, GraphPatternElement gpe) {
    	int numErrors = 0;
		if (gpe instanceof TripleElement) {
			if (((TripleElement) gpe).getSubject() instanceof NamedNode &&
					((NamedNode)((TripleElement)gpe).getSubject()).getNodeType().equals(NodeType.PropertyNode)) {
				reportError(object, "Did not expect a property (" + 
						((NamedNode)((TripleElement)gpe).getSubject()).getName() + 
						") as triple pattern subject.");
				numErrors++;
			}
			if (((TripleElement) gpe).getObject() instanceof NamedNode &&
					((NamedNode)((TripleElement)gpe).getObject()).getNodeType().equals(NodeType.PropertyNode)) {
				if (!(((TripleElement)gpe).getPredicate() instanceof NamedNode) || 
						!((NamedNode)((TripleElement)gpe).getPredicate()).getNamespace().equals(OWL.NAMESPACE.getNameSpace())) {
					reportError(object, "Did not expect a property (" + 
							((NamedNode)((TripleElement)gpe).getObject()).getName() + 
							") as triple pattern object.");
					numErrors++;
				}
			}
			if (((TripleElement) gpe).getPredicate() instanceof NamedNode &&
					!((NamedNode)((TripleElement)gpe).getPredicate()).getNodeType().equals(NodeType.PropertyNode)) {
				reportError(object, "Expected a property as triple pattern predicate rather than " + 
						((NamedNode)((TripleElement)gpe).getPredicate()).getNodeType().toString() + " " + 
						((NamedNode)((TripleElement)gpe).getPredicate()).getName());
				numErrors++;
			}
		}
		else if (gpe instanceof BuiltinElement) {
			if (((BuiltinElement)gpe).getFuncType().equals(BuiltinType.Not)) {
				List<Node> args = ((BuiltinElement)gpe).getArguments();
				if (args != null && args.size() == 1 && args.get(0) instanceof KnownNode) {
					reportError(object, "Phrase 'not known' is not a valid graph pattern; did you mean 'is not known'?");
				}
			}
		}
		if (gpe.getNext() != null) {
			numErrors += validateGraphPatternElement(object, gpe.getNext());
		}
		return numErrors;
	}
    
    public Test[] translateTest(com.ge.research.sadl.sadl.Test object) {
		Expression expr = object.getExpr();
		// we know it's a Test so create one and set as translation target
		Test test = new Test();
		setTranslationTarget(test);

		// now translate the test expression
		Object testtrans = translate(expr);
		
		// Examine testtrans, the results of the translation.
		// The recognition of various Test patterns, so that the LHS, RHS, Comparison of the Test can be
		// properly set is best done on the translation before the ProxyNodes are expanded--their expansion
		// destroys needed information and introduces ambiguity
		
		try {
			Test[] generatedTests = null;
			boolean done = false;
			if (testtrans instanceof BuiltinElement 
					&& IntermediateFormTranslator.isComparisonBuiltin(((BuiltinElement)testtrans).getFuncName())) {
				List<Node> args = ((BuiltinElement)testtrans).getArguments();
				if (args != null && args.size() == 2) {
					test.setCompName(((BuiltinElement)testtrans).getFuncType());
					Object lhsObj = getIfTranslator().expandProxyNodes(args.get(0), false, true);
					Object rhsObj = getIfTranslator().expandProxyNodes(args.get(1), false, true);
					test.setLhs((lhsObj != null && lhsObj instanceof List<?> && ((List<?>)lhsObj).size() > 0) ? lhsObj : args.get(0));
					test.setRhs((rhsObj != null && rhsObj instanceof List<?> && ((List<?>)rhsObj).size() > 0) ? rhsObj : args.get(1));
					generatedTests = new Test[1];
					generatedTests[0] = test;
					done = true;
				}
			}
			else if (testtrans instanceof TripleElement) {
				if (((TripleElement)testtrans).getModifierType() != null &&
						!((TripleElement)testtrans).getModifierType().equals(TripleModifierType.None)) {
					// Filtered query with modification
					TripleModifierType ttype = ((TripleElement)testtrans).getModifierType();
					Object trans = getIfTranslator().expandProxyNodes(testtrans, false, true);
					if ((trans != null && trans instanceof List<?> && ((List<?>)trans).size() > 0)) {
						if (ttype.equals(TripleModifierType.Not)) {
							if (changeFilterDirection(trans)) {
								((TripleElement)testtrans).setType(TripleModifierType.None);
							}
						}
						test.setLhs(trans);
					}
					else {
						if (ttype.equals(TripleModifierType.Not)) {
							changeFilterDirection(testtrans);
						}
						test.setLhs(testtrans);
					}
					generatedTests = new Test[1];
					generatedTests[0] = test;
					done = true;
				}
			}
			
			if (!done) {
				// expand ProxyNodes and see what we can do with the expanded form
				List<Object> expanded = new ArrayList<Object>();
				Object testExpanded = getIfTranslator().expandProxyNodes(testtrans, false, true);
				boolean treatAsMultipleTests = false; {
					if (testExpanded instanceof List<?>) {
						treatAsMultipleTests = containsMultipleTests((List<GraphPatternElement>) testExpanded);
					}
				}
				if (treatAsMultipleTests && testExpanded instanceof List<?>) {
					for (int i = 0; i < ((List<?>)testExpanded).size(); i++) {
						expanded.add(((List<?>)testExpanded).get(i));
					}
				}
				else {
					expanded.add(testExpanded);
				}

				if (expanded.size() == 0) {
					generatedTests = new Test[1];
					generatedTests[0] = test;
				}
				else {
					generatedTests = new Test[expanded.size()];
	
					for (int i = 0; expanded != null && i < expanded.size(); i++) {
						Object testgpe = expanded.get(i);
						if (i > 0) {
							// not the first element; need a new Test
							test = new Test();
						}
						generatedTests[i] = test;
	
						// Case 3: the test translates into a TripleElement
						if (testgpe instanceof TripleElement) {
							test.setLhs(testgpe); 
						}
						else if (!done && testgpe instanceof List<?>) {
							test.setLhs(testgpe);
						}
					}
				}
			}
			
			for (int i = 0; generatedTests != null && i < generatedTests.length; i++) {
				test = generatedTests[i];
				getIfTranslator().postProcessTest(test, object);
				ICompositeNode node = NodeModelUtils.findActualNodeFor(object);
				if (node != null) {
						test.setLineNo(node.getStartLine());
						test.setLength(node.getLength());
						test.setOffset(node.getOffset());
				}
		
				logger.info("Test translation: {}", test);
				List<IFTranslationError> transErrors = getIfTranslator().getErrors();
				for (int j = 0; transErrors != null && j < transErrors.size(); j++) {
					IFTranslationError err = transErrors.get(j);
					try {
						reportError((EObject) err.getErrorLocation(), err.getLocalizedMessage());
					}
					catch (Exception e) {
						// this will happen for standalone testing where there is no Eclipse Workspace
						logger.error("Test: " + test.toString());
						logger.error("  Translation error: " + err.getLocalizedMessage() + 
								(err.getCause() != null ? (" (" + err.getCause().getLocalizedMessage() + ")") : ""));
					}
				}
		
				validateTest(object, test);
				annotateErrors(object, getModel().addTest(test));
			}
			return generatedTests;
		} catch (InvalidNameException e) {
			reportError(expr, "Invalid name in Test: " + test.toString());
			e.printStackTrace();
		} catch (InvalidTypeException e) {
			reportError(expr, "Invalid type in Test: " + test.toString());
			e.printStackTrace();
		} catch (TranslationException e) {
			reportError(expr, "Translation exception in Test: " + test.toString());
			e.printStackTrace();
		}
		return null;
    }
    
    private boolean changeFilterDirection(Object patterns) {
		if (patterns instanceof List<?>) {
			for (int i = 0; i < ((List<?>)patterns).size(); i++) {
				Object litem = ((List<?>)patterns).get(i);
				if (litem instanceof BuiltinElement) {
					IntermediateFormTranslator.builtinComparisonComplement((BuiltinElement)litem);
					return true;
				}
			}
		}
		return false;
	}

	private boolean containsMultipleTests(List<GraphPatternElement> testtrans) {
		if (testtrans.size() == 1) {
			return false;
		}
		List<VariableNode> vars = new ArrayList<VariableNode>();
		for (int i = 0; i < testtrans.size(); i++) {
			GraphPatternElement gpe = testtrans.get(i);
			if (gpe instanceof TripleElement) {
				Node anode = ((TripleElement)gpe).getSubject();
				if (vars.contains(anode)) {
					return false;  // there are vars between patterns
				}
				else if (anode instanceof VariableNode) {
					vars.add((VariableNode) anode);
				}
				anode = ((TripleElement)gpe).getObject();
				if (vars.contains(anode)) {
					return false;  // there are vars between patterns
				}
				else if (anode instanceof VariableNode){
					vars.add((VariableNode) anode);
				}
			}
			else if (gpe instanceof BuiltinElement) {
				List<Node> args = ((BuiltinElement)gpe).getArguments();
				for (int j = 0; args != null && j < args.size(); j++) {
					Node anode = args.get(j);
					if (anode instanceof VariableNode && vars.contains(anode)) {
						return false;  // there are vars between patterns
					}
					else if (anode instanceof VariableNode) {
						vars.add((VariableNode) anode);
					}
				}
			}
		}
		return true;
	}

	public com.ge.research.sadl.model.gp.Rule translateRule(Rule object) {
		String ruleName = object.getName();
		EList<Expression> givens = null;
		EList<Expression> ifs = null;
		EList<Expression> thens = null;
		if (object.getGivens() != null)  {
			givens = object.getGivens().getElements();
		}
		if (object.getIfs() != null) {
			ifs = object.getIfs().getElements();
		}
		if (object.getThens() != null) {
			thens = object.getThens().getElements();
		}
		com.ge.research.sadl.model.gp.Rule rule = new com.ge.research.sadl.model.gp.Rule(ruleName);
		setTranslationTarget(rule);
		if (givens != null) {
			for (int i = 0; i < givens.size(); i++) {
				Object gobj = translate(givens.get(i));
				if (gobj instanceof GraphPatternElement) {
					validateGraphPatternElement(givens.get(i), (GraphPatternElement)gobj);
					rule.addGiven((GraphPatternElement) gobj);
				}
				else if (gobj != null){
					reportError(givens.get(i), "translation of rule '" + ruleName + "' given element '" + gobj.toString() + "' isn't a GraphPatternElement");
				}
			}
		}
		if (ifs != null) {
			for (int i = 0; i < ifs.size(); i++) {
				Object iobj = translate(ifs.get(i));
				if (iobj instanceof GraphPatternElement) {
					validateGraphPatternElement(ifs.get(i), (GraphPatternElement)iobj);
					rule.addIf((GraphPatternElement) iobj);
				}
				else if (iobj != null){
					reportError(ifs.get(i), "translation of '" + ruleName + "' if element '" + (iobj != null ? iobj.toString() : "null") + "' isn't a GraphPatternElement");
				}
			}
		}
		if (thens != null) {
			for (int i = 0; i < thens.size(); i++) {
				Object tobj =  translate(thens.get(i));
				if (tobj instanceof GraphPatternElement) {
					validateGraphPatternElement(thens.get(i), (GraphPatternElement)tobj);
					if (((GraphPatternElement)tobj).isEmbedded()) {
						rule.addIf((GraphPatternElement) tobj);
					}
					else {
						rule.addThen((GraphPatternElement) tobj);
					}
				}
				else if (tobj != null){
					reportError(thens.get(i),"translation of '" + ruleName + "' then element '" + (tobj != null ? tobj.toString() : "null") + "' isn't a GraphPatternElement");
				}
			}
		}
		rule = getIfTranslator().postProcessRule(rule, object);
		return rule;
    }
	
	private int validateQuery(com.ge.research.sadl.sadl.Query object, com.ge.research.sadl.model.gp.Query query) {
		int numErrors = 0;
		List<GraphPatternElement> patterns = query.getPatterns();
		if (patterns != null) {
			numErrors += validateGraphPatternElements(object, patterns);
		}
		return numErrors;
	}
	
	private int validateGraphPatternElements(ModelElement object, List<GraphPatternElement> patterns) {
		int numErrors = 0;
		if (patterns != null) {
			for (int i = 0; i < patterns.size(); i++) {
				GraphPatternElement gpe = patterns.get(i);
				if (gpe instanceof BuiltinElement) {
					numErrors += validateBuiltinElement(object, (BuiltinElement)gpe);
				}
				else if (gpe instanceof TripleElement) {
					// ??
				}
				else if (gpe instanceof Junction) {
					Object lhs = ((Junction)gpe).getLhs();
					if (lhs instanceof List<?>) {
						numErrors += validateGraphPatternElements(object, (List<GraphPatternElement>) lhs);
					}
					else if (lhs instanceof BuiltinElement) {
						numErrors += validateBuiltinElement(object, (BuiltinElement)lhs);
					}
					Object rhs = ((Junction)gpe).getRhs();
					if (rhs instanceof List<?>) {
						numErrors += validateGraphPatternElements(object, (List<GraphPatternElement>) rhs);
					}
					else if (rhs instanceof BuiltinElement) {
						numErrors += validateBuiltinElement(object, (BuiltinElement)rhs);
					}
				}
			}
		}
		return numErrors;
	}
	
	private int validateBuiltinElement(ModelElement object, BuiltinElement bi) {
		int numErrors = 0;
		int expectedArgCnt = bi.getExpectedArgCount();
		if (expectedArgCnt > 0 && 
				(bi.getArguments() == null ||
						bi.getArguments().size() != expectedArgCnt)) {
			ModelError me = new ModelError("Built-in '" + bi.toString() + 
					"' does not have the expected " + expectedArgCnt + " arguments. Perhaps parentheses are needed?", ErrorType.ERROR);
			getModel().addError(me);
			numErrors++;
		}
		return numErrors;
	}

	private int validateRule(Rule object, com.ge.research.sadl.model.gp.Rule rule) throws MalformedURLException, ConfigurationException {
		int numErrors = 0;
		// check typed variable consistency with property domain/range
		Map<String, NamedNode> typedVars = getTypedVars(rule);
		if (typedVars != null) {
			List<ModelError> errors = checkPropertyDomainAndRange(typedVars, rule);
			if (errors != null) {
				for (int i = 0; i < errors.size(); i++) {
					getModel().addError(errors.get(i));
				}
				numErrors += errors.size();
			}
		}
		
		// conclusion binding tests are done in specific translator
		List<ModelError> errors = getModel().getConfigurationMgr().getTranslator().validateRule(rule);
		if (errors != null) {
			for (int i = 0; i < errors.size(); i++) {
				getModel().addError(errors.get(i));
			}
			numErrors += errors.size();
		}
		return numErrors;
	}
	
	private List<ModelError> checkPropertyDomainAndRange(Map<String, NamedNode> typedVars, com.ge.research.sadl.model.gp.Rule rule) {
		List<ModelError> results = checkPropertyDomainAndRange(typedVars, rule.getGivens());
		List<ModelError> moreResults = checkPropertyDomainAndRange(typedVars, rule.getIfs());
		if (moreResults != null) {
			if (results == null) {
				results = moreResults;
			}
			else {
				results.addAll(moreResults);
			}
		}
		moreResults = checkPropertyDomainAndRange(typedVars, rule.getGivens());
		if (moreResults != null) {
			if (results == null) {
				results = moreResults;
			}
			else {
				results.addAll(moreResults);
			}
		}
		return results;
	}
	
	private List<ModelError> checkPropertyDomainAndRange(Map<String, NamedNode> vars, List<GraphPatternElement> gpes) {
		List<ModelError> results = null;
		for (int i = 0; gpes != null && i < gpes.size(); i++) {
			GraphPatternElement gpe = gpes.get(i);
			if (gpe instanceof TripleElement && !(((TripleElement)gpe).getPredicate() instanceof RDFTypeNode)) {
				Node subj = ((TripleElement)gpe).getSubject();
				if (subj instanceof VariableNode && vars.containsKey(((VariableNode) subj).getName()) &&
						((TripleElement)gpe).getPredicate() instanceof NamedNode) {
					NamedNode pred = (NamedNode) ((TripleElement)gpe).getPredicate();
					String varName = ((VariableNode) subj).getName();
					if (!getModel().validateClassInDomain(pred, vars.get(varName))) {
						if (results == null) {
							results = new ArrayList<ModelError>();
						}
						results.add(new ModelError("Variable '" + varName + 
								"' is declared to be of type '" + vars.get(varName) + 
								"' but is used as subject of property '" + pred.toString() + "' and isn't in the domain.", ErrorType.WARNING));
					}
				}
				Node obj = ((TripleElement)gpe).getObject();
				if (obj instanceof VariableNode && vars.containsKey(((VariableNode) obj).getName()) &&
						((TripleElement)gpe).getPredicate() instanceof NamedNode) {
					NamedNode pred = (NamedNode) ((TripleElement)gpe).getPredicate();
					String varName = ((VariableNode) obj).getName();
					if (!getModel().validateClassInRange(pred, vars.get(varName))) {
						if (results == null) {
							results = new ArrayList<ModelError>();
						}
						results.add(new ModelError("Variable '" + varName + 
								"' is declared to be of type '" + vars.get(varName) + 
								"' but is used as object of property '" + pred.toString() + "' and isn't in the range.", ErrorType.WARNING));
					}
				}
			}
			else if (gpe instanceof Junction) {
				Object lobj = ((Junction)gpe).getLhs();
				Object robj = ((Junction)gpe).getRhs();
				List<GraphPatternElement> junctgpes = new ArrayList<GraphPatternElement>();
				if (lobj instanceof GraphPatternElement) {
					junctgpes.add((GraphPatternElement) lobj);
				}
				if (robj instanceof GraphPatternElement) {
					junctgpes.add((GraphPatternElement) robj);
				}
				List<ModelError> moreerrors = checkPropertyDomainAndRange(vars, junctgpes);
				if (results == null) {
					results = moreerrors;
				}
				else if (moreerrors != null){
					results.addAll(moreerrors);
				}
			}
		}
		return results;
	}

	private Map<String, NamedNode> getTypedVars(com.ge.research.sadl.model.gp.Rule rule) {
		Map<String, NamedNode> results = getTypedVars(rule.getGivens());
		Map<String, NamedNode> moreResults = getTypedVars(rule.getIfs());
		if (moreResults != null) {
			if (results == null) {
				results = moreResults;
			}
			else {
				results.putAll(moreResults);
			}
		}
		moreResults = getTypedVars(rule.getThens());
		if (moreResults != null) {
			if (results == null) {
				results = moreResults;
			}
			else {
				results.putAll(moreResults);
			}
		}
		return results;
	}
	
	private Map<String, NamedNode> getTypedVars(List<GraphPatternElement> gpes) {
		Map<String, NamedNode> results = null;
		for (int i = 0; gpes != null && i < gpes.size(); i++) {
			GraphPatternElement gpe = gpes.get(i);
			if (gpe instanceof TripleElement && 
					(((TripleElement)gpe).getModifierType() == null || 
					((TripleElement)gpe).getModifierType().equals(TripleModifierType.None) ||
					((TripleElement)gpe).getModifierType().equals(TripleModifierType.Not)) &&
					((TripleElement)gpe).getSubject() instanceof VariableNode &&
					((TripleElement)gpe).getPredicate() instanceof RDFTypeNode &&
					((TripleElement)gpe).getObject() instanceof NamedNode) {
				if (results == null) {
					results = new HashMap<String, NamedNode>();
				}
				String varName = ((VariableNode)((TripleElement)gpe).getSubject()).getName();
				NamedNode varType = (NamedNode) ((TripleElement)gpe).getObject();
				if (results.containsKey(varName)) {
					NamedNode nn = results.get(varName);
					if (!nn.equals(varType) && !(nn instanceof VariableNode || varType instanceof VariableNode)) {
						getModel().addError(new ModelError("Variable '" + varName + "' is typed more than once in the rule.", ErrorType.WARNING));
					}
				}
				results.put(varName, varType);
			}
			else if (gpe instanceof Junction) {
				Object lobj = ((Junction)gpe).getLhs();
				Object robj = ((Junction)gpe).getRhs();
				if (lobj instanceof GraphPatternElement || robj instanceof GraphPatternElement) {
					List<GraphPatternElement> junctgpes = new ArrayList<GraphPatternElement>();
					if (lobj instanceof GraphPatternElement) {
						junctgpes.add((GraphPatternElement) lobj);
					}
					if (robj instanceof GraphPatternElement) {
						junctgpes.add((GraphPatternElement) robj);
					}
					if (results == null) {
						results = getTypedVars(junctgpes);
					}
					else {
						Map<String, NamedNode> moreresults = getTypedVars(junctgpes);
						if (moreresults != null) {
							results.putAll(moreresults);
						}
					}
				}
			}
		}
		return results;
	}

	private int validateTest(EObject object, Test test) {
		int numErrors = 0;
		Object lhs = test.getLhs();
		if (lhs instanceof GraphPatternElement) {
			numErrors += validateGraphPatternElement(object, (GraphPatternElement)lhs);
		}
		else if (lhs instanceof List<?>) {
			for (int i = 0; i < ((List<?>)lhs).size(); i++) {
				Object lhsinst = ((List<?>)lhs).get(i);
				if (lhsinst instanceof GraphPatternElement) {
					numErrors += validateGraphPatternElement(object, (GraphPatternElement)lhsinst);
				}
			}
		}
		Object rhs = test.getLhs();
		if (rhs instanceof GraphPatternElement) {
			numErrors += validateGraphPatternElement(object, (GraphPatternElement)rhs);
		}
		else if (rhs instanceof List<?>) {
			for (int i = 0; i < ((List<?>)rhs).size(); i++) {
				Object rhsinst = ((List<?>)rhs).get(i);
				if (rhsinst instanceof GraphPatternElement) {
					numErrors += validateGraphPatternElement(object, (GraphPatternElement)rhsinst);
				}
			}
		}
		return numErrors;
	}
	
	public Object translate(EObject expr) {
		Object pattern = null;

		try {
			if (expr instanceof Expression) {
				pattern = getIfTranslator().translate((Expression)expr);
			}
			else if (expr instanceof GraphPattern) {
				pattern = getIfTranslator().translate((GraphPattern)expr);
			}
		}
		catch (InvalidNameException e) {
			String errmsg = "Encountered invalid name in expression '" + expr.toString() + "': " + e.getLocalizedMessage();
			logger.error(errmsg, e);
			reportError(expr, errmsg);
		} catch (InvalidTypeException e) {
			String errmsg = "Encountered invalid type in expression '" + expr.toString() + "': " + e.getLocalizedMessage();
			logger.error(errmsg, e);
			reportError(expr, errmsg);
		} catch (TranslationException e) {
			String errmsg = "Encountered translation error in expression '" + expr.toString() + "': " + e.getLocalizedMessage();
			logger.error(errmsg, e);
			reportError(expr, errmsg);
		}
		catch (Throwable t) {
			String errmsg = "Unexpected translation error in expression '" + expr.toString() + "': " + t.getLocalizedMessage();
			logger.error(errmsg, t);
			reportError(expr, errmsg);			
		}
		if (pattern instanceof Query) {
			return pattern;		// we have found a query during translation
		}
		else if (pattern instanceof GraphPatternElement) {
			final ICompositeNode node = NodeModelUtils.findActualNodeFor(expr);
			if (node != null)  {
				((GraphPatternElement)pattern).setLineNo(node.getStartLine());
				((GraphPatternElement)pattern).setLength(node.getLength());
				((GraphPatternElement)pattern).setOffset(node.getOffset());
//							// anything special here for Built-ins??
			}
		}
		else if (pattern instanceof Junction) {
			// anything special here for Junctions??
		}
		else if (pattern != null && !(pattern instanceof Literal) && !(pattern instanceof NamedNode) && 
				!(pattern instanceof KnownNode) && !(pattern instanceof ValueTableNode)) {
			reportError(expr, "Invalid pattern in Expr: " + expr.toString());
			logger.error("SMM.translate(EObject) doesn't know how to handle '{}'", pattern);
		}
		return pattern;
	}

	public int[] runAllTests(String modelName, boolean validateBeforeTesting, boolean showReasonerTiming) {
		return getModel().runAllTests(modelName, validateBeforeTesting, showReasonerTiming);
	}

	public void runQuery(String modelName, Query query) {
		getModel().runQuery(modelName, query);	
	}
	
	public void runQuery(String modelName, String sparqlQueryStr) {
		getModel().runQuery(modelName, sparqlQueryStr);
	}

	/**
	 * This method will return the next ModelManager message or null if there are none.
	 * 
	 * @return - the next SadlMessage
	 */
	public SadlMessage getNextMessage() {
		return getModel().getMessageManager().getNextMessage();
	}

	public List<ConfigurationItem> getConfiguration(String[] categoryHierarchy, boolean bIncludeSubcategories) throws ConfigurationException, MalformedURLException {
		return getModel().getConfiguration(categoryHierarchy, bIncludeSubcategories);
	}

	public void addConfiguration(ConfigurationItem newItem) throws ConfigurationException, MalformedURLException {
		getModel().addConfiguration(newItem);
	}
	
	public void updateConfiguration(ConfigurationItem newItem) throws ConfigurationException, MalformedURLException {
		getModel().updateConfiguration(newItem);
	}

	public synchronized IConfigurationManagerForIDE getConfigurationMgr(String modelFolderName) throws ConfigurationException, URISyntaxException, IOException {
		if (modelFolderName != null) {
            // What project is this? There should be one ConfigurationManager per project.
			SadlUtils su = new SadlUtils();
			URI mfuri = URI.createURI(su.fileNameToFileUrl(modelFolderName));
            IConfigurationManagerForIDE configurationMgr = getConfigurationMgr(mfuri);
    		lastConfigMgr = configurationMgr;	// this is for when Xtext springs a call on us without a project identifier
    		return configurationMgr;
		}
		return lastConfigMgr;
	}

	public IConfigurationManagerForIDE getConfigurationMgr(URI mfuri) throws ConfigurationException, URISyntaxException, IOException {
        URI projectUri = ResourceManager.getProjectUri(mfuri);
        IConfigurationManagerForIDE configurationMgr = configurationMgrMap.get(projectUri);
        // See if we already have a ConfigurationManager for this project and if so use it.
        String owlModelsFolder = ResourceManager.getOwlModelsFolder(mfuri);
        configurationManagerProvider.setModelFolder(owlModelsFolder);
		if (configurationMgr == null) {
			ResourceManager.validateOwlModelsFolder(owlModelsFolder);
			configurationMgr = configurationManagerProvider.get();
			configurationMgr.setProjectFolderPath(projectUri.toString(), owlModelsFolder);   
			configurationMgrMap.put(projectUri, configurationMgr);
	    	IPreferencesService service = Platform.getPreferencesService();
	    	String format = service.getString("com.ge.research.sadl.Sadl", "OWL_Format", ConfigurationManagerForIDE.getOWLFormat(), null);	
	    	SadlJenaModelGetterPutter modelGetter = new SadlJenaModelGetterPutter(configurationMgr.getTdbFolder(), format);
	    	configurationMgr.setModelGetter(modelGetter);
		}
		else if (configurationMgr.isConfigurationStale()) {
			configurationMgr = configurationManagerProvider.get();
			configurationMgrMap.put(projectUri, configurationMgr);
	    	IPreferencesService service = Platform.getPreferencesService();
	    	String format = service.getString("com.ge.research.sadl.Sadl", "OWL_Format", ConfigurationManagerForIDE.getOWLFormat(), null);	
	    	SadlJenaModelGetterPutter modelGetter = new SadlJenaModelGetterPutter(configurationMgr.getTdbFolder(), format);
	    	configurationMgr.setModelGetter(modelGetter);
			configurationMgr.getModelGetter().setTdbFolder(configurationMgr.getTdbFolder());
		} else if (!configurationMgr.getModelFolderPath().equals(new File(owlModelsFolder))) {
			if (configurationMgr.isConfigChanged()) {
				configurationMgr.saveConfiguration();
			}
			configurationMgr = configurationManagerProvider.get();
			configurationMgrMap.put(projectUri, configurationMgr);
			configurationMgr.getModelGetter().setTdbFolder(configurationMgr.getTdbFolder());
		}
		return configurationMgr;
	}
	
	private ModelManager getModel(Resource resource) {
		ModelInfo minfo = getModelInfo(resource);
		if (minfo != null) {
			return minfo.getModel();
		}
		return null;
	}
	
	/** Method to check to see if this Resource has a ModelManager already created.
	 * 
	 * @param resource
	 * @return true if MM exists for this Resource else false
	 */
	public boolean hasModelManager(Resource resource) {
		if (resource != null) {
			URI uri = resource.getURI();
			ModelInfo minfo = getModelInfo(uri);
			if (minfo == null) {
				return false;
			}
			return true;
		}
		return false;
	}

	public ModelManager getModel() {
		ModelInfo minfo = getModelInfo(getResource());
		if (minfo != null) {
			return minfo.getModel();
		}
		return null;
	}
	
	public boolean graphNeighborhood(ConceptName conceptName) {
		ModelInfo minfo = getModelInfo(getResource());
		if (minfo != null) {
			return minfo.getModel().graphNeighborhood(conceptName);
		}
		return false;
	}

	private void setResource(Resource resource) {
		setCurrentResource(resource);
	}

	private Resource getResource() {
		return getCurrentResource();
	}
	
	public IntermediateFormTranslator getIfTranslator() {
		ModelInfo minfo = getModelInfo(getResource());
		if (minfo != null) {
			IntermediateFormTranslator ift = minfo.getIfTranslator();
			if (ift == null) {
				ift = new IntermediateFormTranslator(getModel());
				minfo.setIfTranslator(ift);
			}
			return ift;
		}
		return null;
	}
	
	private ModelInfo getModelInfo(Resource resource) {
		if (resource != null) {
			URI uri = resource.getURI();
			ModelInfo minfo = getModelInfo(uri);
			if (minfo == null) {
				logger.debug("Creating new ModelInfo for Resource '" + resource.toString());
				minfo = new ModelInfo();
				ModelManager mmgr = new ModelManager();
				minfo.setModel(mmgr);
				modelMgrs.put(resource.getURI(), minfo);
			}
			return minfo;
		}
		return null;
	}
	
	private ModelInfo getModelInfo(URI resourceUri) {
		ModelInfo minfo = modelMgrs.get(resourceUri);
		return minfo;
	}

	public String toString() {
		return getResource().getURI().toString();
	}

    private List<ModelError> annotateErrors(EObject object,
			List<ModelError> errorList) {
		if (errorList != null) {
			if (object != null &&  object.eAdapters() != null) {
				final ICompositeNode node = NodeModelUtils.findActualNodeFor(object);
				if (node != null) {
					int lnnum = node.getStartLine();
					int lnlen = node.getLength();
					int offset = node.getOffset();
					for (int i = 0; i < errorList.size(); i++) {
						ModelError merr = errorList.get(i);
						merr.setLineNumber(lnnum);
						merr.setLineLength(lnlen);
						merr.setOffset(offset);
					}
				}
			}
			else {
				logger.error("annotateErrors called with null object; this shouldn't happen. Please report.");
			}
		}
		return errorList;
	}

    private void createMarker (IFile file, ModelError error) {
		try {
			IMarker mkr = file.createMarker("com.ge.research.sadl.problem");
			mkr.setAttribute(IMarker.MESSAGE, error.getErrorMsg());
			mkr.setAttribute(IMarker.LINE_NUMBER, error.getLineNumber());
			mkr.setAttribute(IMarker.LOCATION, String.format("%s:%s", file.getName(), error.getLineNumber()));
			mkr.setAttribute(IMarker.SEVERITY, error.getErrorType() == ModelError.ErrorType.ERROR ? IMarker.SEVERITY_ERROR : IMarker.SEVERITY_WARNING);
			mkr.setAttribute(IMarker.CHAR_START, error.getOffset());
			mkr.setAttribute(IMarker.CHAR_END, error.getOffset() + error.getLineLength());
		} catch (CoreException e) {
			e.printStackTrace();
		}
    }
    
    public void deleteMarkers (Resource resource, boolean all) {
        //this gives the relative path in the workspace
        URI uri = resource.getURI();
        if (uri != null) {
	        try {
		        //to resolve this we need the workspace root
		        IWorkspaceRoot myWorkspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		
		        //create an new IPath from the URI
		        String platformStr = uri.toPlatformString(false);
		        if (platformStr == null) {
		        	// will this happen only for OWL files not created from SADL?
		        	return;
		        }
		        IPath path = new Path(platformStr);
		
		        //finally resolve the file with the workspace
		        IFile file = myWorkspaceRoot.getFile(path);
		
		        // delete the markers
		        try {
		        	if (savingModel) {
						file.deleteMarkers(null, true, IResource.DEPTH_INFINITE);		// this seems to remove too much but if not here some "quick" error markers seem too sticky...	        		
		        	}
		        	else {
		        		file.deleteMarkers("com.ge.research.sadl.problem", true, IResource.DEPTH_INFINITE);
		        		file.deleteMarkers("com.ge.research.sadl.ui.sadl.check.fast", true, IResource.DEPTH_INFINITE);
		        		if (all) {
		        			// file.deleteMarkers("org.eclipse.xtext.ui.check.fast", true, IResource.DEPTH_INFINITE);
		        			 file.deleteMarkers(null, true, IResource.DEPTH_INFINITE);
		        		}
		        	}
				} catch (CoreException e) {
					e.printStackTrace();
				}
		        setErrorCount(0);
		        setMarkErrors(true);
	        }
	        catch (IllegalStateException e) {
	        	// ignore this--standalone testing is unable to find the Workspace and throws this exception
	        }
        }
    }
    
    public int countErrorMarkers(Resource resource) {
        //this gives the relative path in the workspace
        URI uri = resource.getURI();

        try {
	        //to resolve this we need the workspace root
	        IWorkspaceRoot myWorkspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
	
	        //create an new IPath from the URI
	        IPath path = new Path(uri.toPlatformString(false));
	
	        //finally resolve the file with the workspace
	        IFile file = myWorkspaceRoot.getFile(path);
	
	        // delete the markers
        	IMarker[] markers = file.findMarkers(null, true, IResource.DEPTH_INFINITE);
        	int cnt = 0;
        	for (int i = 0; i < markers.length; i++) {
        		IMarker mkr = markers[i];
        		String type = mkr.getType();
        		String msg = mkr.getAttribute(IMarker.MESSAGE, "");
    			int severity = mkr.getAttribute(IMarker.SEVERITY, 0);
        		if (severity > 1 && !type.equals("com.ge.research.sadl.problem")) {
        			cnt++;
        		}
        	}
        	return cnt;
	        	
		} catch (CoreException e) {
			e.printStackTrace();
		}
	    return 0;   
    }

	public String getAltUrl(String publicUri, URI resourceUri) throws MalformedURLException {
		String modelFolder = null;
		if (resourceUri != null) {
			modelFolder = ResourceManager.getOwlModelsFolder(resourceUri);
		}
		// this ModelManager hasn't been initialized
		try {
			if (getConfigurationMgr(modelFolder) != null) {
				return getConfigurationMgr(modelFolder).getAltUrlFromPublicUri(publicUri);
			}
		} catch (ConfigurationException e) {
			// it isn't necessarily an error to not find a mapping--we may be checking to make
			//	sure a name hasn't been used.
			return null;
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	private void setErrorCount(int _errorCount) {
		errorCount = _errorCount;
	}
	
	private int augmentErrorCount(int newErrors) {
		errorCount += newErrors;
		return errorCount;
	}

	private void setMarkErrors(boolean markErrors) {
		this.markErrors = markErrors;
	}

	public static String removeEscapeCharacters(String string) {
		int idx = string.indexOf('^');
		if (idx >= 0) {
			String value = string;
			while ((idx = value.indexOf('^')) >= 0) {
				value = value.substring(0, idx) + (idx < (value.length() + 1) ? value.substring(idx + 1) : "");
			}
			return value;
		}
		return string;
	}

	public void editorClosed(String partName) {
		// The editor closed--remove the instance of ModelManager for the closed Resource
		if (getModelResource().getURI().toString().endsWith(partName)) {
			modelMgrs.remove(getModelResource());
		}
		else {
			URI closedEditorUri = null;
			Iterator<URI> mmItr = modelMgrs.keySet().iterator();
			while (mmItr.hasNext()) {
				URI key = mmItr.next();
				if (key.lastSegment().equals(partName)) {
					closedEditorUri = key;
					break;
				}
			}
			if (closedEditorUri != null) {
				modelMgrs.remove(closedEditorUri);
			}
		}
	}

	public int validateModel(String modelName, IReasoner reasoner) {
		return getModel().validateModel(modelName, reasoner);
	}
	
	public boolean removeResourceModel(Resource resource) {
		ModelInfo mi = modelMgrs.get(resource.getURI());
		if (mi != null && mi.getModel() != null) {
			try {
				mi.getModel().getConfigurationMgr().resetJena();
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
    	return (modelMgrs.remove(resource.getURI()) == null);
	}

	public ConceptName getObjectPropertyRange(ConceptName propName) throws ConfigurationException {
		return getModel().getObjectPropertyRange(propName);
	}

	public int getNumTranslationErrors() {
		return translationErrors;
	}

	private void setTranslationErrors(int translationErrors) {
		this.translationErrors = translationErrors;
	}

	public MessageManager getMessageManager() {
		return getModel().getMessageManager();
	}

	public void removeAllProjectResourceModels(String prjName) {
		if (modelMgrs != null) {
			List<URI> toClose = null;
			Iterator<URI> uriitr = modelMgrs.keySet().iterator();
			while (uriitr.hasNext()) {
				URI modeluri = uriitr.next();
				String mpstr = modeluri.segment(1);
				if (mpstr != null && mpstr.equals(prjName)) {
					if (toClose == null) {
						toClose = new ArrayList<URI>();
					}
					toClose.add(modeluri);
				}
			}
			if (toClose != null) {
				for (int i = 0; i < toClose.size(); i++) {
					modelMgrs.remove(toClose.get(i));
				}
			}
		}
		if (configurationMgrMap != null) {
			Iterator<URI> cmitr = configurationMgrMap.keySet().iterator();
			while (cmitr.hasNext()) {
				URI prjcmuri = cmitr.next();
				if (prjcmuri.lastSegment().equals(prjName)) {
					configurationMgrMap.remove(prjcmuri);
				}
			}
		}
		
	}
	
	public Enumeration<IConfigurationManagerForIDE> getConfigurationManagers() {
		if (configurationMgrMap != null) {
			return configurationMgrMap.elements();
		}
		return null;
	}

	public Resource getCurrentResource() {
		return currentResource;
	}

	public void setCurrentResource(Resource currentResource) {
		this.currentResource = currentResource;
	}

//	@Override
//	public EObject caseUserDefinedDataType(UserDefinedDataType object) {
//		String name = object.getUdt();
//		DataTypeRestriction dtr = object.getRestriction();
//		if (dtr != null) {
//			String baseType = dtr.getBasetype();
//			EList<String> unionOfTypes = dtr.getBasetypes();
//			Facets fcts = dtr.getFacets();
//			String minexin = null;
//			String min = null;
//			String maxexin = null;
//			String max = null;
//			String regex = null;
//			EList<String> values = null;
//			if (fcts != null) {
//				minexin = fcts.getMinexin();
//				min = fcts.getMin();
//				maxexin = fcts.getMaxexin();
//				max = fcts.getMax();
//				regex = fcts.getRegex();
//				values = fcts.getValues();
//			}
//			annotateErrors(object, getModel().addUserDefinedDataType(name, unionOfTypes, baseType, minexin, min, maxexin, max, regex, values));
//			return super.caseUserDefinedDataType(object);
//		}
//		return null;
//	}

	@Override
	public void partActivated(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void partBroughtToTop(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void partClosed(IWorkbenchPartReference partRef) {
		// TODO
	}

	@Override
	public void partDeactivated(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void partOpened(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void partHidden(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void partVisible(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void partInputChanged(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		
	}

}
