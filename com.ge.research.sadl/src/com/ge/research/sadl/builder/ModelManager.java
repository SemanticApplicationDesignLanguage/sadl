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

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.2 $ Last modified on   $Date: 2014/06/12 14:48:51 $
 ***********************************************************************/

package com.ge.research.sadl.builder;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.activation.DataSource;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.ClassRestrictionCondition;
import com.ge.research.sadl.model.ClassRestrictionCondition.RestrictionType;
import com.ge.research.sadl.model.ConceptIdentifier;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.Explanation;
import com.ge.research.sadl.model.ImportMapping;
import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.ModelError.ExistingNamePart;
import com.ge.research.sadl.model.PendingModelError;
import com.ge.research.sadl.model.PendingModelError.AdditionalCheck;
import com.ge.research.sadl.model.PrefixNotFoundException;
import com.ge.research.sadl.model.SadlEnumeratedClass;
import com.ge.research.sadl.model.SadlIntersectionClass;
import com.ge.research.sadl.model.SadlResourceByRestriction;
import com.ge.research.sadl.model.SadlUnionClass;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.Explain;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.KnownNode;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.Print;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.model.gp.TestResult;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.model.gp.Test.ComparisonType;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.ValueTableNode;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InferenceCanceledException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ReasonerTiming;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.SadlJenaModelGetter;
import com.ge.research.sadl.reasoner.SadlJenaModelGetterPutter;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.utils.SadlUtils;
import com.ge.research.sadl.utils.SadlUtils.ConceptType;
import com.ge.research.sadl.utils.UtilsForJena;
import com.hp.hpl.jena.datatypes.RDFDatatype;
import com.hp.hpl.jena.datatypes.TypeMapper;
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype;
import com.hp.hpl.jena.datatypes.xsd.XSDDateTime;
import com.hp.hpl.jena.datatypes.xsd.XSDDuration;
import com.hp.hpl.jena.ontology.AllValuesFromRestriction;
import com.hp.hpl.jena.ontology.AnnotationProperty;
import com.hp.hpl.jena.ontology.CardinalityRestriction;
import com.hp.hpl.jena.ontology.DataRange;
import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.EnumeratedClass;
import com.hp.hpl.jena.ontology.HasValueRestriction;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.IntersectionClass;
import com.hp.hpl.jena.ontology.MaxCardinalityRestriction;
import com.hp.hpl.jena.ontology.MinCardinalityRestriction;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.ontology.Restriction;
import com.hp.hpl.jena.ontology.SomeValuesFromRestriction;
import com.hp.hpl.jena.ontology.UnionClass;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.Syntax;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.ModelGetter;
import com.hp.hpl.jena.rdf.model.NodeIterator;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFList;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.rdf.model.impl.Util;
import com.hp.hpl.jena.util.ResourceUtils;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.OWL2;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;
import com.hp.hpl.jena.vocabulary.XSD;

/**
 * This class defines the methods used to build the in-memory Jena OntModel for
 * a SADL model file loaded into an editor window.
 * 
 * Notes: 1) Except for requiring the model name first and all imports next, the
 * statements of a SADL model can be in any order. This means that a concept can
 * be referenced in the SADL model before it is defined. The way this is handled
 * by the ModelManager is as follows. When a concept is referenced, the methods
 * getOrCreateDataProperty getOrCreateIndividual getOrCreateObjectProperty
 * getOrCreateOntClass are called, depending on the type of the referenced
 * concept. If the concept is defined, the method just gets it and returns it.
 * If it is not defined and is in the current model (no prefix), the method
 * creates the concept and creates a PendingModelError capturing that the
 * concept is not defined. When a concept definition is provided via calls to
 * ModelManager, the PendingModelError list is checked and if there is a
 * PendingModelError for the concept it is removed. The PendingModelError is
 * added to the errors returned to the caller so that location information can
 * be recorded. After all calls to create the model have been made, the client
 * can call pendingErrorCleared to determine if a PendingModelError is still an
 * error.
 * 
 * 2) URIs, XML Namespaces, and XML base: A URI consists of two parts, the
 * Namespace and the Localname, in that order. The separation between the two
 * parts is determined by working backwards from the end of the URI. "The
 * algorithm tries to find the longest NCName at the end of the [URI], not
 * immediately preceded by the first colon in the string." (From Jena code
 * documentation of com.hp.hpl.jena.rdf.model.impl.Util.splitNamespace(String
 * uri)) For backward compatibility with SADL V1 and for convenience, the model
 * name given by the "uri" statement in a SADL file does not have to end in a
 * non-NCName character. If it does not, a '#' character will be added to the
 * model name to create the model namespace.
 * 
 * @author 200005201
 */
public class ModelManager {
	private static final Logger logger = LoggerFactory
			.getLogger(ModelManager.class);
	private static final String XSD_NS = "http://www.w3.org/2001/XMLSchema#";

	private String owlModelsFolderPath = null;

	public enum ImportListType {
		NAME_AS_URI, NAME_AS_SADL_FILENAME
	}

	private ImportListType importListType = ImportListType.NAME_AS_URI; // default

	private OntModel jenaModel;
	private boolean alwaysCreateNew = true;
	private String modelName;
	private String modelNamespace;
	private String globalPrefix;
	private URI modelActualUrl;
	private String modelBaseUri;
	private String modelVersion;
	
	public class ValidationStatement {
		private Object modelEObject;
		private OntResource subject;
		private OntProperty property;
		private RDFNode object;
		
		public ValidationStatement(OntResource subj, OntProperty prop, RDFNode obj) {
			subject = subj;
			property = prop;
			object = obj;
		}
		
		public RDFNode getObject() {
			return object;
		}

		public OntProperty getProperty() {
			return property;
		}

		public OntResource getSubject() {
			return subject;
		}

		public Object getModelEObject() {
			return modelEObject;
		}
		public void setModelEObject(Object modelEObject) {
			this.modelEObject = modelEObject;
		}
	}
	
	private List<ValidationStatement> pendingValidationStatements = new ArrayList<ValidationStatement>();
	private List<ValidationStatement> queuedValidationStatements = new ArrayList<ValidationStatement>();
	
	private URI previousURI = null; 	// the Resource URI from the last initialization of this ModelManager instance

	private List<Rule> rules = null;
	private List<SadlCommand> sadlCommands = null;

	// *****************************************************************************
	// Error Handling Strategy
	//
	// Some errors are "Pending" errors--they may be found later to not be
	// errors, e.g., reference to a class not yet defined
	// When errors are generated during a call they must be kept separated to
	// allow us to annotate them with the correct information
	// about location for markers.
	// We must keep the error list within this instance of ModelManager inorder
	// to be able to remove errors when needed.
	// Each call to returnAllErrors() marks the end of a method call and will
	// move newErrors to errors and return newErrors so that
	// they can be properly annotated by SadlModelManager.

	private List<ModelError> errors;
	private Map<String, PendingModelError> pendingErrors;
	private List<ModelError> newErrors;

	private MessageManager messageManager = null;

	private Individual lastInstanceCreated = null;

	private List<String> searchedModels = new ArrayList<String>();

	private Map<String, String> namespacesAndPrefixes = null;
	// this is a fast lookup for namespace and prefix for getting lists of
	// resources for content assist
	// namespace is the key, alias is the value
	// Note that this is only explicit prefixes for imports in this model, not
	// indirect imports
	
	private List<String> orderedImportUris = null;			// an ordered set of import URIs to search; ordered for efficiency of search
	private Map<String, ImportMapping> imports = null;

	private ConfigurationManagerForIDE configurationMgr = null;

	private HashMap<String, ConceptName> conceptNamesCache = new HashMap<String, ConceptName>();
	private HashMap<String, ConceptName> variableNamesCache = new HashMap<String, ConceptName>();
	
	private boolean deepValidationOff = false;
	private long deepValidationStartTime;
	
	private int cachingLevel = 2;	// 0 = no caching, 1 = cache variable names, 2 = cache vars and concepts
	
	private boolean hasBeenCancelled = false;

	/**
	 * This enum is for internal use to determine how to display values in
	 * TestResult on fail
	 */
	private enum DisplayType {
		ValueTableNodeDisplay, LiteralDisplay
	}

	/**
	 * Constructor; does not take any arguments
	 */
	public ModelManager() {
		setMessageManager(new MessageManager());
	}

	/**
	 * Call this method to get the instance of the Jena OntModel that has been
	 * created.
	 * 
	 * @return -- the Jena OntModel instance
	 */
	public OntModel getJenaModel() {
		return jenaModel;
	}

	/**
	 * Call this method before starting to construct a new model. Each time the
	 * parser processes a SADL file this method should be called.
	 * 
	 * @param resourceURI -- the Eclipse Resource URI--an actual URL, not a public URI
	 */
	public void init(ConfigurationManagerForIDE configMgr, URI resourceURI) {
		try {
			URI actualUrl = ResourceManager
					.convertPlatformUriToAbsoluteUri(resourceURI);
			setModelActualUrl(actualUrl);
			setConfigurationMgr(configMgr);
			if (getJenaModel() == null || alwaysCreateNew) {
				if (getJenaModel() != null) {
					// This seems to be the only way to get the enhanced node cache to not have instances left over from last creation of this model.
					getJenaModel().getDocumentManager().clearCache();
				}
				setJenaModel(ModelFactory
						.createOntologyModel(OntModelSpec.OWL_MEM));
				logger.debug("New OntModel: " + this.hashCode() + ", "
						+ (configMgr != null ? configMgr.hashCode() : null) + ", " + resourceURI.lastSegment()
						+ ", " + previousURI);
			} else if (previousURI != null && !previousURI.equals(resourceURI)) {
				setJenaModel(ModelFactory
						.createOntologyModel(OntModelSpec.OWL_MEM));
				logger.debug("Replacement OntModel: " + this.hashCode() + ", "
						+ (configMgr != null ? configMgr.hashCode() : null) + ", " + resourceURI
						+ ", " + previousURI);
			} else {
				logger.debug("No new OntModel: " + this.hashCode() + ", "
						+ (configMgr != null ? configMgr.hashCode() : null) + ", " + resourceURI
						+ ", " + previousURI);
				logger.debug("Removing all statements from model");
				getJenaModel().removeAll();
				ExtendedIterator<OntModel> eitr = getJenaModel().listSubModels();
				while (eitr.hasNext()) {
					logger.debug("Removing submodel from model");
					getJenaModel().removeSubModel(eitr.next());
				}
			}
		} catch (IllegalStateException e) {
			// Workspace is closed when we're running the JUnit tests.
			setModelActualUrl(resourceURI);
		}
		logger.info("Initializing model '{}'", getModelActualUrl());

		modelName = null;
		modelBaseUri = null;
		modelVersion = null;
		modelNamespace = null;
		owlModelsFolderPath = null;
		clearRules();
		clearSadlCommands();
		clearErrors();
		clearPendingErrors();
		if (orderedImportUris != null) {
			orderedImportUris.clear();
		}
		if (imports != null) {
			imports.clear();
		}
		if (namespacesAndPrefixes != null) {
			namespacesAndPrefixes.clear();
		}
		conceptNamesCache.clear();
		variableNamesCache.clear();
		setLastInstanceCreated(null);
		previousURI = resourceURI;
	}

	/**
	 * Call this method when you might be parsing an empty model, e.g., when
	 * performing highlighting for a new file. We will need this baseUri in
	 * order to provide an initial model name for content assist.
	 */
	public void setModelBaseUri(String modelBaseUri) {
		this.modelBaseUri = modelBaseUri;
	}

	/**
	 * Call this method to get the find out the current model baseUri. This is
	 * necessary to determine if it has already been set or not.
	 * 
	 * @return - modelBaseUri
	 */
	public String getModelBaseUri() {
		return this.modelBaseUri;
	}

	/**
	 * Call this method when inputs to the model are completed, e.g., at the end
	 * of each pass of the parser.
	 * 
	 * @return -- a List of ModelError instances (if any)
	 * @throws MalformedURLException 
	 */
	public List<ModelError> end() throws MalformedURLException {
		List<ModelError> allErrors = errors;
		if (pendingErrors != null && pendingErrors.size() > 0) {
			if (allErrors == null) {
				allErrors = new ArrayList<ModelError>();
			}
			Iterator<PendingModelError> itr = pendingErrors.values().iterator();
			while (itr.hasNext()) {
				PendingModelError pe = itr.next();
				ConceptName pename = pe.getConceptName();
				ConceptType petype = pe.getConceptType();
				// we should try to remove the concept from the model
				if (petype != null) { // .equals(ConceptType.ONTCLASS)) {
					// undo createClass call
					// OntClass oc = getOntClass(pename);
					Resource r = getJenaModel().getResource(getUri(pename));
					if (r != null) {
						getJenaModel().removeAll(r, null, null);
						getJenaModel()
								.remove(ResourceUtils.reachableClosure(r));
					}
				}
				allErrors.add(pe);
			}
		}

		try {
			if (logger.isTraceEnabled()) {
				logger.trace("Names in model '{}', this model only",
						modelNamespace);
				List<ConceptName> testConcepts = getNamedConceptsInNamedModel(getModelName(), null);
				if (testConcepts != null) {
					for (int i = 0; i < testConcepts.size(); i++) {
						logger.trace("  {}", testConcepts.get(i));
					}
				}

				logger.trace("Names in model '{}', this model plus imports",
						modelNamespace);
				testConcepts = getNamedConceptsOfType(null, Scope.INCLUDEIMPORTS);
				if (testConcepts != null) {
					for (int i = 0; i < testConcepts.size(); i++) {
						logger.trace("  {}", testConcepts.get(i));
					}
				}
			}
		} catch (InvalidNameException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		if (allErrors == null || allErrors.size() == 0) {
			// replace the cache with this model
			getJenaModel().getDocumentManager().getFileManager().addCacheModel(getModelName(), getJenaModel());
		}
		return allErrors;
	}

	/**
	 * Call this method to determine if a PendingModelError has been cleared.
	 * 
	 * @param pendingError
	 *            -- the PendingModelError to check
	 * @return -- true if cleared else false if still error
	 */
	public boolean pendingErrorCleared(PendingModelError pendingError) {
		PendingModelError pending = getPendingError(
				pendingError.getConceptName(), pendingError.getConceptType());
		if (pending != null) {
			return false;
		}
		return true;
	}

	/**
	 * Call this method to set the model name (base URI).
	 * 
	 * How ModelManager works with Jena model caches (FileManager cache and
	 * import ModelMaker cache) For a model to use concepts in an imported
	 * model, the model must be saved. Imported models always use the OWL model
	 * created on disk when a SADL model is saved.
	 * 
	 * When an import statement is encountered, the imported model and any
	 * indirectly imported models are loaded so that resources in those models
	 * will be available for name resolution. These models are cached in the
	 * import ModelMaker cache.
	 * 
	 * When a SADL model is opened in an editor, parsing of the SADL statements
	 * creates an in-memory OWL model. This in-memory OWL model is used to
	 * resolve concepts defined in the same SADL file but is not used by other
	 * models open in other editor windows unless and until it is saved. At that
	 * point a new OWL file is written to disk and the old model, if present, is
	 * removed from the cache(s).
	 * 
	 * For this to work, this class must use a different OWL model than one
	 * which might be cached in the import ModelMaker (and the FileManager??).
	 * 
	 * 
	 * @param _modelName
	 *            -- the namespace of this model (required; cannot be null)
	 * @param version
	 *            -- the model version information (optional; can be null)
	 * @param comments 
	 * @return -- errors if any else null
	 */
	public List<ModelError> setModelName(String _modelName, String version,
			String alias, List<String> comments) {
		if (_modelName == null) {
			addError(0, "Model name (uri) cannot be null.");
		}
		try {
			ResourceManager.validateHTTP_URI(_modelName);
		} catch (Exception e) {
			addError(0, "Invalid model name: " + e.getLocalizedMessage());
		}
		modelName = stripQuotes(_modelName);
		modelNamespace = ConfigurationManager
				.addHashToNonTerminatedNamespace(modelName);

		this.modelVersion = version;

		if (alias != null
				&& alias.equals(IConfigurationManagerForIDE.DEFAULTS_PREFIX)) {
			addError(2, "Alias '" + alias
					+ "' is reserved for the defaults namespace.");
		} else {
			globalPrefix = alias; // the model has been given a [global] prefix
									// to be used everywhere
		}
		ConfigurationManagerForIDE configMgr = null;
		try {
			configMgr = getConfigurationMgr();
		} catch (Exception e) {
			e.printStackTrace();
		}

		String sadlFile;
		if (getModelActualUrl() != null) {
			sadlFile = getModelActualUrl().segment(
					getModelActualUrl().segmentCount() - 1);
		} else {
			sadlFile = "<unknown>";
		}
		if (configMgr != null) {
			try {
				String existingAltUrl = configMgr
						.getAltUrlFromPublicUri(modelName);
				// 2/5/2014, awc: behavior has changed here so that the line above returns the modelName so check not equal
				if (existingAltUrl != null && getModelActualUrl() != null && !existingAltUrl.equals(modelName)) {
					String sadlFilename = ResourceManager
							.sadlFileNameOfOwlAltUrl(existingAltUrl);
					String sadlFileSameUri = ResourceManager
							.findSadlFileInProject(
									new File(
											ResourceManager
													.getOwlModelsFolder(getModelActualUrl()))
											.getParent(), sadlFilename);
					if (sadlFileSameUri != null
							&& !sadlFile.equals("<unknown>")
							&& !sadlFilename.equals(sadlFile)) {
						addError(0, "Another model (" + sadlFileSameUri
								+ ") has the same model name (uri).");
					}
				}
			} catch (ConfigurationException e) {
				// It's ok if there is no existingAltUrl with this model name.
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			try {
				getConfigurationMgr().addGlobalPrefix(modelName, globalPrefix);
				getConfigurationMgr().loadGlobalPrefixes(getJenaModel());
			} catch (ConfigurationException e) {
				addError(2, "Error setting global prefix '" + globalPrefix + "': " + e.getMessage());
			} catch (Throwable t) {
				addError(2, "Error setting global prefix '" + globalPrefix + "': " + t.getMessage());
			}
		}
		getJenaModel().setNsPrefix("", getModelNamespace());
		Ontology ontology = getJenaModel().createOntology(modelName);
		ontology.addComment("This ontology was created from a SADL file '"
				+ sadlFile + "' and should not be edited.", "en");
		if (version != null) {
			ontology.addVersionInfo(version);
		}
		if (comments != null) {
			Iterator<String> cmtsitr = comments.iterator();
			while (cmtsitr.hasNext()) {
				String comment = cmtsitr.next();
				ontology.addComment(comment, "en");
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("Model name set to '{}'", modelName);
			logger.debug("  Model namespace set to '{}'", modelNamespace);
			logger.debug("  Version: {}", version);
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to get this model's base XML namespace URI.
	 * 
	 * @return - base namespace
	 */
	public String getModelName() {
		return this.modelName;
	}

	/**
	 * Call this method to get this model's version information.
	 * 
	 * @return - model version information
	 */
	public String getModelVersionInfo() {
		return this.modelVersion;
	}

	/**
	 * Call this method for each import into the model.
	 * 
	 * @param importUri
	 *            -- the import name: for a SADL model it can be the name of the
	 *            SADL file; for SADL models or other OWL models it can be the
	 *            default namespace (public URI) of the model
	 * @param alias
	 *            -- the prefix of the imported model to be used before
	 *            localnames in this model, e.g., "prefix:Name"
	 * @return -- errors encountered or null if none
	 */
	public List<ModelError> addImports(String importUri, String alias) {
		// Must convert the importUri String to an OWL URI:
		// 1) if the uri starts with "http://" then it is a public URL and we
		// need to figure out
		// the actualUrl to add mapping info for the reader
		// 2) if the uri starts with "file://" or if it doesn't have a type (and
		// it should then
		// end in ".sadl") then look for the local sadl file and find the
		// corresponding OWL
		// file; we must then figure out the public URI to use in the
		// owl:imports in the
		// generated OWL model.
  	
    	if (importUri.startsWith(ResourceManager.FILE_URL_PREFIX)
				&& importUri.endsWith(ResourceManager.SADLEXT)) {
			importUri = importUri.substring(ResourceManager.FILE_URL_PREFIX
					.length());
		}

		URI uri = null;
		try {
			uri = URI.createURI(importUri);
		} catch (Throwable t) {
			addError(0, "Invalid import: " + t.getLocalizedMessage());
		}

		if (uri != null) {
			if (uri.isFile()) {
				// Convert the file import URI to an absolute file URI.
				uri = uri.resolve(getModelActualUrl());
				// Convert any SADL URI to an OWL URI.
				if (ResourceManager.SADLEXT.equalsIgnoreCase(uri
						.fileExtension())) {
					try {
						uri = ResourceManager
								.validateAndReturnOwlUrlOfSadlUri(uri);
					} catch (CoreException e) {
						e.printStackTrace();
						addError(0, "Error resolvling import name ("
								+ importUri + "): " + e.getLocalizedMessage());
					}
				}
				// Validate that the import URI exists.
				// Here we need the imported model's public URI, which
				// should be in the policy file.
				String publicUri = null;
				try {
					publicUri = getConfigurationMgr().getPublicUriFromActualUrl(uri.toString());
					if (publicUri == null) {
						addError(0, "A public URI for import '" + importUri	+ "' was not found. Please check name and spelling.");
					}
					if (getConfigurationMgr().validateImport(publicUri, uri.toString())) {
						addImportToModel(publicUri, alias, uri.toString());
					} else {
						logger.error(
								"Import of model '{}' into SADL model '{}' failed; model not found.",
								importUri, modelName);
						addError(0, "Import of model '" + uri.toFileString()
								+ "' into SADL model '" + importUri
								+ "' failed; model not found.");
					}
				} catch (ConfigurationException e) {
					addError(0,
							"Error finding public name of import from mapping URL '"
									+ uri + "': " + e.getMessage());
				} catch (MalformedURLException e) {
					addError(0,
							"Error finding public name of import from mapping URL '"
									+ uri + "': " + e.getMessage());
				}
			} else {
				// this is an import by publicURI; find the actual URL (altURL)
				String actualUrl = null;
				if (importUri.equals(ResourceManager.ServicesConfigurationURI)) {
					try {
						String sfn = getOwlModelsFolderPath() + File.separator
								+ "SadlServicesConfigurationConcepts.owl";
						actualUrl = SadlUtils.fileNameToFileUrl(sfn);
						boolean copied = ResourceManager
								.copyServicesConfigurationFileToOwlModelsDirectory(sfn);
						if (copied) {
							String gp = "SadlServicesConfigurationConcepts";
							getConfigurationMgr().addMapping(actualUrl,
									ResourceManager.ServicesConfigurationURI,
									gp, IConfigurationManager.SADL);
							getConfigurationMgr().addJenaMapping(
									ResourceManager.ServicesConfigurationURI,
									actualUrl);
							getConfigurationMgr().addGlobalPrefix(
									ResourceManager.ServicesConfigurationURI,
									gp);
						}
					} catch (Exception e) {
						addError(0,
								"Error copying Services file to OwlModelsFolder: "
										+ e.getMessage());
						e.printStackTrace();
					}
				} else {
					try {
						actualUrl = getConfigurationMgr()
								.getAltUrlFromPublicUri(importUri);
					} catch (ConfigurationException e) {
//						e.printStackTrace();
						addError(0,
								"Error finding actual URL of import from the public name '"
										+ importUri + "'.");
						return getErrorsFinal();
					} catch (MalformedURLException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				addImportToModel(importUri, alias, actualUrl);
			}
		}
		return getErrorsFinal();
	}

	private void addImportToModel(String publicUri, String alias,
			String actualUrl) {
		if (publicUri.equals(this.getModelName())) {
			addError(0, "A model should not import itself.");
			return;
		}
		try {
			if (alias == null) {
				alias = getConfigurationMgr().getGlobalPrefix(publicUri);
			}
			if (alias != null) {
				String aliasMapping = getJenaModel().getNsPrefixURI(alias);
				if (aliasMapping != null
						&& !aliasMapping.equals(publicUri + "#")) {
					addWarning(1, "Alias '" + alias + "', to be mapped to '"
							+ publicUri + ", is already mapped elsewhere to '"
							+ aliasMapping + "'");
				} else {
					getJenaModel().setNsPrefix(alias, publicUri + "#");
				}
				addPrefix(alias, publicUri);
			}
			
			if (publicUri.equals(IConfigurationManager.ServicesConfigurationURI)) {
				try {
					getConfigurationMgr().setServicesConfigurationAltUrlMapping();
				} catch (IOException e) {
					addError(0, e.getMessage());
				} catch (URISyntaxException e) {
					addError(0, e.getMessage());
				}
			}
			
			List<ImportMapping> impMappings = getConfigurationMgr().loadImportedModel(
					getJenaModel().getOntology(modelName), getJenaModel(),
					publicUri, actualUrl);
			logger.debug("Adding import '" + publicUri + "' to model: found " + (impMappings == null ? 0 : impMappings.size()) + " imports to add to mappings.");
			for (int i = 0; impMappings != null && i < impMappings.size(); i++) {
				ImportMapping im = impMappings.get(i);
				if (imports == null) {
					imports = new HashMap<String, ImportMapping>();
				}
				String impUri = im.getPublicURI();
				
				if (!imports.containsKey(impUri)) {
					logger.debug("   adding new mapping '" + impUri + "' with prefix '" + im.getPrefix() + "' to import mappings.");
					imports.put(impUri, im);
				}
				else {
					logger.debug("   not adding new mapping '" + impUri + "' to import mappings as it is a duplicate.");					
				}				
			}

			// Now make sure this import didn't indirectly cause this model to
			// be imported (from the OWL). If it did, remove it from the model
			// so
			// we can build it from the SADL file.
			if (getJenaModel().getImportedModel(this.getModelName()) != null) {
				getJenaModel().removeSubModel(
						getJenaModel().getImportedModel(this.getModelName()),
						false);
				logger.debug("Model '"
						+ this.getModelName()
						+ "' indirectly imports itself; removed submodel from Model under construction.");
				addError(0, "Model '"
						+ this.getModelName()
						+ "' indirectly imports itself; removed submodel from Model under construction.");
			}
		} catch (ConfigurationException e) {
			addError(0, "Unexpected error: " + e.getMessage());
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	/**
	 * Call this method to add a new class to the model. This method supports
	 * the following SADL constructs:
	 * 
	 * NewClass is a top-level class. // second argument null NewClass is a
	 * class. // second argument null (equivalent to the above, added in V2)
	 * NewClass is a type of ExistingClass.
	 * 
	 * @param newClassName
	 *            -- the name of the new class
	 * @param superClsName
	 *            -- the name of the superclass to be subclassed or null if this
	 *            is a top-level class
	 * @return -- errors encountered or null if none
	 */
	public List<ModelError> addClass(String newClassName,
			ConceptIdentifier superClsName) {
		OntClass superCls = conceptIdentifierToOntClass(1, 0, superClsName);
		Resource ncr;
		try {
			ncr = getOntResourceInExistingModel(new ConceptName(newClassName));
			if (ncr != null) {
				ConceptType nct = getOntResourceConceptType(ncr);
				if (!nct.equals(ConceptType.ONTCLASS)) {
					addError(0, "'" + newClassName + "' already exists and is of type " + nct.toString());
// The next two lines are useful in debugging problems with things appearing to already exist but shouldn't
//					getJenaModel().write(System.out, "RDF/XML-ABBREV");
//					ncr = getOntResourceInExistingModel(new ConceptName(newClassName));
				}
				else {
					if (!ncr.getNameSpace().equals(getModelNamespace())) {
						addWarning(0, "The class '" + newClassName + "' is already defined in imported model '" + ncr.getNameSpace() + 
								"' and a new class is NOT being created. If you want to create a new class in this namespace, please use this model's prefix.");
					}
					ConceptName ncn = new ConceptName(newClassName);
					ncn.setNamespace(ncr.getNameSpace());
					ncn.setType(ConceptType.ONTCLASS);
					removePendingError(ncn, ConceptType.ONTCLASS);
					if (superCls != null) {
						ncr.as(OntClass.class).addSuperClass(superCls);
					}
				}
			}
			else {
				OntClass cls = null;
				try {
					cls = getJenaModel().createClass(getUri(newClassName));
				} catch (PrefixNotFoundException e) {
					addError(0, "Failed to create class '" + newClassName + "': "
							+ e.getMessage());
				}
				if (cls != null) {
					if (superCls != null) {
						cls.addSuperClass(superCls);
					}
					ConceptName ncn = new ConceptName(newClassName);
					ncn.setNamespace(cls.getNameSpace());
					ncn.setType(ConceptType.ONTCLASS);
					removePendingError(ncn, ConceptType.ONTCLASS);
				} else {
					addError(0, "Failed to create class '" + newClassName + "'");
				}
			}
		} catch (ConfigurationException e1) {
			addError(0, "Unexpected failure creating class '" + newClassName + "': " + e1.getMessage());
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to create an equivalent class to a given class. This
	 * method supports the following SADL constructs:
	 * 
	 * NewClass is ...., must be one of {Instance1, Instance2, Instance3, ...}.
	 * // second argument is SadlEnumeratedClass NewClass is the same as
	 * {ExistingClass1 and ExistingClass2 and...}. // second argument is
	 * SadlIntersectionClass NewClass is the same as {ExistingClass1 or
	 * ExistingClass2 or ...}. // second argument is SadlUnionClass NewClass is
	 * the same as {ExistingClass1, ExistingClass2, ..}. // alternate form for
	 * SadlUnionClass
	 * 
	 * @param className
	 *            -- name of class to have new equivalent class
	 * @param equivalentClass
	 *            -- descriptor of new equivalent class
	 * @return
	 */
	public List<ModelError> addEquivalentClass(ConceptName className,
			ConceptIdentifier equivalentClass) {
		OntClass cls = getOntClass(className);
		if (cls == null) {
			cls = getOrCreateOntClass(0, 0, className);
		}
		if (cls != null) {
			OntClass equivalentCls = null;
			if (equivalentClass instanceof SadlEnumeratedClass) {
				equivalentCls = createEnumeratedClass(1, className,
						(SadlEnumeratedClass) equivalentClass);
			} else {
				equivalentCls = conceptIdentifierToOntClass(1, 0,
						equivalentClass);
			}
			if (equivalentCls != null) {
				cls.addEquivalentClass(equivalentCls);
			} else {
				addError(1, "Failed to create equivalent class '"
						+ equivalentClass.toString() + "'");
			}
			className.setType(ConceptType.ONTCLASS);
			className.setNamespace(className.getNamespace());
			removePendingError(className, ConceptType.ONTCLASS);
		}
		return getErrorsFinal();
	}

	private OntClass getOntClass(ConceptName className) {
		Resource ir;
		try {
			ir = getOntResourceInExistingModel(className);
			if (ir != null) {
				ConceptType it = getOntResourceConceptType(ir);
				if (it.equals(ConceptType.ONTCLASS)) {
					return ir.as(OntClass.class);
				}
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	private OntProperty getOntProperty(ConceptName propName) {
		Resource ir;
		try {
			ir = getOntResourceInExistingModel(propName);
			if (ir != null) {
				if (logger.isDebugEnabled()) {
					exploreProperty(ir.as(OntProperty.class));
				}

				ConceptType it = getOntResourceConceptType(ir);
				if (it.equals(ConceptType.ANNOTATIONPROPERTY)) {
					return ir.as(AnnotationProperty.class);
				}
				else if (it.equals(ConceptType.OBJECTPROPERTY)) {
					return ir.as(ObjectProperty.class);
				}
				else if (it.equals(ConceptType.DATATYPEPROPERTY)) {
					return ir.as(DatatypeProperty.class);
				}
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	private Individual getIndividual(ConceptName instName) {
		Resource ir;
		try {
			ir = getOntResourceInExistingModel(instName);
			if (ir != null) {
				ConceptType it = getOntResourceConceptType(ir);
				if (it.equals(ConceptType.INDIVIDUAL)) {
					return ir.as(Individual.class);
				}
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
		
	/**
	 * Call this method to create an instance of a specified class.
	 * 
	 * @param instName
	 *            -- name of the new instance
	 * @param clsName
	 *            -- name of the class to which the new instance belongs
	 * @return -- errors encountered or null if none
	 */
	public List<ModelError> addInstance(String instName,
			ConceptIdentifier clsName) {
		Resource ir;
		try {
			boolean instNameOK = true;
			ConceptName nin = new ConceptName(instName);
			ir = getOntResourceInExistingModel(nin);
			if (ir != null) {
				ConceptType it = getOntResourceConceptType(ir);
				if (!it.equals(ConceptType.INDIVIDUAL)) {
					addError(0, "'" + instName + "' already exists and is of type " + it.toString());
					instNameOK = false;
				}
			}
			OntClass cls = conceptIdentifierToOntClass(1, 0, clsName);
			if (cls != null) {
				if (instNameOK) {
					Individual inst;
					try {
						inst = getJenaModel().createIndividual(getUri(instName),
								cls);
						setLastInstanceCreated(inst);
						if (inst == null) {
							addError(0, ExistingNamePart.NAME,
									"Unable to create instance '" + instName + "'");
						} else if (instName != null) {
							nin.setNamespace(inst.getNameSpace());
							removePendingError(nin, ConceptType.INDIVIDUAL);
						}
					} catch (PrefixNotFoundException e) {
						addError(0, "Unable to create instance '" + instName
								+ "': " + e.getMessage());
					}
				}
			} else {
				addError(1, "Unable to determine the type (class) of '"
						+ instName + "'");
			}
		} catch (ConfigurationException e1) {
			addError(0, "Unexpected error creating instance '" + instName + "': " + e1.getMessage());
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to add a blank node to the model.
	 * 
	 * @param clsName
	 *            -- identifier of the concept to be added
	 * @param attributes
	 *            -- identifying attributes of the blank node
	 * @return -- errors encountered or null if none
	 */
	public List<ModelError> addBNodeInstance(ConceptIdentifier clsName,
			List<Object[]> attributes) {
		OntClass cls = conceptIdentifierToOntClass(1, 0, clsName);
		if (cls != null) {
			Individual subj = getJenaModel().createIndividual(null, cls);
			setLastInstanceCreated(subj);
			if (subj == null) {
				addError(0, ExistingNamePart.NAME,
						"Unable to create bnode instance");
			} else if (attributes != null) {
				Iterator<Object[]> itr = attributes.iterator();
				int cnt = 0;
				while (itr.hasNext()) {
					Object[] nvp = itr.next();
					ConceptName propName = (ConceptName) nvp[0];
					Object inputVal = nvp[1];
					if (inputVal instanceof Individual
							|| inputVal instanceof ConceptName) {
						Individual obj = null;
						if (inputVal instanceof Individual) {
							obj = (Individual) inputVal;
						} else if (inputVal instanceof ConceptName) {
							ConceptName objName = (ConceptName) inputVal;
							obj = getOrCreateIndividual(3, 0, objName);
						}
						ObjectProperty prop = getOrCreateObjectProperty(1, 0,
								propName);
						if (subj != null && prop != null && obj != null) {
							addPendingValidationStatement(subj, prop, obj);
							getJenaModel().add(subj, prop, obj);
						}
					} else {
						OntProperty prop = null;
						// If this property doesn't exist, we can't tell whether it is intended to be a DatatypeProperty
						//	or an AnnotationProperty so this won't work to just call getOrCreate....
						Resource r = null;
						try {
							r = getOntResourceInExistingModel(propName);
						} catch (ConfigurationException e1) {
							addError(1, determineExistingNamePartOfError(propName),
									"Error finding property '" + propName.toString()
											+ "': " + e1.getMessage());
						}
						if (r == null) {
							addWarning(1, "Property '" + propName.toString() + "' not defined. Assuming a data type and not an annotation property.");
							prop = getOrCreateDataProperty(1, 0, propName);
						}
						else {
							ConceptType propType = getOntResourceConceptType(r);
							if (!propType.equals(ConceptType.DATATYPEPROPERTY)) {
								if (propType
										.equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
									addError(
											1,
											determineExistingNamePartOfError(propName),
											"'" + propName.toString()
													+ "' is not a known property.");
								} else {
									addError(1, ExistingNamePart.BOTH, "'"
											+ propName.toString()
											+ "' is not a property (but is a '"
											+ propType.toString() + "'");
								}
							}
							if (errors() < 1) {
								prop = r.as(OntProperty.class);
								Literal val = null;
								if (prop.isAnnotationProperty()) {
									if (inputVal instanceof String) {
										inputVal = stripQuotes((String) inputVal);
									}
									val = getJenaModel().createTypedLiteral(
											inputVal);
								} else {
									try {
										val = UtilsForJena
												.getLiteralMatchingDataPropertyRange(
														getJenaModel(), prop,
														inputVal);
									} catch (Exception e) {
										e.printStackTrace();
										addError(1, cnt,
												ExistingNamePart.NOTAPPLICABLE,
												"Error creating literal from '"
														+ inputVal.toString() + "': ("
														+ e.getClass().getSimpleName() + ") " + e.getMessage());
									}
								}
	
								if (errors() < 1 && val != null) {
									subj.addProperty(prop, val);
								}
							}
						}

					}
					cnt++;
				}
			}
		}
		return getErrorsFinal();
	}

	/**
	 * Method to check an instance statement to make sure it is valid.
	 * 
	 * @param or - subject
	 * @param prop - predicate
	 * @param val - object
	 * @return
	 * @throws InvalidNameException 
	 */
	public boolean validateStatement(Node s, Node p, Node v) throws InvalidNameException {
		try {
			Resource sr = getOntResourceInExistingModel(new ConceptName(s.toString()));
			if (sr != null) {
				ConceptType st = getOntResourceConceptType(sr);
				if (st.equals(ConceptType.INDIVIDUAL)) {
					Resource pr = getOntResourceInExistingModel(new ConceptName(p.toString()));
					if (pr != null) {
						ConceptType pt = getOntResourceConceptType(pr);
						if (pt.equals(ConceptType.OBJECTPROPERTY) || pt.equals(ConceptType.DATATYPEPROPERTY)) {
							RDFNode obj = null;
							if (v instanceof com.ge.research.sadl.model.gp.Literal) {
								obj = getJenaModel().createTypedLiteral(((com.ge.research.sadl.model.gp.Literal)v).getValue());
							}
							else if (pr.equals(RDF.type)) {
								Resource cr = getOntResourceInExistingModel(new ConceptName(v.toString()));
								if (cr != null) {
									ConceptType ct = getOntResourceConceptType(cr);
									if (!ct.equals(ConceptType.ONTCLASS)) {
										return false;
									}
									return true;
								}
							}
							else {
								if (v.toString().equals(KnownNode.value)) {
									obj = null;
								}
								else {
									Resource or = getOntResourceInExistingModel(new ConceptName(v.toString()));
									if (or != null) {
										ConceptType ot = getOntResourceConceptType(or);
										if (!ot.equals(ConceptType.INDIVIDUAL)) {
											return false;
										}
										else {
											obj = or.as(Individual.class);
										}
									}
								}
							}
							if (sr != null && pr != null) {
								return validateStatement(sr.as(OntResource.class), pr.as(OntProperty.class), obj);
							}
						}
					}
				}
			}
		} catch (ConfigurationException e) {
			addError(0, "Unexpected error: " + e.getMessage());
			e.printStackTrace();
		}
		return false;
	}
	
	private void addPendingValidationStatement(OntResource or, OntProperty prop, RDFNode val) {
		pendingValidationStatements.add(new ValidationStatement(or, prop, val));
	}
	
	/**
	 * Method to add the SADL model object to the ValidationStatements which are pending and move them to the queued list
	 * @param eo -- the SADL model object with which these ValidationStatements are to be associated
	 * @return -- the number of statements moved
	 */
	public int movePendingValidationStatementsToQueuedValidationStatements(Object eo) {
		int numVs = pendingValidationStatements.size();
		for (int i = 0; i < numVs; i++) {
			ValidationStatement vs = pendingValidationStatements.get(i);
			vs.setModelEObject(eo);
			queuedValidationStatements.add(vs);
		}
		pendingValidationStatements.clear();
		return numVs;
	}
	
	/**
	 * Method to perform the actual validation of the queued up ValidationStatements
	 * @return --  a Map of SADL model objects and associated ModelErrors
	 */
	public Map<Object, List<ModelError>> validateStatements() {
		Map<Object, List<ModelError>> errorMap = null;
		int numVs = queuedValidationStatements.size();
		if (numVs > 0) {
			errorMap = new HashMap<Object, List<ModelError>>();
			for (int i = 0; i < numVs; i++) {
				ValidationStatement vs = queuedValidationStatements.get(i);
				if (!validateStatement(vs.getSubject(), vs.getProperty(), vs.getObject())) {
					List<ModelError> errors = getErrorsFinal();
					errorMap.put(vs.getModelEObject(), errors);
				}
			}
		}
		queuedValidationStatements.clear();
		return errorMap;
	}
	
	/**
	 * Method to actually do the work of validating the statements
	 * @param or
	 * @param prop
	 * @param val
	 * @return
	 */
	private boolean validateStatement(OntResource or, OntProperty prop,
			RDFNode val) {
		boolean retval = true;
		if (beginDeepValidation() ) {
			try {
				if (!prop.isAnnotationProperty()) {
					OntResource dor = getDomain(prop);
					if (dor != null && !instanceBelongsToClass(getJenaModel(), or, dor)) {
						addValidationWarning(or, dor,
								"is not in domain (" + classToString(dor)
										+ ") of property " + prop.getLocalName());
						retval = false;
					}
					if (val != null) {
						OntResource ror = getRange(prop);
						if (ror == null) {
							addWarning(1, prop.toString() + " does not have a range.");
							retval = false;
						}
						else if (val instanceof OntResource) {
							if (ror != null
									&& !instanceBelongsToClass(getJenaModel(), (OntResource) val, ror)) {
								addValidationWarning(
										(OntResource) val,
										ror,
										"is not in the range (" + classToString(ror)
												+ ") of property "
												+ prop.getLocalName());
								retval = false;
							}
						} else if (val instanceof Literal) {
							// no need to look for pending errors here...
							if (!literalValueCompatibleWithRange(ror, (Literal)val)) {
								addError(new ModelError(val.toString()
										+ " is not in range (" + classToString(ror)
										+ ") of property " + prop.getLocalName(),
										ErrorType.WARNING));
								retval = false;
							}
						}
					}
				}
			} catch (Throwable t) {
				if (t.getMessage() == null) {
					t.printStackTrace();
				}
				logger.error("Failed to validate statement: "
						+ t.getLocalizedMessage());
			}
			endDeepValidation();
		}
		return retval;
	}

	private boolean literalValueCompatibleWithRange(OntResource ror, Literal val) {
		String dturi = val.getDatatypeURI();
		if (ror.getURI().equals(dturi)) {
			return true;
		}
		else if (ror.getURI().equals(XSD.xfloat.getURI()) && dturi.equals(XSD.xdouble.getURI())) {
			return true;
		}
		else if (ror.getURI().equals(XSD.xdouble.getURI()) && dturi.equals(XSD.xfloat.getURI())) {
			return true;
		}
		return false;
	}

	private String classToString(OntResource cls) {
		if (!cls.isAnon()) {
			return cls.getLocalName();
		} else {
			StringBuilder sb = new StringBuilder();
			if (cls.canAs(UnionClass.class)) {
				sb.append("{");
				try {
					UnionClass ucls = cls.as(UnionClass.class);
					int cnt = 0;
					ExtendedIterator<? extends OntClass> eitr = ucls
							.listOperands();
					while (eitr.hasNext()) {
						Object uclsObj = eitr.next();
						OntClass uclsmember = (OntClass) uclsObj;
						if (cnt++ > 0) {
							sb.append(" or ");
						}
						sb.append(classToString(uclsmember));
					}
					sb.append("}");
				} catch (Exception e) {
					// don't know why this is happening
					logger.error("Union class error that hasn't been resolved or understood.");
					e.printStackTrace();
				}
			} else if (cls.canAs(IntersectionClass.class)) {
				sb.append("{");
				try {
					IntersectionClass icls = cls.as(IntersectionClass.class);
					int cnt = 0;
					ExtendedIterator<? extends OntClass> eitr = icls
							.listOperands();
					while (eitr.hasNext()) {
						OntClass iclsmember = eitr.next();
						if (cnt > 0) {
							sb.append(" and ");
						}
						sb.append(classToString(iclsmember)); //.getLocalName());
						cnt++;
					}
					sb.append("}");
				} catch (Exception e) {
					// don't know why this is happening
					logger.error("Intersection class error that hasn't been resolved or understood.");
				}
			} else if (cls.canAs(Restriction.class)) {
				sb.append("(");
				Restriction rest = cls.as(Restriction.class);
				sb.append("Restriction on property '" + rest.getOnProperty().getLocalName() + "'");
				sb.append(")");
			} else {
				logger.error("Class is neither URI nor Union nor Intersection: "
						+ cls.toString());
				StmtIterator sitr = getJenaModel().listStatements(cls,
						(Property) null, (RDFNode) null);
				while (sitr.hasNext()) {
					logger.error("    " + sitr.nextStatement().toString());
				}
				sb.append("Unexpect class (not URI, Union, or Intersection) "
						+ cls.toString());
			}
			return sb.toString();
		}
	}

	/**
	 * return true if the instance belongs to the class else return false
	 * 
	 * @param inst
	 * @param cls
	 * @return
	 */
	public static boolean instanceBelongsToClass(OntModel m, OntResource inst, OntResource cls) {
		// The following cases must be considered:
		// 1) The class is a union of other classes. Check to see if the instance is a member of any of
		//		the union classes and if so return true.
		// 2) The class is an intersection of other classes. Check to see if the instance is 
		//		a member of each class in the intersection and if so return true.
		// 3) The class is neither a union nor an intersection. If the instance belongs to the class return true. Otherwise
		//		check to see if the instance belongs to a subclass of the classlse
		//		return false. (Superclasses do not need to be considered because even if the instance belongs to a super
		//		class that does not tell us that it belongs to the class.)
		
		/*
		 * e.g., 	Internet is a Network.
		 * 			Network is a type of Subsystem.
		 * 			Subsystem is type of System.
		 */
		if (cls.canAs(UnionClass.class)) {
			List<OntResource> uclses = getOntResourcesInUnionClass(m, cls.as(UnionClass.class));	
			for (int i = 0; i < uclses.size(); i++) {
				OntResource ucls = uclses.get(i);
				if (instanceBelongsToClass(m, inst, ucls)) {
					return true;
				}
			}
		}
		else if (cls.canAs(IntersectionClass.class)) {
			List<OntResource> uclses = getOntResourcesInIntersectionClass(m, cls.as(IntersectionClass.class));	
			for (int i = 0; i < uclses.size(); i++) {
				OntResource ucls = uclses.get(i);
				if (!instanceBelongsToClass(m, inst, ucls)) {
					return false;
				}
			}
			return true;
		}
		else if (cls.canAs(Restriction.class)) {
			Restriction rest = cls.as(Restriction.class);
			OntProperty ontp = rest.getOnProperty();				
			if (rest.isAllValuesFromRestriction()) {
				StmtIterator siter = inst.listProperties(ontp);
				while (siter.hasNext()) {
					Statement stmt = siter.nextStatement();
					RDFNode obj = stmt.getObject();
					if (obj.canAs(Individual.class)) {
						Resource avfc = rest.asAllValuesFromRestriction().getAllValuesFrom();
						if (!instanceBelongsToClass(m, (Individual)obj.as(Individual.class), (OntResource)avfc.as(OntResource.class))) {
							return false;
						}
					}
				}
			}
			else if (rest.isSomeValuesFromRestriction()) {
				if (inst.hasProperty(ontp)) {
					return true;
				}
			}
			else if (rest.isHasValueRestriction()) {
				RDFNode hval = rest.as(HasValueRestriction.class).getHasValue();
				if (inst.hasProperty(ontp, hval)) {
					return true;
				}
			}
			else if (rest.isCardinalityRestriction()) {
				
			}
			else if (rest.isMaxCardinalityRestriction()) {
				
			}
			else if (rest.isMinCardinalityRestriction()) {
				
			}
		}
		else {
			if (inst.canAs(Individual.class)) {
				ExtendedIterator<Resource> eitr = inst.asIndividual().listRDFTypes(false);
				while (eitr.hasNext()) {
					Resource r = eitr.next();				
					OntResource or = m.getOntResource(r);
					if (or.isURIResource()) {
						OntClass oc = m.getOntClass(or.getURI());
						if (classIsSubclassOf(oc, cls, true)) {
							eitr.close();
							return true;
						}
					}
					else if (or.canAs(OntClass.class)) {
						if (classIsSubclassOf(or.as(OntClass.class), cls, true)) {
							eitr.close();
							return true;
						}
					}
				}
			}
		}
		return false;
	}
	
	private static List<OntResource> getOntResourcesInUnionClass(OntModel m, UnionClass ucls) {
		List<OntResource> results = new ArrayList<OntResource>();
		List<RDFNode> clses = ucls.getOperands().asJavaList();
		for (int i = 0; i < clses.size(); i++) {
			RDFNode mcls = clses.get(i);
			if (mcls.canAs(OntResource.class)) {
				results.add(mcls.as(OntResource.class));
			}
		}
		return results;
	}
	
	private static List<OntResource> getOntResourcesInIntersectionClass(OntModel m, IntersectionClass icls) {
		List<OntResource> results = new ArrayList<OntResource>();
		List<RDFNode> clses = icls.getOperands().asJavaList();
		for (int i = 0; i < clses.size(); i++) {
			RDFNode mcls = clses.get(i);
			if (mcls.canAs(OntResource.class)) {
				results.add(mcls.as(OntResource.class));
			}
		}
		return results;
	}

	/**
	 * return true if the first argument class is a subclass of the second
	 * argument class
	 * 
	 * @param subcls
	 * @param cls
	 * @return
	 */
	public static boolean classIsSubclassOf(OntClass subcls, OntResource cls, boolean rootCall) {
		if (subcls == null || cls == null) {
			return false;
		}
		if (cls.isURIResource() && subcls.isURIResource()
				&& cls.getURI().equals(subcls.getURI())) {
			return true;
		}
		if (cls.isAnon()) {
			if (cls.canAs(OntClass.class)) {
				OntClass ocls = cls.as(OntClass.class);
				if (ocls.isUnionClass()) {
					UnionClass ucls = cls.as(UnionClass.class);
					try {
						ExtendedIterator<? extends OntClass> eitr = ucls
								.listOperands();
						while (eitr.hasNext()) {
							OntClass uclsmember = eitr.next();
							if (classIsSubclassOf(subcls, uclsmember, false)) {
								eitr.close();
								return true;
							}
						}
					}
					catch (Exception e) {
						logger.error("Unexpected error during deep validation: apparent Union Class does not return operands.");
					}
				}
			}
		}
		try {
			if (cls.canAs(OntClass.class)) {
				ExtendedIterator<OntClass> eitr = cls.as(OntClass.class).listSubClasses();
				while (eitr.hasNext()) {
					OntClass subCls = eitr.next();
					if (subCls.equals(subcls)) {
						eitr.close();
						return true;
					}
					else {
						if (classIsSubclassOf(subcls, subCls, false)) {
							eitr.close();
							return true;
						}
					}
				}
				eitr.close();
//				if (rootCall && classIsSuperClassOf(cls.as(OntClass.class), subcls)) {
//					return true;
//				}
			}
			if (subcls.isAnon()) {
				if (subcls.isIntersectionClass()) {
					IntersectionClass icls = subcls.asIntersectionClass();
					try {
						ExtendedIterator<? extends OntClass> eitr = icls.listOperands();
						while (eitr.hasNext()) {
							OntClass iclsmember = eitr.next();
							if (classIsSubclassOf(cls.as(OntClass.class), iclsmember, false)) {
								eitr.close();
								return true;
							}
						}
					}
					catch (Exception e) {
						logger.error("Unexpected error during deep validation: apparent Intersection Class does not return operands.");
					}
				}
			}
//			if (subcls.hasSuperClass(cls, false)) {  // this doesn't work, don't know why awc 6/8/2012
//				return true;
//			}
		} catch (Throwable t) {
			t.printStackTrace();
		}
		return false;
	}

	private static boolean classIsSuperClassOf(OntClass cls, OntClass subcls) {
		ExtendedIterator<OntClass> eitr = subcls.listSuperClasses();
		try {
			while (eitr.hasNext()) {
				OntClass sprcls = eitr.next();
				if (sprcls.equals(cls)) {
					return true;
				}
				if (classIsSuperClassOf(cls, sprcls)) {
					return true;
				}
			}
			eitr.close();
			
			eitr = cls.listSuperClasses();
			while (eitr.hasNext()) {
				OntClass equivCls = eitr.next();
				if (classIsSubclassOf(subcls, equivCls, false)) {
					eitr.close();
					return true;
				}
			}
		}
		catch (Throwable t) {
			logger.error("Error checking if class '" + cls.toString() + "' is a superclass of '" + subcls.toString() + 
					"' : " + t.getMessage());
		}
		finally {
			eitr.close();
		}
		return false;
	}

	private void addValidationWarning(OntResource instance, OntResource clss,
			String msg) {
		if (!instance.isAnon()) {
			ConceptName valcn = new ConceptName(
					((OntResource) instance).getLocalName());
			valcn.setNamespace(((OntResource) instance).getNameSpace());
			PendingModelError pe = getPendingError(valcn,
					ConceptType.INDIVIDUAL);
			if (pe != null) {
				// add an additional check to the PendingModelError to make sure
				// it
				AdditionalCheck addChk = pe.new AdditionalCheck(instance, clss,
						msg, ErrorType.WARNING);
				pe.addAdditionalCheck(addChk);
				return;
			}
		}
		addError(new ModelError((instance.isURIResource() ? instance.getLocalName() : "<unnamed>") + " " + msg,
				ErrorType.WARNING));
	}

	private OntResource getDomain(OntProperty prop) {
		OntResource domain = prop.getDomain();
		if (domain == null) {
			if (prop.canAs(ObjectProperty.class)) {
				ExtendedIterator<? extends OntProperty> titr = ((ObjectProperty) prop
						.as(ObjectProperty.class)).listSuperProperties(true);
				while (titr.hasNext()) {
					OntProperty tr = titr.next();
					domain = getDomain((OntProperty) tr);
					if (domain != null) {
						titr.close();
						return domain;
					}
				}
				titr.close();
			}
		}
		return domain;
	}

	private OntResource getRange(OntProperty prop) {
		OntResource range = prop.getRange();
		if (range == null) {
			ExtendedIterator<? extends OntProperty> titr = null;
			if (prop.canAs(ObjectProperty.class)) {
				titr = ((ObjectProperty) prop.as(ObjectProperty.class))
						.listSuperProperties(true);
			} else if (prop.canAs(DatatypeProperty.class)) {
				titr = ((DatatypeProperty) prop.as(DatatypeProperty.class))
						.listSuperProperties(true);
			}
			if (titr != null) {
				while (titr.hasNext()) {
					OntProperty tr = titr.next();
					range = getRange((OntProperty) tr);
					if (range != null) {
						titr.close();
						return range;
					}
				}
				titr.close();
			}
		}
		return range;
	}

	/**
	 * Call this method to create a class as the complement of another class
	 * 
	 * @param ci1
	 * @param ci2
	 * @return
	 */
	public List<ModelError> addComplementOfClass(ConceptName ci1,
			ConceptIdentifier ci2) {
		OntClass c2 = conceptIdentifierToOntClass(1, 0, ci2);
		getJenaModel().createComplementClass(getUri(ci1), c2);
		return getErrorsFinal();
	}

	public void doAdditionalChecks(PendingModelError pe) {
		if (pe.getAdditionalChecks() != null) {
			List<AdditionalCheck> addlChks = pe.getAdditionalChecks();
			for (int i = 0; i < addlChks.size(); i++) {
				AdditionalCheck addChk = addlChks.get(i);
				if (addChk.getInstance() != null
						&& !instanceBelongsToClass(getJenaModel(), addChk.getInstance(),
								addChk.getRequiredClass())) {
					String msg = addChk.getInstance().getLocalName() + " "
							+ addChk.getMessage();
					ModelError newME = new ModelError(msg, addChk.getSeverity());
					newME.setLineLength(pe.getLineLength());
					newME.setLineNumber(pe.getLineNumber());
					newME.setOffset(pe.getOffset());
					addError(newME);
				}
			}
		}
	}

	public List<ModelError> addDefaultValue(ConceptIdentifier clsname,
			ConceptName propname, Object defVal, int level) {
		// make sure the default model is imported
		try {
			checkDefaultsModelImported();
		} catch (Exception e) {
			addError(
					1,
					"Unexpected error adding defaults import: "
							+ e.getMessage());
			e.printStackTrace();
		}

		OntClass cls = ontClassInThisModel(conceptIdentifierToOntClass(0, 0, clsname));
		OntProperty prop = null;
		ConceptType ptype = null;
		Resource pr;
		try {
			pr = getOntResourceInExistingModel(propname);
			if (pr != null) {
				ptype = getOntResourceConceptType(pr);
				if (ptype.equals(ConceptType.OBJECTPROPERTY)
						&& defVal instanceof ConceptName) {
					prop = pr.as(ObjectProperty.class);
				} else if (ptype.equals(ConceptType.DATATYPEPROPERTY)
						&& !(defVal instanceof ConceptName)) {
					prop = pr.as(DatatypeProperty.class);
				} else if (ptype.equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
					if (defVal instanceof ConceptName) {
						prop = getOrCreateObjectProperty(1, 0, propname);
						ptype = ConceptType.OBJECTPROPERTY;
					} else {
						prop = getOrCreateDataProperty(1, 0, propname);
						ptype = ConceptType.DATATYPEPROPERTY;
					}
				}
			}
			else {
				if (defVal instanceof ConceptName) {
					prop = getJenaModel().createObjectProperty(getUri( propname));
					ptype = ConceptType.OBJECTPROPERTY;
					propname.setNamespace(prop.getNameSpace());
					addPendingError(1, 0,
							ConceptType.OBJECTPROPERTY,
							propname,
							"Property '" + propname.toString()
									+ "' not found. " + getModelReference());
				}
				else {
					prop = getJenaModel().createDatatypeProperty(getUri(propname));
					ptype = ConceptType.DATATYPEPROPERTY;
					propname.setNamespace(prop.getNameSpace());
					addPendingError(1, 0,
							ConceptType.DATATYPEPROPERTY,
							propname,
							"Property '" + propname.toString()
									+ "' not found. " + getModelReference());
				}
			}
		} catch (ConfigurationException e1) {
			addError(1, "Unexpected error finding default value property '" + propname + "': " + e1.getMessage());
		}
		Individual seeAlsoDefault = null;
		if (ptype.equals(ConceptType.OBJECTPROPERTY)) {
			if (!(defVal instanceof ConceptName)) {
				addError(2, "Error creating default for property '" + propname + "' for class '" + clsname + "' with value '" + defVal.toString()
						+ "': the value is not a named concept.");
			}
			else {
				Individual defInst = getOrCreateIndividual(3, 0,
						(ConceptName) defVal);
				try {
					seeAlsoDefault = createDefault(cls, prop, defInst, level);
				} catch (Exception e) {
					addError(2, "Error creating default for property '" + prop.toString() + "' for class '" + cls.toString() + "' with value '" + defVal.toString()
							+ "': " + e.getMessage());
				}
			}
		} else {
			if (defVal instanceof ConceptName) {
				addError(2, "Error creating default for property '" + propname + "' for class '" + clsname + "' with value '" + defVal.toString()
						+ "': the value is a named concept but should be a data value.");
			}
			else {
				Literal litVal;
				try {
					litVal = UtilsForJena
							.getLiteralMatchingDataPropertyRange(getJenaModel(),
									prop, defVal);
					try {
						seeAlsoDefault = createDefault(cls, prop, litVal, level);
					} catch (Exception e) {
						addError(2, "Error creating default for property '" + prop.toString() + "' for class '" + cls.toString() + "' with value '" + defVal.toString()
								+ "': " + e.getMessage());
					}
				} catch (Exception e) {
					if (prop.getRange() == null) {
						addWarning(2, "Failed to create literal as range is unknown at this point. Move property definition ahead of this statement?");
					}
					else {
						addError(2, "Error creating literal from '"
								+ defVal.toString() + "': ("
								+ e.getClass().getSimpleName() + ") " + e.getMessage());
					}
				}
			}
		}
		if (seeAlsoDefault != null) {
			cls.addSeeAlso(seeAlsoDefault);
		} else {
			addError(3, "Unable to create default for '" + clsname + "', '"
					+ propname + "', '" + defVal + "'");
		}
		return getErrorsFinal();
	}

	private OntClass ontClassInThisModel(OntClass cls) {
		// make sure we have the class in this model
		if (cls != null) {
			if (!cls.isAnon()) {
				cls = getJenaModel().createClass(cls.getURI());
			}
			else {
				OntResource newor = getJenaModel().getOntResource(cls);
				if (newor.canAs(OntClass.class)) {
					cls = newor.as(OntClass.class);
				}
			}
		}
		return cls;
	}

	/**
	 * Call this method to declare the listed Individuals to be known to be
	 * different
	 * 
	 * @param instances
	 * @return
	 */
	public List<ModelError> addDifferentInstances(List<ConceptName> instances) {
		RDFNode[] nodeArray = new RDFNode[instances.size()];
		int idx = 0;
		Iterator<ConceptName> itr = instances.iterator();
		while (itr.hasNext()) {
			ConceptName nme = itr.next();
			if (nme == null) {
				addError(0, "Instance " + (idx + 1)
						+ " is not specified; can't set all different.");
				return getErrorsFinal();
			}
			Individual inst = getOrCreateIndividual(0, idx + 1, nme);
			if (inst == null) {
				addError(0, "Instance " + (idx + 1)
						+ " is not specified; can't set all different.");
				return getErrorsFinal();
			}
			nodeArray[idx++] = inst;

		}
		RDFList differentMembers = getJenaModel().createList(nodeArray);
		getJenaModel().createAllDifferent(differentMembers);
		return getErrorsFinal();
	}

	/**
	 * Call this method to declare that two classes are disjoint
	 * 
	 * @param cl1
	 * @param cl2
	 * @return
	 */
	public List<ModelError> addDisjointClasses(ConceptName cl1,
			ConceptIdentifier cl2) {
		if (cl1 == null) {
			addError(0, "First of two disjoint classes is not identified.");
		} else if (cl2 == null) {
			addError(1, "Second of two disjoint classes is not identified.");
		} else {
			OntClass c1 = getOrCreateOntClass(0, 0, cl1);
			OntClass c2 = conceptIdentifierToOntClass(1, 0, cl2);
			if (c1 == null) {
				addError(0, "Unable to find class '" + cl1.toString() + "'");
			}
			if (c2 == null) {
				addError(1, "Unable to find class '" + cl2.toString() + "'");
			}
			if (c1 != null && c2 != null) {
				c1.addDisjointWith(c2);
			}
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to declare that two classes are equivalent
	 * 
	 * @param cl1
	 * @param cl2
	 * @return
	 */
	public List<ModelError> addEquivalentConcepts(ConceptIdentifier cl1,
			ConceptIdentifier cl2) {
		ConceptType ct1 = ConceptType.ONTCLASS; // default type
		ConceptType ct2 = ConceptType.ONTCLASS; // default type

		if (cl1 instanceof ConceptName) {
			cl1 = validateConceptName((ConceptName) cl1);
			ct1 = ((ConceptName) cl1).getType();
		}
		if (cl2 instanceof ConceptName) {
			cl2 = validateConceptName((ConceptName) cl2);
			ct2 = ((ConceptName) cl2).getType();
		}

		if (ct1.equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)
				|| ct2.equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
			if (ct1.equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
				if (cl1 instanceof ConceptName) {
					ct1 = ConceptType.ONTCLASS;
				} else {
					addError(0, "Concept '" + cl1 + "' not found.");
				}
			}
			if (ct2.equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
				if (cl2 instanceof ConceptName) {
					ct2 = ConceptType.ONTCLASS;
				} else {
					addError(1, "Concept '" + cl2 + "' not found.");
				}
			}
		}
		if (!ct1.equals(ct2)) {
			addError(0, "Concepts are not of the same type.");
			return getErrorsFinal();
		}
		if (ct1.equals(ConceptType.ONTCLASS)) {
			OntClass c1 = ontClassInThisModel(conceptIdentifierToOntClass(0, 0, cl1));
			OntClass c2 = conceptIdentifierToOntClass(1, 0, cl2);
			if (c1 != null && c2 != null) {
				c1.addEquivalentClass(c2);
			}
		} else if (ct1.equals(ConceptType.OBJECTPROPERTY)) {
			OntProperty p1 = getOrCreateObjectProperty(0, 0, cl1);
			OntProperty p2 = getOrCreateObjectProperty(1, 0, cl2);
			if (p1 != null && p2 != null) {
				p1.addEquivalentProperty(p2);
			}
		} else if (ct1.equals(ConceptType.DATATYPEPROPERTY)) {
			OntProperty p1 = getOrCreateDataProperty(0, 0, cl1);
			OntProperty p2 = getOrCreateDataProperty(1, 0, cl2);
			p1.addEquivalentProperty(p2);
			if (p1 != null && p2 != null) {
				p1.addEquivalentProperty(p2);
			}
		} else if (ct1.equals(ConceptType.INDIVIDUAL)) {
			Individual inst1 = getOrCreateIndividual(0, 0, cl1);
			Individual inst2 = getOrCreateIndividual(1, 0, cl2);
			if (inst1 != null && inst2 != null) {
				inst1.setSameAs(inst2);
			}
		}
		if (cl1 instanceof ConceptName) {
			removePendingError((ConceptName) cl1, ConceptType.ONTCLASS);
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to make a functional property
	 * 
	 * @param prop
	 * @return
	 */
	public List<ModelError> addFunctionalProperty(ConceptName prop) {
		OntProperty p = getOntProperty(prop);
		if (p == null) {
			addError(0, "Property '" + prop.toString()
					+ "' must exist before being declared functional.");
		} else {
			makePropertyFunctional(p);
			if (p.isObjectProperty()) {
				getJenaModel().createObjectProperty(p.getURI(), true);
			} else {
				getJenaModel().createDatatypeProperty(p.getURI(), true);
			}
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to make a property inverse functional
	 * 
	 * @param prop
	 * @return
	 */
	public List<ModelError> addInverseFunctionalProperty(ConceptName prop) {
		OntProperty p = getOntProperty(prop);
		if (p == null) {
			addError(
					0,
					"Property '"
							+ prop.toString()
							+ "' must be exist before being declared inverse functional.");
		} else {
			p.convertToInverseFunctionalProperty();
			getJenaModel().createInverseFunctionalProperty(p.getURI());
		}
		return getErrorsFinal();
	}

	public List<ModelError> addInverseProperties(ConceptName prop1,
			ConceptName prop2) {
		OntProperty p1 = getOrCreateObjectProperty(0, 0, prop1);
		OntProperty p2 = getOrCreateObjectProperty(1, 0, prop2);
		if (p1 == null || p2 == null) {
			addError(1, "Inverse properties cannot both be instantiated.");
		} else {
			p1.addInverseOf(p2);
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to add a DatatypeProperty to the model.
	 * 
	 * @param propName
	 *            -- name of the property to be added
	 * @param superPropName
	 *            -- name of the super property to be subbed or null if none
	 * @param xsdRange
	 *            -- identifier of the DataypeProperty range
	 * @param functional
	 *            -- true if the property is to be functional
	 * @return -- errors encountered or null if none
	 */
	public List<ModelError> addDatatypeProperty(String propName,
			ConceptName superPropName, String xsdRange, boolean functional) {
		Resource dpr;
		try {
			ConceptName pcn = new ConceptName(propName);
			dpr = getOntResourceInExistingModel(pcn);
			if (dpr != null) {
				ConceptType dpt = getOntResourceConceptType(dpr);
				if (!dpt.equals(ConceptType.DATATYPEPROPERTY)) {
					addError(0, "'" + propName + "' already exists and is of type " + dpt.toString());
				}
				else {
					if (superPropName != null) {
						OntProperty superProp = getOrCreateDataProperty(1,
								0, superPropName);
						if (superProp != null) {
							if (dpr.canAs(OntProperty.class)) {
								dpr.as(OntProperty.class).setSuperProperty(superProp);
							}
							else {
								addError(0, "Unable to make '" + propName + "' a subclass of '" + superPropName.toString() + "'; not an ontology property.");
							}
						}
					}
					pcn.setNamespace(dpr.getNameSpace());
					removePendingError(pcn, ConceptType.DATATYPEPROPERTY);
				}
			}
			else {
				DatatypeProperty prop = null;
				try {
					prop = getJenaModel().createDatatypeProperty(getUri(propName));
					if (prop != null) {
						if (logger.isDebugEnabled()) {
							exploreProperty(prop);
						}
						if (superPropName != null) {
							OntProperty superProp = getOrCreateDataProperty(1,
									0, superPropName);
							if (superProp != null) {
								prop.setSuperProperty(superProp);
							}
						}
						if (xsdRange != null) {
							String rngUri = new ConceptName(xsdRange).getUri(getConfigurationMgr());
							Resource r = getJenaModel().getResource(rngUri);
							if (r != null) {
								OntResource existingRange = prop.getRange();
								if (existingRange != null
										&& !existingRange.equals(r)) {
									addError(3, ExistingNamePart.NOTAPPLICABLE,
											"Property '" + prop.getURI()
													+ "' already has range '"
													+ existingRange.toString()
													+ "; can't add second range ("
													+ r.toString()
													+ ") to data property.");
								} else {
									prop.addRange(r);
								}
							}
						}
						if (functional) {
							makePropertyFunctional(prop);
						}
						pcn.setNamespace(prop.getNameSpace());
						removePendingError(pcn,
								ConceptType.DATATYPEPROPERTY);
					} else {
						addError(0, ExistingNamePart.NAME,
								"Unable to create property '" + propName + "'.");
					}
				} catch (PrefixNotFoundException e) {
					addError(0, "Unable to create data type property '" + propName
							+ "': " + e.getMessage());
				} catch (MalformedURLException e) {
					addError(0, "Unable to create data type property '" + propName
							+ "': " + e.getMessage());
				} catch (InvalidNameException e) {
					addError(0, "Unable to create data type property '" + propName
							+ "': " + e.getMessage());
				}
			}
		} catch (ConfigurationException e1) {
			addError(0, "Unexpected error creating data type property '" + propName + "': " + e1.getMessage());
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to add a ObjectProperty to the model.
	 * 
	 * @param propName
	 *            -- name of the property to be added
	 * @param superPropName
	 *            -- name of the super property to be subbed or null if none
	 * @param range
	 *            -- descriptor of the range
	 * @param functional
	 *            -- true if the property is to be functional
	 * @return -- errors encountered or null if none
	 */
	public List<ModelError> addObjectProperty(String propName,
			ConceptName superPropName, ConceptIdentifier range,
			boolean functional) {
		Resource opr;
		try {
			ConceptName pcn = new ConceptName(propName);
			opr = getOntResourceInExistingModel(pcn);
			if (opr != null) {
				ConceptType opt = getOntResourceConceptType(opr);
				if (!opt.equals(ConceptType.OBJECTPROPERTY)) {
					addError(0, "Cannot create '" + propName + "' as an object property; already exists and is of type " + opt.toString());
				}
				else {
					if (range != null) {
						updateObjectPropertyRange(2, opr.as(ObjectProperty.class), range);
					}
					pcn.setNamespace(opr.getNameSpace());
					removePendingError(pcn, ConceptType.OBJECTPROPERTY);
				}
			}
			else {
				ObjectProperty prop = null;
				try {
					prop = getJenaModel().createObjectProperty(getUri(propName),
							functional);
					if (prop != null) {
						if (logger.isDebugEnabled()) {
							exploreProperty(prop);
						}
						if (superPropName != null) {
							ObjectProperty superProp = getOrCreateObjectProperty(1,
									0, superPropName);
							if (superProp != null) {
								prop.addSuperProperty(superProp);
							}
						}
						if (range != null) {
							updateObjectPropertyRange(2, prop, range);
						}
						pcn.setNamespace(prop.getNameSpace());
						removePendingError(pcn, ConceptType.OBJECTPROPERTY);
					} else {
						addError(0, ExistingNamePart.NAME,
								"Unable to create property '" + propName + "'.");
					}
				} catch (PrefixNotFoundException e) {
					addError(0, "Unable to create object property '" + propName
							+ "': " + e.getMessage());
				}
			}
		} catch (ConfigurationException e1) {
			addError(0, "Unexpected error creating object property '" + propName + "': " + e1.getMessage());
		}
		return getErrorsFinal();
	}

	private void exploreProperty(OntProperty prop) {
		logger.debug("Exploring property '" + prop.getURI() + "':");
		ExtendedIterator<? extends OntResource> eitr = prop.listDomain();
		logger.debug("   Domain:");
		while (eitr.hasNext()) {
			OntResource dr = eitr.next();
			logger.debug("      " + classToString(dr));
		}
		eitr = prop.listRange();
		logger.debug("   Range:");
		while (eitr.hasNext()) {
			OntResource rr = eitr.next();
			logger.debug("      " + classToString(rr));
		}
	}

	/**
	 * Call this method to add any of the following class restrictions:
	 * AllValuesFrom SomeValuesFrom HasValue Cardinality MaxCardinality
	 * MinCardinality
	 * 
	 * @param resCls
	 * @param prop
	 * @param crc
	 * @return
	 */
	public List<ModelError> addClassRestriction(ConceptIdentifier resCls,
			ConceptName prop, ClassRestrictionCondition crc) {
		OntClass rcls = ontClassInThisModel(conceptIdentifierToOntClass(0, 0, resCls));
		if (rcls == null) {
			addError(1,
					"Unable to find restricted concept '" + 
							(resCls != null ? resCls.toString() : resCls)
							+ "' for restricted property '" + 
							(prop != null ? prop.toString() : prop)
							+ "'");
			return getErrorsFinal();
		}
		OntProperty p = null;
		Resource pr;
		ConceptType pType = null;
		try {
			pr = getOntResourceInExistingModel(prop);
			if (pr != null) {
				pType = getOntResourceConceptType(pr);
				prop.setType(pType);
				prop.setNamespace(pr.getNameSpace());
				p = pr.as(OntProperty.class);
			}
			else {
				addError(2, "Unable to find property '" + prop.toString()
						+ "'; must be declared before use in a class restriction.");
				return getErrorsFinal();
			}
		} catch (ConfigurationException e) {
			addError(2, "Unexpected error finding property '" + prop.toString() + "': " + e.getMessage());
			return getErrorsFinal();
		}
		RestrictionType rType = crc.getRestrictionType();
		if (rType.equals(RestrictionType.ALLVALUES)) {
			OntClass toCls = conceptIdentifierToOntClass(2, 0,
					crc.getRestrictedToConcept());
			if (toCls != null) {
				AllValuesFromRestriction avf = getJenaModel()
						.createAllValuesFromRestriction(null, p, toCls);
				if (avf != null) {
					rcls.addSuperClass(avf);
				} else {
					addError(1,
							"Unable to create allValuesFromRestriction for unknown reason.");
				}
			} else {
				addError(3, "allValuesFromRestriction to class not known.");
			}
		} else if (rType.equals(RestrictionType.SOMEVALUES)) {
			OntClass toCls = conceptIdentifierToOntClass(2, 0,
					crc.getRestrictedToConcept());
			if (toCls != null) {
				SomeValuesFromRestriction svf = getJenaModel()
						.createSomeValuesFromRestriction(null, p, toCls);
				if (svf != null) {
					rcls.addSuperClass(svf);
				} else {
					addError(1,
							"Unable to create someValuesFromRestriction for unknown reason.");
				}
			} else {
				addError(3, "someValuesFromRestriction to class not known.");
			}
		} else if (rType.equals(RestrictionType.HASVALUE)) {
			HasValueRestriction hvr = createHasValueRestriction(prop, p, 1,
					crc, 2);
			if (hvr != null) {
				rcls.addSuperClass(hvr);
			}
		} else if (rType.equals(RestrictionType.CARDINALITY)) {
			addCardinalityRestriction(1, rcls, prop, p, crc);
		} else if (rType.equals(RestrictionType.MAXCARDINALIY)) {
			addMaxCardinalityRestriction(1, rcls, prop, p, crc);
		} else if (rType.equals(RestrictionType.MINCARDINALITY)) {
			addMinCardinalityRestriction(1, rcls, prop, p, crc);
		} else {
			addError(2, "Invalid class restriction type: " + rType.toString());
		}
		return getErrorsFinal();
	}

	private void addCardinalityRestriction(int idx, OntClass rcls, ConceptName prop, OntProperty p, 
			ClassRestrictionCondition crc) {
		CardinalityRestriction cr = createCardinalityRestriction(prop, p,
				1, crc, 2);
		if (cr != null) {
			rcls.addSuperClass(cr);
		}
	}

	private void addMaxCardinalityRestriction(int idx, OntClass rcls, ConceptName prop, OntProperty p, 
			ClassRestrictionCondition crc) {
		MaxCardinalityRestriction mcr = createMaxCardinalityRestriction(prop, p,
				1, crc, 2);
		if (mcr != null) {
			rcls.addSuperClass(mcr);
		}
	}

	private void addMinCardinalityRestriction(int idx, OntClass rcls, ConceptName prop, OntProperty p, 
			ClassRestrictionCondition crc) {
		MinCardinalityRestriction mcr = createMinCardinalityRestriction(prop, p,
				1, crc, 2);
		if (mcr != null) {
			rcls.addSuperClass(mcr);
		}
	}

	public List<ModelError> addNecessaryAndSufficientCondition(
			ConceptIdentifier subClass, ConceptIdentifier superClass,
			List<ConceptName> propList, List<ClassRestrictionCondition> condList) {
		if (subClass == null) {
			addError(1, "Subclass cannot be empty.");
		} else if (superClass == null || !(superClass instanceof ConceptName)) {
			addError(2, "Superclass must be a name.");
		} else if (propList == null) {
			addError(3, "Property cannot be empty.");
		} else if (condList == null) {
			addError(4, "Condition cannot be empty.");
		} else {
			OntClass rolecls = conceptIdentifierToOntClass(0, 0, subClass);
			if (rolecls == null) {
				addError(1, "Subclass '" + subClass + "' not found.");
				return getErrorsFinal();
			}
			OntClass supercls = ontClassInThisModel(conceptIdentifierToOntClass(1, 0, superClass));
			if (supercls == null) {
				addError(2, "Superclass '" + superClass + "' not found.");
				return getErrorsFinal();
			}
			PendingModelError pme = getPendingError((ConceptName) subClass,
					ConceptType.ONTCLASS);
			if (pme != null) {
				// it's ok to create this on the fly
				((ConceptName)subClass).setNamespace(rolecls.getNameSpace());
				removePendingError((ConceptName) subClass, ConceptType.ONTCLASS);
			}
			if (propList.size() != condList.size()) {
				addError(3, "Invalid restriction condition.");
				return getErrorsFinal();
			}
			List<Restriction> restrictions = new ArrayList<Restriction>();
			for (int i = 0; i < propList.size(); i++) {
				ConceptName prop = propList.get(i);
				ClassRestrictionCondition cond = condList.get(i);
				RestrictionType rType = cond.getRestrictionType();
				OntProperty p = null;
				Resource pr;
				ConceptType ptype = null;
				try {
					pr = getOntResourceInExistingModel(prop);
					if (pr != null) {
						ptype = getOntResourceConceptType(pr);
						prop.setType(ptype);
						prop.setNamespace(pr.getNameSpace());
						p = pr.as(OntProperty.class);
					}
					else {
						if (rType.equals(RestrictionType.ALLVALUES) ||
								rType.equals(RestrictionType.SOMEVALUES)) {
							p = getJenaModel().createObjectProperty(getUri(prop), false);
							pr = p;
							ptype = ConceptType.OBJECTPROPERTY;
						}
						else {
							addError(2, "Unable to find property '" + prop.toString()
									+ "'; must be declared before use in a class restriction.");
							return getErrorsFinal();
						}
					}
				} catch (ConfigurationException e) {
					addError(2, "Unexpected error finding property '" + prop.toString() + "': " + e.getMessage());
					return getErrorsFinal();
				}
				if (rType.equals(RestrictionType.ALLVALUES)) {
					if (!ptype.equals(ConceptType.OBJECTPROPERTY)) {
						addError(2,
								"An allValuesFrom restriction must be on an ObjectProperty");
					} else {
						OntClass restrictedTo = conceptIdentifierToOntClass(3,
								0, cond.getRestrictedToConcept());
						AllValuesFromRestriction avfr = getJenaModel()
								.createAllValuesFromRestriction(null, p,
										restrictedTo);
						if (avfr != null) {
							restrictions.add(avfr);
						} else {
							addError(3,
									"Sufficient condition value restriction'"
											+ condList.toString()
											+ "' not found.");
						}
					}
				} else if (rType.equals(RestrictionType.SOMEVALUES)) {
					if (!ptype.equals(ConceptType.OBJECTPROPERTY)) {
						addError(2,
								"A someValuesFrom restriction must be on an ObjectProperty");
					} else {
						OntClass restrictedTo = conceptIdentifierToOntClass(3,
								0, cond.getRestrictedToConcept());
						SomeValuesFromRestriction svfr = getJenaModel()
								.createSomeValuesFromRestriction(null, p,
										restrictedTo);
						if (svfr != null) {
							restrictions.add(svfr);
						} else {
							addError(3,
									"Sufficient condition value restriction'"
											+ condList.toString()
											+ "' not found.");
						}
					}
				} else if (rType.equals(RestrictionType.HASVALUE)) {
					HasValueRestriction hvr = createHasValueRestriction(prop,
							p, 2, cond, 3);
					if (hvr != null) {
						restrictions.add(hvr);
					} else {
						addError(3, "Sufficient condition value restriction'"
								+ condList.toString() + "' not found.");
					}
				} else if (rType.equals(RestrictionType.CARDINALITY)) {
					CardinalityRestriction cr = createCardinalityRestriction(
							prop, p, 2, cond, 3);
					if (cr != null) {
						restrictions.add(cr);
					} else {
						addError(3, "Sufficient condition value restriction'"
								+ condList.toString() + "' not found.");
					}
				} else if (rType.equals(RestrictionType.MAXCARDINALIY)) {
					if (p == null) {
						addError(2,
								"Cardinality restriction can't tell if property is object or data property.");
					} else {
						int card = cond.getRestrictedToCardinality();
						MaxCardinalityRestriction mcr = getJenaModel()
								.createMaxCardinalityRestriction(null, p, card);
						if (mcr != null) {
							restrictions.add(mcr);
						} else {
							addError(3,
									"Sufficient condition value restriction'"
											+ condList.toString()
											+ "' not found.");
						}
					}
				} else if (rType.equals(RestrictionType.MINCARDINALITY)) {
					if (p == null) {
						addError(2,
								"Cardinality restriction can't tell if property is object or data property.");
					} else {
						int card = cond.getRestrictedToCardinality();
						MinCardinalityRestriction mcr = getJenaModel()
								.createMinCardinalityRestriction(null, p, card);
						if (mcr != null) {
							restrictions.add(mcr);
						} else {
							addError(3,
									"Sufficient condition value restriction'"
											+ condList.toString()
											+ "' not found.");
						}
					}
				}
			}
			if (restrictions.size() == 1) {
				createEquivalentClass(rolecls, supercls, restrictions.get(0));
			} else {
				createEquivalentClass(rolecls, supercls, restrictions);
			}
		}
		return getErrorsFinal();
	}

	private void makePropertyFunctional(OntProperty prop) {
		prop.convertToFunctionalProperty();
		Property fp = getJenaModel().getProperty(
				"http://www.w3.org/2002/07/owl#FunctionalProperty");
		if (fp != null) {
			prop.addSuperProperty(fp);
		}
	}

	private CardinalityRestriction createCardinalityRestriction(
			ConceptName prop, OntProperty p, int i,
			ClassRestrictionCondition rCond, int j) {
		if (p == null) {
			addError(2,
					"Cardinality restriction can't tell if property '" + prop + "' is object or data property.");
			return null;
		} else {
			int card = rCond.getRestrictedToCardinality();
			CardinalityRestriction cr = getJenaModel()
					.createCardinalityRestriction(null, p, card);
			if (cr != null && rCond.getRestrictedToObject() != null) {
				cr.removeAll(OWL.cardinality);
				cr.addLiteral(OWL2.qualifiedCardinality, new Integer(rCond.getRestrictedToCardinality()));
				OntClass qualifiedCls = conceptIdentifierToOntClass(i, j, (ConceptIdentifier) rCond.getRestrictedToObject());
				cr.addProperty(OWL2.onClass, qualifiedCls);
			}
			return cr;
		}
	}

	private MaxCardinalityRestriction createMaxCardinalityRestriction(
			ConceptName prop, OntProperty p, int i,
			ClassRestrictionCondition rCond, int j) {
		if (p == null) {
			addError(2,
					"Max cardinality restriction can't tell if property '" + prop + "' is object or data property.");
			return null;
		} else {
			int card = rCond.getRestrictedToCardinality();
			MaxCardinalityRestriction cr = getJenaModel()
					.createMaxCardinalityRestriction(null, p, card);
			if (cr != null && rCond.getRestrictedToObject() != null) {
				cr.removeAll(OWL.maxCardinality);
				cr.addLiteral(OWL2.maxQualifiedCardinality, new Integer(rCond.getRestrictedToCardinality()));
				OntClass qualifiedCls = conceptIdentifierToOntClass(i, j, (ConceptIdentifier) rCond.getRestrictedToObject());
				cr.addProperty(OWL2.onClass, qualifiedCls);
			}
			return cr;
		}
	}

	private MinCardinalityRestriction createMinCardinalityRestriction(
			ConceptName prop, OntProperty p, int i,
			ClassRestrictionCondition rCond, int j) {
		if (p == null) {
			addError(2,
					"Min cardinality restriction can't tell if property '" + prop + "' is object or data property.");
			return null;
		} else {
			int card = rCond.getRestrictedToCardinality();
			MinCardinalityRestriction cr = getJenaModel()
					.createMinCardinalityRestriction(null, p, card);
			if (cr != null && rCond.getRestrictedToObject() != null) {
				cr.removeAll(OWL.minCardinality);
				cr.addLiteral(OWL2.minQualifiedCardinality, new Integer(rCond.getRestrictedToCardinality()));
				OntClass qualifiedCls = conceptIdentifierToOntClass(i, j, (ConceptIdentifier) rCond.getRestrictedToObject());
				cr.addProperty(OWL2.onClass, qualifiedCls);
			}
			return cr;
		}
	}

	private HasValueRestriction createHasValueRestriction(ConceptName prop,
			OntProperty p, int propIdx, ClassRestrictionCondition rCond,
			int condIdx) {
		RDFNode restrictedTo = null;
		ConceptType ptype = prop.getType();
		if (rCond.getRestrictedToConcept() != null) {
			if (!ptype.equals(ConceptType.OBJECTPROPERTY)) {
				addError(
						2,
						"The restriction is an instance but the hasValue restriction is on a DatatypeProperty");
			} else {
				restrictedTo = getOrCreateIndividual(condIdx, 0,
						rCond.getRestrictedToConcept());
			}
		} else if (rCond.getRestrictedToObject() != null) {
			if (!ptype.equals(ConceptType.DATATYPEPROPERTY)) {
				addError(
						2,
						"The restriction is a literal but the hasValue restriction is on an ObjectProperty");
			} else {
				try {
					restrictedTo = UtilsForJena
							.getLiteralMatchingDataPropertyRange(
									getJenaModel(), p,
									rCond.getRestrictedToObject());
				} catch (Exception e) {
					addError(condIdx,
							"Unable to create Literal matching range of property being restricted.");
					e.printStackTrace();
				}
			}
		}
		if (restrictedTo != null) {
			if (p == null) {
				p = getOrCreateObjectProperty(propIdx, 0, prop);
			}
			HasValueRestriction hvr = getJenaModel().createHasValueRestriction(
					null, p, restrictedTo);
			return hvr;
		}
		addError(condIdx,
				"Unable to identify value to which the class is being restricted.");
		return null;
	}

	private void createEquivalentClass(OntClass rolecls, OntClass supercls,
			Restriction restrict) {
		if (supercls != null && restrict != null) {
			RDFNode[] memberList = new RDFNode[2];
			memberList[0] = supercls;
			memberList[1] = restrict;
			RDFList members = getJenaModel().createList(memberList);
			IntersectionClass eqcls = getJenaModel().createIntersectionClass(
					null, members);
			rolecls.setEquivalentClass(eqcls);
		} else if (restrict != null) {
			rolecls.setEquivalentClass(restrict);
		}
	}

	private void createEquivalentClass(OntClass rolecls, OntClass supercls,
			List<Restriction> restrict) {
		int base = supercls != null ? 1 : 0;
		RDFNode[] memberList = new RDFNode[base + restrict.size()];
		if (base > 0) {
			memberList[0] = supercls;
		}
		for (int i = 0; i < restrict.size(); i++) {
			memberList[base + i] = restrict.get(i);
		}
		RDFList members = getJenaModel().createList(memberList);
		IntersectionClass eqcls = getJenaModel().createIntersectionClass(null,
				members);
		rolecls.setEquivalentClass(eqcls);
	}

	public List<ModelError> addEnumeratedAllValuesFromRestriction(
			ConceptIdentifier restrictedClass, ConceptName restrictedProperty,
			List<Object> enumList) {
		if (enumList != null && enumList.size() > 0) {
			Object firstElement = enumList.get(0);
			OntProperty p = null;
			Resource pr;
			ConceptType propType = null;
			try {
				pr = getOntResourceInExistingModel(restrictedProperty);
				if (pr != null) {
					propType = getOntResourceConceptType(pr);
					p = pr.as(OntProperty.class);
				}
				else {
					addError(2, "Unable to find property '" + restrictedProperty.toString()
							+ "'; must be declared before use in a class restriction.");
					return getErrorsFinal();
				}
			} catch (ConfigurationException e) {
				addError(2, "Unexpected error finding property '" + restrictedProperty.toString() + "': " + e.getMessage());
				return getErrorsFinal();
			}
			OntClass restricted = ontClassInThisModel(conceptIdentifierToOntClass(0, 0,
					restrictedClass));
			if (restricted == null) {
				addError(0, "Restricted class '" + restrictedClass.toString()
						+ "' not found.");
			} else {
				if (firstElement instanceof ConceptIdentifier) {
					if (!propType.equals(ConceptType.OBJECTPROPERTY)) {
						addError(
								1,
								"Property '"
										+ restrictedProperty.toString()
										+ "' is not an object property, can't restrict to '"
										+ firstElement.toString() + "'");
					} else {
						boolean allCNs = true;
						List<ConceptName> ecnList = new ArrayList<ConceptName>();
						for (int i = 0; i < enumList.size(); i++) {
							if (!(enumList.get(i) instanceof ConceptName)) {
								allCNs = false;
								addError(
										2,
										"Expected restricted value list item '"
												+ enumList.get(i)
														.toString()
												+ "' to be an instance but it isn't.");
								break;
							} else {
								ConceptName ecn = validateConceptName((ConceptName) enumList
										.get(i));
								if (!ecn.getType().equals(
										ConceptType.INDIVIDUAL)) {
									addError(
											2,
											"Expected restricted value list item '"
													+ enumList.get(i)
															.toString()
													+ "'  to be an instance but it is a "
													+ ecn.getType());
									allCNs = false;
									break;
								}
								ecnList.add(ecn);
							}
						}
						if (allCNs) {
							SadlEnumeratedClass tmpec = new SadlEnumeratedClass(
									ecnList);
							EnumeratedClass encls = createEnumeratedClass(
									2, null, tmpec);
							AllValuesFromRestriction avf = getJenaModel()
									.createAllValuesFromRestriction(null,
											p, encls);
							if (avf != null) {
								restricted.addSuperClass(avf);
							} else {
								addError(2,
										"Unable to create allValuesFromRestriction for unknown reason.");
							}
						}
					}
				} else {
					if (!propType.equals(ConceptType.DATATYPEPROPERTY)) {
						addError(
								1,
								"Property '"
										+ restrictedProperty.toString()
										+ "' is not a data property, can't restrict to '"
										+ firstElement.toString() + "'");
					} else {
						RDFList rdfl = getJenaModel().createList();
						for (int i = 0; i < enumList.size(); i++) {
							Object lstval = enumList.get(i);
							Literal lval;
							try {
								lval = UtilsForJena
										.getLiteralMatchingDataPropertyRange(
												getJenaModel(), p, lstval);
								rdfl = rdfl.with(lval);
							} catch (Exception e) {
								addError(2,
										"Unable to create Literal matching range of property being restricted.");
								e.printStackTrace();
							}
						}
						DataRange dr = getJenaModel().createDataRange(rdfl);
						AllValuesFromRestriction avf = getJenaModel()
								.createAllValuesFromRestriction(null, p, dr);
						if (avf != null) {
							restricted.addSuperClass(avf);
						} else {
							addError(2,
									"Unable to create allValuesFromRestriction for unknown reason.");
						}
					}
				}
			}
		}
		return getErrorsFinal();
	}

	public List<ModelError> addEnumeratedSomeValuesFromRestriction(
			ConceptIdentifier restrictedClass, ConceptName restrictedProperty,
			List<Object> enumList) {
		if (enumList != null && enumList.size() > 0) {
			Object firstElement = enumList.get(0);
			OntProperty p = null;
			Resource pr;
			ConceptType propType = null;
			try {
				pr = getOntResourceInExistingModel(restrictedProperty);
				if (pr != null) {
					propType = getOntResourceConceptType(pr);
					p = pr.as(OntProperty.class);
				}
				else {
					addError(2, "Unable to find property '" + restrictedProperty.toString()
							+ "'; must be declared before use in a class restriction.");
					return getErrorsFinal();
				}
			} catch (ConfigurationException e) {
				addError(2, "Unexpected error finding property '" + restrictedProperty.toString() + "': " + e.getMessage());
				return getErrorsFinal();
			}
			OntClass restricted = ontClassInThisModel(conceptIdentifierToOntClass(0, 0,
					restrictedClass));
			if (restricted == null) {
				addError(0, "Restricted class '" + restrictedClass.toString()
						+ "' not found.");
			} else {
				if (firstElement instanceof ConceptIdentifier) {
					if (!propType.equals(ConceptType.OBJECTPROPERTY)) {
						addError(
								1,
								"Property '"
										+ restrictedProperty.toString()
										+ "' is not an object property, can't restrict to '"
										+ firstElement.toString() + "'");
					} else {
						boolean allCNs = true;
						List<ConceptName> ecnList = new ArrayList<ConceptName>();
						for (int i = 0; i < enumList.size(); i++) {
							if (!(enumList.get(i) instanceof ConceptName)) {
								allCNs = false;
								addError(
										2,
										"Expected restricted value '"
												+ enumList.get(i)
														.toString()
												+ "' to be an instance but it isn't.");
								break;
							} else {
								ConceptName ecn = validateConceptName((ConceptName) enumList
										.get(i));
								if (ecn.getType().equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
									addError(
											2,
											"Restricted value '"
													+ enumList.get(i)
															.toString()
													+ "'  was not found in any visible model.");
									break;
								}
								else if (!ecn.getType().equals(
										ConceptType.INDIVIDUAL)) {
									addError(
											2,
											"Expected restricted value '"
													+ enumList.get(i)
															.toString()
													+ "'  to be an instance but it is a "
													+ ecn.getType());
									break;
								}
								ecnList.add(ecn);
							}
						}
						if (allCNs && ecnList.size() > 0) {
							SadlEnumeratedClass tmpec = new SadlEnumeratedClass(
									ecnList);
							EnumeratedClass encls = createEnumeratedClass(
									2, null, tmpec);
							SomeValuesFromRestriction svf = getJenaModel()
									.createSomeValuesFromRestriction(null,
											p, encls);
							if (svf != null) {
								restricted.addSuperClass(svf);
							} else {
								addError(2,
										"Unable to create someValuesFromRestriction for unknown reason.");
							}
						}
					}
				} else {
					if (!propType.equals(ConceptType.DATATYPEPROPERTY)) {
						addError(
								1,
								"Property '"
										+ restrictedProperty.toString()
										+ "' is not a data property, can't restrict to '"
										+ firstElement.toString() + "'");
					} else {
						RDFList rdfl = getJenaModel().createList();
						for (int i = 0; i < enumList.size(); i++) {
							Object lstval = enumList.get(i);
							Literal lval;
							try {
								lval = UtilsForJena
										.getLiteralMatchingDataPropertyRange(
												getJenaModel(), p, lstval);
								rdfl = rdfl.with(lval);
							} catch (Exception e) {
								addError(2,
										"Unable to create Literal matching range of property being restricted.");
								e.printStackTrace();
							}
						}
						DataRange dr = getJenaModel().createDataRange(rdfl);
						SomeValuesFromRestriction svf = getJenaModel()
								.createSomeValuesFromRestriction(null, p,
										dr);
						if (svf != null) {
							restricted.addSuperClass(svf);
						} else {
							addError(2,
									"Unable to create allValuesFromRestriction for unknown reason.");
						}
					}
				}
			}
		}
		return getErrorsFinal();
	}

	public List<ModelError> addEnumeratedAllAndSomeValuesFromRestriction(
			ConceptIdentifier restrictedClass, ConceptName restrictedProperty,
			List<Object> enumList) {
		List<ModelError> errors1 = addEnumeratedAllValuesFromRestriction(
				restrictedClass, restrictedProperty, enumList);
		List<ModelError> errors2 = addEnumeratedSomeValuesFromRestriction(
				restrictedClass, restrictedProperty, enumList);
		if (errors1 != null) {
			if (errors2 != null) {
				errors1.addAll(errors2);
			}
			return errors1;
		}
		return errors2;
	}

	/**
	 * Call this method to add an annotation property to the model.
	 * 
	 * @param name
	 * @return
	 */
	public List<ModelError> addAnnotationProperty(String propName) {
		AnnotationProperty prop = null;
		try {
			prop = getJenaModel().createAnnotationProperty(getUri(propName));
			if (prop == null) {
				addError(0, "Property '" + propName.toString()
						+ "' not found. " + getModelReference());
			}
		} catch (PrefixNotFoundException e) {
			addError(0, "Unable to create annotation property '" + propName
					+ "': " + e.getMessage());
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to add an rdfs:label or an rdfs:comment to an existing
	 * Resource (identified by name)
	 * 
	 * @param resourceName
	 * @param annType
	 * @param annContent
	 * @return
	 */
	public List<ModelError> addAnnotationToResource(String resourceName,
			String annType, String annContent) {
		if (resourceName != null && annType != null && annContent != null) {
			OntResource resource = null;
			try {
				resource = getJenaModel().getOntResource(getUri(resourceName));
				if (resource != null) {
					if (annType.equals("note")) {
						resource.addComment(annContent, null);
					} else if (annType.equals("alias")) {
						resource.addLabel(annContent, null);
					}
				} else {
					logger.error("Unable to add annotation '"
							+ annType.toString() + " " + annContent
							+ "' to Resource '" + getUri(resourceName)
							+ "'; not found in model.");
				}
			} catch (PrefixNotFoundException e) {
				logger.error("Unable to find resourceName " + resourceName, e);
			}
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to add domain information to a property
	 * 
	 * @param propName
	 *            -- the property to which the domain information is to be added
	 * @param domain
	 *            -- the domain information (may be null if defining a
	 *            subproperty)
	 * @param objIdentifier 
	 * @return -- errors encountered of null if none
	 */
	public List<ModelError> addPropertyDomain(ConceptName propName,
			ConceptIdentifier domain, boolean isSingleValuedOnClass, Object objIdentifier) {
		OntProperty prop = getOntProperty(propName);
		if (prop == null) {
			try {
				Resource pr = getOntResourceInExistingModel(propName);
				if (pr == null) {
					addError(
						0,
						"Property '"
								+ propName.toString()
								+ "' not found. "
								+ getModelReference()
								+ ". If this property is declared later, please move declaration before this statement so that property type (object or data) will be known.");
				}
//				else {
//					I think this gets reported elsewhere awc 2/28/2014
//				}
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else if (domain != null) {
			if (updatePropertyDomain(1, prop, domain)) {
				if (isSingleValuedOnClass) {
					OntClass domainCls = ontClassInThisModel(conceptIdentifierToOntClass(1, 0,
							domain));
					if (domainCls == null) {
						addError(1, "Domain class '" + domain.toString()
								+ "' not found.");
					} else {
						if (prop.isDatatypeProperty()) {
							// must be trying to restrict property on class for datatype property
							if (maxCardinalityRestrictionAlreadyExists(domainCls, prop, 1)) {
								addWarning(2, "A single-valued restriction has been given to this class and data type property elsewhere so a maximum cardinality restriction of 1 already exists. This does NOT create a qualified cardinality restriction for a specific range class.");
							}
							addMaxCardinalityRestriction(1, domainCls, propName, prop, new ClassRestrictionCondition(RestrictionType.MAXCARDINALIY, 1));
						}
						else if (objIdentifier != null) {
							if (objIdentifier instanceof ConceptIdentifier) {
								addMaxCardinalityRestriction(1, domainCls, propName, prop, 
										new ClassRestrictionCondition(RestrictionType.MAXCARDINALIY, 1, (ConceptIdentifier) objIdentifier));
							}
							else {
								addError(2, "Restricted-to class of a single-valued restriction is not of the expected type: " + objIdentifier.toString());
							}
						}
					}
				}
			} else {
				addError(1, "Error adding domain '" + domain.toString()
						+ "' to property '" + propName.toString() + "'");
			}
		}
		return getErrorsFinal();
	}

	private boolean maxCardinalityRestrictionAlreadyExists(OntClass domainCls,
			OntProperty prop, int mxcard) {
		ExtendedIterator<OntClass> eitr = domainCls.listSuperClasses(true);
		while (eitr.hasNext()) {
			OntClass supercls = eitr.next();
			if (supercls.isRestriction()) {
				Restriction rstrctn = supercls.asRestriction();
				if (rstrctn.isMaxCardinalityRestriction()) {
					MaxCardinalityRestriction maxrest = rstrctn.asMaxCardinalityRestriction();
					if (maxrest.getOnProperty().equals(prop) && maxrest.getMaxCardinality() == mxcard) {
						eitr.close();
						return true;
					}
				}
			}
		}
		eitr.close();
		return false;
	}

	/**
	 * Call this method to add range information to an object property.
	 * 
	 * @param propName
	 *            -- the property to which the range information is to be added
	 * @param range
	 *            -- the range information to add
	 * @return -- errors encountered or null if none
	 */
	public List<ModelError> addObjectPropertyRange(ConceptName propName,
			ConceptIdentifier range) {
		OntProperty prop = getOntProperty(propName);
		if (prop == null) {
			addError(0, "Property '" + propName.toString() + "' not found. "
					+ getModelReference());
		} else {
			updateObjectPropertyRange(1, prop, range);
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to declare a property to be symmetrical
	 * 
	 * @param prop
	 * @return
	 */
	public List<ModelError> addSymmetricalProperty(ConceptName prop) {
		OntProperty symmProp = getOrCreateObjectProperty(0, 0, prop);
		symmProp.convertToSymmetricProperty();
		getJenaModel().createSymmetricProperty(symmProp.getURI());
		return getErrorsFinal();
	}

	/**
	 * Call this method to declare a property to be transitive
	 * 
	 * @param prop
	 * @return
	 */
	public List<ModelError> addTransitiveProperty(ConceptName prop) {
		OntProperty transProp = getOrCreateObjectProperty(0, 0, prop);
		transProp.convertToTransitiveProperty();
		getJenaModel().createTransitiveProperty(transProp.getURI());
		return getErrorsFinal();
	}

	/**
	 * Call this method to add a statement (RDF Triple) to the model when the
	 * property is an ObjectProperty.
	 * 
	 * @param subjName
	 *            -- the name of the subject (instance) of the statement
	 * @param propName
	 *            -- the name of the property of the statement
	 * @param objName
	 *            -- the name of the object value (instance) of the statement
	 * @return
	 */
	public List<ModelError> addStatement(ConceptName subjName,
			ConceptName propName, ConceptName objName) {
		int part = 0;
		Resource pr;
		try {
			OntProperty prop = null;
			Property annProp = null;
			part = 1;
			pr = getOntResourceInExistingModel(propName);
			if (pr != null) {
				ConceptType pt = getOntResourceConceptType(pr);
				if (pt.equals(ConceptType.ANNOTATIONPROPERTY)) {
					if (pr.canAs(OntProperty.class)) {
						prop = pr.as(OntProperty.class);
					}
					else {
						annProp = pr.as(Property.class);
					}
					prop = pr.as(OntProperty.class);
					propName.setType(pt);
				}
				else if (!pt.equals(ConceptType.OBJECTPROPERTY)) {
					addError(1, "'" + propName + "' is not an object property (type is " + pt.toString() + ")");
				}
				else {
					prop = pr.as(OntProperty.class);
					propName.setType(pt);
				}
			}
			else {
				prop = getOrCreateObjectProperty(1, 0, propName);
				propName.setType(ConceptType.OBJECTPROPERTY);
			} 
			
			Individual obj = null;
			part = 2;
			Resource or = getOntResourceInExistingModel(objName);
			if (or != null) {
				ConceptType ot = getOntResourceConceptType(or);
				if (!ot.equals(ConceptType.INDIVIDUAL)) {
					addError(part, "'" + objName.toString() + "' already exists and is not an instance (type is " + ot.toString() + ")");
				}
				else {
					obj = or.as(Individual.class);
					objName.setType(ot);
				}
			}
			else {
				obj = getOrCreateIndividual(part,0, objName);
				objName.setType(ConceptType.INDIVIDUAL);
			}
			
			OntResource subj = null;
			part = 0;
			Resource sr = getOntResourceInExistingModel(subjName);
			if (sr != null) {
				ConceptType st = getOntResourceConceptType(sr);
				if (!st.equals(ConceptType.INDIVIDUAL) && !propName.getType().equals(ConceptType.ANNOTATIONPROPERTY)) {
					addError(part, "'" + subjName.toString() + "' already exists and is not an instance (type is " + st.toString() + ")");
				}
				if (sr.canAs(OntResource.class)) {
					subj = sr.as(OntResource.class);
					subjName.setType(st);
				}
				else {
					addError(part, "'" + subjName.toString() + "' cannot be a subject of a statement.");
					return getErrorsFinal();
				}
			}
			else {
				subj = getOrCreateIndividual(part, 0, subjName);
				subjName.setType(ConceptType.INDIVIDUAL);
			}
			if (subj != null && (prop != null || annProp != null) && obj != null) {
				if (propName.getType().equals(ConceptType.ANNOTATIONPROPERTY)) {
					if (prop != null) {
						addPendingValidationStatement(subj, prop, obj);
						getJenaModel().add(subj, prop, obj);
					}
					else {
						getJenaModel().add(subj, annProp, obj);
					}
				} else {
					addPendingValidationStatement(subj, prop, obj);
					getJenaModel().add(subj, prop, obj);
				}
			}
		} catch (ConfigurationException e) {
			addError(part, "Unexpected error: " + e.getMessage());
		}
		return getErrorsFinal();
	}

	/**
	 * Call this method to add a statement (RDF Triple) to the model when the
	 * property is an DatatypeProperty.
	 * 
	 * @param instName
	 *            -- the name of the subject (instance) of the statement
	 * @param propName
	 *            -- the name of the property of the statement
	 * @param dataValue
	 *            -- the name of the object value (instance) of the statement
	 * @return
	 */
	public List<ModelError> addStatement(ConceptName subjName,
			ConceptName propName, Object dataValue) {
		int part = 0;
		Resource pr;
		try {
			OntProperty prop = null;
			Property annProp = null;
			ConceptType propType;
			part = 1;
			pr = getOntResourceInExistingModel(propName);
			if (pr != null) {
				propType = getOntResourceConceptType(pr);
				propName.setType(propType);
				propName.setNamespace(pr.getNameSpace());
				if (propType.equals(ConceptType.ANNOTATIONPROPERTY)) {
					annProp = pr.as(Property.class);
				}
				else if (propType.equals(ConceptType.OBJECTPROPERTY) && 
						(dataValue instanceof Individual || dataValue instanceof ConceptName)) {
					// this happens when there is a bnode (Individual) or when the parser is confused (ConceptName)
					prop = pr.as(ObjectProperty.class); 
				}
				else if (propType.equals(ConceptType.DATATYPEPROPERTY)) {
					prop = pr.as(DatatypeProperty.class);
				}
				else {
					addError(1, "'" + propName + "' (type " + propType.toString() + ") does not match value (" + dataValue.toString() + ").");
					if (pr.canAs(OntProperty.class)) {
						prop = pr.as(OntProperty.class);
					}
					else {
						return getErrorsFinal();
					}
				}
			}
			else {
				prop = getOrCreateDataProperty(1, 0, propName);
				propName.setType(ConceptType.DATATYPEPROPERTY);
			} 
			
			OntResource subj = null;
			part = 0;
			Resource sr = getOntResourceInExistingModel(subjName);
			if (sr != null) {
				ConceptType st = getOntResourceConceptType(sr);
				subjName.setType(st);
				subjName.setNamespace(sr.getNameSpace());
				if (!st.equals(ConceptType.INDIVIDUAL) && !propName.getType().equals(ConceptType.ANNOTATIONPROPERTY)) {
					addError(part, "'" + subjName.toString() + "' already exists and is not an instance (type is " + st.toString() + ")");
				}
				if (sr.canAs(OntResource.class)) {
					subj = sr.as(OntResource.class);
				}
				else {
					addError(part, "'" + subjName.toString() + "' cannot be a subject of a statement.");
					return getErrorsFinal();
				}
			}
			else {
				subj = getOrCreateIndividual(part, 0, subjName);
				subjName.setType(ConceptType.INDIVIDUAL);
			}
			
			RDFNode val = null;
			part = 2;

			if (propName.getType().equals(ConceptType.ANNOTATIONPROPERTY)) {
				if (dataValue instanceof String) {
					dataValue = stripQuotes((String) dataValue);
				}
				val = getJenaModel().createTypedLiteral(dataValue);
				if (prop != null) {
					subj.addProperty(prop, val);
				}
				else {
					subj.addProperty(annProp, val);
				}
			}
			else {
				if (prop.isObjectProperty()) {
					// this shouldn't really happen--this should be a call to the
					// alternate signature,
					// but we can handle it here....
					if (dataValue instanceof ConceptName) {
						val = getIndividual((ConceptName) dataValue);
					}
					if (dataValue instanceof Individual) {
						// we get here with bnode values
						val = (Individual) dataValue;
					} else {
						addError(2,
								"Can't add a literal value to an object property.");
					}
				} else {
					try {
						val = UtilsForJena.getLiteralMatchingDataPropertyRange(
								getJenaModel(), prop, dataValue);
					} catch (Exception e) {
						if (prop.getRange() == null) {
							addWarning(2, "Failed to create literal as range is unknown at this point. Move property definition ahead of this statement?");
						}
						else {
							addError(
									2,
									"Error creating literal from '"
											+ dataValue.toString() + "': ("
											+ e.getClass().getSimpleName() + ") " + e.getMessage());
						}
					}
				}

				if (subj != null && prop != null && val != null) {
					addPendingValidationStatement(subj, prop, val);
					getJenaModel().add(subj, prop, val);
				}
			}
		}
		catch (ConfigurationException e) {
			addError(part, "Unexpected error adding statement: " + e.getMessage());
		}
		return getErrorsFinal();
	}

	public List<Object> ask(ConceptName subject, ConceptName property)
			throws InvalidNameException {
		OntResource subj;
		Property prop;
		if (subject.getType() == null) {
			subject = validateConceptName(subject);
		}
		if (property.getType() == null) {
			property = validateConceptName(property);
		}
		if (subject.getType().equals(ConceptType.INDIVIDUAL)) {
			subj = getIndividual(subject);
			prop = getOntProperty(property);
		}
		else {
			subj = getJenaModel().getOntResource(subject.getUri());
			prop = getJenaModel().getOntProperty(property.getUri());
		}
		if (subj == null) {
			return null;
		}
		if (prop == null) {
			if (property.getNamespace().equals(RDFS.getURI())) {
				prop = getJenaModel().getProperty(RDFS.getURI() + property.getName());
			}
			else if (property.getNamespace().equals(RDF.getURI())) {
				prop = getJenaModel().getProperty(RDF.getURI() + property.getName());
			}
			else if (property.getNamespace().equals(OWL.getURI())) {
				prop = getJenaModel().getProperty(OWL.getURI() + property.getName());
			}
			else {
				return null;
			}
		}
		NodeIterator nitr = subj.listPropertyValues(prop);
		List<Object> results = null;
		while (nitr.hasNext()) {
			RDFNode objNode = nitr.next();
			if (results == null) {
				results = new ArrayList<Object>();
			}
			if (objNode.canAs(Individual.class)) {
				Individual inst = objNode.as(Individual.class);
				if (namespaceInBaseModel(inst.getNameSpace())) {
					results.add(new ConceptName(inst.getLocalName()));
				} else {
					String prefix = getJenaModel().getNsURIPrefix(
							inst.getNameSpace());
					results.add(new ConceptName(prefix, inst.getLocalName()));
				}
			} else if (objNode instanceof Literal) {
				results.add(((Literal) objNode).getValue());
			}
		}
		return results;
	}

	/**
	 * Call this method to see if there is a concept in the model matching the
	 * identifier name.
	 * 
	 * @param name
	 *            -- the name of the concept
	 * @return The type of token that an identifier is mapped to else
	 *         ConceptType.CONCEPT_NOT_FOUND_IN_MODEL.
	 */
	public ConceptType getConceptType(String name) {
		ConceptName cn = new ConceptName(name);
		if (isRDFDataType(cn)) {
			return ConceptType.RDFDATATYPE;
		}
		Resource r;
		try {
			r = getOntResourceInExistingModel(cn);
			if (r != null) {
				return getOntResourceConceptType(r);
			}
		} catch (ConfigurationException e) {
			logger.error("Unexpected error getting concept type of '" + name + "': " + e.getMessage());
			e.printStackTrace();
		}
		return ConceptType.CONCEPT_NOT_FOUND_IN_MODEL;
	}

	/**
	 * This method takes a ConceptName as input and tries to find a matching
	 * concept in the underlying OWL model. If one is found, the namespace,
	 * prefix, and type are filled in and the ConceptName is returned. If a
	 * match is not found, the type is set to CONCEPT_NOT_FOUND_IN_MODEL and the
	 * ConceptName is still returned.
	 * 
	 * @param name
	 * @return
	 */
	public ConceptName validateConceptName(ConceptName name) {
		if (name.getNamespace() != null
				&& name.getType() != null
				&& !name.getType().equals(
						ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
			// this is already valid
			return name;
		}

		String key = name.toString();
		if (cachingLevel > 0) {
			if (variableNamesCache.containsKey(key)) {
				ConceptName cached = variableNamesCache.get(key);
				name.setType(cached.getType());
				name.setNamespace(cached.getNamespace());
				return name;
			}
			if (cachingLevel > 1 && conceptNamesCache.containsKey(key)) {
				ConceptName cached = conceptNamesCache.get(key);
				name.setPrefix(cached.getPrefix());
				name.setNamespace(cached.getNamespace());
				name.setType(cached.getType());
				return name;
			}
		}

		Resource r;
		try {
			r = getOntResourceInExistingModel(name);
		} catch (ConfigurationException e1) {
			addError(0, "Error validating name '" + name.toString() + "': " + e1.getMessage());
			name.setType(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL);
			return name;
		}
		if (r != null) {
			name.setType(getOntResourceConceptType(r));
			name.setNamespace(r.getNameSpace());
		}
		else {
			name.setType(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL);
		}
		if (cachingLevel > 1 && !name.getType().equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
			conceptNamesCache.put(key, name);
		}
		return name;
	}
	
	private Resource getOntResourceInExistingModel(ConceptName name) throws ConfigurationException {
		String uri = null;
		// first handle explicit namespace or prefix
		if (name.getNamespace() != null) {
			uri = getUri(name.getNamespace(), name.getName());
		} else if (name.getPrefix() != null) {
			try {
				String ns = findPrefixInModel(name);
				uri = getUri(ns, name.getName());
			} catch (PrefixNotFoundException e) {
				logger.error("Prefix '" + name.getPrefix()
						+ "' was not found in model.");
				addError(0, "Prefix '" + name.getPrefix() + "' was not found.");
				name.setType(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL);
				return null;
			}
		}
		// we have an explicit namespace--see if we can find it.
		if (uri != null) {
			if (uri.equals(RDF.type.getURI())) {
				return RDF.type;
			}
			String base = getUriXmlBase(uri);
			OntResource rsrc = null;
			if (base.equals(getModelName())) {
				rsrc = getJenaModel().getOntResource(uri);
			}
			else {
				rsrc = getOntResourceInNamedModel(name, base);
			}
			if (rsrc != null) {
				return rsrc;
			}
			logger.debug("Resource '" + uri + "' not found in model or imports.");
			name.setType(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL);
			return null;
		}
		else {
			Resource rdfsrsrc = getRdfsAnnotationProperty(name);
			if (rdfsrsrc != null) {
				logger.debug("ConceptName '" + name.toString() + "' found to be an RDFS AnnotationProperty '" + rdfsrsrc.toString() + "'");
				return rdfsrsrc;
			}
			// look in the local model
			String ns = getModelName();
			if (ns == null) {
				logger.error("Model does not have a namespace");
				return null;
			}
			uri = getUri(getModelNamespace(), name.getName());
			OntModel m = getJenaModel();
			OntResource r;
			if (m != null) {
				r = getJenaModel().getOntResource(uri);
				if (r != null) {
					logger.debug("ConceptName '" + name.toString() + "' found to be in this model: '" + r.toString() + "'");
					name.setNamespace(getModelNamespace());
					return r;
				}
			}
			else {
				logger.error("MM failed to retrieve an OntModel for the current Resource: " + getModelActualUrl().toString());
			}

			// must look in imported models
			List<String> importUris = getOrderedImportUris();
			logger.debug("Ready to look for '" + name.toString() + "' in " + (importUris == null ? 0 : importUris.size()) + " imported models.");
			for (int i = 0; importUris != null && i < importUris.size(); i++) {
				String imp = importUris.get(i);
				r = getOntResourceInNamedModel(name, imp);
				if (r != null) {
					name.setNamespace(imp + "#");
					return r;
				}
			}
		}
		logger.debug("ConceptName '" + name.toString() + "' not found in any model.");
		name.setType(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL);
		return null;
	}
	
	private String getUriXmlBase(String uri) {
		int hashAt = uri.indexOf("#");
		if (hashAt > 0) {
			return uri.substring(0, hashAt);
		}
		return uri;
	}

	private OntResource getOntResourceInNamedModel(ConceptName name, String modelUri) {
		OntResource r = null;
		String nameUri = getUri(modelUri + "#", name.getName());
		if (imports != null && imports.size() > 0) {
			ImportMapping im = imports.get(modelUri);
			if (im != null) {
				OntModel impModel = im.getModel();
				if (impModel != null) {
					r = impModel.getOntResource(nameUri);
				}
				else {
					logger.debug("ConceptName '" + name.toString() + "' not found in model '" + modelUri + "' as OntModel is null.");
				}
				if (r != null) {
					logger.debug("ConceptName '" + name.toString() + "' found in imported model: '" + r.getNameSpace() + "'");
					setOrderedImportUris(modelUri);
					return r;
				}
			}
			else {
				logger.debug("Import mapping for model '" + modelUri + "' not found.");
				r = getJenaModel().getOntResource(nameUri);
			}
		}
		else {
			logger.debug("There are no import mappings!");
			r = getJenaModel().getOntResource(nameUri);
		}
		return r;
	}

	private Resource getRdfsAnnotationProperty(ConceptName name) {
		if (name.getNamespace() == null
				|| name.getNamespace().equals(RDFS.getURI())) {
			if (RDFS.comment.getLocalName().equals(name.getName())) {
				return RDFS.comment;
			} else if (RDFS.label.getLocalName().equals(name.getName())) {
				return RDFS.label;
			} else if (RDFS.seeAlso.getLocalName().equals(name.getName())) {
				return RDFS.seeAlso;
			}
		}
		return null;
	}
	
	private ConceptType getOntResourceConceptType(Resource r) {
		ConceptType ctype = ConceptType.CONCEPT_NOT_FOUND_IN_MODEL;
		if (r != null) {
			if (r instanceof Individual) {
				ctype = ConceptType.INDIVIDUAL;
			}
			else if (r.canAs(DatatypeProperty.class)){
				ctype = ConceptType.DATATYPEPROPERTY;
			}
			else if (r.canAs(ObjectProperty.class)) {
				ctype = ConceptType.OBJECTPROPERTY;
			}
			else if (r.canAs(OntClass.class)) {
				ctype = ConceptType.ONTCLASS;
			}
			else if (r.canAs(AnnotationProperty.class)) {
				ctype = ConceptType.ANNOTATIONPROPERTY;
			}
			else if (r.getNameSpace().equals(OWL.NAMESPACE.getNameSpace()) || r.getNameSpace().equals(RDFS.getURI())) {            	 
				// this is an OWL or RDFS concept--requires special handling
				if (r.equals(RDFS.comment) || r.equals(RDFS.label) || r.equals(RDFS.seeAlso)) {
					ctype = ConceptType.ANNOTATIONPROPERTY;
				}
				else if (Character.isLowerCase(r.getLocalName().charAt(0))) {
					ctype = ConceptType.OBJECTPROPERTY;
				}
				else {
					ctype = ConceptType.ONTCLASS;
				}
				//                              if (r.equals(OWL.allValuesFrom) || r.equals(OWL.))
			}
			else if (r.equals(RDFS.subClassOf)) {
				ctype = ConceptType.OBJECTPROPERTY;
			}
			else if (r.canAs(Individual.class)) {
				ctype = ConceptType.INDIVIDUAL;
			}
			else if (r.equals(RDF.type)){
				ctype = ConceptType.OBJECTPROPERTY;
			}
		}
		return ctype;
	}

	/**
	 * Call this method to return the namespace of this model.
	 * 
	 * @return -- namespace of this model
	 */
	public String getModelNamespace() {
		return modelNamespace;
	}

	/**
	 * Call this method to get the fully qualified name (URI) of a model
	 * Resource. Names can be of the form:
	 * 
	 * 1. http://com.ge.sadl.mymodel2#myresource -- this will be returned
	 * unchanged 2. nsprefix:myresource -- the prefix nsprefix will be looked up
	 * in the model and replaced with the complete namespace. If nsprefix is the
	 * prefix for http://com.ge.sadl.mymodel2#, then this will return
	 * http://com.ge.sadl.mymodel2#myresource 3. myresource -- in this case it
	 * is assumed that the localname is in the base namespace of this model. If
	 * the base namespace for the current model is http://com.ge.sadl.mymodel,
	 * the this will return http://com.ge.sadl.mymodel#myresource
	 * 
	 * @param name
	 * @return
	 * @throws PrefixNotFoundException
	 */
	public String getUri(String name) throws PrefixNotFoundException {
		if (name != null) {
			String localname = name.substring(Util.splitNamespace(name));
			if (((name.length() - localname.length()) == 1)
					|| (localname.length() == 0)) {
				// this is just a localname
				return getModelNamespace() + name;
			}
			String namespace = name.substring(0, Util.splitNamespace(name));
			if (namespace != null && namespace.length() < name.length()) {
				// check the namespace to see if it is a prefix and needs to be
				// mapped to a namespace
				int colonLoc = name.indexOf(":");
				if (colonLoc > 0) {
					if (getJenaModel() != null) {
						String prefix = name.substring(0, colonLoc);
						String ns = getNamespaceFromPrefix(prefix);
						if (ns == null) {
							// prefix was not found
							throw new PrefixNotFoundException(
									"No namespace was found for prefix '"
											+ prefix + "'");
						}
						return ns + name.substring(colonLoc + 1);
					}
				} else {
					return getModelNamespace() + name;
				}

			}
		}
		return name;
	}

	/**
	 * This method finds a namespace from a prefix in the current OntModel or
	 * any of its submodels
	 * 
	 * @param prefix
	 * @return
	 */
	public String getNamespaceFromPrefix(String prefix) {
		String ns = findPrefixInModel(getJenaModel(), prefix);
		return ns;
	}

	/**
	 * Call this method to get the fully qualified name (URI) of an existing
	 * model Resource. Names can be of the form:
	 * 
	 * 1. http://com.ge.sadl.mymodel2#myresource -- this will be returned
	 * unchanged 2. nsprefix:myresource -- the prefix nsprefix will be looked up
	 * in the model and replaced with the complete corresponding namespace. If
	 * nsprefix is the prefix for http://com.ge.sadl.mymodel2#, then the return
	 * will be http://com.ge.sadl.mymodel2#myresource 3. myresource -- in this
	 * case it is assumed that the localname is in the namespace of this model.
	 * If the namespace for the current model is http://com.ge.sadl.mymodel#,
	 * then the return will be http://com.ge.sadl.mymodel#myresource
	 * 
	 * @param name
	 * @return
	 * @throws PrefixNotFoundException
	 */
	public String getUri(ConceptName name) {
		if (name != null) {
			String localname = name.getName();
			if (name.getNamespace() != null) {
				return getUri(name.getNamespace(), name.getName());
			}
			if (name.getPrefix() != null) {
				String ns;
				try {
					ns = findPrefixInModel(name);
				} catch (PrefixNotFoundException e) {
					return name.toString();
				}
				return getUri(ns, localname);
			}
			return getUri(getModelNamespace(), localname);
		}
		return null;
	}

	/**
	 * Call this method to get the fully qualified name (URI) of an existing
	 * model Resource. Names can be of the form:
	 * 
	 * 1. http://com.ge.sadl.mymodel2#myresource -- this will be returned
	 * unchanged 2. nsprefix:myresource -- the prefix nsprefix will be looked up
	 * in the model and replaced with the complete corresponding namespace. If
	 * nsprefix is the prefix for http://com.ge.sadl.mymodel2#, then the return
	 * will be http://com.ge.sadl.mymodel2#myresource 3. myresource -- in this
	 * case it is assumed that the localname is in the namespace of this model.
	 * If the namespace for the current model is http://com.ge.sadl.mymodel#,
	 * then the return will be http://com.ge.sadl.mymodel#myresource
	 * 
	 * @param namespace
	 * @param localname
	 * @return
	 * @throws PrefixNotFoundException
	 */
	public String getUri(String namespace, String localname) {
		return namespace + localname;
	}

	/**
	 * Call this method to get a list of ExistingNames in the specified
	 * namespace filtered by ConceptType
	 * 
	 * @param modelname
	 * @param ctype
	 * @return
	 * @throws InvalidNameException
	 * @throws ConfigurationException
	 * @throws MalformedURLException 
	 */
	public List<ConceptName> getNamedConceptsInNamedModel(String modelname,
			ConceptType ctype) throws InvalidNameException,
			ConfigurationException, MalformedURLException {
		if (modelname == null) {
			// get the matching concept names in all imported namespaces
			return getNamedConceptsOfType(ctype, Scope.INCLUDEIMPORTS);
		}

		if (modelname.equals(modelName)) {
			// we're getting the concept names in this ModelManager's namespace
			// only
			return getNamedConceptsOfType(ctype, Scope.LOCALONLY);
		}

		OntModel om = null;

		if (imports != null) {
			ImportMapping im = imports.get(modelname);
			if (im != null) {
				om = im.getModel();
			}
		}

		if (om != null) {
			return getNamedConceptsInModel(om, modelname, ctype,
					Scope.INCLUDEIMPORTS); // need indirect imports as well
		} else {
//			logger.error("No OntModel found for namespace '" + modelname + "'"); // this can happen on a non-builder thread during build.
		}
		return new ArrayList<ConceptName>(0);
	}

	/**
	 * Call this method to get a List of ExistingNames which match the
	 * conditions specified by the arguments.
	 * 
	 * @param cType
	 *            -- the ConceptType desired (see ConceptType Enum)
	 * @param scope
	 *            -- the scope: LOCALONLY for this model only else
	 *            INCLUDEIMPORTS
	 * @return
	 * @throws InvalidNameException
	 * @throws ConfigurationException
	 * @throws MalformedURLException 
	 */
	public List<ConceptName> getNamedConceptsOfType(ConceptType cType,
			Scope scope) throws InvalidNameException, ConfigurationException, MalformedURLException {
		if (cType != null && cType.equals(ConceptType.MODELNAME)) {
			// this is to get the modelname only
			return getNamedConceptsInModel(null, this.modelName, cType, scope);
		} else {
			// get concepts in current model
			List<ConceptName> names = getConfigurationMgr() != null ? getConfigurationMgr().getNamedConceptsInModel(getJenaModel(), getModelName(), cType, scope) : new ArrayList<ConceptName>();

			if (scope.equals(Scope.INCLUDEIMPORTS) && imports != null && imports.size() > 0) {
				// get concepts in imports
				Iterator<ImportMapping> imiter = getImportMappings().iterator();
				while (imiter.hasNext()) {
					ImportMapping im = imiter.next();
					if (im.getModel() != null) {
						List<ConceptName> imNames = getConfigurationMgr().getNamedConceptsInModel(im.getModel(), im.getPublicURI(), cType, scope);
						for (int i = 0; imNames != null && i < imNames.size(); i++) {
							if (!names.contains(imNames.get(i))) {
								names.add(imNames.get(i));
							}
						}
					}
				}
			}
			return names;
		}
	}

	/**
	 * Method to return all of the Individuals in the specified Scope belonging
	 * to the specified class.
	 * 
	 * @param constraint
	 * @param scope
	 * @return
	 */
	public List<ConceptName> getNamedConceptsOfType(ConceptType cType,
			Scope scope, ConceptName constraint) {
		try {
			if (cType.equals(ConceptType.INDIVIDUAL)) {	
				Resource cr = getOntResourceInExistingModel(constraint);
				if (cr != null) {
					ConceptType ct = getOntResourceConceptType(cr);
					if (ct.equals(ConceptType.ONTCLASS)) {
						OntClass ontClss = cr.as(OntClass.class);
						ExtendedIterator<? extends OntResource> eitr = ontClss
								.listInstances();
						if (eitr.hasNext()) {
							List<ConceptName> instances = new ArrayList<ConceptName>();
							while (eitr.hasNext()) {
								OntResource or = eitr.next();
								if (or.canAs(Individual.class)) {
									Individual inst = or.asIndividual();
									if (scope.equals(Scope.INCLUDEIMPORTS)
											|| inst.getNameSpace().equals(
													getModelNamespace())) {
										ConceptName newCN = new ConceptName(
												inst.getLocalName());
										newCN.setNamespace(inst.getNameSpace());
										newCN.setType(ConceptType.INDIVIDUAL);
										instances.add(newCN);
									}
								}
							}
							return instances;
						}
					}
				}
			} else if (cType.equals(ConceptType.ONTCLASS)) {
				Resource cr = getOntResourceInExistingModel(constraint);
				if (cr != null) {
					ConceptType ct = getOntResourceConceptType(cr);
					if (ct.equals(ConceptType.ONTCLASS)) {
						OntClass ontClss = cr.as(OntClass.class);
						ExtendedIterator<? extends OntClass> eitr = ontClss
								.listSubClasses();
						if (eitr.hasNext()) {
							List<ConceptName> list = new ArrayList<ConceptName>();
							while (eitr.hasNext()) {
								OntClass cclss = eitr.next();
								if (scope.equals(Scope.INCLUDEIMPORTS)
										|| cclss.getNameSpace().equals(getModelNamespace())) {
									ConceptName newCN = new ConceptName(
											cclss.getLocalName());
									newCN.setNamespace(cclss.getNameSpace());
									newCN.setType(ConceptType.ONTCLASS);
									list.add(newCN);
								}
							}
							return list;
						}
					}
				}
			} else if (cType.equals(ConceptType.DATATYPEPROPERTY)
					|| cType.equals(ConceptType.OBJECTPROPERTY)) {
				Resource pr;
				pr = getOntResourceInExistingModel(constraint);
				if (pr != null) {
					ConceptType pt = getOntResourceConceptType(pr);
					if (pt.equals(ConceptType.DATATYPEPROPERTY) || pt.equals(ConceptType.OBJECTPROPERTY)) {
						OntProperty ontProp = pr.as(OntProperty.class);
						ExtendedIterator<? extends OntProperty> eitr = ontProp
								.listSubProperties();
						if (eitr.hasNext()) {
							List<ConceptName> list = new ArrayList<ConceptName>();
							while (eitr.hasNext()) {
								OntProperty op = eitr.next();
								if (scope.equals(Scope.INCLUDEIMPORTS)
										|| op.getNameSpace().equals(getModelNamespace())) {
									ConceptName newCN = new ConceptName(op.getLocalName());
									newCN.setNamespace(op.getNameSpace());
									newCN.setType(ConceptType.ONTCLASS);
									list.add(newCN);
								}
							}
							return list;
						}
					}
				}
			}
		} catch (ConfigurationException e) {
			addError(new ModelError("Unexpected error getting named concepts of type " + cType.toString(), ErrorType.ERROR));
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Method to find a Resource in this model by URI and return it as a
	 * ConceptName
	 * 
	 * @param uri
	 *            -- the concept URI string
	 * @return
	 */
	public static ConceptName getConceptByUri(OntModel m, String uri) {
		if (m == null) {
			return null;	// this may happen on startup
		}
		ConceptType ctype = null;
		Resource r;
		if (uri.equals(RDF.type.getURI())) {
			r = RDF.type;
			ctype = ConceptType.OBJECTPROPERTY;
		}
		else {
			r = m.getOntResource(uri);
			if (r != null) {
				if (r instanceof Individual) {
					ctype = ConceptType.INDIVIDUAL;
				}
				else if (r.canAs(DatatypeProperty.class)){
					ctype = ConceptType.DATATYPEPROPERTY;
				}
				else if (r.canAs(ObjectProperty.class)) {
					ctype = ConceptType.OBJECTPROPERTY;
				}
				else if (r.canAs(OntClass.class)) {
					ctype = ConceptType.ONTCLASS;
				}
				else if (r.canAs(AnnotationProperty.class)) {
					ctype = ConceptType.ANNOTATIONPROPERTY;
				}
				else if (r.canAs(Individual.class)) {
					ctype = ConceptType.INDIVIDUAL;
				}
			}
		}
		if (r == null) {
			if (uri.equals(RDFS.label.getURI())) {
				r = RDFS.label;
				ctype = ConceptType.ANNOTATIONPROPERTY;
			}
			else if (uri.equals(RDFS.domain.getURI())) {
				r = RDFS.domain;
				ctype = ConceptType.OBJECTPROPERTY;
			}
			else if (uri.equals(RDFS.range.getURI())) {
				r = RDFS.range;
				ctype = ConceptType.OBJECTPROPERTY;
			}
			else if (uri.equals(OWL.Class.getURI())) {
				r = OWL.Class;
				ctype = ConceptType.ONTCLASS;
			}
			else if (uri.equals(OWL.DatatypeProperty.getURI())) {
				r = OWL.DatatypeProperty;
				ctype = ConceptType.ONTCLASS;
			}
			else if (uri.equals(OWL.ObjectProperty.getURI())) {
				r = OWL.ObjectProperty;
				ctype = ConceptType.ONTCLASS;
			}
		}
		if (r != null) {
			ConceptName cn = new ConceptName(r.getLocalName());
			cn.setNamespace(r.getNameSpace());
			cn.setType(ctype);
			return cn;
		}
		return null;
	}

	public List<ConceptName> getPossibleModelNames() {
		List<ConceptName> lst = new ArrayList<ConceptName>();
		if (modelName == null) {
			// this model hasn't been named yet--use default baseUri
			// provided by SadlProposalProvider
			if (getModelBaseUri() == null) {
				logger.debug("This shouldn't happen--getNamedConceptsInModel called for MODELNAME but modelBaseUri hasn't been set.");
				try {
					throw new Exception("modelBaseUri not set");
				} catch (Exception e) {
					e.printStackTrace();
				}
			} else {
				// this of course only makes sense when called to get the model
				try {
					lst.add(new ConceptName(null, modelBaseUri));
				} catch (InvalidNameException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		} else {
			// this model has a name so this should return the name of
			// all other models that might be imported
			List<String> possibleImports = null;
			try {
				HashMap<String, String> mappings = getConfigurationMgr()
						.getMappings();
				if (mappings.size() > 0) {
					Iterator<String> mitr = mappings.keySet().iterator();
					if (mitr.hasNext()) {
						possibleImports = new ArrayList<String>();
						while (mitr.hasNext()) {
							String uri = mitr.next();
							if (!uri.equals(OWL.NAMESPACE.toString())
									&& !uri.equals(ResourceManager.ACUITY_DEFAULTS_URI)) {
								possibleImports.add(uri);
							}
						}
					}
				} else {
					possibleImports = ResourceManager
							.findAllKnownImports(getModelActualUrl());
				}
			} catch (IOException e) {
				e.printStackTrace();
				addError(new ModelError(
						"Unexpected error getting known imports: "
								+ e.getLocalizedMessage(), ErrorType.ERROR));
			} catch (URISyntaxException e) {
				e.printStackTrace();
				addError(new ModelError(
						"Policy file syntax error getting known imports: "
								+ e.getLocalizedMessage(), ErrorType.ERROR));
			} catch (ConfigurationException e) {
				addError(new ModelError(
						"Policy file syntax error getting known imports: "
								+ e.getLocalizedMessage(), ErrorType.ERROR));
			}
			// remove this model
			if (possibleImports != null) {
				if (possibleImports.contains(modelName)) {
					possibleImports.remove(modelName);
				}
				if (imports != null) {
					Iterator<String> itr = imports.keySet().iterator();
					while (itr.hasNext()) {
						String imp = itr.next();
						possibleImports.remove(imp);
					}
				}

				// determine from preferences whether to use URI or SADL file
				// name
				Iterator<String> piitr = possibleImports.iterator();
				while (piitr.hasNext()) {
					String mname = piitr.next();
					if (getImportListType().equals(
							ImportListType.NAME_AS_SADL_FILENAME)) {
						// trade the name in for a SADL filename
						try {
							String altUrl = getConfigurationMgr()
									.getAltUrlFromPublicUri(mname);
							if (altUrl != null) {
								File owlFile = new File(
										SadlUtils
												.fileUrlToFileName(altUrl));
								if (owlFile.exists()) {
									mname = owlFile.getName();
									if (mname
											.indexOf(ResourceManager.getOwlFileExtensionWithPrefix()) > 0) {
										mname = mname.substring(0,
												mname.indexOf('.'))
												+ ResourceManager.SADLEXTWITHPREFIX;
									}
								}
							}
						} catch (ConfigurationException e) {
							e.printStackTrace();
							addError(new ModelError(
									"Unexpected error: "
											+ e.getLocalizedMessage(), ErrorType.ERROR));
						} catch (MalformedURLException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
					lst.add(new ConceptName(mname));
				}
			}
		}
		return lst;

	}

	/**
	 * Get the concept names of everything in this model with this namespace and
	 * matching type
	 * 
	 * @param localModel
	 * @param modelname
	 * @param cType
	 * @param scope
	 * @return
	 * @throws InvalidNameException
	 * @throws ConfigurationException
	 * @throws MalformedURLException 
	 */
	private List<ConceptName> getNamedConceptsInModel(OntModel localModel,
			String modelname, ConceptType cType, Scope scope)
			throws InvalidNameException, ConfigurationException, MalformedURLException {
		logger.debug("getNamedConceptsInModel called with ConceptType '"
				+ cType + "'");
		if (cType == ConceptType.MODELNAME) {
			throw new InvalidNameException(
					"This should not have been called from here!");
		}
		return getConfigurationMgr().getNamedConceptsInModel(getJenaModel(), getModelName(), cType, scope);

	}

	/**
	 * Call this method when the model should be saved to an OWL disk file. The Jena OWL model, in the specified format,
	 * is saved by call to saveJenaModelForSadlIde(...). The rules are save by call to the currently configured translator 
	 * plug-in's translateAndSaveModel(OntModel model, List<Rule> rules, String modelFolder, String modelName, String owlFileName).
	 * 
	 * @param fullyQualifiedOwlFilename
	 * @param format
	 * @return -- a list of ModelError instances if errors occurred else null
	 */
	public List<ModelError> save(String fullyQualifiedOwlFilename) {
		if (getJenaModel() != null) {
			try {
				File owlFile = new File(fullyQualifiedOwlFilename);
				String modelFolderName = owlFile.getParentFile()
						.getCanonicalPath();
				List<ModelError> saveResults = saveJenaModelForSadlIde(owlFile);
				if (saveResults != null) {
					for (int i = 0; i < saveResults.size(); i++) {
						addError(saveResults.get(i));
					}
				} else {
					String format = ConfigurationManagerForIDE.getOWLFormat();
					ITranslator translator = getConfigurationMgr()
							.getTranslator();
					List<ModelError> results = translator
							.translateAndSaveModel(getJenaModel(), rules,
									modelFolderName, getModelName(),
									owlFile.getName());
					if (results != null) {
						for (int i = 0; i < results.size(); i++) {
							addError(results.get(i));
						}
					}
				}
				ResourceManager.refreshResource(modelFolderName);
			} catch (IOException e) {
				e.printStackTrace();
				addError(0, "Error saving OWL model to '"
						+ fullyQualifiedOwlFilename + "': " + e.getMessage());
			} catch (TranslationException e) {
				addError(0, "Translation error saving OWL model to '"
						+ fullyQualifiedOwlFilename + "': " + e.getMessage());
				e.printStackTrace();
			} catch (URISyntaxException e) {
				e.printStackTrace();
				addError(0, "Error saving OWL model to '"
						+ fullyQualifiedOwlFilename + "': " + e.getMessage());
			} catch (ConfigurationException e) {
				e.printStackTrace();
				addError(0, "Error saving OWL model to '"
						+ fullyQualifiedOwlFilename + "': " + e.getMessage());
			}
		} else {
			addError(0, "No Jena OntModel exists; cannot save OWL model.");
		}
		try {
			getConfigurationMgr().clearReasoner();
		} catch (ConfigurationException e) {
			addError(0, "Unexpected erorr: " + e.getMessage());
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return getErrorsFinal();
	}

	private List<ModelError> saveJenaModelForSadlIde(File owlFile)
			throws IOException, URISyntaxException, ConfigurationException {
		List<ModelError> errors = null;
		if (getJenaModel() == null) {
			return addError(errors, new ModelError(
					"Cannot save model in file '" + owlFile.getCanonicalPath()
							+ "' as it has no model.", ErrorType.ERROR));
		}
		if (getModelName() == null) {
			return addError(errors, new ModelError(
					"Cannot save model in file '" + owlFile.getCanonicalPath()
							+ "' as it has no name.", ErrorType.ERROR));
		}

		// check to see if the file being saved is a duplicate SADL filename
		URI actualUri = getModelActualUrl();
		String dupFilename = getConfigurationMgr().checkForDuplicateSadlFile(
				actualUri);
		if (dupFilename != null) {
			return addError(errors, new ModelError("Can't save model file '"
					+ actualUri.toString()
					+ "' because it duplicates the file name of '"
					+ dupFilename, ErrorType.ERROR));
		}

    	String fullyQualifiedOwlFilename = getOwlModelsFolderPath()
				+ File.separator + owlFile.getName();

    	SadlJenaModelGetter modelGetter = (SadlJenaModelGetter) getConfigurationMgr().getModelGetter();
    	if (!(modelGetter instanceof SadlJenaModelGetterPutter)) {
    		return addError(errors, new ModelError("Unable to save model as the model getter is not also a putter.", ErrorType.ERROR));
    	}
    	
//    	getConfigurationMgr();
		String format = ConfigurationManagerForIDE.getOWLFormat();
//    	if (format.equals(IConfigurationManager.OWL_Func_SWRL) || owlFile.getName().equals(ConfigurationManager.ServicesConf_FN)) {
//    		// the services config file is always written to an OWL file in the OwlModels folder
//    		format = IConfigurationManager.RDF_XML_ABBREV_FORMAT;
//    	}
    	((SadlJenaModelGetterPutter)modelGetter).configureToModel(getJenaModel());
    	if (!((SadlJenaModelGetterPutter)modelGetter).saveModel(getJenaModel().getBaseModel(), getModelNamespace(), getModelName(), fullyQualifiedOwlFilename, format)) {
			logger.error("Failed to saving model file '" + modelName
					+ "' to '" + owlFile + "'");
			return addError(errors,
					new ModelError("Failed to save model to file '" + owlFile
							+ "' ", ErrorType.ERROR));
     	}
    	else {
			try {
				getConfigurationMgr()
						.addMapping(
								SadlUtils
										.fileNameToFileUrl(fullyQualifiedOwlFilename),
								modelName, globalPrefix, IConfigurationManager.SADL);
			} catch (Exception e) {
				return addError(errors, new ModelError(
						"Failed to save mapping for model file '" + owlFile
								+ "': " + e.getLocalizedMessage(),
						ErrorType.ERROR));
			}
    		getConfigurationMgr().replaceJenaModelCache(getJenaModel(), modelName);
		}
		return getErrorsFinal();
	}

	/**
	 * This method dumps the model to the output stream in the specified
	 * language.
	 * 
	 * @param out
	 * @param fmt
	 *            .to
	 */
	public void dump(PrintStream out, String lang) {
		getJenaModel().write(out, lang);
	}

	/**
	 * Call this method to combine lists of ModelError
	 * 
	 * @param errors
	 * @param moreErrors
	 * @return
	 */
	public static List<ModelError> combineErrors(List<ModelError> errors,
			List<ModelError> moreErrors) {
		if (moreErrors != null) {
			if (errors != null) {
				errors.addAll(moreErrors);
			} else {
				errors = moreErrors;
			}
		}
		return errors;
	}

	/**
	 * Call this method to add one more error to a list of ModelErrors
	 * 
	 * @param errors
	 * @param newError
	 * @return
	 */
	public static List<ModelError> addError(List<ModelError> errors,
			ModelError newError) {
		if (newError != null) {
			if (errors == null) {
				errors = new ArrayList<ModelError>();
			}
			errors.add(newError);
		}
		return errors;
	}

	/**
	 * Method to return the ImportMapping for a given prefix (alias)
	 * 
	 * @param alias
	 * @return
	 */
	public List<ImportMapping> getImportMappings() {
		List<ImportMapping> mappings = null;
		if (imports != null) {
			mappings = new ArrayList<ImportMapping>();
			Iterator<String> itr = imports.keySet().iterator();
			while (itr.hasNext()) {
				mappings.add(imports.get(itr.next()));
			}
		}
		return mappings;
	}

	/**
	 * Method to add a Query to the list of Queries defined in the model
	 * 
	 * @param query
	 *            -- the new Query
	 * 
	 * @return-- a list of the errors encountered in the Query
	 */
	public List<ModelError> addQuery(Query query) {
		List<ModelError> errors = validateQuery(query);
		if (errors == null) {
			addSadlCommand(query);
		}
		return getErrorsFinal();
	}

	public List<ModelError> addExplain(Explain explain) {
		List<ModelError> errors = validateExplain(explain);
		if (errors == null) {
			addSadlCommand(explain);
		}
		return getErrorsFinal();
	}

	/**
	 * Method to add a Rule to the list of Rules defined in the model
	 * 
	 * @param rule
	 *            -- the new Rule
	 * 
	 * @return-- a list of the errors encountered in the Rule
	 */
	public List<ModelError> addRule(Rule rule) {
		validateRule(rule);
		if (rules == null) {
			rules = new ArrayList<Rule>();
		}
		rules.add(rule);
		return getErrorsFinal();
	}

	/**
	 * Method to add a Test to the list of Tests defined in the model
	 * 
	 * @param test
	 *            -- the new Test
	 * 
	 * @return -- a list of the errors encountered in the Test
	 */
	public List<ModelError> addTest(Test test) {
		List<ModelError> errors = validateTest(test);
		if (errors == null) {
			if (test.getLhs() instanceof List<?> && test.getRhs() == null) {
				List<?> testlist = (List<?>) test.getLhs();
				boolean compoundTest = true;
				for (int i = 0; i < testlist.size(); i++) {
					Object alhs = testlist.get(i);
					if (!(alhs instanceof TripleElement)
							|| ((TripleElement) alhs).getSubject() instanceof VariableNode
							|| ((TripleElement) alhs).getObject() instanceof VariableNode) {
						compoundTest = false;
						break;
					}
				}
				if (compoundTest) {
					// compound test--split it up
					for (int i = 0; i < testlist.size(); i++) {
						Object alhs = testlist.get(i);
						Test atest = new Test();
						atest.setCompName(test.getCompName());
						atest.setLength(test.getLength());
						atest.setLineNo(test.getLineNo());
						atest.setOffset(test.getOffset());
						atest.setLhs(alhs);
						addSadlCommand(atest);
					}
				} else {
					addSadlCommand(test);
				}
			} else {
				addSadlCommand(test);
			}
		}
		return getErrorsFinal();
	}

	public List<ModelError> addPrint(Print display) {
		if (display.getDisplayString() == null && display.getModel() == null) {
			addError(0,
					"Print must have a display string or a model identifier.");
		} else {
			addSadlCommand(display);
		}
		return getErrorsFinal();
	}

	/**
	 * Method to return an ordered list of all of the Rules defined in the model
	 * 
	 * @return
	 */
	public List<Rule> getModelRules() {
		return rules;
	}

	/************************ Private or protected methods **************************************/

	private List<ModelError> validateQuery(Query query) {
		// TODO
		return null;
	}

	private List<ModelError> validateExplain(Explain explain) {
		// TODO
		return null;
	}

	private int validateRule(Rule rule) {
		int numErrors = 0;
		if (rules != null) {
			for (int i = 0; i < rules.size(); i++) {
				if (rule.getRuleName() != null && rules.get(i).getRuleName() != null && 
						rules.get(i).getRuleName().equals(rule.getRuleName())) {
					addError(
							0,
							"Rule name '"
									+ rule.getRuleName()
									+ " is alredy used; rule names must be unique within a model.");
				}
			}
		}
		return numErrors;
	}

	private List<ModelError> validateTest(Test test) {
		// TODO
		return null;
	}

	/**
	 * Call this method to remove double quotes from the beginning and end of a
	 * string so quoted.
	 * 
	 * @param quotedString
	 *            -- the string from which quotes are to be removed
	 */
	public static String stripQuotes(String quotedString) {
		if (quotedString != null && !quotedString.isEmpty()) {
			if (quotedString.charAt(0) == '\"') {
				while (quotedString.charAt(0) == '\"') {
					quotedString = quotedString.substring(1);
				}
				while (quotedString.length() > 0
						&& quotedString.charAt(quotedString.length() - 1) == '\"') {
					quotedString = quotedString.substring(0,
							quotedString.length() - 1);
				}
			}
			else if (quotedString.charAt(0) == '\'') {
				while (quotedString.charAt(0) == '\'') {
					quotedString = quotedString.substring(1);
				}
				while (quotedString.length() > 0
						&& quotedString.charAt(quotedString.length() - 1) == '\'') {
					quotedString = quotedString.substring(0,
							quotedString.length() - 1);
				}
			}
		}
		return quotedString;
	}

	private void setJenaModel(OntModel jenaModel) {
		this.jenaModel = jenaModel;
		if (configurationMgr.getModelGetter() != null) {
			jenaModel.getSpecification().setImportModelGetter((ModelGetter) configurationMgr.getModelGetter());
		}
	}

	public void addError(ModelError error) {
		if (newErrors == null) {
			newErrors = new ArrayList<ModelError>();
		}
		if (!newErrors.contains(error)) {
			newErrors.add(error);
		}
	}

	private void addError(int argIndex, String string) {
		addError(new ModelError(argIndex, ExistingNamePart.NAME, string));
	}

	private void addWarning(int argIndex, String msg) {
		addError(new ModelError(argIndex, ExistingNamePart.NAME, msg,
				ErrorType.WARNING));
	}

	private void addError(int argIndex, ExistingNamePart nmpart, String string) {
		addError(new ModelError(argIndex, nmpart, string));
	}

	private void addError(int argIndex, int lstIndex, ExistingNamePart nmpart,
			String string) {
		addError(new ModelError(argIndex, lstIndex, nmpart, string));
	}

	private void addPendingError(int argIdx, int lstIdx, ConceptType cType,
			ConceptName cName, String msg) {
		if (cName == null) {
			logger.error("Shouldn't be calling addPendingError with null name. Please fix!");
		} else if (msg == null) {
			logger.error("Shouldn't be calling addPendingError with null message. Please fix!");
		}
		if (pendingErrors == null) {
			pendingErrors = new HashMap<String, PendingModelError>();
		}
		PendingModelError err = new PendingModelError(argIdx, lstIdx, cName,
				cType, msg);
		addError(err);
		pendingErrors.put(cName.toFQString(), err);
	}

	private boolean removePendingError(ConceptName cName, ConceptType cType) {
		PendingModelError pending = getPendingError(cName, cType);
		if (pending != null) {
			pendingErrors.remove(cName.toFQString());
			if (errors != null && errors.contains(pending)) {
				removeMatchingErrors(errors, pending);
			}
			if (newErrors != null && newErrors.contains(pending)) {
				removeMatchingErrors(newErrors, pending);
			}
			doAdditionalChecks(pending);
			return true;
		}
		return false;
	}

	private void removeMatchingErrors(List<ModelError> errorList,
			PendingModelError pending) {
		errorList.remove(pending);
		if (errorList.size() > 0) {
			for (int i = 0; i < errorList.size(); i++) {
				ModelError merr = errorList.get(i);
				if (merr instanceof PendingModelError
						&& ((PendingModelError) merr).getConceptName().equals(
								pending.getConceptName())
						&& ((PendingModelError) merr).getConceptType().equals(
								pending.getConceptType())
						&& merr.getErrorType().equals(pending.getErrorType())) {
					errorList.remove(i);
				}
			}
		}
	}

	private PendingModelError getPendingError(ConceptName cName,
			ConceptType cType) {
		if (pendingErrors != null) {
			PendingModelError pending = pendingErrors.get(cName.toFQString());
			if (pending != null && (pending.getConceptType().equals(cType)
					|| pending.getConceptType().equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL))) {
				return pending;
			}
		}
		return null;
	}

	/**
	 * Method to get a pending error by name only, for use in vaidation
	 * 
	 * @param conceptName
	 * @return
	 */
	public PendingModelError getPendingError(String conceptName) {
		if (pendingErrors != null) {
			if (pendingErrors.containsKey(conceptName)) {
				return pendingErrors.get(conceptName);
			}
			String uri;
			try {
				uri = getUri(conceptName);
				if (pendingErrors.containsKey(uri)) {
					return pendingErrors.get(uri);
				}
			} catch (PrefixNotFoundException e) {
				addError(new ModelError("Name '" + conceptName + "' not found.", ErrorType.ERROR));
			}
		}
		return null;
	}
	
	public int errors() {
		return errors(false);
	}

	public int errors(boolean bIncludePending) {
		int cnt = 0;
		if (errors != null && errors.size() > 0) {
			for (int i = 0; i < errors.size(); i++) {
				ModelError me = errors.get(i);
				if (me.getErrorType().equals(ErrorType.ERROR)) {
					if (pendingErrors == null || bIncludePending 
							|| !pendingErrors.containsValue(me)) {
						cnt++;
					}
				}
			}
		} else if (newErrors != null && newErrors.size() > 0) {
			for (int i = 0; i < newErrors.size(); i++) {
				ModelError me = newErrors.get(i);
				if (me.getErrorType().equals(ErrorType.ERROR)) {
					if (pendingErrors == null
							|| !pendingErrors.containsValue(me)) {
						cnt++;
					}
				}
			}
		}
		return cnt;
	}

	public int warnings() {
		int cnt = 0;
		if (errors != null && errors.size() > 0) {
			for (int i = 0; i < errors.size(); i++) {
				ModelError me = errors.get(i);
				if (me.getErrorType().equals(ErrorType.WARNING)) {
					if (pendingErrors == null
							|| !pendingErrors.containsValue(me)) {
						cnt++;
					}
				}
			}
		} else if (newErrors != null && newErrors.size() > 0) {
			for (int i = 0; i < newErrors.size(); i++) {
				ModelError me = newErrors.get(i);
				if (me.getErrorType().equals(ErrorType.WARNING)) {
					if (pendingErrors == null
							|| !pendingErrors.containsValue(me)) {
						cnt++;
					}
				}
			}
		}
		return cnt;
	}

	private void clearRules() {
		if (rules != null) {
			rules.clear();
		}
	}

	private void clearSadlCommands() {
		if (sadlCommands != null) {
			sadlCommands.clear();
		}
	}

	private List<ModelError> getErrorsFinal() {
		if (newErrors != null && newErrors.size() > 0) {
			if (errors == null) {
				errors = new ArrayList<ModelError>();
			}
			for (int i = 0; i < newErrors.size(); i++) {
				ModelError me = newErrors.get(i);
				if (!errors.contains(me)) {
					errors.add(me);
				}
			}
			List<ModelError> errorsToReturn = newErrors;
			newErrors = null;
			return errorsToReturn;
		}
		return null;
	}

	private void clearErrors() {
		if (errors != null) {
			errors.clear();
		}
		if (newErrors != null) {
			newErrors.clear();
		}
	}

	private void clearPendingErrors() {
		if (pendingErrors != null) {
			pendingErrors.clear();
		}
	}

	private String findPrefixInModel(ConceptName name)
			throws PrefixNotFoundException {
		if (name.getPrefix() == null) {
			return null;
		}
		searchedModels.clear();
		String ns = findPrefixInModel(getJenaModel(), name.getPrefix());
		if (ns == null) {
			throw new PrefixNotFoundException("Prefix '" + name.getPrefix()
					+ "' not found in model. " + getModelReference());
		}
		return ns;
	}

	private String findPrefixInModel(OntModel mdl, String prefix) {
		String ns = mdl.getNsPrefixURI(prefix);
		if (ns == null) {
			ns = findNsPrefixInImportedModels(mdl, prefix);
		}
		if (ns == null) {
			try {
				ns = getConfigurationMgr().getUriFromGlobalPrefix(prefix);
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return ns;
	}

	private String findNsPrefixInImportedModels(OntModel mdl, String prefix) {
		List<String> importUris;
		try {
			importUris = getOrderedImportUris();
			for (int i = 0; importUris != null && i < importUris.size(); i++) {
				String imp = importUris.get(i);
				if (imports != null) {
					ImportMapping im = imports.get(imp);
					if (im != null && !searchedModels.contains(im.getPublicURI())) {
						searchedModels.add(im.getPublicURI());
						OntModel impModel = imports.get(imp).getModel();
						if (impModel != null && !mdl.equals(impModel)) {
							String ns = findPrefixInModel(impModel, prefix);
							if (ns != null) {
								return ns;
							}
						}
					}
				}
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	private ExistingNamePart determineExistingNamePartOfError(ConceptName ename) {
		try {
			findPrefixInModel(ename);
		} catch (PrefixNotFoundException e) {
			return ExistingNamePart.PREFIX;
		}
		return ExistingNamePart.NAME;
	}

	private boolean updatePropertyDomain(int argIdx, OntProperty prop,
			ConceptIdentifier domain) {
		boolean retval = false;
		OntResource domainCls = conceptIdentifierToOntClass(argIdx, 0, domain);
		OntResource existingDomain = prop.getDomain();
		if (domainCls != null) {
			domainCls = addClassToUnionClass(existingDomain,
					(OntClass) domainCls);
			if (existingDomain != null) {
				if (!existingDomain.equals(domainCls)) {
					prop.removeDomain(existingDomain);
					prop.addDomain(domainCls);
				}
				retval = true; // return true if it was already the domain
			} else {
				prop.addDomain(domainCls);
				retval = true;
			}
		} else {
			addError(argIdx, ExistingNamePart.NOTAPPLICABLE,
					"Unable to convert domain identifier (" + domain
							+ ") to a class.");
		}
		return retval;
	}

	private boolean updateObjectPropertyRange(int argIdx, OntProperty prop,
			ConceptIdentifier range) {
		boolean retval = false;
		OntResource rangeCls = null;
		OntResource existingRange = prop.getRange();
		if (range instanceof ConceptName) {
			rangeCls = getOntClass((ConceptName) range);
			if (rangeCls == null) {
				rangeCls = getOrCreateOntClass(argIdx, 0, (ConceptName) range);
				if (rangeCls != null) {
					rangeCls = addClassToUnionClass(existingRange,
							(OntClass) rangeCls);
				}
			}
			if (existingRange != null) {
				if (rangeCls != null && !existingRange.equals(rangeCls)) {
					prop.removeRange(existingRange);
					prop.addRange(addClassToUnionClass(existingRange, rangeCls)); // prop.addRange(rangeCls);
					retval = true;
				}
			} else {
				if (rangeCls != null) {
					prop.addRange(rangeCls);
					retval = true;
				} else {
					addError(argIdx, ExistingNamePart.NOTAPPLICABLE,
							"Range not found.");
				}
			}
		} else if (range instanceof SadlIntersectionClass) {
			IntersectionClass rngcls = createIntersectionClass(2,
					(SadlIntersectionClass) range);
			if (rngcls != null) {
				prop.addRange(rngcls);
				retval = true;
			}
			else {
				addError(argIdx, ExistingNamePart.NOTAPPLICABLE, 
						"Range identifier (" + range.toString()
						+ ") failed to convert to Intersection.");

			}
		} else if (range instanceof SadlUnionClass) {
			UnionClass rngcls = createUnionClass(2, (SadlUnionClass) range);
			if (rngcls != null) {
				prop.addRange(rngcls);
				retval = true;
			} else {
				addError(argIdx, ExistingNamePart.NOTAPPLICABLE,
						"Range identifier (" + range.toString()
								+ ") failed to convert to Union.");
			}
		} else {
			addError(argIdx, ExistingNamePart.NOTAPPLICABLE,
					"Range identifier (" + range.toString()
							+ ") not of expected type.");
		}
		return retval;
	}

	private OntResource addClassToUnionClass(OntResource existingCls,
			OntResource cls) {
		if (existingCls != null && !existingCls.equals(cls)) {
			if (existingCls.canAs(OntClass.class) && classIsSubclassOf(existingCls.as(OntClass.class), cls, true)) {
				return cls;
			}
			else if (cls.canAs(OntClass.class) && classIsSubclassOf(cls.as(OntClass.class), existingCls, true)) {
				return existingCls;
			}
			else {
				RDFList classes = null;
				if (existingCls.canAs(UnionClass.class)) {
					try {
						 UnionClass ucls = existingCls.as(UnionClass.class);
						 ucls.addOperand(cls);
						 return ucls;
					} catch (Exception e) {
						// don't know why this is happening
						logger.error("Union class error that hasn't been resolved or understood.");
						return cls;
					}
				} else {
					if (cls.equals(existingCls)) {
						return existingCls;
					}
					classes = getJenaModel().createList();
					classes = classes.with(existingCls);
					classes = classes.with(cls);
				}
				OntResource unionClass = getJenaModel().createUnionClass(null,
						classes);
				return unionClass;
			}
		} else {
			return cls;
		}
	}

	/**
	 * Call this method to convert a SadlUnionClass to a Jena UnionClass.
	 * 
	 * @param argIdx
	 *            -- the argument index of SadlUnionClass in the original call
	 *            to ModelManager
	 * @param union
	 *            -- the encapsulated list of classes to place in the UnionClass
	 * @return -- the UnionClass if successful else null if not
	 */
	private UnionClass createUnionClass(int argIdx, SadlUnionClass union) {
		List<ConceptIdentifier> unionClasses = union.getClasses();
		if (unionClasses != null) {
			RDFNode[] clsarray = new RDFNode[unionClasses.size()];
			for (int i = 0; i < unionClasses.size(); i++) {
				ConceptIdentifier clsName = unionClasses.get(i);
				OntClass cls = conceptIdentifierToOntClass(argIdx, i, clsName);
				if (cls != null) {
					clsarray[i] = cls;
				} else {
					return null;
				}
			}
			RDFList classes = getJenaModel().createList(clsarray);
			if (!classes.isEmpty()) {
				return getJenaModel().createUnionClass(null, classes);
			}
		}
		return null;
	}

	/**
	 * Call this method to convert a SadlIntersectionClass to a Jena
	 * IntersectionClass.
	 * 
	 * @param argIdx
	 *            -- the argument index of the SadlIntersectionClass in the
	 *            original call to ModelManager
	 * @param intersect
	 *            -- the encapuslated list of classes to place in the
	 *            IntersectionClass
	 * @return -- the IntersectionClass if successful else null if not
	 */
	private IntersectionClass createIntersectionClass(int argIdx,
			SadlIntersectionClass intersect) {
		List<ConceptIdentifier> intersectClasses = intersect.getClasses();
		if (intersectClasses != null) {
			RDFList classes = getJenaModel().createList();
			for (int i = 0; i < intersectClasses.size(); i++) {
				ConceptIdentifier clsName = intersectClasses.get(i);
				OntClass cls = conceptIdentifierToOntClass(argIdx, i, clsName);
				if (cls != null) {
					classes = classes.with(cls);
				}
			}
			if (!classes.isEmpty()) {
				return getJenaModel().createIntersectionClass(null, classes);
			}
		}
		return null;
	}

	/**
	 * Call this method to convert a SadlEnumeratedClass to a Jena
	 * EnumeratedClass.
	 * 
	 * @param argIdx
	 *            -- the argument index of the SadlEnumeratedClass in the
	 *            orginal call to ModelManager
	 * @param clsName
	 *            -- the class to which the instances should belong
	 * @param enumerated
	 *            -- the encapsulated list of individuals to place in the
	 *            EnumeratedClass
	 * @return -- the EnumeratedClass if successful else null if not
	 */
	private EnumeratedClass createEnumeratedClass(int argIdx,
			ConceptName clsName, SadlEnumeratedClass enumerated) {
		List<ConceptName> enumeratedInstances = enumerated.getInstances();
		OntClass cls = null;
		if (clsName != null) {
			cls = getOrCreateOntClass(argIdx + 1, 0, clsName);
		}
		List<Individual> enumedInst = new ArrayList<Individual>();
		for (int i = 0; i < enumeratedInstances.size(); i++) {
			ConceptName inst = enumeratedInstances.get(i);
			Resource io;
			try {
				io = getOntResourceInExistingModel(inst);
				Individual indiv = null;
				if (io != null) {
					ConceptType it = getOntResourceConceptType(io);
					if (!it.equals(ConceptType.INDIVIDUAL)) {
						addError(argIdx, "'" + inst.toString() + "' already exists and is of type " + it.toString());
					}
					indiv = io.as(Individual.class);
				}
				else {
					indiv = getJenaModel().createIndividual(getUri(inst),
							cls);
					inst.setNamespace(indiv.getNameSpace());
					removePendingError(inst, ConceptType.INDIVIDUAL);
				}
				if (indiv != null) {
					if (cls != null) {
						indiv.addOntClass(cls);
					}
					enumedInst.add(indiv);
				}
			} catch (ConfigurationException e) {
				addError(argIdx, "Unexpected error creating instance '" + inst.toString() + "': " + e.getMessage());
			}
		}
		if (!enumedInst.isEmpty()) {
			Individual[] enumedArray = enumedInst
					.toArray(new Individual[enumedInst.size()]);
			RDFList rdfl = getJenaModel().createList(enumedArray);
			EnumeratedClass enumCls = getJenaModel().createEnumeratedClass(
					null, rdfl);
			return enumCls;
		} else {
			addError(argIdx,
					"Unable to generate enumerated class because instance list is empty.");
		}
		return null;
	}

	/**
	 * This method will return a relative URI for a corresponding SADL model if
	 * one exists in the project
	 * 
	 * @param u
	 * @return
	 */
	public URI equivalentSadlModelInProject(URI u) {
		return ResourceManager.validateAndReturnSadlFileEquivalentOfOwlUrl(u);
	}
	
	public boolean graphNeighborhood(ConceptName conceptName) {
		ResultSet rs = null;
		String query = null;
		try {
			if (conceptName.getType().equals(ConceptType.ONTCLASS)) {
				query = "select (<" + conceptName.getUri() + "> as ?s) ?p ?v where {<" + conceptName.getUri() + "> ?p ?v}";
			}
			else if (conceptName.getType().equals(ConceptType.OBJECTPROPERTY) || 
					conceptName.getType().equals(ConceptType.DATATYPEPROPERTY)) {
				query = "select (<" + conceptName.getUri() + "> as ?s) ?p ?v where {<" + conceptName.getUri() + "> ?p ?v}";
			}
			else if (conceptName.getType().equals(ConceptType.INDIVIDUAL)) {
//				query = "select ?s ?p ?v where { VALUES ?x {<" + conceptName.getUri() + ">} (?x as ?s) ?p ?v} UNION {?s ?p (?x as ?v)}}";
				query = "select ?s ?p ?v where { VALUES ?s {<" + conceptName.getUri() + ">} ?s ?p ?v}";
}
			QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(query, Syntax.syntaxARQ), getJenaModel());
			qexec.setTimeout(1000);
			com.hp.hpl.jena.query.ResultSet	results = qexec.execSelect();
			rs = convertFromJenaResultSetToReasonerResultSet(results);
			rs.setShowNamespaces(getShowNamespaces());
			File tmpdir = getProjectTempDir();
			if (tmpdir != null) {
				File dotfile = constructResultSetToDotFile(rs, tmpdir, null, conceptName.getName());
				createGraphVizGraph(dotfile.getAbsolutePath());
			}
		}
		catch (Throwable t) {
			getMessageManager().error("Unable to visualize graph: " + t.getMessage());
			return false;
		}
		return true;
	}

	public static ResultSet convertFromJenaResultSetToReasonerResultSet(com.hp.hpl.jena.query.ResultSet results) {
		if (!results.hasNext()) {
			return null;
		}
		ArrayList<ArrayList<Object>> o = new ArrayList<ArrayList<Object>>();
		List<String> queryVars = results.getResultVars();
		String[] columnName = new String[queryVars.size()];
		columnName = queryVars.toArray(columnName);
		while (results.hasNext()) {
			QuerySolution soln = results.next();
			ArrayList<Object> temp = new ArrayList<Object>();
			for (int j = 0; j < columnName.length; j++) {
				RDFNode n = soln.get(columnName[j]);
				if (n != null && n.isLiteral()) {
					Object val = ((Literal)n).getValue();
					if (val instanceof XSDDateTime) {
						temp.add(((XSDDateTime)val).asCalendar().getTime());
					} 
					else if (val instanceof XSDDuration) {
						temp.add(((XSDDuration)val).toString());
					}
					else {
						temp.add(val);
					}
				}
				else if (n != null && n.isResource()) {
					if (!((Resource)n).isAnon()){
						temp.add(((Resource)n).getURI());
					}
					else {
						temp.add(n.toString() + "(blank node)");
					}
				}
				else {
					temp.add(n == null? n : n.toString());	// for queries with OPTIONAL n can be null
				}
			}
			o.add(temp);
		}

		Object array[][] = new Object[o.size()][columnName.length];
		for(int i=0; i<o.size(); i++)
			array[i] = (o.get(i)).toArray(new Object[columnName.length]);
		
		ResultSet rs = new ResultSet(columnName, array);
		return rs;		
	}
	
	/**
	 * Call this method to get an existing OntClass or to create a new one with
	 * a PendingModelError.
	 * 
	 * @param argIdx
	 * @param lstIdx
	 * @param cName
	 * @return
	 */
	private OntClass getOrCreateOntClass(int argIdx, int lstIdx,
			ConceptName cName) {
		try {
			Resource r = getOntResourceInExistingModel((ConceptName)cName);
			if (r != null) {
				ConceptType ctype = getOntResourceConceptType(r);
				if (!ctype.equals(ConceptType.ONTCLASS)) {
					addWarning(2, "'" + ((ConceptName) cName).toString() + "' is a " + ctype.toString() + ", expected a class.");
				}
				((ConceptName) cName).setType(ctype);
				if (r.canAs(OntClass.class)) {
					return r.as(OntClass.class);
				}
				addError(argIdx, "Unable to convert named concept '" + cName.toString() + "' to a class. (Appears to be of type " + ctype.toString() + ".)");
				return null;
			}
			else {
				OntClass cls = getJenaModel().createClass(
						getUri(((ConceptName) cName)));
				((ConceptName)cName).setNamespace(cls.getNameSpace());
				addPendingError(argIdx, lstIdx, ConceptType.ONTCLASS,
						((ConceptName) cName),
						"Class '" + cName.toString()
								+ "' not found. " + getModelReference());
				return cls;
			}
		} catch (ConfigurationException e) {
			addError(argIdx, lstIdx, determineExistingNamePartOfError((ConceptName) cName),
					"Error finding class '" + getUri((ConceptName) cName) + "': " + e.getMessage());
		}
		return null;
	}

	/**
	 * Call this method to convert a ConceptIdentifier to an OntClass.
	 * 
	 * @param argIdx
	 * @param superClsName
	 * @return
	 */
	private OntClass conceptIdentifierToOntClass(int argIdx, int lstIdx,
			ConceptIdentifier superClsName) {
		OntClass superCls = null;
		if (superClsName instanceof ConceptName) {
			superCls = getOrCreateOntClass(argIdx, lstIdx,
					(ConceptName) superClsName);
		} else if (superClsName instanceof SadlUnionClass) {
			superCls = createUnionClass(argIdx, ((SadlUnionClass) superClsName));
		} else if (superClsName instanceof SadlIntersectionClass) {
			superCls = createIntersectionClass(argIdx,
					((SadlIntersectionClass) superClsName));
		}
		else if (superClsName instanceof SadlResourceByRestriction) {
			ConceptName pname = ((SadlResourceByRestriction)superClsName).getOnProperty();
			ClassRestrictionCondition crc = ((SadlResourceByRestriction)superClsName).getRestrictCondition();
			superCls = getRestriction(crc, pname);
		}
		return superCls;
	}
	
	private Restriction getRestriction(ClassRestrictionCondition crc, ConceptName prop) {
		Restriction rest = null;
		Resource pr = null;
		OntProperty p = null;
		ConceptType pType;
		try {
			pr = getOntResourceInExistingModel(prop);
			if (pr != null) {
				pType = getOntResourceConceptType(pr);
				prop.setType(pType);
				prop.setNamespace(pr.getNameSpace());
				p = pr.as(OntProperty.class);
			}
			else {
				addError(2, "Unable to find property '" + prop.toString()
						+ "'; must be declared before use in a class restriction.");
				return null;
			}
		} catch (ConfigurationException e) {
			addError(2, "Unexpected error finding property '" + prop.toString() + "': " + e.getMessage());
			return null;
		}
		RestrictionType rType = crc.getRestrictionType();
		if (rType.equals(RestrictionType.ALLVALUES)) {
			OntClass toCls = conceptIdentifierToOntClass(2, 0,
					crc.getRestrictedToConcept());
			if (toCls != null) {
				AllValuesFromRestriction avf = getJenaModel()
						.createAllValuesFromRestriction(null, p, toCls);
				if (avf != null) {
					rest = avf;
				} else {
					addError(1,
							"Unable to create allValuesFromRestriction for unknown reason.");
				}
			} else {
				addError(3, "allValuesFromRestriction to class not known.");
			}
		} else if (rType.equals(RestrictionType.SOMEVALUES)) {
			OntClass toCls = conceptIdentifierToOntClass(2, 0,
					crc.getRestrictedToConcept());
			if (toCls != null) {
				SomeValuesFromRestriction svf = getJenaModel()
						.createSomeValuesFromRestriction(null, p, toCls);
				if (svf != null) {
					rest = svf;
				} else {
					addError(1,
							"Unable to create someValuesFromRestriction for unknown reason.");
				}
			} else {
				addError(3, "someValuesFromRestriction to class not known.");
			}
		} else if (rType.equals(RestrictionType.HASVALUE)) {
			HasValueRestriction hvr = createHasValueRestriction(prop, p, 1,
					crc, 2);
			if (hvr != null) {
				rest = hvr;
			}
		} else if (rType.equals(RestrictionType.CARDINALITY)) {
			rest = createMinCardinalityRestriction(prop, p,1, crc, 2);
		} else if (rType.equals(RestrictionType.MAXCARDINALIY)) {
			rest = createMaxCardinalityRestriction(prop, p,1, crc, 2);
		} else if (rType.equals(RestrictionType.MINCARDINALITY)) {
			rest = createCardinalityRestriction(prop, p,1, crc, 2);
		} else {
			addError(2, "Invalid class restriction type: " + rType.toString());
		}
		return rest;
	}

	/**
	 * Call this method to get an existing Individual or to create a new one
	 * with a PendingModelError.
	 * 
	 * @param argIdx
	 * @param lstIdx
	 * @param iName
	 * @return
	 */
	private Individual getOrCreateIndividual(int argIdx, int lstIdx,
			ConceptIdentifier iName) {
		if (iName instanceof ConceptName) {
			try {
				Resource r = getOntResourceInExistingModel((ConceptName)iName);
				if (r != null) {
					ConceptType ctype = getOntResourceConceptType(r);
					if (!ctype.equals(ConceptType.INDIVIDUAL)) {
						addWarning(2, "'" + ((ConceptName) iName).toString() + "' is a " + ctype.toString() + ", expected an Individual.");
					}
					((ConceptName) iName).setType(ctype);
					if (r.canAs(Individual.class)) {
						return r.as(Individual.class);
					}
					addError(argIdx, "Unable to convert named concept '" + iName.toString() + "' to an instance. (Appears to be of type " + ctype.toString() + ".)");
					return null;
				}
				else {
					Individual inst = getJenaModel().createIndividual(
							getUri(((ConceptName) iName)), null);
					((ConceptName)iName).setNamespace(inst.getNameSpace());
					addPendingError(argIdx, lstIdx, ConceptType.INDIVIDUAL,
							((ConceptName) iName),
							"Instance '" + iName.toString()
									+ "' not found. " + getModelReference());
					return inst;
				}
			} catch (ConfigurationException e) {
				addError(argIdx, lstIdx, determineExistingNamePartOfError((ConceptName) iName),
						"Error finding instance '" + getUri((ConceptName) iName) + "': " + e.getMessage());
			}
		} else {
			addError(argIdx, "Instance name should not be a ConceptIdentifier");
			return null;
		}
		return null;
	}

	/**
	 * Call this method to get an existing DatatypeProperty or to create a new
	 * one with a PendingModelError.
	 * 
	 * @param argIdx
	 * @param lstIdx
	 * @param pName
	 * @return
	 */
	private OntProperty getOrCreateDataProperty(int argIdx, int lstIdx,
			ConceptIdentifier pName) {
		OntProperty prop;
		if (pName instanceof ConceptName) {
			Resource r;
			try {
				r = getOntResourceInExistingModel((ConceptName)pName);
				if (r != null) {
					ConceptType ctype = getOntResourceConceptType(r);
					if (!ctype.equals(ConceptType.DATATYPEPROPERTY)) {
						addWarning(2, "'" + ((ConceptName) pName).toString() + "' is a " + ctype.toString() + ", expected a data type property.");
					}
					((ConceptName) pName).setType(ctype);
					if (r.canAs(DatatypeProperty.class)) {
						return r.as(DatatypeProperty.class);
					}
					addError(argIdx, "Unable to convert named concept '" + pName.toString() + "' to a data type property. (Appears to be of type " + ctype.toString() + ").");
					return null;
				}
				else {
					prop = getJenaModel().createDatatypeProperty(
							getUri(((ConceptName) pName)));
					((ConceptName)pName).setNamespace(prop.getNameSpace());
					addPendingError(argIdx, lstIdx,
							ConceptType.DATATYPEPROPERTY,
							((ConceptName) pName),
							"Property '" + pName.toString()
									+ "' not found. " + getModelReference());
					return prop.asDatatypeProperty();
				}
			} catch (ConfigurationException e) {
				addError(argIdx, lstIdx, determineExistingNamePartOfError((ConceptName) pName),
						"Error finding data type property '" + getUri((ConceptName) pName) + "': " + e.getMessage());
			}
		} else {
			addError(argIdx, "Data property name should not be a ConceptIdentifier");
			return null;
		}
		return null;
	}

	/**
	 * Call this method to get an existing ObjectProperty or to create a new one
	 * with a PendingModelError.
	 * 
	 * @param argIdx
	 * @param lstIdx
	 * @param pName
	 * @return
	 */
	private ObjectProperty getOrCreateObjectProperty(int argIdx, int lstIdx,
			ConceptIdentifier pName) {
		OntProperty prop;
		if (pName instanceof ConceptName) {
			Resource r;
			try {
				r = getOntResourceInExistingModel((ConceptName)pName);
				if (r != null) {
					ConceptType ctype = getOntResourceConceptType(r);
					if (!ctype.equals(ConceptType.OBJECTPROPERTY)) {
						addWarning(2, "'" + ((ConceptName) pName).toString() + "' is a " + ctype.toString() + ", expected an object property.");
					}
					((ConceptName) pName).setType(ctype);
					if (r.canAs(ObjectProperty.class)) {
						return r.as(ObjectProperty.class);
					}
					addError(argIdx, "Unable to convert named concept '" + pName.toString() + "' to an object property. (Appears to be of type " + ctype.toString() + ").");
					return null;
				}
				else {
					prop = getJenaModel().createObjectProperty(
							getUri(((ConceptName) pName)));
					((ConceptName)pName).setNamespace(prop.getNameSpace());
					addPendingError(argIdx, lstIdx,
							ConceptType.OBJECTPROPERTY,
							((ConceptName) pName),
							"Property '" + pName.toString()
									+ "' not found. " + getModelReference());
					return prop.asObjectProperty();
				}
			} catch (ConfigurationException e) {
				addError(argIdx, lstIdx, determineExistingNamePartOfError((ConceptName) pName),
						"Error finding data type property '" + getUri((ConceptName) pName) + "': " + e.getMessage());
			}
		} else {
			addError(argIdx, "Data property name should not be a ConceptIdentifier");
			return null;
		}
		return null;
	}

	private String getModelReference() {
		return "(Referenced in model '" + getModelName() + "'.)";
	}

	private boolean addPrefix(String prefix, String namespace) {
		if (namespacesAndPrefixes == null) {
			namespacesAndPrefixes = new HashMap<String, String>();
		} else {
			if (namespacesAndPrefixes.containsKey(namespace)) {
				String prfx = namespacesAndPrefixes.get(namespace);
				if (prfx != null && prefix != null && !prfx.equals(prefix)) {
					addError(
							1,
							"Namespace '"
									+ namespace
									+ "' is already imported but with a different prefix ("
									+ prfx + " instead of " + prefix + ").");
				}
				return false;
			} else if (namespacesAndPrefixes.containsValue(prefix)) {
				addError(2, "Alias '" + prefix
						+ "' is already used for an import in this model.");
				return false;
			}
		}
		namespacesAndPrefixes.put(namespace, prefix);
		return true;
	}

	private boolean namespaceInBaseModel(String nspace) {
		if (nspace != null) {
			if (nspace.equals(modelNamespace)) {
				return true;
			}
		}
		return false;
	}

	private void setLastInstanceCreated(Individual lastBNodeCreated) {
		this.lastInstanceCreated = lastBNodeCreated;
	}

	/**
	 * This method returns the last bnode instance created in the model
	 * 
	 * @return
	 */
	public Individual getLastInstanceCreated() {
		return lastInstanceCreated;
	}

	private void addSadlCommand(SadlCommand sadlCommand) {
		if (sadlCommands == null) {
			sadlCommands = new ArrayList<SadlCommand>();
		}
		sadlCommands.add(sadlCommand);
	}

	public void runQuery(String modelName, String sparqlQueryStr) {
		try {
			ResultSet rs = query(modelName, sparqlQueryStr);
			if (rs != null && rs.getRowCount() > 0) {
				consoleOutput("Query '" + sparqlQueryStr + "' results:");
				rs.setShowNamespaces(getShowNamespaces());
				for (int irow = 0; irow < rs.getRowCount(); irow++) {
					String row = "";
					for (int icol = 0; icol < rs.getColumnCount(); icol++) {
						if (icol > 0)
							row += ", ";
						row += rs.getResultAt(irow, icol);
					}
					consoleOutput(row);
				}
			} else {
				consoleOutput("Query '" + sparqlQueryStr
						+ "' returned no results.");
			}
		} catch (ConfigurationException e) {
			outputError("Configuration error: " + e.getLocalizedMessage());
		} catch (ReasonerNotFoundException e) {
			outputError("Reasoner not found error: " + e.getLocalizedMessage());
			// } catch (TranslationException e) {
			// outputError("Translation error: " + e.getLocalizedMessage());
		} catch (QueryParseException e) {
			outputError("Query parse error: " + e.getLocalizedMessage());
			// } catch (TripleNotFoundException e) {
			// outputError("Query execution error: " + e.getLocalizedMessage());
		} catch (MalformedURLException e) {
			outputError("Malformed URL: " + e.getLocalizedMessage());
			e.printStackTrace();
		} catch (QueryCancelledException e) {
			outputError("Query canceled on timeout: " + e.getLocalizedMessage());
		} catch (InferenceCanceledException e) {
			outputError("Inference canceled by user: " + e.getLocalizedMessage());
		}
	}

	public ResultSet query(String modelName, String sparqlQueryStr)
			throws ConfigurationException, MalformedURLException,
			ReasonerNotFoundException, QueryParseException,
			QueryCancelledException {
		IReasoner reasoner = getConfigurationMgr().getReasoner();
		getConfigurationMgr();
		int iStatus = reasoner.initializeReasoner(getOwlModelsFolderPath(),
				modelName, ConfigurationManagerForIDE.getOWLFormat());
		if (iStatus == 0) {
			logger.error("Reasoner initialization returned failure status 0.");
		}
		ResultSet rs = reasoner.ask(sparqlQueryStr);
		return rs;
	}

	public void runQuery(String modelName, Query query) {
		try {
			getConfigurationMgr().setInferenceCanceled(false);
			IReasoner reasoner = getConfigurationMgr().getReasoner();
//			getConfigurationMgr();
			int iStatus = reasoner.initializeReasoner(getOwlModelsFolderPath(),
					modelName, ConfigurationManagerForIDE.getOWLFormat());
			if (iStatus == 0) {
				logger.error("Reasoner initialization returned failure status 0.");
			}
			ResultSet rs = runQuery(reasoner, query);
			if (rs != null && rs.getRowCount() > 0) {
				getMessageManager().info(
						"Query '" + query + "' results:\n",
						getMessageManager().new HyperlinkInfo(
								getModelActualUrl().toFileString(), query
										.getLineNo(), query.getOffset(), query
										.getLength()));
				rs.setShowNamespaces(getShowNamespaces());
				for (int irow = 0; irow < rs.getRowCount(); irow++) {
					String row = "    ";
					for (int icol = 0; icol < rs.getColumnCount(); icol++) {
						if (icol > 0)
							row += ", ";
						row += rs.getResultAt(irow, icol);
					}
					getMessageManager().info(row + "\n");
				}
			} else {
				getMessageManager().warn(
						"Query '" + query + "' returned no results.\n",
						getMessageManager().new HyperlinkInfo(
								getModelActualUrl().toFileString(), query
										.getLineNo(), query.getOffset(), query
										.getLength()));
			}
		} catch (ConfigurationException e) {
			getMessageManager().error(
					"Configuration error: " + e.getLocalizedMessage() + "\n",
					getMessageManager().new HyperlinkInfo(getModelActualUrl()
							.toFileString(), query.getLineNo(), query
							.getOffset(), query.getLength()));
		} catch (ReasonerNotFoundException e) {
			getMessageManager().error(
					"Reasoner not found error: " + e.getLocalizedMessage()
							+ "\n",
					getMessageManager().new HyperlinkInfo(getModelActualUrl()
							.toFileString(), query.getLineNo(), query
							.getOffset(), query.getLength()));
		} catch (QueryParseException e) {
			getMessageManager().error(
					"Query parse error: " + e.getLocalizedMessage() + "\n",
					getMessageManager().new HyperlinkInfo(getModelActualUrl()
							.toFileString(), query.getLineNo(), query
							.getOffset(), query.getLength()));
		} catch (TranslationException e) {
			getMessageManager().error(
					"Query translation error: " + e.getLocalizedMessage()
							+ "\n",
					getMessageManager().new HyperlinkInfo(getModelActualUrl()
							.toFileString(), query.getLineNo(), query
							.getOffset(), query.getLength()));
		} catch (InvalidNameException e) {
			getMessageManager().error(
					"Query translation error: " + e.getLocalizedMessage()
							+ "\n",
					getMessageManager().new HyperlinkInfo(getModelActualUrl()
							.toFileString(), query.getLineNo(), query
							.getOffset(), query.getLength()));
		} catch (MalformedURLException e) {
			outputError("Malformed URL: " + e.getLocalizedMessage());
			e.printStackTrace();
		} catch (QueryCancelledException e) {
			getMessageManager().info("Query canceled on timeout: " + e.getLocalizedMessage());
		} catch (InferenceCanceledException e) {
			getMessageManager().info("Inference canceled by user: " + e.getLocalizedMessage());
		}
	}

	/**
	 * Method to get the boolean value of the pShowNamespaces property of the
	 * Console Category from the configuration.
	 * 
	 * @return - value found else default true
	 */
	public boolean getShowNamespaces() {
		String[] consoleCategory = { "Console" };
		List<ConfigurationItem> cil;
		try {
			cil = getConfigurationMgr()
					.getConfiguration(consoleCategory, false);
			if (cil != null && cil.size() > 0) {
				ConfigurationItem ci = cil.get(0);
				Object objVal = ci.getNamedValue("pShowNamespaces");
				if (objVal != null && objVal instanceof Boolean) {
					return ((Boolean) objVal).booleanValue();
				}
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}

	private ResultSet runQuery(IReasoner reasoner, Query cmd)
			throws TranslationException, ConfigurationException,
			QueryParseException, InvalidNameException, MalformedURLException, QueryCancelledException {
		String queryStr = getConfigurationMgr().getTranslator().translateQuery(
				jenaModel, (Query) cmd);
		logger.info("Translated query: " + queryStr);
		ResultSet rs = reasoner.ask(queryStr);
		return rs;
	}

	/**
	 * This method runs all Tests, Queries, etc., in the specified model in the
	 * specified knowledge base
	 * 
	 * @param owlModelsFolderPath
	 * @param modelName
	 * @return - an array of two numbers: tests passed and total tests
	 */
	public int[] runAllTests(String modelName, boolean validateBeforeTesting,
			boolean showReasonerTimingInformation) {
		int queryCnt = 0;
		int testCnt = 0;
		int testsPassed = 0;
		int cancelled = 0 ;
		
		logger.info("Running tests");

		ConfigurationManagerForIDE configMgr = null;
		IReasoner reasoner = null;
		try {
			configMgr = getConfigurationMgr();
			configMgr.setInferenceCanceled(false);	// initialize for this run
			if (validateBeforeTesting
					|| (sadlCommands != null && sadlCommands.size() > 0)) {
				String actualUrl = configMgr.getAltUrlFromPublicUri(modelName);
				if (actualUrl.startsWith(ResourceManager.FILE_SHORT_PREFIX)) {
					// check to make sure that the OWL file has been built
					if (!configMgr.getModelGetter().getFormat().equals(IConfigurationManager.JENA_TDB)) {
						File mf = new File(SadlUtils.fileUrlToFileName(actualUrl));
						if (mf.exists()) {
							long owlTS = mf.lastModified();
							String sadlFN = ResourceManager.sadlFileNameOfOwlAltUrl(actualUrl);
							sadlFN = ResourceManager.findSadlFileInProject(SadlUtils.fileUrlToFileName(ResourceManager.getProjectUri(URI.createURI(actualUrl)).toString()), sadlFN);
							File sf = new File(SadlUtils.fileUrlToFileName(sadlFN));
							if (sf.exists()) {
								long sadlTS = sf.lastModified();
								if (sadlTS > owlTS) {
									getMessageManager().error("The OWL model for the SADL model '" + sadlFN + 
											"' appears to be out-of-date. Please enable 'Build Automatically' or do a Project Build manually."); 
								}
							}
						}
					}
				}
				configMgr.clearReasoner();
				reasoner = configMgr.getReasoner();
				reasoner.collectTimingInformation(showReasonerTimingInformation);
				int iStatus = reasoner.initializeReasoner(
						getOwlModelsFolderPath(), modelName, ConfigurationManagerForIDE.getOWLFormat());
				if (iStatus == 0) {
					logger.error("Reasoner initialization returned failure status 0.");
				} else if (validateBeforeTesting) {
					validateModel(modelName, reasoner);
				}
			}

			if (sadlCommands == null || sadlCommands.size() < 1) {
				consoleOutput("Model '" + modelName
						+ "' contains nothing to run.");
				int[] retval = new int[2];
				retval[0] = testsPassed;
				retval[1] = testCnt;
				return retval;
			}

			// look to see if derivations need to be on for explanation
			for (int i = 0; i < sadlCommands.size(); i++) {
				SadlCommand cmd = sadlCommands.get(i);
				if (cmd instanceof Explain
						&& ((Explain) cmd).getPatterns() != null) {
					reasoner.enableExplanation(true);
					break;
				}
			}
			for (int i = 0; i < sadlCommands.size(); i++) {
				SadlCommand cmd = sadlCommands.get(i);
				try {
					if (cmd instanceof Print) {
						long tp1 = System.currentTimeMillis();
						String msg = null;
						if (((Print) cmd).getDisplayString() != null) {
							msg = ((Print) cmd).getDisplayString() + "\n";
							getMessageManager().info(
									msg,
									getMessageManager().new HyperlinkInfo(
											getModelActualUrl().toFileString(),
											cmd.getLineNo(), cmd.getOffset(),
											cmd.getLength()));
						} else {
							try {
								File tmpdir = getProjectTempDir();
								if (tmpdir != null) {
									String type = ((Print) cmd).getModel();
									String outfn = getModelActualUrl()
											.lastSegment().toString()
											+ "."
											+ type;
									String outputfilename = tmpdir
											.getAbsolutePath()
											+ File.separator
											+ outfn
											+ ".owl";
									msg = "Printing "
											+ ((Print) cmd).getModel();
									reasoner.saveInferredModel(
											outputfilename, modelName,
											(type.equals("Model") ? false
													: true));
									ResourceManager.refreshResource(tmpdir
											.getAbsolutePath());
									getMessageManager()
											.info(msg,
													getMessageManager().new HyperlinkInfo(
															getModelActualUrl()
																	.toFileString(),
															cmd.getLineNo(),
															cmd.getOffset(),
															cmd.getLength()));
									getMessageManager()
											.info("    Output is available at " + outputfilename,
													getMessageManager().new HyperlinkInfo(
															outputfilename,
															1, 1, 1, 5, -1));
								} else {
									msg = "Printing failed.\n";
									getMessageManager()
											.info(msg,
													getMessageManager().new HyperlinkInfo(
															getModelActualUrl()
																	.toFileString(),
															cmd.getLineNo(),
															cmd.getOffset(),
															cmd.getLength()));
								}
							} catch (Exception e) {
								if (msg == null) {
									msg = "Printing failed: "
											+ e.getLocalizedMessage() + "\n";
								} else {
									msg += "     " + e.getLocalizedMessage()
											+ "\n";
								}
								getMessageManager().info(
										msg,
										getMessageManager().new HyperlinkInfo(
												getModelActualUrl()
														.toFileString(), cmd
														.getLineNo(), cmd
														.getOffset(), cmd
														.getLength()));
							}
						}
						if (showReasonerTimingInformation) {
							long tp2 = System.currentTimeMillis();
							getMessageManager().info("     (Print command took " + (tp2 - tp1) + " ms, not part of reasoner time.)");
						}
					} else if (cmd instanceof Explain) {
						explain((Explain) cmd);
					} else if (cmd instanceof Query) {
						ResultSet rs = runQuery(reasoner, (Query) cmd);
						String qstr;
						if (((Query)cmd).isToBeEvaluated()) {
							Object evalstr = rs.getResultAt(0, 0);
							logger.info("Evaluation of query: " + evalstr.toString());
							qstr = reasoner.prepareQuery(evalstr.toString());
							rs = reasoner.ask(qstr);
						}
						else {
							qstr = ((Query)cmd).toString();
						}
						if (qstr != null && qstr.length() > 9 && qstr.substring(0, 9).compareToIgnoreCase("construct") == 0) {
							try {
								File tmpdir = getProjectTempDir();
								if (tmpdir != null) {
									String bfn;
									int lastslash = modelName.lastIndexOf("/");
									if (lastslash > 0) {
										bfn = modelName.substring(lastslash + 1);
									}
									else {
										bfn = modelName;
									}
									rs.setShowNamespaces(getShowNamespaces());
									File dotfile = constructResultSetToDotFile(rs, tmpdir, bfn, ("" + queryCnt));
									createGraphVizGraph(dotfile.getAbsolutePath());
								}
							}
							catch (Throwable t) {
								getMessageManager().error("Unable to visualize graph: " + t.getMessage());
							}
						}
						consoleOutput((Query) cmd, rs);
						queryCnt++;
					} else if (cmd instanceof Test) {
						testCnt++;
						TestResult testResult = null;
						Object lhs = ((Test) cmd).getLhs();
						Object rhs = ((Test) cmd).getRhs();
						if (lhs == null || rhs == null) {
							// this is just a pass (true) or fail (false) test,
							// not a comparison
							if (lhs instanceof Query) {
								ResultSet rs = runQuery(reasoner, (Query) lhs);
								addError(new ModelError("Error: this Query case is not implemented!", ErrorType.ERROR));
							} else {
								TripleElement triple = null;
								if (lhs instanceof TripleElement) {
									triple = (TripleElement) lhs;
								} else if (rhs instanceof TripleElement) {
									triple = (TripleElement) rhs;
								}
								if (triple != null) {
									testResult = testTriple(reasoner, triple);
								} else if (lhs instanceof List<?>
										&& ((List<?>) lhs).size() == 2) {
									testResult = testFilteredQuery(reasoner,
											(List<?>) lhs);
								} else if (rhs instanceof List<?>
										&& ((List<?>) rhs).size() == 2) {
									testResult = testFilteredQuery(reasoner,
											(List<?>) rhs);
								} else if (lhs instanceof List<?>
										&& rhs == null) {
									Object lhobj = convertToComparableObject(
											getOwlModelsFolderPath(), reasoner,
											lhs, ((Test) cmd).getLhsVariables());
									if (lhobj instanceof ResultSet
											&& ((ResultSet) lhobj)
													.getColumnCount() > 0) {
										testResult = new TestResult(true);
									}
								} else {
									testResult = new TestResult(false);
									if (lhs == null && rhs == null) {
										testResult.setMsg("Unable to convert '"
												+ cmd.toString()
												+ "' to a test.");
									} else if (lhs == null) {
										testResult.setMsg("'" + rhs.toString()
												+ "' did not return a value.");
									} else if (rhs == null) {
										testResult.setMsg("'" + lhs.toString()
												+ "' did not return a value.");
									}
								}
							}
						} else {
							Object lhobj = convertToComparableObject(
									getOwlModelsFolderPath(), reasoner,
									((Test) cmd).getLhs(),
									((Test) cmd).getLhsVariables());
							Object rhobj = convertToComparableObject(
									getOwlModelsFolderPath(), reasoner,
									((Test) cmd).getRhs(),
									((Test) cmd).getRhsVariables());
							ComparisonType type = ((Test) cmd).getCompType();
							if (type != null
									&& (type.equals(ComparisonType.IsNot) || type
											.equals(ComparisonType.Neq))
									&& ((lhobj == null && (rhobj instanceof KnownNode || rhobj != null)) || (rhobj == null && (lhobj instanceof KnownNode || lhobj != null)))) {
								testResult = new TestResult(true);

							} else if (lhobj != null && rhobj != null
									&& type != null) {
								testResult = doTestComparison(lhobj, rhobj,
										type);
							} else {
								testResult = new TestResult(false);
								String msg = "";
								if (type == null) {
									msg += "Test has no comparison operator. ";
								}
								if (lhobj == null) {
									msg += "'" + lhs.toString()
											+ "' did not return a value. ";
								}
								if (rhobj == null) {
									msg += "'" + rhs.toString()
											+ "' did not return a value. ";
								}
								testResult.setMsg(msg);
							}
						}
						if (testResult == null) {
							String msg = "Test result is null. This should not happen. Test is: "
									+ cmd.toString();
							getMessageManager().error(
									msg,
									getMessageManager().new HyperlinkInfo(
											getModelActualUrl().toFileString(),
											cmd.getLineNo(), cmd.getOffset(),
											cmd.getLength(), 13, -1));
						} else if (testResult.isPassed()) {
							testsPassed++;
							String msg = "Test passed: " + cmd.toString()
									+ "\n";
							getMessageManager().info(
									msg,
									getMessageManager().new HyperlinkInfo(
											getModelActualUrl().toFileString(),
											cmd.getLineNo(), cmd.getOffset(),
											cmd.getLength(), 13, -1));
						} else {
							String msg = "Test failed: " + cmd.toString()
									+ "\n";
							if (testResult.getMsg() != null) {
								msg += "    " + testResult.getMsg() + "\n";
							}
							getMessageManager().error(
									msg,
									getMessageManager().new HyperlinkInfo(
											getModelActualUrl().toFileString(),
											cmd.getLineNo(), cmd.getOffset(),
											cmd.getLength(), 13, -1));
							getMessageManager().error(
									"    " + testResult.toString((Test) cmd)
											+ "\n");
						}
					}
				} catch (TranslationException e) {
					getMessageManager().error(
							"Translation error: " + e.getLocalizedMessage()
									+ "\n",
							getMessageManager().new HyperlinkInfo(
									getModelActualUrl().toFileString(), cmd
											.getLineNo(), cmd.getOffset(), cmd
											.getLength()));
				} catch (QueryParseException e) {
					getMessageManager().error(
							"Query parse error: " + e.getLocalizedMessage()
									+ "\n",
							getMessageManager().new HyperlinkInfo(
									getModelActualUrl().toFileString(), cmd
											.getLineNo(), cmd.getOffset(), cmd
											.getLength()));
				} catch (QueryCancelledException e) {
					getMessageManager().info(e.getLocalizedMessage(),
					getMessageManager().new HyperlinkInfo(
							getModelActualUrl().toFileString(), cmd
									.getLineNo(), cmd.getOffset(), cmd
									.getLength()));
					cancelled++;
				} catch (InferenceCanceledException e) {
					getMessageManager().info(e.getLocalizedMessage());
					break;
				} catch (TripleNotFoundException e) {
					getMessageManager().error(
							"Query execution error: " + e.getLocalizedMessage()
									+ "\n",
							getMessageManager().new HyperlinkInfo(
									getModelActualUrl().toFileString(), cmd
											.getLineNo(), cmd.getOffset(), cmd
											.getLength()));
				} catch (InvalidNameException e) {
					getMessageManager().error(
							"Query execution error: " + e.getLocalizedMessage()
									+ "\n",
							getMessageManager().new HyperlinkInfo(
									getModelActualUrl().toFileString(), cmd
											.getLineNo(), cmd.getOffset(), cmd
											.getLength()));
				} catch (Throwable t) {
					// catch anything inside the for so that only one test is
					// "lost"
					t.printStackTrace();
				}
			} // end for

			if (showReasonerTimingInformation) {
				List<ReasonerTiming> timingInfo = reasoner
						.getTimingInformation();
				if (timingInfo != null) {
					long totalTime = 0;
					getMessageManager().info("\nReasoner timing information:");
					for (int i = 0; i < timingInfo.size(); i++) {
						ReasonerTiming rt = timingInfo.get(i);
						getMessageManager().info(
								"    " + rt.getMilliseconds() + " (ms): "
										+ rt.getDescription());
						totalTime += rt.getMilliseconds();
					}
					getMessageManager()
							.info("    Total time (ms):" + totalTime);
				}
			}

			getMessageManager().info(
					"Passed " + testsPassed + " of " + testCnt + " tests.\n");
			if (queryCnt > 0) {
				getMessageManager().info("Run " + queryCnt + " queries.\n");
			}
			if (cancelled > 0) {
				getMessageManager().info("Cancelled " + cancelled + " queries due to timeout.");
			}
			try {
				DataSource ds = reasoner.getDerivations();
				if (ds != null) {
					File tmpdir = getProjectTempDir();
					if (tmpdir == null) {
						getMessageManager().error("Unable to locate Temp folder into which to output derivations file.");
						
					}
					else {
						String baseName = getModelActualUrl().lastSegment()
								.toString();
						if (baseName.endsWith(ResourceManager.SADLEXT)) {
							baseName = baseName.substring(0,
									baseName.length() - 5);
						}
						String outputfilename = tmpdir.getAbsolutePath()
								+ File.separator + baseName
								+ ".Derivations.log";
						String msg = "Derivations written to '"
								+ outputfilename + "'\n";
						File f = new File(outputfilename);
						f.createNewFile();
						OutputStream os = new FileOutputStream(f);
						InputStream is = ds.getInputStream();
						byte[] buf = new byte[1024];
						int i = 0;
						while ((i = is.read(buf)) != -1) {
							os.write(buf, 0, i);
						}
						is.close();
						os.close();
						ResourceManager.refreshResource(tmpdir
								.getAbsolutePath());
						getMessageManager().info(
								msg,
								getMessageManager().new HyperlinkInfo(
										outputfilename, 0, 1, 1));
					}
				} else if (reasoner.isExplanationEnabled()) {
					getMessageManager().info(
							"No derivations reported by reasoner for model '"
									+ getModelActualUrl() + "'.");
				}
			} catch (Throwable t) {
				t.printStackTrace();
				getMessageManager()
						.error("Error getting derivations: "
								+ t.getLocalizedMessage());
			}
		} catch (ConfigurationException e) {
			getMessageManager().error(
					"Configuration error: " + e.getLocalizedMessage() + "\n");
		} catch (ReasonerNotFoundException e) {
			getMessageManager().error(
					"Reasoner not found error: " + e.getLocalizedMessage()
							+ "\n");
		} catch (Throwable t) {
			t.printStackTrace();
			getMessageManager().error(
					"Unexpected throwable: " + t.getLocalizedMessage() + "\n");
		}
		int[] retval = new int[2];
		retval[0] = testsPassed;
		retval[1] = testCnt;
		return retval;
	}

	private void createGraphVizGraph(String dotfilepath) throws IOException {
    	IPreferencesService service = Platform.getPreferencesService();
    	String exec = service.getString("com.ge.research.sadl.Sadl", "graphvizpath", null, null);
    	String dotexec = null;
    	if (exec == null) {
    		return;
    	}
//		String exec = "e:\\win32app\\release\\bin\\dotty.exe";
    	if (!exec.endsWith("dotty") && !exec.endsWith("dotty.exe")) {
//    		dotexec = exec + File.separator + "dot";
    		exec = exec + File.separator + "dotty";
    	}
		ProcessBuilder pb = new ProcessBuilder(exec, dotfilepath);
		try {
			pb.start();
		} catch (IOException e) {
			throw new IOException("Unable to run GraphViz dotty; is GraphViz installed and on path? (" + e.getMessage() + ")");
		}
		if (dotexec != null) {
			ProcessBuilder bppng = new ProcessBuilder(dotexec, "-Tpng", dotfilepath,">", dotfilepath + ".png");
			try {
				bppng.start();
			} catch (IOException e) {
				throw new IOException("Unable to run GraphViz dot to generate PNG file; is GraphViz path set properly? (" + e.getMessage() + ")");
			}
		}
	}

	private File constructResultSetToDotFile(ResultSet rs, File tmpdir, String bfn, String queryCnt) throws IOException {
		StringBuilder sb = new StringBuilder("digraph g");
		sb.append(queryCnt);
		sb.append(" {\n");
		List<String> nodes = new ArrayList<String>();
		sb.append("    label=\"Construct ");
		sb.append(queryCnt + 1);
		sb.append("\";\n    labelloc=top;\n    labeljust=left;\n");
		
		int nothingCount = 0;
		
		while (rs.hasNext()) {
			boolean repeatObjNode; // end of directed edge
			boolean repeatSubjNode;	// start of directed edge
			Object[] row = rs.next();
			String slbl;			// name of start of directed edge
			String olbl;			// name of end of directed edge
			Object s;
			if (row[0].equals(OWL.Nothing.getURI())) {
				s = OWL.Nothing;
				slbl = OWL.Nothing.getLocalName() + nothingCount;
				nothingCount++;
				repeatSubjNode = false;
			}
			else {
				s = rs.getShowNamespaces() ? row[0] : rs.extractLocalName(row[0]);
				if (!nodes.contains(s.toString())) {
					nodes.add(s.toString());
					slbl = "n" + nodes.size();
					repeatSubjNode = false;
				}
				else {
					slbl = "n" + (nodes.indexOf(s.toString()) + 1);
					repeatSubjNode = true;
				}
			}
			Object o;
			if (row[2].equals(OWL.Nothing.getURI())) {
				o = OWL.Nothing;
				olbl = OWL.Nothing.getLocalName() + nothingCount;
				nothingCount++;
				repeatObjNode = false;
			}
			else {
				o = rs.getShowNamespaces() ? row[2] : rs.extractLocalName(row[2]);
				if (!nodes.contains(o.toString())) {
					nodes.add(o.toString());
					olbl = "n" + nodes.size();
					repeatObjNode = false;
				}
				else {
					olbl = "n" + (nodes.indexOf(o.toString()) + 1);
					repeatObjNode = true;
				}
			}
			sb.append("     ");
			if (!repeatSubjNode) {
				sb.append(slbl);
				if(s.equals(OWL.Nothing)) {
					sb.append("[shape=point label=\"");
				}
				else {
					sb.append("[shape=box label=\"");
				}
				sb.append(s.toString());
				sb.append("\"];\n");
			}
			if (!repeatObjNode) {
				sb.append("     ");
				sb.append(olbl);
				if (o.equals(OWL.Nothing)) {
					sb.append("[shape=point label=\"");
				}
				else {
					sb.append("[shape=box label=\"");
				}
				sb.append(o.toString());
				sb.append("\"];\n");
			}
			sb.append("     ");
			sb.append(slbl);
			sb.append("->");
			sb.append(olbl);
			sb.append("[label=\"");
			sb.append(rs.getShowNamespaces() ? row[1].toString() : rs.extractLocalName(row[1]));
			sb.append("\"];\n");
		}
		sb.append("}\n");
		File dotFile = new java.io.File(tmpdir.getAbsolutePath() + File.separator + 
				((bfn != null ? bfn : "") + "query" + queryCnt + "Graph.dot"));
		ResourceManager.stringToFile(dotFile, sb.toString(), false);
		return dotFile;
	}

	private File getProjectTempDir() throws Exception {
		String owlModelDir = ResourceManager
				.getOwlModelsFolder(this
						.getModelActualUrl());
		if (owlModelDir == null) {
			throw new Exception("Failed to find OwlModel folder. This should not happen!");
		}
		try {
			owlModelDir = ResourceManager
					.getOwlModelsFolder(this
							.getModelActualUrl());
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		File omd = new File(owlModelDir);
		if (omd != null) {
			File prjdir = omd.getParentFile();
			if (prjdir.exists()) {
				File tmpdir = new File(
						prjdir.getAbsoluteFile()
								+ File.separator
								+ "Temp");
				if (!tmpdir.exists()) {
					boolean bdirok = tmpdir.mkdir();
					if (!bdirok) {
						throw new Exception(
								"Failed to create Temp folder.");
					}
				}
				return tmpdir;
			}
		}
		return null;
	}

	/**
	 * Method to validate the model using current reasoner capabilities
	 * @param reasoner
	 * @return number of errors/warnings found
	 * @throws ConfigurationException 
	 */
	public int validateModel(String modelName, IReasoner reasoner) {
		if (reasoner == null) {
			ConfigurationManagerForIDE configMgr;
			try {
				configMgr = getConfigurationMgr();
				configMgr.clearReasoner();
				reasoner = configMgr.getReasoner();
				int iStatus = reasoner.initializeReasoner(
						getOwlModelsFolderPath(), modelName, ConfigurationManagerForIDE.getOWLFormat());
				if (iStatus == 0) {
					getMessageManager().error("Reasoner initialization returned failure status 0.");
					return 1;
				} 
			} catch (ConfigurationException e) {
				getMessageManager().error("Valided to validate model: " + e.getMessage());
			} catch (ReasonerNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		int errorCnt = 0;
		List<com.ge.research.sadl.reasoner.ModelError> validityErrors = reasoner
				.checkModelValidity();
		if (validityErrors == null) {
			getMessageManager()
					.info("Validity check returned no errors or warnings.");
		} else {
			errorCnt = validityErrors.size();
			for (int i = 0; i < validityErrors.size(); i++) {
				com.ge.research.sadl.reasoner.ModelError merr = validityErrors
						.get(i);
				if (merr.getErrorType().equals(ErrorType.WARNING)) {
					getMessageManager().warn(merr.getErrorMsg());
				} else {
					getMessageManager().error(merr.getErrorMsg());
				}
			}
		}
		return errorCnt;
	}

	private void explain(Explain explain) {
		if (explain.getRuleName() != null) {
			String rulename = explain.getRuleName();
			try {
				consoleOutput(explain, getConfigurationMgr().getReasoner()
						.explain(rulename));
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else if (explain.getPatterns() != null) {
			List<GraphPatternElement> patterns = explain.getPatterns();
			try {
				consoleOutput(explain, getConfigurationMgr().getReasoner()
						.explain(patterns));
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private TestResult testFilteredQuery(IReasoner reasoner, List<?> lst)
			throws QueryParseException {
		if (lst.size() != 2) {
			throw new QueryParseException(
					"Filtered Query not of expected size 2");
		}
		if (!(lst.get(0) instanceof TripleElement)) {
			throw new QueryParseException(
					"First element of Filtered Query not a Triple");
		}
		// if (!(lst.get(1) instanceof Junction)) {
		// throw new
		// QueryParseException("Second element of FilteredQuery not a Junction");
		// }
		Query newQuery = new Query();
		try {
			newQuery.setPatterns((List<GraphPatternElement>) lst);
			setVariablesFromPatterns(newQuery);
			String queryStr = getConfigurationMgr().getTranslator()
					.translateQuery(jenaModel, newQuery);
			logger.info("Translated query: " + queryStr);
			ResultSet lhResultSet = reasoner.ask(queryStr);
			if (lhResultSet != null) {
				return new TestResult(true);
			}
			TestResult tr = new TestResult(false);
			tr.setMsg("Test query '" + queryStr + "' did not return a value");
			return tr;
		} catch (Exception e) {
			TestResult tr = new TestResult(false);
			tr.setMsg("Encountered error evaluating test: "
					+ e.getLocalizedMessage());
			return tr;
		}
	}

	private TestResult testTriple(IReasoner reasoner, TripleElement triple)
			throws TripleNotFoundException, InvalidTypeException {
		Node sn = triple.getSubject();
		String subject = null;
		if (sn instanceof NamedNode) {
			subject = ((NamedNode) sn).toFullyQualifiedString();
		}
		Node pn = triple.getPredicate();
		String predicate = null;
		if (pn instanceof NamedNode) {
			predicate = ((NamedNode) pn).toFullyQualifiedString();
		}
		Node on = triple.getObject();
		String object = null;
		if (triple.getModifierType().equals(TripleModifierType.None)
				&& (pn instanceof RDFTypeNode || (on instanceof NamedNode
						&& sn instanceof NamedNode && !(sn instanceof VariableNode)))) {
			object = ((NamedNode) on).toFullyQualifiedString();
		}
		if (subject != null && predicate != null) {
			ResultSet rs = reasoner.ask(subject, predicate, object);
			if (logger.isInfoEnabled())
				logger.info("ResultSet: "
						+ (rs != null ? rs.toString() : "null"));
			if (on instanceof NamedNode) { // object != null) {
				if (triple.getModifierType().equals(TripleModifierType.None)) {
					if (rs != null && rs.getRowCount() > 0) {
						TestResult testResult = new TestResult();
						testResult.setPassed(true);
						return testResult;
					} else {
						TestResult tr = new TestResult(false);
						tr.setMsg("Triple '" + triple.toString()
								+ "' not found in model.");
						return tr;
					}
				} else {
					if (triple.getModifierType().equals(TripleModifierType.Not)) {
						return doTestComparison(on, rs, ComparisonType.Neq);
					} else if (triple.getModifierType().equals(
							TripleModifierType.Only)) {
						return doTestComparison(on, rs, ComparisonType.IsOnly);
					} else if (triple.getModifierType().equals(
							TripleModifierType.NotOnly)) {
						return doTestComparison(on, rs,
								ComparisonType.IsNotOnly);
					} else {
						throw new InvalidTypeException(
								"Triple test has unknown type '"
										+ triple.getModifierType() + "'");
					}
				}
			} else {
				TestResult testResult = new TestResult();
				if (on instanceof com.ge.research.sadl.model.gp.Literal
						|| on instanceof ValueTableNode
						|| on instanceof KnownNode) {
					int rcnt;
					if (rs != null && (rcnt = rs.getRowCount()) > 0) {
						// see if we can get a match on the literal
						if (triple.getModifierType().equals(
								TripleModifierType.None)) {
							OntProperty oprop = getJenaModel().getOntProperty(
									predicate);
							if (oprop != null && oprop.isDatatypeProperty()) {
								OntResource rngrsrc = null;
								if (oprop != null) {
									rngrsrc = oprop.getRange();
								}
								for (int i = 0; i < rcnt; i++) {
									Object result = rs.getResultAt(i, 0);
									if (compareValues(on, result, rngrsrc)) {
										testResult.setPassed(true);
										return testResult;
									}
								}
							} else {
								return doTestComparison(on, rs,
										ComparisonType.Eq);
							}
						} else {
							if (triple.getModifierType().equals(
									TripleModifierType.Not)) {
								return doTestComparison(on, rs,
										ComparisonType.Neq);
							} else if (triple.getModifierType().equals(
									TripleModifierType.Only)) {
								return doTestComparison(on, rs,
										ComparisonType.IsOnly);
							} else if (triple.getModifierType().equals(
									TripleModifierType.NotOnly)) {
								return doTestComparison(on, rs,
										ComparisonType.IsNotOnly);
							} else {
								throw new InvalidTypeException(
										"Triple test has unknown type '"
												+ triple.getModifierType()
												+ "'");
							}
						}
					} else if (triple.getModifierType().equals(
							TripleModifierType.Not)) {
						return new TestResult(true);
					}
				}
				testResult.setPassed(false);
				testResult.setType(ComparisonType.Eq);
				testResult.addLhsResult(rs);
				testResult.addRhsResult(on);
				return testResult;
			}
		} else {
			TestResult testResult = new TestResult();
			testResult.setPassed(false);
			testResult
					.setMsg("Triple pattern without subject and predicate not yet supported.");
			return testResult;
		}
	}

	private boolean compareValues(Node node, Object objVal, OntResource rngrsrc) {
		Object v1 = node;
		Object v2 = objVal;
		if (node instanceof Literal) {
			v1 = ((Literal) node).getValue();
		} else if (node instanceof com.ge.research.sadl.model.gp.Literal) {
			v1 = ((com.ge.research.sadl.model.gp.Literal) node).getValue();
		}
		if (objVal instanceof Literal) {
			v2 = ((Literal) objVal).getValue();
		} else if (objVal instanceof com.ge.research.sadl.model.gp.Literal) {
			v2 = ((com.ge.research.sadl.model.gp.Literal) objVal).getValue();
		}
		return compareObjects(v1, v2, rngrsrc);
	}

	private boolean compareObjects(Object lval, Object objVal,
			OntResource rngrsrc) {
		if ((lval instanceof KnownNode && objVal != null)
				|| (objVal instanceof KnownNode && lval != null)) {
			return true;
		}
		if (lval instanceof String && objVal instanceof String
				&& lval.equals(objVal)) {
			return true;
		} else if (lval instanceof Boolean && objVal instanceof Boolean) {
			if (lval.equals(objVal)) {
				return true;
			} else {
				return false;
			}
		} else if (lval instanceof Number && objVal instanceof Number) {
			if (lval.equals(objVal)) {
				return true;
			} else {
				double d1 = ((Number) lval).doubleValue();
				double d2 = ((Number) objVal).doubleValue();
				// double diff = d1 - d2;
				// if (Math.abs((double)diff / Math.max(Math.max(d1, d2),
				// .0000001)) < .0000001) {
				return ResultSet.areDoublesEqual(d1, d2); // true;
				// }
			}
		} else if (lval != null && objVal != null && lval.equals(objVal)) {
			return true;
		} else if (rngrsrc != null) {
			// we know the range so let's try to convert each value to the
			// desired value type and compare
			Literal v1 = getJenaModel().createTypedLiteral(lval,
					rngrsrc.getURI());
			Literal v2 = getJenaModel().createTypedLiteral(objVal,
					rngrsrc.getURI());
			if (v1 != null && v2 != null && v1.equals(v2)) {
				return true;
			}
			if (rngrsrc.equals(XSD.date)
					&& v1.getDatatypeURI().equals(XSD.date.getURI())
					&& v2.getDatatypeURI().equals(XSD.date.getURI())) {
				Object v1obj = v1.getValue();
				Object v2obj = v2.getValue();
				if (v1obj instanceof XSDDateTime
						&& v2obj instanceof XSDDateTime
						&& ((XSDDateTime) v1obj).getYears() == ((XSDDateTime) v2obj)
								.getYears()
						&& ((XSDDateTime) v1obj).getMonths() == ((XSDDateTime) v2obj)
								.getMonths()
						&& ((XSDDateTime) v1obj).getDays() == ((XSDDateTime) v2obj)
								.getDays()) {
					return true;
				}
			}
		}
		return false;
	}

	private Object convertToComparableObject(String knowledgeBaseIdentifier,
			IReasoner reasoner, Object obj, List<String> testVars)
			throws TranslationException, ConfigurationException,
			QueryParseException, TripleNotFoundException, QueryCancelledException {
		if (obj instanceof Query) {
			if (((Query) obj).getVariables() == null && testVars != null) {
				((Query) obj).setVariables(testVars);
			}
		} else if (obj instanceof TripleElement) {
			Query newQuery = new Query();
			newQuery.addPattern((TripleElement) obj);
			if (testVars != null) {
				newQuery.setVariables(testVars);
			}
			obj = newQuery;
		} else if (obj instanceof List<?>
				&& ((List<?>) obj).get(0) instanceof GraphPatternElement) {
			Query newQuery = new Query();
			newQuery.setPatterns((List<GraphPatternElement>) obj);
			if (testVars != null) {
				newQuery.setVariables(testVars);
			}
			obj = newQuery;
		} else if (obj instanceof ValueTableNode) {
			return obj;
		} else if (obj instanceof NamedNode) {
			return obj;
		} else if (obj instanceof com.ge.research.sadl.model.gp.Literal) {
			Object litObj = ((com.ge.research.sadl.model.gp.Literal) obj)
					.getValue();
			if (litObj instanceof String) {
				if (isSparqlQuery((String)litObj)) {
					Query tmpQuery = new Query();
					tmpQuery.setSparqlQueryString(litObj.toString());
					try {
						String queryStr = getConfigurationMgr().getTranslator()
								.translateQuery(getJenaModel(), tmpQuery);
						logger.info("Found SPARQL query as literal: "
								+ queryStr);
						obj = reasoner.ask(queryStr);
					} catch (InvalidNameException e) {
						e.printStackTrace();
						throw new TranslationException(
								"Unable to prepare SPARQL query", e);
					} catch (MalformedURLException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
			return obj;
		} else if (obj instanceof KnownNode) {
			return obj;
		} else {
			throw new TranslationException("Conversion of '" + obj.toString()
					+ "' to a comparable object not currently supported.");
		}
		if (obj instanceof Query) {
			if (((Query) obj).getVariables() == null) {
				setVariablesFromPatterns((Query) obj);
			}
			if (((Query) obj).getVariables() != null) {
				String queryStr = null;
				try {
					queryStr = getConfigurationMgr().getTranslator()
							.translateQuery(jenaModel, (Query) obj);
					logger.info("Translated query: " + queryStr);
					ResultSet lhResultSet = reasoner.ask(queryStr);
					obj = lhResultSet;
				} catch (InvalidNameException e) {
					throw new TranslationException("Translation failed: ", e);
				} catch (MalformedURLException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			} else {
				List<GraphPatternElement> elements = ((Query) obj)
						.getPatterns();
				for (int i = 0; elements != null && i < elements.size(); i++) {
					GraphPatternElement gpe = elements.get(i);
					if (gpe instanceof TripleElement) {
						// these should all be NamedNodes or there would have
						// been some variable found in the if clause
						Node subj = ((TripleElement) gpe).getSubject();
						Node pred = ((TripleElement) gpe).getPredicate();
						Node objval = ((TripleElement) gpe).getObject();
						String strObjVal;
						if (objval instanceof com.ge.research.sadl.model.gp.Literal) {
							strObjVal = getJenaModel()
									.createTypedLiteral(
											((com.ge.research.sadl.model.gp.Literal) objval)
													.getValue()).toString();
						} else {
							strObjVal = ((NamedNode) objval)
									.toFullyQualifiedString();
						}
						ResultSet results = reasoner.ask(
								((NamedNode) subj).toFullyQualifiedString(),
								((NamedNode) pred).toFullyQualifiedString(),
								strObjVal);
						if (results != null && results.hasNext()) {
							obj = new Boolean(true);
						} else {
							obj = new Boolean(false);
						}
					}
				}
			}
		}
		return obj;
	}

	public static boolean isSparqlQuery(String litObj) {
		litObj = litObj.trim();
		litObj = SadlUtils.stripQuotes(litObj);
		litObj = litObj.trim();
		if (litObj.toLowerCase().indexOf("where") > 0 &&
				(( litObj.toLowerCase().indexOf("select ") == 0 && ((String) litObj).indexOf("?") > 0) ||
				litObj.toLowerCase().indexOf("construct ") == 0)) {
			return true;
		}
		else if (litObj.length() > 4){
			int askidx = litObj.indexOf("ask ");
			int opidx = litObj.indexOf('{');
			int cpidx = litObj.indexOf('}');	
			if (askidx == 0 && opidx > 3 && cpidx > opidx) {
				return true;
			}
		}
		return false;
	}

	private void setVariablesFromPatterns(Query query) {
		List<GraphPatternElement> elements = query.getPatterns();
		List<String> vars = new ArrayList<String>();
		for (int i = 0; i < elements.size(); i++) {
			GraphPatternElement gpe = elements.get(i);
			vars = getVarsInGraphPatternElement(vars, gpe);
		}
		if (vars.size() > 0) {
			if (query.getVariables() == null) {
				query.setVariables(vars);
			} else {
				query.getVariables().addAll(vars);
			}
		}
	}

	private List<String> getVarsInGraphPatternElement(List<String> vars,
			GraphPatternElement gpe) {
		if (gpe instanceof TripleElement) {
			Node subj = ((TripleElement) gpe).getSubject();
			Node obj = ((TripleElement) gpe).getObject();
			if (subj instanceof VariableNode
					&& !vars.contains(((VariableNode) subj).getName())) {
				vars.add(((VariableNode) subj).getName());
			}
			if (obj instanceof VariableNode
					&& !vars.contains(((VariableNode) obj).getName())) {
				vars.add(((VariableNode) obj).getName());
			}
		} else if (gpe instanceof Junction) {
			vars = getVarsInGraphPatternElement(vars,
					(GraphPatternElement) ((Junction) gpe).getLhs());
			vars = getVarsInGraphPatternElement(vars,
					(GraphPatternElement) ((Junction) gpe).getRhs());
		}
		return vars;
	}

	private TestResult doTestComparison(Object lhobj, Object rhobj,
			ComparisonType type) {
		Object lhval = toComparableObject(lhobj);
		Object rhval = toComparableObject(rhobj);
		if (lhval != null && rhval != null) {
			if (ResultSet.valuesMatch(lhval, rhval)) {
				if (type.equals(ComparisonType.Eq)
						|| type.equals(ComparisonType.GTE)
						|| type.equals(ComparisonType.LTE)) {
					return new TestResult(true);
				} else if (type.equals(ComparisonType.LT)
						|| type.equals(ComparisonType.GT)) {
					return createTestFailed(lhobj, lhval, rhobj, rhval, type);
				} else if (type.equals(ComparisonType.Neq)
						|| type.equals(ComparisonType.IsNot)) {
					return createTestFailed(lhobj, lhval, rhobj, rhval, type);
				}
			}
			if (type.equals(ComparisonType.Neq)
					|| type.equals(ComparisonType.IsNot)) {
				return new TestResult(true);
			}
			// this leaves GT, GTE, LT, & LTE when not Eq, IsOnly, IsNotOnly
			if (type.equals(ComparisonType.GTE)
					|| type.equals(ComparisonType.GT)) {
				if (ResultSet.lessThan(rhval, lhval)) {
					return new TestResult(true);
				}
			}
			if (type.equals(ComparisonType.LTE)
					|| type.equals(ComparisonType.LT)) {
				if (ResultSet.lessThan(lhval, rhval)) {
					return new TestResult(true);
				}
			}
			if (type.equals(ComparisonType.IsOnly)) {
				if (ResultSet.valuesMatchExactly(lhval, rhval)) {
					return new TestResult(true);
				}
			}
			if (type.equals(ComparisonType.IsNotOnly)) {
				if (!ResultSet.valuesMatchExactly(lhval, rhval)) {
					return new TestResult(true);
				}
			}
		}
		if ((type.equals(ComparisonType.IsNot) || type
				.equals(ComparisonType.Neq))) {
			if ((rhval instanceof KnownNode && lhval == null)
					|| (lhval instanceof KnownNode && rhval == null)) {
				return new TestResult(true);
			}
			if ((lhval instanceof ResultSet && rhval == null)
					|| (rhval instanceof ResultSet && lhval == null)) {
				return new TestResult(true);
			} else if ((rhval == null && lhval != null)
					|| (lhval == null && rhval != null)) {
				return new TestResult(true);
			}
		}
		return createTestFailed(lhobj, lhval, rhobj, rhval, type);
	}

	private TestResult createTestFailed(Object lhobj, Object lhval,
			Object rhobj, Object rhval, ComparisonType type) {
		TestResult result = new TestResult(false);
		try {
			DisplayType dtype = determineDisplayType(lhobj, rhobj, lhval, rhval);
			result.addLhsResult(toDisplayString(lhval != null ? lhval : lhobj,
					dtype));
			result.addRhsResult(toDisplayString(rhval != null ? rhval : rhobj,
					dtype));
		} catch (Throwable t) {
			result.setMsg("Unexpected exception running test: "
					+ t.getLocalizedMessage());
		}
		result.setType(type);
		return result;
	}

	private DisplayType determineDisplayType(Object lobj, Object robj,
			Object lval, Object rval) {
		if (((lobj != null
				&& (lobj instanceof ValueTableNode || lobj instanceof ResultSet) || (lval != null && (lval instanceof ValueTableNode || lval instanceof ResultSet)))
				&& ((robj != null && (robj instanceof ValueTableNode || robj instanceof ResultSet))) || (rval != null && (rval instanceof ValueTableNode || rval instanceof ResultSet)))) {
			return DisplayType.ValueTableNodeDisplay;
		}
		return DisplayType.LiteralDisplay;
	}

	private String toDisplayString(Object obj, DisplayType dtype) {
		boolean displayNS = false;
		if (obj == null) {
			return null;
		}
		if (dtype.equals(DisplayType.ValueTableNodeDisplay)) {
			if (obj instanceof ValueTableNode) {
				return ((ValueTableNode) obj).toString();
			} else if (obj instanceof ResultSet) {
				return ((ResultSet) obj).toValueTableNode().toString();
			} else {
				return toDisplayString(obj, DisplayType.LiteralDisplay);
			}
		} else if (dtype.equals(DisplayType.LiteralDisplay)) {
			if (obj instanceof com.ge.research.sadl.model.gp.Literal) {
				return ((com.ge.research.sadl.model.gp.Literal) obj).getValue()
						.toString();
			} else if (!displayNS && obj instanceof String
					&& ((String) obj).indexOf("#") > 0) {
				return ((String) obj)
						.substring(((String) obj).indexOf("#") + 1);
			}
			return obj.toString();
		} else {
			return obj.toString();
		}
	}

	private Object toComparableObject(Object obj) {
		if (obj instanceof com.ge.research.sadl.model.gp.Literal) {
			return ((com.ge.research.sadl.model.gp.Literal) obj).getValue();
		} else if (obj instanceof ResultSet) {
			ResultSet rs = (ResultSet) obj;
			if (rs.getRowCount() < 1 || rs.getColumnCount() < 1) {
				return null;
			}
			if (rs.getColumnCount() == 1 && rs.getRowCount() == 1) {
				return rs.getResultAt(0, 0);
			} else {
				return rs;
			}
		} else if (obj instanceof ValueTableNode) {
			return ((ValueTableNode) obj).toResultSet();
		} else if (obj instanceof TripleElement) {
			Object[][] data = new Object[1][];
			Object[] row = new Object[3];
			data[0] = row;
			row[0] = toComparableObject(((TripleElement) obj).getSubject());
			row[1] = toComparableObject(((TripleElement) obj).getPredicate());
			row[2] = toComparableObject(((TripleElement) obj).getObject());
			ResultSet rs = new ResultSet(data);
			return rs;
		} else if (obj instanceof BuiltinElement) {
			// see if we can evaluate the builtin
			// TODO
		} else if (obj instanceof NamedNode) {
			return ((NamedNode) obj).toFullyQualifiedString();
		} else if (obj instanceof KnownNode) {
			return obj;
		}
		return obj;
	}

	private void outputError(String msg) {
		logger.error(msg);
	}

	private void consoleOutput(Query query, ResultSet rs) {
		getMessageManager().info(
				"Results for query '" + query.toString() + "':\n",
				getMessageManager().new HyperlinkInfo(getModelActualUrl()
						.toFileString(), query.getLineNo(), query.getOffset(),
						query.getLength()));
		if (rs != null) {
			rs.setShowNamespaces(getShowNamespaces());
			getMessageManager().info(rs.toStringWithIndent(5));
		} else {
			getMessageManager().info("   no results found\n");
		}
	}

	private void consoleOutput(Explain explain, List<Explanation> explanations) {
		getMessageManager().info(
				explain.toString() + ":\n",
				getMessageManager().new HyperlinkInfo(getModelActualUrl()
						.toFileString(), explain.getLineNo(), explain
						.getOffset(), explain.getLength()));
		if (explanations == null || explanations.size() == 0) {
			getMessageManager().info("    No applicable rules found.\n");
		}
		for (int i = 0; explanations != null && i < explanations.size(); i++) {
			Explanation expl = explanations.get(i);
			GraphPatternElement tp = expl.getGrpahPatternElement();
			String prefix = expl.getPatternPrefix();
			if (prefix != null) {
				getMessageManager().info("    " + prefix);
			}
			// else {
			// getMessageManager().equals("     ");
			// }
			if (tp != null) {
				getMessageManager().info("    " + tp.toString() + ":\n");
			}
			// else {
			// getMessageManager().info("     undefined triple:\n");
			// }
			List<String> explains = expl.getExplanations();
			for (int j = 0; explains != null && j < explains.size(); j++) {
				getMessageManager().info(explains.get(j) + "\n");
			}
		}
	}

	private void consoleOutput(String msg) {
		getMessageManager().info(msg);
	}

	private void setMessageManager(MessageManager messageManager) {
		this.messageManager = messageManager;
	}

	public MessageManager getMessageManager() {
		return messageManager;
	}

	private void setConfigurationMgr(ConfigurationManagerForIDE configurationMgr) {
		this.configurationMgr = configurationMgr;
	}

	/**
	 * Method to obtain the instance of the ConfigurationManager used for this
	 * model.
	 * 
	 * @param owlModelsFolderPath
	 * @return
	 * @throws ConfigurationException
	 * @throws MalformedURLException 
	 */
	public ConfigurationManagerForIDE getConfigurationMgr()
			throws ConfigurationException, MalformedURLException {
		String knowledgeBaseIdentifier = getOwlModelsFolderPath();
		if (knowledgeBaseIdentifier != null) {
			if (configurationMgr == null) {
				setConfigurationMgr(new ConfigurationManagerForIDE(knowledgeBaseIdentifier, ConfigurationManagerForIDE.getOWLFormat()));
//				throw new ConfigurationException("ModelManager doesn't have a ConfigurationManager; this shouldn't happen");
			} 
		}
		return configurationMgr;
	}

	private String getOwlModelsFolderPath() throws MalformedURLException {
		if (owlModelsFolderPath == null) {
			if (getModelActualUrl() != null) {
				owlModelsFolderPath = ResourceManager
						.getOwlModelsFolder(getModelActualUrl());
			}
		}
		return owlModelsFolderPath;
	}

	public List<ConfigurationItem> getConfiguration(String[] categoryHierarchy,
			boolean includeSubcategories) throws ConfigurationException, MalformedURLException {
		return getConfigurationMgr().getConfiguration(categoryHierarchy,
				includeSubcategories);
	}

	public void addConfiguration(ConfigurationItem newItem)
			throws ConfigurationException, MalformedURLException {
		getConfigurationMgr().addConfiguration(newItem);
	}

	public void updateConfiguration(ConfigurationItem newItem)
			throws ConfigurationException, MalformedURLException {
		getConfigurationMgr().updateConfiguration(newItem);
	}

	public String toString() {
		return modelName;
	}

	private Individual createDefault(OntClass restricted, OntProperty prop,
			RDFNode defValue, int level) throws Exception {
		if (defValue instanceof Individual) {
			OntClass instDefCls = getJenaModel().getOntClass(
					ResourceManager.ACUITY_DEFAULTS_NS + "ObjectDefault");
			if (instDefCls == null) {
				addError(new ModelError(
						"Unable to find ObjectDefault in Defaults model",
						ErrorType.ERROR));
				return null;
			}
			Individual def = getJenaModel().createIndividual(
					getUri(new ConceptName(createUniqueDefaultValName(
							restricted, prop))), instDefCls);
			def.addProperty(
					getJenaModel().getOntProperty(
							getUri(ResourceManager.ACUITY_DEFAULTS_NS,
									"appliesToProperty")), prop);
			def.addProperty(
					getJenaModel().getOntProperty(
							getUri(ResourceManager.ACUITY_DEFAULTS_NS,
									"hasObjectDefault")), defValue);
			if (level > 0) {
				String hlpuri = getUri(ResourceManager.ACUITY_DEFAULTS_NS,
						"hasLevel");
				OntProperty hlp = getJenaModel().getOntProperty(hlpuri);
				if (hlp == null) {
					addError(new ModelError(
							"Unable to find hasLevel property in Defaults model",
							ErrorType.ERROR));
					return null;
				}
				Literal defLvl = getJenaModel().createTypedLiteral(level);
				def.addProperty(hlp, defLvl);
			}
			return def;
		} else if (defValue instanceof Literal) {
			OntClass litDefCls = getJenaModel().getOntClass(
					getUri(ResourceManager.ACUITY_DEFAULTS_NS, "DataDefault"));
			if (litDefCls == null) {
				addError(new ModelError(
						"Unable to find DataDefault in Defaults model",
						ErrorType.ERROR));
				return null;
			}
			Individual def = getJenaModel().createIndividual(
					getUri(modelNamespace,
							createUniqueDefaultValName(restricted, prop)),
					litDefCls);
			def.addProperty(
					getJenaModel().getOntProperty(
							getUri(ResourceManager.ACUITY_DEFAULTS_NS,
									"appliesToProperty")), prop);
			def.addProperty(
					getJenaModel().getOntProperty(
							getUri(ResourceManager.ACUITY_DEFAULTS_NS,
									"hasDataDefault")), defValue);
			if (level > 0) {
				String hlpuri = getUri(ResourceManager.ACUITY_DEFAULTS_NS,
						"hasLevel");
				OntProperty hlp = getJenaModel().getOntProperty(hlpuri);
				if (hlp == null) {
					addError(new ModelError(
							"Unable to find hasLevel in Defaults model",
							ErrorType.ERROR));
					return null;
				}
				Literal defLvl = getJenaModel().createTypedLiteral(level);
				def.addProperty(hlp, defLvl);
			}
			return def;
		}
		return null;
	}

	public void checkDefaultsModelImported() throws IOException,
			URISyntaxException, ConfigurationException {
		if (namespacesAndPrefixes == null
				|| !namespacesAndPrefixes
						.containsKey(ResourceManager.ACUITY_DEFAULTS_URI)) {
			String modelFolder = ResourceManager
					.getOwlModelsFolder(getModelActualUrl());
			File defaultsFile = new File(modelFolder + File.separator
					+ ResourceManager.ACUITY_DEFAULTS_OWL_FN);
			getConfigurationMgr().setDefaultsAltUrlMapping();
			addImportToModel(ResourceManager.ACUITY_DEFAULTS_URI,
					IConfigurationManager.ACUITY_DEFAULTS_PREFIX,
					SadlUtils.fileNameToFileUrl(defaultsFile
							.getAbsolutePath()));
		}
	}

	private String createUniqueDefaultValName(OntClass restricted,
			OntProperty prop) throws PrefixNotFoundException {
		String nmBase = restricted.getLocalName() + "_" + prop.getLocalName()
				+ "_default";
		String nm = nmBase;
		int cntr = 0;
		while (getJenaModel().getIndividual(getUri(new ConceptName(nm))) != null) {
			nm = nmBase + ++cntr;
		}
		return nm;
	}

	private void setModelActualUrl(URI modelActualUrl) {
		this.modelActualUrl = modelActualUrl;
	}

	public URI getModelActualUrl() {
		return modelActualUrl;
	}

	public void setImportListType(ImportListType importListType) {
		this.importListType = importListType;
	}

	public ImportListType getImportListType() {
		return importListType;
	}

	public boolean validateClassInDomain(NamedNode pred, NamedNode domainClass) {
		if (beginDeepValidation()) {
			OntProperty prop = getJenaModel().getOntProperty(
					pred.toFullyQualifiedString());
			if (prop != null) {
				OntResource dr = prop.getDomain();
				OntClass dcls = getJenaModel().getOntClass(
						domainClass.toFullyQualifiedString());
				if (dcls != null) {
					if (dr == null) {
						ExtendedIterator<? extends OntProperty> spitr = prop.listSuperProperties();
						while (spitr.hasNext()) {
							OntProperty sprop = spitr.next();
							dr = sprop.getDomain();
							if (dr != null) {
								if (classIsSubclassOf(dcls, dr, true)) {
									spitr.close();
									endDeepValidation();
									return true;
								}
							}
						}
						spitr.close();
					}
					else {
						boolean bresult = classIsSubclassOf(dcls, dr, true); // dr.as(OntClass.class), dcls);
						endDeepValidation();
						return bresult;
					}
				}
			}
		}
		return true; // annotation properties aren't OntProperties
	}

	public boolean validateClassInRange(NamedNode pred, NamedNode rangeClass) {
		if (beginDeepValidation()) {
			OntProperty prop = getJenaModel().getOntProperty(
					pred.toFullyQualifiedString());
			if (prop != null && prop.isObjectProperty()) {
				OntResource rr = prop.getRange();
				OntClass rcls = getJenaModel().getOntClass(
						rangeClass.toFullyQualifiedString());
				if (rcls != null) {
					if (rr == null) {
						ExtendedIterator<? extends OntProperty> spitr = prop.listSuperProperties();
						while (spitr.hasNext()) {
							OntProperty sprop = spitr.next();
							rr = sprop.getDomain();
							if (rr != null) {
								if (classIsSubclassOf(rcls, rr, true)) {
									spitr.close();
									endDeepValidation();
									return true;
								}
							}
						}
						spitr.close();
					}
					else {
						boolean breturn = classIsSubclassOf(rcls, rr, true);
						endDeepValidation();
						return breturn;
					}
				}
			}
			else if (prop == null) {
// TODO this might need more checking awc 9/15/2013				
				return true;
			}
			endDeepValidation();
			return false;
		}
		return true;
	}

	private void endDeepValidation() {
		if (!isDeepValidationOff() && ((System.currentTimeMillis() - deepValidationStartTime) > 1000)) {
			setDeepValidationOff(true);
			getMessageManager().info("Disabling deep validation of model; taking excessive amounts of time.");
		}
	}

	private boolean beginDeepValidation() {
		if (!isDeepValidationOff()) {
			deepValidationStartTime = System.currentTimeMillis();
			return true;
		}
		return false;
	}

	public void resetConceptNamesCache() {
		conceptNamesCache.clear();
	}

	public void resetVariableNamesCache() {
		variableNamesCache.clear();
	}

	public boolean addToVariableNamesCache(ConceptName var) {
		if (cachingLevel > 0) {
			String key = var.toString();
			if (!variableNamesCache.containsKey(key)) {
				variableNamesCache.put(key, var);
				return true;
			}
		}
		return false;
	}

	private List<String> getOrderedImportUris() throws ConfigurationException {
		if (orderedImportUris == null || orderedImportUris.size() < 1) {
			if (imports != null) {
				orderedImportUris = new ArrayList<String>();
				Iterator<String> itr = imports.keySet().iterator();
				while (itr.hasNext()) {
					orderedImportUris.add(itr.next());
				}
			}			
		}
		return orderedImportUris;
	}

	private void setOrderedImportUris(String successfulUri) {
		if (orderedImportUris != null) {
			if (orderedImportUris.size() > 0 && !orderedImportUris.get(0).equals(successfulUri)) {
				orderedImportUris.remove(successfulUri);
				orderedImportUris.add(0, successfulUri);
			}
			else if (!orderedImportUris.contains(successfulUri)) {
				orderedImportUris.add(successfulUri);
			}
		}
		else {
			orderedImportUris = new ArrayList<String>();
			orderedImportUris.add(successfulUri);
		}
	}

	private boolean isDeepValidationOff() {
		return deepValidationOff;
	}

	public void setDeepValidationOff(boolean deepValidationOff) {
		this.deepValidationOff = deepValidationOff;
	}
	
	/**
	 * Method to find a named class in the range of the given property.
	 * 
	 * @param propName
	 * @return
	 * @throws ConfigurationException
	 */
	public ConceptName getObjectPropertyRange(ConceptName propName) throws ConfigurationException {
		Resource pr = getOntResourceInExistingModel(propName);
		if (pr != null && pr.canAs(OntProperty.class)) {
			OntProperty opr = pr.as(OntProperty.class);
			OntResource rr = getRange(opr);
			if (rr != null) {
				if (rr.isAnon()) {
					rr = findUriResourceInAnon(rr);
				}
				if (rr != null) {
					ConceptName rcn = new ConceptName(rr.getLocalName());
					rcn.setNamespace(rr.getNameSpace());
					rcn.setType(ConceptType.OBJECTPROPERTY);
					return rcn;
				}
			}
		}
		return null;
	}

	private OntResource findUriResourceInAnon(OntResource rr) {
		if (rr.canAs(UnionClass.class)) {
			try {
				UnionClass urr = rr.as(UnionClass.class);
				ExtendedIterator<? extends OntClass> eitr = urr.listOperands();
				while (eitr.hasNext()) {
					OntClass uclsmember = eitr.next();
					if (!uclsmember.isAnon()) {
						eitr.close();
						return uclsmember;
					}
					else {
						OntResource or = findUriResourceInAnon(uclsmember);
						if (or != null) {
							return or;
						}
					}
				}
			} catch (Exception e) {
				e.printStackTrace(); // don't know why this is happening
				logger.error("Union class error that hasn't been resolved or understood: "
						+ e.getLocalizedMessage());
			}
		} else if (rr.canAs(IntersectionClass.class)) {
			try {
				IntersectionClass icls = rr.as(IntersectionClass.class);
				ExtendedIterator<? extends OntClass> eitr = icls.listOperands();
				while (eitr.hasNext()) {
					OntClass iclsmember = eitr.next();
					if (!iclsmember.isAnon()) {
						eitr.close();
						return iclsmember;
					}
					else {
						OntResource or = findUriResourceInAnon(iclsmember);
						if (or != null) {
							return or;
						}
					}
				}
			} catch (Exception e) {
				// don't know why this is happening
				logger.error("Intersection class error that hasn't been resolved or understood: "
						+ e.getLocalizedMessage());
			}
		}
		return null;
	}

	/**
	 * Method to replace the OntModel in this ModelManager's imports with a newly saved OntModel from another ModelManager.
	 * @param savingMM
	 * @return
	 */
	public boolean updateImport(ModelManager savingMM) {
		String savedModelName = savingMM.getModelName();
		if (imports != null && imports.containsKey(savedModelName)) {
			imports.get(savedModelName).setModel(savingMM.getJenaModel());
			return true;
		}
		return false;
	}

	public boolean isHasBeenCancelled() {
		return hasBeenCancelled;
	}

	public void setHasBeenCancelled(boolean hasBeenCancelled) {
		this.hasBeenCancelled = hasBeenCancelled;
	}

	public boolean isRDFDataType(Object name) {
		String uri = null;
		if (name instanceof SadlIntersectionClass || name instanceof SadlUnionClass) {
			return false;
		}
		if (name instanceof ConceptName && (((ConceptName)name).hasPrefix() || ((ConceptName)name).getNamespace() != null)) {
			try {
				uri = ((ConceptName)name).getUri(getConfigurationMgr());
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvalidNameException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (uri != null && TypeMapper.getInstance().getSafeTypeByName(uri) != null) {
				return true;
			}
		}
		else {
			Iterator<RDFDatatype> rdfdtiter = TypeMapper.getInstance().listTypes();
			while (rdfdtiter.hasNext()) {
				RDFDatatype dt = rdfdtiter.next();
				if (dt.getURI().endsWith("#" + name.toString())) {
					if (name instanceof ConceptName) {
						((ConceptName)name).setNamespace(dt.getURI().substring(0, dt.getURI().indexOf('#') + 1));
					}
					return true;
				}
			}
		}
		return false;
	}

	public List<ModelError> addUserDefinedDataType(String name,
			EList<String> unionOfTypes, String baseType, String minexin, String min, String maxexin,
			String max, String regex, EList<String> values) {
		String uri = getModelName();
		
		StringBuilder sb = new StringBuilder();
		sb.append("<xsd:schema xmlns:xsd =\"http://www.w3.org/2001/XMLSchema\">\n");
		sb.append("<xsd:simpleType name=\"");
		sb.append(name);
		sb.append("\">\n");
		if (unionOfTypes != null && unionOfTypes.size() > 0) {
			sb.append("<xsd:union>\n");
			for (int i = 0; i < unionOfTypes.size(); i++) {
				sb.append("<xsd:simpleType>\n<xsd:restriction base=\"xsd:");
				sb.append(unionOfTypes.get(i));
				sb.append("\"/>\n");
				sb.append("</xsd:simpleType>\n");
			}
			sb.append("</xsd:union>\n");			
		}
		else {
			sb.append(" <xsd:restriction base=\"xsd:");
			sb.append(baseType);
			sb.append("\">\n");
			if (min != null) {
				if (minexin.equals("[")) {
					sb.append("<xsd:minInclusive value=\"");
				}
				else {
					sb.append("<xsd:minExclusive value=\"");
				}
				sb.append(min);
				sb.append("\"/>\n");
			}
			if (max != null) {
				if (minexin.equals("]")) {
					sb.append("<xsd:maxInclusive value=\"");
				}
				else {
					sb.append("<xsd:maxExclusive value=\"");
				}
				sb.append(max);
				sb.append("\"/>\n");
			}
			sb.append("  </xsd:restriction>\n");
		}
		sb.append("</xsd:simpleType>\n</xsd:schema>\n");
		
		File xsdFile;
		try {
			xsdFile = new File(getConfigurationMgr().getModelFolder() + "/" + name + ".xsd");
			SadlUtils.stringToFile(xsdFile, sb.toString(), false);
	        XSDDatatype.loadUserDefined(uri, new FileReader(xsdFile), null, TypeMapper.getInstance());
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			addError(0, "Unexpected error creating user-defined data type '" + name + "': " + e.getMessage());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			addError(0, "Unexpected error creating user-defined data type '" + name + "': " + e.getMessage());
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			addError(0, "Unexpected error creating user-defined data type '" + name + "': " + e.getMessage());
		}
		return getErrorsFinal();
	}
}
