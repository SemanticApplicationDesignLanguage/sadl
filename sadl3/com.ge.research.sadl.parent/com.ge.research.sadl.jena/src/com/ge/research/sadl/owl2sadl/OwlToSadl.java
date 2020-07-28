/************************************************************************
 * Copyright � 2007-2018 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.owl2sadl;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.ontology.AllValuesFromRestriction;
import org.apache.jena.ontology.AnnotationProperty;
import org.apache.jena.ontology.CardinalityRestriction;
import org.apache.jena.ontology.ComplementClass;
import org.apache.jena.ontology.ConversionException;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.EnumeratedClass;
import org.apache.jena.ontology.HasValueRestriction;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.IntersectionClass;
import org.apache.jena.ontology.MaxCardinalityRestriction;
import org.apache.jena.ontology.MinCardinalityRestriction;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.ontology.OntProperty;
import org.apache.jena.ontology.OntResource;
import org.apache.jena.ontology.Ontology;
import org.apache.jena.ontology.Restriction;
import org.apache.jena.ontology.SomeValuesFromRestriction;
import org.apache.jena.ontology.UnionClass;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.Syntax;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.NodeIterator;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFList;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResIterator;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.OWL2;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.XSD;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.external.XMLHelper;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SparqlQueries;
import com.ge.research.sadl.reasoner.AmbiguousNameException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManagerFactory;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.google.common.base.Optional;

/**
 * This class converts an OWL file to a SADL file.
 * 
 * @author 200005201
 *
 */
public class OwlToSadl {
	private static final Logger logger = LoggerFactory.getLogger(OwlToSadl.class);

	private OntModel theModel = null;
	private OntModel theBaseModel = null;
	
	private List<String> imports = null;
	private Map<String,String> qNamePrefixes = new HashMap<String,String>();
	
	private StringBuilder sadlModel = null;
	
	private List<String> allTokens = null;

	private List<OntResource> resourcesOutput = new ArrayList<OntResource>();
	
	private HashMap<String, OntResource> restrictions = null;
	
	private boolean neverUsePrefixes = false;

	//	replaced with getSadlKeywords to get straight from grammar
//	// copied from com.ge.research.sadl.parser.antlr.internal.InternalSadlParser in com.ge.research.sadl project.
//    public static final String[] tokenNames = new String[] {
//        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "RULE_STRING", "RULE_EOS", "RULE_ID", "RULE_UNSIGNED_NUMBER", "RULE_INT", "RULE_ML_COMMENT", "RULE_SL_COMMENT", "RULE_WS", "RULE_ANY_OTHER", "'uri'", "'alias'", "'version'", "'import'", "'as'", "'('", "'note'", "')'", "'{'", "','", "'}'", "'or'", "'and'", "'is'", "'a'", "'top-level'", "'class'", "'are'", "'classes'", "'type'", "'of'", "'types'", "'must'", "'be'", "'one'", "'described'", "'by'", "'has'", "'with'", "'single'", "'value'", "'values'", "'A'", "'An'", "'an'", "'The'", "'the'", "'same'", "'disjoint'", "'not'", "'only'", "'can'", "'level'", "'default'", "'at'", "'least'", "'each'", "'always'", "'most'", "'exactly'", "'if'", "'relationship'", "'to'", "'annotation'", "'describes'", "'subject'", "'symmetrical'", "'transitive'", "'inverse'", "'any'", "'Rule'", "':'", "'given'", "'then'", "'Ask:'", "'Test:'", "'Expr:'", "'Print:'", "'Deductions'", "'Model'", "'Explain:'", "'select'", "'distinct'", "'*'", "'where'", "'order by'", "'asc'", "'desc'", "'||'", "'&&'", "'='", "'=='", "'!='", "'<'", "'<='", "'>'", "'>='", "'+'", "'-'", "'/'", "'^'", "'%'", "'!'", "'PI'", "'known'", "'['", "']'", "'true'", "'false'", "'.'", "'~'", "'string'", "'boolean'", "'decimal'", "'int'", "'long'", "'float'", "'double'", "'duration'", "'dateTime'", "'time'", "'date'", "'gYearMonth'", "'gYear'", "'gMonthDay'", "'gDay'", "'gMonth'", "'hexBinary'", "'base64Binary'", "'anyURI'", "'data'"
//    };
    		
	private String baseUri;
	
	private String prefix;

	private String sourceFile = null;

	private OntModelSpec spec;

	private List<String> propertiesProcessed = new ArrayList<String>();	// properties already completely processed
	
	private List<Statement> statementsProcessed = new ArrayList<Statement>();

	private boolean verboseMode = false;
	private StringBuilder verboseModeStringBuilder = null;
	
	public class ModelConcepts {
		private List<Ontology> ontologies = new ArrayList<Ontology>();
		private List<OntClass> classes = new ArrayList<OntClass>();
		private List<OntClass> anonClasses = new ArrayList<OntClass>();
		private List<ObjectProperty> objProperties = new ArrayList<ObjectProperty>();
		private List<DatatypeProperty> dtProperties = new ArrayList<DatatypeProperty>();
		private List<Property> rdfProperties = new ArrayList<Property>();
		private List<Resource> pseudoObjProperties = new ArrayList<Resource>();
		private List<Resource> pseudoDtProperties = new ArrayList<Resource>();
		private List<AnnotationProperty> annProperties = new ArrayList<AnnotationProperty>();
		private List<Individual> instances = new ArrayList<Individual>();
		private List<Statement> statements = new ArrayList<Statement>();
		private List<Restriction> unMappedRestrictions = new ArrayList<Restriction>();
		private Map<OntClass, List<Restriction>> mappedRestrictions = new HashMap<OntClass, List<Restriction>>();
		private List<Resource> completed = new ArrayList<Resource>();
		private List<OntResource> datatypes = new ArrayList<OntResource>();
		private List<String> errorMessages = new ArrayList<String>();
		private List<String> warningMessages = new ArrayList<String>();
		private List<String> infoMessages = new ArrayList<String>();
		
		protected void sort() {
			sortOntResources(ontologies);
			sortOntResources(classes);
			sortOntResources(objProperties);
			sortOntResources(dtProperties);
			sortOntResources(annProperties);
			sortOntResources(instances);
		}
		
		protected void sortOntResources(List rlist) {
			java.util.Collections.sort(rlist, new Comparator<OntResource>(){
                public int compare(OntResource r1,OntResource r2){
                      if (!r1.isURIResource() || !r2.isURIResource()) {
                    	  return 0;
                      }
                      else if (r1.getURI().equals(r2.getURI())) {
                    	  return 0;
                      }
                      else {
                    	  return (r1.getURI().compareTo(r2.getURI()));
                      }
                }});
		}
		
		protected List<OntClass> getClasses() {
			return classes;
		}
		
		protected boolean addClass(OntClass cls) {
			if (!classes.contains(cls)) {
				classes.add(cls);
				return true;
			}
			return false;
		}
		
		protected List<ObjectProperty> getObjProperties() {
			return objProperties;
		}
		
		protected boolean addObjProperty(ObjectProperty objProperty) {
			if (!objProperties.contains(objProperty)) {
				objProperties.add(objProperty);
				return true;
			}
			return false;
		}
		
		protected List<DatatypeProperty> getDtProperties() {
			return dtProperties;
		}
		
		protected boolean addDtProperty(DatatypeProperty dtProperty) {
			if (!dtProperties.contains(dtProperty)) {
				dtProperties.add(dtProperty);
				return true;
			}
			return false;
		}
		
		protected List<Individual> getInstances() {
			return instances;
		}
		
		protected boolean addInstance(Individual instance) {
			if (!getInstances().contains(instance)) {
				getInstances().add(instance);
				return true;
			}
			return false;
		}
		
		protected List<Statement> getStatements() {
			return statements;
		}
		
		protected void setStatements(List<Statement> statements) {
			this.statements = statements;
		}

		protected List<Ontology> getOntologies() {
			return ontologies;
		}

		protected boolean addOntology(Ontology ontology) {
			if (!ontologies.contains(ontology)) {
				ontologies.add(ontology);
				return true;
			}
			return false;
		}

		protected List<AnnotationProperty> getAnnProperties() {
			return annProperties;
		}

		protected boolean addAnnProperty(AnnotationProperty annProperty) {
			if (!annProperties.contains(annProperty)) {
				annProperties.add(annProperty);
				return true;
			}
			return false;
		}

		protected List<Restriction> getUnMappedRestrictions() {
			return unMappedRestrictions;
		}

		protected void addUnMappedRestriction(Restriction unMappedRestriction) {
			if (!unMappedRestrictions.contains(unMappedRestriction)) {
				unMappedRestrictions.add(unMappedRestriction);
				logger.debug("Unmapped restriction information:");
				StmtIterator sitr = unMappedRestriction.listProperties();
				while (sitr.hasNext()) {
					logger.debug("    " + sitr.nextStatement().toString());
				}
				OntClass equiv = unMappedRestriction.getEquivalentClass();
				if (equiv != null) {
					logger.debug("      equivalent class:" + equiv.toString());
				}
			}
		}

		protected Map<OntClass, List<Restriction>> getMappedRestrictions() {
			return mappedRestrictions;
		}

		protected void addMappedRestriction(OntClass cls, Restriction restriction) {
			
			if (mappedRestrictions.containsKey(cls)) {
				List<Restriction> restList = mappedRestrictions.get(cls);
				if (!restList.contains(restriction)) {
					restList.add(restriction);
				}
			}
			else {
				List<Restriction> restList = new ArrayList<Restriction>();
				restList.add(restriction);
				this.mappedRestrictions.put(cls, restList);
			}
		}

		protected List<Resource> getCompleted() {
			return completed;
		}

		protected void addCompleted(Resource complete) {
			if (!completed.contains(complete)) {
				completed.add(complete);
			}
		}

		protected List<OntResource> getDatatypes() {
			return datatypes;
		}

		protected void addDatatype(OntResource datatype) {
			if (!datatypes.contains(datatype)) {
				datatypes.add(datatype);
			}
		}

		protected List<Resource> getPseudoObjProperties() {
			return pseudoObjProperties;
		}

		protected void addPseudoObjProperty(Resource pseudoObjProperty) {
			if (!pseudoObjProperties.contains(pseudoObjProperty)) {
				pseudoObjProperties.add(pseudoObjProperty);
			}
		}

		protected List<Resource> getPseudoDtProperties() {
			return pseudoDtProperties;
		}

		protected void addPseudoDtProperty(Resource pseudoDtProperty) {
			if (!pseudoDtProperties.contains(pseudoDtProperty)) {
				pseudoDtProperties.add(pseudoDtProperty);
			}
		}
		
		protected List<Property> getRdfProperties() {
			return rdfProperties;
		}
		
		protected void addRdfProperty(Property rdfProperty) {
			if (!rdfProperties.contains(rdfProperty)) {
				rdfProperties.add(rdfProperty);
			}
		}

		protected List<String> getErrorMessages() {
			return errorMessages;
		}

		protected void addErrorMessage(String errorMessage) {
			if (!errorMessages.contains(errorMessage)) {
				errorMessages.add(errorMessage);
			}
		}

		protected List<String> getWarningMessages() {
			return warningMessages;
		}

		protected void addWarningMessage(String warningMessage) {
			if (!warningMessages.contains(warningMessage)) {
				warningMessages.add(warningMessage);
			}
		}

		protected List<String> getInfoMessages() {
			return infoMessages;
		}

		protected void addInfoMessage(String infoMessage) {
			if (!infoMessages.contains(infoMessage)) {
				infoMessages.add(infoMessage);
			}
		}

		private List<OntClass> getAnonClasses() {
			return anonClasses;
		}

		private void addAnonClass(OntClass anonClass) {
			this.anonClasses.add(anonClass);
		}
	}

    /**
	 * Constructor taking only Jena OntModel to be converted to SADL
	 * 
	 * @param model
	 * @throws Exception
	 */
	public OwlToSadl(OntModel model) {
		theModel = model;
	}
	
    /**
	 * Constructor taking only Jena OntModel to be converted to SADL
	 * 
	 * @param model
	 * @throws Exception
	 */
	public OwlToSadl(OntModel model, String baseUri) {
		theModel = model;
		this.setBaseUri(baseUri);
	}
	
	/**
	 * Constructor taking the URL of an OWL file to be converted to SADL.
	 * 
	 * @param owlFileUrl
	 * @throws IOException
	 */
	public OwlToSadl(URL owlFileUrl) throws IOException {
		String modelUrl = owlFileUrl.toString();
		validateOntologyName(modelUrl);
		OntModelSpec spec = new OntModelSpec(OntModelSpec.OWL_MEM);
		setSpec(spec);
		theModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM); //_RDFS_INF);
		theModel.getDocumentManager().setProcessImports(false);
        theModel.read(modelUrl);
	}
	
	public OwlToSadl(URL owlFileUrl, String modelUri) throws IOException {
		setBaseUri(modelUri);
		String modelUrl = owlFileUrl.toString();
		validateOntologyName(modelUrl);
		OntModelSpec spec = new OntModelSpec(OntModelSpec.OWL_MEM);
		setSpec(spec);
		theModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM); //_RDFS_INF);
		theModel.getDocumentManager().setProcessImports(false);
        theModel.read(modelUrl);
	}

	/**
	 * Constructor taking an OWL File to be converted to SADL.
	 * 
	 * @param owlFile
	 * @throws IOException
	 * @throws OwlImportException
	 */
	public OwlToSadl(File owlFile) throws IOException, OwlImportException {
		if (!owlFile.exists()) {
			throw new OwlImportException("File '" + owlFile.getCanonicalPath() + "' does not exist.");
		}
		if (!owlFile.isFile()) {
			throw new OwlImportException("File '" + owlFile.getCanonicalPath() + "' is not a file.");
		}
		OntModelSpec spec = new OntModelSpec(OntModelSpec.OWL_MEM);
		setSpec(spec);
		theModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM); //_RDFS_INF);
		theModel.getDocumentManager().setProcessImports(false);
		if (owlFile.getName().endsWith(".ttl")) {
			theModel.read(owlFile.getCanonicalPath());
		}
		else {
			String baseUri = getBaseUri();
			if (baseUri == null) {
				String fn = owlFile.getName();
				int lastDot = fn.lastIndexOf('.');
				if (lastDot > 0) {
					String ext = fn.substring(lastDot + 1);
					if (ext.equalsIgnoreCase("rdf") || ext.equalsIgnoreCase("owl") || ext.equalsIgnoreCase("xml")) {
						try {
							Optional<String> buri = new XMLHelper().tryReadBaseUri(new SadlUtils().fileToString(owlFile));
							if (buri.isPresent()) {
								baseUri = buri.get();
								baseUri = baseUri.endsWith("#") ? baseUri.substring(0, baseUri.length() - 1) : baseUri;

							}
						} catch (IOException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						
					}
				}
			}
			theModel.read(new FileInputStream(owlFile), baseUri);
			setBaseUri(baseUri);
		}
		
//        theModel.read(owlFile.getCanonicalPath());
	}

	public OwlToSadl(String owlContent) {
		OntModelSpec spec = new OntModelSpec(OntModelSpec.OWL_MEM);
		setSpec(spec);
		theModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM); //_RDFS_INF);
		theModel.getDocumentManager().setProcessImports(false);
        theModel.read(new ByteArrayInputStream(owlContent.getBytes()), null);
	}
	
	public OwlToSadl(String owlContent, String modelUri) {
		setBaseUri(modelUri);
		OntModelSpec spec = new OntModelSpec(OntModelSpec.OWL_MEM);
		setSpec(spec);
		theModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM); //_RDFS_INF);
		theModel.getDocumentManager().setProcessImports(false);
        theModel.read(new ByteArrayInputStream(owlContent.getBytes()), null);
	}

	// This is a hack
	public static List<String> getSadlKeywords() {
		return Arrays.asList("select",
				"Test:",
				"construct",
				"type",
				"disjoint",
				"symmetrical",
				"property",
				"Stage",
				"if",
				"transitive",
				"order",
				"element",
				"!",
				"using",
				"in",
				"%",
				"double",
				"byte",
				"(",
				")",
				"index",
				"is",
				"*",
				"then",
				"+",
				",",
				"anyURI",
				"version",
				"-",
				"an",
				".",
				"/",
				"as",
				"contains",
				"at",
				"Graph",
				"seventh",
				"unique",
				"returns",
				":",
				"must",
				"Rule",
				"!=",
				"<",
				"=",
				">",
				"dateTime",
				"A",
				"other",
				"be",
				"E",
				"top-level",
				"another",
				"least",
				"An",
				"long",
				"matching",
				"The",
				"sixth",
				"see",
				"default",
				"same",
				"known",
				"are",
				"does",
				"by",
				"Ask",
				"where",
				"[",
				"after",
				"relationship",
				"]",
				"table",
				"^",
				"annotation",
				"a",
				"contain",
				"e",
				"one",
				"uri",
				"gMonth",
				"...",
				"describes",
				"the",
				"single",
				"asc",
				"sublist",
				"Relationship",
				"ask",
				"Model",
				"located",
				"fifth",
				"exists",
				"{",
				"to",
				"fourth",
				"}",
				"None",
				"return",
				"first",
				"||",
				"date",
				"<=",
				"data",
				"before",
				"subject",
				"anySimpleType",
				"integer",
				"Update",
				"float",
				"second",
				"gYear",
				"negativeInteger",
				"only",
				"Explain:",
				"unsignedByte",
				"List",
				"from",
				"gDay",
				"has",
				"described",
				"--",
				"always",
				"==",
				"=>",
				"given",
				"last",
				"level",
				"count",
				"most",
				"base64Binary",
				"Print:",
				"Write:",
				"External",
				"true",
				"decimal",
				"desc",
				">=",
				"&&",
				"note",
				"some",
				"Expr:",
				"tenth",
				"import",
				"string",
				"instances",
				"classes",
				"values",
				"for",
				"insert",
				"distinct",
				"nonNegativeInteger",
				"delete",
				"duration",
				"can",
				"not",
				"and",
				"hexBinary",
				"of",
				"alias",
				"class",
				"value",
				"gMonthDay",
				"inverse",
				"types",
				"or",
				"length",
				"false",
				"eighth",
				"Equation",
				"exactly",
				"any",
				"int",
				"nonPositiveInteger",
				"with",
				"boolean",
				"third",
				"Read:",
				"there",
				"positiveInteger",
				"ninth",
				"unsignedInt",
				"PI",
				"Deductions",
				"time",
				"gYearMonth");	
	}
	
	/**
	 * Save the SADL model to the specified file
	 * 
	 * @param sadlFile -- complete path and name of the SADL file to which the translated model is to be saved
	 * @return
	 * @throws IOException
	 * @throws OwlImportException 
	 */
	public boolean saveSadlModel(String sadlFile) throws IOException, OwlImportException {
		if (sadlModel == null) {
			process();
		}
		boolean writeProtect = true;
		
		File aFile = new File(sadlFile);
		
    	if (aFile.exists() && !aFile.isFile()) {
    		throw new IllegalArgumentException("Should not be a directory: " + aFile);
    	}
    	if (aFile.exists()) {
    		aFile.delete();
    	}
    	if (!aFile.exists()) {
    		aFile.createNewFile();
    	}
    	if (!aFile.canWrite()) {
    		throw new IllegalArgumentException("File cannot be written: " + aFile);
    	}
    	//declared here only to make visible to finally clause; generic reference
    	Writer output = null;
    	try {
    		//use buffering
    		//FileWriter always assumes default encoding is OK!
    		output = new BufferedWriter( new FileWriter(aFile) );
    		output.write( sadlModel.toString() );
    	}
    	finally {
    		//flush and close both "output" and its underlying FileWriter
    		if (output != null) output.close();
    	}
    	if (writeProtect) {
    		try {
    			aFile.setReadOnly();
    		}
    		catch (SecurityException e) {
    			e.printStackTrace();
    		}
    	}

		return true;
	}

	/**
	 * Get the SADL model resulting from the OWL translation as a DataSource
	 * 
	 * @return
	 * @throws OwlImportException 
	 * @throws Exception 
	 */
	public String getSadlModel() throws OwlImportException {
		if (sadlModel == null) {
	        process();
		}
		if (sadlModel != null) {
			return sadlModel.toString();
		}
		return null;
	}

	public void process() throws OwlImportException {
		if (theModel == null) {
			throw new OwlImportException("There is no OWL model to translate!");
		}

		initialize();
		
		if (sadlModel == null) {
			sadlModel = new StringBuilder();
		}

		sadlModel.append("uri \"");
		sadlModel.append(getBaseUri());
		sadlModel.append("\"");
		
		String alias = null;
		if (alias == null) {
			if (qNamePrefixes.containsKey(getBaseUri() + "#")) {
				alias = qNamePrefixes.get(getBaseUri() + "#");
			}
		}
		if (alias != null && alias.length() > 0) {
			sadlModel.append(" alias ");
			sadlModel.append(alias);
		}
		Ontology ont = theModel.getOntology(getBaseUri());
		if (ont == null && !getBaseUri().endsWith("#")) {
			ont = theModel.getOntology(getBaseUri() + "#");
		}
		
		String version;
		if (ont != null && (version = ont.getVersionInfo()) != null) {
			sadlModel.append("\n    version \"");
			sadlModel.append(version);
			sadlModel.append("\"");
		}

		if (ont != null) {
			StmtIterator sitr = theModel.listStatements(ont, RDFS.label, (RDFNode)null);
			if (sitr.hasNext()) {
				int cnt = 0;
				sadlModel.append("\n    (alias ");
				while (sitr.hasNext()) {
					Statement s = sitr.nextStatement();
					RDFNode obj = s.getObject();
					String label = obj.isLiteral() ? ((Literal)obj).getString() : obj.toString();
					if (cnt++ > 0) {
						sadlModel.append(", ");
					}
					sadlModel.append("\"");
					sadlModel.append(label);
					sadlModel.append("\"");
				}
				sadlModel.append(")");
			}
		
			sitr = theModel.listStatements(ont, RDFS.comment, (RDFNode)null);
			if (sitr.hasNext()) {
				int cnt = 0;
				sadlModel.append("\n    (note ");
				while (sitr.hasNext()) {
					Statement s = sitr.nextStatement();
					RDFNode obj = s.getObject();
					String comment = obj.isLiteral() ? ((Literal)obj).getString() : obj.toString();
					if (cnt++ > 0) {
						sadlModel.append(", ");
					}
					sadlModel.append("\"");
					sadlModel.append(comment);
					sadlModel.append("\"");
				}
				sadlModel.append(")");
			}
		}
		
		if (sourceFile != null) {
			sadlModel.append("\n    (note \"");
			sadlModel.append("This model was generated from the OWL model in file '");
			sadlModel.append(sourceFile);
			sadlModel.append("'\")");
		}

		addEndOfStatement(sadlModel, 2);
		
		if (isVerboseMode()) {
			ExtendedIterator<RDFNode> citr = ont.listComments(null);
			while (citr.hasNext()) {
				RDFNode comment = citr.next();
				sadlModel.append("// " + comment.toString());
				sadlModel.append(".\n");			}
		}
		
		// 2. imports
		if (logger.isDebugEnabled()) {
			OutputStream os = new ByteArrayOutputStream();
			theModel.getBaseModel().write(os);
			logger.debug(os.toString());
		}
		
		if (ont != null) {
			Iterator<Statement> impitr = theModel.listStatements(theModel.getResource(getBaseUri()), OWL.imports, (RDFNode)null);
			while (impitr.hasNext()) {
				String impuri = impitr.next().getObject().toString();
				logger.debug("Import: " + impuri);
			}
			ExtendedIterator<OntResource> iitr = ont.listImports();
			int prefixCntr = 0;
			while (iitr.hasNext()) {
				OntResource imp = iitr.next();
				String impUri;
				try {
					impUri = imp.asOntology().getURI(); //getNameSpace();
				}
				catch (Exception e) {
					impUri = imp.toString();
				}
				if (isImplicitUri(impUri)) {
					continue;
				}
				String prefix = theModel.getNsURIPrefix(impUri);
				sadlModel.append("import \"");
				sadlModel.append(impUri);
				sadlModel.append("\"");
				if (prefix == null) {
					prefix = theModel.getNsURIPrefix(impUri+"#");
					prefix = sadlizePrefix(prefix);
				}
				if (prefix != null && prefix.length() > 0) {
					sadlModel.append(" as ");
					sadlModel.append(prefix);
					theModel.setNsPrefix(prefix, impUri + "#");
				}

				sadlModel.append(".\n");
				if (imports == null) {
					imports = new ArrayList<String>();
				}
				imports.add(impUri);
			}
		}
		
//		// 3. classes with their properties
//		sadlModel.append("\n// Classes and class restrictions\n");
//		List<OntClass> classes = namedClassesAlphabeticalOrder();
//		addClasses(classes, null);
//		
//		ExtendedIterator<ComplementClass> citr = theModel.listComplementClasses();
//		if (citr.hasNext()) {
//			ComplementClass ccls = citr.next();
//			OntClass comp = ccls.getOperand();
//			if (!ccls.isAnon()) {
//				sadlModel.append(resourceToString(ccls, false));
//				sadlModel.append(" is the same as not ");
//				sadlModel.append(resourceToString(comp, false));
//			}
//			else {
//				sadlModel.append("not " );
//				sadlModel.append(resourceToString(comp, false));
//			}
//			sadlModel.append(".\n");
//		}
//
//		// 4. pick up any properties not already covered
//		sadlModel.append("\n// Properties\n");
//		List<OntProperty> properties = namedPropertiesAlphabeticalOrder(true);
//		addProperties(properties, true);
//		
//		// 6. Any restrictions not already output (shouldn't be any?)
//		if (restrictions != null && restrictions.size() > 0) {
//			sadlModel.append("\n// Additional restrictions\n");
//			Iterator<String> itr = restrictions.keySet().iterator();
//			while (itr.hasNext()) {
//				String key = itr.next();
//				if (restrictions.get(key) != null) {
//					sadlModel.append(key);
//				}
//			}
//			restrictions.clear();
//		}

//		// 5. finally add instance declarations
//		sadlModel.append("\n// Instance declarations\n");
//		List<Individual> individuals = individualsAlphabeticalOrder();
//		addIndividuals(individuals);
		
		// TODO
		/*
		 * change this approach: 
		 * 1. get the base model (theModel.getBaseModel())
		 * 2. get all statements of the model
		 * 3. if the subject is a resource in this namespace, make sure it is output
		 * 4. if the predicate is a resource in this namespace, do likewise
		 * 5. if the object is a resource in this namespace, do likewise
		 * 6. output the statements that remain (how do we know what remains?)
		 */
		
		List<Statement> statements = new ArrayList<Statement>();
		
		StmtIterator siter = theModel.getBaseModel().listStatements();
		while (siter.hasNext()) {
			Statement s = siter.nextStatement();
			statements.add(s);
			logger.debug("Processing model statement: " + s.toString());
			if (isVerboseMode()) {
				verboseModeStringBuilder.append("// Processed statement: ");
				verboseModeStringBuilder.append(s.toString());
				verboseModeStringBuilder.append(System.lineSeparator());
			}
			Resource subj = s.getSubject();
			OntResource ontSubj = theModel.getOntResource(subj);
			if (shouldResourceBeOutput(ontSubj, false, false, true)) {
				if (!addResourceToList(getConcepts(), ontSubj)) {
					logger.debug("subject resource not added to a list: " + ontSubj.toString());
					if (isVerboseMode()) {
						verboseModeStringBuilder.append("//     subject resource not added to processing list: \": ");
						verboseModeStringBuilder.append(ontSubj.toString());
						verboseModeStringBuilder.append(System.lineSeparator());
					}
				}
			}
			Property prop = s.getPredicate();
			Property mprop = theModel.getProperty(prop.getURI());
			if (mprop.canAs(OntProperty.class)) {
				OntProperty ontProp = mprop.as(OntProperty.class);
				if (shouldResourceBeOutput(ontProp, false, false, true)) {
					if(!addResourceToList(getConcepts(), ontProp)) {
						logger.debug("predicate resource not added to a list: " + ontProp.toString());
						if (isVerboseMode()) {
							verboseModeStringBuilder.append("//     predicate resource not added to processing list: \": ");
							verboseModeStringBuilder.append(ontProp.toString());
							verboseModeStringBuilder.append(System.lineSeparator());
						}
					}
				}
			}
			else {
				if (shouldResourceBeOutput(mprop, false, false, true)) {
					if (isPropertyInThisNamespace(mprop)) {
						getConcepts().addRdfProperty(mprop);
					}
				}
			}
			RDFNode obj = s.getObject();
			if (obj.isResource()) {
				OntResource ontObj = theModel.getOntResource(obj.asResource());
				if (shouldResourceBeOutput(ontObj, false, false, true)) {
					if(!addResourceToList(getConcepts(), ontObj)) {
						logger.debug("object resource not added to a list: " + ontObj.toString());
						if (isVerboseMode()) {
							verboseModeStringBuilder.append("//     object resource not added to processing list: \": ");
							verboseModeStringBuilder.append(ontObj.toString());
							verboseModeStringBuilder.append(System.lineSeparator());
						}
					}
				}
			}
			else {
				// this is a literal, object of a Datatype property
			}
			
			/* Is this statement only in the base model?
			 * 
			 */
			if (theModel.getBaseModel().contains(s)) {
				getConcepts().getStatements().add(s);
			}
		}
		
		// Output messages as SADL comments
		if (getConcepts().getErrorMessages() != null && getConcepts().getErrorMessages().size() > 0) {
			sadlModel.append("\n\n// Errors:\n");
			Iterator<String> erritr = getConcepts().getErrorMessages().iterator();
			while (erritr.hasNext()) {
				sadlModel.append("// ");
				sadlModel.append(erritr.next());
				sadlModel.append(System.lineSeparator());
			}
		}
		else if (isVerboseMode()) {
			sadlModel.append("\n\n// No Errors\n");
		}
		
		if (getConcepts().getWarningMessages() != null && getConcepts().getWarningMessages().size() > 0) {
			sadlModel.append("\n\n// Warnings:\n");
			Iterator<String> erritr = getConcepts().getWarningMessages().iterator();
			while (erritr.hasNext()) {
				sadlModel.append("// ");
				sadlModel.append(erritr.next());
				sadlModel.append(System.lineSeparator());
			}
		}
		else if (isVerboseMode()) {
			sadlModel.append("\n\n// No Warnings\n");
		}
		
		if (getConcepts().getInfoMessages() != null && getConcepts().getInfoMessages().size() > 0) {
			sadlModel.append("\n\n// Info:\n");
			Iterator<String> erritr = getConcepts().getInfoMessages().iterator();
			while (erritr.hasNext()) {
				sadlModel.append("// ");
				sadlModel.append(erritr.next());
				sadlModel.append(System.lineSeparator());
			}
		}
		else if (isVerboseMode()) {
			sadlModel.append("\n\n// No Info output\n");
		}
		
		// sort before starting to generate output
		getConcepts().sort();
		
		if (isVerboseMode()) {
			sadlModel.append("\n\n// Ontologies:\n");
			List<Ontology> onts = getConcepts().getOntologies();
			for (int i = 0; i < onts.size(); i++) {
				Ontology onti = onts.get(i);
				sadlModel.append("//    ");
				sadlModel.append(onti.toString());
				sadlModel.append(System.lineSeparator());
			}
		}
		
		if (isVerboseMode() || !getConcepts().getDatatypes().isEmpty()) {
			sadlModel.append("\n\n// Datatype Declarations:\n");
			List<OntResource> datatypes = getConcepts().getDatatypes();
			for (int i = 0; i < datatypes.size(); i++) {
				sadlModel.append(datatypeToSadl(getConcepts(), datatypes.get(i)));
			}
		}
		
		if (isVerboseMode() || !getConcepts().getAnnProperties().isEmpty()) {
			sadlModel.append("\n\n// Annotation Properties:\n");
			List<AnnotationProperty> anns = getConcepts().getAnnProperties();
			for (int i = 0; i < anns.size(); i++) {
				AnnotationProperty ann = anns.get(i);
				if (!getConcepts().getCompleted().contains(ann)) {
					sadlModel.append(annotationsToSadl(getConcepts(), ann));
					getConcepts().addCompleted(ann);
				}
			}
		}
		if (isVerboseMode() || !getConcepts().getRdfProperties().isEmpty()) {
			sadlModel.append("\n\n// RDF Properties:\n");
			List<Property> rdfProperties = getConcepts().getRdfProperties();
			for (int i = 0; i < rdfProperties.size(); i++) {
				Property prop = rdfProperties.get(i);
				addRdfProperty(getConcepts(), sadlModel, prop);
				getConcepts().addCompleted(prop);
			}
		}
		
		List<ObjectProperty> objProperties = getConcepts().getObjProperties();
		if (isVerboseMode() || !objProperties.isEmpty()) {
			StringBuilder tempSb = new StringBuilder();
			for (int i = 0; i < objProperties.size(); i++) {
				OntResource prop = objProperties.get(i);
				addSuperPropertiesWithoutRange(getConcepts(), tempSb, prop);
				getConcepts().addCompleted(prop);
			}
			if (isVerboseMode() || tempSb.length() > 0) {
				sadlModel.append("\n\n// Object properties without specified range:\n");
				sadlModel.append(tempSb);
			}
		}
		
		List<DatatypeProperty> dtProperties = getConcepts().getDtProperties();
		if (isVerboseMode() || !dtProperties.isEmpty()) {
			StringBuilder tempSb = new StringBuilder();
			for (int i = 0; i < dtProperties.size(); i++) {
				OntResource prop = dtProperties.get(i);
				addSuperPropertiesWithoutRange(getConcepts(), tempSb, prop);
			}
			if (isVerboseMode() || tempSb.length() > 0) {
				sadlModel.append("\n\n// Datatype properties without specified range:\n");
				sadlModel.append(tempSb);
			}
		}
		
		if (isVerboseMode() || !getConcepts().getClasses().isEmpty()) {
			sadlModel.append("\n\n// Class definitions:\n");
			List<OntClass> classes = getConcepts().getClasses();
			for (int i = 0; i < classes.size(); i++) {
				OntClass cls = classes.get(i);
				sadlModel.append(classToSadl(getConcepts(), cls));
			}
		}
		
		if (isVerboseMode() || objProperties.size() > 0) {
			StringBuilder tempSb = new StringBuilder();
			objProperties = getConcepts().getObjProperties();
			for (int i = 0; i < objProperties.size(); i++) {
				OntResource prop = objProperties.get(i);
				if (!getConcepts().getCompleted().contains(prop)) {
					tempSb.append(objPropertyToSadl(getConcepts(), prop));
				}
			}
			if (isVerboseMode() || tempSb.length() > 0) {
				sadlModel.append("\n\n// Other object Properties:\n");
				sadlModel.append(tempSb);
			}
		}
		
		if (isVerboseMode() || dtProperties.size() > 0) {
			StringBuilder tempSb = new StringBuilder();
			dtProperties = getConcepts().getDtProperties();
			for (int i = 0; i < dtProperties.size(); i++) {
				OntResource prop = dtProperties.get(i);
				if (!getConcepts().getCompleted().contains(prop)) {
					if (!ignoreNamespace(prop, true)) {
						tempSb.append(dtPropertyToSadl(getConcepts(), prop));
					}
				}
			}
			if (isVerboseMode() || tempSb.length() > 0) {
				sadlModel.append("\n\n// Other datatype Properties:\n");
				sadlModel.append(tempSb);
			}
		}
		
		if (isVerboseMode() || !getConcepts().getInstances().isEmpty()) {
			sadlModel.append("\n\n// Individuals:\n");
			List<Individual> instances = getConcepts().getInstances();
			for (int i = 0; i < instances.size(); i++) {
				Individual inst = instances.get(i);
				sadlModel.append(individualToSadl(getConcepts(), inst, false));
			}
		}
		
		if (isVerboseMode() || !getConcepts().getUnMappedRestrictions().isEmpty()) {
			sadlModel.append("\n\n// Other restrictions:\n");
			List<Restriction> ress = getConcepts().getUnMappedRestrictions();
			for (int i = 0; i < ress.size(); i++) {
				Restriction res = ress.get(i);
				boolean invalid = false;
				// what is a subclasses of this restriction?
				StmtIterator oitr = theModel.listStatements(null, RDFS.subClassOf, res);
				while (oitr.hasNext()) {
					Statement s1 = oitr.nextStatement();
					logger.debug(s1.toString());
					Resource subj = s1.getSubject();
					if (subj.canAs(Ontology.class) || subj.equals(OWL.Ontology)) {
						sadlModel.append("// restriction on Ontology not supported in SADL: \n    // ");
						sadlModel.append(restrictionToString(getConcepts(), null, res));
						sadlModel.append(System.lineSeparator());
						invalid = true;
						break;
					}
				}
				if (!invalid) {
					sadlModel.append(restrictionToString(getConcepts(), null, res));
					addEndOfStatement(sadlModel, 1);
				}
			}
		}
		
		if (isVerboseMode() || !getConcepts().getStatements().isEmpty()) {
			getConcepts().getStatements().removeAll(statementsProcessed);
			StringBuilder otherSB = new StringBuilder();
			Iterator<Statement> stmtitr = getConcepts().getStatements().iterator();
			while (stmtitr.hasNext()) {
				Statement s = stmtitr.next();
				String stmtstr = statementToString(s);
				if (stmtstr != null) {
					otherSB.append(stmtstr);
					statementsProcessed.add(s);
				}
			}
			getConcepts().getStatements().removeAll(statementsProcessed);
			stmtitr = getConcepts().getStatements().iterator();
			while (stmtitr.hasNext()) {
				Statement s = stmtitr.next();
				Iterator<Statement> pitr = statementsProcessed.iterator();
				boolean alreadyDone = false;
				while (pitr.hasNext()) {
					if (pitr.next().getObject().equals(s.getSubject())) {
						alreadyDone = true;
						break;
					}
				}
				if (!alreadyDone) {
					String stmtstr = blankNodeSubjectStatementToString(s);
					if (stmtstr != null && !stmtstr.trim().equals(".")) {
						otherSB.append(stmtstr);
					}
				}
			}
			if (otherSB.length() > 0) {
				sadlModel.append("\n\n// Other statements:\n");
				sadlModel.append(otherSB.toString());
			}

		}
		
		if (isVerboseMode() && verboseModeStringBuilder.length() > 0) {
			sadlModel.append("\n\n");
			sadlModel.append(verboseModeStringBuilder);
		}
	}

	private void initialize() throws OwlImportException {
		setConcepts(new ModelConcepts());

		theModel.getDocumentManager().setProcessImports(false);
		Map<String,String> modelPrefixMap = theModel.getNsPrefixMap();
		if (modelPrefixMap != null) {
			Iterator<String> pitr = modelPrefixMap.keySet().iterator();
			while (pitr.hasNext()) {
				String prefix = pitr.next();
				String uri = modelPrefixMap.get(prefix);
				if (getBaseUri() != null && stripNamespaceDelimiter(uri).equals(stripNamespaceDelimiter(getBaseUri()))) {
					setPrefix(prefix);
				}
				qNamePrefixes.put(uri, prefix);
			}
		}
		if (logger.isDebugEnabled()) {
			OutputStream os = new ByteArrayOutputStream();
			theModel.getBaseModel().write(os);
			logger.debug(os.toString());
		}
		getSpec().getDocumentManager().setProcessImports(false);
		theBaseModel = ModelFactory.createOntologyModel(getSpec(), theModel);
		
		if (allTokens == null) {
			allTokens = getSadlKeywords();
		}
		if (getBaseUri() == null) {
			String uri = modelPrefixMap.get("");
			if (uri != null) {
				setBaseUri(uri);
			}
			else {
				try {
					Writer writer = new StringWriter();
					theModel.write(writer, "RDF/XML");
					Optional<String> xmlbaseuri = new XMLHelper().tryReadBaseUri(writer.toString());
					if (xmlbaseuri.isPresent()) {
						uri = xmlbaseuri.get();
					}
				}
				catch (Exception e) {
					throw new OwlImportException(e.getMessage(), e);
				}
				if (uri == null) {
					throw new OwlImportException("Namespace of model to import cannot be identified");
				}
			}
		}
		if (getBaseUri() == null) {
			ExtendedIterator<Ontology> eitr = theModel.listOntologies();
			while (eitr.hasNext()) {
				Ontology ont = eitr.next();
				setBaseUri(ont.getURI());
				break;		// will this model's ontology always be first?
			}
		}
		if (getBaseUri() == null) {
			Map<String, String> map = theModel.getNsPrefixMap();
			Iterator<String> mitr = map.keySet().iterator();
			while (mitr.hasNext()) {
				String prefix = mitr.next();
				logger.debug("Mapping: " + prefix + " = " + map.get(prefix));
			}			setBaseUri("http://sadl.org/baseless");
		}
		if (getBaseUri().endsWith("#")) {
			setBaseUri(getBaseUri().substring(0, getBaseUri().length() - 1));
		}
	}
	
	public static final String SADL_BASE_MODEL_URI = "http://sadl.org/sadlbasemodel";
	public static final String SADL_LIST_MODEL_URI = "http://sadl.org/sadllistmodel";
	public static final String SADL_IMPLICIT_MODEL_URI = "http://sadl.org/sadlimplicitmodel";
	public static final String SADL_BUILTIN_FUNCTIONS_URI = "http://sadl.org/builtinfunctions";

	private ModelConcepts concepts;

	private IConfigurationManager configMgr = null;

	private String stripNamespaceDelimiter(String ns) {
		if (ns.endsWith("#")) {
			ns = ns.substring(0, ns.length() - 1);
		}
		return ns;
	}

	private boolean isImplicitUri(String impUri) {
		if (impUri.equals(SADL_BASE_MODEL_URI)) {
			return true;
		}
		else if (impUri.equals(SADL_BUILTIN_FUNCTIONS_URI)) {
			return true;
		}
		else if (impUri.equals(SADL_IMPLICIT_MODEL_URI)) {
			return true;
		}
		else if (impUri.equals(SADL_LIST_MODEL_URI)) {
			return true;
		}
		return false;
	}

	private void addEndOfStatement(StringBuilder sb, int numLineFeeds) {
		if (!sb.toString().trim().endsWith(".")) {
			if (sb.length() > 1 && Character.isDigit(sb.charAt(sb.length() - 1))) {
				sb.append(" .");
			}
			else {
				sb.append(".");
			}
		}
		for (int i = 0; i < numLineFeeds; i++) {
			sb.append(System.lineSeparator());
		}
	}

	private void addRdfProperty(ModelConcepts concepts, StringBuilder sb, Property prop) throws OwlImportException {
		sb.append(rdfPropertyToSadl(concepts, prop));
		
	}

	private void addSuperPropertiesWithoutRange(ModelConcepts concepts,
			StringBuilder sb, OntResource prop) throws OwlImportException {
		if (!hasRange(prop)) {
			String thisPropString;
			if (prop.isObjectProperty() || concepts.getPseudoObjProperties().contains(prop)) {
				thisPropString = objPropertyToSadl(concepts, prop);
			}
			else {
				thisPropString = dtPropertyToSadl(concepts, prop);
			}
			if (concepts.getPseudoObjProperties().contains(prop)) {
				sb.append("// ");
				sb.append(prop.getLocalName());
				sb.append(" is not typed in the input but SADL requires typed properties\n//    --typed as object property to match subproperties\n");
			}
			else if (concepts.getPseudoDtProperties().contains(prop)) {
				sb.append("// ");
				sb.append(prop.getLocalName());
				sb.append(" is not typed in the input but SADL requires typed properties\n//    --typed as datatype property to match subproperties\n");
			}
			concepts.addCompleted(prop);
			NodeIterator nitr2 = prop.listPropertyValues(RDFS.subPropertyOf);
			if (nitr2.hasNext()) {
				RDFNode superprop = nitr2.nextNode();
				if (superprop.canAs(OntProperty.class)){
					OntProperty supProp = superprop.as(OntProperty.class);
					addSuperPropertiesWithoutRange(concepts, sb, supProp);
				}
			}
			sb.append(thisPropString);
		}
	}

	private boolean hasRange(OntResource prop) {
		NodeIterator nitr = prop.listPropertyValues(RDFS.range);
		if (!nitr.hasNext()) {
			return false;
		}
		else {
			RDFNode rng = nitr.nextNode();
			if (rng.equals(RDFS.Resource)) {
				// rdfs:Resource is treated (for now?) as not a range as it adds no information and creates a SADL error.
				return false;
			}
		}
		return true;
	}

	private boolean isRDFDatatypeString(String dturi, RDFNode value) {
		if (dturi.startsWith(XSD.getURI())) {
			// this is a direct type
			if (dturi.equals(XSD.xstring.getURI()) || dturi.equals(XSD.date.getURI()) || 
					dturi.equals(XSD.dateTime.getURI()) || dturi.equals(XSD.time.getURI()) || 
					dturi.equals(XSD.anyURI.getURI())) {
				return true;
			}
			else {
				return false;
			}
		}
		Resource rsrc = theModel.getResource(dturi);
		// is there an equivalent class?
		List<String> intersection = null;
		List<String> union = null;
		String xsdtype = null;
		RDFNode eqCls = null;
		StmtIterator eqitr = rsrc.listProperties(OWL.equivalentClass);
		if (eqitr.hasNext()) {
			eqCls = eqitr.nextStatement().getObject();
			if (eqCls.canAs(Resource.class)) {
				if (eqCls.canAs(UnionClass.class)) {
					union = new ArrayList<String>();
					RDFList opList = eqCls.as(UnionClass.class).getOperands();
					for (int i = 0; i < opList.size(); i++) {
						RDFNode opnode = opList.get(i);
						union.add(opnode.asResource().getLocalName());
					}
				}
				else if (eqCls.canAs(IntersectionClass.class)) {
					intersection  = new ArrayList<String>();
					RDFList opList = eqCls.as(IntersectionClass.class).getOperands();
					for (int i = 0; i < opList.size(); i++) {
						RDFNode opnode = opList.get(i);
						intersection.add(opnode.asResource().getLocalName());
					}
				}
				else {
					RDFNode odt = eqCls.as(OntResource.class).getPropertyValue(OWL2.onDatatype);
					if (odt != null && odt.canAs(Resource.class)) {
						String ns = odt.as(Resource.class).getNameSpace();
						if (ns.startsWith(XSD.getURI())) {
							xsdtype = odt.as(Resource.class).getLocalName();
						}
					}
				}
			}
		}
		else {
			System.err.println("Unable to determine if RDFDatatype '" + dturi + " has string value.");
		}
		if (xsdtype != null && xsdtype.equals(XSD.xstring.getLocalName())) {
			return true;
		}
		if (union != null) {
			if (union.contains(XSD.xstring.getLocalName())) {
				if (value.asLiteral().getValue() instanceof Number) {
					return false;
				}
				return true;
			}
		}
		return false;
	}
	
	public String datatypeToSadl(String rsrcUri) throws OwlImportException {
		OntResource rsrc = theModel.getOntResource(rsrcUri);
		if (rsrc != null) {
			return datatypeToSadl(getConcepts(), rsrc).toString();
		}
		return null;
	}

	private Object datatypeToSadl(ModelConcepts concepts,
			OntResource rsrc) throws OwlImportException {
		StringBuilder sb = new StringBuilder();
		if (rsrc.isURIResource()) {
			// is there an equivalent class?
			List<String> intersection = null;
			List<String> union = null;
			String xsdtype = null;
			RDFNode eqCls = null;
			StmtIterator eqitr = rsrc.listProperties(OWL.equivalentClass);
			if (eqitr.hasNext()) {
				eqCls = eqitr.nextStatement().getObject();
				if (eqCls.canAs(Resource.class)) {
					if (eqCls.canAs(UnionClass.class)) {
						union = new ArrayList<String>();
						RDFList opList = eqCls.as(UnionClass.class).getOperands();
						for (int i = 0; i < opList.size(); i++) {
							RDFNode opnode = opList.get(i);
							union.add(opnode.asResource().getLocalName());
						}
					}
					else if (eqCls.canAs(IntersectionClass.class)) {
						intersection  = new ArrayList<String>();
						RDFList opList = eqCls.as(IntersectionClass.class).getOperands();
						for (int i = 0; i < opList.size(); i++) {
							RDFNode opnode = opList.get(i);
							intersection.add(opnode.asResource().getLocalName());
						}
					}
					else {
						RDFNode odt = eqCls.as(OntResource.class).getPropertyValue(OWL2.onDatatype);
						if (odt != null && odt.canAs(Resource.class)) {
							String ns = odt.as(Resource.class).getNameSpace();
							if (ns.startsWith(XSD.getURI())) {
								xsdtype = odt.as(Resource.class).getLocalName();
							}
						}
					}
				}
			}
			if (eqCls == null) {
				sb.append("// Error: RDFDatatype '" + uriToSadlString(concepts, rsrc) + "' does not have an equivalent class.\n");
			}
			else if ((union == null || union.size() == 0) && (intersection == null || intersection.size() == 0) && xsdtype == null) {
				sb.append("// Error: RDFDatatype '" + uriToSadlString(concepts, rsrc) + "' does not have a union, intersection, nor xsd datatype.\n");
			}
			else {
				sb.append(uriToSadlString(concepts, rsrc));
				addRdfsAnnotations(sb, rsrc);
				sb.append(" is a type of ");
				if (union != null && union.size() > 0) {
					sb.append("{");
					for (int i = 0; i < union.size(); i++) {
						if (i > 0) {
							sb.append(" or ");
						}
						sb.append(union.get(i));
					}
					sb.append("}");
				}
				else if (intersection != null && intersection.size() > 0) {
					sb.append("{");
					for (int i = 0; i < union.size(); i++) {
						if (i > 0) {
							sb.append(" and ");
						}
						sb.append(union.get(i));
					}
					sb.append("}");
				}	
				else if (xsdtype != null) {
					String facets = rdfDatatypeFacetsToSadl(eqCls);
					if (facets == null) {
						sb.append("// Error: RDFDatatype '" + uriToSadlString(concepts, rsrc) + "' is of type '" + xsdtype + "' but has no facets to define it.");
					}
					else {
						sb.append(xsdtype);
						sb.append(" ");
						sb.append(facets);
					}
				}
				addEndOfStatement(sb, 1);
			}
		}
		else {
			sb.append("// anonymous rdfs:Datatype encountered: ");
			sb.append(rsrc.toString());
			sb.append(System.lineSeparator());
		}
		return sb.toString();
	}

	private String rdfDatatypeFacetsToSadl(RDFNode eqCls) {
		List<String> enums = null;
		String len = null;
		String minLen = null;
		String maxLen = null;
		String pattern = null;
		String minIncl = null;
		String maxIncl = null;
		String minExcl = null;
		String maxExcl = null;
		NodeIterator nitr = eqCls.as(OntResource.class).listPropertyValues(OWL2.withRestrictions);
		while (nitr.hasNext()) {
			 RDFNode n = nitr.next();
			 if (n != null && n.canAs(RDFList.class)){
				 RDFList lst = n.as(RDFList.class);
				 List<Triple> list = rdfListToList(lst.asNode(), null);
				 if (list != null) {
					 Iterator<Triple> titr = list.iterator();
					 while (titr.hasNext()) {
						 Triple t = titr.next();
						 if (t.getMatchPredicate().equals(xsdProperty("minLength").asNode())) {
							 minLen = t.getMatchObject().getLiteralValue().toString();
						 }
						 else if (t.getMatchPredicate().equals(xsdProperty("maxLength").asNode())) {
							 maxLen = t.getMatchObject().getLiteralValue().toString();
						 }
						 else if (t.getMatchPredicate().equals(xsdProperty("length").asNode())) {
							 len = t.getMatchObject().getLiteralValue().toString();
						 }
						 else if (t.getMatchPredicate().equals(xsdProperty("pattern").asNode())) {
							 pattern = t.getMatchObject().getLiteralValue().toString();
						 }
						 else if (t.getMatchPredicate().equals(xsdProperty("enumeration").asNode())) {
							 if (enums == null) {
								 enums = new ArrayList<String>();
							 }
							 enums.add(t.getMatchObject().getLiteralValue().toString());
						 }
						 else if (t.getMatchPredicate().equals(xsdProperty("minInclusive").asNode())) {
							 minIncl = t.getMatchObject().getLiteralValue().toString();
						 }
						 else if (t.getMatchPredicate().equals(xsdProperty("maxInclusive").asNode())) {
							 maxIncl = t.getMatchObject().getLiteralValue().toString();
						 }
						 else if (t.getMatchPredicate().equals(xsdProperty("minExclusive").asNode())) {
							 minExcl = t.getMatchObject().getLiteralValue().toString();
						 }
						 else if (t.getMatchPredicate().equals(xsdProperty("maxExclusive").asNode())) {
							 maxExcl = t.getMatchObject().getLiteralValue().toString();
						 }
						 else if (t.getMatchPredicate().equals(xsdProperty("regex").asNode())) {
							 pattern = t.getMatchObject().getLiteralValue().toString();
						 }
					 }
				 }
			 }
			 else if (n.canAs(OntResource.class)){
				 StmtIterator sitr = n.as(OntResource.class).listProperties();
				 while (sitr.hasNext()) {
					 logger.debug(sitr.nextStatement().toString());									 
				 }
			 }
		}
		StringBuilder sb = new StringBuilder();
		if (minIncl != null || minExcl != null || maxIncl != null || maxExcl != null) {
			if (minIncl != null) {
				sb.append("[");
				sb.append(minIncl);
				sb.append(",");
			}
			else if (minExcl != null) {
				sb.append("(");
				sb.append(minExcl);
				sb.append(",");
			}
			else {
				sb.append("( ,");
			}
			if (maxIncl != null) {
				sb.append(maxIncl);
				sb.append("]");
			}
			else if (maxExcl != null) {
				sb.append(maxExcl);
				sb.append(")");
			}
			else {
				sb.append(" )");
			}
		}
		else if (len != null) {
			sb.append("length ");
			sb.append(len);
		}
		else if (minLen != null || maxLen != null) {
			sb.append("length ");
			if (minLen != null) {
				sb.append(minLen);
			}
			else {
				sb.append("0");
			}
			sb.append("-");
			if (maxLen != null) {
				sb.append(maxLen);
			}
		}
		else if (pattern != null) {
			sb.append("pattern");
		}
		return sb.toString();
	}

	private final static Property xsdProperty(String local) {
		return ResourceFactory.createProperty(XSD.getURI() + local);
	}
	
	private List<Triple> rdfListToList(Node lst, List<Triple> trLst) {
		ExtendedIterator<Triple> litr = theModel.getGraph().find(lst, RDF.Nodes.first, null);
		while (litr.hasNext()) {
			Triple t = litr.next();
			logger.debug("Triple with 'first': " + t.toString());
			Node mo = t.getMatchObject();
			ExtendedIterator<Triple> litr2 = theModel.getGraph().find(mo, null, null);
			while (litr2.hasNext()) {
				Triple t2 = litr2.next();
				if (t2.getMatchPredicate().equals(RDF.Nodes.rest)) {
					if (!t2.getMatchObject().equals(RDF.Nodes.nil)) {
						trLst = rdfListToList(t2.getMatchObject(), trLst); 
					}
				}
				else {
					if (trLst == null) {
						trLst = new ArrayList<Triple>();
					}
					if (!trLst.contains(t2)) {
						trLst.add(t2);
						logger.debug("Adding triple: " + t2.toString());
					}
				}
			}
		}
		return trLst;
	}
	
	private String individualNameAndAnnotations(ModelConcepts concepts, Individual inst) throws OwlImportException {
		StringBuilder sb = new StringBuilder();
		if (!isNeverUsePrefixes() && !inst.getNameSpace().equals(getBaseUri()+'#')) {
			if (qNamePrefixes.containsKey(inst.getNameSpace())) {
				String prefix = qNamePrefixes.get(inst.getNameSpace());
				sb.append(checkLocalnameForKeyword(inst.getLocalName()));
				sb.append(" /* Given URI of instance is '");
				sb.append(inst.getNameSpace());
				sb.append("'");
				if (prefix != null) {
					sb.append("(prefix ");
					sb.append(prefix);
					sb.append(")");
				}
				sb.append(" but creating an instance in another namespace is not currently supported in SADL; creating in this namespace */\n");
			}
			else {
				String ln = inst.getLocalName();
				if (ln != null && ln.length() > 0) {
					sb.append(checkLocalnameForKeyword(inst.getLocalName()));
				}
				else if (inst.isURIResource()){
					sb.append(inst.getURI());
				}
				else {
					sb.append(inst.toString());
				}
			}
		}
		else {
			sb.append(uriToSadlString(concepts, inst));
		}
		addRdfsAnnotations(sb, inst);
		return sb.toString();
	}

	private String listToSadl(ModelConcepts concepts, Individual inst, boolean embeddedBNode) throws OwlImportException {
		Individual rest = inst;
		StringBuilder sb = new StringBuilder("[");
		int cntr = 0;
		while (rest != null) {
			Statement fstmt = rest.getProperty(RDF.first);
			if (fstmt != null) {
				RDFNode first = fstmt.getObject();
				if (cntr++ > 0) sb.append(",");
				sb.append(rdfNodeToSadlString(concepts, first, true));
				Statement rststmt = rest.getProperty(RDF.rest);
				if (rststmt != null) {
					RDFNode rstobj = rststmt.getObject();
					if (rstobj != null && !rstobj.equals(RDF.nil) && rstobj.canAs(Individual.class)) {
						rest = rstobj.as(Individual.class);
					}
					else {
						rest = null;
					}
				}
			}
			else {
				rest = null;
			}
		}
		sb.append("]");
		return sb.toString();
	}
	
	private String sadlListToSadl(ModelConcepts concepts, Individual inst, boolean embeddedBNode) throws OwlImportException {
		Individual rest = inst;
		StringBuilder sb = new StringBuilder("[");
		int cntr = 0;
		while (rest != null) {
			RDFNode first = getSadlFirstPropertyValue(rest); //rest.getProperty(getSadlListFirstProperty()).getObject();
			if (cntr++ > 0) sb.append(",");
			sb.append(rdfNodeToSadlString(concepts, first, true));
			RDFNode rstobj = getSadlRestPropertyValue(rest);
//			Statement rststmt = rest.getProperty(getSadlListRestProperty());
//			if (rststmt != null) {
//				RDFNode rstobj = rststmt.getObject();
				if (rstobj != null && rstobj.canAs(Individual.class)) {
					rest = rstobj.as(Individual.class);
				}
				else {
					rest = null;
				}
//				statementsProcessed.add(rststmt);
//			}
//			else {
//				rest = null;
//			}
		}
		sb.append("]");
		return sb.toString();
	}	
	
	/**
	 * Method to determine if an Individual is a SADL typed list
	 * @param lstInst -- the instance that might be of type SADL typed list
	 * @return -- true if a SADL List else false
	 */
	private boolean isaSadlList(Individual lstInst) {
		StmtIterator pitr = lstInst.listProperties();
		while (pitr.hasNext()) {
			if (pitr.nextStatement().getPredicate().getURI().equals(SADL_LIST_MODEL_URI + "#first")) {
				return  true;
			}
		}
		return false;
	}

	private RDFNode getSadlRestPropertyValue(Individual lstInst) {
		StmtIterator pitr = lstInst.listProperties();
		while (pitr.hasNext()) {
			Statement nxtStmt = pitr.nextStatement();
			if (nxtStmt.getPredicate().getURI().equals(SADL_LIST_MODEL_URI + "#rest")) {
				statementsProcessed.add(nxtStmt);
				return nxtStmt.getObject();
			}
		}
		return null;
	}

	private RDFNode getSadlFirstPropertyValue(Individual lstInst) {
		StmtIterator pitr = lstInst.listProperties();
		while (pitr.hasNext()) {
			Statement nxtStmt = pitr.nextStatement();
			if (nxtStmt.getPredicate().getURI().equals(SADL_LIST_MODEL_URI + "#first")) {
				statementsProcessed.add(nxtStmt);
				return nxtStmt.getObject();
			}
		}
		return null;
	}

	/**
	 * Method to convert an Individual, identifed by instUri, to a SADL statement
	 * @param instUri
	 * @param embeddedBNode
	 * @return
	 * @throws OwlImportException
	 */
	public String individualToSadl(String instUri, boolean embeddedBNode) throws OwlImportException {
		Individual inst = theModel.getIndividual(instUri);
		if (inst != null) {
			return individualToSadl(getConcepts(), inst, embeddedBNode);
		}
		return null;
	}
	
	/**
	 * Method to convert an equation, identified by eqUri, to one or more SADL statements
	 * @param eqUri
	 * @param embeddedBNode
	 * @param iConfigurationManager
	 * @return
	 */
	public String[] equationToSadl(String eqUri, boolean embeddedBNode, IConfigurationManager iConfigurationManager) {
		Individual inst = theModel.getIndividual(eqUri);
		if (inst != null) {
			String[] returnvals = null;
			StringBuilder sb = new StringBuilder();
			StringBuilder sb2 = null;
			ExtendedIterator<Resource> titr = inst.listRDFTypes(true);
			while (titr.hasNext()) {
				Resource typ = titr.next();
				if (typ.isURIResource() && typ.getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNAL_EQUATION_CLASS_URI)) {
					sb.append("External ");
				}
				else {
					sb.append("^Equation ");
				}
			}
			sb.append(inst.getLocalName());
			Resource dfr = theModel.getResource(SadlConstants.SADL_IMPLICIT_MODEL_DERIVEDFROM_PROPERTY_URI);
			if (dfr != null && dfr.canAs(Property.class)) {
				StmtIterator stmtitr = theModel.listStatements(inst, dfr.as(Property.class), (RDFNode)null);
				if (stmtitr.hasNext()) {
					sb.append(" (");
					int cnt = 0;
					do {
						RDFNode dfn = stmtitr.nextStatement().getObject();
						if (dfn.isURIResource()) {
							if (cnt++ > 0) {
								sb.append(", ");
							}
							sb.append("derivedFrom ");
							sb.append(dfn.asResource().getLocalName());
						}
					} while (stmtitr.hasNext());
					sb.append(") ");
				}				
			}
			sb.append("(");
			
			try {
				IReasoner reasoner = iConfigurationManager.getReasoner();
				if (!reasoner.isInitialized()) {
					reasoner.initializeReasoner(theModel, getBaseUri(), null, null);
				}
				List<Object> params = Arrays.asList(new NamedNode(eqUri));
				String pq = reasoner.parameterizeQuery(SparqlQueries.ARGUMENTS_QUERY, params);
				pq = reasoner.prepareQuery(pq);
				com.ge.research.sadl.reasoner.ResultSet rs = reasoner.ask(pq);	// ?argname ?argtype
				if (rs != null) {
					rs.setShowNamespaces(false);
					for (int r = 0; r < rs.getRowCount(); r++) {
						if (r > 0) sb.append(", ");
						sb.append(rs.getResultAt(r, 1));
						sb.append(" ");
						sb.append(rs.getResultAt(r, 0));
					}
				}
				sb.append(") returns ");
				
				pq = reasoner.parameterizeQuery(SparqlQueries.RETURN_TYPES_QUERY, params);
				pq = reasoner.prepareQuery(pq);
				rs = reasoner.ask(pq);	// ?retname ?rettype
				if (rs != null) {
					rs.setShowNamespaces(false);
					for (int r = 0; r < rs.getRowCount(); r++) {
						sb.append(rs.getResultAt(r, 1));
					}
				}
				sb.append(": ");
				RDFNode exturi = inst.getPropertyValue(theModel.getProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNALURL_PROPERTY_URI));
				if (exturi != null) {
					sb.append("\"");
					sb.append(exturi.asLiteral().getValue().toString());
					sb.append("\".");
				}
				
				RDFNode expr = inst.getPropertyValue(theModel.getProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXPRESSTION_PROPERTY_URI));
				if (expr != null && expr.isResource()) {
					sb2 = new StringBuilder();
					StmtIterator stmtitr = expr.asResource().listProperties(theModel.getProperty(SadlConstants.SADL_IMPLICIT_MODEL_SCRIPT_PROPERTY_URI));
					if (stmtitr.hasNext()) {
						sb2.append(inst.getLocalName());
						int cntr = 0;
						while (stmtitr.hasNext()) {
							if (cntr++ > 0) sb2.append(", ");
							sb2.append(" has expression (a Script ");
							Statement stmt = stmtitr.nextStatement();
							RDFNode scrpt = stmt.getObject();
							if (scrpt != null && scrpt.isLiteral()) {
								String scrptStr = scrpt.asLiteral().getValue().toString();
								Statement stmt2 = expr.asResource().getProperty(theModel.getProperty(SadlConstants.SADL_IMPLICIT_MODEL_LANGUAGE_PROPERTY_URI));
								if (stmt2 != null) {
									RDFNode lang = stmt2.getObject();
									if (lang.isURIResource()) {
										String langStr = lang.asResource().getLocalName();
										if (langStr != null) {
											sb2.append("with language ");
											sb2.append(langStr);
											sb2.append(", ");
										}
										if (scrptStr != null) {
											sb2.append("with script ");
											sb2.append("\"");
											sb2.append(escapeDoubleQuotes(scrptStr));
											sb2.append("\"");
										}
									}
								}
							}
							sb2.append(")");
						}
						sb2.append(".");
					}
				}
				if (sb2 != null) {
					returnvals = new String[2];
					returnvals[0] = sb.toString();
					returnvals[1] = sb2.toString();
				}
				else {
					returnvals = new String[1];
					returnvals[0] = sb.toString();
				}
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvalidNameException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (QueryParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (QueryCancelledException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ReasonerNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (AmbiguousNameException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			NodeIterator nitr = inst.listPropertyValues(theModel.getOntProperty(SadlConstants.SADL_IMPLICIT_MODEL_ARGUMENTS_PROPERTY_URI));
			while (nitr.hasNext()) {
				RDFNode arg = nitr.next();
				if (arg.isResource()) {
					System.out.println(arg.toString());
				}
			}
			return returnvals;
		}
		
		return null;
	}

	/**
	 * Method to replace each double quote (") with an escaped double quote (\")
	 * @param strIn -- input string
	 * @return -- output string
	 */
	private String escapeDoubleQuotes(String strIn) {
		return strIn.replace("\"", "\\\"");
	}
	
	private String individualToSadl(ModelConcepts concepts, Individual inst, boolean embeddedBNode) throws OwlImportException {
		StringBuilder sb = new StringBuilder();
		boolean bnode = false;
		boolean isEquation = false;
		if (inst.isURIResource()) {
			isEquation = isEquationType(inst);
			if (isEquation) {
				String[] sds = equationToSadl(inst.getURI(), false, getConfigurationManager());
				for (String sd : sds) {
					sb.append(sd);
					addEndOfStatement(sb, 1);
				}
			}
			else {
				sb.append(individualNameAndAnnotations(concepts, inst));
				if (isNewLineAtEndOfBuffer(sb)) {
					sb.append("    ");
				}
				sb.append(" is a ");
			}
		}
		else {
			sb.append("(a ");
			bnode = true;
		}
		if (!isEquation) {
			StmtIterator stmtitr = theBaseModel.listStatements(inst, RDF.type, (RDFNode)null);
			int itercnt = 0;
			int bracketLoc = 0;
			boolean bNamedIndividualFound = false;
			boolean intersectionClass = false;
			while (stmtitr.hasNext()) {
				RDFNode type = stmtitr.nextStatement().getObject();
				if (type.isResource()) {
					if (type.isURIResource() && type.asResource().getNameSpace().equals(OWL.NS) && 
							type.asResource().getLocalName().equals("NamedIndividual")) {
						// this is an OWL 2 construct that must be left out of SADL
						bNamedIndividualFound = true;
					}
					else if (type.isURIResource() && type.asResource().getURI().equals(OWL.Class.getURI())) {
						// ignore
					}
					else {
						if (itercnt == 0) {
							if (stmtitr.hasNext()) {
								intersectionClass = true;
							}
							if (intersectionClass) {
								bracketLoc = sb.length();
								sb.append("{");
							}
						}
						if (itercnt++ > 0) {
							sb.append(" and ");
						}
						sb.append(uriToSadlString(concepts, type.asResource()));
					}
				}
			}
			if (bNamedIndividualFound) {
				if (itercnt == 0) {
					// only NamedIndividual
					sb.setLength(0);
					return sb.toString();
				}
				if (itercnt == 1) {
					// remove opening curley bracket
					int curlen = sb.length();
					String afterBracket = sb.substring(bracketLoc + 1); 
					sb.replace(bracketLoc, curlen, afterBracket);
				}
			}
			else if (intersectionClass) {
				sb.append("}");
			}
			addResourceProperties(sb, concepts, inst, embeddedBNode);
			if (bnode) {
				sb.append(")");
			}
			else {
				addEndOfStatement(sb, 1);
			}
		}
		return sb.toString();
	}

	private IConfigurationManager getConfigurationManager() throws OwlImportException {
		if (configMgr == null) {
			try {
				configMgr = ConfigurationManagerFactory.getConfigurationManager(IConfigurationManager.dummyModelFolderPath, null);
			} catch (ConfigurationException e) {
				throw new OwlImportException(e.getMessage(), e);
			}
		}
		return configMgr;
	}

	private boolean isEquationType(Individual inst) {
		Resource rdfType = inst.getRDFType(true);
		if (rdfType != null) {
			if (rdfType.getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNAL_EQUATION_CLASS_URI) ||
					rdfType.getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_EQUATION_CLASS_URI)) {
				return true;
			}
		}
		return false;
	}

	private void addResourceProperties(StringBuilder sb, ModelConcepts concepts, Resource inst,
			boolean embeddedBNode) throws OwlImportException {
		StmtIterator insitr = inst.listProperties();
		int cntr = 0;
		while (insitr.hasNext()) {
			Statement s = insitr.next();
			if (s.getPredicate().equals(RDF.type) || s.getPredicate().equals(RDFS.label) || 
					s.getPredicate().equals(RDFS.comment) || s.getPredicate().equals(RDFS.seeAlso)) {
				continue;
			}
			if (embeddedBNode) {
				if (cntr > 0) {
					sb.append(",");
				}
				sb.append("\n    with ");
			}
			else {
				sb.append(",\n    has ");
			}
			sb.append(uriToSadlString(concepts, s.getPredicate()));
			sb.append(" ");
			sb.append(rdfNodeToSadlString(concepts, s.getObject(), false));
			statementsProcessed.add(s);
			cntr++;
		}
	}

	private String statementToString(Statement s) throws OwlImportException {
		if (s.getSubject().isAnon()) {
			return null;	// wait and see if this is the object of a statement
		}
		if (stripNamespaceDelimiter(s.getSubject().getNameSpace()).equals(getBaseUri())) {
			return null;	// the model cannot be a subject
		}
		if (!s.getPredicate().equals(RDF.type) && ignoreNamespace(s.getPredicate(), false)) {
			return null;
		}
		if (s.getPredicate().equals(RDF.type) && s.getObject().isURIResource()) {
			String uri = s.getObject().asResource().getURI();
			if (uri.equals(OWL.NS + "NamedIndividual")){
				return null;
			}
			else if (uri.equals(OWL.Ontology.getURI())) {
				return null;
			}
			else if (uri.equals(OWL.Class.getURI())) {
				return null;
			}
		}
		StringBuilder sb = new StringBuilder();
		Resource subj = s.getSubject();
		if (subj.canAs(Individual.class)) {
			sb.append(individualToSadl(getConcepts(), subj.as(Individual.class), false));
		}
		else if (subj.canAs(OntClass.class)){
			sb.append(ontClassToString(subj.as(OntClass.class), null));
		}
		else if (subj.isURIResource()) {
			sb.append(uriToSadlString(getConcepts(), subj));
		}

		if (s.getPredicate().equals(RDF.type)) {
			sb.append(" is a ");
		}
		else {
			sb.append(" has ");
			sb.append(uriToSadlString(getConcepts(), s.getPredicate()));
			sb.append(" ");
		}
		RDFNode obj = s.getObject();
		if (obj.isAnon()) {
			sb.append(blankNodeToString(obj.asResource(), true));
		}
		else if (obj.isResource() && obj.asResource().equals(OWL.Ontology)) {
			return null;	// the object can't be owl:Ontology
		}
		else {
			sb.append(rdfNodeToString(obj, 0));
		}
		sb.append(".\n");
		return sb.toString();
	}

	private String blankNodeSubjectStatementToString(Statement s) throws OwlImportException {
		if (ignoreNamespace(s.getPredicate(), false)) {
			return null;
		}
		StringBuilder sb = new StringBuilder();
		Resource bnodeSubj = s.getSubject();
		sb.append(blankNodeToString(bnodeSubj, false));
		sb.append(".\n");
		return sb.toString();
	}

	private String blankNodeToString(Resource bnodeSubj, boolean embedded) throws OwlImportException {
		StringBuilder sb = new StringBuilder();
		if (bnodeSubj.canAs(Individual.class)) {
			return individualToSadl(getConcepts(), bnodeSubj.as(Individual.class), true);
		}
		// find type statement
		StmtIterator stmtitr = theModel.listStatements(bnodeSubj, RDF.type, (RDFNode)null);
		if (stmtitr.hasNext()) {
			RDFNode typ = stmtitr.nextStatement().getObject();
			if (typ.canAs(OntClass.class)) {
				if (embedded) {
					sb.append("(");
					sb.append("a ");
				}
				else {
					sb.append("A ");
				}
				sb.append(ontClassToString(typ.as(OntClass.class), null));
				addResourceProperties(sb, getConcepts(), bnodeSubj, true);
				if (embedded) {
					sb.append(")");
				}
			}
		}
		return sb.toString();
	}

	private String annotationsToSadl(ModelConcepts concepts,
			AnnotationProperty ann) throws OwlImportException {
		StringBuilder sb = new StringBuilder();
		sb.append(uriToSadlString(concepts, ann));
		addRdfsAnnotations(sb, ann);
		sb.append(" is a type of annotation.\n");
		return sb.toString();
	}

	public String rdfPropertyToSadl(String propUri) throws OwlImportException {
		Property prop = theModel.getProperty(propUri);
		if (prop != null) {
			return rdfPropertyToSadl(getConcepts(), prop);
		}
		return null;
	}
	
	private String rdfPropertyToSadl(ModelConcepts concepts, Property prop) throws OwlImportException {
		StringBuilder sb = new StringBuilder();
		sb.append(uriToSadlString(concepts, prop));
		sb.append(" is a property.\n");
		return sb.toString();
	}
	
	public String dtPropertyToSadl(String propUri) throws OwlImportException {
		OntResource or = theModel.getOntResource(propUri);
		if (or != null) {
			return dtPropertyToSadl(getConcepts(), or);
		}
		return null;
	}
	
	private String dtPropertyToSadl(ModelConcepts concepts, OntResource prop) throws OwlImportException {
		StringBuilder sb = new StringBuilder();
		boolean useTranslation = true;
		sb.append(uriToSadlString(concepts, prop));
		addRdfsAnnotations(sb, prop);
		if (prop.canAs(DatatypeProperty.class)) {
			useTranslation = propertyToSadl(sb, concepts, prop);
		}
		else {
			sb.append(" is a property");
		}
		if (!sb.toString().contains("with values")) {
			sb.append(" with values of type data");
		}
		if (useTranslation) {
			addEndOfStatement(sb, 1);
		}
		else {
			sb.setLength(0);
		}
		return sb.toString();
	}
	
	public String objPropertyToSadl(String propUri) throws OwlImportException {
		OntResource prop = theModel.getOntResource(propUri);
		if (prop != null) {
			return objPropertyToSadl(getConcepts(), prop);
		}
		return null;
	}

	private String objPropertyToSadl(ModelConcepts concepts, OntResource prop) throws OwlImportException {
		StringBuilder sb = new StringBuilder();
		boolean useTranslation = true;
		sb.append(uriToSadlString(concepts, prop));
		addRdfsAnnotations(sb, prop);
		if (prop.canAs(ObjectProperty.class)) {
			useTranslation = propertyToSadl(sb, concepts, prop);
		}
		else {
			sb.append(" is a property");
		}
		if (useTranslation) {
			addEndOfStatement(sb, 1);
		}
		else {
			sb.setLength(0);
		}
		return sb.toString();
	}

	public String classToSadl(String clsUri) throws OwlImportException {
		OntClass cls = theModel.getOntClass(clsUri);
		if (cls != null) {
			return classToSadl(getConcepts(), cls);
		}
		return null;
	}
	
	private String classToSadl(ModelConcepts concepts, OntClass cls) throws OwlImportException {
		StringBuilder sb = new StringBuilder();
		sb.append(uriToSadlString(concepts, cls));
		addRdfsAnnotations(sb, cls);
		List<Resource> supers = new ArrayList<Resource>();
		List<OntClass> mappedRestrictions = new ArrayList<OntClass>();
//		ExtendedIterator<OntClass> eitr = ((OntClass)cls.as(OntClass.class)).listSuperClasses(true);
		StmtIterator eitr = theModel.listStatements(cls, RDFS.subClassOf, (RDFNode)null);
		if (eitr.hasNext()) {
			while (eitr.hasNext()) {
				RDFNode spcls = eitr.next().getObject();
				if (spcls.canAs(OntClass.class)) { 
					if (concepts.getMappedRestrictions().containsValue(spcls)) {
						mappedRestrictions.add(spcls.as(OntClass.class));
					}
					else if (spcls.isAnon()) {
						if (spcls.as(OntClass.class).isRestriction()) {
							mappedRestrictions.add(spcls.as(OntClass.class));
						}
						else {
							supers.add(spcls.as(OntClass.class));
						}
					}
					else if (!spcls.equals(OWL.Thing)) {
						supers.add(spcls.as(OntClass.class));
					}
				}
				else {
					// this must be in an imported model
					supers.add(spcls.asResource());
				}
			}
		}
		if (supers.size() > 0) {
			sb.append(" is a type of ");			
			if (supers.size() > 1) {
				List<String> typeStrings = new ArrayList<String>();
				Iterator<Resource> spIter = supers.iterator();
				while (spIter.hasNext()) {
					Resource spcls = spIter.next();
					String spStr = uriToSadlString(concepts, spcls);
					if (!typeStrings.contains(spStr)) {
						typeStrings.add(spStr);
					}
				}
				if (typeStrings.size() > 1) {
					sb.append("{");
				}
				int itercnt = 0;
				for (String typeStr : typeStrings) {
					if (itercnt++ > 0) {
						sb.append(" and ");
					}
					sb.append(typeStr);
				}
				if (typeStrings.size() > 1) {
					sb.append("}");
				}
			}
			else {
				sb.append(uriToSadlString(concepts, supers.get(0)));
			}
		}
		else {
			sb.append(" is a class");
		}
		OntClass eqcls = cls.getEquivalentClass();
		if (eqcls != null) {
			if (concepts.getAnonClasses().contains(eqcls)) {
				concepts.getAnonClasses().remove(eqcls);
				String str = uriToSadlString(concepts, eqcls);
//				sb.append(" must be ");
				sb.append(str);
			}
		}
		// add properties with this class in the domain
		List<Property> pList = new ArrayList<Property>();
		StmtIterator eitr2 = theModel.listStatements(null, RDFS.domain, cls);
		while (eitr2.hasNext()) {
			Property p = eitr2.next().getSubject().as(Property.class);
			if (p.equals(RDFS.subClassOf) || p.canAs(AnnotationProperty.class)) { // p.isAnnotationProperty()) {
				continue;
			}
			pList.add(p);
		}
		// sort p alphabetically
		pList = sortResources(pList);
		for (int i = 0; i < pList.size(); i++) {
			Property p = pList.get(i);
			String key = null;
			if (p.isURIResource()) {
				key = p.getURI();
				key += cls.getURI();
			}
			// TODO remove p from list? what if it has other domain classes?
			sb.append(",\n");
			sb.append("    described by ");
			sb.append(uriToSadlString(concepts, p));
			if (!concepts.getCompleted().contains(p)) {
				addRdfsAnnotations(sb, p);
			}
			key = generateSadlRangeStatement(concepts, p, sb, key);
//
//			ExtendedIterator<? extends OntResource> eitr3 = p.listRange();
//			while (eitr3.hasNext()) {
//				OntResource r = eitr3.next();
//				if (!r.equals(RDFS.Resource)) {
//					sb.append(" with values of type ");
//					sb.append(uriToSadlString(concepts, r));
//				}
//			}
			propertyAlreadyProcessed(key);	// add to properties already processed
		}
		addEndOfStatement(sb, 1);
		if (concepts.getMappedRestrictions().containsKey(cls)) {
			List<Restriction> restList = concepts.getMappedRestrictions().get(cls);
			for (int i = 0; restList != null && i < restList.size(); i++) {
				Restriction res = restList.get(i);
				try {
					OntProperty op = res.getOnProperty();
					if (!concepts.getCompleted().contains(op)) {
						if (op.isObjectProperty()) {
							sb.append(objPropertyToSadl(concepts, op));
						}
						else {
							sb.append(dtPropertyToSadl(concepts, op));
						}
						concepts.addCompleted(op);
					}
				}
				catch (Throwable t) {
					concepts.addErrorMessage(t.getMessage());
				}
				sb.append(restrictionToString(concepts, cls, res));
				addEndOfStatement(sb, 1);
			}
		}
		if (mappedRestrictions != null) {
			for (OntClass res : mappedRestrictions) {
				if (res.isRestriction()) {
					try {
						OntProperty op = res.asRestriction().getOnProperty();
						if (!concepts.getCompleted().contains(op)) {
							if (op.isObjectProperty()) {
								sb.append(objPropertyToSadl(concepts, op));
							}
							else {
								sb.append(dtPropertyToSadl(concepts, op));
							}
							concepts.addCompleted(op);
						}
					}
					catch (Throwable t) {
						concepts.addErrorMessage(t.getMessage());
					}
				}
				sb.append(restrictionToString(concepts, cls, res.asRestriction()));
				addEndOfStatement(sb, 1);
			}
		}
		return sb.toString();
	}

	private List<Property> sortResources(List<Property> pList) {
		if (pList.isEmpty()) {
			return pList;
		}
		List<Property> newList = new ArrayList<Property>();
		newList.add(pList.get(0));
		for (int i = 1; i < pList.size(); i++) {
			Property nxt = pList.get(i);
			for (int j = 0; j < newList.size(); j++) {
				Property nlp = newList.get(j);
				String s1 = nxt.getURI();
				String s2 = nlp.getURI();
				if (s1.compareTo(s2) < 0) {
					newList.add(j, nxt);
					nxt = null;
					break;
				}
			}
			if (nxt != null) {
				newList.add(nxt);
			}
		}
		return newList;
	}

	private void addRdfsAnnotations(StringBuilder sb, Resource rsrc) throws OwlImportException {
		StmtIterator sitr = rsrc.listProperties(RDFS.label);
		if (sitr.hasNext()) {
//			addNewLineIfNotAtEndOfBuffer(sb);
			sb.append(" (alias ");
			int cntr = 0; 
			while (sitr.hasNext()) {
				RDFNode alias = sitr.nextStatement().getObject();
				if (cntr++ > 0) {
					sb.append(", ");
				}
				sb.append(rdfNodeToSadlString(null, alias, true));
			}
			sb.append(")");
		}
		sitr = rsrc.listProperties(RDFS.comment);
		if (sitr.hasNext()) {
			addNewLineIfNotAtEndOfBuffer(sb);
			sb.append("    (note ");
			int cntr = 0; 
			while (sitr.hasNext()) {
				RDFNode note = sitr.nextStatement().getObject();
				if (cntr++ > 0) {
					sb.append(", ");
				}
				sb.append(rdfNodeToSadlString(null, note, true));
			}
			sb.append(")\n    ");
		}
		sitr = rsrc.listProperties(RDFS.seeAlso);
		if (sitr.hasNext()) {
			addNewLineIfNotAtEndOfBuffer(sb);
			sb.append("    (see ");
			int cntr = 0; 
			while (sitr.hasNext()) {
				RDFNode seeAlso = sitr.nextStatement().getObject();
				if (cntr++ > 0) {
					sb.append(", ");
				}
				sb.append(rdfNodeToSadlString(null, seeAlso, true));
			}
			sb.append(")\n    ");
		}
	}

	private String restrictionToString(ModelConcepts concepts, OntClass restrictedClass, Restriction res) throws OwlImportException {
		if (res.isURIResource()) {
			return uriToSadlString(concepts, res);
		}
		StringBuilder sb = new StringBuilder();
		OntProperty onprop = null;
		try {
			onprop = res.getOnProperty();
			StmtIterator resDmnItr = theModel.listStatements(onprop, RDFS.domain, (RDFNode)null);
			if (resDmnItr.hasNext()) {
				// this restriction is in the domain of the restricted property; this is an OWL2 construct
				if (res.isSomeValuesFromRestriction()) {
					Resource onClass = res.asSomeValuesFromRestriction().getSomeValuesFrom();
					if (onClass.canAs(OntClass.class)) {
						StmtIterator rngItr = theModel.listStatements(onprop, RDFS.range, (RDFNode)null);
						if (rngItr.hasNext()) {
							RDFNode rng = rngItr.nextStatement().getObject();
							if (rng.isResource() && rng.asResource().canAs(Restriction.class)) {
								Restriction rngres = rng.asResource().as(Restriction.class);
								String str = restrictionToString(concepts, onClass.as(OntClass.class), rngres);
								sb.append(str);
								return sb.toString();
							}
						}
					}
				}
			}
		}
		catch (ConversionException e) {
			onprop = getOnPropertyFromConversionException(e);
			if (onprop == null) {
				sb.append("// Error: " + e.getMessage());
				return sb.toString();
			}
		}
		sb.append(uriToSadlString(concepts,onprop));
		if (restrictedClass == null) {
			OntClass supcls = res.getSuperClass();
			if (supcls != null) {
				restrictedClass = supcls;
			}
		}
		if (restrictedClass != null) {
			sb.append(" of ");
			sb.append(uriToSadlString(concepts, restrictedClass));
		}
		if (res.isHasValueRestriction()) {
			sb.append(" always has value ");
			sb.append(rdfNodeToSadlString(concepts, res.asHasValueRestriction().getHasValue(), false));
		}
		else if (res.isAllValuesFromRestriction()) {
			sb.append(" only has values of type ");
			sb.append(uriToSadlString(concepts, res.asAllValuesFromRestriction().getAllValuesFrom()));
		}
		else if (res.isSomeValuesFromRestriction()) {
			sb.append(" has at least one value of type ");
			sb.append(uriToSadlString(concepts, res.asSomeValuesFromRestriction().getSomeValuesFrom()));
		}
		else if (res.isCardinalityRestriction()) {
			sb.append(" has exactly ");
			int card = res.asCardinalityRestriction().getCardinality();
			sb.append(card);
			if (card == 1) {
				sb.append(" value");
			}
			else {
				sb.append(" values");
			}
			RDFNode onCls = res.getPropertyValue(OWL2.onClass);
			if (onCls != null) {
				sb.append(" of type ");
				sb.append(rdfNodeToSadlString(concepts, onCls, false));
			}
		}
		else if (res.isMinCardinalityRestriction()) {
			sb.append(" has at least ");
			int card = res.asMinCardinalityRestriction().getMinCardinality();
			sb.append(card);
			if (card == 1) {
				sb.append(" value");
			}
			else {
				sb.append(" values");
			}
			RDFNode onCls = res.getPropertyValue(OWL2.onClass);
			if (onCls != null) {
				sb.append(" of type ");
				sb.append(rdfNodeToSadlString(concepts, onCls, false));
			}
		}
		else if (res.isMaxCardinalityRestriction()) {
			sb.append(" has at most ");
			int card = res.asMaxCardinalityRestriction().getMaxCardinality();
			sb.append(card);
			if (card == 1) {
				sb.append(" value");
			}
			else {
				sb.append(" values");
			}
			RDFNode onCls = res.getPropertyValue(OWL2.onClass);
			if (onCls != null) {
				sb.append(" of type ");
				sb.append(rdfNodeToSadlString(concepts, onCls, false));
			}
		}
		else if (res.hasProperty(OWL2.maxQualifiedCardinality)) {
			RDFNode op = res.getPropertyValue(OWL.onProperty);
			RDFNode oc = res.getPropertyValue(OWL2.onClass);
			if (op.isResource() && op.equals(onprop)) {
				sb.append(" has at most ");
				RDFNode mcv = res.getPropertyValue(OWL2.maxQualifiedCardinality);
				if (mcv.isLiteral()) {
					int mcvInt = mcv.asLiteral().getInt();
					sb.append(mcvInt);
					if (mcvInt > 1) {
						sb.append(" values of type ");
					}
					else {
						sb.append(" value of type ");
					}
					sb.append(resourceToString(oc.asResource(), false));
				}
			}
		}
		else if (res.hasProperty(OWL2.qualifiedCardinality)) {
			RDFNode op = res.getPropertyValue(OWL.onProperty);
			RDFNode oc = res.getPropertyValue(OWL2.onClass);
			if (op.isResource() && op.equals(onprop)) {
				sb.append(" has exactly ");
				RDFNode cv = res.getPropertyValue(OWL2.qualifiedCardinality);
				if (cv.isLiteral()) {
					int cvInt = cv.asLiteral().getInt();
					sb.append(cvInt);
					if (cvInt > 1) {
						sb.append(" values of type ");
					}
					else {
						sb.append(" value of type ");
					}
					sb.append(resourceToString(oc.asResource(), false));
				}
			}
		}
		else if (res.hasProperty(OWL2.minQualifiedCardinality)) {
			RDFNode op = res.getPropertyValue(OWL.onProperty);
			RDFNode oc = res.getPropertyValue(OWL2.onClass);
			if (op.isResource() && op.equals(onprop)) {
				sb.append(" has at least ");
				RDFNode mcv = res.getPropertyValue(OWL2.minQualifiedCardinality);
				if (mcv.isLiteral()) {
					int mcvInt = mcv.asLiteral().getInt();
					sb.append(mcvInt);
					if (mcvInt > 1) {
						sb.append(" values of type ");
					}
					else {
						sb.append(" value of type ");
					}
					sb.append(resourceToString(oc.asResource(), false));
				}
			}
		}
		return sb.toString();
	}

	private OntProperty getOnPropertyFromConversionException(ConversionException e) {
		String msg = e.getMessage();
		int start = msg.indexOf("Cannot convert node ") + 20;
		int end = start;
		if (start > 0 && end > start) {
			while (!Character.isWhitespace(msg.charAt(++end))) {}
			String uri = msg.substring(start, end);
			return theModel.createOntProperty(uri);
		}
		return null;
	}

	private boolean propertyToSadl(StringBuilder sb, ModelConcepts concepts,
			OntResource prop) throws OwlImportException {
		boolean retVal = true;
		String key = null;
		OntProperty ontprop = prop.asProperty();
		if (ontprop.isURIResource()) {
			key = ontprop.getURI();
		}
		ExtendedIterator<? extends OntResource> deitr = ontprop.listDomain();
		OntProperty sprprop = null;
		try {
			sprprop = ontprop.getSuperProperty();
		}
		catch(Exception e) {
			concepts.addErrorMessage(e.getMessage());
		}
		if (sprprop != null) {
			sb.append(" is a type of ");
			sb.append(uriToSadlString(concepts, sprprop));
		}
		if (deitr.hasNext()) {
			sb.append(" describes ");
			boolean unionDomain = false;
			int domainctr = 0;
			while (deitr.hasNext()) {
				OntResource dr = deitr.next();
				if (deitr.hasNext() && domainctr == 0) {
					unionDomain = true;
					sb.append("{");
				}
				if (domainctr++ > 0) {
					sb.append(" or ");
				}
				sb.append(uriToSadlString(concepts, dr));
				if (key != null && dr.isURIResource()) {
					key += dr.getURI();
				}
			}
			if (unionDomain) {
				sb.append("}");
			}
		}
		else if (sprprop == null) {
			sb.append(" is a property");				
		}
		key = generateSadlRangeStatement(concepts, ontprop, sb, key);
		if (key != null) {
			if (propertyAlreadyProcessed(key)) {
				retVal = false;
			}
		}
		return retVal;
	}

	private String generateSadlRangeStatement(ModelConcepts concepts, Property prop, StringBuilder sb,
			String key) throws OwlImportException {
//		ExtendedIterator<? extends OntResource> reitr = ontprop.listRange();
		StmtIterator reitr = theModel.listStatements(prop, RDFS.range, (RDFNode)null);
		List<Resource> rngList = new ArrayList<Resource>();
		while (reitr.hasNext()) {
			RDFNode rr = reitr.next().getObject();
			if (rr.isResource()) {
				rngList.add(rr.asResource());
			}
		}
		int rngcnt = rngList.size();
		if (rngcnt > 0) {
			sb.append(" with values of type ");
			if (rngcnt == 1) {
				sb.append(uriToSadlString(concepts, rngList.get(0)));
				if (key != null && rngList.get(0).isURIResource()) {
					key += rngList.get(0).getURI();
				}
			}
			else {
				sb.append("{");
				for (int i = 0; i < rngcnt; i++) {
					if (i > 0) {
						sb.append(" or ");
					}
					sb.append(uriToSadlString(concepts, rngList.get(i)));
					if (key != null && rngList.get(i).isURIResource()) {
						key += rngList.get(i).getURI();
					}
				}
				sb.append("}");
			}
		}
		return key;
	}

	/**
	 * Method to determine if the property (key is property + domain + range) has already been processed
	 * @param key
	 * @return
	 */
	private boolean propertyAlreadyProcessed(String key) {
		if (propertiesProcessed.contains(key)) {
			return true;
		}
		propertiesProcessed.add(key);
		return false;
	}

	private String rdfNodeToSadlString(ModelConcepts concepts, RDFNode object, boolean forceQuotes) throws OwlImportException {
		if (object.isURIResource()) {
			String objStr = uriToSadlString(concepts, object.asResource());
			return makeStringDoubleQuoted(objStr);
		}
		else if (object.isLiteral()) {
			String dturi = object.asLiteral().getDatatypeURI();
			forceQuotes = forceQuotes ? true : (dturi != null ? isRDFDatatypeString(dturi, object) : true);
			if (object.asLiteral().getDatatypeURI() == null) {
				String lf = object.asLiteral().getLexicalForm();
				if (forceQuotes || lf.contains(" ") || lf.contains("\"")) {
					String s = object.asLiteral().getLexicalForm();
					return makeStringDoubleQuoted(s); 
				}
				return object.asLiteral().getLexicalForm();
			}
			if (forceQuotes || object.asLiteral().getDatatypeURI().equals(XSD.xstring.getURI())) {
				String s = object.asLiteral().getLexicalForm();
				if (s.startsWith("\"") && s.endsWith("\"")) {
					s = s.substring(1, s.length() - 2);
				}
				return makeStringDoubleQuoted(s); 
			}
			else {
				return object.asLiteral().getLexicalForm();
			}
		}
		else if (object.canAs(Individual.class)){
			// a bnode
			if (concepts.getInstances().contains(object)) {
				concepts.getInstances().remove(object);
			}
			// is it a list?
			if (object.as(Individual.class).getProperty(RDF.first) != null) {
				return listToSadl(concepts, object.as(Individual.class), true);
			}
			else if (isaSadlList(object.as(Individual.class))) { 
				return sadlListToSadl(concepts, object.as(Individual.class), true);
			}
			return individualToSadl(concepts, object.as(Individual.class), true);
		}
		else {
			return object.toString();
		}
	}

	private String makeStringDoubleQuoted(String s) {
		s = s.replace("\"", "\\\"");
		return "\"" + s + "\"";
	}

	private String uriToSadlString(ModelConcepts concepts, Resource rsrc) throws OwlImportException {
		if (rsrc.isURIResource()) {
			if (rsrc.getNameSpace().equals(getBaseUri() + "#") || isNeverUsePrefixes()) {
				String ln = rsrc.getLocalName();
				if (allTokens.contains(ln)) {
					if (rsrc.getNameSpace().equals(XSD.getURI())) {
						// don't escape a localname if it is a keyword in the XSD types.
						return ln;
					}
				}
				return checkLocalnameForKeyword(ln);
			}
			else {
				if (rsrc.getNameSpace().equals(XSD.getURI())) {
					return rsrc.getLocalName();
				}
				String ns = rsrc.getNameSpace();
				String trimmedNs = ns.endsWith("#") ? ns.substring(0, ns.length() - 1) : null;
				if (qNamePrefixes.containsKey(ns)) {
					String prefix = qNamePrefixes.get(ns).trim();
					if (prefix.length() > 0) {
						return prefix + ":" + checkLocalnameForKeyword(rsrc.getLocalName());
					}
					else {
						return checkLocalnameForKeyword(rsrc.getLocalName());
					}
				}
				else {
					if (trimmedNs != null) {
						if (qNamePrefixes.containsKey(trimmedNs)) {
							String prefix = qNamePrefixes.get(trimmedNs).trim();
							if (prefix.length() > 0 && isAmbiguousName(rsrc)) {
								return prefix + ":" + checkLocalnameForKeyword(rsrc.getLocalName());
							}
							else {
								return checkLocalnameForKeyword(rsrc.getLocalName());
							}
						}
					}
//					return rsrc.getLocalName();	// returning the URI may draw attention to the error but....
					return rsrc.getURI();
				}
			}
		}
		else {
			if (concepts.getMappedRestrictions().containsValue(rsrc)) {
				return null;
			}
			else if (rsrc.canAs(Restriction.class)) {
				return restrictionToString(concepts, null, rsrc.as(Restriction.class));
			}
			else if (rsrc instanceof OntClass) {
//				OntClass eqcls = ((OntClass)rsrc).getEquivalentClass();
//				if (eqcls != null) {
//					return uriToSadlString(concepts, eqcls);
//				}
				if (((OntClass)rsrc).isEnumeratedClass()) {
					EnumeratedClass enumcls = ((OntClass)rsrc).asEnumeratedClass();
					return enumeratedClassToString(enumcls);
//					if (enumcls != null) {
//						ExtendedIterator<? extends OntResource> eitr = enumcls.listInstances();
//						while (eitr.hasNext()) {
//							OntResource en = eitr.next();
//							en.toString();
//						}
//						ExtendedIterator<RDFNode> eitr2 = enumcls.listIsDefinedBy();
//						while (eitr2.hasNext()) {
//							RDFNode en = eitr2.next();
//							en.toString();
//						}
//						RDFList oneoflst = enumcls.getOneOf();
//						List<RDFNode> nodeLst = oneoflst.asJavaList();
//						if (nodeLst != null && nodeLst.size() > 0) {
//							StringBuilder sb = new StringBuilder();
//							sb.append("one of {");
//							int cntr = 0;
//							for (int i = 0; i < nodeLst.size(); i++) {
//								RDFNode n = nodeLst.get(i);
//								if (cntr > 0) sb.append(", ");
//								sb.append("\n    ");
//								if (n.canAs(Individual.class)&& concepts.getInstances().contains(n)) {
//									sb.append(individualNameAndAnnotations(concepts, n.as(Individual.class)));
//									concepts.getInstances().remove(n);
//								}
//								else {
//									sb.append(rdfNodeToSadlString(concepts, n, false));
//								}
//								cntr++;
//							}
//							sb.append("}");
//							return sb.toString();
//						}
//					}
				}
				else {
					return ontClassToString((OntClass)rsrc, null);
//					System.err.println("Blank node OntClass is not of handled type: " + rsrc.getClass().getCanonicalName());
				}
			}
		}
		return rsrc.toString();
	}

	private boolean isAmbiguousName(Resource rsrc) {
		int cntr = 0;
		ExtendedIterator<OntModel> smitr = theModel.listSubModels(true);
		while (smitr.hasNext()) {
			if (smitr.next().containsResource(rsrc)) {
				cntr++;
			}
		}
		return (cntr > 1);
	}

	private String checkLocalnameForKeyword(String localName) {
		String ln = localName;
		if (allTokens.contains(ln)) {
			return "^" + ln;
		}
		return localName;
	}

	private boolean addResourceToList(ModelConcepts concepts, OntResource ontRsrc) {
		Resource type = ontRsrc.getRDFType();
		if (type.equals(OWL.Class)) {
			if (ontRsrc.isAnon() && ontRsrc.canAs(OntClass.class)) {
				concepts.addAnonClass(ontRsrc.as(OntClass.class));
				return true;
			}
			else {
				if (!ignoreNamespace(ontRsrc, true)) {
					concepts.addClass(ontRsrc.as(OntClass.class));
					return true;
				}
			}
		}
		else if (type.equals(OWL.ObjectProperty)) {
			if (isPropertyInThisNamespace(ontRsrc.asObjectProperty())) {
				concepts.addObjProperty(ontRsrc.asObjectProperty());
			}
			return true;
		}
		else if (type.equals(OWL.DatatypeProperty)) {
			if (isPropertyInThisNamespace(ontRsrc.asDatatypeProperty())) {
				concepts.addDtProperty(ontRsrc.asDatatypeProperty());
			}
			return true;
		}
		else if (type.equals(RDF.Property)) {
			// does anything a subproperty of this property?
//			Resource superPropType = getSuperPropertyType(ontRsrc);
//			if (superPropType != null) {
//				if (superPropType.equals(OWL.DatatypeProperty)) {
//					concepts.addPseudoDtProperty(ontRsrc);
//					return true;
//				}
//				else {
//					concepts.addPseudoObjProperty(ontRsrc);
//					return true;
//				}
//			}
			if (isPropertyInThisNamespace(ontRsrc.asProperty())) {
				concepts.addRdfProperty(ontRsrc.asProperty());
				return true;
			}
		}
		else if (type.equals(OWL.AnnotationProperty)) {
			if (isPropertyInThisNamespace(ontRsrc.asAnnotationProperty())) {
				concepts.addAnnProperty(ontRsrc.asAnnotationProperty());
			}
			return true;
		}
		else if (type.equals(OWL.Ontology)) {
			if (!isImplicitUri(ontRsrc.getURI())) {
				concepts.addOntology(ontRsrc.asOntology());
				return true;
			}
			else {
				return false;
			}
		}
		else if (type.equals(RDFS.Datatype)) {
			if (ontRsrc.isAnon()) {
				// This should be the equivalent class so ignore this one
				RDFNode eqcls = ontRsrc.getPropertyValue(OWL.equivalentClass);
				if (eqcls == null) {
					StmtIterator itr = theModel.listStatements(null, OWL.equivalentClass, ontRsrc);
					if (itr.hasNext()) {
						eqcls = itr.nextStatement().getSubject();
					}
				}
				if (eqcls != null) {
					logger.debug("Ignoring RDFDatatype blank node which is equivalent to '" + eqcls.toString() + "'");
				}
				return false;
			}
			logger.debug("Examining rdfs:Datatype: " + ontRsrc.toString());
			ExtendedIterator<Resource> eitr = ontRsrc.listRDFTypes(true);
			while (eitr.hasNext()) {
				logger.debug("    " + eitr.next().toString());
			}
			concepts.addDatatype(ontRsrc);
			return false;
		}
		else if (type.equals(OWL.Restriction)) {
			try {
				if (ontRsrc.canAs(Restriction.class)) {
					Restriction restr = ontRsrc.as(Restriction.class);
					StmtIterator stmtitr = theModel.listStatements(restr, OWL.onProperty, (RDFNode)null);
					if (stmtitr.hasNext()) {
						RDFNode objnode = stmtitr.nextStatement().getObject();
						if (objnode.isResource() && objnode.asResource().equals(RDFS.Resource)) {
							return false;
						}
					}
				}
			}
			catch (Throwable t) {
				concepts.addErrorMessage(t.getMessage());
			}
			if (ontRsrc.canAs(HasValueRestriction.class)) {
				HasValueRestriction res = ontRsrc.as(HasValueRestriction.class);
				OntClass supcls = res.getSuperClass();
				if (supcls != null) {
					concepts.addMappedRestriction(supcls, res);
					return true;
				}
				else {
					try {
						OntClass subcls = res.getSubClass();
						if (subcls != null) {
							concepts.addMappedRestriction(subcls, res);
							return true;
						}
					}
					catch (Throwable t) {
						// this one is not necessarily a real issue--don't report?
					}
				}
				concepts.addUnMappedRestriction(res);
				return true;
			}
			else if (ontRsrc.canAs(SomeValuesFromRestriction.class)) {
				SomeValuesFromRestriction res = ontRsrc.as(SomeValuesFromRestriction.class);
				OntClass supcls = res.getSuperClass();
				if (supcls != null) {
					concepts.addMappedRestriction(supcls, res);
					return true;
				}
				else {
					try {
						OntClass subcls = res.getSubClass();
						if (subcls != null) {
							concepts.addMappedRestriction(subcls, res);
							return true;
						}
					}
					catch (Throwable t) {
						// not necessarily a real error?
					}
				}
				concepts.addUnMappedRestriction(res);
				return true;
			}
			else if (ontRsrc.canAs(AllValuesFromRestriction.class)) {
				AllValuesFromRestriction res = ontRsrc.as(AllValuesFromRestriction.class);
				OntClass supcls = res.getSuperClass();
				if (supcls != null) {
					concepts.addMappedRestriction(supcls, res);
					return true;
				}
				else {
					try {
						OntClass subcls = res.getSubClass();
						if (subcls != null) {
							concepts.addMappedRestriction(subcls, res);
							return true;
						}
					}
					catch (Throwable t) {
						// not necessarily a real error?
					}
				}
				concepts.addUnMappedRestriction(res);
				return true;
			}
			else if (ontRsrc.canAs(CardinalityRestriction.class)) {
				CardinalityRestriction res = ontRsrc.as(CardinalityRestriction.class);
				OntClass supcls = res.getSuperClass();
				if (supcls != null) {
					concepts.addMappedRestriction(supcls, res);
					return true;
				}
				else {
					try {
						OntClass subcls = res.getSubClass();
						if (subcls != null) {
							concepts.addMappedRestriction(subcls, res);
							return true;
						}
					}
					catch (Throwable t) {
						// not necessarily a real error?
					}
				}
				concepts.addUnMappedRestriction(res);
				return true;
			}
			else if (ontRsrc.canAs(MinCardinalityRestriction.class)) {
				MinCardinalityRestriction res = ontRsrc.as(MinCardinalityRestriction.class);
				OntClass supcls = res.getSuperClass();
				if (supcls != null) {
					concepts.addMappedRestriction(supcls, res);
					return true;
				}
				else {
					try {
						OntClass subcls = res.getSubClass();
						if (subcls != null) {
							concepts.addMappedRestriction(subcls, res);
							return true;
						}
					}
					catch (Throwable t) {
						// not necessarily a real error?
					}
				}
				concepts.addUnMappedRestriction(res);
				return true;
			}
			else if (ontRsrc.canAs(MaxCardinalityRestriction.class)) {
				MaxCardinalityRestriction res = ontRsrc.as(MaxCardinalityRestriction.class);
				OntClass supcls = res.getSuperClass();
				if (supcls != null) {
					concepts.addMappedRestriction(supcls, res);
					return true;
				}
				else {
					try {
						OntClass subcls = res.getSubClass();
						if (subcls != null) {
							concepts.addMappedRestriction(subcls, res);
							return true;
						}
					}
					catch (Throwable t) {
						// not necessarily a real error?
					}
				}
				concepts.addUnMappedRestriction(res);
				return true;
			}
			return false;	// for now
		}
		else if (ontRsrc.canAs(Individual.class)) {
			Individual inst = ontRsrc.asIndividual();
			// only named instances can stand alone
			if (inst.isURIResource() && !ignoreNamespace(ontRsrc, true)) {
				concepts.addInstance(inst);
				return true;
			}
		}
		return false;
	}

	private boolean isPropertyInThisNamespace(Property mprop) {
		String ns = mprop.getNameSpace();
		if (ns.endsWith("#")) {
			ns = ns.substring(0, ns.length() - 1);
		}
		if (mprop.isURIResource() && ns.equals(getBaseUri())) {
			return true;
		}
		return false;
	}

	private Resource getSuperPropertyType(Resource ontRsrc) {
		StmtIterator sitr = theModel.listStatements(null, RDFS.subPropertyOf, ontRsrc);
		if (sitr.hasNext()) {
			Statement s = sitr.nextStatement();
			Resource superprop = s.getSubject();
			if (superprop.canAs(ObjectProperty.class)) {
				return OWL.ObjectProperty;
			}
			else if (superprop.canAs(DatatypeProperty.class)) {
				return OWL.DatatypeProperty;
			}
			else {
				return getSuperPropertyType(superprop);
			}
		}
		return null;
	}

	private boolean isNewLineAtEndOfBuffer(StringBuilder sb) {
		int len = sb.length();
		if (len < 1) return false;
		char ch = sb.charAt(--len);
		boolean done = false;
		while (!done) {
			if (ch == '\n') {
				return true;
			}
			else if (!Character.isWhitespace(ch)) {
				done = true;
			}
			else if (--len <= 0) {
				done = true;
			}
		}		
		return false;
	}

	private void addNewLineIfNotAtEndOfBuffer(StringBuilder sb) {
		if (!isNewLineAtEndOfBuffer(sb)) {
			sb.append(System.lineSeparator());
		}
		
	}

	private List<OntClass> namedClassesAlphabeticalOrder() {
		ExtendedIterator<OntClass> citr = theBaseModel.listNamedClasses();
		if (citr.hasNext()) {
			List<OntClass> lst = new ArrayList<OntClass>();
			while (citr.hasNext()) {
				OntClass cls = citr.next();
//				OntClass equivCls = cls.getEquivalentClass();
//				if (equivCls != null) {
//					ExtendedIterator<OntClass> eitr = cls.listEquivalentClasses();
//					while (eitr.hasNext()) {
//						equivCls = eitr.next();
//						logger.debug("Equivalent class to '" + ontClassToString(cls, null) + "': "  + ontClassToString(equivCls, null));
//					}
//				}
				if (shouldResourceBeOutput(cls, true, false, false)) {
					if (lst.size() > 0) {
						for (int i = 0; i < lst.size(); i++) {
//							String lstUri = lst.get(i).getURI();
//							String clsUri = cls.getURI();
							if (lst.get(i).getURI().compareTo(cls.getURI()) > 0) {
								// if this [ith] item in the list is after cls, insert cls here before it
								lst.add(i, cls);
								cls = null;
								break;
							}
						}
					}
					if (cls != null) {
						lst.add(cls);
					}
				}
			}
			return lst;
		}
		return null;
	}

	private List<OntProperty> namedPropertiesAlphabeticalOrder(boolean includeEvenIfProcessed) {
		ExtendedIterator<OntProperty> pitr = theBaseModel.listAllOntProperties();
		if (pitr.hasNext()) {
			List<OntProperty> lst = new ArrayList<OntProperty>();
			while (pitr.hasNext()) {
				OntProperty prop = pitr.next();
				if (shouldResourceBeOutput(prop, true, includeEvenIfProcessed, false)) {
					if (lst.size() > 0) {
						for (int i = 0; i < lst.size(); i++) {
							if (lst.get(i).getURI().compareTo(prop.getURI()) > 0) {
								lst.add(i, prop);
								prop = null;
								break;
							}
						}
					}
					if (prop != null) {
						lst.add(prop);
					}
				}
			}
			return lst;
		}
		return null;
	}

	private List<Individual> individualsAlphabeticalOrder() {
		ExtendedIterator<Individual> institr = theBaseModel.listIndividuals();
		List<Individual> lst = new ArrayList<Individual>();
		if (institr.hasNext()) {
			while (institr.hasNext()) {
				Individual inst = institr.next();
				if (shouldResourceBeOutput(inst, true, false, true)) {
					if (lst.size() > 0 && !inst.isAnon()) {
						for (int i = 0; i < lst.size(); i++) {
							Individual lstinst = lst.get(i);
							if (lstinst.isAnon() || lstinst.getURI().compareTo(inst.getURI()) > 0) {
								lst.add(i, inst);
								inst = null;
								break;
							}
						}
					}
					if (inst != null) {
						lst.add(inst);
					}
				}
			}
		}
		ResIterator subjItr = theBaseModel.listSubjects();
		while (subjItr.hasNext()) {
			Resource r = subjItr.nextResource();
			if (r.canAs(Individual.class) && r.getURI() != null && r.getURI().indexOf('#') > 0 && shouldResourceBeOutput(r.as(Individual.class), true, false, false)) {
				if (!lst.contains(r)) {
					boolean added = false;
					if (lst.isEmpty()) {
						lst.add(r.as(Individual.class));
						added = true;
					}
					else {
						for (int i = 0; i < lst.size(); i++) {
							Individual lstinst = lst.get(i);
							if (lstinst.isAnon() || lstinst.getURI().compareTo(r.getURI()) > 0) {
								lst.add(r.as(Individual.class));
								added = true;
								break;
							}
						}
					}
					if (!added) {
						lst.add(r.as(Individual.class));
					}
				}
			}
		}
		return lst.size() > 0 ? lst : null;
	}

	private String addClasses(List<OntClass> clist, OntClass subclass) {
		int cnt = 0;
		String clsses = "";
		for (int i = 0; clist != null && i < clist.size(); i++) {
			OntClass cls = clist.get(i);
			if (cls.isAnon() || !ignoreNamespace(cls, false)) {
				String addlName = ontClassToString(cls, subclass);
				if (addlName != null) {
					if (cnt > 0) {
						clsses += " and ";
					}
					clsses += addlName;
					cnt++;
				}
			}
			List<OntClass> sclst = getSuperclasses(cls);
			String superClsses = null;
			if (sclst != null) {
				superClsses = addClasses(sclst, cls);
			}
			if (shouldResourceBeOutput(cls, true, false, false)) {
				// this is a top-level class
				resourcesOutput.add(cls);
				sadlModel.append(resourceToString(cls, true));
				if (superClsses == null) {
					sadlModel.append(" is a class");
				}
				else {
					sadlModel.append(" is a type of ");
					sadlModel.append(superClsses);
				}

				ExtendedIterator<OntClass> eitr = cls.listEquivalentClasses();
				if (eitr.hasNext()) {
					while (eitr.hasNext()) {
						OntClass ec = eitr.next();
						if (!ec.equals(cls)) {			// don't do equivalence to self (which may be inferred)
							if (!cls.isAnon() && !ec.isAnon()) {
								sadlModel.append(resourceToString(cls, false));
								sadlModel.append(" is the same as ");
								sadlModel.append(resourceToString(ec, false));
								addEndOfStatement(sadlModel, 1);
							}
							else {
								sadlModel.append(", ");
								sadlModel.append(ontClassToString(ec, cls));
							}
						}
					}
					
				}
				
				try {
					ExtendedIterator<OntProperty> pitr = cls.listDeclaredProperties(true);
					while (pitr.hasNext()) {
						OntProperty prop = pitr.next();
						if (shouldResourceBeOutput(prop, true, false, false)) {
							sadlModel.append(",\n    described by ");
							sadlModel.append(resourceToString(prop, false));
							String rngString = rangeToString(prop);
							if (rngString != null && rngString.length() > 0) {
								if (isSingleValued(cls, prop, rngString)) {    //if (prop.isFunctionalProperty()) {
									sadlModel.append(" with a single value of type ");
								}
								else {
									sadlModel.append(" with values of type ");
								}
								sadlModel.append(rngString);
	//							resourcesOutput.add(prop);	// if we get to here consider the property defined. No, it can have multiple classes in domain.
							}
						}
					}
				}
				catch (Throwable t) {
					logger.debug("Unexpected error processing class '" + cls.toString() + "': " + t.getMessage());
				}
				
				addEndOfStatement(sadlModel, 1);
				
				addRestrictionsToOutput(cls, true);
				
				ExtendedIterator<OntClass> ditr = cls.listDisjointWith();
				if (ditr.hasNext()) {
					OntClass dcls = ditr.next();
					sadlModel.append(resourceToString(cls, false));
					sadlModel.append(" is disjoint with ");
					sadlModel.append(resourceToString(dcls, false));
					addEndOfStatement(sadlModel, 1);
				}
			}
		}
		if (cnt > 1) {
			clsses = "{" + clsses + "}";
		}
		return clsses.length() > 0 ? clsses : null;
	}

	private List<OntClass> getSuperclasses(OntClass cls) {
		try {
			ExtendedIterator<OntClass> eitr = cls.listSuperClasses(true);
			if (eitr.hasNext()) {
				List<OntClass> lst = new ArrayList<OntClass>();
				while (eitr.hasNext()) {
					lst.add(eitr.next());
				}
				return lst;
			}
		}
		catch (Throwable t) {
			logger.debug("Unexpected erro getting super classes of '" + cls.toString() + "': " + t.getLocalizedMessage());
		}
		return null;
	}

	private void addProperties(List<OntProperty> plst, boolean outputEvenIfDuplicate) throws IOException {
		for (int i = 0; plst != null && i < plst.size(); i++) {
			OntProperty prop = plst.get(i);
			if (shouldResourceBeOutput(prop, true, outputEvenIfDuplicate, false)) {
				resourcesOutput.add(prop);
				if (prop.isObjectProperty() && !prop.isFunctionalProperty() && !prop.isInverseFunctionalProperty()) {
					sadlModel.append("relationship of ");
					sadlModel.append(domainToString(prop));
					sadlModel.append(" to ");
					sadlModel.append(rangeToString(prop));
					sadlModel.append(" is ");
					sadlModel.append(resourceToString(prop, false));
					addEndOfStatement(sadlModel, 1);
					if (prop.isFunctionalProperty()) {
						sadlModel.append("\t");
						sadlModel.append(resourceToString(prop, false));
						sadlModel.append(" has a single value.\n");
					}
				}
				else {
					sadlModel.append(resourceToString(prop, false));
					sadlModel.append(" describes ");
					sadlModel.append(domainToString(prop));
					if (prop.isFunctionalProperty()) {
						sadlModel.append(" with a single value of type ");
					}
					else {
						sadlModel.append(" with values of type ");
					}
					sadlModel.append(rangeToString(prop));
					sadlModel.append(".\n");
				}
			}
		}
	}

	private void addIndividuals(List<Individual> instlst) throws Exception {
		Individual inst;
		for (int i = 0; instlst != null && i < instlst.size(); i++) {
			inst = instlst.get(i);
			if (shouldResourceBeOutput(inst, true, false, true)) {
				ExtendedIterator<OntClass> typeitr = inst.listOntClasses(true);
				if (typeitr.hasNext()) {
					List<OntClass> types = new ArrayList<OntClass>();
					while (typeitr.hasNext()) {
						try {
							OntClass type = typeitr.next();
							types.add(type);
						}
						catch (Exception e) {
							ExtendedIterator<Resource> eitr = inst.listRDFTypes(true);
							while (eitr.hasNext()) {
								Resource rtype = eitr.next();
								if (!rtype.isAnon()) {
									OntClass newClass = theBaseModel.createClass(rtype.getURI());
									types.add(newClass);
								}
							}
						}
					}
					String typeString = classListToString(types, null);
					if (inst.isAnon()) {
						sadlModel.append("A ");
						sadlModel.append(typeString);
					}
					else {
						sadlModel.append(resourceToString(inst, false));
						sadlModel.append(" is a ");
						sadlModel.append(typeString);
					}
					String instProps = instancePropertiesToString(inst, false, 1);
					if (instProps != null) {
						sadlModel.append(instProps);
					}
					addEndOfStatement(sadlModel, 1);
					
					resourcesOutput.add(inst);
				}
				else {
					throw new Exception("The instance '" + inst.toString() + "' does not have a specified type (class); SADL cannot import it.");
				}
			}
		}
		
	}
	
	private String instancePropertiesToString(Individual inst, boolean useWith, int indentLevel) {
		List<Statement> orderedStatements = getInstancePropertiesOrdered(inst);
//		StmtIterator sitr = inst.listProperties();
		Iterator<Statement> sitr = orderedStatements.iterator();
		if (sitr.hasNext()) {
			List<Statement> stmts = new ArrayList<Statement>();
			while (sitr.hasNext()) {
				stmts.add(sitr.next());
			}
			stmts = mostPreciseStatementsOnly(stmts);
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < stmts.size(); i++) {							
				Statement s = stmts.get(i);
				if (!ignoreNamespace(s.getSubject(), false) &&
						!ignoreNamespace(s.getPredicate(), false)) {
					sb.append(",\n");
					for (int j = 0; j < indentLevel; j++) {
						sb.append("\t");
					}
					sb.append((useWith ? "with " : "has ") + resourceToString(s.getPredicate(), false));
					sb.append(" ");
					sb.append(rdfNodeToString(s.getObject(), indentLevel));
				}
			}
			return sb.toString();
		}
		return null;
	}
	
	private List<Statement> getInstancePropertiesOrdered(Individual inst) {
		List<Statement> results = new ArrayList<Statement>();
		if (!inst.isAnon()) {
			String query = "select ?p ?v where {<" + inst.getURI() + "> ?p ?v} order by ?p ?v";
			logger.debug("Owl2Sadl query: " + query);
			QueryExecution qexec = null;	
			qexec = QueryExecutionFactory.create(QueryFactory.create(query, Syntax.syntaxARQ), theModel);
			ResultSet rs = qexec.execSelect();
			while (rs.hasNext()) {
				QuerySolution soln = rs.next();
				Resource p = soln.getResource("p");
				if (p.canAs(Property.class)) {
					RDFNode v = soln.get("v");
					StmtIterator sitr = theBaseModel.listStatements(inst, p.as(Property.class), v);
					Statement s = sitr.next();
					if (s != null) {
						results.add(s);
					}
				}
			}
		}
		else {
			StmtIterator sitr = theBaseModel.listStatements(inst, (Property)null, (RDFNode)null);
			while (sitr.hasNext()) {
				results.add(sitr.nextStatement());
			}
		}
		return results;
	}

	private List<Statement> mostPreciseStatementsOnly(List<Statement> stmts) {
		List<Statement> removals = null;
		for (int i = 0; i < stmts.size(); i++) {
			Statement si = stmts.get(i);
			for (int j = 0; j < stmts.size(); j++) {
				if (i != j) {
					Statement sj = stmts.get(j);
					if (si.getObject().equals(sj.getObject())) {
						Property pi = si.getPredicate();
						Property pj = sj.getPredicate();
						if (!pi.equals(pj) && pi.canAs(OntProperty.class) && pj.canAs(OntProperty.class)){
							if (pi.as(OntProperty.class).hasSuperProperty(pj, false)) {
								if (removals == null) removals = new ArrayList<Statement>();
								removals.add(sj);
							}
						}
					}
				}
			}
		}
		if (removals != null) {
			for (int i = 0; i < removals.size(); i++) {
				stmts.remove(removals.get(i));
			}
		}
		return stmts;
	}

	private String getIntersectionClassRangeString(IntersectionClass intersectionClass) {
		String rslt = "";
		int cnt = 0;
		RDFList iclsses = intersectionClass.getOperands();
		if (iclsses != null) {
			ExtendedIterator<RDFNode> eitr = iclsses.iterator();
			while (eitr.hasNext()) {
				RDFNode node = eitr.next();
				if (cnt > 0) rslt += " and ";
				if (node.canAs(OntClass.class)) {
					rslt += ontClassToString(node.as(OntClass.class), null);
				}
				cnt++;
			}
			if (cnt > 1) {
				rslt = "{" + rslt + "}";
			}
		}
		return rslt;
	}

	private String getUnionClassRangeString(UnionClass unionClass) {
		String rslt = "";
		int cnt = 0;
		RDFList uclsses = unionClass.getOperands();
		if (uclsses != null) {
			ExtendedIterator<RDFNode> eitr = uclsses.iterator();
			while (eitr.hasNext()) {
				RDFNode node = eitr.next();
				if (cnt > 0) rslt += " or ";
				if (node.canAs(OntClass.class)) {
					rslt += ontClassToString(node.as(OntClass.class), null);
				}
				cnt++;
			}
			if (cnt > 1) {
				rslt = "{" + rslt + "}";
			}
		}
		return rslt;
	}

	/**
	 * Method to convert a list of types (superclasses) into a string containing only the most immediate supertypes
	 * @param types
	 * @return
	 */
	private String classListToString(List<OntClass> types, OntClass subClass) {
		types = lowestIndependentClassesOnly(types);
		String rslt = "";
		for (int i = 0; i < types.size(); i++) {
			OntClass r = types.get(i);
			if (i > 0) rslt += " or ";
			rslt += ontClassToString(r, subClass);
		}
		if (types.size() > 1) {
			rslt = "{" + rslt + "}";
		}
		return rslt;
	}

	private List<OntClass> lowestIndependentClassesOnly(List<OntClass> types) {
		List<OntClass> removals = null;
		for (int i = 0; i < types.size(); i++) {
			for (int j = 0; j < types.size(); j++) {
				if (j != i) {
					OntClass clsi = types.get(i);
					OntClass clsj = types.get(j);
					if (clsi.isUnionClass()) {
						if (unionClassContainsSuperclass(clsi.asUnionClass(), clsj)) {
							if (removals == null) removals = new ArrayList<OntClass>();
							removals.add(clsi);
						}
					}
					else if (clsj.isUnionClass()) {
						if (unionClassContainsSuperclass(clsj.asUnionClass(), clsi)) {
							if (removals == null) removals = new ArrayList<OntClass>();
							removals.add(clsj);							
						}
					}
					else if (clsi.hasSuperClass(clsj)) {
						if (removals == null) removals = new ArrayList<OntClass>();
						removals.add(clsj);
					}
					else if (clsj.hasSuperClass(clsi)) {
						if (removals == null) removals = new ArrayList<OntClass>();
						removals.add(clsi);

					}
				}
			}
		}
		if (removals != null) {
			for (int i = 0; i < removals.size(); i++) {
				if (types.contains(removals.get(i))) {
					types.remove(removals.get(i));
				}
			}
		}
		return types;
	}
	
	private boolean unionClassContainsSuperclass(UnionClass unionCls, OntClass cls) {
		ExtendedIterator<RDFNode> eitr = unionCls.getOperands().iterator();
		while (eitr.hasNext()) {
			RDFNode node = eitr.next();
			if (node.equals(cls) || (node.canAs(OntClass.class) && cls.hasSuperClass((OntClass)node.as(OntClass.class)))) {
				eitr.close();
				return true;
			}
		}
		return false;
	}

	private String domainToString(OntProperty prop) throws IOException {
		ExtendedIterator<? extends OntResource> ditr = prop.listDomain();
		while (ditr.hasNext()) {
			OntResource dmnNode = ditr.next();
			if (dmnNode.canAs(OntClass.class)) {
				return ontClassToString(dmnNode.as(OntClass.class), null);
			}
			else {
				throw new IOException("Domain of property '" + prop.toString() + "' is not an OntClass!");
			}
		}
		return null;
	}

	private String rangeToString(OntProperty prop) {
		ExtendedIterator<? extends OntResource> ritr = prop.listRange();
		String rng = "";
		int cnt = 0;
		while (ritr.hasNext()) {
			OntResource rngNode = ritr.next();
			if (!rngNode.isAnon() && rngNode.getNameSpace().equals(XSD.getURI())) {
				rng += rngNode.getLocalName();
			}
			else if (rngNode.canAs(OntClass.class)) {
				rng += ontClassToString((OntClass)rngNode.as(OntClass.class), null);
			}
			else {
				rng += rngNode.toString();
			}
			cnt++;
		}
		if (cnt > 1) {
			rng = "{" + rng + "}";
		}
		return rng;
	}

	private String ontClassToString(OntClass cls, OntClass subclass) {
		if (cls.isUnionClass()) {
			return getUnionClassRangeString(cls.asUnionClass());
		}
		else if (cls.isIntersectionClass()) {
			return getIntersectionClassRangeString(cls.asIntersectionClass());
		}
		else if (cls.isRestriction()) {
			if (!shouldResourceBeOutput(cls, true, false, false)) {
				Restriction rest = cls.asRestriction();
				Property onProp = rest.getOnProperty();
				if (rest.isAllValuesFromRestriction()) {
					Resource avfr = rest.asAllValuesFromRestriction().getAllValuesFrom();
					addRestriction(subclass, resourceToString(onProp, false) + " of " + resourceToString(subclass, false) + " only has values of type " + 
							resourceToString(avfr, false) + ".\n");
				}
				else if (rest.isSomeValuesFromRestriction()) {
					Resource svfr = rest.asSomeValuesFromRestriction().getSomeValuesFrom();
					addRestriction(subclass, resourceToString(onProp, false) + " of " + resourceToString(subclass, false) + " has at least one value of type " + 
							resourceToString(svfr, false) + ".\n");				
				}
				else if (rest.isMaxCardinalityRestriction()) {
					int maxCard = rest.asMaxCardinalityRestriction().getMaxCardinality();
					addRestriction(subclass, resourceToString(onProp, false) + " of " + resourceToString(subclass, false) + " has at most " + 
							maxCard + (maxCard > 1 ? " values.\n" : " value.\n"));
				}
				else if (rest.isMinCardinalityRestriction()) {
					int minCard = rest.asMinCardinalityRestriction().getMinCardinality();
					addRestriction(subclass, resourceToString(onProp, false) + " of " + resourceToString(subclass, false) + " has at least " + 
							minCard + (minCard > 1 ? " values.\n" : " value.\n"));
				}
				else if (rest.isCardinalityRestriction()) {
					int card = rest.asCardinalityRestriction().getCardinality();
					addRestriction(subclass, resourceToString(onProp, false) + " of " + resourceToString(subclass, false) + " has exactly " + 
							card + (card > 1 ? " values.\n" : " value.\n"));
				}
				else if (rest.isHasValueRestriction()) {
					RDFNode hvr = rest.asHasValueRestriction().getHasValue();
					addRestriction(subclass, resourceToString(onProp, false) + " of " + resourceToString(subclass, false) + " always has value " + 
							rdfNodeToString(hvr, 0) + " .\n");				
				}
				else {
					logger.debug("Unhandled restriction: " + rest.getClass());
				}
				resourcesOutput.add(cls);
			}
			if (cls.isAnon()) {
//				logger.debug("returning null on anon restriction--should this happen??");
				return null;
			}
			else {
				return cls.toString();
			}
			
		}
		else if (cls.isEnumeratedClass()) {
			EnumeratedClass enumcls = cls.asEnumeratedClass();
			return enumeratedClassToString(enumcls);
		}
		else if (cls.isComplementClass()) {
			ComplementClass ccls = cls.asComplementClass();
			OntClass thecls = ccls.getDisjointWith();
			if (thecls != null) {
				return resourceToString(thecls, false);
			}
			else {
				return null;
			}
		}
		else if (!cls.isAnon()) {
			return resourceToString(cls, false);
		}
		else {
			ExtendedIterator<Resource> titr = cls.listRDFTypes(true);
			System.err.println("Anon class types:");
			while (titr.hasNext()) {
				System.err.println(titr.next().toString());
			}
			StmtIterator pitr = cls.listProperties();
			System.err.println("Anon class properties:");
			while (pitr.hasNext()) {
				System.err.println(pitr.nextStatement().toString());
			}
			logger.debug("Anon class; returning string equivalent--this shouldn't happen.");
			return cls.toString();
		}
	}
	
	private String enumeratedClassToString(EnumeratedClass enumcls) {
		StringBuilder sb = new StringBuilder(" must be one of {");
		ExtendedIterator<? extends OntResource> eitr = enumcls.listOneOf();
		int cnt = 0;
		while (eitr.hasNext()) {
			OntResource r = eitr.next();
			if (cnt++ > 0) {
				sb.append(", ");
			}
			sb.append(resourceToString(r, false));
			checkResourceOutputComplete(r);
		}
		sb.append("}");
		return sb.toString();
	}

	private void checkResourceOutputComplete(OntResource r) {
		if (r.canAs(Individual.class)) {
			Individual inst = r.asIndividual();
			StmtIterator sitr = inst.listProperties();
			while (sitr.hasNext()) {
				Statement s = sitr.next();
				if (!s.getPredicate().equals(RDF.type)) {
					logger.debug(s.toString());
					sitr.close();
					return;
				}
			}
			resourcesOutput.add(r);
			sitr.close();
		}
	}

	private void addRestriction(OntResource cls, String rest) {
		if (restrictions == null) restrictions = new HashMap<String, OntResource>();
		if (!restrictions.containsKey(rest)) {
			restrictions.put(rest, cls);
		}
	}
	
	private void addRestrictionsToOutput(OntResource cls, boolean indent) {
		if (restrictions != null && restrictions.containsValue(cls)) {
			Iterator<String>  itr = restrictions.keySet().iterator();
			while (itr.hasNext()) {
				String key = itr.next();
				OntResource r = restrictions.get(key);
				if (r != null && r.equals(cls)) {
					if (indent) {
						sadlModel.append("\t");
					}
					sadlModel.append(key);
					restrictions.put(key, null);	// clear
				}
			}
		}
		
	}

	private String resourceToString(Resource rsrc, boolean includeAnnotations) {
		if (rsrc == null) {
			return "null";
		}
		if (rsrc.isAnon()) {
			if (rsrc.canAs(OntClass.class)) {
				return ontClassToString(rsrc.as(OntClass.class), null);
			}
			else {
				return rdfNodeToString(rsrc, 0);
			}
		}
		String ns;
		String ln;
		String uri = rsrc.getURI();
		if (uri.indexOf('#') > 0) {
			ns = uri.substring(0, uri.indexOf('#') + 1);
			ln = uri.substring(uri.indexOf('#') + 1);
		}
		else {
			ns = rsrc.getNameSpace();
			ln = rsrc.getLocalName();
		}
		String prefix = null;
		if (!sameNs(ns, getBaseUri())) {
			// don't include NS if in the base model.
			if (prefix == null) {
				prefix = theModel.getNsURIPrefix(ns);
			}
			prefix = sadlizePrefix(prefix);
		}
		if (allTokens.contains(ln)) {
			ln = "^" + ln;
		}
		if (prefix != null && prefix.length() > 0) {
			ln = prefix + ":" + ln;
		}
		if (rsrc.canAs(OntResource.class) && shouldResourceBeOutput(rsrc.as(OntResource.class), true, includeAnnotations, true)) {
			if (includeAnnotations) {
				StmtIterator sitr = rsrc.listProperties(RDFS.label);
				if (sitr.hasNext()) {
					ln += " (alias ";
					int cntr = 0;
					while (sitr.hasNext()) {
						Statement stmt = sitr.nextStatement();
						if (cntr++ > 0) {
							ln += ", ";
						}
						ln += "\"" + stmt.getObject().asLiteral().getLexicalForm() + "\"";
					}
					ln += ")";
				}
				
				sitr = rsrc.listProperties(RDFS.comment);
				if (sitr.hasNext()) {
					ln += " (note ";
					int cntr = 0;
					while (sitr.hasNext()) {
						Statement stmt = sitr.nextStatement();
						if (cntr++ > 0) {
							ln += ", ";
						}
						ln += "\"" + stmt.getObject().asLiteral().getLexicalForm() + "\"";
					}
					ln += ")";
				}

				sitr = rsrc.listProperties(RDFS.seeAlso);
				if (sitr.hasNext()) {
					ln += " (see ";
					int cntr = 0;
					while (sitr.hasNext()) {
						Statement stmt = sitr.nextStatement();
						if (cntr++ > 0) {
							ln += ", ";
						}
						ln += "\"" + stmt.getObject().asResource().getURI() + "\"";
					}
					ln += ")";
				}
}
		}
		return ln;
	}

	private String sadlizePrefix(String prefix) {
		if (prefix != null) {
			prefix = prefix.replace('.', '_');	// Jena likes to create prefixes with period.
			prefix = prefix.replace('-', '_');
		}
		return prefix;
	}

	private String rdfNodeToString(RDFNode node, int indentLevel) {
		if (node.isAnon()) {
			if (node instanceof Resource && ((Resource)node).canAs(Individual.class)) {
				// this is an unnamed individual
				Individual bnodeInst = ((Resource)node).as(Individual.class);
				resourcesOutput.add(bnodeInst);
				String typedBNode = "(a ";
				ExtendedIterator<Resource> eitr = bnodeInst.listRDFTypes(true);
				if (eitr.hasNext()) {
					Resource dcls = eitr.next();
					typedBNode += resourceToString(dcls, false);
				}
				else {
					typedBNode += "bnode_missing_type";
				}
				String instProps = instancePropertiesToString(bnodeInst, true, indentLevel + 1);
				if (instProps != null) {
					typedBNode += instProps;
				}
				typedBNode += ")";
				return typedBNode;
			}
		}
		if (node instanceof Resource) {
			return resourceToString((Resource) node, false);
		}
		else if (node instanceof Literal) {
			Object objVal = ((Literal)node).getValue();
			if (objVal instanceof Integer || objVal instanceof Long) {
				return objVal.toString() + " ";
			}
			else if (objVal instanceof Number) {
				return objVal.toString();
			}
			else if (objVal instanceof Boolean) {
				return objVal.toString();
			}
			else  {
				String val = objVal.toString().trim();
				if (val.startsWith("\"") && val.endsWith("\"")) {
					// string is already quoted
					return objVal.toString();
				}
				else {
					return "\"" + objVal.toString() + "\"";
				}
			}
//			else {
//				return "\"" + objVal.toString() + "\"";
//			}
		}
		return "this shouldn't happen in rdfNodeToString";
	}

	protected boolean shouldResourceBeOutput(OntResource rsrc, boolean bThisModelOnly, boolean includeProcessed, boolean includeAnon) {
		if (rsrc.isAnon() && !includeAnon) {
			return false;
		}
//		if (!includeProcessed && resourcesOutput.contains(rsrc)) {
//			return false;
//		}
		if (! rsrc.isAnon() && ignoreNamespace(rsrc, bThisModelOnly)) {
			return false;
		}
		if (!theModel.getBaseModel().containsResource(rsrc)) {
			return false;
		}
		StmtIterator typitr = theModel.listStatements(rsrc, RDF.type, (RDFNode)null);
		if (typitr.hasNext()) {
			return true;
		}
		try {
			if (rsrc.getRDFType() == null) {
				return false;
			}
		}
		catch (Throwable t) {
			concepts.addErrorMessage("Error trying to get type of '" + rsrc.toString() + "': " + t.getMessage());
			return false;
		}
		return true;
	}

	protected boolean shouldResourceBeOutput(Resource rsrc, boolean bThisModelOnly, boolean includeProcessed, boolean includeAnon) {
		if (rsrc.isAnon() && !includeAnon) {
			return false;
		}
//		if (!includeProcessed && resourcesOutput.contains(rsrc)) {
//			return false;
//		}
		if (! rsrc.isAnon() && ignoreNamespace(rsrc, bThisModelOnly)) {
			return false;
		}
		if (!theModel.getBaseModel().containsResource(rsrc)) {
			return false;
		}
		return true;
	}

	protected boolean ignoreNamespace(Resource rsrc, boolean bThisModelOnly) {
		String uri = rsrc.getNameSpace();
		if (uri == null) {
			return true;
		}
		String nm = uri.endsWith("#") ? uri.substring(0, uri.length() - 1) : uri;
		if (nm.equals(getBaseUri())) {
			return false;
		}
		if (bThisModelOnly && imports != null && imports.contains(nm)) {
			// if this is in an import namespace, ignore it.
			return true;
		}
		// namespaces to ingore: rdf, rdfs, owl
		if (uri.equals("http://www.w3.org/2000/01/rdf-schema#") ||
				uri.equals("http://www.w3.org/1999/02/22-rdf-syntax-ns#") ||
				uri.equals("http://www.w3.org/2002/07/owl#") ||
				uri.startsWith("http://purl.org/dc/elements/1.1") ||
				uri.startsWith("http://www.w3.org/2001/XMLSchema#") ||
				nm.equals(SADL_BASE_MODEL_URI) ||
				nm.equals(SADL_BUILTIN_FUNCTIONS_URI) ||
				nm.equals(SADL_IMPLICIT_MODEL_URI) ||
				nm.equals(SADL_LIST_MODEL_URI) ||
				uri.equals("")) {
			return true;
		}
		else if (bThisModelOnly){
			if (!rsrc.isAnon() && sameNs(rsrc.getNameSpace(),getBaseUri())) {
				return false;
			}
			String prefix = theModel.getNsURIPrefix(uri);
			if (prefix != null && prefix.length() > 0) {
				return true;
			}
		}
		return false;
	}

	private boolean sameNs(String ns1, String ns2) {
		if (ns1.endsWith("#")) {
			ns1 = ns1.substring(0, ns1.length() - 1);
		}
		if (ns2.endsWith("#")) {
			ns2 = ns2.substring(0, ns2.length() - 1);
		}
		if (ns1.equals(ns2)) {
			return true;
		}
		return false;
	}

//	protected OntDocumentManager prepare(String modelUrl, String policyFilename) throws IOException {
//		File of = validateOntologyName(modelUrl);
//		File pf;
//		if (policyFilename != null && policyFilename.length() > 0) {
//			pf = new File(policyFilename);
//		}
//		else {
//			throw new IOException("Policy file name is invalid");
//		}
//		return new UtilsForJena().loadMappings(pf);
//		
//		OntModel m = new UtilsForJena().createAndInitializeJenaModel(policyFilename);
//		m.getSpecification();
//	}
//
	protected File validateOntologyName(String ontology) throws IOException {
		URL url = new URL(ontology);
		String filename = URLDecoder.decode(url.getFile(), "UTF-8");
		File of = new File(filename);
		if (!of.exists()) {
			File cd = new File(".");
			if (cd.exists()) {
				logger.debug("Current directory: " + cd.getCanonicalPath());
			}
			throw new IOException("Ontology file '" + ontology + "' does not exist.");
		}
		else if (of.isDirectory()) {
			throw new IOException("Ontology file '" + ontology + "' is a directory.");
		}
		sourceFile = of.getName();
		return of;
	}
	
	protected File validateFileName(String urlStr) throws IOException {
		URL url = new URL(urlStr);
		String filename = URLDecoder.decode(url.getFile(), "UTF-8");
		File of = new File(filename);
		if (of.exists()) {
			return of;
		}
		throw new IOException("File '" + filename + "' does not exist.");
	}

	protected File getSiblingFile(File theFile, String fileName) throws IOException {
		File parentDir = theFile.getParentFile();
		String siblingFileName = parentDir.getCanonicalPath() + File.separator + fileName;
		File siblingFile = new File(siblingFileName);
		if (siblingFile.exists()) {
			return siblingFile;
		}
		return null;
	}

	protected static boolean isSingleValued(OntClass cls, OntProperty prop, String rngString) {
		if (prop.isFunctionalProperty()) {
			return true;
		}
		if (cls != null) {
			ExtendedIterator<OntClass> eitr = cls.listSuperClasses(false);
			while (eitr.hasNext()) {
				OntClass supercls = eitr.next();
				if (supercls.isRestriction()) {
					Restriction rstrct = supercls.asRestriction();
					if (rstrct.isMaxCardinalityRestriction()) {
						MaxCardinalityRestriction mxcr = rstrct.asMaxCardinalityRestriction();
						if (mxcr.getOnProperty().equals(prop) && mxcr.getMaxCardinality() == 1) {
							return true;
						}
					}
					else if (rstrct.isCardinalityRestriction()) {
						if (rstrct.isCardinalityRestriction()) {
							CardinalityRestriction cr = rstrct.asCardinalityRestriction();
							if (cr.getOnProperty().equals(prop) && cr.getCardinality() == 1) {
								return true;
							}
						}
					}
					else {
						if (rstrct.hasProperty(OWL2.maxQualifiedCardinality)) {
							if (rstrct.getOnProperty().equals(prop) && rstrct.getProperty(OWL2.maxQualifiedCardinality).getInt() == 1) {
								// check class
								if (rstrct.getProperty(OWL2.onClass).getResource().toString().equals(rngString)) {
									return true;
								}
							}
						}
						else if (rstrct.hasProperty(OWL2.qualifiedCardinality)) {
							if (rstrct.getOnProperty().equals(prop) && rstrct.getProperty(OWL2.qualifiedCardinality).getInt() == 1) {
								// check class
								if (rstrct.getProperty(OWL2.onClass).getResource().toString().equals(rngString)) {
									return true;
								}
							}							
						}
//						StmtIterator siter = rstrct.listProperties();
//						while (siter.hasNext()) {
//							logger.debug(siter.nextStatement().toString());
//						}
					}
				}
			}
		}
		return false;
	}

	/**
	 * Call this method to remove double quotes from the beginning and end of a
	 * string so quoted.
	 * 
	 * @param quotedString
	 *            -- the string from which quotes are to be removed
	 */
	protected String stripQuotes(String quotedString) {
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

	private OntModelSpec getSpec() {
		if (spec == null) {
			spec = OntModelSpec.OWL_MEM;
		}
		return spec;
	}

	private void setSpec(OntModelSpec spec) {
		this.spec = spec;
	}

	public boolean isVerboseMode() {
		return verboseMode;
	}

	public void setVerboseMode(boolean verboseMode) {
		this.verboseMode = verboseMode;
		if (verboseMode && verboseModeStringBuilder == null) {
			verboseModeStringBuilder = new StringBuilder();
		}
	}

	private ModelConcepts getConcepts() throws OwlImportException {
		if (concepts == null) {
			initialize();
		}
		return concepts;
	}

	private void setConcepts(ModelConcepts concepts) {
		this.concepts = concepts;
	}

	public boolean isNeverUsePrefixes() {
		return neverUsePrefixes;
	}

	public void setNeverUsePrefixes(boolean neverUsePrefixes) {
		this.neverUsePrefixes = neverUsePrefixes;
	}

	public String getBaseUri() {
		return baseUri;
	}

	public void setBaseUri(String baseUri) {
		this.baseUri = baseUri;
	}

	private String getPrefix() {
		return prefix;
	}

	private void setPrefix(String prefix) {
		this.prefix = prefix;
	}
}
