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
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.xtext.GrammarUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.SADLStandaloneSetup;
import com.ge.research.sadl.parser.antlr.SADLParser;
import com.google.inject.Injector;
import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.graph.Triple;
import com.hp.hpl.jena.ontology.AllValuesFromRestriction;
import com.hp.hpl.jena.ontology.AnnotationProperty;
import com.hp.hpl.jena.ontology.CardinalityRestriction;
import com.hp.hpl.jena.ontology.ComplementClass;
import com.hp.hpl.jena.ontology.ConversionException;
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
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.query.Syntax;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.NodeIterator;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFList;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.ResourceFactory;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.OWL2;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;
import com.hp.hpl.jena.vocabulary.XSD;

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

	//	replaced with getSadlKeywords to get straight from grammar
//	// copied from com.ge.research.sadl.parser.antlr.internal.InternalSadlParser in com.ge.research.sadl project.
//    public static final String[] tokenNames = new String[] {
//        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "RULE_STRING", "RULE_EOS", "RULE_ID", "RULE_UNSIGNED_NUMBER", "RULE_INT", "RULE_ML_COMMENT", "RULE_SL_COMMENT", "RULE_WS", "RULE_ANY_OTHER", "'uri'", "'alias'", "'version'", "'import'", "'as'", "'('", "'note'", "')'", "'{'", "','", "'}'", "'or'", "'and'", "'is'", "'a'", "'top-level'", "'class'", "'are'", "'classes'", "'type'", "'of'", "'types'", "'must'", "'be'", "'one'", "'described'", "'by'", "'has'", "'with'", "'single'", "'value'", "'values'", "'A'", "'An'", "'an'", "'The'", "'the'", "'same'", "'disjoint'", "'not'", "'only'", "'can'", "'level'", "'default'", "'at'", "'least'", "'each'", "'always'", "'most'", "'exactly'", "'if'", "'relationship'", "'to'", "'annotation'", "'describes'", "'subject'", "'symmetrical'", "'transitive'", "'inverse'", "'any'", "'Rule'", "':'", "'given'", "'then'", "'Ask:'", "'Test:'", "'Expr:'", "'Print:'", "'Deductions'", "'Model'", "'Explain:'", "'select'", "'distinct'", "'*'", "'where'", "'order by'", "'asc'", "'desc'", "'||'", "'&&'", "'='", "'=='", "'!='", "'<'", "'<='", "'>'", "'>='", "'+'", "'-'", "'/'", "'^'", "'%'", "'!'", "'PI'", "'known'", "'['", "']'", "'true'", "'false'", "'.'", "'~'", "'string'", "'boolean'", "'decimal'", "'int'", "'long'", "'float'", "'double'", "'duration'", "'dateTime'", "'time'", "'date'", "'gYearMonth'", "'gYear'", "'gMonthDay'", "'gDay'", "'gMonth'", "'hexBinary'", "'base64Binary'", "'anyURI'", "'data'"
//    };
    		
	private String baseUri;

	private String sourceFile = null;

	private OntModelSpec spec;

	private List<String> propertiesProcessed = new ArrayList<String>();	// properties already completely processed
	
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
				System.out.println("Unmapped restriction information:");
				StmtIterator sitr = unMappedRestriction.listProperties();
				while (sitr.hasNext()) {
					System.out.println("    " + sitr.nextStatement().toString());
				}
				OntClass equiv = unMappedRestriction.getEquivalentClass();
				if (equiv != null) {
					System.out.println("      equivalent class:" + equiv.toString());
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
		theModel.read(new FileInputStream(owlFile), baseUri);
//        theModel.read(owlFile.getCanonicalPath());
	}

	public OwlToSadl(String owlContent) {
		OntModelSpec spec = new OntModelSpec(OntModelSpec.OWL_MEM);
		setSpec(spec);
		theModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM); //_RDFS_INF);
		theModel.getDocumentManager().setProcessImports(false);
        theModel.read(new ByteArrayInputStream(owlContent.getBytes()), null);
	}
	
	private List<String> getSadlKeywords() {
		SADLParser sparser = null;
		Set<String> sadlkeywords = null;
		
	    Injector injector = new SADLStandaloneSetup().createInjectorAndDoEMFRegistration();
	    sparser = injector.getInstance(SADLParser.class);
		if (sparser != null) {
			sadlkeywords = GrammarUtil.getAllKeywords(sparser.getGrammarAccess().getGrammar());
			if (sadlkeywords != null) {
				StringBuilder sb = new StringBuilder("public static final String[] tokenNames = new String[] {");
				List<String> tokens = new ArrayList<String>();
				Iterator<String> itr = sadlkeywords.iterator();
				int cntr = 0;
				while (itr.hasNext()) {
					String token = itr.next();
					tokens.add(token);
					if (cntr++ > 0) {
						sb.append(",");
					}
					sb.append("\"");
					sb.append(token);
					sb.append("\"");
				}
				sb.append("};");
				System.out.println(sb.toString());
				return tokens;
			}
		}
		return null;
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

	private void process() throws OwlImportException {
		if (theModel == null) {
			throw new OwlImportException("There is no OWL model to translate!");
		}

		ModelConcepts concepts = new ModelConcepts();

		theModel.getDocumentManager().setProcessImports(false);
		Map<String,String> modelPrefixMap = theModel.getNsPrefixMap();
		if (modelPrefixMap != null) {
			Iterator<String> pitr = modelPrefixMap.keySet().iterator();
			while (pitr.hasNext()) {
				String prefix = pitr.next();
				String uri = modelPrefixMap.get(prefix);
				qNamePrefixes.put(uri, prefix);
			}
		}
		if (logger.isDebugEnabled()) {
			theModel.getBaseModel().write(System.out);
		}
		getSpec().getDocumentManager().setProcessImports(false);
		theBaseModel = ModelFactory.createOntologyModel(getSpec(), theModel);
		
		if (allTokens == null) {
//			allTokens = new ArrayList<String>();
//			String[] tokens = tokenNames;
//			for (int i = 0; i < tokens.length; i++) {
//				String strippedToken = stripQuotes(tokens[i]);
//				String[] words = strippedToken.trim().split(" ");
//				if (words != null) {
//					for (int j = 0; j < words.length; j++) {
//						allTokens.add(words[j]);
//					}
//				}
//			}
			allTokens = getSadlKeywords();
		}
		if (sadlModel == null) {
			sadlModel = new StringBuilder();
		}
		baseUri = theModel.getNsPrefixURI("");
		if (baseUri == null) {
			ExtendedIterator<Ontology> eitr = theModel.listOntologies();
			while (eitr.hasNext()) {
				Ontology ont = eitr.next();
				baseUri = ont.getURI();
				break;		// will this model's ontology always be first?
			}
		}
		if (baseUri == null) {
			Map<String, String> map = theModel.getNsPrefixMap();
			Iterator<String> mitr = map.keySet().iterator();
			while (mitr.hasNext()) {
				String prefix = mitr.next();
				System.out.println("Mapping: " + prefix + " = " + map.get(prefix));
			}			baseUri = "http://sadl.org/baseless";
		}
		if (baseUri.endsWith("#")) {
			baseUri = baseUri.substring(0, baseUri.length() - 1);
		}
		sadlModel.append("uri \"");
		sadlModel.append(baseUri);
		sadlModel.append("\"");
		
		String alias = null;
		if (alias == null) {
			if (qNamePrefixes.containsKey(baseUri + "#")) {
				alias = qNamePrefixes.get(baseUri + "#");
			}
		}
		if (alias != null && alias.length() > 0) {
			sadlModel.append(" alias ");
			sadlModel.append(alias);
		}
		
		Ontology ont = theModel.getOntology(baseUri);
		if (ont == null) {
			ont = theModel.getOntology(baseUri + "#");
		}
		
		String version;
		if (ont != null && (version = ont.getVersionInfo()) != null) {
			sadlModel.append("\n    version \"");
			sadlModel.append(version);
			sadlModel.append("\"");
		}

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
		if (ont != null) {
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
				if (prefix == null) {
					prefix = theModel.getNsURIPrefix(impUri+"#");
				}
				prefix = sadlizePrefix(prefix);
				sadlModel.append("import \"");
				sadlModel.append(impUri);
				sadlModel.append("\" as ");
				if (prefix == null) {
					prefix = "pre_" + prefixCntr++;
				}
				sadlModel.append(prefix);
				theModel.setNsPrefix(prefix, impUri + "#");
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
			System.out.println("Processing model statement: " + s.toString());
			if (isVerboseMode()) {
				verboseModeStringBuilder.append("// Processed statement: ");
				verboseModeStringBuilder.append(s.toString());
				verboseModeStringBuilder.append("\n");
			}
			Resource subj = s.getSubject();
			OntResource ontSubj = theModel.getOntResource(subj);
			if (shouldResourceBeOutput(ontSubj, false, false, true)) {
				if (!addResourceToList(concepts, ontSubj)) {
					System.out.println("subject resource not added to a list: " + ontSubj.toString());
					if (isVerboseMode()) {
						verboseModeStringBuilder.append("//     subject resource not added to processing list: \": ");
						verboseModeStringBuilder.append(ontSubj.toString());
						verboseModeStringBuilder.append("\n");
					}
				}
			}
			Property prop = s.getPredicate();
			Property mprop = theModel.getProperty(prop.getURI());
			if (mprop.canAs(OntProperty.class)) {
				OntProperty ontProp = mprop.as(OntProperty.class);
				if (shouldResourceBeOutput(ontProp, false, false, true)) {
					if(!addResourceToList(concepts, ontProp)) {
						System.out.println("predicate resource not added to a list: " + ontProp.toString());
						if (isVerboseMode()) {
							verboseModeStringBuilder.append("//     predicate resource not added to processing list: \": ");
							verboseModeStringBuilder.append(ontProp.toString());
							verboseModeStringBuilder.append("\n");
						}
					}
				}
			}
			else {
				if (shouldResourceBeOutput(mprop, false, false, true)) {
					concepts.addRdfProperty(mprop);
				}
			}
			RDFNode obj = s.getObject();
			if (obj.isResource()) {
				OntResource ontObj = theModel.getOntResource(obj.asResource());
				if (shouldResourceBeOutput(ontObj, false, false, true)) {
					if(!addResourceToList(concepts, ontObj)) {
						System.out.println("object resource not added to a list: " + ontObj.toString());
						if (isVerboseMode()) {
							verboseModeStringBuilder.append("//     object resource not added to processing list: \": ");
							verboseModeStringBuilder.append(ontObj.toString());
							verboseModeStringBuilder.append("\n");
						}
					}
				}
			}
			else {
				// this is a literal, object of a Datatype property
			}
		}
		
		// Output messages as SADL comments
		if (concepts.getErrorMessages() != null && concepts.getErrorMessages().size() > 0) {
			sadlModel.append("\n\n// Errors:\n");
			Iterator<String> erritr = concepts.getErrorMessages().iterator();
			while (erritr.hasNext()) {
				sadlModel.append("// ");
				sadlModel.append(erritr.next());
				sadlModel.append("\n");
			}
		}
		else if (isVerboseMode()) {
			sadlModel.append("\n\n// No Errors\n");
		}
		
		if (concepts.getWarningMessages() != null && concepts.getWarningMessages().size() > 0) {
			sadlModel.append("\n\n// Warnings:\n");
			Iterator<String> erritr = concepts.getWarningMessages().iterator();
			while (erritr.hasNext()) {
				sadlModel.append("// ");
				sadlModel.append(erritr.next());
				sadlModel.append("\n");
			}
		}
		else if (isVerboseMode()) {
			sadlModel.append("\n\n// No Warnings\n");
		}
		
		if (concepts.getInfoMessages() != null && concepts.getInfoMessages().size() > 0) {
			sadlModel.append("\n\n// Info:\n");
			Iterator<String> erritr = concepts.getInfoMessages().iterator();
			while (erritr.hasNext()) {
				sadlModel.append("// ");
				sadlModel.append(erritr.next());
				sadlModel.append("\n");
			}
		}
		else if (isVerboseMode()) {
			sadlModel.append("\n\n// No Info output\n");
		}
		
		// sort before starting to generate output
		concepts.sort();
		
		if (isVerboseMode()) {
			sadlModel.append("\n\n// Ontologies:\n");
			List<Ontology> onts = concepts.getOntologies();
			for (int i = 0; i < onts.size(); i++) {
				Ontology onti = onts.get(i);
				sadlModel.append("//    ");
				sadlModel.append(onti.toString());
				sadlModel.append("\n");
			}
		}
		
		if (isVerboseMode() || !concepts.getDatatypes().isEmpty()) {
			sadlModel.append("\n\n// Datatype Declarations:\n");
			List<OntResource> datatypes = concepts.getDatatypes();
			for (int i = 0; i < datatypes.size(); i++) {
				sadlModel.append(datatypeToSadl(concepts, datatypes.get(i)));
			}
		}
		
		if (isVerboseMode() || !concepts.getAnnProperties().isEmpty()) {
			sadlModel.append("\n\n// Annotation Properties:\n");
			List<AnnotationProperty> anns = concepts.getAnnProperties();
			for (int i = 0; i < anns.size(); i++) {
				AnnotationProperty ann = anns.get(i);
				if (!concepts.getCompleted().contains(ann)) {
					sadlModel.append(annotationsToSadl(concepts, ann));
					concepts.addCompleted(ann);
				}
			}
		}
		if (isVerboseMode() || !concepts.getRdfProperties().isEmpty()) {
			sadlModel.append("\n\n// RDF Properties:\n");
			List<Property> rdfProperties = concepts.getRdfProperties();
			for (int i = 0; i < rdfProperties.size(); i++) {
				Property prop = rdfProperties.get(i);
				addRdfProperty(concepts, sadlModel, prop);
				concepts.addCompleted(prop);
			}
		}
		
		List<ObjectProperty> objProperties = concepts.getObjProperties();
		if (isVerboseMode() || !objProperties.isEmpty()) {
			StringBuilder tempSb = new StringBuilder();
			for (int i = 0; i < objProperties.size(); i++) {
				OntResource prop = objProperties.get(i);
				addSuperPropertiesWithoutRange(concepts, tempSb, prop);
				concepts.addCompleted(prop);
			}
			if (isVerboseMode() || tempSb.length() > 0) {
				sadlModel.append("\n\n// Object properties without specified range:\n");
				sadlModel.append(tempSb);
			}
		}
		
		List<DatatypeProperty> dtProperties = concepts.getDtProperties();
		if (isVerboseMode() || !dtProperties.isEmpty()) {
			StringBuilder tempSb = new StringBuilder();
			for (int i = 0; i < dtProperties.size(); i++) {
				OntResource prop = dtProperties.get(i);
				addSuperPropertiesWithoutRange(concepts, tempSb, prop);
			}
			if (isVerboseMode() || tempSb.length() > 0) {
				sadlModel.append("\n\n// Datatype properties without specified range:\n");
				sadlModel.append(tempSb);
			}
		}
		
		if (isVerboseMode() || !concepts.getClasses().isEmpty()) {
			sadlModel.append("\n\n// Class definitions:\n");
			List<OntClass> classes = concepts.getClasses();
			for (int i = 0; i < classes.size(); i++) {
				OntClass cls = classes.get(i);
				sadlModel.append(classToSadl(concepts, cls));
			}
		}
		
		if (isVerboseMode() || objProperties.size() > 0) {
			StringBuilder tempSb = new StringBuilder();
			objProperties = concepts.getObjProperties();
			for (int i = 0; i < objProperties.size(); i++) {
				OntResource prop = objProperties.get(i);
				if (!concepts.getCompleted().contains(prop)) {
					tempSb.append(objPropertyToSadl(concepts, prop));
				}
			}
			if (isVerboseMode() || tempSb.length() > 0) {
				sadlModel.append("\n\n// Other object Properties:\n");
				sadlModel.append(tempSb);
			}
		}
		
		if (isVerboseMode() || dtProperties.size() > 0) {
			StringBuilder tempSb = new StringBuilder();
			dtProperties = concepts.getDtProperties();
			for (int i = 0; i < dtProperties.size(); i++) {
				OntResource prop = dtProperties.get(i);
				if (!concepts.getCompleted().contains(prop)) {
					if (!ignoreNamespace(prop, true)) {
						tempSb.append(dtPropertyToSadl(concepts, prop));
					}
				}
			}
			if (isVerboseMode() || tempSb.length() > 0) {
				sadlModel.append("\n\n// Other datatype Properties:\n");
				sadlModel.append(tempSb);
			}
		}
		
		if (isVerboseMode() || !concepts.getInstances().isEmpty()) {
			sadlModel.append("\n\n// Individuals:\n");
			List<Individual> instances = concepts.getInstances();
			for (int i = 0; i < instances.size(); i++) {
				Individual inst = instances.get(i);
				sadlModel.append(individualToSadl(concepts, inst, false));
			}
		}
		
		if (isVerboseMode() || !concepts.getUnMappedRestrictions().isEmpty()) {
			sadlModel.append("\n\n// Other restrictions:\n");
			List<Restriction> ress = concepts.getUnMappedRestrictions();
			for (int i = 0; i < ress.size(); i++) {
				Restriction res = ress.get(i);
				boolean invalid = false;
				// what is a subclasses of this restriction?
				StmtIterator oitr = theModel.listStatements(null, RDFS.subClassOf, res);
				while (oitr.hasNext()) {
					Statement s1 = oitr.nextStatement();
					System.out.println(s1.toString());
					Resource subj = s1.getSubject();
					if (subj.canAs(Ontology.class) || subj.equals(OWL.Ontology)) {
						sadlModel.append("// restriction on Ontology not supported in SADL: \n    // ");
						sadlModel.append(restrictionToString(concepts, null, res));
						sadlModel.append("\n");
						invalid = true;
						break;
					}
				}
				if (!invalid) {
					sadlModel.append(restrictionToString(concepts, null, res));
					addEndOfStatement(sadlModel, 1);
				}
			}
		}
		
		if (isVerboseMode() && verboseModeStringBuilder.length() > 0) {
			sadlModel.append("\n\n");
			sadlModel.append(verboseModeStringBuilder);
		}
	}
	
	public static final String SADL_BASE_MODEL_URI = "http://sadl.org/sadlbasemodel";
	public static final String SADL_LIST_MODEL_URI = "http://sadl.org/sadllistmodel";
	public static final String SADL_IMPLICIT_MODEL_URI = "http://sadl.org/sadlimplicitmodel";
	public static final String SADL_BUILTIN_FUNCTIONS_URI = "http://sadl.org/builtinfunctions";

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
		if (sb.length() > 1 && Character.isDigit(sb.charAt(sb.length() - 1))) {
			sb.append(" .");
		}
		else {
			sb.append(".");
		}
		for (int i = 0; i < numLineFeeds; i++) {
			sb.append("\n");
		}
	}

	private void addRdfProperty(ModelConcepts concepts, StringBuilder sb, Property prop) {
		sb.append(rdfPropertyToSadl(concepts, prop));
		
	}

	private void addSuperPropertiesWithoutRange(ModelConcepts concepts,
			StringBuilder sb, OntResource prop) {
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

	private Object datatypeToSadl(ModelConcepts concepts,
			OntResource rsrc) {
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
				addNotesAndAliases(sb, rsrc);
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
			sb.append("\n");
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
					 System.out.println(sitr.nextStatement().toString());									 
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
			System.out.println("Triple with 'first': " + t.toString());
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
						System.out.println("Adding triple: " + t2.toString());
					}
				}
			}
		}
		return trLst;
	}
	
	private String individualNameAndAnnotations(ModelConcepts concepts, Individual inst) {
		StringBuilder sb = new StringBuilder();
		if (!inst.getNameSpace().equals(baseUri+'#')) {
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
		addNotesAndAliases(sb, inst);
		return sb.toString();
	}

	private String individualToSadl(ModelConcepts concepts, Individual inst, boolean embeddedBNode) {
		StringBuilder sb = new StringBuilder();
		boolean bnode = false;
		if (inst.isURIResource()) {
			sb.append(individualNameAndAnnotations(concepts, inst));
			if (isNewLineAtEndOfBuffer(sb)) {
				sb.append("    ");
			}
			sb.append(" is a ");
		}
		else {
			sb.append("(a ");
			bnode = true;
		}
		ExtendedIterator<OntClass> eitr = inst.listOntClasses(true);
		int itercnt = 0;
		boolean intersectionClass = false;
		while (eitr.hasNext()) {
			try {
				OntClass cls = eitr.next();
				if (itercnt == 0) {
					if (eitr.hasNext()) {
						intersectionClass = true;
					}
					if (intersectionClass) {
						sb.append("{");
					}
				}
				if (itercnt++ > 0 && eitr.hasNext()) {
					sb.append(" or ");
				}
				sb.append(uriToSadlString(concepts, cls));
			}
			catch (Exception e){
				System.err.println(e.getMessage());
			}
		}
		if (intersectionClass) {
			sb.append("}");
		}
		StmtIterator insitr = inst.listProperties();
		int cntr = 0;
		while (insitr.hasNext()) {
			Statement s = insitr.next();
			if (s.getPredicate().equals(RDF.type) || s.getPredicate().equals(RDFS.label) || s.getPredicate().equals(RDFS.comment)) {
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
			cntr++;
		}
		if (bnode) {
			sb.append(")");
		}
		else {
			addEndOfStatement(sb, 1);
		}
		return sb.toString();
	}

	private String annotationsToSadl(ModelConcepts concepts,
			AnnotationProperty ann) {
		StringBuilder sb = new StringBuilder();
		sb.append(uriToSadlString(concepts, ann));
		addNotesAndAliases(sb, ann);
		sb.append(" is a type of annotation.\n");
		return sb.toString();
	}

	private String rdfPropertyToSadl(ModelConcepts concepts, Property prop) {
		StringBuilder sb = new StringBuilder();
		sb.append(uriToSadlString(concepts, prop));
		sb.append(" is a property.\n");
		return sb.toString();
	}
	
	private String dtPropertyToSadl(ModelConcepts concepts, OntResource prop) {
		StringBuilder sb = new StringBuilder();
		boolean useTranslation = true;
		sb.append(uriToSadlString(concepts, prop));
		addNotesAndAliases(sb, prop);
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

	private String objPropertyToSadl(ModelConcepts concepts, OntResource prop) {
		StringBuilder sb = new StringBuilder();
		boolean useTranslation = true;
		sb.append(uriToSadlString(concepts, prop));
		addNotesAndAliases(sb, prop);
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

	private String classToSadl(ModelConcepts concepts, OntClass cls) {
		StringBuilder sb = new StringBuilder();
		sb.append(uriToSadlString(concepts, cls));
		addNotesAndAliases(sb, cls);
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
				sb.append("{");
				Iterator<Resource> spIter = supers.iterator();
				int itercnt = 0;
				while (spIter.hasNext()) {
					if (itercnt++ > 0) {
						sb.append(" and ");
					}
					Resource spcls = spIter.next();
					String spStr = uriToSadlString(concepts, spcls);
					sb.append(spStr);
				}
				sb.append("}");
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
				sb.append(" must be ");
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
				addNotesAndAliases(sb, p);
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

	private void addNotesAndAliases(StringBuilder sb, Resource rsrc) {
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
	}

	private String restrictionToString(ModelConcepts concepts, OntClass restrictedClass, Restriction res) {
		if (res.isURIResource()) {
			return uriToSadlString(concepts, res);
		}
		StringBuilder sb = new StringBuilder();
		OntProperty onprop = null;
		try {
			onprop = res.getOnProperty();
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
			OntResource prop) {
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
			String key) {
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

	private String rdfNodeToSadlString(ModelConcepts concepts, RDFNode object, boolean forceQuotes) {
		if (object.isURIResource()) {
			return uriToSadlString(concepts, object.asResource());
		}
		else if (object.isLiteral()) {
			String dturi = object.asLiteral().getDatatypeURI();
			forceQuotes = forceQuotes ? true : (dturi != null ? isRDFDatatypeString(dturi, object) : true);
			if (object.asLiteral().getDatatypeURI() == null) {
				String lf = object.asLiteral().getLexicalForm();
				if (forceQuotes || lf.contains(" ") || lf.contains("\"")) {
					String s = object.asLiteral().getLexicalForm();
					s = s.replace("\"", "\\\"");
					return "\"" + s + "\""; 
				}
				return object.asLiteral().getLexicalForm();
			}
			if (forceQuotes || object.asLiteral().getDatatypeURI().equals(XSD.xstring.getURI())) {
				String s = object.asLiteral().getLexicalForm();
				if (s.startsWith("\"") && s.endsWith("\"")) {
					s = s.substring(1, s.length() - 2);
				}
				s = s.replace("\"", "\\\"");
				return "\"" + s + "\""; 
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
			return individualToSadl(concepts, object.as(Individual.class), true);
		}
		else {
			return object.toString();
		}
	}

	private String uriToSadlString(ModelConcepts concepts, Resource rsrc) {
		if (rsrc.isURIResource()) {
			if (rsrc.getNameSpace().equals(baseUri + "#")) {
				return checkLocalnameForKeyword(rsrc.getLocalName());
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
							if (prefix.length() > 0) {
								return prefix + ":" + checkLocalnameForKeyword(rsrc.getLocalName());
							}
							else {
								return checkLocalnameForKeyword(rsrc.getLocalName());
							}
						}
					}
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
			concepts.addObjProperty(ontRsrc.asObjectProperty());
			return true;
		}
		else if (type.equals(OWL.DatatypeProperty)) {
			concepts.addDtProperty(ontRsrc.asDatatypeProperty());
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
			concepts.addRdfProperty(ontRsrc.asProperty());
			return true;
		}
		else if (type.equals(OWL.AnnotationProperty)) {
			concepts.addAnnProperty(ontRsrc.asAnnotationProperty());
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
					System.out.println("Ignoring RDFDatatype blank node which is equivalent to '" + eqcls.toString() + "'");
				}
				return false;
			}
			System.out.println("Examining rdfs:Datatype: " + ontRsrc.toString());
			ExtendedIterator<Resource> eitr = ontRsrc.listRDFTypes(true);
			while (eitr.hasNext()) {
				System.out.println("    " + eitr.next().toString());
			}
			concepts.addDatatype(ontRsrc);
			return false;
		}
		else if (type.equals(OWL.Restriction)) {
			try {
				if (ontRsrc.canAs(Restriction.class) && ontRsrc.as(Restriction.class).getOnProperty().equals(RDFS.Resource)) {
					return false;
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
			sb.append("\n");
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
//						System.out.println("Equivalent class to '" + ontClassToString(cls, null) + "': "  + ontClassToString(equivCls, null));
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
					System.out.println("Unexpected error processing class '" + cls.toString() + "': " + t.getMessage());
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
			System.out.println("Unexpected erro getting super classes of '" + cls.toString() + "': " + t.getLocalizedMessage());
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
					System.out.println("Unhandled restriction: " + rest.getClass());
				}
				resourcesOutput.add(cls);
			}
			if (cls.isAnon()) {
//				System.out.println("returning null on anon restriction--should this happen??");
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
			System.out.println("Anon class; returning string equivalent--this shouldn't happen.");
			return cls.toString();
		}
	}
	
	private String enumeratedClassToString(EnumeratedClass enumcls) {
		StringBuilder sb = new StringBuilder("must be one of {");
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
					System.out.println(s);
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
		if (prefix == null) {
			prefix = theModel.getNsURIPrefix(ns);
		}
		prefix = sadlizePrefix(prefix);
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
		if (rsrc.getRDFType() == null) {
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
				uri.equals("")) {
			return true;
		}
		else if (bThisModelOnly){
			if (!rsrc.isAnon() && sameNs(rsrc.getNameSpace(),baseUri)) {
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
				System.out.println("Current directory: " + cd.getCanonicalPath());
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
//							System.out.println(siter.nextStatement().toString());
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
}
