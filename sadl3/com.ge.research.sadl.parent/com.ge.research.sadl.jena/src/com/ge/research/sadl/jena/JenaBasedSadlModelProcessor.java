/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.CheckType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.SadlMessage;
import com.ge.research.sadl.external.ExternalEmfResource;
import com.ge.research.sadl.jena.inference.SadlJenaModelGetterPutter;
import com.ge.research.sadl.model.CircularDefinitionException;
import com.ge.research.sadl.model.ConceptName.RangeValueType;
import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.ConstantNode;
import com.ge.research.sadl.model.gp.Equation;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.Junction.JunctionType;
//import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.ProxyNode;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.TripleElement.TripleSourceType;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.reasoner.CircularDependencyException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.SadlJenaModelGetter;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.AskExpression;
import com.ge.research.sadl.sADL.BinaryOperation;
import com.ge.research.sadl.sADL.BooleanLiteral;
import com.ge.research.sadl.sADL.Constant;
import com.ge.research.sadl.sADL.ConstructExpression;
import com.ge.research.sadl.sADL.Declaration;
import com.ge.research.sadl.sADL.EndWriteStatement;
import com.ge.research.sadl.sADL.EquationStatement;
import com.ge.research.sadl.sADL.ExplainStatement;
import com.ge.research.sadl.sADL.Expression;
import com.ge.research.sadl.sADL.ExpressionStatement;
import com.ge.research.sadl.sADL.ExternalEquationStatement;
import com.ge.research.sadl.sADL.Name;
import com.ge.research.sadl.sADL.NumberLiteral;
import com.ge.research.sadl.sADL.PrintStatement;
import com.ge.research.sadl.sADL.PropOfSubject;
import com.ge.research.sadl.sADL.QueryStatement;
import com.ge.research.sadl.sADL.ReadStatement;
import com.ge.research.sadl.sADL.RuleStatement;
import com.ge.research.sadl.sADL.SADLPackage;
import com.ge.research.sadl.sADL.SadlAllValuesCondition;
import com.ge.research.sadl.sADL.SadlAnnotation;
import com.ge.research.sadl.sADL.SadlBooleanLiteral;
import com.ge.research.sadl.sADL.SadlCanOnlyBeOneOf;
import com.ge.research.sadl.sADL.SadlCardinalityCondition;
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration;
import com.ge.research.sadl.sADL.SadlCondition;
import com.ge.research.sadl.sADL.SadlConstantLiteral;
import com.ge.research.sadl.sADL.SadlDataType;
import com.ge.research.sadl.sADL.SadlDataTypeFacet;
import com.ge.research.sadl.sADL.SadlDifferentFrom;
import com.ge.research.sadl.sADL.SadlDisjointClasses;
import com.ge.research.sadl.sADL.SadlExplicitValue;
import com.ge.research.sadl.sADL.SadlHasValueCondition;
import com.ge.research.sadl.sADL.SadlImport;
import com.ge.research.sadl.sADL.SadlInstance;
import com.ge.research.sadl.sADL.SadlIntersectionType;
import com.ge.research.sadl.sADL.SadlIsAnnotation;
import com.ge.research.sadl.sADL.SadlIsInverseOf;
import com.ge.research.sadl.sADL.SadlIsTransitive;
import com.ge.research.sadl.sADL.SadlModel;
import com.ge.research.sadl.sADL.SadlModelElement;
import com.ge.research.sadl.sADL.SadlMustBeOneOf;
import com.ge.research.sadl.sADL.SadlNecessaryAndSufficient;
import com.ge.research.sadl.sADL.SadlNumberLiteral;
import com.ge.research.sadl.sADL.SadlParameterDeclaration;
import com.ge.research.sadl.sADL.SadlPrimitiveDataType;
import com.ge.research.sadl.sADL.SadlProperty;
import com.ge.research.sadl.sADL.SadlPropertyCondition;
import com.ge.research.sadl.sADL.SadlPropertyInitializer;
import com.ge.research.sadl.sADL.SadlPropertyRestriction;
import com.ge.research.sadl.sADL.SadlRangeRestriction;
import com.ge.research.sadl.sADL.SadlResource;
import com.ge.research.sadl.sADL.SadlSameAs;
import com.ge.research.sadl.sADL.SadlSimpleTypeReference;
import com.ge.research.sadl.sADL.SadlStringLiteral;
import com.ge.research.sadl.sADL.SadlTypeAssociation;
import com.ge.research.sadl.sADL.SadlTypeReference;
import com.ge.research.sadl.sADL.SadlUnionType;
import com.ge.research.sadl.sADL.SadlValueList;
import com.ge.research.sadl.sADL.SelectExpression;
import com.ge.research.sadl.sADL.StartWriteStatement;
import com.ge.research.sadl.sADL.StringLiteral;
import com.ge.research.sadl.sADL.SubjHasProp;
import com.ge.research.sadl.sADL.Sublist;
import com.ge.research.sadl.sADL.TestStatement;
import com.ge.research.sadl.sADL.UnaryExpression;
import com.ge.research.sadl.sADL.Unit;
import com.ge.research.sadl.sADL.ValueRow;
import com.ge.research.sadl.sADL.ValueTable;
//import com.ge.research.sadl.server.ISadlServer;
//import com.ge.research.sadl.server.SessionNotFoundException;
//import com.ge.research.sadl.server.server.SadlServerImpl;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.inject.Inject;
import com.hp.hpl.jena.ontology.AllValuesFromRestriction;
import com.hp.hpl.jena.ontology.AnnotationProperty;
import com.hp.hpl.jena.ontology.CardinalityRestriction;
import com.hp.hpl.jena.ontology.ComplementClass;
import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.EnumeratedClass;
import com.hp.hpl.jena.ontology.HasValueRestriction;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.IntersectionClass;
import com.hp.hpl.jena.ontology.MaxCardinalityRestriction;
import com.hp.hpl.jena.ontology.MinCardinalityRestriction;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.ontology.Restriction;
import com.hp.hpl.jena.ontology.SomeValuesFromRestriction;
import com.hp.hpl.jena.ontology.UnionClass;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFList;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.RDFWriter;
import com.hp.hpl.jena.rdf.model.ResourceFactory;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.OWL2;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;
import com.hp.hpl.jena.vocabulary.XSD;

public class JenaBasedSadlModelProcessor extends SadlModelProcessor {
	private static final String SYNTHETIC_FROM_TEST = "__synthetic";

	private static final Logger logger = LoggerFactory.getLogger(JenaBasedSadlModelProcessor.class);

    public final static String XSDNS = XSD.getURI();

    public final static Property xsdProperty( String local )
        { return ResourceFactory.createProperty( XSDNS + local ); }

	private Resource currentResource;
	protected OntModel theJenaModel;
	protected OntModelSpec spec;
//	protected ISadlServer kServer = null;
	
	protected enum AnnType {ALIAS, NOTE}	

	public enum OPERATORS_RETURNING_BOOLEAN {contains, unique, is, gt, ge, lt, le, and, or, not, was, hasBeen}
	
	public enum BOOLEAN_LITERAL_TEST {BOOLEAN_TRUE, BOOLEAN_FALSE, NOT_BOOLEAN, NOT_BOOLEAN_NEGATED}

	private int vNum = 0;	// used to create unique variables
	private List<String> userDefinedVariables = new ArrayList<String>();
	
	@Inject
	public DeclarationExtensions declarationExtensions;
	
	protected String modelName;
	protected String modelAlias;
	protected String modelNamespace;
	private OntDocumentManager jenaDocumentMgr;
	protected IConfigurationManagerForIDE configMgr;
	
	private OntModel sadlBaseModel = null;
	private static final String SADL_BASE_MODEL_FILENAME = "SadlBaseModel";
	private static final String SADL_BASE_MODEL_URI = "http://sadl.org/sadlbasemodel";
	private static final String SADL_BASE_MODEL_PREFIX = "sadlbasemodel";
	private static final String SADL_BASE_MODEL_EQUATION_URI = SADL_BASE_MODEL_URI + "#Equation";
	private static final String SADL_BASE_MODEL_EXTERNAL_URI = SADL_BASE_MODEL_URI + "#ExternalEquation";
	private static final String SADL_BASE_MODEL_EQ_EXPRESSION_URI = SADL_BASE_MODEL_URI + "#expression";
	private static final String SADL_BASE_MODEL_EXTERNALURI_URI = SADL_BASE_MODEL_URI + "#externalURI";

	private boolean importSadlListModel = false;
	private OntModel sadlListModel = null;
	private static final String SADL_LIST_MODEL_FILENAME = "SadlListModel";
	private static final String SADL_LIST_MODEL_URI = "http://sadl.org/sadllistmodel";
	private static final String SADL_LIST_MODEL_PREFIX = "sadllistmodel";
	public static final String SADL_LIST_MODEL_LIST_URI = SADL_LIST_MODEL_URI + "#List";
	public static final String SADL_LIST_MODEL_FIRST_URI = SADL_LIST_MODEL_URI + "#first";
	private static final String SADL_LIST_MODEL_REST_URI = SADL_LIST_MODEL_URI + "#rest";
	private static final String SADL_LIST_MODEL_LENGTH_RESTRICTION_URI = SADL_LIST_MODEL_URI + "#lengthRestriction";
	private static final String SADL_LIST_MODEL_MINLENGTH_RESTRICTION_URI = SADL_LIST_MODEL_URI + "#lengthMinRestriction";
	private static final String SADL_LIST_MODEL_MAXLENGTH_RESTRICTION_URI = SADL_LIST_MODEL_URI + "#lengthMaxRestriction";
	
	private static final String LIST_RANGE_ANNOTATION_PROPERTY = "http://sadl.org/range/annotation/listtype";
	
	private OntModel sadlImplicitModel = null;
	public static final String SADL_IMPLICIT_MODEL_FOLDER = "ImplicitModel";
	public static final String SADL_IMPLICIT_MODEL_FILENAME = "SadlImplicitModel.sadl";	// this is a .sadl file and for now will be imported explicitly
	private static final String OWL_IMPLICIT_MODEL_FILENAME = "SadlImplicitModel.owl";
	private static final String SADL_IMPLICIT_MODEL_URI = "http://sadl.org/sadlimplicitmodel";
	private static final String SADL_IMPLICIT_MODEL_PREFIX = "sadlimplicitmodel";
	private static final String SADL_IMPLICIT_MODEL_EVENT_URI = SADL_IMPLICIT_MODEL_URI + "#Event";
	private static final String SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI = SADL_IMPLICIT_MODEL_URI + "#UnittedQuantity";
	private static final String SADL_IMPLICIT_MODEL_UNIT_URI = SADL_IMPLICIT_MODEL_URI + "#unit";
	private static final String SADL_IMPLICIT_MODEL_VALUE_URI = SADL_IMPLICIT_MODEL_URI + "#value";
	public static final String SADL_IMPLICIT_MODEL_IMPLIED_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#impliedProperty";
	private static final String SADL_IMPLICIT_MODEL_QUERY_STRING_URI = SADL_IMPLICIT_MODEL_URI + "#queryString";
	
	private static final String SADL_RULE_PATTERN_URI = "http://sadl.org/rule/patterns";
	private static final String SADL_RULE_PATTERN_PREFIX = "";
	private static final String SADL_RULE_PATTERN_DATA_URI = "http://sadl.org/rule/patterns/data";
	private static final String SADL_RULE_PATTERN_DATA_PREFIX = "";
	private static final String SADL_SERIVCES_CONFIGURATION_CONCEPTS_URI = "http://com.ge.research.sadl/sadlserver/Services";
	private static final String SADL_SERIVCES_CONFIGURATION_CONCEPTS_PREFIX = "";
	public static final String SADL_SERIVCES_CONFIGURATION_URI = "http://com.ge.research.sadl/sadlserver/ServicesConfig";
	private static final String SADL_SERIVCES_CONFIGURATION_PREFIX = "";


	private JenaBasedSadlModelValidator modelValidator = null;
	protected ValidationAcceptor issueAcceptor = null;
	protected CancelIndicator cancelIndicator = null;

	protected List<String> importsInOrderOfAppearance = null;	// an ordered set of import URIs, ordered by appearance in file.
	private List<Rule> rules = null;
	private List<Equation> equations = null;
	private List<SadlCommand> sadlCommands = null;
	
	int modelErrorCount = 0;
	int modelWarningCount = 0;
	int modelInfoCount = 0;

	private IntermediateFormTranslator intermediateFormTranslator = null;

	protected boolean generationInProgress = false;

	
	public static String[] reservedFileNames = {"SadlBaseModel.sadl", "SadlListModel.sadl", 
			"RulePatterns.sadl", "RulePatternsData.sadl", "SadlServicesConfigurationConcepts.sadl", "ServicesConfig.sadl"};
	public static String[] reservedModelURIs = {SADL_BASE_MODEL_URI,SADL_LIST_MODEL_URI,
			SADL_RULE_PATTERN_URI, SADL_RULE_PATTERN_DATA_URI,SADL_SERIVCES_CONFIGURATION_CONCEPTS_URI,SADL_SERIVCES_CONFIGURATION_URI};
	public static String[] reservedPrefixes = {SADL_BASE_MODEL_PREFIX,SADL_LIST_MODEL_PREFIX};

	public JenaBasedSadlModelProcessor() {
		logger.debug("New " + this.getClass().getCanonicalName() + "' created");
	}
	/**
	 * For TESTING
	 * @return
	 */
	public OntModel getTheJenaModel() {
		return theJenaModel;
	}
	
	protected void setCurrentResource(Resource currentResource) {
		this.currentResource = currentResource;
	}
	
	public Resource getCurrentResource() {
		return currentResource;
	}

	@SuppressWarnings("restriction")
	@Override
	public void onGenerate(Resource resource, IFileSystemAccess2 fsa, ProcessorContext context) {
		generationInProgress  = true;
		setProcessorContext(context);
		List<String[]> newMappings = new ArrayList<String[]>();
    	logger.debug("onGenerate called for Resource '" + resource.getURI() + "'");
//    	System.out.println("onGenerate called for Resource '" + resource.getURI() + "'");
		// save the model
		if (getTheJenaModel() == null) {
			OntModel m = OntModelProvider.find(resource);
			if (m == null) {
				onValidate(resource, null, CheckMode.FAST_ONLY, context);
			}
			else {
				theJenaModel = m;
				setModelName(OntModelProvider.getModelName(resource));
			}
		}
		if (fsa !=null) {
			if (isReservedName(resource)) {
				addError("'" + resource.getURI().lastSegment() + "' is a reserved name. Model cannot be saved. Please choose a different name.", resource.getContents().get(0));
				return;
			}
			String format = getOwlModelFormat(context);

//			// Output the OWL file for the ontology model
			URI lastSeg = fsa.getURI(resource.getURI().lastSegment());
			String owlFN = lastSeg.trimFileExtension().appendFileExtension(ResourceManager.getOwlFileExtension(format)).lastSegment().toString();
			RDFWriter w = getTheJenaModel().getWriter(format);
			w.setProperty("xmlbase",getModelName());
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			w.write(getTheJenaModel().getBaseModel(), out, getModelName());
			Charset charset = Charset.forName("UTF-8"); 
			CharSequence seq = new String(out.toByteArray(), charset);
			fsa.generateFile(owlFN, seq);
			
//			// if there are equations, output them to a Prolog file
//			List<Equation> eqs = getEquations();
//			if (eqs != null) {
//				StringBuilder sb = new StringBuilder();
//				for (int i = 0; i < eqs.size(); i++) {
//					sb.append(eqs.get(i).toFullyQualifiedString());
//					sb.append("\n");
//				}
//				fsa.generateFile(lastSeg.appendFileExtension("pl").lastSegment().toString(), sb.toString());
//			}
			
			try {
				String modelFolder = getModelFolderPath(resource);
				SadlUtils su = new SadlUtils();
				String fn = SADL_BASE_MODEL_FILENAME + "." + ResourceManager.getOwlFileExtension(format);
				if (!fsa.isFile(fn)) {
					sadlBaseModel = OntModelProvider.getSadlBaseModel();
					if(sadlBaseModel != null) {
						RDFWriter w2 = sadlBaseModel.getWriter(format);
						w.setProperty("xmlbase",SADL_BASE_MODEL_URI);
						ByteArrayOutputStream out2 = new ByteArrayOutputStream();
						w2.write(sadlBaseModel.getBaseModel(), out2, SADL_BASE_MODEL_URI);
						CharSequence seq2 = new String(out2.toByteArray(), charset);
						fsa.generateFile(fn, seq2);
						String[] mapping = new String[3];
						mapping[0] = su.fileNameToFileUrl(modelFolder + "/" + fn);
						mapping[1] = SADL_BASE_MODEL_URI;
						mapping[2] = null;
						newMappings.add(mapping);
					}
				}
				fn = SADL_LIST_MODEL_FILENAME + "." + ResourceManager.getOwlFileExtension(format);
				if (!fsa.isFile(fn)) {
					sadlListModel = OntModelProvider.getSadlListModel();
					if(sadlListModel != null) {
						RDFWriter w2 = sadlListModel.getWriter(format);
						w.setProperty("xmlbase",SADL_BASE_MODEL_URI);
						ByteArrayOutputStream out2 = new ByteArrayOutputStream();
						w2.write(sadlListModel.getBaseModel(), out2, SADL_LIST_MODEL_URI);
						CharSequence seq2 = new String(out2.toByteArray(), charset);
						fsa.generateFile(fn, seq2);
						String[] mapping = new String[3];
						mapping[0] = su.fileNameToFileUrl(modelFolder + "/" + fn);
						mapping[1] = SADL_LIST_MODEL_URI;
						mapping[2] = null;
						newMappings.add(mapping);
					}
				}
				
	//			// Output the ont-policy.rdf mapping file: the mapping will have been updated already via onValidate
	//			if (!fsa.isFile(UtilsForJena.ONT_POLICY_FILENAME)) {
	//				fsa.generateFile(UtilsForJena.ONT_POLICY_FILENAME, getDefaultPolicyFileContent());
	//			}

				String[] mapping = new String[3];
				mapping[0] = su.fileNameToFileUrl(modelFolder + "/" + owlFN);
				mapping[1] = getModelName();
				mapping[2] = null;

				newMappings.add(mapping);
				
				// Output the Rules and any other knowledge structures via the specified translator
				List<Object> otherContent = OntModelProvider.getOtherContent(resource);
				if (otherContent != null) {
					for (int i = 0;  i < otherContent.size(); i++) {
						if (otherContent.get(i) instanceof List<?>) {
							equations = (List<Equation>) otherContent.get(i); 
						}
					}
				}
				List<ModelError> results = translateAndSaveModel(resource, owlFN, format, newMappings);
				if (results != null) {
					modelErrorsToOutput(resource, results);
				}
			}
			catch (Exception e) {
				
			}
		}
		generationInProgress   = false;
	   	logger.debug("onGenerate completed for Resource '" + resource.getURI() + "'");
	}
	
	private String getOwlModelFormat(ProcessorContext context) {
		String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT; // default
		if (context != null) {
			String pv = context.getPreferenceValues().getPreference(SadlPreferences.OWL_MODEL_FORMAT);
			if (pv != null && pv.length() > 0) {
				format = pv;
			}
		}
		return format;
	}
	
	private List<ModelError> translateAndSaveModel(Resource resource, String owlFN, String _repoType, List<String[]> newMappings) {
		String modelFolderPathname = getModelFolderPath(resource);
		try {
//			IConfigurationManagerForIDE configMgr = new ConfigurationManagerForIDE(modelFolderPathname , _repoType);
			if (newMappings != null) {
				getConfigMgr(resource, _repoType).addMappings(newMappings, true, "SADL");
			}
			ITranslator translator = getConfigMgr(resource, _repoType).getTranslator();
			List<ModelError> results = translator
					.translateAndSaveModel(getTheJenaModel(), getRules(),
							modelFolderPathname, getModelName(), getImportsInOrderOfAppearance(), 
							owlFN);
			if (results != null) {
				modelErrorsToOutput(resource, results);
			}
			else if (getOtherKnowledgeStructure() != null) {
				results = translator.translateAndSaveModelWithOtherStructure(getTheJenaModel(), getOtherKnowledgeStructure(), 
						modelFolderPathname, getModelName(), getImportsInOrderOfAppearance(), owlFN);
				return results;
			}
		} catch (MalformedURLException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (com.ge.research.sadl.reasoner.ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TranslationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		return null;
	}
	
	private String getModelFolderPath(Resource resource) {
		URI v = resource.getURI().trimSegments(resource.getURI().segmentCount() - 2);
		v = v.appendSegment(UtilsForJena.OWL_MODELS_FOLDER_NAME);
		String modelFolderPathname;
		if (v.isPlatform()) {
			 IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(v.toPlatformString(true)));
			 modelFolderPathname = file.getRawLocation().toPortableString();
		}
		else {
			modelFolderPathname = findModelFolderPath(resource.getURI());
			if(modelFolderPathname == null) {
				modelFolderPathname = v.toFileString();
			}
		}
		return modelFolderPathname;
	}
	
    static String findModelFolderPath(URI uri){
    	File file = new File(uri.path());
    	if(file != null){
    		if(file.isDirectory()){
    			if(file.getAbsolutePath().endsWith(UtilsForJena.OWL_MODELS_FOLDER_NAME)){
    				return file.getAbsolutePath();
    			}
    			
    			for(File child : file.listFiles()){
    				if(child.getAbsolutePath().endsWith(UtilsForJena.OWL_MODELS_FOLDER_NAME)){
    					return child.getAbsolutePath();
    				}
    			}
    			//Didn't find a project file in this directory, check parent
    			if(file.getParentFile() != null){
    				return findModelFolderPath(uri.trimSegments(1));
    			}
    		}
    		if(file.isFile() && file.getParentFile() != null){
    			return findModelFolderPath(uri.trimSegments(1));
    		}
    	}
    	
    	return null;
    }
	
	private Object getOtherKnowledgeStructure() {
		if (getEquations() != null) {
			return getEquations();
		}
		return null;
	}
	
	private void modelErrorsToOutput(Resource resource, List<ModelError> errors) {
		for (int i = 0; errors != null && i < errors.size(); i++) {
			ModelError err = errors.get(i);
			addError(err.getErrorMsg(), resource.getContents().get(0));
		}
	}
	
	/**
	 * Method to retrieve a list of the model's imports ordered according to appearance
	 * @return
	 */
	public List<String> getImportsInOrderOfAppearance() {
		return importsInOrderOfAppearance;
	}

	private void addOrderedImport(String importUri) {
		if (importsInOrderOfAppearance == null) {
			importsInOrderOfAppearance = new ArrayList<String>();
		}
		if (!importsInOrderOfAppearance.contains(importUri)) {
			importsInOrderOfAppearance.add(importUri);
		}
		
	}
	
	protected IMetricsProcessor metricsProcessor;
	
	public String getDefaultMakerSubjectUri() {
		return null;
	}

	private ProcessorContext processorContext;

    public static void refreshResource(Resource newRsrc) {
    	try {
    		URI uri = newRsrc.getURI();
    		uri = newRsrc.getResourceSet().getURIConverter().normalize(uri);
    		String scheme = uri.scheme();
    		if ("platform".equals(scheme) && uri.segmentCount() > 1 &&
    				"resource".equals(uri.segment(0)))
    		{
    			StringBuffer platformResourcePath = new StringBuffer();
    			for (int j = 1, size = uri.segmentCount() - 1; j < size; ++j)
    			{
    				platformResourcePath.append('/');
    				platformResourcePath.append(uri.segment(j));
    			}
    			IResource r = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(platformResourcePath.toString()));
    			r.refreshLocal(IResource.DEPTH_INFINITE, null);
    		}
    	}
    	catch (Throwable t) {
    		// this will happen if in test environment
    	}
	}

    @Override
	public void onValidate(Resource resource, ValidationAcceptor issueAcceptor, CheckMode mode, ProcessorContext context) {
    	logger.debug("onValidate called for Resource '" + resource.getURI() + "'");
		if (mode.shouldCheck(CheckType.EXPENSIVE)) {
			// do expensive validation, i.e. those that should only be done when 'validate' action was invoked. 
		}
		setIssueAcceptor(issueAcceptor);
		setProcessorContext(context);
		setCancelIndicator(cancelIndicator);
		if (resource.getContents().size() < 1) {
			return;
		}
		SadlModel model = (SadlModel) resource.getContents().get(0);
		String modelActualUrl =resource.getURI().lastSegment();
		if (isReservedName(resource)) {
			addError("'" + modelActualUrl + "' is a reserved name. Please choose a different name", model);
		}
		String modelName = model.getBaseUri();
		setModelName(modelName);
		setModelNamespace(assureNamespaceEndsWithHash(modelName));
		setModelAlias(model.getAlias());
		if (getModelAlias() == null) {
			setModelAlias("");
		}
		
		try {
			theJenaModel = prepareEmptyOntModel(resource);
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			addError(e1.getMessage(), model);
			return; // this is a fatal error
		}
		getTheJenaModel().setNsPrefix(getModelAlias(), getModelNamespace());
		Ontology modelOntology = getTheJenaModel().createOntology(modelName);
		logger.debug("Ontology '" + modelName + "' created");
		modelOntology.addComment("This ontology was created from a SADL file '"
				+ modelActualUrl + "' and should not be directly edited.", "en");
		
		String modelVersion = model.getVersion();
		if (modelVersion != null) {
			modelOntology.addVersionInfo(modelVersion);
		}

		EList<SadlAnnotation> anns = model.getAnnotations();
		addAnnotationsToResource(modelOntology, anns);
		
		if (!resource.getURI().lastSegment().equals(SADL_IMPLICIT_MODEL_FILENAME)) {
			OntModelProvider.registerResource(resource);
			try {
				sadlBaseModel = getOntModelFromString(resource, getSadlBaseModel());
				OntModelProvider.setSadlBaseModel(sadlBaseModel);
				addImportToJenaModel(getModelName(), SADL_BASE_MODEL_URI, sadlBaseModel);
				String implfn = checkImplicitSadlModelExistence(resource, context);
				if (implfn != null) {
					Resource imrsrc = resource.getResourceSet().getResource(URI.createFileURI(implfn), true);
					if (sadlImplicitModel == null) {
						if (imrsrc instanceof XtextResource) {
							sadlImplicitModel = OntModelProvider.find((XtextResource)imrsrc);
						}
						else if (imrsrc instanceof ExternalEmfResource) {
							sadlImplicitModel = ((ExternalEmfResource) imrsrc).getJenaModel();
						}
						if (sadlImplicitModel == null) {
							if (imrsrc instanceof XtextResource) {
								((XtextResource) imrsrc).getResourceServiceProvider().getResourceValidator().validate(imrsrc, CheckMode.FAST_ONLY, cancelIndicator);
								sadlImplicitModel = OntModelProvider.find(imrsrc);
								OntModelProvider.attach(imrsrc, sadlImplicitModel, SADL_IMPLICIT_MODEL_URI);
							}
							else {
								IConfigurationManagerForIDE cm = getConfigMgr(resource, getOwlModelFormat(context));
								if (cm.getModelGetter() == null) {
									cm.setModelGetter(new SadlJenaModelGetter(cm, null));
								}
								cm.getModelGetter().getOntModel(SADL_IMPLICIT_MODEL_URI, ResourceManager.getProjectUri(resource).appendSegment(ResourceManager.OWLDIR).appendFragment(OWL_IMPLICIT_MODEL_FILENAME).toFileString(), getOwlModelFormat(context));
							}
						}
						if (sadlImplicitModel != null) {
							addImportToJenaModel(getModelName(), SADL_IMPLICIT_MODEL_URI, sadlImplicitModel);
						}
					}
				}
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (ConfigurationException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (URISyntaxException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (JenaProcessorException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		}
		
		EList<SadlImport> implist = model.getImports();
		Iterator<SadlImport> impitr = implist.iterator();
		if (impitr.hasNext()) {
			while (impitr.hasNext()) {
				SadlImport simport = impitr.next();
				SadlModel importedResource = simport.getImportedResource();
				if (importedResource != null) {
					URI importingResourceUri = resource.getURI();
					String importUri = importedResource.getBaseUri();
					String importPrefix = simport.getAlias();
					Resource eResource = importedResource.eResource();
					if (eResource instanceof XtextResource) {
						XtextResource xtrsrc = (XtextResource) eResource;
						URI importedResourceUri = xtrsrc.getURI();
						OntModel importedOntModel = OntModelProvider.find(xtrsrc);
						if (importedOntModel == null) {
							if (OntModelProvider.checkForCircularImport(eResource)) {
								addError("Import of '" + importedResourceUri.toString() + "' appears to be part of a circular set of imports.", simport);
							}
							else {
					        	logger.debug("JenaBasedSadlModelProcessor encountered null OntModel for Resource '" + importedResourceUri + "' while processing Resource '" + importingResourceUri + "'");
								xtrsrc.getResourceServiceProvider().getResourceValidator().validate(xtrsrc, CheckMode.FAST_ONLY, cancelIndicator);
						        importedOntModel = OntModelProvider.find(xtrsrc);
						        if (OntModelProvider.hasCircularImport(resource)) {
									addError("Import of '" + importedResourceUri.toString() + "' appears to be part of a circular set of imports.", simport);
						        }
							}
						}
						if (importedOntModel == null) {
				        	logger.debug("JenaBasedSadlModelProcessor failed to resolve null OntModel for Resource '" + importedResourceUri + "' while processing Resource '" + importingResourceUri + "'");
				    		addError("Imported model has null OntModel", simport);
						}
						else {
							addImportToJenaModel(modelName, importUri, importedOntModel);							
				    	}
					} else if (eResource instanceof ExternalEmfResource) {
						ExternalEmfResource emfResource = (ExternalEmfResource) eResource;
						addImportToJenaModel(modelName, importUri, emfResource.getJenaModel());
					}
					else {
						addError("Import resolved to a null XtextResource", simport);
						return;
					}
				}
				else {
					addError("Import failed to provide an imported Resource" , simport);
				}
			}
		}

		boolean enableMetricsCollection = false;
		String enableMetrics = context.getPreferenceValues().getPreference(SadlPreferences.ENABLE_METRICS_COLLECTION);
		if (enableMetrics != null && enableMetrics.length() > 0) {
			enableMetricsCollection = Boolean.parseBoolean(enableMetrics);
		}
		try {
			if (enableMetricsCollection) {
				metricsProcessor = new MetricsProcessor(modelName, resource, getConfigMgr(resource, getOwlModelFormat(context)), this);
			}
		} catch (JenaProcessorException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		boolean disableTypeChecking = true;
		String typechecking = context.getPreferenceValues().getPreference(SadlPreferences.DISABLE_TYPE_CHECKING);
		if (typechecking != null) {
			disableTypeChecking = Boolean.parseBoolean(typechecking);
		}
		// create validator for expressions
		if (!disableTypeChecking) {
			modelValidator = new JenaBasedSadlModelValidator(issueAcceptor, theJenaModel, declarationExtensions, metricsProcessor);
		}
		
		// process rest of parse tree
		List<SadlModelElement> elements = model.getElements();
		if (elements != null) {
			Iterator<SadlModelElement> elitr = elements.iterator();
			while (elitr.hasNext()) {
				// check for cancelation from time to time
				if (context.getCancelIndicator().isCanceled()) {
					throw new OperationCanceledException();
				}
				SadlModelElement element = elitr.next();
				try {
					if (element instanceof SadlClassOrPropertyDeclaration) {
						processSadlClassOrPropertyDeclaration((SadlClassOrPropertyDeclaration) element);	
					}
					else if (element instanceof SadlProperty) {
						processSadlProperty(null, (SadlProperty) element);
					}
					else if (element instanceof SadlNecessaryAndSufficient) {
						processSadlNecessaryAndSufficient((SadlNecessaryAndSufficient)element);
					}
					else if (element instanceof SadlDifferentFrom) {
						processSadlDifferentFrom((SadlDifferentFrom)element);
					}
					else if (element instanceof SadlInstance) {
						processSadlInstance((SadlInstance) element);
					}
					else if (element instanceof SadlDisjointClasses) {
						processSadlDisjointClasses((SadlDisjointClasses)element);
					}
					else if (element instanceof SadlSameAs) {
						processSadlSameAs((SadlSameAs)element);
					}
					else if (element instanceof RuleStatement) {
						processStatement((RuleStatement)element);
					}
					else if (element instanceof EquationStatement) {
						processStatement((EquationStatement)element);
					}
					else if (element instanceof PrintStatement) {
						processStatement((PrintStatement)element);
					}
					else if (element instanceof ReadStatement) {
						processStatement((ReadStatement)element);
					}
					else if (element instanceof StartWriteStatement) {
						processStatement((StartWriteStatement)element);
					}
					else if (element instanceof EndWriteStatement) {
						processStatement((EndWriteStatement)element);
					}
					else if (element instanceof ExplainStatement) {
						processStatement((ExplainStatement)element);
					}
					else if (element instanceof QueryStatement) {
						processStatement((QueryStatement)element);
					}
					else if (element instanceof SadlResource) {
						processStatement((SadlResource)element);
					}
					else if (element instanceof TestStatement) {
						processStatement((TestStatement)element);
					}
					else if (element instanceof ExternalEquationStatement) {
						processStatement((ExternalEquationStatement)element);
					}
					else if (element instanceof ExpressionStatement) {
						processExpression(((ExpressionStatement)element).getExpr());
					}
					else {
						throw new JenaProcessorException("onValidate for element of type '" + element.getClass().getCanonicalName() + "' not implemented");
					}
				}
				catch (JenaProcessorException e) {
					addError(e.getMessage(), element);
				} catch (InvalidNameException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (InvalidTypeException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (TranslationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
    	logger.debug("onValidate completed for Resource '" + resource.getURI() + "'");
		OntModelProvider.attach(model.eResource(), getTheJenaModel(), getModelName());
		if (issueAcceptor != null) {
			try {
				if (!resource.getURI().lastSegment().equals("SadlImplicitModel.sadl")) {
//					System.out.println("Metrics for '" + resource.getURI().lastSegment() + "':");
					if (issueAcceptor.getErrorCount() > 0) {
						String msg = "    Model totals: " + countPlusLabel(issueAcceptor.getErrorCount(), "error") + ", " + 
								countPlusLabel(issueAcceptor.getWarningCount(), "warning") + ", " + 
								countPlusLabel(issueAcceptor.getInfoCount(), "info");
//						System.out.flush();
						System.err.println("No OWL model output generated for '" + resource.getURI() + "'.");
						System.err.println(msg);
						System.err.flush();
					}
//					else {
//						System.out.println(msg);
//					}
					if (metricsProcessor != null) {
						metricsProcessor.saveMetrics(ConfigurationManager.RDF_XML_ABBREV_FORMAT);
					}
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalStateException e) {
				// this is OK--will happen during standalone testing
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	private void addAnnotationsToResource(OntResource modelOntology, EList<SadlAnnotation> anns) {
		Iterator<SadlAnnotation> iter = anns.iterator();
		while (iter.hasNext()) {
			SadlAnnotation ann = iter.next();
			String anntype = ann.getType();
			EList<String> annContents = ann.getContents();
			Iterator<String> anniter = annContents.iterator();
			while (anniter.hasNext()) {
				String annContent = anniter.next();
				if (anntype.equalsIgnoreCase(AnnType.ALIAS.toString())) {
					modelOntology.addLabel(annContent, "en");
				}
				else if (anntype.equalsIgnoreCase(AnnType.NOTE.toString())) {
					modelOntology.addComment(annContent, "en");
				}
			}
		}
	}
	
	private OntModel prepareEmptyOntModel(Resource resource) throws ConfigurationException {
		try {
			IConfigurationManagerForIDE cm = getConfigMgr(resource, getOwlModelFormat(getProcessorContext()));
			OntDocumentManager owlDocMgr = cm.getJenaDocumentMgr();
			OntModelSpec spec = new OntModelSpec(OntModelSpec.OWL_MEM);
			setSpec(spec);
			String modelFolderPathname = getModelFolderPath(resource);
			if (modelFolderPathname != null && !modelFolderPathname.startsWith(SYNTHETIC_FROM_TEST)) {
				spec.setImportModelGetter(new SadlJenaModelGetterPutter(spec, modelFolderPathname));
			}
			if (owlDocMgr != null) {
				spec.setDocumentManager(owlDocMgr);
				owlDocMgr.setProcessImports(true);
			}
			return ModelFactory.createOntologyModel(spec);
		}
		catch (ConfigurationException e) {
			e.printStackTrace();
			throw e;
		}
		catch (Exception e) {
			e.printStackTrace();
			throw new ConfigurationException(e.getMessage(), e);
		}
	}
	
	private void setProcessorContext(ProcessorContext ctx) {
		processorContext = ctx;
	}
	
	private ProcessorContext getProcessorContext() {
		return processorContext;
	}
	
	private String countPlusLabel(int count, String label) {
		if (count == 0 || count > 1) {
			label = label + "s";
		}
		return "" + count + " " + label;
	}

	private void addImportToJenaModel(String modelName, String importUri, Model importedOntModel) {
		Ontology modelOntology = getTheJenaModel().createOntology(modelName);
		com.hp.hpl.jena.rdf.model.Resource importedOntology = getTheJenaModel().createResource(importUri);
		getTheJenaModel().addSubModel(importedOntModel);
		modelOntology.addImport(importedOntology);
		addOrderedImport(importUri);
	}

//	/**
//	 * Method to determine the OWL model URI and actual URL for each import and add that, along with the prefix,
//	 * to the Jena OntDocumentManager so that it will be loaded when we do a Jena loadImports
//	 * @param sadlImports -- the list of imports to 
//	 * @return 
//	 */
//	private List<Resource> getIndirectImportResources(SadlModel model) {
//		EList<SadlImport> implist = model.getImports();
//		Iterator<SadlImport> impitr = implist.iterator();
//		if (impitr.hasNext()) {
//			List<Resource> importedResources = new ArrayList<Resource>();
//			while (impitr.hasNext()) {
//				SadlImport simport = impitr.next();
//				SadlModel importedModel = simport.getImportedResource();
//				if (importedModel != null) {
//					String importUri = importedModel.getBaseUri();
//					String importPrefix = simport.getAlias();
//		    		if (importPrefix != null) {
//		    			getTheJenaModel().setNsPrefix(importPrefix, assureNamespaceEndsWithHash(importUri));
//		    		}
//			    	importedResources.add(importedModel.eResource());
//				}
//				else {
//					addError("Unable to obtain import URI", simport);
//				}
//				List<Resource> moreImports = getIndirectImportResources(importedModel);
//				if (moreImports != null) {
//					importedResources.addAll(moreImports);
//				}
//			}
//			return importedResources;
//		}
//		return null;
//	}

	private boolean isReservedName(Resource resource) {
		String nm = resource.getURI().lastSegment();
		for (String rnm:reservedFileNames) {
			if (rnm.equals(nm)) {
				return true;
			}
		}
		return false;
	}
	
	private void processStatement(ExplainStatement element) throws JenaProcessorException {
		throw new JenaProcessorException("Processing for " + element.getClass().getCanonicalName() + " not yet implemented");		
	}
	
	private void processStatement(EndWriteStatement element) throws JenaProcessorException {
		throw new JenaProcessorException("Processing for " + element.getClass().getCanonicalName() + " not yet implemented");		
	}
	
	private void processStatement(SadlResource element) throws TranslationException {
		Object srobj = processExpression(element);
		int i = 0;
	}
	
	private void processStatement(StartWriteStatement element) throws JenaProcessorException {
		throw new JenaProcessorException("Processing for " + element.getClass().getCanonicalName() + " not yet implemented");		
	}
	
	public Test[] processStatement(TestStatement element) throws JenaProcessorException {
		throw new JenaProcessorException("Processing for " + element.getClass().getCanonicalName() + " not yet implemented");		
	}
	
	private void processStatement(ReadStatement element) throws JenaProcessorException {
		throw new JenaProcessorException("Processing for " + element.getClass().getCanonicalName() + " not yet implemented");		
	}
	
	private void processStatement(PrintStatement element) throws JenaProcessorException {
		throw new JenaProcessorException("Processing for " + element.getClass().getCanonicalName() + " not yet implemented");		
	}
	
	public Query processStatement(QueryStatement element) throws JenaProcessorException, InvalidNameException, InvalidTypeException, TranslationException {
		Expression qexpr = element.getExpr();
		if (qexpr != null) {
			Object qobj = processExpression(qexpr);
			Query query;
			if (qobj instanceof Query) {
				query = (Query) qobj;
			}
			else {
				query = processQuery(qobj);
			}
			if (element.getName() != null) {
				query.setFqName(getModelNamespace() + element.getName());
			}
			addSadlCommand(query);
			return query;
		}
		return null;
	}
	public Query processExpression(SelectExpression expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		Query query = new Query();
		query.setContext(expr);
//		setTranslationTarget(query);
//		if (parent != null) {
//			getIfTranslator().setEncapsulatingTarget(parent);
//		}
		
		// get variables and other information from the SelectExpression
		EList<SadlResource> varList = null;
		if (expr instanceof SelectExpression) {
			query.setKeyword("select");
			if (((SelectExpression)expr).isDistinct()) {
				query.setDistinct(true);
			}
			varList = ((SelectExpression)expr).getSelectFrom();
			if (varList != null) {
				List<String> names = new ArrayList<String>();
				for (int i = 0; i < varList.size(); i++) {
					Object var = translate(varList.get(i));
					if (!(var instanceof VariableNode)) {
						throw new InvalidNameException("'" + var.toString() + "' isn't a variable as expected in query select names.");
					}
					names.add(((VariableNode)var).getName());
				}
				query.setVariables(names);
			}
		}
		else if (expr instanceof ConstructExpression) {
			query.setKeyword("construct");
			List<String> names = new ArrayList<String>();
			names.add(translate(((ConstructExpression)expr).getSubj()).toString());
			names.add(translate(((ConstructExpression)expr).getPred()).toString());
			names.add(translate(((ConstructExpression)expr).getObj()).toString());
			query.setVariables(names);
		}
		else if (expr instanceof AskExpression) {
			query.setKeyword("ask");
		}

		// Translate the query to the resulting intermediate form.
		Object pattern = translate(expr.getWhereExpression());
		
		Object expandedPattern = null;
		try {
			expandedPattern = getIfTranslator().expandProxyNodes(pattern, false, true);
		} catch (InvalidNameException e) {
			addError("Invalid name in query: " + pattern.toString(), expr);
			e.printStackTrace();
		} catch (InvalidTypeException e) {
			addError("Invalid type in query: " + pattern.toString(), expr);
			e.printStackTrace();
		} catch (TranslationException e) {
			addError("Translation error in query: " + pattern.toString(), expr);
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
		logger.debug("Ask translation: {}", query);
		return query;
	}

	private Query processQuery(Object qobj) throws JenaProcessorException {
		String qstr;
		if (qobj instanceof com.ge.research.sadl.model.gp.Literal) {
			qstr = ((com.ge.research.sadl.model.gp.Literal)qobj).getValue().toString();
		}
		else if (qobj instanceof String) {
			qstr = qobj.toString();
		}
		else {
			throw new JenaProcessorException("Unexpected query type: " + qobj.getClass().getCanonicalName());
		}
		Query q = new Query();
		q.setSparqlQueryString(qstr);
		return q;
	}
	
	public void processStatement(EquationStatement element) throws JenaProcessorException, InvalidNameException, InvalidTypeException, TranslationException {
		SadlResource nm = element.getName();
		EList<SadlParameterDeclaration> params = element.getParameter();
		SadlTypeReference rtype = element.getReturnType();
		Expression bdy = element.getBody();
		Equation eq = createEquation(nm, rtype, params, bdy);
		addEquation(element.eResource(), eq);
		Individual eqinst = getTheJenaModel().createIndividual(declarationExtensions.getConceptUri(nm), 
				getTheJenaModel().getOntClass(SADL_BASE_MODEL_EQUATION_URI));
		eqinst.addProperty(getTheJenaModel().getDatatypeProperty(SADL_BASE_MODEL_EQ_EXPRESSION_URI), 
				getTheJenaModel().createTypedLiteral(eq.toString()));
	}
	protected Equation createEquation(SadlResource nm, SadlTypeReference rtype, EList<SadlParameterDeclaration> params,
			Expression bdy)
			throws JenaProcessorException, TranslationException, InvalidNameException, InvalidTypeException {
		Equation eq = new Equation(declarationExtensions.getConcreteName(nm));
		eq.setNamespace(declarationExtensions.getConceptNamespace(nm));
		Node rtnode = sadlTypeReferenceToNode(rtype);
		eq.setReturnType(rtnode);
		if (params != null && params.size() > 0) {
			List<Node> args = new ArrayList<Node>();
			List<Node> argtypes = new ArrayList<Node>();
			for (int i = 0; i < params.size(); i++) {
				SadlParameterDeclaration param = params.get(i);
				SadlResource pr = param.getName();
				Object pn = processExpression(pr);
				args.add((Node) pn);
				SadlTypeReference prtype = param.getType();
				Node prtnode = sadlTypeReferenceToNode(prtype); 
				argtypes.add(prtnode);
			}
			eq.setArguments(args);
			eq.setArgumentTypes(argtypes);
		}
		Object bdyobj = processExpression(bdy);
		if (bdyobj instanceof List<?>) {
			eq.setBody((List<GraphPatternElement>) bdyobj);
		}
		else if (bdyobj instanceof GraphPatternElement) {
			eq.addBodyElement((GraphPatternElement)bdyobj);
		}
		logger.debug("Equation: " + eq.toFullyQualifiedString());
		return eq;
	}
	
	protected void processStatement(ExternalEquationStatement element) throws JenaProcessorException, InvalidNameException, InvalidTypeException, TranslationException {
		SadlResource nm = element.getName();
		EList<SadlParameterDeclaration> params = element.getParameter();
		SadlTypeReference rtype = element.getReturnType();
		String uri = element.getUri();
		String location = element.getLocation();
		Equation eq = createExternalEquation(nm, uri, rtype, params, location);
		addEquation(element.eResource(), eq);
		Individual eqinst = getTheJenaModel().createIndividual(declarationExtensions.getConceptUri(nm), 
				getTheJenaModel().getOntClass(SADL_BASE_MODEL_EXTERNAL_URI));
		eqinst.addProperty(getTheJenaModel().getDatatypeProperty(SADL_BASE_MODEL_EXTERNALURI_URI),
				getTheJenaModel().createTypedLiteral(uri));
	}
	
	private Equation createExternalEquation(SadlResource nm, String uri, SadlTypeReference rtype,
			EList<SadlParameterDeclaration> params, String location)
			throws JenaProcessorException, TranslationException {
		Equation eq = new Equation(declarationExtensions.getConcreteName(nm));
		eq.setNamespace(declarationExtensions.getConceptNamespace(nm));
		eq.setExternal(true);
		eq.setUri(uri);
		if (location != null) {
			eq.setLocation(location);
		}
		Node rtnode = sadlTypeReferenceToNode(rtype);
		eq.setReturnType(rtnode);
		if (params != null && params.size() > 0) {
			List<Node> args = new ArrayList<Node>();
			List<Node> argtypes = new ArrayList<Node>();
			for (int i = 0; i < params.size(); i++) {
				SadlParameterDeclaration param = params.get(i);
				SadlResource pr = param.getName();
				Object pn = processExpression(pr);
				args.add((Node) pn);
				SadlTypeReference prtype = param.getType();
				Node prtnode = sadlTypeReferenceToNode(prtype); 
				argtypes.add(prtnode);
			}
			eq.setArguments(args);
			eq.setArgumentTypes(argtypes);
		}
		
		logger.debug("External Equation: " + eq.toFullyQualifiedString());
		return eq;
	}
	
	private NamedNode sadlTypeReferenceToNode(SadlTypeReference rtype) throws JenaProcessorException {
		com.hp.hpl.jena.rdf.model.Resource rtobj = sadlTypeReferenceToResource(rtype);
		if (rtobj == null) {
			throw new JenaProcessorException("SadlTypeReference was not resolved to a model resource.");
		}
		if (rtobj.isURIResource()) {
			NamedNode rtnn = new NamedNode(((com.hp.hpl.jena.rdf.model.Resource)rtobj).getLocalName());
			rtnn.setNamespace(((com.hp.hpl.jena.rdf.model.Resource)rtobj).getNameSpace());
			return rtnn;
		}
		else {
			throw new JenaProcessorException("SadlTypeReference is not a URI resource");
		}
	}
	
	protected void addEquation(Resource resource, Equation eq) {
		if (equations == null) {
			equations = new ArrayList<Equation>();
			OntModelProvider.addOtherContent(resource, equations);
		}
		equations.add(eq);
	}
	
	public List<Equation> getEquations() {
		return equations;
	}
	
	private void processStatement(RuleStatement element) throws InvalidNameException, InvalidTypeException, TranslationException {
		String ruleName = element.getName();
		Rule rule = new Rule(ruleName);
		EList<Expression> ifs = element.getIfs();
		EList<Expression> thens = element.getThens();
		setRulePart(RulePart.PREMISE);
		for (int i = 0; ifs != null && i < ifs.size(); i++) {
			Expression expr = ifs.get(i);
			Object result = processExpression(expr);
			if (result instanceof GraphPatternElement) {
				rule.addIf((GraphPatternElement) result);
			}
			else {
				addError("If expression (" + result + ") is not a GraphPatternElement", expr);
			}
		}
		setRulePart(RulePart.CONCLUSION);
		for (int i = 0; thens != null && i < thens.size(); i++) {
			Expression expr = thens.get(i);
			Object result = processExpression(expr);
			if (result instanceof GraphPatternElement) {
				rule.addThen((GraphPatternElement) result);
			}
			else {
				addError("Then expression (" + result + ") not a GraphPatternElement", expr);
			}
		}
		getIfTranslator().setTarget(rule);
		rule = getIfTranslator().postProcessRule(rule, element);
		if (rules == null) {
			rules = new ArrayList<Rule>();
		}
		rules.add(rule);
	}
	
	protected void addSadlCommand(SadlCommand sadlCommand) {
		if (sadlCommands == null) {
			sadlCommands = new ArrayList<SadlCommand>();
		}
		sadlCommands.add(sadlCommand);
	}
	
	protected List<SadlCommand> getSadlCommands() {
		return sadlCommands;
	}

	public IntermediateFormTranslator getIfTranslator() {
		if (intermediateFormTranslator == null) {
			intermediateFormTranslator = new IntermediateFormTranslator(getTheJenaModel());
		}
		return intermediateFormTranslator;
	}
	
	@Override
	public Object translate(Expression expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		return processExpression(expr);
	}
	
	public Object processExpression(final Expression expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (expr instanceof BinaryOperation) {
			return processExpression((BinaryOperation)expr);
		}
		else if (expr instanceof BooleanLiteral) {
			return processExpression((BooleanLiteral)expr);
		}
		else if (expr instanceof Constant) {
			return processExpression((Constant)expr);
		}
		else if (expr instanceof Declaration) {
			return processExpression((Declaration)expr);
		}
		else if (expr instanceof Name) {
			return processExpression((Name)expr);
		}
		else if (expr instanceof NumberLiteral) {
			return processExpression((NumberLiteral)expr);
		}
		else if (expr instanceof PropOfSubject) {
			return processExpression((PropOfSubject)expr);
		}
		else if (expr instanceof StringLiteral) {
			return processExpression((StringLiteral)expr);
		}
		else if (expr instanceof SubjHasProp) {
			return processExpression((SubjHasProp)expr);
		}
		else if (expr instanceof SadlResource) {
			return processExpression((SadlResource)expr);
		}
		else if (expr instanceof Unit) {
			return processExpression((Unit)expr);
		}
		else if (expr instanceof UnaryExpression) {
			return processExpression((UnaryExpression)expr);
		}
		else if (expr instanceof Sublist) {
			return processExpression((Sublist)expr);
		}
		else if (expr instanceof ValueTable) {
			return processExpression((ValueTable)expr);
		}
		else if (expr instanceof SelectExpression) {
			return processExpression((SelectExpression)expr);
		}
		else if (expr != null){
			throw new TranslationException("Unhandled rule expression type: " + expr.getClass().getCanonicalName());
		}
		return expr;
	}
	
	public Object processExpression(ValueTable expr) {
		ValueRow row = ((ValueTable)expr).getRow();
		if (row == null) {
			EList<ValueRow> rows = ((ValueTable)expr).getRows();
			if (rows == null || rows.size() == 0) {
				ValueTable vtbl = ((ValueTable)expr).getValueTable();
				return processExpression(vtbl);
			}
			return null;
		}
		return null;
	}
	
	public Object processExpression(BinaryOperation expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		//Validate BinaryOperation expression
		StringBuilder errorMessage = new StringBuilder();
		if(modelValidator != null) {
			if (!modelValidator.validate(expr, errorMessage)) {
				issueAcceptor.addError(errorMessage.toString(), expr);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
			}
			else {
				OntModelProvider.addImpliedProperties(expr.eResource(), modelValidator.getImpliedPropertiesUsed());
				// TODO must add implied properties to rules, tests, etc.
			}
		}
		
		String op = expr.getOp();
		BuiltinType optype = BuiltinType.getType(op);
		
		Expression lexpr = expr.getLeft();
		Object lobj;
		if (lexpr != null) {
			lobj = translate(lexpr);			
		}
		else {
			throw new TranslationException("Left side of '" + op + "' is null (Resource '" + expr.eResource().getURI() + "')");
		}
		Expression rexpr = expr.getRight();
		Object robj;
		if (rexpr != null) {
			robj = translate(rexpr);
		}
		else {
			throw new TranslationException("Right side of '" + op + "' is null (Resource '" + expr.eResource().getURI() + "')");
		}
				
		if (optype == BuiltinType.Equal || optype == BuiltinType.NotEqual) {
			// If we're doing an assignment, we can simplify the pattern.
			Node assignedNode = null;
			Object pattern = null;
			if (rexpr instanceof Declaration) {
				if (lobj instanceof Node && robj instanceof Node) {
					TripleElement trel = new TripleElement((Node)lobj, new RDFTypeNode(), (Node)robj);
					trel.setSourceType(TripleSourceType.ITC);
					return trel;
				}
				else {
					throw new TranslationException("Unhandled binary operation condition: left and right are not both nodes.");
				}
			}
			if (lobj instanceof NamedNode && !(lobj instanceof VariableNode) && hasCommonVariableSubject(robj)) {
				TripleElement trel = (TripleElement)robj;
				while (trel != null) {
					trel.setSubject((Node) lobj);
					trel = (TripleElement) trel.getNext();
				}
				return robj;
			}
			if (getRulePart().equals(RulePart.CONCLUSION) &&
					(lobj instanceof TripleElement || (lobj instanceof com.ge.research.sadl.model.gp.Literal && isSparqlQuery(((com.ge.research.sadl.model.gp.Literal)lobj).toString())))
					) {
				if (robj instanceof com.ge.research.sadl.model.gp.Literal) {
					if (((TripleElement)lobj).getObject() == null) {
						((TripleElement)lobj).setObject((com.ge.research.sadl.model.gp.Literal)robj);
						return lobj;
					}
					else {
						addError("Unhandled rule conclusion construct", expr);
					}
				}
				else if (robj instanceof BuiltinElement) {
					if (isModifiedTriple(((BuiltinElement)robj).getFuncType())) {
						assignedNode = ((BuiltinElement)robj).getArguments().get(0);
						optype = ((BuiltinElement)robj).getFuncType();
						pattern = lobj;
					}
					else if (isComparisonBuiltin(((BuiltinElement)robj).getFuncName())) {
						if ( ((BuiltinElement)robj).getArguments().get(0) instanceof com.ge.research.sadl.model.gp.Literal) {
							((TripleElement)lobj).setObject(nodeCheck(robj));
							return lobj;
						}
						else {
							return createBinaryBuiltin(rexpr, ((BuiltinElement)robj).getFuncName(), lobj, ((BuiltinElement)robj).getArguments().get(0));
						}
					}
				}
				else {
					addError("Unhandled assignment construct in rule conclusion", expr);
				}
			}
			else if (lobj instanceof Node && robj instanceof TripleElement) {
				assignedNode = validateNode((Node) lobj);
				pattern = (TripleElement) robj;
			}
			else if (robj instanceof Node && lobj instanceof TripleElement) {
				assignedNode = validateNode((Node) robj);
				pattern = (TripleElement) lobj;
			}
			if (assignedNode != null && pattern != null) {
				// We're expressing the type of a named thing.
				if (pattern instanceof TripleElement && ((TripleElement)pattern).getSubject() == null) {
					if (isModifiedTripleViaBuitin(robj)) {
						optype = ((BuiltinElement)((TripleElement)pattern).getNext()).getFuncType();	
						((TripleElement)pattern).setNext(null);
					}
					((TripleElement)pattern).setSubject(assignedNode);
					if (optype != BuiltinType.Equal) {
						((TripleElement)pattern).setType(getTripleModifierType(optype));
					}
				}
				else if (pattern instanceof TripleElement && ((TripleElement)pattern).getObject() == null && 
						(((TripleElement)pattern).getSourceType().equals(TripleSourceType.PSnewV) 
								|| ((TripleElement)pattern).getSourceType().equals(TripleSourceType.PSV))) {
					if (isModifiedTripleViaBuitin(robj)) {
						optype = ((BuiltinElement)((TripleElement)pattern).getNext()).getFuncType();	
						((TripleElement)pattern).setNext(null);
					}
					((TripleElement)pattern).setObject(assignedNode);
					if (optype != BuiltinType.Equal) {
						((TripleElement)pattern).setType(getTripleModifierType(optype));
					}
				}
				else if (pattern instanceof TripleElement && ((TripleElement)pattern).getSourceType().equals(TripleSourceType.SPV)
						&& assignedNode instanceof NamedNode && getProxyWithNullSubject(((TripleElement)pattern)) != null) {
					TripleElement proxyFor = getProxyWithNullSubject(((TripleElement)pattern));
					assignNullSubjectInProxies(((TripleElement)pattern), proxyFor, assignedNode);
					if (optype != BuiltinType.Equal) {
						proxyFor.setType(getTripleModifierType(optype));
					}
				}
				else if (isModifiedTriple(optype) || 
						(optype.equals(BuiltinType.Equal) && pattern instanceof TripleElement && 
								(((TripleElement)pattern).getObject() == null || 
										((TripleElement)pattern).getObject() instanceof NamedNode ||
										((TripleElement)pattern).getObject() instanceof com.ge.research.sadl.model.gp.Literal))){
					if (pattern instanceof TripleElement && isModifiedTripleViaBuitin(robj)) {
						optype = ((BuiltinElement)((TripleElement)pattern).getNext()).getFuncType();
						((TripleElement)pattern).setObject(assignedNode);
						((TripleElement)pattern).setNext(null);
						((TripleElement)pattern).setType(getTripleModifierType(optype));
					}
					else if (isComparisonViaBuiltin(robj, lobj)) {
						BuiltinElement be = (BuiltinElement)((TripleElement)robj).getNext();
						be.addMissingArgument((Node) lobj);
						return pattern;
					}
					else if (pattern instanceof TripleElement){
						TripleElement lastPattern = (TripleElement)pattern;
						// this while may need additional conditions to narrow application to nested triples?
						while (lastPattern.getNext() != null && lastPattern instanceof TripleElement) {
							lastPattern = (TripleElement) lastPattern.getNext();
						}
						if (getEncapsulatingTarget() instanceof Test) {
							((Test)getEncapsulatingTarget()).setRhs(assignedNode);
							((Test)getEncapsulatingTarget()).setCompName(optype);
						}
						else if (getEncapsulatingTarget() instanceof Query && getTarget() instanceof Test) {
							((Test)getTarget()).setRhs(getEncapsulatingTarget());
							((Test)getTarget()).setLhs(assignedNode);
							((Test)getTarget()).setCompName(optype);
						}
						else if (getTarget() instanceof Test && assignedNode != null) {
							((Test)getTarget()).setLhs(pattern);
							((Test)getTarget()).setRhs(assignedNode);
							((Test)getTarget()).setCompName(optype);
							((TripleElement) pattern).setType(TripleModifierType.None);
							optype = BuiltinType.Equal;
						}
						else {
							lastPattern.setObject(assignedNode);
						}
						if (!optype.equals(BuiltinType.Equal)) {
							((TripleElement)pattern).setType(getTripleModifierType(optype));
						}
					}
					else {
						if (getTarget() instanceof Test) {
							((Test)getTarget()).setLhs(lobj);
							((Test)getTarget()).setRhs(assignedNode);
							((Test)getTarget()).setCompName(optype);
						}
					}
				}
				else if (getEncapsulatingTarget() instanceof Test) {
					((Test)getEncapsulatingTarget()).setRhs(assignedNode);
					((Test)getEncapsulatingTarget()).setCompName(optype);
				}
				else if (getTarget() instanceof Rule && pattern instanceof TripleElement && ((TripleElement)pattern).getSourceType().equals(TripleSourceType.ITC) && 
						((TripleElement)pattern).getSubject() instanceof VariableNode && assignedNode instanceof VariableNode) {
					// in a rule of this type we just want to replace the pivot node variable
					doVariableSubstitution(((TripleElement)pattern), (VariableNode)((TripleElement)pattern).getSubject(), (VariableNode)assignedNode);
				}
				return pattern;
			}
			BuiltinElement bin = null;
			boolean binOnRight = false;
			Object retObj = null;
			if (lobj instanceof Node && robj instanceof BuiltinElement) {
				assignedNode = validateNode((Node)lobj);
				bin = (BuiltinElement)robj;
				retObj = robj;
				binOnRight = true;
			}
			else if (robj instanceof Node && lobj instanceof BuiltinElement) {
				assignedNode = validateNode((Node)robj);
				bin = (BuiltinElement)lobj;
				retObj = lobj;
				binOnRight = false;
			}
			if (bin != null && assignedNode != null) {
				if ((assignedNode instanceof VariableNode ||
					(assignedNode instanceof NamedNode && ((NamedNode)assignedNode).getNodeType().equals(NodeType.VariableNode)))) {
					while (bin.getNext() instanceof BuiltinElement) {
						bin = (BuiltinElement) bin.getNext();
					}
					if (bin.isCreatedFromInterval()) {
						bin.addArgument(0, assignedNode);
					}
					else {
						bin.addArgument(assignedNode);
					}
					return retObj;
				}
				else if (assignedNode instanceof Node && isComparisonBuiltin(bin.getFuncName())) {
					// this is a comparison with an extra "is"
					if (bin.getArguments().size() == 1) {
						if (bin.isCreatedFromInterval() || binOnRight) {
							bin.addArgument(0, assignedNode);
						}
						else {
							bin.addArgument(assignedNode);
						}
						return bin;
					}
				}
			}
			// We're describing a thing with a graph pattern.
			Set<VariableNode> vars = pattern instanceof TripleElement ? getSelectVariables(((TripleElement)pattern)) : null; 
			if (vars != null && vars.size() == 1) {
				// Find where the unbound variable occurred in the pattern
				// and replace each place with the assigned node.
				VariableNode var = vars.iterator().next();
				GraphPatternElement gpe = ((TripleElement)pattern);
				while (gpe instanceof TripleElement) {
					TripleElement triple = (TripleElement) gpe;
					if (var.equals(triple.getSubject())) {
						triple.setSubject(assignedNode);
					}
					if (var.equals(triple.getObject())) {
						triple.setObject(assignedNode);
					}
					gpe = gpe.getNext();
				}
				return pattern;
			}
		}
		// if we get to here we want to actually create a BuiltinElement for the BinaryOpExpression
		// However, if the type is equal ("is", "equal") and the left side is a VariableNode and the right side is a literal
		//	and the VariableNode hasn't already been bound, change from type equal to type assign.
		if (optype == BuiltinType.Equal && getTarget() instanceof Rule && lobj instanceof VariableNode && robj instanceof com.ge.research.sadl.model.gp.Literal && 
				!variableIsBound((Rule)getTarget(), null, (VariableNode)lobj)) {
			return createBinaryBuiltin(expr, "assign", robj, lobj);
		}
		return createBinaryBuiltin(expr, op, lobj, robj);
	}
	
	private Object processFunction(Name expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		EList<Expression> arglist = expr.getArglist();
		Node fnnode = processExpression(expr.getName());
		String funcname;
		if (fnnode instanceof VariableNode) {
			funcname = ((VariableNode) fnnode).getName();
		}
		else {
			funcname = fnnode.toString();
		}
		BuiltinElement builtin = new BuiltinElement();
		builtin.setFuncName(funcname);
		if (fnnode instanceof NamedNode && ((NamedNode)fnnode).getNamespace()!= null) {
			builtin.setFuncUri(fnnode.toFullyQualifiedString());
		}
		if (arglist != null && arglist.size() > 0) {
			List<Object> args = new ArrayList<Object>();
			for (int i = 0; i < arglist.size(); i++) {
				args.add(processExpression(arglist.get(i)));
			}
			if (args != null) {
				for (Object arg : args) {
					builtin.addArgument(nodeCheck(arg));
					if (arg instanceof GraphPatternElement) {
						((GraphPatternElement)arg).setEmbedded(true);
					}
				}
			}
		}
		return builtin;
	}
	
	private boolean hasCommonVariableSubject(Object robj) {
		if (robj instanceof TripleElement && 
				(((TripleElement)robj).getSubject() instanceof VariableNode && 
						(((TripleElement)robj).getSourceType().equals(TripleSourceType.SPV)) ||
						((TripleElement)robj).getSourceType().equals(TripleSourceType.ITC))) {
			VariableNode subjvar = (VariableNode) ((TripleElement)robj).getSubject();
			Object trel = robj;
			while (trel != null && trel instanceof TripleElement) {
				if (!(trel instanceof TripleElement) || 
						(((TripleElement)trel).getSubject() != null &&!(((TripleElement)trel).getSubject().equals(subjvar)))) {
					return false;
				}
				trel = ((TripleElement)trel).getNext();
			}
			if (trel == null) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the bottom triple whose subject was replaced.
	 * @param pattern
	 * @param proxyFor
	 * @param assignedNode
	 * @return
	 */
	private TripleElement assignNullSubjectInProxies(TripleElement pattern,
			TripleElement proxyFor, Node assignedNode) {
		if (pattern.getSubject() instanceof ProxyNode) {
			Object proxy = ((ProxyNode)pattern.getSubject()).getProxyFor();
			if (proxy instanceof TripleElement) {
//				((ProxyNode)pattern.getSubject()).setReplacementNode(assignedNode);
				if (((TripleElement)proxy).getSubject() == null) {
					// this is the bottom of the recursion
					((TripleElement)proxy).setSubject(assignedNode);
					return (TripleElement) proxy;
				}
				else {
					// recurse down
					TripleElement bottom = assignNullSubjectInProxies(((TripleElement)proxy), proxyFor, assignedNode);
					// make the proxy next and reassign this subject as assignedNode
					((ProxyNode)((TripleElement)proxy).getSubject()).setReplacementNode(assignedNode);
					((TripleElement)proxy).setSubject(assignedNode);
					if (bottom.getNext() == null) {
						bottom.setNext(pattern);
					}
					return bottom;
				}
			}
		}
		return null;
	}

	private TripleElement getProxyWithNullSubject(TripleElement pattern) {
		if (pattern.getSubject() instanceof ProxyNode) {
			Object proxy = ((ProxyNode)pattern.getSubject()).getProxyFor();
			if (proxy instanceof TripleElement) {
				if (((TripleElement)proxy).getSubject() == null) {
					return (TripleElement)proxy;
				}
				else {
					return getProxyWithNullSubject(((TripleElement)proxy));
				}
			}
		}
		return null;
	}

	private boolean isComparisonViaBuiltin(Object robj, Object lobj) {
		if (robj instanceof TripleElement && lobj instanceof Node &&
				((TripleElement)robj).getNext() instanceof BuiltinElement) {
			BuiltinElement be = (BuiltinElement) ((TripleElement)robj).getNext();
			if (isComparisonBuiltin(be.getFuncName()) && be.getArguments().size() == 1) {
				return true;
			}
		}
		return false;
	}

	private boolean isModifiedTripleViaBuitin(Object robj) {
		if (robj instanceof TripleElement && ((TripleElement)robj).getNext() instanceof BuiltinElement) {
			BuiltinElement be = (BuiltinElement) ((TripleElement)robj).getNext();
			if (((TripleElement)robj).getPredicate() instanceof RDFTypeNode) {
				if (isModifiedTriple(be.getFuncType())) {
					Node subj = ((TripleElement)robj).getSubject();
					Node arg = (be.getArguments() != null && be.getArguments().size() > 0) ? be.getArguments().get(0) : null;
					if (subj == null && arg == null) {
						return true;
					}
					if (subj != null && arg != null && subj.equals(arg)) {
						return true;
					}
				}
			}
			else {
				if (isModifiedTriple(be.getFuncType()) && ((TripleElement)robj).getObject().equals(be.getArguments().get(0))) {
					return true;
				}
			}
		}
		return false;
	}

	private boolean doVariableSubstitution(GraphPatternElement gpe, VariableNode v1, VariableNode v2) {
		boolean retval = false;
		do {
			if (gpe instanceof TripleElement) {
				if (((TripleElement)gpe).getSubject().equals(v1)) {
					((TripleElement)gpe).setSubject(v2);
					retval = true;
				}
				else if (((TripleElement)gpe).getObject().equals(v1)) {
					((TripleElement)gpe).setObject(v2);
					retval = true;
				}
			}
			else if (gpe instanceof BuiltinElement) {
				List<Node> args = ((BuiltinElement)gpe).getArguments();
				for (int j = 0; j < args.size(); j++) {
					if (args.get(j).equals(v1)) {
						args.set(j, v2);
						retval = true;
					}
				}
			}
			else if (gpe instanceof Junction) {
				logger.error("Not yet handled");
			}
			gpe = gpe.getNext();
		} while (gpe != null);
		return retval;
	}

	/**
	 * This method returns true if the argument node is bound in some other element of the rule
	 * 
	 * @param rule
	 * @param gpe
	 * @param v
	 * @return
	 */
	public static boolean variableIsBound(Rule rule, GraphPatternElement gpe,
			Node v) {
		if (v instanceof NamedNode) {
			if (((NamedNode)v).getNodeType() != null && !(((NamedNode)v).getNodeType().equals(NodeType.VariableNode))) {
				return true;
			}
		}
		// Variable is bound if it appears in a triple or as the return argument of a built-in
		List<GraphPatternElement> givens = rule.getGivens();
		if (variableIsBoundInOtherElement(givens, 0, gpe, true, false, v)) {
			return true;
		}
		List<GraphPatternElement> ifs = rule.getIfs();
		if (variableIsBoundInOtherElement(ifs, 0, gpe, true, false, v)) {
			return true;
		}
		List<GraphPatternElement> thens = rule.getThens();
		if (variableIsBoundInOtherElement(thens, 0, gpe, false, true, v)) {
			return true;
		}
		return false;
	}

	private GraphPatternElement createBinaryBuiltin(Expression expr, String name, Object lobj, Object robj) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (name.equals(JunctionType.AND_ALPHA) || name.equals(JunctionType.AND_SYMBOL) || name.equals(JunctionType.OR_ALPHA) || name.equals(JunctionType.OR_SYMBOL)) {
			Junction jct = new Junction();
			jct.setJunctionName(name);
			jct.setLhs(lobj);
			jct.setRhs(robj);
			return jct;
		}
		else {
			BuiltinElement builtin = new BuiltinElement();
			builtin.setFuncName(name);
			if (lobj != null) {
				builtin.addArgument(nodeCheck(lobj));
			}
			if (robj != null) {
				builtin.addArgument(nodeCheck(robj));
			}
			return builtin;
		}
	}
	
	private Junction createJunction(Expression expr, String name, Object lobj, Object robj) {
		Junction junction = new Junction();
		junction.setJunctionName(name);
		junction.setLhs(lobj);
		junction.setRhs(robj);
		return junction;
	}

	private Object createUnaryBuiltin(Expression sexpr, String name, Object sobj) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (sobj instanceof com.ge.research.sadl.model.gp.Literal && BuiltinType.getType(name).equals(BuiltinType.Minus)) {
			Object theVal = ((com.ge.research.sadl.model.gp.Literal)sobj).getValue();
			if (theVal instanceof Integer) {
				theVal = ((Integer)theVal) * -1;
			}
			else if (theVal instanceof Long) {
				theVal = ((Long)theVal) * -1;
			}
			else if (theVal instanceof Float) {
				theVal = ((Float)theVal) * -1;
			}
			else if (theVal instanceof Double) {
				theVal = ((Double)theVal) * -1;
			}
			((com.ge.research.sadl.model.gp.Literal)sobj).setValue(theVal);
			((com.ge.research.sadl.model.gp.Literal)sobj).setOriginalText("-" + ((com.ge.research.sadl.model.gp.Literal)sobj).getOriginalText());
			return sobj;
		}
		if (sobj instanceof Junction) {
			// If the junction has two literal values, apply the op to both of them.
			Junction junc = (Junction) sobj;
			Object lhs = junc.getLhs();
			Object rhs = junc.getRhs();
			if (lhs instanceof com.ge.research.sadl.model.gp.Literal && rhs instanceof com.ge.research.sadl.model.gp.Literal) {
				lhs = createUnaryBuiltin(sexpr, name, lhs);
				rhs = createUnaryBuiltin(sexpr, name, rhs);
				junc.setLhs(lhs);
				junc.setRhs(rhs);
			}
			return junc;
		}
		if (BuiltinType.getType(name).equals(BuiltinType.Equal)) {
			if (sobj instanceof BuiltinElement) {
				if (isComparisonBuiltin(((BuiltinElement)sobj).getFuncName())) {
					// this is a "is <comparison>"--translates to <comparsion> (ignore is)
					return sobj;
				}
			}
			else if (sobj instanceof com.ge.research.sadl.model.gp.Literal || sobj instanceof NamedNode) {
				// an "=" interval value of a value is just the value
				return sobj;
			}
		}
		BuiltinElement builtin = new BuiltinElement();
		builtin.setFuncName(name);
		if (isModifiedTriple(builtin.getFuncType())) {
			if (sobj instanceof TripleElement) {
				((TripleElement)sobj).setType(getTripleModifierType(builtin.getFuncType()));
				return sobj;
			}
		}
		if (sobj != null) {
			builtin.addArgument(nodeCheck(sobj));
		}
		return builtin;
	}

	private Node nodeCheck(Object nodeObj) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (nodeObj == null) {
//			throw new InvalidTypeException("nodeCheck called with null argument; this should not happen.");
			return null;
		}
		if (nodeObj instanceof Node) {
			return (Node) nodeObj; 
		}
		else if (nodeObj instanceof TripleElement) {
			if (((TripleElement)nodeObj).getPredicate() == null 
					&& ((TripleElement)nodeObj).getObject() == null
					&& ((TripleElement)nodeObj).getSubject() != null) {
				return ((TripleElement)nodeObj).getSubject();
			}
		}
		return new ProxyNode(nodeObj);
	}

	private TripleModifierType getTripleModifierType(BuiltinType btype) {
		if (btype.equals(BuiltinType.Not) || btype.equals(BuiltinType.NotEqual)) {
			return TripleModifierType.Not;
		}
		else if (btype.equals(BuiltinType.Only)) {
			return TripleModifierType.Only;
		}
		else if (btype.equals(BuiltinType.NotOnly)) {
			return TripleModifierType.NotOnly;
		}
		return null;
	}

	public String processExpression(BooleanLiteral expr) {
//		System.out.println("processing " + expr.getClass().getCanonicalName() + ": " + expr.getValue());
		return expr.getValue();
	}
	
	public ConstantNode processExpression(Constant expr) throws InvalidNameException {
//		System.out.println("processing " + expr.getClass().getCanonicalName() + ": " + expr.getConstant());
		return new ConstantNode(expr.getConstant());
	}
	
	public Object processExpression(Declaration expr) throws TranslationException {
		String nn = expr.getNewName();
		SadlTypeReference type = expr.getType();
		Object typenode = processExpression(type);
		return typenode;
	}
	
	private Object processExpression(SadlTypeReference type) throws TranslationException {
		if (type instanceof SadlSimpleTypeReference) {
			return processExpression(((SadlSimpleTypeReference)type).getType());
		}
		else if (type instanceof SadlPrimitiveDataType) {
			SadlDataType pt = ((SadlPrimitiveDataType)type).getPrimitiveType();
			String typeStr = pt.getLiteral();
			return typeStr;
		}
		throw new TranslationException("Unhandled type of SadlTypeReference");
	}
	
	public Object processExpression(Name expr) throws TranslationException, InvalidNameException, InvalidTypeException {
		if (expr.isFunction()) {
			return processFunction(expr);
		}
		SadlResource qnm =expr.getName();
		String nm = declarationExtensions.getConcreteName(qnm);
		if (nm == null) {
			SadlResource srnm = qnm.getName();
			if (srnm != null) {
				return processExpression(srnm);
			}
			addError("translate(Name) called with a SadlResource which resolved to null; this needs to be caught in validation", expr);
			nm = "nullvarname";
		}
		return processExpression(qnm);
	}
	
	private String getPrefix(String qn) {
		if (qn.contains(":")) {
			return qn.substring(0,qn.indexOf(":"));
		}
		return qn;
	}
	public Object 	processExpression(NumberLiteral expr) {
		Object lit = translate(expr);
		return lit;
	}
	
	public Object processExpression(StringLiteral expr) {
		return translate(expr);
	}
	
	public Object processExpression(PropOfSubject expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		Expression predicate = expr.getLeft();
		Expression subject = expr.getRight();
		Node subjNode = null;
		Node predNode = null;
		if (predicate instanceof Constant) {
			// this is a pseudo PropOfSubject; the predicate is a constant
			String cnstval = ((Constant)predicate).getConstant();
			if (cnstval.equals("length")) {
			}
			else if (cnstval.equals("count")) {
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject)subject).getLeft();
					subject = ((PropOfSubject)subject).getRight();
				}
				Object elobj = translate(predicate);
			}
			else if (cnstval.equals("index")) {
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject)subject).getLeft();
					subject = ((PropOfSubject)subject).getRight();
				}
				Object idxobj = translate(predicate);
			}
			else if (cnstval.equals("first element")) {
			}
			else if (cnstval.equals("last element")) {
			}
			else {
				System.err.println("Unhandled constant property in translate PropOfSubj: " + cnstval);
			}
		}
		else {
			Object predobj = translate(predicate);
			if(predobj instanceof Node) {
				predNode = (Node) predobj;
			}
			else {
				throw new TranslationException("Predicate '" + predobj.toString() + "' did not translate to Node");
			}
		}
		if (subject != null) {
			Object sn = translate(subject);
			if (sn instanceof Node) {
				subjNode = (Node) sn;
			}
			else {
				throw new TranslationException("Subject '" + sn.toString() + "' did not translate to Node");
			}
		}
		else {
			throw new TranslationException("Subject of PropOfSubject is null (Resource '" + expr.eResource().getURI() + "')");
		}
		TripleElement returnTriple = null;
		if (predNode != null) {
			returnTriple = new TripleElement(subjNode, predNode, null);
			returnTriple.setSourceType(TripleSourceType.PSV);
		}
		else {
			predNode = new RDFTypeNode();			
			Node variable = getVariableNode(expr, null, predNode, subjNode);
			returnTriple = new TripleElement();
			returnTriple.setSubject(variable);
			returnTriple.setPredicate(predNode);
			returnTriple.setObject(subjNode);
			if (subjNode instanceof NamedNode && !((NamedNode)subjNode).getNodeType().equals(NodeType.ClassNode)) {
				addError("'" + subjNode.toString() + "' is not a Class.", subject);
			}
			returnTriple.setSourceType(TripleSourceType.ITC);
		}

		return returnTriple;
	}
	
	public Node processExpression(SadlResource expr) throws TranslationException {
		String nm =  declarationExtensions.getConcreteName(expr);
		String ns = declarationExtensions.getConceptNamespace(expr);
		String prfx = declarationExtensions.getConceptPrefix(expr);
		OntConceptType type;
		try {
			type = declarationExtensions.getOntConceptType(expr);
		} catch (CircularDefinitionException e) {
			type = e.getDefinitionType();
			addError(e.getMessage(), expr);
		}
		if (type.equals(OntConceptType.VARIABLE)) {
			VariableNode vn = new VariableNode(nm);
			vn.setNamespace(ns);
			vn.setPrefix(prfx);
			vn.setNodeType(ontConceptTypeToNodeType(type));
			return vn;
		}
		else {
			NamedNode n = new NamedNode(nm, ontConceptTypeToNodeType(type));
			n.setNamespace(ns);
			n.setPrefix(prfx);
			n.setNodeType(ontConceptTypeToNodeType(type));
			return n;
		}
	}
	
	public Object processExpression(SubjHasProp expr) throws InvalidNameException, InvalidTypeException, TranslationException {
//		System.out.println("processing " + expr.getClass().getCanonicalName() + ": " + expr.getProp().toString());
		Expression subj = expr.getLeft();
		Expression pred = expr.getProp();
		Expression obj = expr.getRight();
		Object sobj = null;
		Object pobj = null;
		Object oobj = null;
		if (subj != null) {
			sobj = translate(subj);
		}
		if (pred != null) {
			pobj = translate(pred);
		}
		if (obj != null) {
			oobj = translate(obj);
		}
		TripleElement returnTriple = null;
		if (pobj != null) {
			returnTriple = new TripleElement(null, nodeCheck(pobj), null);
			returnTriple.setSourceType(TripleSourceType.SPV);
		}
		if (sobj != null) {
			returnTriple.setSubject(nodeCheck(sobj));
		}
		if (oobj != null) {
			returnTriple.setObject(nodeCheck(oobj));
		}
		return returnTriple;
	}
	
	public Object processExpression(Sublist expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		Expression list = expr.getList();
		Expression where = expr.getWhere();
		Object lobj = translate(list);
		Object wobj = translate(where);
		
		addError("Processing of sublist construct not yet implemented: " + lobj.toString() + ", " + wobj.toString(), expr);
		
		BuiltinElement builtin = new BuiltinElement();
		builtin.setFuncName("sublist");
		builtin.addArgument(nodeCheck(lobj));
		if (lobj instanceof GraphPatternElement) {
			((GraphPatternElement)lobj).setEmbedded(true);
		}
		builtin.addArgument(nodeCheck(wobj));
		if (wobj instanceof GraphPatternElement) {
			((GraphPatternElement)wobj).setEmbedded(true);
		}
		return builtin;
	}
	
	public Object processExpression(UnaryExpression expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		Object eobj = translate(expr.getExpr());
		String op = expr.getOp();
		if (eobj instanceof com.ge.research.sadl.model.gp.Literal) {
			Object val = ((com.ge.research.sadl.model.gp.Literal)eobj).getValue();
			if (val instanceof Number) {
				val = -1.0 * ((Number)val).doubleValue();
				((com.ge.research.sadl.model.gp.Literal)eobj).setValue(val);
				((com.ge.research.sadl.model.gp.Literal)eobj).setOriginalText(op + ((com.ge.research.sadl.model.gp.Literal)eobj).getOriginalText());
				return eobj;
			}
		}
		BuiltinElement bi = new BuiltinElement();
		bi.setFuncName(op);
		bi.addArgument((Node) eobj);
		return bi;
	}
	
	public Object processExpression(Unit expr) {
		String unit = expr.getUnit();
		NumberLiteral value = expr.getValue();
		Object valobj = translate(value);
		if (valobj instanceof com.ge.research.sadl.model.gp.Literal) {
			((com.ge.research.sadl.model.gp.Literal)valobj).setUnits(unit);
		}
		return valobj;
	}
	
	private void processSadlSameAs(SadlSameAs element) throws JenaProcessorException {
		SadlResource sr = element.getNameOrRef();
		String uri = declarationExtensions.getConceptUri(sr);
		OntResource rsrc = getTheJenaModel().getOntResource(uri);
		SadlTypeReference smas = element.getSameAs();
		OntConceptType sameAsType;
		if (rsrc == null) {
			// concept does not exist--try to get the type from the sameAs
			sameAsType = getSadlTypeReferenceType(smas);
			
		}
		else {
			try {
				sameAsType = declarationExtensions.getOntConceptType(sr);
			} catch (CircularDefinitionException e) {
				sameAsType = e.getDefinitionType();
				addError(e.getMessage(), element);
			}
		}
		if (sameAsType.equals(OntConceptType.CLASS)) {
			OntClass smasCls = sadlTypeReferenceToOntResource(smas).asClass();
			// this is a class axiom
			OntClass cls = getTheJenaModel().getOntClass(uri);
			if (cls == null) {
				// this is OK--create class
				cls = createOntClass(declarationExtensions.getConcreteName(sr), (String)null, null);
			}
			if (element.isComplement()) {
				ComplementClass cc = getTheJenaModel().createComplementClass(cls.getURI(), smasCls);
				logger.debug("New complement class '" + cls.getURI() + "' created");
			}
			else {
				cls.addEquivalentClass(smasCls);
				logger.debug("Class '" + cls.toString() + "' given equivalent class '" + smasCls.toString() + "'");
			}
		}
		else if (sameAsType.equals(OntConceptType.INSTANCE)) {
			OntResource smasInst = sadlTypeReferenceToOntResource(smas);
			rsrc.addSameAs(smasInst);
			logger.debug("Instance '" + rsrc.toString() + "' declared same as '" + smas.toString() + "'");
		}
		else {
			throw new JenaProcessorException("Unexpected concept type for same as statement: " + sameAsType.toString());
		}
	}

	private List<OntResource> processSadlClassOrPropertyDeclaration(SadlClassOrPropertyDeclaration element) throws JenaProcessorException, TranslationException {
		// Get the names of the declared concepts and store in a list
		List<String> newNames = new ArrayList<String>();
		Map<String, EList<SadlAnnotation>> nmanns = null;
		EList<SadlResource> clses = element.getClassOrProperty();
		if (clses != null) {
			Iterator<SadlResource> citer = clses.iterator();
			while (citer.hasNext()) {
				SadlResource sr = citer.next();
				String nm = declarationExtensions.getConceptUri(sr);
				newNames.add(nm);
				EList<SadlAnnotation> anns = sr.getAnnotations();
				if (anns != null && anns.size() > 0) {
					if (nmanns == null) {
						nmanns = new HashMap<String,EList<SadlAnnotation>>();
					}
					nmanns.put(nm,  anns);
				}
			}
		}
		
		if (newNames.size() < 1) {
			throw new JenaProcessorException("No names passed to processSadlClassOrPropertyDeclaration");
		}
		List<OntResource> rsrcList = new ArrayList<OntResource>();
		// The declared concept(s) will be of type class, property, or datatype. 
		//	Determining which will depend on the structure, including the superElement....
		// 	Get the superElement
		SadlTypeReference superElement = element.getSuperElement();
		boolean isList = typeRefIsList(superElement);
		//		1) if superElement is null then it is a top-level class declaration
		if (superElement == null) {
			OntClass cls = createOntClass(newNames.get(0), (OntClass)null);
			if (nmanns != null && nmanns.get(newNames.get(0)) != null) {
				addAnnotationsToResource(cls, nmanns.get(newNames.get(0)));
			}
			rsrcList.add(cls);
		}
		//  	2) if superElement is not null then the type of the new concept is the same as the type of the superElement
		// 			the superElement can be:
		// 				a) a SadlSimpleTypeReference
		else if (superElement instanceof SadlSimpleTypeReference) {
			SadlResource superSR = ((SadlSimpleTypeReference)superElement).getType();
			String superSRUri = declarationExtensions.getConceptUri(superSR);	
			OntConceptType superElementType;
			try {
				superElementType = declarationExtensions.getOntConceptType(superSR);
				if (isList) {
					superElementType = OntConceptType.CLASS_LIST;
				}
			} catch (CircularDefinitionException e) {
				superElementType = e.getDefinitionType();
				addError("Part of circular definition", superElement);
			}
			if (superElementType.equals(OntConceptType.CLASS)) {
				for (int i = 0; i < newNames.size(); i++) {
					OntClass cls = createOntClass(newNames.get(i), superSRUri, superSR);
					if (nmanns != null && nmanns.get(newNames.get(i)) != null) {
						addAnnotationsToResource(cls, nmanns.get(newNames.get(i)));
					}
					rsrcList.add(cls);
				}
			}
			else if (superElementType.equals(OntConceptType.CLASS_LIST) || superElementType.equals(OntConceptType.DATATYPE_LIST)) {
				for (int i = 0; i < newNames.size(); i++) {
					rsrcList.add(createListSubclass(newNames.get(i), superSRUri, superSR.eResource()));
				}
			}
			else if (superElementType.equals(OntConceptType.CLASS_PROPERTY)) {
				for (int i = 0; i < newNames.size(); i++) {
					OntProperty prop = createObjectProperty(newNames.get(i), superSRUri);
					if (nmanns != null && nmanns.get(newNames.get(i)) != null) {
						addAnnotationsToResource(prop, nmanns.get(newNames.get(i)));
					}
					rsrcList.add(prop);
				}
			}
			else if (superElementType.equals(OntConceptType.DATATYPE_PROPERTY)) {
				for (int i = 0; i < newNames.size(); i++) {
					DatatypeProperty prop = createDatatypeProperty(newNames.get(i), superSRUri);
					if (nmanns != null && nmanns.get(newNames.get(i)) != null) {
						addAnnotationsToResource(prop, nmanns.get(newNames.get(i)));
					}
					rsrcList.add(prop);
				}
			}
			else if (superElementType.equals(OntConceptType.ANNOTATION_PROPERTY)) {
				for (int i = 0; i < newNames.size(); i++) {
					AnnotationProperty prop = createAnnotationProperty(newNames.get(i), superSRUri);
					if (nmanns != null && nmanns.get(newNames.get(i)) != null) {
						addAnnotationsToResource(prop, nmanns.get(newNames.get(i)));
					}
					rsrcList.add(prop);
				}
			}
			else if (superElementType.equals(OntConceptType.RDF_PROPERTY)) {
				for (int i = 0; i < newNames.size(); i++) {
					OntProperty prop = createRdfProperty(newNames.get(i), superSRUri);
					if (nmanns != null && nmanns.get(newNames.get(i)) != null) {
						addAnnotationsToResource(prop, nmanns.get(newNames.get(i)));
					}
					rsrcList.add(prop);
				}
			}
		}
		//				b) a SadlPrimitiveDataType
		else if (superElement instanceof SadlPrimitiveDataType) {
			if (isList) {
				com.hp.hpl.jena.rdf.model.Resource spdt = processSadlPrimitiveDataType(element, (SadlPrimitiveDataType) superElement, null);
				rsrcList.add(createListSubclass(newNames.get(0), spdt.getURI(), superElement.eResource()));
			}
			else {
				com.hp.hpl.jena.rdf.model.Resource spdt = processSadlPrimitiveDataType(element, (SadlPrimitiveDataType) superElement, newNames.get(0));
				if (spdt instanceof OntClass) {
					rsrcList.add((OntClass)spdt);
				}
				else if (spdt.canAs(OntResource.class)){
					rsrcList.add(spdt.as(OntResource.class));
				}
				else {
					throw new JenaProcessorException("Expected OntResource to be returned");  // .add(spdt);
				}
			}
		}
		//				c) a SadlPropertyCondition
		else if (superElement instanceof SadlPropertyCondition) {
			OntClass propCond = processSadlPropertyCondition((SadlPropertyCondition) superElement);
			rsrcList.add(propCond);
		}
		//				d) a SadlTypeReference
		else if (superElement instanceof SadlTypeReference) {
			// this can only be a class; can't create a property as a SadlTypeReference
			Object superClsObj = sadlTypeReferenceToObject(superElement);
			if (superClsObj instanceof List) {
				// must be a union of xsd datatypes; create RDFDatatype
				OntClass unionCls = createRdfsDatatype(newNames.get(0), (List)superClsObj, null, null);
				rsrcList.add(unionCls);
			}
			else if (superClsObj instanceof OntResource) {
				OntResource superCls = (OntResource)superClsObj;
				if (superCls != null) {
					if (superCls instanceof UnionClass) {
						ExtendedIterator<? extends com.hp.hpl.jena.rdf.model.Resource> itr = ((UnionClass)superCls).listOperands();
						while (itr.hasNext()) {
							com.hp.hpl.jena.rdf.model.Resource cls = itr.next();
//							System.out.println("Union member: " + cls.toString());
						}
					}
					else if (superCls instanceof IntersectionClass) {
						ExtendedIterator<? extends com.hp.hpl.jena.rdf.model.Resource> itr = ((IntersectionClass)superCls).listOperands();
						while (itr.hasNext()) {
							com.hp.hpl.jena.rdf.model.Resource cls = itr.next();
//							System.out.println("Intersection member: " + cls.toString());
						}
					}
					rsrcList.add(createOntClass(newNames.get(0), superCls.as(OntClass.class)));
				}
			}
		}
		EList<SadlPropertyRestriction> restrictions = element.getRestrictions();
		if (restrictions != null) {
			Iterator<SadlPropertyRestriction> ritr = restrictions.iterator();
			while (ritr.hasNext()) {
				SadlPropertyRestriction rest = ritr.next();
				if (rest instanceof SadlMustBeOneOf) {
					EList<SadlExplicitValue> instances = ((SadlMustBeOneOf)rest).getValues();
					if (instances != null) {
						Iterator<SadlExplicitValue> iitr = instances.iterator();
						while (iitr.hasNext()) {
							SadlExplicitValue inst = iitr.next();
							if (inst instanceof SadlResource) {
								for (int i = 0; i < rsrcList.size(); i++) {
									createIndividual((SadlResource)inst, rsrcList.get(i).asClass());
								}
							}
							else {
								throw new JenaProcessorException("Unhandled type of SadlExplicitValue: " + inst.getClass().getCanonicalName());
							}
						}
					}
				}
			}
		}
		for (int i = 0; i < rsrcList.size(); i++) {
			Iterator<SadlProperty> dbiter = element.getDescribedBy().iterator();
			while (dbiter.hasNext()) {
				SadlProperty sp = dbiter.next();
				// if this is an assignment of a range to a property the property will be returned (prop) for domain assignment,
				//  but if it is a condition to be added as property restriction null will be returned
				Property prop = processSadlProperty(rsrcList.get(i), sp);
				if (prop != null) {
					addPropertyDomain(prop, rsrcList.get(i));
				}
			}

		}
		if (isList) {
			// check for list length restrictions
			SadlDataTypeFacet facet = element.getFacet();
			if (facet != null) {
				if (facet.getLen() != null) {
					int len = Integer.parseInt(facet.getLen());
					HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, getTheJenaModel().getProperty(SADL_LIST_MODEL_LENGTH_RESTRICTION_URI), getTheJenaModel().createTypedLiteral(len));
					((OntClass)rsrcList.get(0)).addSuperClass(hvr);
				}
				if (facet.getMinlen() != null) {
					int minlen = Integer.parseInt(facet.getMinlen());
					HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, getTheJenaModel().getProperty(SADL_LIST_MODEL_MINLENGTH_RESTRICTION_URI), getTheJenaModel().createTypedLiteral(minlen));
					((OntClass)rsrcList.get(0)).addSuperClass(hvr);
				}
				if (facet.getMaxlen() != null && !facet.getMaxlen().equals("*")) {
					int maxlen = Integer.parseInt(facet.getMaxlen());
					HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, getTheJenaModel().getProperty(SADL_LIST_MODEL_MAXLENGTH_RESTRICTION_URI), getTheJenaModel().createTypedLiteral(maxlen));
					((OntClass)rsrcList.get(0)).addSuperClass(hvr);
				}
			}
		}
		return rsrcList;
	}

	private Property processSadlProperty(OntResource subject, SadlProperty element) throws JenaProcessorException {
		Property retProp = null;
		// this has multiple forms:
		//	1) <prop> is a property...
		//	2) relationship of <Domain> to <Range> is <prop>
		//  3) <prop> describes <class> with <range info>	(1st spr is a SadlTypeAssociation, the domain; 2nd spr is a SadlRangeRestriction, the range)
		//  4) <prop> of <class> <restriction>	(1st spr is a SadlTypeAssociation, the class being restricted; 2nd spr is a SadlCondition
		//  5) <prop> of <class> can only be one of {<instances> or <datavalues>} (1st spr is SadlTypeAssociation, 2nd spr is a SadlCanOnlyBeOneOf)
		//  6) <prop> of <class> must be one of {<instances> or <datavalues>} (1st spr is SadlTypeAssociation, 2nd spr is a SadlCanOnlyBeOneOf)
		SadlResource sr = sadlResourceFromSadlProperty(element);
		String propUri = declarationExtensions.getConceptUri(sr);
		OntConceptType propType;
		try {
			propType = declarationExtensions.getOntConceptType(sr);
		} catch (CircularDefinitionException e) {
			propType = e.getDefinitionType();
			addError(e.getMessage(), element);
		}
		
		
		Iterator<SadlPropertyRestriction> spitr = element.getRestrictions().iterator();
		if (spitr.hasNext()) {
			SadlPropertyRestriction spr1 = spitr.next();
			if (spr1 instanceof SadlIsAnnotation) {
				retProp = getTheJenaModel().createAnnotationProperty(propUri);
			}
			else if (spr1 instanceof SadlIsTransitive) {
				OntProperty pr;
				if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
					pr = getOrCreateObjectProperty(propUri);
				}
				else {
					throw new JenaProcessorException("Only object properties can be transitive");
				}
				if (pr == null) {
					throw new JenaProcessorException("Property '" + propUri + "' not found in ontology.");
				}
				pr.convertToTransitiveProperty();
				retProp = getTheJenaModel().createTransitiveProperty(pr.getURI());
			}
			else if (spr1 instanceof SadlIsInverseOf) {
				OntProperty pr;
				if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
					pr = getOrCreateObjectProperty(propUri);
				}
				else {
					throw new JenaProcessorException("Only object properties can have inverses");
				}
				if (pr == null) {
					throw new JenaProcessorException("Property '" + propUri + "' not found in ontology.");
				}
				SadlResource otherProp = ((SadlIsInverseOf)spr1).getOtherProperty();
				String otherPropUri = declarationExtensions.getConceptUri(otherProp);
				OntConceptType optype;
				try {
					optype = declarationExtensions.getOntConceptType(otherProp);
				} catch (CircularDefinitionException e) {
					optype = e.getDefinitionType();
					addError(e.getMessage(), element);
				}
				if (!optype.equals(OntConceptType.CLASS_PROPERTY)) {
					throw new JenaProcessorException("Only object properties can have inverses");
				}
				OntProperty opr = getOrCreateObjectProperty(otherPropUri);
				if (opr == null) {
					throw new JenaProcessorException("Property '" + otherPropUri + "' not found in ontology.");
				}
				pr.addInverseOf(opr);
			}
			else if (spr1 instanceof SadlRangeRestriction) {
				if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
					retProp = getOrCreateObjectProperty(propUri);					
				}
				else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
					retProp = createDatatypeProperty(propUri, null);					
				}
				else if (propType.equals(OntConceptType.RDF_PROPERTY)) {
					retProp = createRdfProperty(propUri, null);
				}
				else {
					throw new JenaProcessorException("Property '" + propUri + "' has unhandled type " + propType.toString());
				}
				SadlTypeReference rng = ((SadlRangeRestriction)spr1).getRange();
				if (rng != null) {
					boolean isList = typeRefIsList(rng);
					RangeValueType rngValueType = RangeValueType.CLASS_OR_DT;	// default
					if (rng instanceof SadlPrimitiveDataType) {
						// this is a DatatypeProperty with explicit range
						String rngName = ((SadlPrimitiveDataType)rng).getPrimitiveType().getName();
						RDFNode rngNode = primitiveDatatypeToRDFNode(rngName);
						DatatypeProperty prop = null;
						if (!checkForExistingCompatibleDatatypeProperty(propUri, rngNode)) {
							prop = createDatatypeProperty(propUri, null);
							addPropertyRange(propType, prop, rngNode, rngValueType, rng);
						}
						else {
							prop = getTheJenaModel().getDatatypeProperty(propUri);
							addPropertyRange(propType, prop, rngNode, rngValueType, rng);
						}
						retProp = prop;
					}
					else {
						// this is an ObjectProperty with explicit range
						OntResource rngRsrc = sadlTypeReferenceToOntResource(rng);
						if (rngRsrc == null) {
							throw new JenaProcessorException("Range failed to resolve to a class or datatype");
						}
						retProp = assignRangeToProperty(propUri, propType, rngRsrc, rngValueType, rng);
					}
				}
			}
			else if (spr1 instanceof SadlCondition) {
				if (subject.canAs(OntClass.class)){ 
					OntClass cls = subject.as(OntClass.class);
					OntProperty prop = getTheJenaModel().getOntProperty(propUri);
					if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
						OntClass condCls = sadlConditionToOntClass((SadlCondition) spr1, prop, propType);
						cls.addSuperClass(condCls);
						retProp = null;
					}
					else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
						OntClass condCls = sadlConditionToOntClass((SadlCondition) spr1, prop, propType);
						cls.addSuperClass(condCls);
						retProp = null;
					}
					else if (propType.equals(OntConceptType.RDF_PROPERTY)) {
						if (prop == null) {
							prop = getOrCreateRdfProperty(propUri);
						}
						OntClass condCls = sadlConditionToOntClass((SadlCondition) spr1, prop, propType);
						cls.addSuperClass(condCls);
						retProp = null;
					}
					else {
						throw new JenaProcessorException("Invalid property type: " + propType.toString());
					}
				}
				else {
					throw new JenaProcessorException("Unable to convert concept being restricted (" + subject.toString() + ") to an OntClass.");
				}
			}
			else if (spitr.hasNext()) {
				SadlPropertyRestriction spr2 = spitr.next();
				if (spitr.hasNext()) {
					StringBuilder sb = new StringBuilder();
					int cntr = 0;
					while (spitr.hasNext()) {
						if (cntr++ > 0) sb.append(", ");
						sb.append(spitr.next().getClass().getCanonicalName());
					}
					throw new JenaProcessorException("Unexpected SadlProperty has more than 2 restrictions: " + sb.toString());
				}
				if (spr1 instanceof SadlTypeAssociation && spr2 instanceof SadlRangeRestriction) {
					// this is case 3
					SadlTypeReference domain = ((SadlTypeAssociation)spr1).getDomain();
					OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
					OntProperty prop;
					if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
						prop = getOrCreateObjectProperty(propUri);
					}
					else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)){
						prop = getOrCreateDatatypeProperty(propUri);
					}
					else if (propType.equals(OntConceptType.RDF_PROPERTY)) {
						prop = getOrCreateRdfProperty(propUri);
					}
					else {
						throw new JenaProcessorException("Invalid property type (" + propType.toString() + ") for '" + propUri + "'");
					}
					addPropertyDomain(prop, domainrsrc);
					SadlTypeReference from = element.getFrom();
					if (from != null) {
						OntResource fromrsrc = sadlTypeReferenceToOntResource(from);
						throw new JenaProcessorException("What is 'from'?");
					}
					SadlTypeReference to = element.getTo();
					if (to != null) {
						OntResource torsrc = sadlTypeReferenceToOntResource(to);
						throw new JenaProcessorException("What is 'to'?");
					}
					
					RangeValueType rngValueType = RangeValueType.CLASS_OR_DT;	// default
					SadlTypeReference rng = ((SadlRangeRestriction)spr2).getRange();
					if (rng instanceof SadlPrimitiveDataType) {
						String rngName = ((SadlPrimitiveDataType)rng).getPrimitiveType().getName();
						RDFNode rngNode = primitiveDatatypeToRDFNode(rngName);
						DatatypeProperty prop2 = null;
						if (!checkForExistingCompatibleDatatypeProperty(propUri, rngNode)) {
							//TODO should this ever happen? spr1 should have created the property?
							prop2 = createDatatypeProperty(propUri, null);
							addPropertyRange(propType, prop, rngNode, rngValueType, rng);
						}
						else {
							prop2 = getTheJenaModel().getDatatypeProperty(propUri);
						}
						retProp = prop2;
					}
					else if (((SadlRangeRestriction)spr2).getTypeonly() == null) {				
						OntResource rngRsrc = sadlTypeReferenceToOntResource(rng);
						if (rngRsrc == null) {
							addError("Range failed to resolve to a class or datatype", rng);
						}
						else {
							retProp = assignRangeToProperty(propUri, propType, rngRsrc, rngValueType, rng);
						}
					}
					else {
						retProp = prop;
					}
				}
				else if (spr1 instanceof SadlTypeAssociation && spr2 instanceof SadlCondition) {
					// this is case 4
					SadlTypeReference domain = ((SadlTypeAssociation)spr1).getDomain();
					OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
					if (domainrsrc == null) {
						addError("Unable to find domain.", domain);
						return null;
					}
					else if (domainrsrc.canAs(OntClass.class)){ 
						OntClass cls = domainrsrc.as(OntClass.class);
						Property prop = getTheJenaModel().getProperty(propUri);
						if (prop != null) {
							OntClass condCls = sadlConditionToOntClass((SadlCondition) spr2, prop, propType);
							if (condCls != null) {
								cls.addSuperClass(condCls);
							}
							else {
								addError("Unable to add restriction; unable to create condition class", domain);
							}
							retProp = prop;
						}
						else {
							throw new JenaProcessorException("Unable to convert property '" + propUri + "' to OntProperty.");
						}
					}
					else {
						throw new JenaProcessorException("Unable to convert concept being restricted (" + domainrsrc.toString() + ") to an OntClass.");
					}
				}
				else if (spr1 instanceof SadlTypeAssociation && spr2 instanceof SadlCanOnlyBeOneOf) {
					SadlTypeReference domain = ((SadlTypeAssociation)spr1).getDomain();
					OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
					if (domainrsrc == null) {
						addError("Unable to find domain.", domain);
						return null;
					}
					else if (domainrsrc.canAs(OntClass.class)){ 
						OntClass cls = domainrsrc.as(OntClass.class);
						Property prop = getTheJenaModel().getProperty(propUri);
						if (prop != null) {
							EList<SadlExplicitValue> values = ((SadlCanOnlyBeOneOf)spr2).getValues();
							if (values != null) {
								EnumeratedClass enumCls = sadlExplicitValuesToEnumeratedClass(values);
								AllValuesFromRestriction avf = getTheJenaModel()
										.createAllValuesFromRestriction(null,
												prop, enumCls);
								if (avf != null) {
									cls.addSuperClass(avf);
								} else {
									addError("Unable to create allValuesFromRestriction for unknown reason.", spr2);
								}
							}
							else {
								addError("Unable to add all values from restriction; unable to create oneOf class", domain);
							}
							retProp = prop;
						}
						else {
							throw new JenaProcessorException("Unable to convert property '" + propUri + "' to OntProperty.");
						}
					}
				}
				else if (spr1 instanceof SadlTypeAssociation && spr2 instanceof SadlMustBeOneOf) {
					SadlTypeReference domain = ((SadlTypeAssociation)spr1).getDomain();
					OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
					if (domainrsrc == null) {
						addError("Unable to find domain.", domain);
						return null;
					}
					else if (domainrsrc.canAs(OntClass.class)){ 
						OntClass cls = domainrsrc.as(OntClass.class);
						Property prop = getTheJenaModel().getProperty(propUri);
						if (prop != null) {
							EList<SadlExplicitValue> values = ((SadlMustBeOneOf)spr2).getValues();
							if (values != null) {
								EnumeratedClass enumCls = sadlExplicitValuesToEnumeratedClass(values);
								SomeValuesFromRestriction svf = getTheJenaModel()
										.createSomeValuesFromRestriction(null,
												prop, enumCls);
								if (svf != null) {
									cls.addSuperClass(svf);
								} else {
									addError("Unable to create someValuesFromRestriction for unknown reason.", spr2);
								}
							}
							else {
								addError("Unable to add some values from restriction; unable to create oneOf class", domain);
							}
							retProp = prop;
						}
						else {
							throw new JenaProcessorException("Unable to convert property '" + propUri + "' to OntProperty.");
						}
					}					
				}
				else {
					throw new JenaProcessorException("Unhandled restriction: spr1 is '" + spr1.getClass().getName() + "', spr2 is '" + spr2.getClass().getName() + "'");
				}
			}
			else if (spr1 instanceof SadlTypeAssociation) {
				// this is case 3 but with range not present
				SadlTypeReference domain = ((SadlTypeAssociation)spr1).getDomain();
				OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
				ObjectProperty prop = getOrCreateObjectProperty(propUri);
				if (domainrsrc != null) {
					addPropertyDomain(prop, domainrsrc);
				}
			}
			else {
				throw new JenaProcessorException("Unhandled SadlProperty expression");
			}
			
			while (spitr.hasNext()) {
				SadlPropertyRestriction spr = spitr.next();
				if (spr instanceof SadlRangeRestriction) {
					RangeValueType rngValueType = RangeValueType.CLASS_OR_DT;	// default
					SadlTypeReference rng = ((SadlRangeRestriction)spr).getRange();
					if (rng instanceof SadlPrimitiveDataType) {
						String rngName = ((SadlPrimitiveDataType)rng).getPrimitiveType().getName();
						RDFNode rngNode = primitiveDatatypeToRDFNode(rngName);
						DatatypeProperty prop = null;
						if (!checkForExistingCompatibleDatatypeProperty(propUri, rngNode)) {
							prop = createDatatypeProperty(propUri, null);
							addPropertyRange(propType, prop, rngNode, rngValueType, rng);
						}
						else {
							prop = getTheJenaModel().getDatatypeProperty(propUri);
						}
						retProp = prop;
					}
					else {				
						OntResource rngRsrc = sadlTypeReferenceToOntResource(rng);
						if (rngRsrc == null) {
							throw new JenaProcessorException("Range failed to resolve to a class or datatype");
						}
						retProp = assignRangeToProperty(propUri, propType, rngRsrc, rngValueType, rng);
					}
				}
				else if (spr instanceof SadlCondition) {
					if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
						ObjectProperty prop = getOrCreateObjectProperty(propUri);
						OntClass condCls = sadlConditionToOntClass((SadlCondition) spr, prop, propType);
						addPropertyRange(propType, prop, condCls, RangeValueType.CLASS_OR_DT, spr);		// use default?
						//TODO don't we need to add this class as superclass??
						retProp = prop;
					}
					else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
						ObjectProperty prop = getOrCreateObjectProperty(propUri);
						OntClass condCls = sadlConditionToOntClass((SadlCondition) spr, prop, propType);
						//TODO don't we need to add this class as superclass??
						retProp = prop;
	//					throw new JenaProcessorException("SadlCondition on data type property not handled");
					}
					else {
						throw new JenaProcessorException("Invalid property type: " + propType.toString());
					}
				}
				else if (spr instanceof SadlTypeAssociation) {
					SadlTypeReference domain = ((SadlTypeAssociation)spr).getDomain();
					OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
					ObjectProperty prop = getOrCreateObjectProperty(propUri);
					if (domainrsrc != null) {
						addPropertyDomain(prop, domainrsrc);
					}
					SadlTypeReference from = element.getFrom();
					if (from != null) {
						OntResource fromrsrc = sadlTypeReferenceToOntResource(from);
						throw new JenaProcessorException("What is 'from'?");
					}
					SadlTypeReference to = element.getTo();
					if (to != null) {
						OntResource torsrc = sadlTypeReferenceToOntResource(to);
						throw new JenaProcessorException("What is 'to'?");
					}
				}
				else if (spr instanceof SadlIsAnnotation) {
					retProp = getTheJenaModel().createAnnotationProperty(propUri);
				}
				else if (spr instanceof SadlIsTransitive) {
					OntProperty pr = getOrCreateObjectProperty(propUri);
					pr.convertToTransitiveProperty();
					retProp = getTheJenaModel().createTransitiveProperty(pr.getURI());
				}
				else {
					throw new JenaProcessorException("Unhandled SadlPropertyRestriction type: " + spr.getClass().getCanonicalName());
				}
			} // end while
		}
		else {
			// No restrictions--this will become an rdf:Property
			retProp = createRdfProperty(propUri, null);
		}
		if (sr != null && retProp != null && sr.getAnnotations() != null && retProp.canAs(OntResource.class)) {
			addAnnotationsToResource(retProp.as(OntResource.class), sr.getAnnotations());
		}
		return retProp;
	}
	private EnumeratedClass sadlExplicitValuesToEnumeratedClass(EList<SadlExplicitValue> values)
			throws JenaProcessorException {
		List<RDFNode> nodevals = new ArrayList<RDFNode>();
		for (int i = 0; i < values.size(); i++) {
			SadlExplicitValue value = values.get(i);
			if (value instanceof SadlResource) {
				String uri = declarationExtensions.getConceptUri((SadlResource) value);
				com.hp.hpl.jena.rdf.model.Resource rsrc = getTheJenaModel().getResource(uri);
				if (rsrc.canAs(Individual.class)){
					nodevals.add(rsrc.as(Individual.class));
				}
				else {
					nodevals.add(rsrc);
				}
			}
			else {
				Literal litval = sadlExplicitValueToLiteral(value, null);
				nodevals.add(litval);
			}
		}
		RDFNode[] enumedArray = nodevals
				.toArray(new RDFNode[nodevals.size()]);
		RDFList rdfl = getTheJenaModel().createList(enumedArray);
		EnumeratedClass enumCls = getTheJenaModel().createEnumeratedClass(null, rdfl);
		return enumCls;
	}
	
	private Property assignRangeToProperty(String propUri, OntConceptType propType, OntResource rngRsrc,
			RangeValueType rngValueType, SadlTypeReference rng) throws JenaProcessorException {
		Property retProp;
		if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
			OntClass rngCls = rngRsrc.asClass();
			ObjectProperty prop = getOrCreateObjectProperty(propUri);
			addPropertyRange(propType, prop, rngCls, rngValueType, rng);
			retProp = prop;
		}
		else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
			DatatypeProperty prop = getOrCreateDatatypeProperty(propUri);
			addPropertyRange(propType, prop, rngRsrc, rngValueType, rng);
			retProp = prop;
		}
		else if (propType.equals(OntConceptType.RDF_PROPERTY)) {
			OntProperty prop = getOrCreateRdfProperty(propUri);
			addPropertyRange(propType, prop, rngRsrc, rngValueType, rng);
//			getTheJenaModel().add(prop, RDFS.range, rngRsrc);
			retProp = prop;
		}
		else {
			throw new JenaProcessorException("Property '" + propUri + "' of unexpected type '" + rngRsrc.toString() + "'");
		}
		return retProp;
	}

	private SadlResource sadlResourceFromSadlProperty(SadlProperty element) {
		SadlResource sr = element.getNameOrRef();
		if (sr == null) {
			sr = element.getProperty();
		}
		if (sr == null) {
			sr = element.getNameDeclarations().iterator().next();
		}
		return sr;
	}

	private void addPropertyRange(OntConceptType propType, OntProperty prop, RDFNode rngNode, RangeValueType rngValueType, EObject context) throws JenaProcessorException {
		if (context instanceof SadlSimpleTypeReference && ((SadlSimpleTypeReference)context).isList()) {
			rngValueType = RangeValueType.LIST;
			setImportSadlListModel(true);
		}
		else if (context instanceof SadlPrimitiveDataType && ((SadlPrimitiveDataType)context).isList()) {
			rngValueType = RangeValueType.LIST;
			setImportSadlListModel(true);
		}
		RDFNode propOwlType = null;
		boolean existingRangeAnywhere = false;
		boolean existingRangeThisModel = false;
		StmtIterator existingRngItr = getTheJenaModel().listStatements(prop, RDFS.range, (RDFNode)null);
		while (existingRngItr.hasNext()) {
			existingRangeAnywhere = true;
			RDFNode existingRngNode = existingRngItr.next().getObject();
			if (rngNode.equals(existingRngNode)) {
				// do nothing-- rngNode is already in range
				return;
			}
			
			if (existingRngNode.canAs(OntClass.class)){
				// is the new range a subclass of the existing range, or vice versa?
				if (rngNode.canAs(OntClass.class) && checkForSubclassing(rngNode.as(OntClass.class), existingRngNode.as(OntClass.class), context)) {
					StringBuilder sb = new StringBuilder("This range is a subclass of the range which is already defined");
					String existingRange = nodeToString(existingRngNode);
					if (existingRange != null) {
						sb.append(" (");
						sb.append(existingRange);
						sb.append(") ");
					}
					sb.append("; perhaps you meant to restrict values of this property on this class with an 'only has values of type' restriction?");
					addWarning(sb.toString(), context);		
					return;
				}
			}
		}

		StmtIterator inModelStmtItr = getTheJenaModel().getBaseModel().listStatements(prop, RDFS.range, (RDFNode)null);
		if (inModelStmtItr.hasNext()) {
			existingRangeThisModel = true;
		}
		if (existingRangeAnywhere && !existingRangeThisModel) {
			addWarning("This changes the range of property '" + prop.getURI() + "' which has an imported range; are you sure that's what you want to do?", context);
		}
		if (existingRangeAnywhere && existingRangeThisModel) {
			// property has existing range known to this model
			if (prop.isDatatypeProperty()) {
				String existingRange = stmtIteratorToObjectString(getTheJenaModel().listStatements(prop, RDFS.range, (RDFNode)null));
				addError("Data type property '" + prop.getURI() + "' has range '" + existingRange + "' so can't assign range '" + rngNode.toString(), context);
			}
			else {
				if (rngNode.isResource() && rngNode.asResource().canAs(OntClass.class)) {
					OntClass newRngCls;
					if (rngValueType.equals(RangeValueType.LIST)) {
						newRngCls = createListSubclass(null, rngNode.toString(), context.eResource());
						// TODO this should be removed as soon as translators are updated to new List ontology representation
						AnnotationProperty annprop = getTheJenaModel().createAnnotationProperty(LIST_RANGE_ANNOTATION_PROPERTY);
						prop.addProperty(annprop, RangeValueType.LIST.toString());
					}
					else {
						newRngCls = rngNode.asResource().as(OntClass.class);
					}
					ExtendedIterator<? extends OntResource> ritr = prop.listRange();
					if (updateObjectPropertyRange(prop, newRngCls, ritr, rngValueType, context)) {
						propOwlType = OWL.ObjectProperty;
					}
					ritr.close();
				}
				else {
					throw new JenaProcessorException("Unable to convert object property range to a class");
				}
			}
		}
		else {
			// no existing range--this is a fresh range assignment
			if (rngValueType.equals(RangeValueType.LIST)) {
				// range is a List
				prop.addRange(createListSubclass(null, rngNode.toString(), context.eResource()));
				// TODO this should be removed as soon as translators are updated to new List ontology representation
				AnnotationProperty annprop = getTheJenaModel().createAnnotationProperty(LIST_RANGE_ANNOTATION_PROPERTY);
				prop.addProperty(annprop, RangeValueType.LIST.toString());
				propOwlType = OWL.ObjectProperty;
			}
			else {
				// range is not a List
				com.hp.hpl.jena.rdf.model.Resource rngrsrc = rngNode.asResource();
				if (rngrsrc.hasProperty(RDF.type, RDFS.Datatype)) {
					propOwlType = OWL.DatatypeProperty;
				}
				else if (rngrsrc.canAs(OntClass.class)){
					propOwlType = OWL.ObjectProperty;					
				}
				else {
					propOwlType = OWL.DatatypeProperty;
				}
				getTheJenaModel().add(prop, RDFS.range, rngrsrc);
			}
			if (logger.isDebugEnabled()) {
				StringBuffer sb = new StringBuffer();
				sb.append(rngNode.toString());
				if (rngValueType.equals(RangeValueType.LIST)) {
					sb.append(" List");
				}
				logger.debug("Range '" + sb.toString() + "' given to property '" + prop.toString() + "'");
			}
		}
		if (propType.equals(OntConceptType.RDF_PROPERTY) && propOwlType != null) {
			getTheJenaModel().add(prop, RDF.type, propOwlType);
		}
	}

	private String stmtIteratorToObjectString(StmtIterator stmtitr) {
		StringBuilder sb = new StringBuilder();
		while (stmtitr.hasNext()) {
			RDFNode obj = stmtitr.next().getObject();
			if (sb.length() > 0) {
				sb.append(", ");
			}
			sb.append(nodeToString(obj));
		}
		return sb.toString();
	}
	
	private String nodeToString(RDFNode obj) {
		StringBuilder sb = new StringBuilder();
		if (obj.isURIResource()) {
			sb.append(obj.toString());
		}
		else if (obj.canAs(UnionClass.class)){
			UnionClass ucls = obj.as(UnionClass.class);
			ExtendedIterator<RDFNode> uitr = ucls.getOperands().iterator();
			sb.append("(");
			while (uitr.hasNext()) {
				if (sb.length() > 1) {
					sb.append(" or ");
				}
				sb.append(nodeToString(uitr.next()));
			}
			sb.append(")");
		}
		else if (obj.canAs(IntersectionClass.class)){
			IntersectionClass icls = obj.as(IntersectionClass.class);
			ExtendedIterator<RDFNode> iitr = icls.getOperands().iterator();
			sb.append("(");
			while (iitr.hasNext()) {
				if (sb.length() > 1) {
					sb.append(" and ");
				}
				sb.append(nodeToString(iitr.next()));
			}
			sb.append(")");
		}
		else {
			sb.append("<blank node>");
		}
		return sb.toString();
	}
	
	private boolean checkForSubclassing(OntClass rangeCls, OntResource existingRange, EObject context) throws JenaProcessorException {
		// this is changing the range of a property defined in a different model
		try {
			if (SadlUtils.classIsSubclassOf((OntClass) rangeCls, existingRange, true, null)) {
				return true;
			}
		} catch (CircularDependencyException e) {
			throw new JenaProcessorException(e.getMessage(), e);
		}
		return false;
	}
	
	private boolean updateObjectPropertyRange(OntProperty prop, OntResource rangeCls, ExtendedIterator<? extends OntResource> ritr, RangeValueType rngValueType, EObject context) throws JenaProcessorException {
		boolean retval = false;
		if (rangeCls != null) {
			OntResource newRange = createUnionOfClasses(rangeCls, ritr);
			if (newRange != null) {
				if (newRange.equals(rangeCls)) {
					return retval;	// do nothing--the rangeCls is already in the range
				}
			}
			if (prop.getRange() != null) {
				// remove existing range in this model
				prop.removeRange(prop.getRange());
			}
			prop.addRange(newRange); 
			retval = true;
		} else {
			addError("Range to be assigned is null.", context);
		}
		return retval;
	}

	private void addError(String msg, EObject context) {
		if (getIssueAcceptor() != null) {
			getIssueAcceptor().addError(msg, context);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
		}
		else  if (!generationInProgress){
			System.err.println(msg);
		}
	}

	private void addWarning(String msg, EObject context) {
		if (getIssueAcceptor() != null) {
			getIssueAcceptor().addWarning(msg, context);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.WARNING_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
		}
		else if (!generationInProgress) {
			System.out.println(msg);			
		}
	}

	private OntResource addClassToUnionClass(OntResource existingCls,
			OntResource cls) throws JenaProcessorException {
		if (existingCls != null && !existingCls.equals(cls)) {
			try {
				if (existingCls.canAs(OntClass.class) && SadlUtils.classIsSubclassOf(existingCls.as(OntClass.class), cls, true, null)) {
					return cls;
				}
				else if (cls.canAs(OntClass.class) && SadlUtils.classIsSubclassOf(cls.as(OntClass.class), existingCls, true, null)) {
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
						classes = getTheJenaModel().createList();
						OntResource inCurrentModel = null;
						if (existingCls.isURIResource()) {
							inCurrentModel = getTheJenaModel().getOntResource(existingCls.getURI());
						}
						if (inCurrentModel != null) {
							classes = classes.with(inCurrentModel);
						}
						else {
							classes = classes.with(existingCls);
						}
						classes = classes.with(cls);
					}
					OntResource unionClass = getTheJenaModel().createUnionClass(null,
							classes);
					return unionClass;
				}
			} catch (CircularDependencyException e) {
				throw new JenaProcessorException(e.getMessage(), e);
			}
		} else {
			return cls;
		}
	}

	private OntResource createUnionClassInCurrentModel(OntResource existingCls, OntResource cls) throws JenaProcessorException {
		if (existingCls != null && !existingCls.equals(cls)) {
			try {
				if (existingCls.canAs(OntClass.class) && SadlUtils.classIsSubclassOf(existingCls.as(OntClass.class), cls, true, null)) {
					return cls;
				}
				else if (cls.canAs(OntClass.class) && SadlUtils.classIsSubclassOf(cls.as(OntClass.class), existingCls, true, null)) {
					return existingCls;
				}
				else {
					RDFList classes = null;
					if (existingCls.canAs(UnionClass.class)) {
						try {
							classes = getTheJenaModel().createList();
							UnionClass ucls = existingCls.as(UnionClass.class);
							RDFList members = ucls.getOperands();
							for (int i = 0; i < members.size(); i++) {
								RDFNode mcls = members.get(i);
								if (mcls.canAs(OntResource.class)) {
									if (mcls.asResource().isURIResource()) {
										OntResource inCurrentModel = getTheJenaModel().getOntResource(mcls.as(OntResource.class).getURI());
										classes = classes.with(inCurrentModel);
									}
									else {
										throw new JenaProcessorException("Nested union classes not yet supported");
									}
								}
							}
							classes = classes.with(cls);
						} catch (Exception e) {
							// don't know why this is happening
							logger.error("Union class error that hasn't been resolved or understood.");
							return cls;
						}
					} else {
						if (cls.equals(existingCls)) {
							return existingCls;
						}
						classes = getTheJenaModel().createList();
						OntResource inCurrentModel = null;
						if (existingCls.isURIResource()) {
							inCurrentModel = getTheJenaModel().getOntResource(existingCls.getURI());
						}
						if (inCurrentModel != null) {
							classes = classes.with(inCurrentModel);
						}
						else {
							classes = classes.with(existingCls);
						}
						classes = classes.with(cls);
					}
					OntResource unionClass = getTheJenaModel().createUnionClass(null,
							classes);
					return unionClass;
				}
			} catch (CircularDependencyException e) {
				throw new JenaProcessorException(e.getMessage(), e);
			}
		} else {
			return cls;
		}
	}

	private void processSadlNecessaryAndSufficient(SadlNecessaryAndSufficient element) throws JenaProcessorException {
		OntClass supercls = sadlTypeReferenceToOntResource(element.getSubject()).asClass();
		OntClass rolecls = getOrCreateOntClass(declarationExtensions.getConcreteName(element.getObject()));
		Iterator<SadlPropertyCondition> itr = element.getPropConditions().iterator();
		List<OntClass> conditionClasses = new ArrayList<OntClass>();
		while (itr.hasNext()) {
			SadlPropertyCondition nxt = itr.next();
			conditionClasses.add(processSadlPropertyCondition(nxt));
		}
		// we have all the parts--create the equivalence class
		if (conditionClasses.size() == 1) {
			if (supercls != null && conditionClasses.get(0) != null) {
				IntersectionClass eqcls = createIntersectionClass(supercls, conditionClasses.get(0));
				rolecls.setEquivalentClass(eqcls);
				logger.debug("New intersection class created as equivalent of '" + rolecls.getURI() + "'");
			} else if (conditionClasses.get(0) != null) {
				rolecls.setEquivalentClass(conditionClasses.get(0));
				logger.debug("Equivalent class set for '" + rolecls.getURI() + "'");
			}
			else {
				throw new JenaProcessorException("Necessary and sufficient conditions appears to have invalid input.");
			}
		}
		else {
			int base = supercls != null ? 1 : 0;
			RDFNode[] memberList = new RDFNode[base + conditionClasses.size()];
			if (base > 0) {
				memberList[0] = supercls;
			}
			for (int i = 0; i < conditionClasses.size(); i++) {
				memberList[base + i] = conditionClasses.get(i);
			}
			IntersectionClass eqcls = createIntersectionClass(memberList);
			rolecls.setEquivalentClass(eqcls);
			logger.debug("New intersection class created as equivalent of '" + rolecls.getURI() + "'");
		}
	}

	private void processSadlDifferentFrom(SadlDifferentFrom element) throws JenaProcessorException {
		List<Individual> differentFrom = new ArrayList<Individual>();
		Iterator<SadlClassOrPropertyDeclaration> dcitr = element.getTypes().iterator();
		while(dcitr.hasNext()) {
			SadlClassOrPropertyDeclaration decl = dcitr.next();
			Iterator<SadlResource> djitr = decl.getClassOrProperty().iterator();
			while (djitr.hasNext()) {
				SadlResource sr = djitr.next();
				String declUri = declarationExtensions.getConceptUri(sr);
				if (declUri == null) {
					throw new JenaProcessorException("Failed to get concept URI for SadlResource in processSadlDifferentFrom");
				}
				Individual inst = getTheJenaModel().getIndividual(declUri);
				differentFrom.add(inst);
			}
		}
		SadlTypeReference nsas = element.getNotTheSameAs();
		if (nsas != null) {
			OntResource nsasrsrc = sadlTypeReferenceToOntResource(nsas);
			differentFrom.add(nsasrsrc.asIndividual());
			SadlResource sr = element.getNameOrRef();
			Individual otherInst = getTheJenaModel().getIndividual(declarationExtensions.getConceptUri(sr));
			differentFrom.add(otherInst);
		}
		RDFNode[] nodeArray = null;
		if (differentFrom.size() > 0) {
			nodeArray = differentFrom.toArray(new Individual[differentFrom.size()]);
		}
		else {
			throw new JenaProcessorException("Unexpect empty array in processSadlDifferentFrom");
		}
		RDFList differentMembers = getTheJenaModel().createList(nodeArray);
		getTheJenaModel().createAllDifferent(differentMembers);
		logger.debug("New all different from created");
	}

	private Individual processSadlInstance(SadlInstance element) throws JenaProcessorException {
		// this has two forms:
		//	1) <name> is a <type> ...
		//	2) a <type> <name> ....
		SadlTypeReference type = element.getType();
		boolean isList = typeRefIsList(type);
		SadlResource sr = sadlResourceFromSadlInstance(element);
		String instUri = null;
		if (sr != null) {
			instUri = declarationExtensions.getConceptUri(sr);
			if (instUri == null) {
				throw new JenaProcessorException("Failed to get concept URI of SadlResource in processSadlInstance");
			}
		}
		OntClass cls = null;
		if (type != null) {
			if (type instanceof SadlPrimitiveDataType) {
				com.hp.hpl.jena.rdf.model.Resource rsrc = sadlTypeReferenceToResource(type);
				if (isList) {
					try {
						cls = createListSubclass(null, rsrc.getURI(), type.eResource());
					} catch (JenaProcessorException e) {
						addError(e.getMessage(), type);
					}
				}
			}
			else {
				OntResource or = sadlTypeReferenceToOntResource(type);
				if (or != null) {
					cls = or.asClass();
					if (isList) {
						try {
							cls = createListSubclass(null, cls.getURI(), type.eResource());
						} catch (JenaProcessorException e) {
							addError(e.getMessage(), type);
						}
					}
				}
			}
		} 
		Individual inst;
		if (cls != null) {
			if (sr != null) {
				inst = createIndividual(instUri, cls);
			}
			else {
				inst = createIndividual((String)null, cls);
			}
		}
		else if (instUri != null) {
			inst = createIndividual(instUri, (OntClass)null);
		}
		else {
			throw new JenaProcessorException("Can't create an unnamed instance with no class given");
		}
		
		Iterator<SadlPropertyInitializer> itr = element.getPropertyInitializers().iterator();
		while (itr.hasNext()) {
			SadlPropertyInitializer propinit = itr.next();
			SadlResource prop = propinit.getProperty();
			EObject val = propinit.getValue();
			if (val != null) {
				assignInstancePropertyValue(inst, cls, prop, val);
			}
			else if (val == null) {
				throw new JenaProcessorException("no value found");
			}
			else {
				throw new JenaProcessorException("unhandled SadlPropertyInitializer, value is null");
			}
		}
		SadlValueList listInitializer = element.getListInitializer();
		if (listInitializer != null) {
			addListValues(inst, cls, listInitializer);
		}
		return inst;
	}

	private boolean typeRefIsList(SadlTypeReference type) throws JenaProcessorException {
		boolean isList = false;
		if (type instanceof SadlSimpleTypeReference) {
			isList = ((SadlSimpleTypeReference)type).isList();
		}
		else if (type instanceof SadlPrimitiveDataType) {
			isList = ((SadlPrimitiveDataType)type).isList();
		}
		if (isList) {
			importSadlListModel(type.eResource());			
		}
		return isList;
	}
	
	private void addListValues(Individual inst, OntClass cls, SadlValueList listInitializer) {
		com.hp.hpl.jena.rdf.model.Resource to = null;
		ExtendedIterator<OntClass> scitr = cls.listSuperClasses(true);
		while (scitr.hasNext()) {
			OntClass sc = scitr.next(); 
			if (sc.isRestriction() && ((sc.as(Restriction.class)).isAllValuesFromRestriction() && 
					sc.as(AllValuesFromRestriction.class).onProperty(getTheJenaModel().getProperty(SADL_LIST_MODEL_FIRST_URI)))) {
				to = sc.as(AllValuesFromRestriction.class).getAllValuesFrom();
				break;
			}
		}
		if (to == null) {
//			addError("No 'to' resource found in restriction of List subclass", listInitializer);
		}
		Iterator<SadlExplicitValue> values = listInitializer.getExplicitValues().iterator();
		addValueToList(null, inst, cls, to, values);
	}
	
	private Individual addValueToList(Individual lastInst, Individual inst, OntClass cls, com.hp.hpl.jena.rdf.model.Resource type, 
			Iterator<SadlExplicitValue> valueIterator) {
		if (inst == null) {
			inst = getTheJenaModel().createIndividual(cls);
		}
		SadlExplicitValue val = valueIterator.next();
		if (val instanceof SadlResource) {
			Individual listInst;
			try {
				listInst = createIndividual((SadlResource)val, ((OntResource)type).as(OntClass.class));
				ExtendedIterator<com.hp.hpl.jena.rdf.model.Resource> itr = listInst.listRDFTypes(false);
				boolean match = false;
				while (itr.hasNext()) {
					com.hp.hpl.jena.rdf.model.Resource typ = itr.next();
					if (typ.equals(type)) {
						match = true;
					}
				}
				if (!match) {
					addError("The Instance '" + listInst.toString() + "' doesn't match the List type.", val);
				}
				getTheJenaModel().add(inst, getTheJenaModel().getProperty(SADL_LIST_MODEL_FIRST_URI), listInst);
			} catch (JenaProcessorException e) {
				addError(e.getMessage(), val);
			} catch (TranslationException e) {
				addError(e.getMessage(), val);
			}
		}
		else {
			Literal lval;
			try {
				lval = sadlExplicitValueToLiteral((SadlExplicitValue)val, type);
				getTheJenaModel().add(inst, getTheJenaModel().getProperty(SADL_LIST_MODEL_FIRST_URI), lval);
			} catch (JenaProcessorException e) {
				addError(e.getMessage(), val);
			}
		}
		if (valueIterator.hasNext()) {
			Individual rest = addValueToList(inst, null, cls, type, valueIterator);
			getTheJenaModel().add(inst, getTheJenaModel().getProperty(SADL_LIST_MODEL_REST_URI), rest);
		}
		return inst;
	}
	
	private void assignInstancePropertyValue(Individual inst, OntClass cls, SadlResource prop, EObject val) throws JenaProcessorException {
		OntConceptType type;
		try {
			type = declarationExtensions.getOntConceptType(prop);
		} catch (CircularDefinitionException e) {
			type = e.getDefinitionType();
			addError(e.getMessage(), prop);
		}
		String propuri = declarationExtensions.getConceptUri(prop);
		if (type.equals(OntConceptType.CLASS_PROPERTY)) {
			ObjectProperty oprop = getTheJenaModel().getObjectProperty(propuri);
			if (oprop == null) {
				addError("Property '" + propuri + "' does not exist in the Jena model", prop);
			}
			else {
				if (val instanceof SadlInstance) {
					Individual instval = processSadlInstance((SadlInstance) val);
					inst.addProperty(oprop, instval);
					logger.debug("added value '" + instval.toString() + "' to property '" + propuri + "' for instance '" + inst.toString() + "'");
				}
				else if (val instanceof SadlResource) {
					String uri = declarationExtensions.getConceptUri((SadlResource) val);
					com.hp.hpl.jena.rdf.model.Resource rsrc = getTheJenaModel().getResource(uri);
					if (rsrc.canAs(Individual.class)){
						inst.addProperty(oprop, rsrc.as(Individual.class));
					}
					else {
						throw new JenaProcessorException("unhandled value type SadlResource that isn't an instance (URI is '" + uri + "')");
					}
				}
				else if (val instanceof SadlExplicitValue) {
					OntResource rng = oprop.getRange();
					if (rng == null) {
						// this isn't really an ObjectProperty--should probably be an rdf:Property
						Literal lval = sadlExplicitValueToLiteral((SadlExplicitValue)val, null);
						inst.addProperty(oprop, lval);
					}
					else {
						addError("A SadlExplicitValue is given to an an ObjectProperty", val);
					}
				}
				else if (val instanceof SadlValueList) {
					EList<SadlExplicitValue> vals = ((SadlValueList)val).getExplicitValues();
					addListValues(inst, cls, (SadlValueList) val);
				}
				else {
					throw new JenaProcessorException("unhandled value type for object property");
				}
			}
		}
		else if (type.equals(OntConceptType.DATATYPE_PROPERTY)) {
			DatatypeProperty dprop = getTheJenaModel().getDatatypeProperty(propuri);
			if (dprop == null) {
//				dumpModel(getTheJenaModel());
				addError("Property '" + propuri + "' does not exist in the Jena model", prop);
			}
			else {
				if (val instanceof SadlValueList) {
					EList<SadlExplicitValue> vals = ((SadlValueList)val).getExplicitValues();
					addListValues(inst, cls, (SadlValueList) val);
				}
				else if (val instanceof SadlExplicitValue) {
					Literal lval = sadlExplicitValueToLiteral((SadlExplicitValue)val, dprop.getRange());
					if (lval != null) {
						inst.addProperty(dprop, lval);
						logger.debug("added value '" + lval.toString() + "' to property '" + propuri + "' for instance '" + inst.toString() + "'");
					}
				}
				else {
					throw new JenaProcessorException("unhandled value type for data property");
				}
			}
		}
		else if (type.equals(OntConceptType.ANNOTATION_PROPERTY)) {
			AnnotationProperty annprop = getTheJenaModel().getAnnotationProperty(propuri);
			if (annprop == null) {
				addError("Annotation property '" + propuri + "' not found in Jena model", prop);
			}
			else {
				RDFNode rsrcval;
				if (val instanceof SadlResource) {
					String uri = declarationExtensions.getConceptUri((SadlResource) val);
					 rsrcval = getTheJenaModel().getResource(uri);
				}
				else if (val instanceof SadlInstance) {
					rsrcval = processSadlInstance((SadlInstance) val);
				}
				else if (val instanceof SadlExplicitValue) {
					rsrcval = sadlExplicitValueToLiteral((SadlExplicitValue)val, null);
				}
				else {
					throw new JenaProcessorException("unable to handle annotation value of type '" + val.getClass().getCanonicalName() + "')");
				}
				inst.addProperty(annprop, rsrcval);
			}
		}
		else if (type.equals(OntConceptType.RDF_PROPERTY)) {
			Property rdfprop = getTheJenaModel().getProperty(propuri);
			if (rdfprop == null) {
				addError("RDF property '" + propuri + "' not found in Jena Model", prop);
			}
			RDFNode rsrcval;
			if (val instanceof SadlResource) {
				String uri = declarationExtensions.getConceptUri((SadlResource) val);
				 rsrcval = getTheJenaModel().getResource(uri);
			}
			else if (val instanceof SadlInstance) {
				rsrcval = processSadlInstance((SadlInstance) val);
			}
			else if (val instanceof SadlExplicitValue) {
				rsrcval = sadlExplicitValueToLiteral((SadlExplicitValue)val, null);
			}
			else {
				throw new JenaProcessorException("unable to handle rdf property value of type '" + val.getClass().getCanonicalName() + "')");
			}
			inst.addProperty(rdfprop, rsrcval);
		}
		else if (type.equals(OntConceptType.VARIABLE)) {
			// a variable for a property type is only valid in a rule or query.
		}
		else {
			throw new JenaProcessorException("unhandled property type");
		}
	}

	private void dumpModel(OntModel m) {
		System.out.println("Dumping OntModel");
		PrintStream strm = System.out;
		m.write(strm);
		ExtendedIterator<OntModel> itr = m.listSubModels();
		while (itr.hasNext()) {
			dumpModel(itr.next());
		}
	}
	
	private SadlResource sadlResourceFromSadlInstance(SadlInstance element) throws JenaProcessorException {
		SadlResource sr = element.getNameOrRef();
		if (sr == null) {
			return element.getInstance();
		}
		return sr;
	}

	private void processSadlDisjointClasses(SadlDisjointClasses element) throws JenaProcessorException {
		List<OntClass> disjointClses = new ArrayList<OntClass>();
		if (element.getClasses() != null) {
			Iterator<SadlResource> dcitr = element.getClasses().iterator();
			while (dcitr.hasNext()) {
				SadlResource sr = dcitr.next();
				String declUri = declarationExtensions.getConceptUri(sr);
				if (declUri == null) {
					throw new JenaProcessorException("Failed to get concept URI of SadlResource in processSadlDisjointClasses");
				}
				OntClass cls = getTheJenaModel().getOntClass(declUri);
				if (cls == null) {
					throw new JenaProcessorException("Failed to get class '" + declUri + "' from Jena model.");
				}
				disjointClses.add(cls.asClass());
			}
		}
		Iterator<SadlClassOrPropertyDeclaration> dcitr = element.getTypes().iterator();
		while(dcitr.hasNext()) {
			SadlClassOrPropertyDeclaration decl = dcitr.next();
			Iterator<SadlResource> djitr = decl.getClassOrProperty().iterator();
			while (djitr.hasNext()) {
				SadlResource sr = djitr.next();
				String declUri = declarationExtensions.getConceptUri(sr);
				if (declUri == null) {
					throw new JenaProcessorException("Failed to get concept URI of SadlResource in processSadlDisjointClasses");
				}
				OntClass cls = getTheJenaModel().getOntClass(declUri);
				disjointClses.add(cls.asClass());
			}
		}
		// must set them disjoint pairwise
		for (int i = 0; i < disjointClses.size(); i++) {
			for (int j = i + 1; j < disjointClses.size(); j++) {
				disjointClses.get(i).addDisjointWith(disjointClses.get(j));
			}
		}
	}

	private ObjectProperty getOrCreateObjectProperty(String propName) {
		ObjectProperty prop = getTheJenaModel().getObjectProperty(propName);
		if (prop == null) {
			prop = getTheJenaModel().createObjectProperty(propName);
			logger.debug("New object property '" + prop.getURI() + "' created");
		}
		return prop;
	}

	private DatatypeProperty getOrCreateDatatypeProperty(String propUri) throws JenaProcessorException {
		DatatypeProperty prop = getTheJenaModel().getDatatypeProperty(propUri);
		if (prop == null) {
			prop = createDatatypeProperty(propUri, null);
		}
		return prop;
	}
	
	private OntProperty getOrCreateRdfProperty(String propUri) {
		Property op = getTheJenaModel().getProperty(propUri);
		if (op != null) {
			return op.as(OntProperty.class);
		}
		return createRdfProperty(propUri, null);
	}

	private boolean checkForExistingCompatibleDatatypeProperty(
			String propUri, RDFNode rngNode) {
		DatatypeProperty prop = getTheJenaModel().getDatatypeProperty(propUri);
		if (prop != null) {
			OntResource rng = prop.getRange();
			if (rng != null && rng.equals(rngNode)) {
				return true;
			}
		}
		return false;
	}

	private OntProperty createOntPropertyInCurrentModel(OntProperty prop) {
		if (!prop.getNameSpace().equals(getModelNamespace())) {
			if (prop.isDatatypeProperty()) {
				prop = getTheJenaModel().createDatatypeProperty(prop.getURI());
			}
			else {
				prop = getTheJenaModel().createObjectProperty(prop.getURI());
			}
		}
		return prop;
	}

	private void addPropertyDomain(Property prop, OntResource cls) throws JenaProcessorException {
		boolean existingDomainAnywhere = false;
		boolean existingDomainThisModel = false;
		StmtIterator sitr = getTheJenaModel().listStatements(prop, RDFS.domain, (RDFNode)null);
		if (sitr.hasNext()) {
			existingDomainAnywhere = true;
			// property already has a domain known to this model
			StmtIterator inModelStmtItr = getTheJenaModel().getBaseModel().listStatements(prop, RDFS.domain, (RDFNode)null);
			if (inModelStmtItr.hasNext()) {
				existingDomainThisModel = true;
			}
			ExtendedIterator<? extends OntResource> ditr = prop.as(OntProperty.class).listDomain();
			if (existingDomainThisModel && ditr.hasNext()) {
				// domain is already specified in this model
				OntResource newCls = createUnionOfClasses(cls, ditr);
				ditr.close();
				if (newCls != null) {
					if (newCls.equals(cls)) {
						return;		// do nothing--the cls is already in domain
					}
					cls = newCls;
					getTheJenaModel().remove(getTheJenaModel().listStatements(prop, RDFS.domain, (RDFNode)null));
				}
			}
			else {
				// check to see if this is something new
				while (sitr.hasNext()) {
					RDFNode existingDomain = sitr.next().getObject();
					if (existingDomain.equals(cls)) {
						sitr.close();
						return;	// already in domain, nothing to add
					}
				}
			}
		}
		getTheJenaModel().add(prop, RDFS.domain, cls);
		logger.debug("Domain '" + cls.toString() + "' added to property '" + prop.getURI() + "'");
	}

	private OntResource createUnionOfClasses(OntResource cls, ExtendedIterator<? extends OntResource> ditr) throws JenaProcessorException {
		OntResource unionClass = null;
		RDFList classes = null;
		boolean allEqual = true;
		while (ditr.hasNext()) {
			OntResource existingCls = ditr.next();
			if (!existingCls.canAs(OntResource.class)){
				throw new JenaProcessorException("Unable to '" + existingCls.toString() + "' to OntResource to put into union of classes");
			}
			if (existingCls.equals(cls)) {
				continue;
			}
			else {
				allEqual = false;
			}
			if (existingCls.as(OntResource.class).canAs(UnionClass.class)) {
				try {
					existingCls.as(UnionClass.class).addOperand(cls);
					return existingCls.as(UnionClass.class);
				} catch (Exception e) {
					// don't know why this is happening
					logger.error("Union class error that hasn't been resolved or understood.");					
					return cls;
				}
			} else {
				if (classes == null) {
					classes = getTheJenaModel().createList();
				}
				classes = classes.with(existingCls.as(OntResource.class));
				classes = classes.with(cls);
				unionClass = getTheJenaModel().createUnionClass(null, classes);
			}
		}
		if (allEqual) {
			return cls;
		}
		return unionClass;
	}
	
	private RDFNode primitiveDatatypeToRDFNode(String name) {
		return getTheJenaModel().getResource(XSD.getURI() + name);
	}

	private OntClass getOrCreateOntClass(String name) {
		OntClass cls = getTheJenaModel().getOntClass(name);
		if (cls == null) {
			cls = createOntClass(name, (OntClass)null);
		}
		return cls;
	}
	
	private OntClass createOntClass(String newName, String superSRUri, EObject superSR) {
		if (superSRUri != null) {
			OntClass superCls = getTheJenaModel().getOntClass(superSRUri);
			if (superCls == null) {
				superCls = getTheJenaModel().createClass(superSRUri);
			}
			return createOntClass(newName, superCls);
		}
		return createOntClass(newName, (OntClass)null);
	}
	
	private OntClass createOntClass(String newName, OntClass superCls) {
		OntClass newCls = getTheJenaModel().createClass(newName);
		logger.debug("New class '" + newCls.getURI() + "' created");
		if (superCls != null) {
			newCls.addSuperClass(superCls);
			logger.debug("    Class '" + newCls.getURI() + "' given super class '" + superCls.toString() + "'");
		}
		return newCls;
	}
	
	private OntClass createListSubclass(String newName, String typeUri, Resource resource) throws JenaProcessorException {
		if (sadlListModel == null) {
			try {
				importSadlListModel(resource);
			} catch (Exception e) {
				e.printStackTrace();
				throw new JenaProcessorException("Failed to load SADL List model", e);
			}
			setImportSadlListModel(true); 
		}
		OntClass lstcls = getTheJenaModel().getOntClass(SADL_LIST_MODEL_LIST_URI);
		OntClass newcls =  createOntClass(newName, lstcls);
		com.hp.hpl.jena.rdf.model.Resource typeResource = getTheJenaModel().getResource(typeUri);
		Property pfirst = getTheJenaModel().getProperty(SADL_LIST_MODEL_FIRST_URI);
		AllValuesFromRestriction avf = getTheJenaModel().createAllValuesFromRestriction(null, pfirst, typeResource);
		newcls.addSuperClass(avf);
		Property prest = getTheJenaModel().getProperty(SADL_LIST_MODEL_REST_URI);
		AllValuesFromRestriction avf2 = getTheJenaModel().createAllValuesFromRestriction(null, prest, newcls);
		newcls.addSuperClass(avf2);
		return newcls;
	}
	
	private OntProperty createObjectProperty(String newName, String superSRUri) throws JenaProcessorException {
		OntProperty newProp = getTheJenaModel().createObjectProperty(newName);
		logger.debug("New object property '" + newProp.getURI() + "' created");
		if (superSRUri != null) {
			OntProperty superProp = getTheJenaModel().getOntProperty(superSRUri);
			if (superProp == null) {
//				throw new JenaProcessorException("Unable to find super property '" + superSRUri + "'");
				getTheJenaModel().createObjectProperty(superSRUri);
			}
			newProp.addSuperProperty(superProp);
			logger.debug("   Object property '" + newProp.getURI() + "' given super property '" + superSRUri + "'");
		}
		return newProp;
	}
	
	private AnnotationProperty createAnnotationProperty(String newName, String superSRUri) {
		AnnotationProperty annProp = getTheJenaModel().createAnnotationProperty(newName);
		logger.debug("New annotation property '" + annProp.getURI() + "' created");
		if (superSRUri != null) {
			Property superProp = getTheJenaModel().getProperty(superSRUri);
			if (superProp == null) {
				superProp = getTheJenaModel().createOntProperty(superSRUri);
			}
			getTheJenaModel().add(annProp, RDFS.subPropertyOf, superProp);
			logger.debug("   Property '" + annProp.getURI() + "' given super property '" + superSRUri + "'");
		}
		return annProp;
	}
	
	private OntProperty createRdfProperty(String newName, String superSRUri) {
		OntProperty newProp = getTheJenaModel().createOntProperty(newName);
		logger.debug("New object property '" + newProp.getURI() + "' created");
		if (superSRUri != null) {
			Property superProp = getTheJenaModel().getProperty(superSRUri);
			if (superProp == null) {
				superProp = getTheJenaModel().createOntProperty(superSRUri);
			}
			getTheJenaModel().add(newProp, RDFS.subPropertyOf, superProp);
			logger.debug("   Property '" + newProp.getURI() + "' given super property '" + superSRUri + "'");
		}
		return newProp;
	}
	
	private DatatypeProperty createDatatypeProperty(String newName, String superSRUri) throws JenaProcessorException {
		DatatypeProperty newProp = getTheJenaModel().createDatatypeProperty(newName);
		logger.debug("New datatype property '" + newProp.getURI() + "' created");
		if (superSRUri != null) {
			OntProperty superProp = getTheJenaModel().getOntProperty(superSRUri);
			if (superProp == null) {
//				throw new JenaProcessorException("Unable to find super property '" + superSRUri + "'");
				if (superProp == null) {
					getTheJenaModel().createDatatypeProperty(superSRUri);
				}
			}
			newProp.addSuperProperty(superProp);
			logger.debug("    Datatype property '" + newProp.getURI() + "' given super property '" + superSRUri + "'");
		}
		return newProp;
	}
	
	private Individual createIndividual(SadlResource srsrc, OntClass type) throws JenaProcessorException, TranslationException {
		Node n = processExpression(srsrc);
		if (n == null) {
			throw new JenaProcessorException("SadlResource failed to convert to Node");
		}
		Individual inst = createIndividual(n.toFullyQualifiedString(), type);
		EList<SadlAnnotation> anns = srsrc.getAnnotations();
		if (anns != null) {
			addAnnotationsToResource(inst, anns);
		}
		return inst;
	}
	
	private Individual createIndividual(String newName, String superSRUri) {
		OntClass cls = null;
		if (superSRUri != null) {
			cls = getTheJenaModel().getOntClass(superSRUri);
			if (cls == null) {
				getTheJenaModel().createClass(superSRUri);
			}
		}
		return createIndividual(newName, cls);
	}
	
	private Individual createIndividual(String newName, OntClass supercls) {
		Individual inst = getTheJenaModel().createIndividual(newName, supercls);
		logger.debug("New instance '" + (newName != null ? newName : "(bnode)") + "' created");
		return inst;
	}
	
	private OntResource sadlTypeReferenceToOntResource(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		com.hp.hpl.jena.rdf.model.Resource obj = sadlTypeReferenceToResource(sadlTypeRef);
		if (obj == null) {
			return null;	// this happens when sadlTypeRef is a variable (even if unintended)
		}
		if (obj instanceof OntResource) {
			return (OntResource)obj;
		}
		else if (obj instanceof RDFNode) {
			if (((RDFNode)obj).canAs(OntResource.class)) {
				return ((RDFNode)obj).as(OntResource.class);
			}
		}
		throw new JenaProcessorException("Unable to convert SadlTypeReference '" + sadlTypeRef + "' to OntResource");
	}
	
	private com.hp.hpl.jena.rdf.model.Resource sadlTypeReferenceToResource(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		Object obj = sadlTypeReferenceToObject(sadlTypeRef);
		if (obj == null) {
			return null;	// this happens when sadlTypeRef is a variable (even if unintended)
		}
		if (obj instanceof com.hp.hpl.jena.rdf.model.Resource) {
			return (com.hp.hpl.jena.rdf.model.Resource) obj;
		}
		else if (obj instanceof RDFNode) {
			if (((RDFNode)obj).canAs(com.hp.hpl.jena.rdf.model.Resource.class)) {
				return ((RDFNode)obj).as(com.hp.hpl.jena.rdf.model.Resource.class);
			}
		}
		throw new JenaProcessorException("Unable to convert SadlTypeReference '" + sadlTypeRef + "' to OntResource");
	}
	
	private RDFNode sadlTypeReferenceToRDFNode(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		Object obj = sadlTypeReferenceToObject(sadlTypeRef);
		if (obj == null) {
			return null;	// this happens when sadlTypeRef is a variable (even if unintended)
		}
		else if (obj instanceof RDFNode) {
			return (RDFNode)obj;
		}
		throw new JenaProcessorException("Unable to convert SadlTypeReference '" + sadlTypeRef + "' to RDFNode");
	}

	private Object sadlTypeReferenceToObject(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		OntResource rsrc = null;
		// TODO How do we tell if this is a union versus an intersection?						
		if (sadlTypeRef instanceof SadlSimpleTypeReference) {
			SadlResource strSR = ((SadlSimpleTypeReference)sadlTypeRef).getType();
			//TODO check for proxy, i.e. unresolved references
			OntConceptType ctype;
			try {
				ctype = declarationExtensions.getOntConceptType(strSR);
			} catch (CircularDefinitionException e) {
				ctype = e.getDefinitionType();
				addError(e.getMessage(), sadlTypeRef);
			}
			String strSRUri = declarationExtensions.getConceptUri(strSR);	
			if (strSRUri == null) {
				if (ctype.equals(OntConceptType.VARIABLE)) {
					addError("Range should not be a variable.", sadlTypeRef);
					return null;
				}
				throw new JenaProcessorException("Failed to get concept URI of SadlResource in sadlTypeReferenceToObject");
			}
			if (ctype.equals(OntConceptType.CLASS)) {
				rsrc = getTheJenaModel().getOntClass(strSRUri);
				if (rsrc == null) {
					return createOntClass(strSRUri, (OntClass)null);
				}
			}
			else if (ctype.equals(OntConceptType.CLASS_LIST)) {
				rsrc = getTheJenaModel().getOntClass(strSRUri);
				if (rsrc == null) {
					return createListSubclass(strSRUri, strSRUri, strSR.eResource());
				}
			}
			else if (ctype.equals(OntConceptType.DATATYPE_LIST)) {
				rsrc = getTheJenaModel().getOntClass(strSRUri);
				if (rsrc == null) {
					return createListSubclass(strSRUri, strSRUri, strSR.eResource());
				}
			}
			else if (ctype.equals(OntConceptType.INSTANCE)) {
				rsrc = getTheJenaModel().getIndividual(strSRUri);
				if (rsrc == null) {
					// is it OK to create Individual without knowing class??
					return createIndividual(strSRUri, (OntClass)null);
				}
			}
			else if (ctype.equals(OntConceptType.DATATYPE)) {				
				OntResource dt = getTheJenaModel().getOntResource(strSRUri);
				if (dt == null) {
					throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri + "' not found; it should exist as there isn't enough information to create it.");
				}
				return dt;
			}
			else if (ctype.equals(OntConceptType.CLASS_PROPERTY)) {
				OntProperty otp = getTheJenaModel().getOntProperty(strSRUri);
				if (otp == null) {
					throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri + "' not found; should have found an ObjectProperty");
				}
				return otp;
			}
			else if (ctype.equals(OntConceptType.DATATYPE_PROPERTY)) {
				OntProperty dtp = getTheJenaModel().getOntProperty(strSRUri);
				if (dtp == null) {
					throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri + "' not found; should have found an DatatypeProperty");
				}
				return dtp;
			}
			else {
				throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri + "' was of a type not yet handled: " + ctype.toString());
			}
		}
		else if (sadlTypeRef instanceof SadlPrimitiveDataType) {
			return processSadlPrimitiveDataType(null, (SadlPrimitiveDataType) sadlTypeRef, null);
		}
		else if (sadlTypeRef instanceof SadlPropertyCondition) {
			return processSadlPropertyCondition((SadlPropertyCondition) sadlTypeRef);		
		}
		else if (sadlTypeRef instanceof SadlUnionType) {
			RDFNode lftNode = null; RDFNode rhtNode = null;
			SadlTypeReference lft = ((SadlUnionType)sadlTypeRef).getLeft();
			Object lftObj = sadlTypeReferenceToObject(lft);
			if (lftObj == null) {
				return null;
			}
			if (lftObj instanceof OntResource) {
				lftNode = ((OntResource)lftObj).asClass();
			}
			else {
				if (lftObj instanceof RDFNode) {
					lftNode = (RDFNode) lftObj;
				}
				else if (lftObj instanceof List) {
					// carry on: RDFNode list from nested union
				}
				else {
					throw new JenaProcessorException("Union member of unsupported type: " + lftObj.getClass().getCanonicalName());
				}
			}
			SadlTypeReference rht = ((SadlUnionType)sadlTypeRef).getRight();
			Object rhtObj = sadlTypeReferenceToObject(rht);
			if (rhtObj == null) {
				return null;
			}
			if (rhtObj instanceof OntResource) {
				rhtNode = ((OntResource)rhtObj).asClass();
			}
			else {
				if (rhtObj instanceof RDFNode) {
					rhtNode = (RDFNode) rhtObj;
				}
				else if (rhtObj instanceof List) {
					// carry on: RDFNode list from nested union
				}
				else {
					throw new JenaProcessorException("Union member of unsupported type: " + rhtObj != null ? rhtObj.getClass().getCanonicalName() : "null");
				}
			}
			if (lftNode instanceof OntResource && rhtNode instanceof OntResource) {
				OntClass unionCls = createUnionClass(lftNode, rhtNode);
				return unionCls;
			}
			else if (lftObj instanceof List && rhtNode instanceof RDFNode) {
				((List)lftObj).add(rhtNode);
				return lftObj;
			}
			else if (lftObj instanceof RDFNode && rhtNode instanceof List) {
				((List)rhtNode).add(lftNode);
				return rhtNode;
			}
			else if (lftNode instanceof RDFNode && rhtNode instanceof RDFNode){
				List<RDFNode> rdfdatatypelist = new ArrayList<RDFNode>();
				rdfdatatypelist.add((RDFNode) lftNode);
				rdfdatatypelist.add((RDFNode) rhtNode);
				return rdfdatatypelist;
			}
			else {
				throw new JenaProcessorException("Left and right sides of union are of incompatible types: " + lftNode.toString() + " and " + rhtNode.toString());
			}
		}
		else if (sadlTypeRef instanceof SadlIntersectionType) {
			RDFNode lftNode = null; RDFNode rhtNode = null;
			SadlTypeReference lft = ((SadlIntersectionType)sadlTypeRef).getLeft();
			Object lftObj = sadlTypeReferenceToObject(lft);
			if (lftObj == null) {
				return null;
			}
			if (lftObj instanceof OntResource) {
				lftNode = ((OntResource)lftObj).asClass();
			}
			else {
				if (lftObj instanceof RDFNode) {
					lftNode = (RDFNode) lftObj;
				}
				else if (lftObj == null) {
					addError("SadlIntersectionType did not resolve to an ontology object (null)", sadlTypeRef);
				}
				else {
					throw new JenaProcessorException("Intersection member of unsupported type: " + lftObj.getClass().getCanonicalName());
				}
			}
			SadlTypeReference rht = ((SadlIntersectionType)sadlTypeRef).getRight();
			if (rht == null) {
				throw new JenaProcessorException("No right-hand side to intersection");
			}
			Object rhtObj = sadlTypeReferenceToObject(rht);
			if (rhtObj == null) {
				return null;
			}
			if (rhtObj instanceof OntResource) {
				rhtNode = ((OntResource)rhtObj).asClass();
			}
			else {
				if (rhtObj instanceof RDFNode) {
					rhtNode = (RDFNode) rhtObj;
				}
				else {
					throw new JenaProcessorException("Intersection member of unsupported type: " + rhtObj.getClass().getCanonicalName());
				}
			}
			if (lftNode instanceof OntResource && rhtNode instanceof OntResource) {
				OntClass intersectCls = createIntersectionClass(lftNode, rhtNode);
				return intersectCls;
			}
			else if (lftNode instanceof RDFNode && rhtNode instanceof RDFNode){
				List<RDFNode> rdfdatatypelist = new ArrayList<RDFNode>();
				rdfdatatypelist.add((RDFNode) lftNode);
				rdfdatatypelist.add((RDFNode) rhtNode);
				return rdfdatatypelist;
			}
			else {
				throw new JenaProcessorException("Left and right sides of union are of incompatible types: " + lftNode.toString() + " and " + rhtNode.toString());
			}
		}
		return rsrc;
	}

	private com.hp.hpl.jena.rdf.model.Resource processSadlPrimitiveDataType(SadlClassOrPropertyDeclaration element, SadlPrimitiveDataType sadlTypeRef, String newDatatypeUri) throws JenaProcessorException {
		SadlDataType pt = sadlTypeRef.getPrimitiveType();
		String typeStr = pt.getLiteral();
		com.hp.hpl.jena.rdf.model.Resource onDatatype;
		if (typeStr.equals(XSD.xstring.getLocalName())) onDatatype = XSD.xstring;
		else if (typeStr.equals(XSD.anyURI.getLocalName())) onDatatype = XSD.anyURI;
		else if (typeStr.equals(XSD.base64Binary.getLocalName())) onDatatype = XSD.base64Binary;
		else if (typeStr.equals(XSD.date.getLocalName())) onDatatype = XSD.date;
		else if (typeStr.equals(XSD.dateTime.getLocalName())) onDatatype = XSD.dateTime;
		else if (typeStr.equals(XSD.decimal.getLocalName())) onDatatype = XSD.decimal;
		else if (typeStr.equals(XSD.duration.getLocalName())) onDatatype = XSD.duration;
		else if (typeStr.equals(XSD.gDay.getLocalName())) onDatatype = XSD.gDay;
		else if (typeStr.equals(XSD.gMonth.getLocalName())) onDatatype = XSD.gMonth;
		else if (typeStr.equals(XSD.gMonthDay.getLocalName())) onDatatype = XSD.gMonthDay;
		else if (typeStr.equals(XSD.gYear.getLocalName())) onDatatype = XSD.gYear;
		else if (typeStr.equals(XSD.gYearMonth.getLocalName())) onDatatype = XSD.gYearMonth;
		else if (typeStr.equals(XSD.hexBinary.getLocalName())) onDatatype = XSD.hexBinary;
		else if (typeStr.equals(XSD.integer.getLocalName())) onDatatype = XSD.integer;
		else if (typeStr.equals(XSD.time.getLocalName())) onDatatype = XSD.time;
		else if (typeStr.equals(XSD.xboolean.getLocalName())) onDatatype = XSD.xboolean;
		else if (typeStr.equals(XSD.xdouble.getLocalName())) onDatatype = XSD.xdouble;
		else if (typeStr.equals(XSD.xfloat.getLocalName())) onDatatype = XSD.xfloat;
		else if (typeStr.equals(XSD.xint.getLocalName())) onDatatype = XSD.xint;
		else if (typeStr.equals(XSD.xlong.getLocalName())) onDatatype = XSD.xlong;
		else if (typeStr.equals(XSD.anyURI.getLocalName())) onDatatype = XSD.anyURI;
		else if (typeStr.equals(XSD.anyURI.getLocalName())) onDatatype = XSD.anyURI;
		else {
			throw new JenaProcessorException("Unexpected primitive data type: " + typeStr);
		}
		if (newDatatypeUri == null) {
			return onDatatype;
		}
		SadlDataTypeFacet facet = element.getFacet();
		OntClass datatype = createRdfsDatatype(newDatatypeUri, null, onDatatype, facet);
		return datatype;
	}
	private OntClass createRdfsDatatype(String newDatatypeUri, List<RDFNode> unionOfTypes, com.hp.hpl.jena.rdf.model.Resource onDatatype,
			SadlDataTypeFacet facet) throws JenaProcessorException {
		OntClass datatype = getTheJenaModel().createOntResource(OntClass.class, RDFS.Datatype, newDatatypeUri);
		OntClass equivClass = getTheJenaModel().createOntResource(OntClass.class, RDFS.Datatype, null);
		if (onDatatype != null) {
			equivClass.addProperty(OWL2.onDatatype, onDatatype);
			if (facet != null) {
				com.hp.hpl.jena.rdf.model.Resource restrictions = facetsToRestrictionNode(newDatatypeUri, facet);
				// Create a list containing the restrictions
				RDFList list = getTheJenaModel().createList(new RDFNode[] {restrictions});
				equivClass.addProperty(OWL2.withRestrictions, list);
			}
		}
		else if (unionOfTypes != null) {
			Iterator<RDFNode> iter = unionOfTypes.iterator();
			RDFList collection = getTheJenaModel().createList();
			while (iter.hasNext()) {
				RDFNode dt = iter.next();
				collection = collection.with(dt);
			}
			equivClass.addProperty(OWL.unionOf, collection);
		}
		else {
			throw new JenaProcessorException("Invalid arguments to createRdfsDatatype");
		}
		datatype.addEquivalentClass(equivClass);
		return datatype;
	}

	private com.hp.hpl.jena.rdf.model.Resource facetsToRestrictionNode(String newName, SadlDataTypeFacet facet) {
		com.hp.hpl.jena.rdf.model.Resource anon = getTheJenaModel().createResource();
		
		anon.addProperty(xsdProperty(facet.isMinInclusive()?"minInclusive":"minExclusive"), "" + facet.getMin());
		anon.addProperty(xsdProperty(facet.isMaxInclusive()?"maxInclusive":"maxExclusive"), "" + facet.getMax());
		
		if (facet.getLen() != null) {
			anon.addProperty(xsdProperty("length"), "" + facet.getLen());
		}
		if (facet.getMinlen() != null) {
			anon.addProperty(xsdProperty("minLength"), "" + facet.getMinlen());
		}
		if (facet.getMaxlen() != null && !facet.getMaxlen().equals("*")) {
			anon.addProperty(xsdProperty("maxLength"), "" + facet.getMaxlen());
		}
		if (facet.getRegex() != null) {
			anon.addProperty(xsdProperty("pattern"), "" + facet.getRegex());
		}
		if (facet.getValues() != null) {
			Iterator<String> iter = facet.getValues().iterator();
			while (iter.hasNext()) {
				anon.addProperty(xsdProperty("enumeration"), iter.next());
			}
		}
		return anon;
	}

	private OntClass processSadlPropertyCondition(SadlPropertyCondition sadlPropCond) throws JenaProcessorException {
		OntClass retval = null;
		SadlResource sr = ((SadlPropertyCondition)sadlPropCond).getProperty();
		String propUri = declarationExtensions.getConceptUri(sr);
		if (propUri == null) {
			throw new JenaProcessorException("Failed to get concept URI of SadlResource in processSadlPropertyCondition");
		}
		OntConceptType propType;
		try {
			propType = declarationExtensions.getOntConceptType(sr);
		} catch (CircularDefinitionException e) {
			propType = e.getDefinitionType();
			addError(e.getMessage(), sadlPropCond);
		}
		OntProperty prop = getTheJenaModel().getOntProperty(propUri);
		if (prop == null) {
			if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
				prop = getTheJenaModel().createObjectProperty(propUri);
			}
			else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
				prop = getTheJenaModel().createDatatypeProperty(propUri);
			}
			else if (propType.equals(OntConceptType.ANNOTATION_PROPERTY)) {
				prop = getTheJenaModel().createAnnotationProperty(propUri);
			}
			else {
				prop = getTheJenaModel().createOntProperty(propUri);
			}
		}
		Iterator<SadlCondition> conditer = ((SadlPropertyCondition)sadlPropCond).getCond().iterator();
		while (conditer.hasNext()) {
			SadlCondition cond = conditer.next();
			retval = sadlConditionToOntClass(cond, prop, propType);
			if (conditer.hasNext()) {
				throw new JenaProcessorException("Multiple property conditions not currently handled");
			}
		}
		return retval;
	}

	private OntClass sadlConditionToOntClass(SadlCondition cond, Property prop, OntConceptType propType) throws JenaProcessorException {
		OntClass retval = null;
		if (prop == null) {
			addError("Can't create restiction on unresolvable property", cond);
		}
		else if (cond instanceof SadlAllValuesCondition) {
			SadlTypeReference type = ((SadlAllValuesCondition)cond).getType();
			if (type instanceof SadlPrimitiveDataType) {
				SadlDataType pt = ((SadlPrimitiveDataType)type).getPrimitiveType();
				String typeStr = pt.getLiteral();
				typeStr = XSD.getURI() + typeStr;
			}
			com.hp.hpl.jena.rdf.model.Resource typersrc = sadlTypeReferenceToResource(type);
			if (typersrc == null) {
				addError("Can't create all values from restriction on unresolvable property value restriction", type);
			}
			else {
				AllValuesFromRestriction avf = getTheJenaModel().createAllValuesFromRestriction(null, prop, typersrc);
				logger.debug("New all values from restriction on '" + prop.getURI() + "' to values of type '" + typersrc.toString() + "'");
				retval = avf;
			}
		}
		else if (cond instanceof SadlHasValueCondition) {
			SadlExplicitValue value = ((SadlHasValueCondition)cond).getRestriction();
			RDFNode val;
			if (value instanceof SadlResource) {
				OntConceptType srType;
				try {
					srType = declarationExtensions.getOntConceptType((SadlResource)value);
				} catch (CircularDefinitionException e) {
					srType = e.getDefinitionType();
					addError(e.getMessage(), cond);
				}
				SadlResource srValue = (SadlResource) value;
				if (srType == null) {
					srValue = ((SadlResource)value).getName();
					try {
						srType = declarationExtensions.getOntConceptType(srValue);
					} catch (CircularDefinitionException e) {
						srType = e.getDefinitionType();
						addError(e.getMessage(), cond);
					}
				}
				if (srType == null) {
					throw new JenaProcessorException("Unable to resolve SadlResource value");
				}
				if (srType.equals(OntConceptType.INSTANCE)) {
					String valUri = declarationExtensions.getConceptUri(srValue);
					if (valUri == null) {
						throw new JenaProcessorException("Failed to find SadlResource in Xtext model");
					}
					val = getTheJenaModel().getIndividual(valUri);
					if (val == null) {
						throw new JenaProcessorException("Failed to retrieve instance '" + valUri + "' from Jena model");
					}
				}
				else {
					throw new JenaProcessorException("A has value restriction is to a SADL resource that did not resolve to an instance in the model");
				}
			}
			else {
				if (prop instanceof OntProperty) {
					val = sadlExplicitValueToLiteral(value, ((OntProperty)prop).getRange());
				}
				else {
					val = sadlExplicitValueToLiteral(value, null);
				}
			}
			if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
				Individual valInst = val.as(Individual.class);
				if (prop instanceof OntProperty && valueInObjectTypePropertyRange((OntProperty)prop, valInst)) {
					HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, prop, valInst);
					logger.debug("New has value restriction on '" + prop.getURI() + "' to value '" + valInst.toString() + "'");
					retval =  hvr;
				}
				else {
					throw new JenaProcessorException("Value '" + valInst.getURI() + "' not in range of object property '" + prop.getURI() + "'");
				}
			}
			else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
				if (prop instanceof OntProperty && val.isLiteral() && valueInDatatypePropertyRange((OntProperty)prop, val.asLiteral())) {
					HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, prop, val);
					logger.debug("New has value restriction on '" + prop.getURI() + "' to value '" + val.toString() + "'");
					retval =  hvr;
				}	
				else {
					throw new JenaProcessorException("Value '" + val.toString() + "' not in range of datatype property '" + prop.getURI() + "' or range not given");
				}
			}
			else if (propType.equals(OntConceptType.RDF_PROPERTY)) {
				HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, prop, val);
				logger.debug("New has value restriction on '" + prop.getURI() + "' to value '" + val.toString() + "'");
				retval = hvr;
			}
			else {
				throw new JenaProcessorException("Has value restriction on unexpected property type: " + propType.toString());
			}
		}
		else if (cond instanceof SadlCardinalityCondition) {
			// Note: SomeValuesFrom is embedded in cardinality in the SADL grammar--an "at least" cardinality with "one" instead of # 
			String cardinality = ((SadlCardinalityCondition)cond).getCardinality();
			SadlTypeReference type = ((SadlCardinalityCondition)cond).getType();
			OntResource typersrc = null;
			if (type != null) {
				typersrc = sadlTypeReferenceToOntResource(type);					
			}
			if (cardinality.equals("one")) {
				// this is interpreted as a someValuesFrom restriction
				if (type == null) {
					throw new JenaProcessorException("'one' means some value from class so a type must be given");
				}
				SomeValuesFromRestriction svf = getTheJenaModel().createSomeValuesFromRestriction(null, prop, typersrc);
				logger.debug("New some values from restriction on '" + prop.getURI() + "' to values of type '" + typersrc.toString() + "'");
				retval =  svf;
			}
			else {
				// cardinality restrictioin
				int cardNum = Integer.parseInt(cardinality);
				String op = ((SadlCardinalityCondition)cond).getOperator();
				if (op == null) {
					CardinalityRestriction cr = getTheJenaModel().createCardinalityRestriction(null, prop, cardNum);	
					logger.debug("New cardinality restriction " + cardNum + " on '" + prop.getURI() + "' created");
					if (type != null) {
						cr.removeAll(OWL.cardinality);
						cr.addLiteral(OWL2.qualifiedCardinality, cardNum);
						cr.addProperty(OWL2.onClass, typersrc);
					}
					retval =  cr;
				}
				else if (op.equals("least")) {
					MinCardinalityRestriction cr = getTheJenaModel().createMinCardinalityRestriction(null, prop, cardNum);							
					logger.debug("New min cardinality restriction " + cardNum + " on '" + prop.getURI() + "' created");
					if (type != null) {
						cr.removeAll(OWL.minCardinality);
						cr.addLiteral(OWL2.minQualifiedCardinality, cardNum);
						cr.addProperty(OWL2.onClass, typersrc);
					}
					retval =  cr;
				}
				else if (op.equals("most")) {
					logger.debug("New max cardinality restriction " + cardNum + " on '" + prop.getURI() + "' created");
					MaxCardinalityRestriction cr = getTheJenaModel().createMaxCardinalityRestriction(null, prop, cardNum);							
					if (type != null) {
						cr.removeAll(OWL.maxCardinality);
						cr.addLiteral(OWL2.maxQualifiedCardinality, cardNum);
						cr.addProperty(OWL2.onClass, typersrc);
					}
					retval =  cr;
				}
				if (logger.isDebugEnabled()) {
					if (type != null) {
						logger.debug("   cardinality is qualified; values must be of type '" + typersrc + "'");
					}	
				}
			}
		}
		else {
			throw new JenaProcessorException("Unhandled SadlCondition type: " + cond.getClass().getCanonicalName());
		}
		return retval;
	}

	private boolean valueInDatatypePropertyRange(OntProperty prop, Literal val) {
		String ptype = prop.getRange().getURI();
		if (ptype == null) {
			return true;
		}
		String dtype = val.getDatatypeURI();
		if (dtype.equals(ptype)) {
			return true;
		}
		if (dtype.equals(XSD.xint.getURI())) {	// the literal is an integer
			if (ptype.equals(XSD.integer.getURI())) return true;
			if (ptype.equals(XSD.xlong.getURI())) return true;
		}
		return false;
	}

	private Literal sadlExplicitValueToLiteral(SadlExplicitValue value, com.hp.hpl.jena.rdf.model.Resource rng) throws JenaProcessorException {
		try {
			if (value instanceof SadlNumberLiteral) {
				String val = ((SadlNumberLiteral)value).getLiteralNumber();
				if (rng != null) {
					return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), val);
				}
				else {
					if (val.contains(".")) {
						return getTheJenaModel().createTypedLiteral(Double.parseDouble(val));
					}
					else {
						return getTheJenaModel().createTypedLiteral(Integer.parseInt(val));
					}
				}
			}
			else if (value instanceof SadlStringLiteral) {
				String val = ((SadlStringLiteral)value).getLiteralString();
				if (rng != null) {
					return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), val);
				}
				else {
					return getTheJenaModel().createTypedLiteral(val);
				}
			}
			else if (value instanceof SadlBooleanLiteral) {
				SadlBooleanLiteral val = ((SadlBooleanLiteral)value);
				if (rng != null) {
					return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), val.toString());
				}
				else {
					getTheJenaModel().createTypedLiteral(Boolean.parseBoolean(val.toString()));
				}
			}
			else if (value instanceof SadlValueList) {
				throw new JenaProcessorException("A SADL value list cannot be converted to a Literal");
			}
			else if (value instanceof SadlConstantLiteral) {
				String val = ((SadlConstantLiteral)value).getTerm();
				if (rng != null) {
					return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), val);
				}
				else {
					try {
						int ival = Integer.parseInt(val);
						return getTheJenaModel().createTypedLiteral(ival);
					}
					catch (Exception e) {
						try {
							double dval = Double.parseDouble(val);
							return getTheJenaModel().createTypedLiteral(dval);
						}
						catch (Exception e2) {
							return getTheJenaModel().createTypedLiteral(val);
						}
					}
				}
			}
			else if (value instanceof SadlResource) {
				Node nval = processExpression((SadlResource)value);
				throw new JenaProcessorException("Unable to convert concept '" + nval.toFullyQualifiedString() + "to a literal");
			}
			else {
				throw new JenaProcessorException("Unhandled sadl explicit vaue type: " + value.getClass().getCanonicalName());
			}
		}
		catch (Throwable t) {
			addError(t.getMessage(), value);
		}
		return null;
	}

	private boolean valueInObjectTypePropertyRange(OntProperty prop, Individual valInst) throws JenaProcessorException {
		ExtendedIterator<? extends OntResource> itr = prop.listRange();
		while (itr.hasNext()) {
			OntResource nxt = itr.next();
			if (nxt.isClass()) {
				if (instanceBelongsToClass(getTheJenaModel(), valInst, nxt)) {
					return true;
				}
			}
		}
		return false;
	}
	
	private IntersectionClass createIntersectionClass(List<RDFNode> members) throws JenaProcessorException {
		RDFNode[] array = members.toArray(new RDFNode[members.size()]);
		return createIntersectionClass(array);
	}

	private IntersectionClass createIntersectionClass(RDFNode... members) throws JenaProcessorException {
		RDFList classes = getTheJenaModel().createList(members);
		if (!classes.isEmpty()) {
			IntersectionClass intersectCls = getTheJenaModel().createIntersectionClass(null, classes);
			logger.debug("New intersection class created");
			return intersectCls;
		}
		throw new JenaProcessorException("createIntersectionClass called with empty list of classes");
	}

	private UnionClass createUnionClass(List<RDFNode> members) throws JenaProcessorException {
		RDFNode[] array = members.toArray(new RDFNode[members.size()]);
		return createUnionClass(array);
	}
	private UnionClass createUnionClass(RDFNode... members) throws JenaProcessorException {
		RDFList classes = getTheJenaModel().createList(members);
		if (!classes.isEmpty()) {
			UnionClass unionCls = getTheJenaModel().createUnionClass(null, classes);
			logger.debug("New union class created");
			return unionCls;
		}
		throw new JenaProcessorException("createUnionClass called with empty list of classes");
	}

	private OntConceptType getSadlTypeReferenceType(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		if (sadlTypeRef instanceof SadlSimpleTypeReference) {
			SadlResource sr = ((SadlSimpleTypeReference)sadlTypeRef).getType();
			try {
				return declarationExtensions.getOntConceptType(sr);
			} catch (CircularDefinitionException e) {
				addError(e.getMessage(), sadlTypeRef);
				return e.getDefinitionType();
			}
		}
		else if (sadlTypeRef instanceof SadlPrimitiveDataType) {
			return OntConceptType.DATATYPE;
		}
		else if (sadlTypeRef instanceof SadlPropertyCondition) {
			// property conditions => OntClass
			return OntConceptType.CLASS;
		}
		else if (sadlTypeRef instanceof SadlUnionType) {
			SadlTypeReference lft = ((SadlUnionType)sadlTypeRef).getLeft();
			OntConceptType lfttype = getSadlTypeReferenceType(lft);
			return lfttype;
//			SadlTypeReference rght = ((SadlUnionType)sadlTypeRef).getRight();
		}
		else if (sadlTypeRef instanceof SadlIntersectionType) {
			SadlTypeReference lft = ((SadlIntersectionType)sadlTypeRef).getLeft();
			OntConceptType lfttype = getSadlTypeReferenceType(lft);
			return lfttype;
//			SadlTypeReference rght = ((SadlIntersectionType)sadlTypeRef).getRight();
		}
		throw new JenaProcessorException("Unexpected SadlTypeReference subtype: " + sadlTypeRef.getClass().getCanonicalName());
	}

	private String assureNamespaceEndsWithHash(String name) {
		name = name.trim();
		if (!name.endsWith("#")) {
			return name + "#";
		}
		return name;
	}

	private String getModelNamespace() {
		return modelNamespace;
	}

	private void setModelNamespace(String modelNamespace) {
		this.modelNamespace = modelNamespace;
	}

	public OntDocumentManager getJenaDocumentMgr(OntModelSpec ontModelSpec) {
		if (jenaDocumentMgr == null) {
			if (getMappingModel() != null) {
				setJenaDocumentMgr(new OntDocumentManager(getMappingModel()));
				if (ontModelSpec != null) {
					ontModelSpec.setDocumentManager(jenaDocumentMgr);
				}
			}
			else {
				setJenaDocumentMgr(OntDocumentManager.getInstance());
			}
		}
		return jenaDocumentMgr;
	}

	private void setJenaDocumentMgr(OntDocumentManager ontDocumentManager) {
		jenaDocumentMgr = ontDocumentManager;
	}

	private Model getMappingModel() {
		// TODO Auto-generated method stub
		return null;
	}
	
	/**
	 * return true if the instance belongs to the class else return false
	 * 
	 * @param inst
	 * @param cls
	 * @return
	 * @throws JenaProcessorException 
	 */
	private boolean instanceBelongsToClass(OntModel m, OntResource inst, OntResource cls) throws JenaProcessorException {
		// The following cases must be considered:
		// 1) The class is a union of other classes. Check to see if the instance is a member of any of
		//		the union classes and if so return true.
		// 2) The class is an intersection of other classes. Check to see if the instance is 
		//		a member of each class in the intersection and if so return true.
		// 3) The class is neither a union nor an intersection. If the instance belongs to the class return true. Otherwise
		//		check to see if the instance belongs to a subclass of the class else
		//		return false. (Superclasses do not need to be considered because even if the instance belongs to a super
		//		class that does not tell us that it belongs to the class.)
		
		/*
		 * e.g., 	Internet is a Network.
		 * 			Network is a type of Subsystem.
		 * 			Subsystem is type of System.
		 */
		if (cls.isURIResource()) {
			cls = m.getOntClass(cls.getURI());
		}
		if (cls == null) {
			return false;
		}
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
						com.hp.hpl.jena.rdf.model.Resource avfc = rest.asAllValuesFromRestriction().getAllValuesFrom();
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
				throw new JenaProcessorException("Unhandled cardinality restriction");
			}
			else if (rest.isMaxCardinalityRestriction()) {
				throw new JenaProcessorException("Unhandled max cardinality restriction");
			}
			else if (rest.isMinCardinalityRestriction()) {
				throw new JenaProcessorException("Unhandled min cardinality restriction");
			}
		}
		else {
			if (inst.canAs(Individual.class)) {
				ExtendedIterator<com.hp.hpl.jena.rdf.model.Resource> eitr = inst.asIndividual().listRDFTypes(false);
				while (eitr.hasNext()) {
					com.hp.hpl.jena.rdf.model.Resource r = eitr.next();				
					OntResource or = m.getOntResource(r);
					try {
						if (or.isURIResource()) {
							OntClass oc = m.getOntClass(or.getURI());
							if (SadlUtils.classIsSubclassOf(oc, cls, true, null)) {
								eitr.close();
								return true;
							}
						}
						else if (or.canAs(OntClass.class)) {
							if (SadlUtils.classIsSubclassOf(or.as(OntClass.class), cls, true, null)) {
								eitr.close();
								return true;
							}
						}
					} catch (CircularDependencyException e) {
						throw new JenaProcessorException(e.getMessage(), e);
					}
				}
			}
		}
		return false;
	}
	private List<OntResource> getOntResourcesInUnionClass(OntModel m, UnionClass ucls) {
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
	
	private List<OntResource> getOntResourcesInIntersectionClass(OntModel m, IntersectionClass icls) {
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

	private ValidationAcceptor getIssueAcceptor() {
		return issueAcceptor;
	}

	private void setIssueAcceptor(ValidationAcceptor issueAcceptor) {
		this.issueAcceptor = issueAcceptor;
	}

	private CancelIndicator getCancelIndicator() {
		return cancelIndicator;
	}

	private void setCancelIndicator(CancelIndicator cancelIndicator) {
		this.cancelIndicator = cancelIndicator;
	}

	protected String getModelName() {
		return modelName;
	}

	protected void setModelName(String modelName) {
		this.modelName = modelName;
	}

	@Override
	public void processExternalModels(String mappingFileFolder, List<String> fileNames) throws IOException {
		File mff = new File(mappingFileFolder);
		if (!mff.exists()) {
			mff.mkdirs();
		}
		if (!mff.isDirectory()) {
			throw new IOException("Mapping file location '" + mappingFileFolder + "' exists but is not a directory.");
		}
		System.out.println("Ready to save mappings in folder: " + mff.getCanonicalPath());
		for (int i = 0; i < fileNames.size(); i++) {
			System.out.println("   URL: " + fileNames.get(i));
		}
	}
	
	private String getModelAlias() {
		return modelAlias;
	}
	private void setModelAlias(String modelAlias) {
		this.modelAlias = modelAlias;
	}
	private OntModelSpec getSpec() {
		return spec;
	}
	private void setSpec(OntModelSpec spec) {
		this.spec = spec;
	}
	
	private boolean isVariable(SadlResource sr) {
		OntConceptType ct;
		try {
			ct = declarationExtensions.getOntConceptType(sr);
		} catch (CircularDefinitionException e) {
			ct = e.getDefinitionType();
			addError(e.getMessage(), sr);
		}
		if (ct.equals(OntConceptType.VARIABLE)) {
			return true;
		}
		return false;
	}
	/**
	 * This method looks in the clauses of a Rule to see if there is already a triple matching the given pattern. If there is
	 * a new variable of the same name is created (to make sure the count is right) and returned. If not a rule or no match
	 * a new variable (new name) is created and returned.
	 * @param expr 
	 * @param subject
	 * @param predicate
	 * @param object
	 * @return
	 */
	protected VariableNode getVariableNode(Expression expr, Node subject, Node predicate, Node object) {
		if (getTarget() != null) {
			// Note: when we find a match we still create a new VariableNode with the same name in order to have the right reference counts for the new VariableNode
			if (getTarget() instanceof Rule) {
				VariableNode var = findVariableInTripleForReuse(((Rule)getTarget()).getGivens(), subject, predicate, object);
				if (var != null) {
					return new VariableNode(var.getName());
				}
				var = findVariableInTripleForReuse(((Rule)getTarget()).getIfs(), subject, predicate, object);
				if (var != null) {
					return new VariableNode(var.getName());
				}
				var = findVariableInTripleForReuse(((Rule)getTarget()).getThens(), subject, predicate, object);
				if (var != null) {
					return new VariableNode(var.getName());
				}
			}
		}
		return new VariableNode(getNewVar(expr));
	}
	
	protected String getNewVar(Expression expr) {
		IScopeProvider scopeProvider = ((XtextResource)expr.eResource()).getResourceServiceProvider().get(IScopeProvider.class);
		IScope scope = scopeProvider.getScope(expr, SADLPackage.Literals.SADL_RESOURCE__NAME);
		String proposedName = "v" + vNum;
		while (userDefinedVariables.contains(proposedName)
				|| 	scope.getSingleElement(QualifiedName.create(proposedName)) != null) {
			vNum++;
			proposedName = "v" + vNum;
		}
		vNum++;
		return proposedName;
	}
	
	/**
	 * Supporting method for the method above (getVariableNode(Node, Node, Node))
	 * @param gpes
	 * @param subject
	 * @param predicate
	 * @param object
	 * @return
	 */
	protected VariableNode findVariableInTripleForReuse(List<GraphPatternElement> gpes, Node subject, Node predicate, Node object) {
		if (gpes != null) {
			Iterator<GraphPatternElement> itr = gpes.iterator();
			while (itr.hasNext()) {
				GraphPatternElement gpe = itr.next();
				while (gpe != null) {
					if (gpe instanceof TripleElement) {
						TripleElement tr = (TripleElement)gpe;
						Node tsn = tr.getSubject();
						Node tpn = tr.getPredicate();
						Node ton = tr.getObject();
						if (subject == null && tsn instanceof VariableNode) {
							if (predicate != null && predicate.equals(tpn) && object != null && object.equals(ton)) {
								return (VariableNode) tsn;
							}
						}
						if (predicate == null && tpn instanceof VariableNode) {
							if (subject != null && subject.equals(tsn) && object != null && object.equals(ton)) {
								return (VariableNode) tpn;
							}
						}
						if (object == null && ton instanceof VariableNode) {
							if (subject != null && subject.equals(tsn) && predicate != null && predicate.equals(tpn)) {
								return (VariableNode) ton;
							}
						}
					}
					gpe = gpe.getNext();
				}
			}
		}
		return null;
	}
	
	public List<Rule> getRules() {
		return rules;
	}
	
	@Override
	public void processCommands(Resource resource, ValidationAcceptor issueAcceptor, IAcceptor<SadlMessage> resultAcceptor, ProcessorContext context) {
		setCancelIndicator(cancelIndicator);
//		TODO Need to restore SADL Server functionality
		boolean useKSever = false;
//		String useKS = context.getPreferenceValues().getPreference(SadlPreferences.TEST_WITH_KSERVER);
//		if (useKS != null && useKS.equals("true")) {
//			useKSever = true;
//		}
		if (resource.getContents().size() < 1) {
			return;
		}
		if (getTheJenaModel() == null) {
			// it always is?
			onValidate(resource, null, CheckMode.FAST_ONLY, context);
		}
		
		List<SadlCommand> toProcess = getSadlCommands();
		if (toProcess != null) {
			Iterator<SadlCommand> tpitr = toProcess.iterator();
			while (tpitr.hasNext()) {
				SadlCommand cmd = tpitr.next();
				try {
					if (cmd instanceof Query) {
						String query = ((Query)cmd).getSparqlQueryString();
						if (query == null) {
							query = getConfigMgr(getCurrentResource(), null).getTranslator().translateQuery(getTheJenaModel(), (Query) cmd);
						}
						query = SadlUtils.stripQuotes(query);
						if (useKSever) {
//							getKServer(resource).query(query);
						}
						else {
							processAdhocQuery(resource, issueAcceptor, context, query);
						}
					}
				} catch (TranslationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (InvalidNameException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} //catch (QueryCancelledException e) {
					// OK to cancel--no error
					//issueAcceptor.addInfo("Query cancelled by user", (EObject) cmd.getContext());
				//} catch (QueryParseException e) {
					// TODO Auto-generated catch block
					//e.printStackTrace();
				//} catch (ReasonerNotFoundException e) {
					// TODO Auto-generated catch block
					//e.printStackTrace();
				//} //catch (SessionNotFoundException e) {
					// TODO Auto-generated catch block
					//e.printStackTrace();
				//}
			}
		}
		SadlModel model = (SadlModel) resource.getContents().get(0);
		String modelActualUrl =resource.getURI().lastSegment();
		if (isReservedName(resource)) {
			addError("'" + modelActualUrl + "' is a reserved name. Please choose a different name", model);
		}
		modelName = model.getBaseUri();
		EList<SadlModelElement> elements = model.getElements();
		if (elements != null) {
			String _repoType = ConfigurationManager.RDF_XML_ABBREV_FORMAT; // default
			try {
				getConfigMgr(resource, _repoType).clearReasoner();
				Iterator<SadlModelElement> elitr = elements.iterator();
				while (elitr.hasNext()) {
					SadlModelElement element = elitr.next();
					if (element instanceof QueryStatement) {
						try {
							Object queryObj = translate(((QueryStatement)element).getExpr());
							if (queryObj instanceof Query) {
								String query = ((Query)queryObj).getSparqlQueryString();
								if (query == null) {
									query = getConfigMgr(getCurrentResource(), null).getTranslator().translateQuery(getTheJenaModel(), (Query) queryObj);
								}
								query = SadlUtils.stripQuotes(query);
								if (useKSever) {
//									getKServer(resource).query(query);
								}
								else {
									processAdhocQuery(resource, issueAcceptor, context, query);
								}
							}
							else {
								throw new TranslationException("Unexpected query translation result: " + queryObj.toString());
							}
						} catch (InvalidNameException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						} catch (InvalidTypeException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						} catch (TranslationException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						} //catch (QueryCancelledException e) {
							// TODO Auto-generated catch block
							//e.printStackTrace();
						//} catch (QueryParseException e) {
							// TODO Auto-generated catch block
							//e.printStackTrace();
						//} catch (ReasonerNotFoundException e) {
							// TODO Auto-generated catch block
							//e.printStackTrace();
						//} //catch (SessionNotFoundException e) {
							// TODO Auto-generated catch block
							//e.printStackTrace();
						//}
					}
					else if (element instanceof TestStatement) {
						
					}
					else if (element instanceof PrintStatement) {
						
					}
				}
			} catch (ConfigurationException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		}
		
	}
	
//	private ISadlServer getKServer(Resource resource) {
//		if (kServer == null) {
//			String modelFolder;
//			try {
//				modelFolder = getConfigMgr(resource, null).getModelFolder();
//				kServer = new SadlServerImpl(modelFolder);
//			} catch (IOException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			} catch (ConfigurationException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
//		}
//		return kServer;
//	}
	
	@Override
	public void processAdhocQuery(Resource resource, ValidationAcceptor issueAcceptor, ProcessorContext context,
			String query) {
		String queryString;
		String modelFolderPathname = getModelFolderPath(resource);
		
		String _repoType = ConfigurationManager.RDF_XML_ABBREV_FORMAT; // default
		try {
			if (getTheJenaModel() == null) {
				// it always is?
				XtextResource xtrsrc = (XtextResource) resource.getContents().get(0).eResource();
				if (xtrsrc != null) {
					URI resourceUri = xtrsrc.getURI();
					OntModel ontModel = OntModelProvider.find(xtrsrc);
					if (ontModel != null) {
						theJenaModel = ontModel;
					}
				}
				if (getTheJenaModel() == null) {
					onValidate(resource, null, CheckMode.FAST_ONLY, context);
				}
			}
			ITranslator translator = getConfigMgr(resource, _repoType).getTranslator();
			IReasoner reasoner = getConfigMgr(resource, _repoType).getReasoner();
			if (!reasoner.isInitialized()) {
				reasoner.setConfigurationManager(getConfigMgr(resource, _repoType));
				reasoner.initializeReasoner(modelFolderPathname, modelName, _repoType);
			}
			queryString = translator.translateQuery(getTheJenaModel(), processQuery(query));
			ResultSet results =  reasoner.ask(queryString);
			if (results != null) {
				System.out.println(results.toStringWithIndent(5));
			}
			else {
				System.out.println("Query returned no results");
			}
		} catch (com.ge.research.sadl.reasoner.ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TranslationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvalidNameException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (QueryParseException e) {
			System.err.println("Query parse exception: " + e.getMessage());
		} catch (QueryCancelledException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ReasonerNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (JenaProcessorException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
//	@Override
	public ResultSet processNamedQuery(Resource resource, String queryName) throws JenaProcessorException {
		SadlModel model = (SadlModel) resource.getContents().get(0);
		if (model != null) {
			URI resourceUri = resource.getURI();
			XtextResource xtrsrc = (XtextResource) model.eResource();
			if (xtrsrc != null) {
				URI importedResourceUri = xtrsrc.getURI();
				OntModel m = OntModelProvider.find(xtrsrc);
				if (m == null) {
					throw new JenaProcessorException("Unable to retrieve OntModel for resource '" + resourceUri + "'");
				}
				OntResource query = m.getOntResource(queryName);
				Statement s = query.getProperty(m.getProperty(SADL_IMPLICIT_MODEL_QUERY_STRING_URI));
			}
		}
		return null;
	}
	
	private String checkImplicitSadlModelExistence(Resource resource, ProcessorContext context) throws IOException, ConfigurationException, URISyntaxException, JenaProcessorException {
		UtilsForJena ufj = new UtilsForJena();
		String policyFileUrl = ufj.getPolicyFilename(resource);
		String policyFilename = policyFileUrl != null ? ufj.fileUrlToFileName(policyFileUrl) : null;
		if (policyFilename != null) {
			File projectFolder = new File(policyFilename).getParentFile().getParentFile();
			String relPath = SADL_IMPLICIT_MODEL_FOLDER + "/" + SADL_IMPLICIT_MODEL_FILENAME;
			String platformPath = projectFolder.getName() + "/" + relPath;
			String implicitSadlModelFN = projectFolder + "/" + relPath;
			File implicitModelFile = new File(implicitSadlModelFN);
			if (!implicitModelFile.exists()) {
				createSadlImplicitModel(implicitModelFile);
				try {
					Resource newRsrc = resource.getResourceSet().createResource(URI.createPlatformResourceURI(platformPath, false)); // createFileURI(implicitSadlModelFN));
//					newRsrc.load(new StringInputStream(implicitModel), resource.getResourceSet().getLoadOptions());
					newRsrc.load(resource.getResourceSet().getLoadOptions());
					refreshResource(newRsrc);
				}
				catch (Throwable t) {}
			}
			return implicitModelFile.getAbsolutePath();
		}
		return null;
	}
	
	static public void createSadlImplicitModel(File implicitModelFile) throws IOException{
		String implicitModel = getSadlImplicitModel();
		SadlUtils su = new SadlUtils();
		implicitModelFile.getParentFile().mkdirs();
		su.stringToFile(implicitModelFile, implicitModel, true);
	}
	
	static public String getSadlImplicitModel() {
		StringBuilder sb = new StringBuilder();
		sb.append("uri \"");
		sb.append(SADL_IMPLICIT_MODEL_URI);
		sb.append("\" alias ");
		sb.append(SADL_IMPLICIT_MODEL_PREFIX);
		sb.append(".\n");

		sb.append("Event is a class.\n");

		sb.append("impliedProperty is a type of annotation.\n");

		sb.append("UnittedQuantity is a class,\n");
		sb.append(" 	described by ^value with values of type decimal,\n");
		sb.append(" 	described by unit with values of type string.\n");
		return sb.toString();
	}
	
	static public String getSadlBaseModel() {
		StringBuilder sb = new StringBuilder();
		sb.append("<rdf:RDF\n");
		sb.append("	    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n");
		sb.append("	    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"\n");
		sb.append("	    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\n");
		sb.append("	    xmlns:sadlbasemodel=\"http://sadl.org/sadlbasemodel#\"\n");
		sb.append("	    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\n");
		sb.append("	    xml:base=\"http://sadl.org/sadlbasemodel\">\n");
		sb.append("	  <owl:Ontology rdf:about=\"\">\n");
		sb.append("	    <rdfs:comment xml:lang=\"en\">Base model for SADL. These concepts can be used without importing.</rdfs:comment>\n");
		sb.append("	  </owl:Ontology>\n");
		sb.append("	  <owl:Class rdf:ID=\"Equation\"/>\n");
		sb.append("	  <owl:Class rdf:ID=\"ExternalEquation\"/>\n");
		sb.append("	  <owl:DatatypeProperty rdf:ID=\"expression\">\n");
		sb.append("	    <rdfs:domain rdf:resource=\"#Equation\"/>\n");
		sb.append("	    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#string\"/>\n");
		sb.append("	  </owl:DatatypeProperty>\n");
		sb.append("	  <owl:DatatypeProperty rdf:ID=\"externalURI\">\n");
		sb.append("	    <rdfs:domain rdf:resource=\"#Equation\"/>\n");
		sb.append("	    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#anyURI\"/>\n");
		sb.append("	  </owl:DatatypeProperty>\n");
		sb.append("</rdf:RDF>\n");
		return sb.toString();
	}
	
	static public String getSadlListModel() {
		StringBuilder sb = new StringBuilder();
		sb.append("<rdf:RDF\n");
		sb.append("    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n");
		sb.append("    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"\n");
		sb.append("    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\n");
		sb.append("    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\n");
		sb.append("    xmlns:base=\"http://sadl.org/sadllistmodel\"\n");
		sb.append("    xmlns:sadllistmodel=\"http://sadl.org/sadllistmodel#\" > \n");
		sb.append("  <rdf:Description rdf:about=\"http://sadl.org/sadllistmodel#first\">\n");
		sb.append("    <rdfs:domain rdf:resource=\"http://sadl.org/sadllistmodel#List\"/>\n");
		sb.append("    <rdf:type rdf:resource=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#Property\"/>\n");
		sb.append("  </rdf:Description>\n");
		sb.append("  <rdf:Description rdf:about=\"http://sadl.org/sadllistmodel#List\">\n");
		sb.append("    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#Class\"/>\n");
		sb.append("  </rdf:Description>\n");
		sb.append("  <rdf:Description rdf:about=\"http://sadl.org/sadllistmodel\">\n");
		sb.append("    <rdfs:comment xml:lang=\"en\">Typed List model for SADL.</rdfs:comment>\n");
		sb.append("    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#Ontology\"/>\n");
		sb.append("  </rdf:Description>\n");
		sb.append("  <rdf:Description rdf:about=\"http://sadl.org/sadllistmodel#rest\">\n");
		sb.append("    <rdfs:domain rdf:resource=\"http://sadl.org/sadllistmodel#List\"/>\n");
		sb.append("    <rdfs:range rdf:resource=\"http://sadl.org/sadllistmodel#List\"/>\n");
		sb.append("    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#ObjectProperty\"/>\n");
		sb.append("  </rdf:Description>\n");
		sb.append("  <rdf:Description rdf:about=\"http://sadl.org/sadllistmodel#lengthRestriction\">\n");
		sb.append("    <rdfs:domain rdf:resource=\"http://sadl.org/sadllistmodel#List\"/>\n");
		sb.append("    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#int\"/>\n");
		sb.append("    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#DatatypeProperty\"/>\n");
		sb.append("  </rdf:Description>\n");
		sb.append("  <rdf:Description rdf:about=\"http://sadl.org/sadllistmodel#minLengthRestriction\">\n");
		sb.append("    <rdfs:domain rdf:resource=\"http://sadl.org/sadllistmodel#List\"/>\n");
		sb.append("    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#int\"/>\n");
		sb.append("    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#DatatypeProperty\"/>\n");
		sb.append("  </rdf:Description>\n");
		sb.append("  <rdf:Description rdf:about=\"http://sadl.org/sadllistmodel#maxLengthRestriction\">\n");
		sb.append("    <rdfs:domain rdf:resource=\"http://sadl.org/sadllistmodel#List\"/>\n");
		sb.append("    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#int\"/>\n");
		sb.append("    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#DatatypeProperty\"/>\n");
		sb.append("  </rdf:Description>\n");
		sb.append("</rdf:RDF>\n");
		return sb.toString();
	}
	
	private boolean isImportSadlListModel() {
		return importSadlListModel;
	}
	
	private void setImportSadlListModel(boolean importSadlListModel) {
		this.importSadlListModel = importSadlListModel;
	}
	
	public OntModel getOntModelFromString(Resource resource, String serializedModel) throws IOException, ConfigurationException, URISyntaxException, JenaProcessorException {
		OntModel listModel = prepareEmptyOntModel(resource);
		InputStream stream = new ByteArrayInputStream(serializedModel.getBytes());
		listModel.read(stream, null);
		return listModel;
	}
	
	private boolean importSadlListModel(Resource resource) throws JenaProcessorException {
		if (sadlListModel == null) {
			try {
				sadlListModel = getOntModelFromString(resource, getSadlListModel());
				OntModelProvider.setSadlListModel(sadlListModel);
			} catch (Exception e) {
				throw new JenaProcessorException(e.getMessage(), e);
			}
			getTheJenaModel().addSubModel(sadlListModel);
			setImportSadlListModel(true);
			return true;
		}
		return false;
	}
	
	private IConfigurationManagerForIDE getConfigMgr(Resource resource, String format) throws ConfigurationException {
		if (format == null) {
			format = ConfigurationManager.RDF_XML_ABBREV_FORMAT; // default
		}
		if (configMgr == null) {
			String modelFolderPathname = getModelFolderPath(resource);
			if ((modelFolderPathname == null && 
					resource.getURI().toString().startsWith("synthetic")) ||
							resource.getURI().toString().startsWith(SYNTHETIC_FROM_TEST)) {
				modelFolderPathname = null;
				configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderPathname, format, true);
			}
			else {
				configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderPathname , format);
			}
		}
		return configMgr;
	}
	protected boolean isBooleanOperator(String op) {
		OPERATORS_RETURNING_BOOLEAN[] ops = OPERATORS_RETURNING_BOOLEAN.values();
		for (int i = 0; i < ops.length; i++) {
			if (op.equals(ops[i].toString())) {
				return true;
			}
		}
		return false;
	}
	
//	private boolean importSadlBaseModel(Resource resource) throws IOException, ConfigurationException, URISyntaxException, JenaProcessorException {
//		if (sadlBaseModel == null) {
//			sadlBaseModel  = getOntModelFromString(resource, getSadlBaseModel());
//			getTheJenaModel().addSubModel(sadlBaseModel);
//			return true;
//		}
//		return false;
//	}
}
