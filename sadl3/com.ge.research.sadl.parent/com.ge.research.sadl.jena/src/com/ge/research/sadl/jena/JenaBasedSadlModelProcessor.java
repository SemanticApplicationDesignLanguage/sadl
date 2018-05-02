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

import static com.ge.research.sadl.processing.ISadlOntologyHelper.ContextBuilder.MISSING_SUBJECT;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.PROPOFSUBJECT_PROP;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.PROPOFSUBJECT_RIGHT;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLPROPERTYINITIALIZER_PROPERTY;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLPROPERTYINITIALIZER_VALUE;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLSTATEMENT_SUPERELEMENT;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.EMFPlugin;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.service.OperationCanceledError;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.CheckType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.errorgenerator.generator.SadlErrorMessages;
import com.ge.research.sadl.external.ExternalEmfResource;
import com.ge.research.sadl.jena.JenaBasedSadlModelValidator.ImplicitPropertySide;
import com.ge.research.sadl.jena.JenaBasedSadlModelValidator.TypeCheckInfo;
import com.ge.research.sadl.jena.inference.SadlJenaModelGetterPutter;
import com.ge.research.sadl.model.CircularDefinitionException;
import com.ge.research.sadl.model.ConceptIdentifier;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.ConceptName.RangeValueType;
import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.PrefixNotFoundException;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.ConstantNode;
import com.ge.research.sadl.model.gp.EndWrite;
import com.ge.research.sadl.model.gp.Equation;
import com.ge.research.sadl.model.gp.Explain;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.Junction.JunctionType;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.Print;
import com.ge.research.sadl.model.gp.ProxyNode;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.Query.Order;
import com.ge.research.sadl.model.gp.Query.OrderingPair;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Read;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.model.gp.StartWrite;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.TripleElement.TripleSourceType;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.ISadlOntologyHelper.Context;
import com.ge.research.sadl.processing.OntModelProvider;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SadlConstants.OWL_FLAVOR;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.processing.ValidationAcceptorExt;
import com.ge.research.sadl.reasoner.CircularDependencyException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.SadlJenaModelGetter;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.AskExpression;
import com.ge.research.sadl.sADL.BinaryOperation;
import com.ge.research.sadl.sADL.BooleanLiteral;
import com.ge.research.sadl.sADL.Constant;
import com.ge.research.sadl.sADL.ConstructExpression;
import com.ge.research.sadl.sADL.Declaration;
import com.ge.research.sadl.sADL.ElementInList;
import com.ge.research.sadl.sADL.EndWriteStatement;
import com.ge.research.sadl.sADL.EquationStatement;
import com.ge.research.sadl.sADL.ExplainStatement;
import com.ge.research.sadl.sADL.Expression;
import com.ge.research.sadl.sADL.ExpressionStatement;
import com.ge.research.sadl.sADL.ExternalEquationStatement;
import com.ge.research.sadl.sADL.Name;
import com.ge.research.sadl.sADL.NamedStructureAnnotation;
import com.ge.research.sadl.sADL.NumberLiteral;
import com.ge.research.sadl.sADL.OrderElement;
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
import com.ge.research.sadl.sADL.SadlDefaultValue;
import com.ge.research.sadl.sADL.SadlDifferentFrom;
import com.ge.research.sadl.sADL.SadlDisjointClasses;
import com.ge.research.sadl.sADL.SadlExplicitValue;
import com.ge.research.sadl.sADL.SadlHasValueCondition;
import com.ge.research.sadl.sADL.SadlImport;
import com.ge.research.sadl.sADL.SadlInstance;
import com.ge.research.sadl.sADL.SadlIntersectionType;
import com.ge.research.sadl.sADL.SadlIsAnnotation;
import com.ge.research.sadl.sADL.SadlIsInverseOf;
import com.ge.research.sadl.sADL.SadlIsSymmetrical;
import com.ge.research.sadl.sADL.SadlIsTransitive;
import com.ge.research.sadl.sADL.SadlModel;
import com.ge.research.sadl.sADL.SadlModelElement;
import com.ge.research.sadl.sADL.SadlMustBeOneOf;
import com.ge.research.sadl.sADL.SadlNecessaryAndSufficient;
import com.ge.research.sadl.sADL.SadlNestedInstance;
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
import com.ge.research.sadl.sADL.SadlUnaryExpression;
import com.ge.research.sadl.sADL.SadlUnionType;
import com.ge.research.sadl.sADL.SadlValueList;
import com.ge.research.sadl.sADL.SelectExpression;
import com.ge.research.sadl.sADL.StartWriteStatement;
import com.ge.research.sadl.sADL.StringLiteral;
import com.ge.research.sadl.sADL.SubjHasProp;
import com.ge.research.sadl.sADL.Sublist;
import com.ge.research.sadl.sADL.TestStatement;
import com.ge.research.sadl.sADL.UnaryExpression;
import com.ge.research.sadl.sADL.UnitExpression;
import com.ge.research.sadl.sADL.ValueRow;
import com.ge.research.sadl.sADL.ValueTable;
import com.ge.research.sadl.utils.PathToFileUriConverter;
import com.ge.research.sadl.utils.ResourceManager;
import com.ge.research.sadl.utils.SadlASTUtils;
import com.google.common.base.Preconditions;
import com.hp.hpl.jena.datatypes.TypeMapper;
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
import com.hp.hpl.jena.rdf.model.NodeIterator;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFList;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.RDFWriter;
import com.hp.hpl.jena.rdf.model.ResourceFactory;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.sparql.JenaTransactionException;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.OWL2;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;
import com.hp.hpl.jena.vocabulary.XSD;

public class JenaBasedSadlModelProcessor extends SadlModelProcessor implements IJenaBasedModelProcessor {
	public static final String THERE_EXISTS = "thereExists";

	private static final Logger logger = LoggerFactory.getLogger(JenaBasedSadlModelProcessor.class);

	public final static String XSDNS = XSD.getURI();

	public final static Property xsdProperty(String local) {
		return ResourceFactory.createProperty(XSDNS + local);
	}

	private Resource currentResource;
	protected OntModel theJenaModel;
	protected OntModelSpec spec;
	private OWL_FLAVOR owlFlavor = OWL_FLAVOR.OWL_DL;
	// protected ISadlServer kServer = null;

	public enum AnnType {
		ALIAS, NOTE
	}

	private List<String> comparisonOperators = Arrays.asList(">=", ">", "<=", "<", "==", "!=", "is", "=", "not",
			"unique", "in", "contains", "does", /* "not", */"contain");
	private List<String> numericOperators = Arrays.asList("*", "+", "/", "-", "%", "^");
	private List<String> numericComparisonOperators = Arrays.asList(">=", ">", "<=", "<");
	private List<String> equalityInequalityComparisonOperators = Arrays.asList("==", "!=", "is", "=");
	private List<String> canBeNumericOperators = Arrays.asList(">=", ">", "<=", "<", "==", "!=", "is", "=");

	public enum OPERATORS_RETURNING_BOOLEAN {
		contains, unique, is, gt, ge, lt, le, and, or, not, was, hasBeen
	}

	public enum BOOLEAN_LITERAL_TEST {
		BOOLEAN_TRUE, BOOLEAN_FALSE, NOT_BOOLEAN, NOT_BOOLEAN_NEGATED
	}

	private int vNum = 0; // used to create unique variables
	private List<String> userDefinedVariables = new ArrayList<String>();

	// A "crule" variable has a type, a number indicating its ordinal, and a name
	// They are stored by type as key to a list of names, the index in the list is
	// its ordinal number
	private Map<NamedNode, List<VariableNode>> cruleVariables = null;
	private List<EObject> preprocessedEObjects = null;

	protected String modelName;
	protected String modelAlias;
	protected String modelNamespace;
	private OntDocumentManager jenaDocumentMgr;
	protected IConfigurationManagerForIDE configMgr;

	private OntModel sadlBaseModel = null;

	private OntModel sadlListModel = null;
	private OntModel sadlDefaultsModel = null;

	private OntModel sadlImplicitModel = null;
	private OntModel sadlBuiltinFunctionModel = null;

	protected JenaBasedSadlModelValidator modelValidator = null;
	protected ValidationAcceptor issueAcceptor = null;
	protected CancelIndicator cancelIndicator = null;

	private boolean lookingForFirstProperty = false; // in rules and other constructs, the first property may be
														// significant (the binding, for example)

	protected List<String> importsInOrderOfAppearance = null; // an ordered set of import URIs, ordered by appearance in
																// file.
	private List<Rule> rules = null;
	private List<Equation> equations = null;
	private Equation currentEquation = null;
	private List<SadlCommand> sadlCommands = null;
	private SadlCommand targetCommand = null;

	private List<EObject> operationsPullingUp = null;

	int modelErrorCount = 0;
	int modelWarningCount = 0;
	int modelInfoCount = 0;

	protected IntermediateFormTranslator intermediateFormTranslator = null;

	protected boolean generationInProgress = false;

	public static String[] reservedFolderNames = { "Graphs", "OwlModels", "Temp",
			SadlConstants.SADL_IMPLICIT_MODEL_FOLDER };
	public static String[] reservedFileNames = { "Project.sadl", "SadlBaseModel.sadl", "SadlListModel.sadl",
			"RulePatterns.sadl", "RulePatternsData.sadl", "SadlServicesConfigurationConcepts.sadl",
			"ServicesConfig.sadl", "defaults.sadl", "SadlImplicitModel.sadl", "SadlBuiltinFunctions.sadl" };
	public static String[] reservedModelURIs = { SadlConstants.SADL_BASE_MODEL_URI, SadlConstants.SADL_LIST_MODEL_URI,
			SadlConstants.SADL_RULE_PATTERN_URI, SadlConstants.SADL_RULE_PATTERN_DATA_URI,
			SadlConstants.SADL_SERIVCES_CONFIGURATION_CONCEPTS_URI, SadlConstants.SADL_SERIVCES_CONFIGURATION_URI,
			SadlConstants.SADL_DEFAULTS_MODEL_URI };
	public static String[] reservedPrefixes = { SadlConstants.SADL_BASE_MODEL_PREFIX,
			SadlConstants.SADL_LIST_MODEL_PREFIX, SadlConstants.SADL_DEFAULTS_MODEL_PREFIX };

	protected boolean includeImpliedPropertiesInTranslation = false; // should implied properties be included in
																		// translator output? default false

	private DeclarationExtensions declarationExtensions;

	public JenaBasedSadlModelProcessor() {
		logger.debug("New " + this.getClass().getCanonicalName() + "' created");
		setDeclarationExtensions(new DeclarationExtensions());
	}

	/**
	 * For TESTING
	 * 
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
	
	//------ PEFERENCE GETTERS AND SETTERS --------
	public void setUseArticlesInValidation(boolean useArticlesInValidation) {
		this.useArticlesInValidation = useArticlesInValidation;
	}
	
	public void setTypeUnsupportedDownstream(boolean useTypeUnsupportedDownstreamWarnings) {
		this.typeUnsupportedDownstreamWarnings = useTypeUnsupportedDownstreamWarnings;
	}
	
	public void setIncludeImpliedPropertiesInTranslation(boolean useIncludeImpliedPropertiesInTranslation) {
		this.includeImpliedPropertiesInTranslation = useIncludeImpliedPropertiesInTranslation;
	}


	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.ge.research.sadl.jena.IJenaBasedModelProcessor#onGenerate(org.eclipse.emf
	 * .ecore.resource.Resource, org.eclipse.xtext.generator.IFileSystemAccess2,
	 * com.ge.research.sadl.processing.IModelProcessor.ProcessorContext)
	 */
	@Override
	public void onGenerate(Resource resource, IFileSystemAccess2 fsa, ProcessorContext context) {
				
		generationInProgress = true;
		setProcessorContext(context);
		List<String[]> newMappings = new ArrayList<String[]>();
		logger.debug("onGenerate called for Resource '" + resource.getURI() + "'");
		// System.out.println("onGenerate called for Resource '" + resource.getURI() +
		// "'");
		// save the model
		if (getTheJenaModel() == null) {
			OntModel m = OntModelProvider.find(resource);
			theJenaModel = m;
			setModelName(OntModelProvider.getModelName(resource));
			setModelAlias(OntModelProvider.getModelPrefix(resource));
		}
		if (fsa != null) {
			String format = getOwlModelFormat(context);
			try {
				ITranslator translator = null;
				List<SadlCommand> cmds = getSadlCommands();
				if (cmds != null) {
					Iterator<SadlCommand> cmditr = cmds.iterator();
					List<String> namedQueryList = null;
					while (cmditr.hasNext()) {
						SadlCommand cmd = cmditr.next();
						if (cmd instanceof Query && ((Query) cmd).getName() != null) {
							if (translator == null) {
								translator = getConfigMgr(resource, format).getTranslator();
								namedQueryList = new ArrayList<String>();
							}
							Individual queryInst = getTheJenaModel().getIndividual(((Query) cmd).getFqName());
							if (queryInst != null && !namedQueryList.contains(queryInst.getURI())) {
								try {
									String translatedQuery = null;
									try {
										translatedQuery = translator.translateQuery(getTheJenaModel(), (Query) cmd);
									} catch (UnsupportedOperationException e) {
										IReasoner defaultReasoner = getConfigMgr(resource, format)
												.getOtherReasoner(ConfigurationManager.DEFAULT_REASONER);
										translator = getConfigMgr(resource, format)
												.getTranslatorForReasoner(defaultReasoner);
										translatedQuery = translator.translateQuery(getTheJenaModel(), (Query) cmd);
									}
									Literal queryLit = getTheJenaModel().createTypedLiteral(translatedQuery);
									queryInst.addProperty(RDFS.isDefinedBy, queryLit);
									namedQueryList.add(queryInst.getURI());
								} catch (TranslationException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								} catch (InvalidNameException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								}
							}
						}
					}
				}
			} catch (ConfigurationException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}

			// // Output the OWL file for the ontology model
			URI lastSeg = fsa.getURI(resource.getURI().lastSegment());
			String owlFN = lastSeg.trimFileExtension().appendFileExtension(ResourceManager.getOwlFileExtension(format))
					.lastSegment().toString();
			RDFWriter w = getTheJenaModel().getWriter(format);
			w.setProperty("xmlbase", getModelName());
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			w.write(getTheJenaModel().getBaseModel(), out, getModelName());
			Charset charset = Charset.forName("UTF-8");
			CharSequence seq = new String(out.toByteArray(), charset);
			fsa.generateFile(owlFN, seq);

			// // if there are equations, output them to a Prolog file
			// List<Equation> eqs = getEquations();
			// if (eqs != null) {
			// StringBuilder sb = new StringBuilder();
			// for (int i = 0; i < eqs.size(); i++) {
			// sb.append(eqs.get(i).toFullyQualifiedString());
			// sb.append("\n");
			// }
			// fsa.generateFile(lastSeg.appendFileExtension("pl").lastSegment().toString(),
			// sb.toString());
			// }

			try {
				String modelFolder = getModelFolderPath(resource);
				SadlUtils su = new SadlUtils();
				String fn = SadlConstants.SADL_BASE_MODEL_FILENAME + "." + ResourceManager.getOwlFileExtension(format);
				if (!fileExists(fsa, fn)) {
					sadlBaseModel = OntModelProvider.getSadlBaseModel();
					if (sadlBaseModel != null) {
						RDFWriter w2 = sadlBaseModel.getWriter(format);
						w.setProperty("xmlbase", SadlConstants.SADL_BASE_MODEL_URI);
						ByteArrayOutputStream out2 = new ByteArrayOutputStream();
						w2.write(sadlBaseModel.getBaseModel(), out2, SadlConstants.SADL_BASE_MODEL_URI);
						CharSequence seq2 = new String(out2.toByteArray(), charset);
						fsa.generateFile(fn, seq2);
						String[] mapping = new String[3];
						mapping[0] = su.fileNameToFileUrl(modelFolder + "/" + fn);
						mapping[1] = SadlConstants.SADL_BASE_MODEL_URI;
						mapping[2] = SadlConstants.SADL_BASE_MODEL_PREFIX;
						newMappings.add(mapping);
					}
				}
				fn = SadlConstants.SADL_LIST_MODEL_FILENAME + "." + ResourceManager.getOwlFileExtension(format);
				if (!fileExists(fsa, fn)) {
					sadlListModel = OntModelProvider.getSadlListModel();
					if (sadlListModel != null) {
						RDFWriter w2 = sadlListModel.getWriter(format);
						w.setProperty("xmlbase", SadlConstants.SADL_LIST_MODEL_URI);
						ByteArrayOutputStream out2 = new ByteArrayOutputStream();
						w2.write(sadlListModel.getBaseModel(), out2, SadlConstants.SADL_LIST_MODEL_URI);
						CharSequence seq2 = new String(out2.toByteArray(), charset);
						fsa.generateFile(fn, seq2);
						String[] mapping = new String[3];
						mapping[0] = su.fileNameToFileUrl(modelFolder + "/" + fn);
						mapping[1] = SadlConstants.SADL_LIST_MODEL_URI;
						mapping[2] = SadlConstants.SADL_LIST_MODEL_PREFIX;
						newMappings.add(mapping);
					}
				}
				fn = SadlConstants.SADL_DEFAULTS_MODEL_FILENAME + "." + ResourceManager.getOwlFileExtension(format);
				if (!fileExists(fsa, fn)) {
					sadlDefaultsModel = OntModelProvider.getSadlDefaultsModel();
					if (sadlDefaultsModel != null) {
						RDFWriter w2 = sadlDefaultsModel.getWriter(format);
						w.setProperty("xmlbase", SadlConstants.SADL_DEFAULTS_MODEL_URI);
						ByteArrayOutputStream out2 = new ByteArrayOutputStream();
						w2.write(sadlDefaultsModel.getBaseModel(), out2, SadlConstants.SADL_DEFAULTS_MODEL_URI);
						CharSequence seq2 = new String(out2.toByteArray(), charset);
						fsa.generateFile(fn, seq2);
						String[] mapping = new String[3];
						mapping[0] = su.fileNameToFileUrl(modelFolder + "/" + fn);
						mapping[1] = SadlConstants.SADL_DEFAULTS_MODEL_URI;
						mapping[2] = SadlConstants.SADL_DEFAULTS_MODEL_PREFIX;
						newMappings.add(mapping);
					}
				}

				String[] mapping = new String[3];
				mapping[0] = su.fileNameToFileUrl(modelFolder + "/" + owlFN);
				mapping[1] = getModelName();
				mapping[2] = getModelAlias();

				newMappings.add(mapping);

				// Output the Rules and any other knowledge structures via the specified
				// translator
				List<Object> otherContent = OntModelProvider.getOtherContent(resource);
				if (otherContent != null) {
					for (int i = 0; i < otherContent.size(); i++) {
						Object oc = otherContent.get(i);
						if (oc instanceof List<?>) {
							if (((List<?>) oc).get(0) instanceof Equation) {
								setEquations((List<Equation>) oc);
							} else if (((List<?>) oc).get(0) instanceof Rule) {
								rules = (List<Rule>) oc;
							}
						}
					}
				}
				List<ModelError> results = translateAndSaveModel(resource, owlFN, format, newMappings);
				if (results != null) {
					generationInProgress = false; // we need these errors to show up
					modelErrorsToOutput(resource, results);
				}
			} catch (Exception e) {

			}
		}
		generationInProgress = false;
		logger.debug("onGenerate completed for Resource '" + resource.getURI() + "'");
	}

	// akitta: get rid of this hack once
	// https://github.com/eclipse/xtext-core/issues/180 is fixed
	private boolean fileExists(IFileSystemAccess2 fsa, String fileName) {
		try {
			return fsa.isFile(fileName);
		} catch (Exception e) {
			return false;
		}
	}

	private List<ModelError> translateAndSaveModel(Resource resource, String owlFN, String _repoType,
			List<String[]> newMappings) {
		String modelFolderPathname = getModelFolderPath(resource);
		try {
			if (newMappings != null) {
				getConfigMgr(resource, _repoType).addMappings(newMappings, false, "SADL");
			}
			ITranslator translator = getConfigMgr(resource, _repoType).getTranslator();
			List<ModelError> results = translator.translateAndSaveModel(getTheJenaModel(), getRules(),
					modelFolderPathname, getModelName(), getImportsInOrderOfAppearance(), owlFN);
			if (results != null) {
				modelErrorsToOutput(resource, results);
			} else if (getOtherKnowledgeStructure(resource) != null) {
				results = translator.translateAndSaveModelWithOtherStructure(getTheJenaModel(),
						getOtherKnowledgeStructure(resource), modelFolderPathname, getModelName(),
						getImportsInOrderOfAppearance(), owlFN);
				return results;
			}
		} catch (MalformedURLException e1) {
			e1.printStackTrace();
		} catch (com.ge.research.sadl.reasoner.ConfigurationException e) {
			e.printStackTrace();
		} catch (TranslationException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (URISyntaxException e) {
			e.printStackTrace();
		}

		return null;
	}

	private String getModelFolderPath(Resource resource) {
		final URI resourceUri = resource.getURI();
		final URI modelFolderUri = resourceUri.trimSegments(resourceUri.isFile() ? 1 : resourceUri.segmentCount() - 2)
				.appendSegment(UtilsForJena.OWL_MODELS_FOLDER_NAME);

		if (resourceUri.isPlatformResource()) {
			final IFile file = ResourcesPlugin.getWorkspace().getRoot()
					.getFile(new Path(modelFolderUri.toPlatformString(true)));
			return file.getRawLocation().toPortableString();
		} else {
			final String modelFolderPathname = findModelFolderPath(resource.getURI());
			return modelFolderPathname == null ? modelFolderUri.toFileString() : modelFolderPathname;
		}
	}

	static String findProjectPath(URI uri) {
		String modelFolder = findModelFolderPath(uri);
		if (modelFolder != null) {
			return new File(modelFolder).getParent();
		}
		return null;
	}

	public static String findModelFolderPath(URI uri) {
		File file = new File(uri.path());
		if (file != null) {
			if (file.isDirectory()) {
				if (file.getAbsolutePath().endsWith(UtilsForJena.OWL_MODELS_FOLDER_NAME)) {
					return file.getAbsolutePath();
				}

				for (File child : file.listFiles()) {
					if (child.getAbsolutePath().endsWith(UtilsForJena.OWL_MODELS_FOLDER_NAME)) {
						return child.getAbsolutePath();
					}
				}
				// Didn't find a project file in this directory, check parent
				if (file.getParentFile() != null) {
					return findModelFolderPath(uri.trimSegments(1));
				}
			}
			if (file.isFile() && file.getParentFile() != null) {
				return findModelFolderPath(uri.trimSegments(1));
			}
		}

		return null;
	}

	private Object getOtherKnowledgeStructure(Resource resource) {
		if (getEquations(resource) != null) {
			return getEquations(resource);
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
	 * Method to retrieve a list of the model's imports ordered according to
	 * appearance
	 * 
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

	private String reasonerClassName = null;
	private String translatorClassName = null;

	//------------PEFERENCES ---------------
	protected boolean ignoreUnittedQuantities;

	private boolean useArticlesInValidation;

	protected boolean domainAndRangeAsUnionClasses = true;

	private boolean typeCheckingWarningsOnly;
	
	private boolean typeUnsupportedDownstreamWarnings;
	
	//-----------END PEFERENCES --------------

	private List<OntResource> allImpliedPropertyClasses = null;

	private List<OntResource> allExpandedPropertyClasses = null;

	private ArrayList<Object> intermediateFormResults = null;

	private EObject hostEObject = null;

	private Map<EObject, VariableNode> variablesInDefinition = null;

	public static void refreshResource(Resource newRsrc) {
		try {
			URI uri = newRsrc.getURI();
			uri = newRsrc.getResourceSet().getURIConverter().normalize(uri);
			String scheme = uri.scheme();
			if ("platform".equals(scheme) && uri.segmentCount() > 1 && "resource".equals(uri.segment(0))) {
				StringBuffer platformResourcePath = new StringBuffer();
				for (int j = 1, size = uri.segmentCount() - 1; j < size; ++j) {
					platformResourcePath.append('/');
					platformResourcePath.append(uri.segment(j));
				}
				IResource r = ResourcesPlugin.getWorkspace().getRoot()
						.getFile(new Path(platformResourcePath.toString()));
				r.refreshLocal(IResource.DEPTH_INFINITE, null);
			}
		} catch (Throwable t) {
			// this will happen if in test environment
		}
	}

	@Override
	public void validate(Context context, SadlResource candidate) {
		ValidationAcceptor savedIssueAccpetor = this.issueAcceptor;
		setIssueAcceptor(context.getAcceptor());

		String contextId = context.getGrammarContextId().orNull();
		OntModel ontModel = context.getOntModel();
		SadlResource subject = context.getSubject();
		System.out.println("Subject: " + getDeclarationExtensions().getConceptUri(subject));
		System.out.println("Candidate: " + getDeclarationExtensions().getConceptUri(candidate));

		try {
			if (subject == MISSING_SUBJECT) {
				return;
			}
			switch (contextId) {
			case SADLPROPERTYINITIALIZER_PROPERTY: {
				OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
				if (!isProperty(candtype)) {
					context.getAcceptor().add("No", candidate, Severity.ERROR);
					return;
				}
				modelValidator.checkPropertyDomain(ontModel, subject, candidate, candidate, true);
				return;
			}
			case SADLPROPERTYINITIALIZER_VALUE: {
				SadlResource prop = context.getRestrictions().iterator().next();
				OntConceptType proptype = getDeclarationExtensions().getOntConceptType(prop);
				if (proptype.equals(OntConceptType.DATATYPE_PROPERTY)) {
					context.getAcceptor().add("No", candidate, Severity.ERROR);
					return;
				}
				if (proptype.equals(OntConceptType.CLASS_PROPERTY)) {
					OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
					if (!candtype.equals(OntConceptType.INSTANCE)) {
						context.getAcceptor().add("No", candidate, Severity.ERROR);
						return;
					}
				}
				Iterator<SadlResource> ritr = context.getRestrictions().iterator();
				while (ritr.hasNext()) {
					System.out.println("Restriction: " + getDeclarationExtensions().getConceptUri(ritr.next()));
				}
				modelValidator.checkPropertyDomain(ontModel, subject, prop, subject, true);
				StringBuilder errorMessageBuilder = new StringBuilder();
				if (!modelValidator.validateBinaryOperationByParts(candidate, prop, candidate, "is",
						errorMessageBuilder, false)) {
					context.getAcceptor().add(errorMessageBuilder.toString(), candidate, Severity.ERROR);
				}
				return;
			}
			case SADLSTATEMENT_SUPERELEMENT: {
				OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
				if (candtype.equals(OntConceptType.CLASS) || candtype.equals(OntConceptType.CLASS_LIST)
						|| candtype.equals(OntConceptType.CLASS_PROPERTY) || candtype.equals(OntConceptType.DATATYPE)
						|| candtype.equals(OntConceptType.DATATYPE_LIST)
						|| candtype.equals(OntConceptType.DATATYPE_PROPERTY)
						|| candtype.equals(OntConceptType.RDF_PROPERTY)) {
					return;
				}
				context.getAcceptor().add("No", candidate, Severity.ERROR);
			}
			case PROPOFSUBJECT_RIGHT: {
				OntConceptType subjtype = getDeclarationExtensions().getOntConceptType(subject);
				OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
				if ((candtype.equals(OntConceptType.CLASS) || candtype.equals(OntConceptType.INSTANCE))
						&& isProperty(subjtype)) {
					modelValidator.checkPropertyDomain(ontModel, candidate, subject, candidate, true);
					return;
				}
				context.getAcceptor().add("No", candidate, Severity.ERROR);
				return;

			}
			case PROPOFSUBJECT_PROP: {
				OntConceptType subjtype = getDeclarationExtensions().getOntConceptType(subject);
				OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
				if ((candtype.equals(OntConceptType.CLASS) || candtype.equals(OntConceptType.INSTANCE))
						&& isProperty(subjtype)) {
					modelValidator.checkPropertyDomain(ontModel, candidate, subject, candidate, true);
					return;
				}
				context.getAcceptor().add("No", candidate, Severity.ERROR);
				return;
			}
			default: {
				// Ignored
			}
			}
		} catch (InvalidTypeException e) {
			throw new RuntimeException(e);
		} catch (CircularDefinitionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} finally {
			if (savedIssueAccpetor != null) {
				setIssueAcceptor(savedIssueAccpetor);
			}
		}
	}

	@Override
	public boolean isSupported(String fileExtension) {
		return "sadl".equals(fileExtension);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.ge.research.sadl.jena.IJenaBasedModelProcessor#onValidate(org.eclipse.emf
	 * .ecore.resource.Resource, com.ge.research.sadl.processing.ValidationAcceptor,
	 * org.eclipse.xtext.validation.CheckMode,
	 * com.ge.research.sadl.processing.IModelProcessor.ProcessorContext)
	 */
	@Override
	public void onValidate(Resource resource, ValidationAcceptor issueAcceptor, CheckMode mode,
			ProcessorContext context) {

		logger.debug("onValidate called for Resource '" + resource.getURI() + "'");
		if (mode.shouldCheck(CheckType.EXPENSIVE)) {
			// do expensive validation, i.e. those that should only be done when 'validate'
			// action was invoked.
		}
		setIssueAcceptor(issueAcceptor);
		setProcessorContext(context);
		setCancelIndicator(cancelIndicator);
		if (resource.getContents().size() < 1) {
			return;
		}
		setCurrentResource(resource);
		SadlModel model = (SadlModel) resource.getContents().get(0);
		String modelActualUrl = resource.getURI().lastSegment();
		validateResourcePathAndName(resource, model, modelActualUrl);
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
			e1.printStackTrace();
			addError(SadlErrorMessages.CONFIGURATION_ERROR.get(e1.getMessage()), model);
			addError(e1.getMessage(), model);
			return; // this is a fatal error
		}
		getTheJenaModel().setNsPrefix(getModelAlias(), getModelNamespace());
		Ontology modelOntology = getTheJenaModel().createOntology(modelName);
		logger.debug("Ontology '" + modelName + "' created");
		modelOntology.addComment("This ontology was created from a SADL file '" + modelActualUrl
				+ "' and should not be directly edited.", "en");

		String modelVersion = model.getVersion();
		if (modelVersion != null) {
			modelOntology.addVersionInfo(modelVersion);
		}

		EList<SadlAnnotation> anns = model.getAnnotations();
		addAnnotationsToResource(modelOntology, anns);

		OntModelProvider.registerResource(resource);

		try {
			// Add SadlBaseModel to everything except the SadlImplicitModel
			if (!resource.getURI().lastSegment().equals(SadlConstants.SADL_IMPLICIT_MODEL_FILENAME)) {
				addSadlBaseModelImportToJenaModel(resource);
			}
			// Add the SadlImplicitModel to everything except itself and the
			// SadlBuilinFunctions
			if (!resource.getURI().lastSegment().equals(SadlConstants.SADL_IMPLICIT_MODEL_FILENAME)
					&& !resource.getURI().lastSegment().equals(SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME)) {
				addImplicitSadlModelImportToJenaModel(resource, context);
				addImplicitBuiltinFunctionModelImportToJenaModel(resource, context);

			}
		} catch (IOException e1) {
			e1.printStackTrace();
		} catch (ConfigurationException e1) {
			e1.printStackTrace();
		} catch (URISyntaxException e1) {
			e1.printStackTrace();
		} catch (JenaProcessorException e1) {
			e1.printStackTrace();
		}

		if (!processModelImports(modelOntology, resource.getURI(), model)) {
			System.err.println("Unable to import models. Is project build enabled?");
		}

		boolean enableMetricsCollection = true; // no longer a preference
		try {
			if (enableMetricsCollection) {
				if (!isSyntheticUri(null, resource)) {
					setMetricsProcessor(new MetricsProcessor(modelName, resource,
							getConfigMgr(resource, getOwlModelFormat(context)), this));
				}
			}
		} catch (JenaProcessorException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		initializePreferences(context);

		// create validator for expressions
		initializeModelValidator();
		initializeAllImpliedPropertyClasses();
		initializeAllExpandedPropertyClasses();

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
				processModelElement(element);
			}
		}
		logger.debug("onValidate completed for Resource '" + resource.getURI() + "'");
		if (getSadlCommands() != null && getSadlCommands().size() > 0) {
			OntModelProvider.attach(model.eResource(), getTheJenaModel(), getModelName(), getModelAlias(),
					getSadlCommands());
		} else {
			OntModelProvider.attach(model.eResource(), getTheJenaModel(), getModelName(), getModelAlias());
		}
		if (rules != null && rules.size() > 0) {
			List<Object> other = OntModelProvider.getOtherContent(model.eResource());
			if (other != null) {
				other.add(rules);
			} else {
				OntModelProvider.addOtherContent(model.eResource(), rules);
			}
		}
		if (issueAcceptor instanceof ValidationAcceptorExt) {
			final ValidationAcceptorExt acceptor = (ValidationAcceptorExt) issueAcceptor;
			try {
				if (!resource.getURI().lastSegment().equals("SadlImplicitModel.sadl")
						&& !resource.getURI().lastSegment().equals(SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME)) {
					// System.out.println("Metrics for '" + resource.getURI().lastSegment() + "':");
					if (acceptor.getErrorCount() > 0) {
						String msg = "    Model totals: " + countPlusLabel(acceptor.getErrorCount(), "error") + ", "
								+ countPlusLabel(acceptor.getWarningCount(), "warning") + ", "
								+ countPlusLabel(acceptor.getInfoCount(), "info");
						// System.out.flush();
						System.err.println("No OWL model output generated for '" + resource.getURI() + "'.");
						System.err.println(msg);
						System.err.flush();
					}
					// else {
					// System.out.println(msg);
					// }
					if (!isSyntheticUri(null, resource)) {
						// don't do metrics on JUnit tests
						if (getMetricsProcessor() != null) {
							getMetricsProcessor().saveMetrics(ConfigurationManager.RDF_XML_ABBREV_FORMAT);
						}
					}
				}
			} catch (IOException e) {
				e.printStackTrace();
			} catch (IllegalStateException e) {
				// this is OK--will happen during standalone testing
			} catch (ConfigurationException e) {
				e.printStackTrace();
			} catch (URISyntaxException e) {
				e.printStackTrace();
			}
		}
	}

	@Override
	public void initializePreferences(ProcessorContext context) {
		setTypeCheckingWarningsOnly(true);
		String typechecking = context.getPreferenceValues().getPreference(SadlPreferences.TYPE_CHECKING_WARNING_ONLY);
		if (typechecking != null) {
			setTypeCheckingWarningsOnly(Boolean.parseBoolean(typechecking));
		}
		ignoreUnittedQuantities = true;
		String ignoreUnits = context.getPreferenceValues().getPreference(SadlPreferences.IGNORE_UNITTEDQUANTITIES);
		if (ignoreUnits != null) {
			ignoreUnittedQuantities = Boolean.parseBoolean(ignoreUnits);
		}

		setUseArticlesInValidation(false);
		String useArticles = context.getPreferenceValues().getPreference(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION);
		if (useArticles != null) {
			setUseArticlesInValidation(Boolean.parseBoolean(useArticles));
		}
		domainAndRangeAsUnionClasses = true;
		String domainAndRangeAsUnionClassesStr = context.getPreferenceValues()
				.getPreference(SadlPreferences.CREATE_DOMAIN_AND_RANGE_AS_UNION_CLASSES);
		if (domainAndRangeAsUnionClassesStr != null) {
			domainAndRangeAsUnionClasses = Boolean.parseBoolean(domainAndRangeAsUnionClassesStr);
		}
	}

	protected void processModelElement(SadlModelElement element) {
		try {
			if (element instanceof SadlClassOrPropertyDeclaration) {
				processSadlClassOrPropertyDeclaration((SadlClassOrPropertyDeclaration) element);
			} else if (element instanceof SadlProperty) {
				processSadlProperty(null, (SadlProperty) element);
			} else if (element instanceof SadlNecessaryAndSufficient) {
				processSadlNecessaryAndSufficient((SadlNecessaryAndSufficient) element);
			} else if (element instanceof SadlDifferentFrom) {
				processSadlDifferentFrom((SadlDifferentFrom) element);
			} else if (element instanceof SadlInstance) {
				processSadlInstance((SadlInstance) element);
			} else if (element instanceof SadlDisjointClasses) {
				processSadlDisjointClasses((SadlDisjointClasses) element);
			} else if (element instanceof SadlSameAs) {
				processSadlSameAs((SadlSameAs) element);
			} else if (element instanceof RuleStatement) {
				processStatement((RuleStatement) element);
			} else if (element instanceof EquationStatement) {
				processStatement((EquationStatement) element);
			} else if (element instanceof PrintStatement) {
				processStatement((PrintStatement) element);
			} else if (element instanceof ReadStatement) {
				processStatement((ReadStatement) element);
			} else if (element instanceof StartWriteStatement) {
				processStatement((StartWriteStatement) element);
			} else if (element instanceof EndWriteStatement) {
				processStatement((EndWriteStatement) element);
			} else if (element instanceof ExplainStatement) {
				processStatement((ExplainStatement) element);
			} else if (element instanceof QueryStatement) {
				processStatement((QueryStatement) element);
			} else if (element instanceof SadlResource) {
				if (!SadlASTUtils.isUnit(element)) {
					processExpression((SadlResource) element);
				}
			} else if (element instanceof TestStatement) {
				processStatement((TestStatement) element);
			} else if (element instanceof ExternalEquationStatement) {
				processStatement((ExternalEquationStatement) element);
			} else if (element instanceof ExpressionStatement) {
				clearCruleVariables();
				Object rawResult = postProcessTranslationResult(
						processExpression(((ExpressionStatement) element).getExpr()));
				if (isSyntheticUri(null, element.eResource())) {
					// for tests, do not expand; expansion, if desired, will be done upon retrieval
					addIntermediateFormResult(rawResult);
				} else if (rawResult != null) {
					// for IDE, expand and also add as info marker
					addInfo(rawResult.toString(), element);
					Object intForm = getIfTranslator().expandProxyNodes(rawResult, false, true);
					if (intForm != null) {
						if (intForm instanceof List<?>) {
							if (((List<?>) intForm).size() > 0) {
								addIntermediateFormResult(intForm);
								addInfo(intForm.toString(), element);
								return;
							}
						}
					}
					if (rawResult instanceof VariableNode) {
						addInfo(((VariableNode) rawResult).toDescriptiveString(), element);
					} else {
						addInfo(rawResult.toString(), element);
					}
				}
			} else {
				throw new JenaProcessorException("onValidate for element of type '"
						+ element.getClass().getCanonicalName() + "' not implemented");
			}
		} catch (JenaProcessorException e) {
			addError(e.getMessage(), element);
		} catch (InvalidNameException e) {
			e.printStackTrace();
		} catch (InvalidTypeException e) {
			e.printStackTrace();
		} catch (TranslationException e) {
			e.printStackTrace();
		} catch (CircularDefinitionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Throwable t) {
			t.printStackTrace();
		}
	}

	private PathToFileUriConverter getUriConverter(Resource resource) {
		return ((XtextResource) resource).getResourceServiceProvider().get(PathToFileUriConverter.class);
	}

	protected void validateResourcePathAndName(Resource resource, SadlModel model, String modelActualUrl) {
		if (!isReservedFolder(resource, model)) {
			if (isReservedName(resource)) {
				if (!isSyntheticUri(null, resource)) {
					addError(SadlErrorMessages.RESERVED_NAME.get(modelActualUrl), model);
				}
			}
		}
	}

	private void addImplicitBuiltinFunctionModelImportToJenaModel(Resource resource, ProcessorContext context)
			throws ConfigurationException, IOException, URISyntaxException, JenaProcessorException {
		if (isSyntheticUri(null, resource)) {
			// test case: get SadlImplicitModel OWL model from the OntModelProvider
			URI simTestUri = URI.createURI(SadlConstants.SADL_BUILTIN_FUNCTIONS_SYNTHETIC_URI);
			try {
				sadlBuiltinFunctionModel = OntModelProvider
						.find(resource.getResourceSet().getResource(simTestUri, true));
			} catch (Exception e) {
				// this happens if the test case doesn't cause the implicit model to be
				// loaded--here now for backward compatibility but test cases should be fixed?
				sadlBuiltinFunctionModel = null;
			}
		} else {
			java.nio.file.Path implfn = checkImplicitBuiltinFunctionModelExistence(resource, context);
			if (implfn != null) {
				if (sadlBuiltinFunctionModel == null) {
					final URI uri = getUri(resource, implfn);
					Resource imrsrc = resource.getResourceSet().getResource(uri, true);
					if (imrsrc instanceof XtextResource) {
						sadlBuiltinFunctionModel = OntModelProvider.find((XtextResource) imrsrc);
					} else if (imrsrc instanceof ExternalEmfResource) {
						sadlBuiltinFunctionModel = ((ExternalEmfResource) imrsrc).getOntModel();
					}
					if (sadlBuiltinFunctionModel == null) {
						if (imrsrc instanceof XtextResource) {
							((XtextResource) imrsrc).getResourceServiceProvider().getResourceValidator()
									.validate(imrsrc, CheckMode.FAST_ONLY, cancelIndicator);
							sadlBuiltinFunctionModel = OntModelProvider.find(imrsrc);
							OntModelProvider.attach(imrsrc, sadlBuiltinFunctionModel,
									SadlConstants.SADL_BUILTIN_FUNCTIONS_URI,
									SadlConstants.SADL_BUILTIN_FUNCTIONS_ALIAS);
						} else {
							IConfigurationManagerForIDE cm = getConfigMgr(resource, getOwlModelFormat(context));
							if (cm.getModelGetter() == null) {
								cm.setModelGetter(new SadlJenaModelGetter(cm, null));
							}
							cm.getModelGetter().getOntModel(SadlConstants.SADL_BUILTIN_FUNCTIONS_URI,
									ResourceManager.getProjectUri(resource).appendSegment(ResourceManager.OWLDIR)
											.appendFragment(SadlConstants.OWL_BUILTIN_FUNCTIONS_FILENAME)
											.toFileString(),
									getOwlModelFormat(context));
						}
					}
				}
			}
		}
		if (sadlBuiltinFunctionModel != null) {
			addImportToJenaModel(getModelName(), SadlConstants.SADL_BUILTIN_FUNCTIONS_URI,
					SadlConstants.SADL_BUILTIN_FUNCTIONS_ALIAS, sadlBuiltinFunctionModel);
		}
	}

	/**
	 * 
	 * @param anyResource
	 *            any resource is just to
	 * @param resourcePath
	 *            the Java NIO path of the resource to load as a
	 *            `platform:/resource/` if the Eclipse platform is running,
	 *            otherwise loads it as a file resource.
	 */
	private URI getUri(Resource anyResource, java.nio.file.Path resourcePath) {
		Preconditions.checkArgument(anyResource instanceof XtextResource,
				"Expected an Xtext resource. Got: " + anyResource);
		final String resourceFileName = resourcePath.getFileName().toString();
		Preconditions.checkArgument(
				resourceFileName.equals(SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME)
				|| resourceFileName.equals(SadlConstants.SADL_IMPLICIT_MODEL_FILENAME),
				"Expected the resource path of either the implicit model or the built-in function model. Got " + resourcePath + " instead.");

		if (EMFPlugin.IS_ECLIPSE_RUNNING) {
			IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
			java.nio.file.Path containerProjectPath = resourcePath.getParent().getParent();
			IProject containerProject = Arrays.stream(workspaceRoot.getProjects())
				.filter(p -> p.isAccessible())
				.filter((p -> Paths.get(p.getLocationURI()).equals(containerProjectPath)))
				.findFirst()
				.orElseThrow(() -> new RuntimeException("Cannot locate container project for " + resourcePath + "."));
			IPath relativeResourcePath = new Path(containerProject.getName()).append(containerProjectPath.relativize(resourcePath).toString());
			return URI.createPlatformResourceURI(relativeResourcePath.toOSString(), true);
		} else {
			final PathToFileUriConverter uriConverter = getUriConverter(anyResource);
			return uriConverter.createFileUri(resourcePath);
		}

	}

	private java.nio.file.Path checkImplicitBuiltinFunctionModelExistence(Resource resource, ProcessorContext context)
			throws IOException, ConfigurationException {
		UtilsForJena ufj = new UtilsForJena();
		String policyFileUrl = ufj.getPolicyFilename(resource);
		String policyFilename = policyFileUrl != null ? ufj.fileUrlToFileName(policyFileUrl) : null;
		if (policyFilename != null) {
			File projectFolder = new File(policyFilename).getParentFile().getParentFile();
			if (projectFolder == null) {
				return null;
			}
			String relPath = SadlConstants.SADL_IMPLICIT_MODEL_FOLDER + "/"
					+ SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME;
			String platformPath = projectFolder.getName() + "/" + relPath;
			String implicitSadlModelFN = projectFolder + "/" + relPath;
			File implicitModelFile = new File(implicitSadlModelFN);
			if (!implicitModelFile.exists()) {
				createBuiltinFunctionImplicitModel(projectFolder.getAbsolutePath());
				try {
					Resource newRsrc = resource.getResourceSet()
							.createResource(URI.createPlatformResourceURI(platformPath, false));
					newRsrc.load(resource.getResourceSet().getLoadOptions());
					refreshResource(newRsrc);
				} catch (Throwable t) {
				}
			}
			return implicitModelFile.getAbsoluteFile().toPath();
		}
		return null;
	}

	private void addImplicitSadlModelImportToJenaModel(Resource resource, ProcessorContext context)
			throws IOException, ConfigurationException, URISyntaxException, JenaProcessorException {
		if (isSyntheticUri(null, resource)) {
			// test case: get SadlImplicitModel OWL model from the OntModelProvider
			URI simTestUri = URI.createURI(SadlConstants.SADL_IMPLICIT_MODEL_SYNTHETIC_URI);
			try {
				sadlImplicitModel = OntModelProvider.find(resource.getResourceSet().getResource(simTestUri, true));
			} catch (Exception e) {
				// this happens if the test case doesn't cause the implicit model to be
				// loaded--here now for backward compatibility but test cases should be fixed?
				sadlImplicitModel = null;
			}
		} else {
			java.nio.file.Path implfn = checkImplicitSadlModelExistence(resource, context);
			if (implfn != null) {
				if (sadlImplicitModel == null) {
					final URI uri = getUri(resource, implfn);
					Resource imrsrc = resource.getResourceSet().getResource(uri, true);
					if (imrsrc instanceof XtextResource) {
						sadlImplicitModel = OntModelProvider.find((XtextResource) imrsrc);
					} else if (imrsrc instanceof ExternalEmfResource) {
						sadlImplicitModel = ((ExternalEmfResource) imrsrc).getOntModel();
					}
					if (sadlImplicitModel == null) {
						if (imrsrc instanceof XtextResource) {
							((XtextResource) imrsrc).getResourceServiceProvider().getResourceValidator()
									.validate(imrsrc, CheckMode.FAST_ONLY, cancelIndicator);
							sadlImplicitModel = OntModelProvider.find(imrsrc);
							OntModelProvider.attach(imrsrc, sadlImplicitModel, SadlConstants.SADL_IMPLICIT_MODEL_URI,
									SadlConstants.SADL_IMPLICIT_MODEL_PREFIX);
						} else {
							IConfigurationManagerForIDE cm = getConfigMgr(resource, getOwlModelFormat(context));
							if (cm.getModelGetter() == null) {
								cm.setModelGetter(new SadlJenaModelGetter(cm, null));
							}
							cm.getModelGetter().getOntModel(SadlConstants.SADL_IMPLICIT_MODEL_URI,
									ResourceManager.getProjectUri(resource).appendSegment(ResourceManager.OWLDIR)
											.appendFragment(SadlConstants.OWL_IMPLICIT_MODEL_FILENAME).toFileString(),
									getOwlModelFormat(context));
						}
					}
				}
			}
		}
		if (sadlImplicitModel != null) {
			addImportToJenaModel(getModelName(), SadlConstants.SADL_IMPLICIT_MODEL_URI,
					SadlConstants.SADL_IMPLICIT_MODEL_PREFIX, sadlImplicitModel);
		}
	}

	private void addSadlBaseModelImportToJenaModel(Resource resource)
			throws IOException, ConfigurationException, URISyntaxException, JenaProcessorException {
		if (sadlBaseModel == null) {
			sadlBaseModel = OntModelProvider.getSadlBaseModel();
			if (sadlBaseModel == null) {
				sadlBaseModel = getOntModelFromString(resource, getSadlBaseModel());
				OntModelProvider.setSadlBaseModel(sadlBaseModel);
			}
		}
		addImportToJenaModel(getModelName(), SadlConstants.SADL_BASE_MODEL_URI, SadlConstants.SADL_BASE_MODEL_PREFIX,
				sadlBaseModel);
	}

	protected void addAnnotationsToResource(OntResource modelOntology, EList<SadlAnnotation> anns) {
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
				} else if (anntype.equalsIgnoreCase(AnnType.NOTE.toString())) {
					modelOntology.addComment(annContent, "en");
				}
			}
		}
	}

	public OntModel prepareEmptyOntModel(Resource resource) throws ConfigurationException {
		try {
			IConfigurationManagerForIDE cm = getConfigMgr(resource, getOwlModelFormat(getProcessorContext()));
			OntDocumentManager owlDocMgr = cm.getJenaDocumentMgr();
			OntModelSpec spec = new OntModelSpec(OntModelSpec.OWL_MEM);
			setSpec(spec);
			String modelFolderPathname = getModelFolderPath(resource);
			if (modelFolderPathname != null && !modelFolderPathname.startsWith(SYNTHETIC_FROM_TEST)) {
				File mff = new File(modelFolderPathname);
				mff.mkdirs();
				spec.setImportModelGetter(new SadlJenaModelGetterPutter(spec, modelFolderPathname));
			}
			if (owlDocMgr != null) {
				spec.setDocumentManager(owlDocMgr);
				owlDocMgr.setProcessImports(true);
			}
			return ModelFactory.createOntologyModel(spec);
		} catch (ConfigurationException e) {
			e.printStackTrace();
			throw e;
		} catch (Exception e) {
			e.printStackTrace();
			throw new ConfigurationException(e.getMessage(), e);
		}
	}

	protected void setProcessorContext(ProcessorContext ctx) {
		processorContext = ctx;
	}

	protected ProcessorContext getProcessorContext() {
		return processorContext;
	}

	protected String countPlusLabel(int count, String label) {
		if (count == 0 || count > 1) {
			label = label + "s";
		}
		return "" + count + " " + label;
	}

	private void addImportToJenaModel(String modelName, String importUri, String importPrefix, Model importedOntModel) {
		getTheJenaModel().getDocumentManager().addModel(importUri, importedOntModel, true);
		Ontology modelOntology = getTheJenaModel().createOntology(modelName);
		if (importPrefix == null) {
			try {
				importPrefix = getConfigMgr(getCurrentResource(), getOwlModelFormat(getProcessorContext()))
						.getGlobalPrefix(importUri);
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		if (importPrefix != null) {
			getTheJenaModel().setNsPrefix(importPrefix, importUri);
		}
		com.hp.hpl.jena.rdf.model.Resource importedOntology = getTheJenaModel().createResource(importUri);
		modelOntology.addImport(importedOntology);
		getTheJenaModel().addSubModel(importedOntModel);
		getTheJenaModel().addLoadedImport(importUri);
		addOrderedImport(importUri);
	}

	/**
	 * Method to check for erroneous use of a reserved folder name
	 * 
	 * @param resource
	 * @param model
	 * @return
	 */
	private boolean isReservedFolder(Resource resource, SadlModel model) {
		URI prjuri = ResourceManager.getProjectUri(resource);
		if (prjuri == null) {
			return false; // this is the path that JUnit tests will follow
		}
		URI rsrcuri = resource.getURI();
		String[] rsrcsegs = rsrcuri.segments();
		String[] prjsegs = prjuri.segments();
		if (rsrcsegs.length > prjsegs.length) {
			String topPrjFolder = rsrcsegs[prjsegs.length];
			for (String fnm : reservedFolderNames) {
				if (topPrjFolder.equals(fnm)) {
					if (fnm.equals(SadlConstants.SADL_IMPLICIT_MODEL_FOLDER)) {
						if (!isReservedName(resource)) {
							// only reserved names allowed here
							addError(SadlErrorMessages.RESERVED_FOLDER.get(fnm), model);
						}
						return true;
					} else {
						addError(SadlErrorMessages.RESERVED_FOLDER.get(fnm), model);
						return true;
					}
				}
			}
		}
		return false;
	}

	private boolean isReservedName(Resource resource) {
		String nm = resource.getURI().lastSegment();
		for (String rnm : reservedFileNames) {
			if (rnm.equals(nm)) {
				return true;
			}
		}
		return false;
	}

	public Test[] processStatement(TestStatement element) throws JenaProcessorException {
		Test[] generatedTests = null;
		Test sadlTest = null;
		boolean done = false;
		try {
			EList<Expression> tests = element.getTests();
			for (int tidx = 0; tidx < tests.size(); tidx++) {
				Expression expr = tests.get(tidx);
				// we know it's a Test so create one and set as translation target
				Test test = new Test();
				final ICompositeNode node = NodeModelUtils.findActualNodeFor(element);
				if (node != null) {
					test.setOffset(node.getOffset() - 1);
					test.setLength(node.getLength());
				}
				setTarget(test);

				// now translate the test expression
				Object testtrans = postProcessTranslationResult(processExpression(expr));

				// Examine testtrans, the results of the translation.
				// The recognition of various Test patterns, so that the LHS, RHS, Comparison of
				// the Test can be
				// properly set is best done on the translation before the ProxyNodes are
				// expanded--their expansion
				// destroys needed information and introduces ambiguity

				if (testtrans instanceof BuiltinElement
						&& IntermediateFormTranslator.isComparisonBuiltin(((BuiltinElement) testtrans).getFuncName())) {
					List<Node> args = ((BuiltinElement) testtrans).getArguments();
					if (args != null && args.size() == 2) {
						if (((BuiltinElement)testtrans).getFuncType().equals(BuiltinType.Equal) && 
								args.get(0) instanceof NamedNode && ((NamedNode)args.get(0)).getNodeType().equals(NodeType.InstanceNode) &&
								args.get(1) instanceof VariableNode && ((VariableNode)args.get(1)).getType() instanceof NamedNode &&
								((NamedNode)((VariableNode)args.get(1)).getType()).getNodeType().equals(NodeType.ClassNode)) {
							TripleElement testtriple = new TripleElement((NamedNode)args.get(0), new RDFTypeNode(), ((VariableNode)args.get(1)).getType());
							test.setLhs(testtriple);
							generatedTests = new Test[1];
							generatedTests[0] = test;
							done = true;
						}
						else {
							test.setCompName(((BuiltinElement) testtrans).getFuncType());
							Object lhsObj = getIfTranslator().expandProxyNodes(args.get(0), false, true);
							Object rhsObj = getIfTranslator().expandProxyNodes(args.get(1), false, true);
							test.setLhs(
									(lhsObj != null && lhsObj instanceof List<?> && ((List<?>) lhsObj).size() > 0) ? lhsObj
											: args.get(0));
							test.setRhs(
									(rhsObj != null && rhsObj instanceof List<?> && ((List<?>) rhsObj).size() > 0) ? rhsObj
											: args.get(1));
							generatedTests = new Test[1];
							generatedTests[0] = test;
							done = true;
						}
					}
				} else if (testtrans instanceof TripleElement) {
					if (((TripleElement) testtrans).getModifierType() != null
							&& !((TripleElement) testtrans).getModifierType().equals(TripleModifierType.None)) {
						// Filtered query with modification
						TripleModifierType ttype = ((TripleElement) testtrans).getModifierType();
						Object trans = getIfTranslator().expandProxyNodes(testtrans, false, true);
						if ((trans != null && trans instanceof List<?> && ((List<?>) trans).size() > 0)) {
							if (ttype.equals(TripleModifierType.Not)) {
								if (changeFilterDirection(trans)) {
									((TripleElement) testtrans).setType(TripleModifierType.None);
								}
							}
							test.setLhs(trans);
						} else {
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
					boolean treatAsMultipleTests = false;
					{
						if (testExpanded instanceof List<?>) {
							treatAsMultipleTests = containsMultipleTests((List<GraphPatternElement>) testExpanded);
						}
					}
					if (treatAsMultipleTests && testExpanded instanceof List<?>) {
						for (int i = 0; i < ((List<?>) testExpanded).size(); i++) {
							expanded.add(((List<?>) testExpanded).get(i));
						}
					} else {
						expanded.add(testExpanded);
					}

					if (expanded.size() == 0) {
						generatedTests = new Test[1];
						generatedTests[0] = test;
					} else {
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
							} else if (!done && testgpe instanceof List<?>) {
								test.setLhs(testgpe);
							}
						}
					}
				}
			}

			for (int i = 0; generatedTests != null && i < generatedTests.length; i++) {
				sadlTest = generatedTests[i];
				applyImpliedProperties(sadlTest, tests.get(0));
				getIfTranslator().postProcessTest(sadlTest, element);
				// ICompositeNode node = NodeModelUtils.findActualNodeFor(element);
				// if (node != null) {
				// test.setLineNo(node.getStartLine());
				// test.setLength(node.getLength());
				// test.setOffset(node.getOffset());
				// }

				logger.debug("Test translation: {}", sadlTest);
				List<IFTranslationError> transErrors = getIfTranslator().getErrors();
				for (int j = 0; transErrors != null && j < transErrors.size(); j++) {
					IFTranslationError err = transErrors.get(j);
					try {
						addError(err.getLocalizedMessage(), element);
					} catch (Exception e) {
						// this will happen for standalone testing where there is no Eclipse Workspace
						logger.error("Test: " + sadlTest.toString());
						logger.error("  Translation error: " + err.getLocalizedMessage()
								+ (err.getCause() != null ? (" (" + err.getCause().getLocalizedMessage() + ")") : ""));
					}
				}
				validateTest(element, sadlTest);
				addSadlCommand(sadlTest);
			}
			return generatedTests;
		} catch (InvalidNameException e) {
			addError(SadlErrorMessages.INVALID_NAME.get("test", e.getMessage()), element);
			e.printStackTrace();
		} catch (InvalidTypeException e) {
			addError(SadlErrorMessages.INVALID_PROP_TYPE.get(e.getMessage()), element);
			e.printStackTrace();
		} catch (TranslationException e) {
			addError(SadlErrorMessages.TEST_TRANSLATION_EXCEPTION.get(e.getMessage()), element);
			// e.printStackTrace();
		}
		return generatedTests;
	}

	private void applyImpliedProperties(Test sadlTest, Expression element) throws TranslationException {
		sadlTest.setLhs(applyImpliedPropertiesToSide(sadlTest.getLhs(), element));
		sadlTest.setRhs(applyImpliedPropertiesToSide(sadlTest.getRhs(), element));
	}

	private Object applyImpliedPropertiesToSide(Object side, Expression element) throws TranslationException {
		Map<EObject, List<Property>> impprops = OntModelProvider.getAllImpliedProperties(getCurrentResource());
		if (impprops != null) {
			Iterator<EObject> imppropitr = impprops.keySet().iterator();
			while (imppropitr.hasNext()) {
				EObject eobj = imppropitr.next();
				String uri = null;
				if (eobj instanceof SadlResource) {
					uri = getDeclarationExtensions().getConceptUri((SadlResource) eobj);
				} else if (eobj instanceof Name) {
					uri = getDeclarationExtensions().getConceptUri(((Name) eobj).getName());
				}
				if (uri != null) {
					if (side instanceof NamedNode) {
						if (((NamedNode) side).toFullyQualifiedString().equals(uri)) {
							List<Property> props = impprops.get(eobj);
							if (props != null && props.size() > 0) {
								if (props.size() > 1) {
									throw new TranslationException("More than 1 implied property found!");
								}
								// apply impliedProperties
								NamedNode pred = new NamedNode(props.get(0).getURI());
								if (props.get(0) instanceof DatatypeProperty) {
									pred.setNodeType(NodeType.DataTypeProperty);
								} else if (props.get(0) instanceof ObjectProperty) {
									pred.setNodeType(NodeType.ObjectProperty);
								} else {
									pred.setNodeType(NodeType.PropertyNode);
								}
								return new TripleElement((NamedNode) side, pred, new VariableNode(getNewVar(element)));
							}
						}
					}

				}
			}
		}
		return side;
	}

	public VariableNode createVariable(SadlResource sr) throws IOException, PrefixNotFoundException,
			InvalidNameException, InvalidTypeException, TranslationException, ConfigurationException {
		VariableNode var = createVariable(getDeclarationExtensions().getConceptUri(sr), sr);
		addVariableNamingEObject(var, sr);
		if (var.getType() != null) {
			return var; // all done
		}
		SadlResource decl = getDeclarationExtensions().getDeclaration(sr);
		EObject defnContainer = decl.eContainer();
		try {
			TypeCheckInfo tci = null;
			if (defnContainer instanceof BinaryOperation) {
				if (((BinaryOperation) defnContainer).getLeft().equals(decl)) {
					Expression defn = ((BinaryOperation) defnContainer).getRight();
					tci = getModelValidator().getType(defn);
				} else if (((BinaryOperation) defnContainer).getLeft() instanceof PropOfSubject) {
					tci = getModelValidator().getType(((BinaryOperation) defnContainer).getLeft());
				}
			} else if (defnContainer instanceof SadlParameterDeclaration) {
				SadlTypeReference type = ((SadlParameterDeclaration) defnContainer).getType();
				tci = getModelValidator().getType(type);
			} else if (defnContainer instanceof SubjHasProp) {
				if (((SubjHasProp) defnContainer).getLeft().equals(decl)) {
					// need domain of property
					Expression pexpr = ((SubjHasProp) defnContainer).getProp();
					if (pexpr instanceof SadlResource) {
						String puri = getDeclarationExtensions().getConceptUri((SadlResource) pexpr);
						OntConceptType ptype = getDeclarationExtensions().getOntConceptType((SadlResource) pexpr);
						if (isProperty(ptype)) {
							Property prop = getTheJenaModel().getProperty(puri);
							tci = getModelValidator().getTypeInfoFromDomain(new ConceptName(puri), prop, defnContainer);
						} else if (EcoreUtil2.getContainerOfType(defnContainer, QueryStatement.class) == null) {
							addError("Right of SubjHasProp not handled (" + ptype.toString() + ")", defnContainer);
						}
					} else {
						addError("Right of SubjHasProp not a Name (" + pexpr.getClass().toString() + ")",
								defnContainer);
					}
				} else if (((SubjHasProp) defnContainer).isComma()
						&& ((SubjHasProp) defnContainer).getRight() == null) {
					addError("This appears to be an unhandled comma-based construct", defnContainer);
				} else if (((SubjHasProp) defnContainer).getRight() != null
						&& ((SubjHasProp) defnContainer).getRight().equals(decl)) {
					// need range of property
					tci = getModelValidator().getType(((SubjHasProp) defnContainer).getProp());
				}
			} else if (sr.eContainer() instanceof Name && ((Name)sr.eContainer()).isFunction()) {
				addWarning("Variable '" + var.getName() + "' is not defined", sr);
			} else if (EcoreUtil2.getContainerOfType(sr, QueryStatement.class) == null) {
				addError("Variable '" + var.getName() + "' is not defined", sr);
			}
			if (tci != null && tci.getTypeCheckType() instanceof NamedNode) {
				setVarType(var, tci.getTypeCheckType(), tci.getRangeValueType().equals(RangeValueType.LIST), sr);
			}
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (DontTypeCheckException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (CircularDefinitionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (CircularDependencyException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (PropertyWithoutRangeException e) {
			addError("Property does not have a range", defnContainer);
		}
		return var;
	}

	private boolean isContainedBy(EObject eobj, Class cls) {
		if (eobj.eContainer() != null) {
			// TODO fix this to be generic
			if (eobj.eContainer() instanceof QueryStatement) {
				return true;
			} else {
				return isContainedBy(eobj.eContainer(), cls);
			}
		}
		return false;
	}

	protected VariableNode createVariable(String name, EObject context) throws IOException, PrefixNotFoundException,
			InvalidNameException, InvalidTypeException, TranslationException, ConfigurationException {
		Object trgt = getTarget();
		if (trgt instanceof Rule) {
			if (((Rule) trgt).getVariable(name) != null) {
				return ((Rule) trgt).getVariable(name);
			}
		} else if (trgt instanceof Query) {
			if (((Query) trgt).getVariable(name) != null) {
				return ((Query) trgt).getVariable(name);
			}
		} else if (trgt instanceof Test) {
			// TODO
		}
		VariableNode newVar = new VariableNode(name);
		if (trgt instanceof Rule) {
			((Rule) trgt).addRuleVariable(newVar);
		} else if (trgt instanceof Query) {
			((Query) trgt).addVariable(newVar);
		} else if (trgt instanceof Test) {
			// TODO
		}
		return newVar;
	}

	private boolean containsMultipleTests(List<GraphPatternElement> testtrans) {
		if (testtrans.size() == 1) {
			return false;
		}
		List<VariableNode> vars = new ArrayList<VariableNode>();
		for (int i = 0; i < testtrans.size(); i++) {
			GraphPatternElement gpe = testtrans.get(i);
			if (gpe instanceof TripleElement) {
				Node anode = ((TripleElement) gpe).getSubject();
				if (vars.contains(anode)) {
					return false; // there are vars between patterns
				} else if (anode instanceof VariableNode) {
					vars.add((VariableNode) anode);
				}
				anode = ((TripleElement) gpe).getObject();
				if (vars.contains(anode)) {
					return false; // there are vars between patterns
				} else if (anode instanceof VariableNode) {
					vars.add((VariableNode) anode);
				}
			} else if (gpe instanceof BuiltinElement) {
				List<Node> args = ((BuiltinElement) gpe).getArguments();
				for (int j = 0; args != null && j < args.size(); j++) {
					Node anode = args.get(j);
					if (anode instanceof VariableNode && vars.contains(anode)) {
						return false; // there are vars between patterns
					} else if (anode instanceof VariableNode) {
						vars.add((VariableNode) anode);
					}
				}
			}
		}
		return true;
	}

	private boolean changeFilterDirection(Object patterns) {
		if (patterns instanceof List<?>) {
			for (int i = 0; i < ((List<?>) patterns).size(); i++) {
				Object litem = ((List<?>) patterns).get(i);
				if (litem instanceof BuiltinElement) {
					IntermediateFormTranslator.builtinComparisonComplement((BuiltinElement) litem);
					return true;
				}
			}
		}
		return false;
	}

	private int validateTest(EObject object, Test test) {
		int numErrors = 0;
		Object lhs = test.getLhs();
		if (lhs instanceof GraphPatternElement) {
			numErrors += validateGraphPatternElement(object, (GraphPatternElement) lhs);
		} else if (lhs instanceof List<?>) {
			for (int i = 0; i < ((List<?>) lhs).size(); i++) {
				Object lhsinst = ((List<?>) lhs).get(i);
				if (lhsinst instanceof GraphPatternElement) {
					numErrors += validateGraphPatternElement(object, (GraphPatternElement) lhsinst);
				}
			}
		}
		Object rhs = test.getLhs();
		if (rhs instanceof GraphPatternElement) {
			numErrors += validateGraphPatternElement(object, (GraphPatternElement) rhs);
		} else if (rhs instanceof List<?>) {
			for (int i = 0; i < ((List<?>) rhs).size(); i++) {
				Object rhsinst = ((List<?>) rhs).get(i);
				if (rhsinst instanceof GraphPatternElement) {
					numErrors += validateGraphPatternElement(object, (GraphPatternElement) rhsinst);
				}
			}
		}
		return numErrors;
	}

	/**
	 * This method checks a GraphPatternElement for errors and warnings and
	 * generates the same if found.
	 * 
	 * @param gpe
	 * @return
	 */
	private int validateGraphPatternElement(EObject object, GraphPatternElement gpe) {
		int numErrors = 0;
		if (gpe instanceof TripleElement) {
			if (((TripleElement) gpe).getSubject() instanceof NamedNode
					&& ((NamedNode) ((TripleElement) gpe).getSubject()).getNodeType() != null
					&& ((NamedNode) ((TripleElement) gpe).getSubject()).getNodeType().equals(NodeType.PropertyNode)) {
				addError(SadlErrorMessages.UNEXPECTED_TRIPLE
						.get(((NamedNode) ((TripleElement) gpe).getSubject()).getName()), object);
				numErrors++;
			}
			if (((TripleElement) gpe).getObject() instanceof NamedNode
					&& ((NamedNode) ((TripleElement) gpe).getObject()).getNodeType() != null
					&& ((NamedNode) ((TripleElement) gpe).getObject()).getNodeType().equals(NodeType.PropertyNode)) {
				if (!(((TripleElement) gpe).getPredicate() instanceof NamedNode)
						|| !((NamedNode) ((TripleElement) gpe).getPredicate()).getNamespace()
								.equals(OWL.NAMESPACE.getNameSpace())) {
					addError(SadlErrorMessages.UNEXPECTED_TRIPLE
							.get(((NamedNode) ((TripleElement) gpe).getSubject()).getName()), object);
					numErrors++;
				}
			}
			if (((TripleElement) gpe).getPredicate() instanceof NamedNode
					&& ((NamedNode) ((TripleElement) gpe).getPredicate()).getNodeType() != null
					&& !(((NamedNode) ((TripleElement) gpe).getPredicate()).getNodeType().equals(NodeType.PropertyNode))
					&& !(((NamedNode) ((TripleElement) gpe).getPredicate()).getNodeType()
							.equals(NodeType.ObjectProperty))
					&& !(((NamedNode) ((TripleElement) gpe).getPredicate()).getNodeType()
							.equals(NodeType.DataTypeProperty))) {
				if (((NamedNode) ((TripleElement) gpe).getPredicate()).getNodeType().equals(NodeType.VariableNode)) {
					addWarning(SadlErrorMessages.VARIABLE_INSTEAD_OF_PROP
							.get(((NamedNode) ((TripleElement) gpe).getPredicate()).getName()), object);
				} else {
					addError(SadlErrorMessages.EXPECTED_A.get("property as triple pattern predicate rather than "
							+ ((NamedNode) ((TripleElement) gpe).getPredicate()).getNodeType().toString() + " "
							+ ((NamedNode) ((TripleElement) gpe).getPredicate()).getName()), object);
					numErrors++;
				}
			}
		} else if (gpe instanceof BuiltinElement) {
			if (((BuiltinElement) gpe).getFuncType().equals(BuiltinType.Not)) {
				List<Node> args = ((BuiltinElement) gpe).getArguments();
				if (args != null && args.size() == 1 && ITranslator.isKnownNode(args.get(0))) {
					addError(SadlErrorMessages.PHRASE_NOT_KNOWN.toString(), object);
					addError("Phrase 'not known' is not a valid graph pattern; did you mean 'is not known'?", object);
				}
			}
		}
		if (gpe.getNext() != null) {
			numErrors += validateGraphPatternElement(object, gpe.getNext());
		}
		return numErrors;
	}
	
	private void processStatement(ExplainStatement element)
			throws JenaProcessorException, InvalidNameException, InvalidTypeException, TranslationException {
		String ruleName = element.getRulename() != null ? declarationExtensions.getConcreteName(element.getRulename())
				: null;
		if (ruleName != null) {
			Explain cmd = new Explain(ruleName);
			addSadlCommand(cmd);
		} else {
			Object result = processExpression(element.getExpr());
			if (result instanceof GraphPatternElement) {
				Explain cmd = new Explain((GraphPatternElement) result);
				addSadlCommand(cmd);
			} else if (result != null) {
				throw new TranslationException("Unhandled ExplainStatement: " + result.toString());
			}
		}
	}

	private void processStatement(StartWriteStatement element) throws JenaProcessorException {
		String dataOnly = element.getDataOnly();
		StartWrite cmd = new StartWrite(dataOnly != null);
		addSadlCommand(cmd);
	}

	private void processStatement(EndWriteStatement element) throws JenaProcessorException {
		String outputFN = element.getFilename();
		EndWrite cmd = new EndWrite(outputFN);
		addSadlCommand(cmd);
	}

	private void processStatement(ReadStatement element) throws JenaProcessorException {
		String filename = element.getFilename();
		String templateFilename = element.getTemplateFilename();
		Read readCmd = new Read(filename, templateFilename);
		addSadlCommand(readCmd);
	}

	private void processStatement(PrintStatement element) throws JenaProcessorException {
		String dispStr = ((PrintStatement) element).getDisplayString();
		Print print = new Print(dispStr);
		String mdl = ((PrintStatement) element).getModel();
		if (mdl != null) {
			print.setModel(mdl);
		}
		addSadlCommand(print);
	}

	public Query processStatement(QueryStatement element) throws JenaProcessorException, InvalidNameException,
			InvalidTypeException, TranslationException, CircularDefinitionException {
		Expression qexpr = element.getExpr();
		if (qexpr != null) {
			Query theQuery = new Query();
			theQuery.setContext(qexpr);
			setTarget(theQuery);
			if (qexpr instanceof Name) {
				OntConceptType qntype = getDeclarationExtensions().getOntConceptType(((Name) qexpr).getName());
				if (qntype.equals(OntConceptType.STRUCTURE_NAME)) {
					// this is just a named query declared elsewhere
					SadlResource qdecl = getDeclarationExtensions().getDeclaration(((Name) qexpr).getName());
					EObject qdeclcont = qdecl.eContainer();
					if (qdeclcont instanceof QueryStatement) {
						qexpr = ((QueryStatement) qdeclcont).getExpr();
					} else {
						addError("Unexpected named structure name whose definition is not a query statement", qexpr);
						return null;
					}
				}
			}
			Object qobj = processExpression(qexpr);
			Query query = null;
			if (qobj instanceof Query) {
				query = (Query) qobj;
			} else if (qobj == null) {
				// maybe this is a query by name?
				if (qexpr instanceof Name) {
					SadlResource qnm = ((Name) qexpr).getName();
					String qnmuri = getDeclarationExtensions().getConceptUri(qnm);
					if (qnmuri != null) {
						Individual qinst = getTheJenaModel().getIndividual(qnmuri);
						if (qinst != null) {
							StmtIterator stmtitr = qinst.listProperties();
							while (stmtitr.hasNext()) {
								System.out.println(stmtitr.nextStatement().toString());
							}
						}
					}
				}
			} else {
				query = processQuery(postProcessTranslationResult(qobj));
			}
			if (query != null) {
				if (element.getName() != null) {
					String uri = declarationExtensions.getConceptUri(element.getName());
					query.setFqName(uri);
					OntClass nqcls = getTheJenaModel()
							.getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_NAMEDQUERY_CLASS_URI);
					if (nqcls != null) {
						Individual nqry = getTheJenaModel().createIndividual(uri, nqcls);
						// Add annotations, if any
						EList<NamedStructureAnnotation> annotations = element.getAnnotations();
						if (annotations != null && annotations.size() > 0) {
							addNamedStructureAnnotations(nqry, annotations);
						}
					}
				}
				if (element.getStart().equals("Graph")) {
					query.setGraph(true);
				}
				final ICompositeNode node = NodeModelUtils.findActualNodeFor(element);
				if (node != null) {
					query.setOffset(node.getOffset() - 1);
					query.setLength(node.getLength());
				}
				query = addExpandedPropertiesToQuery(query, qexpr);
				addSadlCommand(query);
				return query;
			}
		} else {
			// this is a reference to a named query defined elsewhere
			SadlResource sr = element.getName();
			SadlResource sr2 = declarationExtensions.getDeclaration(sr);
			if (sr2 != null) {
				EObject cont = sr2.eContainer();
				if (cont instanceof QueryStatement && ((QueryStatement) cont).getExpr() != null) {
					return processStatement((QueryStatement) cont);
				}
			}
		}
		return null;
	}

	private Query addExpandedPropertiesToQuery(Query query, Expression expr) {
		List<VariableNode> vars = query.getVariables();
		List<GraphPatternElement> elements = query.getPatterns();
		if (elements != null) {
			List<TripleElement> triplesToAdd = null;
			for (GraphPatternElement e : elements) {
				if (e instanceof TripleElement) {
					Node subj = ((TripleElement) e).getSubject();
					Node obj = ((TripleElement) e).getObject();
					boolean implicitObject = false;
					if (obj == null) {
						obj = new VariableNode(getIfTranslator().getNewVar());
						((TripleElement) e).setObject(obj);
						implicitObject = true;
					}
					if (implicitObject || obj instanceof VariableNode) {
						VariableNode vn = (VariableNode) ((TripleElement) e).getObject();
						// if (vars != null && vars.contains(vn.getName())) {
						Node pred = ((TripleElement) e).getPredicate();
						ConceptName predcn = new ConceptName(pred.toFullyQualifiedString());
						Property predProp = getTheJenaModel().getProperty(pred.toFullyQualifiedString());
						setPropertyConceptNameType(predcn, predProp);
						try {
							TypeCheckInfo tci = getModelValidator().getTypeInfoFromRange(predcn, predProp, null);
							if (tci != null) {
								Node tct = tci.getTypeCheckType();
								if (tct instanceof NamedNode) {
									OntClass rngcls = getTheJenaModel()
											.getOntClass(((NamedNode) tct).toFullyQualifiedString());
									if (rngcls != null) {
										List<String> expandedProps = getExpandedProperties(rngcls);
										if (expandedProps != null) {
											for (int i = 0; i < expandedProps.size(); i++) {
												String epstr = expandedProps.get(i);
												if (!subjPredMatch(elements, vn, epstr)) {
													NamedNode propnode = validateNamedNode(
															new NamedNode(epstr, NodeType.ObjectProperty));
													String vnameprefix = (subj instanceof NamedNode)
															? ((NamedNode) subj).getName()
															: "x";
													if (pred instanceof NamedNode) {
														vnameprefix += "_" + ((NamedNode) pred).getName();
													}
													VariableNode newvar = new VariableNode(
															vnameprefix + "_" + propnode.getName()); // getIfTranslator().getNewVar());
													TripleElement newtriple = new TripleElement(vn, propnode, newvar);
													if (vars == null) {
														vars = new ArrayList<VariableNode>();
														query.setVariables(vars);
													}
													vars.add(newvar);
													if (triplesToAdd == null)
														triplesToAdd = new ArrayList<TripleElement>();
													triplesToAdd.add(newtriple);
												}
											}
										}
									}
								}
							}
						} catch (DontTypeCheckException e1) {
							// TODO Auto-generated catch block
							e1.printStackTrace();
						} catch (InvalidTypeException e1) {
							// TODO Auto-generated catch block
							e1.printStackTrace();
						}
						// }
						catch (TranslationException e2) {
							// TODO Auto-generated catch block
							e2.printStackTrace();
						} catch (InvalidNameException e1) {
							// TODO Auto-generated catch block
							e1.printStackTrace();
						}
					}
					if (triplesToAdd == null && implicitObject) {
						query.addVariable((VariableNode) obj);
					}
				}
			}
			if (triplesToAdd != null) {
				for (int i = 0; i < triplesToAdd.size(); i++) {
					query.addPattern(triplesToAdd.get(i));
				}
			}
		}
		return query;
	}

	public void setPropertyConceptNameType(ConceptName predcn, Property predProp) {
		if (predProp instanceof ObjectProperty) {
			predcn.setType(ConceptType.OBJECTPROPERTY);
		} else if (predProp instanceof DatatypeProperty) {
			predcn.setType(ConceptType.DATATYPEPROPERTY);
		} else if (predProp instanceof AnnotationProperty) {
			predcn.setType(ConceptType.ANNOTATIONPROPERTY);
		} else {
			predcn.setType(ConceptType.RDFPROPERTY);
		}
	}

	private boolean subjPredMatch(List<GraphPatternElement> elements, VariableNode vn, String epstr) {
		for (int i = 0; elements != null && i < elements.size(); i++) {
			GraphPatternElement gp = elements.get(i);
			if (gp instanceof TripleElement) {
				TripleElement tr = (TripleElement) gp;
				if (tr.getSubject().equals(vn) && tr.getPredicate().toFullyQualifiedString().equals(epstr)) {
					return true;
				}
			}
		}
		return false;
	}

	public Query processExpression(SelectExpression expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		return processAsk(expr);
	}

	public Query processExpression(ConstructExpression expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		return processAsk(expr);
	}

	public Query processExpression(AskExpression expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		return processAsk(expr);
	}

	private Query processAsk(Expression expr) {
		Query query = (Query) getTarget();
		// if (parent != null) {
		// getIfTranslator().setEncapsulatingTarget(parent);
		// }

		// get variables and other information from the SelectExpression
		EList<SadlResource> varList = null; // the specified list of variables to be returned
		List<VariableNode> names = null; // the actual names generated from the varList
		Expression whexpr = null;
		if (expr instanceof SelectExpression) {
			whexpr = ((SelectExpression) expr).getWhereExpression();
			query.setKeyword("select");
			if (((SelectExpression) expr).isDistinct()) {
				query.setDistinct(true);
			}
			varList = ((SelectExpression) expr).getSelectFrom();
			if (varList != null) {
				names = new ArrayList<VariableNode>();
				for (int i = 0; i < varList.size(); i++) {
					Object var = null;
					try {
						var = processExpression(varList.get(i));
					} catch (TranslationException e2) {
						// TODO Auto-generated catch block
						e2.printStackTrace();
					}
					TypeCheckInfo tci = null;
					try {
						tci = modelValidator.getType(varList.get(i));
					} catch (DontTypeCheckException e1) {
						// OK to not type check
					} catch (CircularDefinitionException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					} catch (URISyntaxException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					} catch (IOException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					} catch (ConfigurationException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					} catch (CircularDependencyException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (PropertyWithoutRangeException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (InvalidNameException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (TranslationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (InvalidTypeException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					if (!(var instanceof VariableNode)) {
						try {
							OntConceptType vtype = getDeclarationExtensions().getOntConceptType(varList.get(i));
							if (vtype.equals(OntConceptType.VARIABLE)) {
								int k = 0;
							}
						} catch (CircularDefinitionException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						// throw new InvalidNameException("'" + var.toString() + "' isn't a variable as
						// expected in query select names.");
						if (var != null) {
							addError(SadlErrorMessages.QUERY_ISNT_VARIABLE.get(var.toString()), expr);
						}
					} else {
						names.add(((VariableNode) var));
					}
				}
			}
			if (((SelectExpression) expr).getOrderby() != null) {
				EList<OrderElement> ol = ((SelectExpression) expr).getOrderList();
				List<OrderingPair> orderingPairs = new ArrayList<OrderingPair>();
				for (int i = 0; i < ol.size(); i++) {
					OrderElement oele = ol.get(i);
					SadlResource ord = oele.getOrderBy();
					orderingPairs.add(query.new OrderingPair(getDeclarationExtensions().getConcreteName(ord),
							(oele.isDesc() ? Order.DESC : Order.ASC)));
				}
				query.setOrderBy(orderingPairs);
			}
		} else if (expr instanceof ConstructExpression) {
			whexpr = ((ConstructExpression) expr).getWhereExpression();
			query.setKeyword("construct");
			names = new ArrayList<VariableNode>();
			try {
				Object result = processExpression(((ConstructExpression) expr).getSubj());
				if (result instanceof VariableNode) {
					names.add(((VariableNode) result));
				} else {
					names.add(createVariable(result.toString(), expr));
				}
				result = processExpression(((ConstructExpression) expr).getPred());
				if (result instanceof VariableNode) {
					names.add((VariableNode) result);
				} else {
					names.add(createVariable(result.toString(), expr));
				}
				result = processExpression(((ConstructExpression) expr).getObj());
				if (result instanceof VariableNode) {
					names.add(((VariableNode) result));
				} else {
					names.add(createVariable(result.toString(), expr));
				}
				if (names.size() != 3) {
					addWarning("A 'construct' statement should have 3 variables so as to be able to generate a graph.",
							expr);
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
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (PrefixNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else if (expr instanceof AskExpression) {
			whexpr = ((AskExpression) expr).getWhereExpression();
			query.setKeyword("ask");
		}

		// Translate the query to the resulting intermediate form.
		if (modelValidator != null) {
			try {
				TypeCheckInfo tct = modelValidator.getType(whexpr);
				if (tct != null && tct.getImplicitProperties() != null) {
					List<ConceptName> ips = tct.getImplicitProperties();
					int i = 0;
				}
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (DontTypeCheckException e) {
				// OK to not be able to type check
			} catch (CircularDefinitionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (CircularDependencyException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (PropertyWithoutRangeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvalidNameException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (TranslationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvalidTypeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		Object pattern = null;
		try {
			pattern = postProcessTranslationResult(processExpression(whexpr));
		} catch (InvalidNameException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (InvalidTypeException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (TranslationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		if (names != null && names.size() > 0) {
			query.setVariables(names); // this will replace implicitly added variables if there are explicit ones
		}

		Object expandedPattern = null;
		try {
			expandedPattern = getIfTranslator().expandProxyNodes(pattern, false, true);
		} catch (InvalidNameException e) {
			addError(SadlErrorMessages.INVALID_NAME.get("query", pattern.toString()), expr);
			e.printStackTrace();
		} catch (InvalidTypeException e) {
			addError(SadlErrorMessages.INVALID_TYPE.get("query", pattern.toString()), expr);
			e.printStackTrace();
		} catch (TranslationException e) {
			addError(SadlErrorMessages.TRANSLATION_ERROR.get("query", pattern.toString()), expr);
			e.printStackTrace();
		}
		if (expandedPattern != null && expandedPattern instanceof List<?> && ((List<?>) expandedPattern).size() > 0) {
			pattern = expandedPattern;
		}

		if (pattern instanceof List<?>) {
			if (query.getVariables() == null) {
				Set<VariableNode> nodes = getIfTranslator().getSelectVariables((List<GraphPatternElement>) pattern);
				if (nodes != null && nodes.size() > 0) {
					names = new ArrayList<VariableNode>(1);
					for (VariableNode node : nodes) {
						names.add(node);
					}
					query.setVariables(names);
					if (query.getKeyword() == null) {
						query.setKeyword("select");
					}
				} else {
					// no variables, assume an ask
					if (query.getKeyword() == null) {
						query.setKeyword("ask");
					}
				}
			}
			query.setPatterns((List<GraphPatternElement>) pattern);
		} else if (pattern instanceof Literal) {
			// this must be a SPARQL query
			query.setSparqlQueryString(((Literal) pattern).getValue().toString());
		}
		logger.debug("Ask translation: {}", query);
		return query;
	}

	private Query processQuery(Object qobj) throws JenaProcessorException {
		String qstr = null;
		Query q = (Query) getTarget();
		if (q == null) {
			q = new Query();
			setTarget(q);
		}
		if (qobj instanceof com.ge.research.sadl.model.gp.Literal) {
			qstr = ((com.ge.research.sadl.model.gp.Literal) qobj).getValue().toString();
			q.setSparqlQueryString(qstr);
		} else if (qobj instanceof String) {
			qstr = qobj.toString();
			q.setSparqlQueryString(qstr);
		} else if (qobj instanceof NamedNode) {
			if (isProperty(((NamedNode) qobj).getNodeType())) {
				VariableNode sn = new VariableNode(getIfTranslator().getNewVar());
				TripleElement tr = new TripleElement(sn, (Node) qobj, null);
				q.addPattern(tr);
				List<VariableNode> vars = q.getVariables();
				if (vars == null) {
					vars = new ArrayList<VariableNode>();
					q.setVariables(vars);
				}
				q.getVariables().add(sn);
			}
		} else if (qobj instanceof TripleElement) {
			Set<VariableNode> vars = getIfTranslator().getSelectVariables((GraphPatternElement) qobj);
			List<IFTranslationError> errs = getIfTranslator().getErrors();
			if (errs == null || errs.size() == 0) {
				if (vars != null && vars.size() > 0) {
					List<VariableNode> varNames = new ArrayList<VariableNode>();
					Iterator<VariableNode> vitr = vars.iterator();
					while (vitr.hasNext()) {
						varNames.add(vitr.next());
					}
					q.setVariables(varNames);
				}
				q.addPattern((GraphPatternElement) qobj);
			}
		} else if (qobj instanceof BuiltinElement) {
			String fn = ((BuiltinElement) qobj).getFuncName();
			List<Node> args = ((BuiltinElement) qobj).getArguments();
			int i = 0;
		} else if (qobj instanceof Junction) {
			q.addPattern((Junction) qobj);
			q.setKeyword("select");
		} else {
			throw new JenaProcessorException("Unexpected query type: " + qobj.getClass().getCanonicalName());
		}
		setTarget(null);
		return q;
	}

	public void processStatement(EquationStatement element)
			throws JenaProcessorException, InvalidNameException, InvalidTypeException, TranslationException {
		SadlResource nm = element.getName();
		EList<SadlParameterDeclaration> params = element.getParameter();
		SadlTypeReference rtype = element.getReturnType();
		Expression bdy = element.getBody();
		Equation eq = createEquation(element, nm, rtype, params, bdy);
		addEquation(element.eResource(), eq, nm);
		Individual eqinst = getTheJenaModel().createIndividual(getDeclarationExtensions().getConceptUri(nm),
				getTheJenaModel().getOntClass(SadlConstants.SADL_BASE_MODEL_EQUATION_URI));
		DatatypeProperty dtp = getTheJenaModel().getDatatypeProperty(SadlConstants.SADL_BASE_MODEL_EQ_EXPRESSION_URI);
		Literal literal = getTheJenaModel().createTypedLiteral(eq.toString());
		if (eqinst != null && dtp != null) {
			// these can be null during clean/build with resource open in editor
			eqinst.addProperty(dtp, literal);
		}
	}

	protected Equation createEquation(EquationStatement element, SadlResource nm, SadlTypeReference rtype,
			EList<SadlParameterDeclaration> params, Expression bdy)
			throws JenaProcessorException, TranslationException, InvalidNameException, InvalidTypeException {
		Equation eq = new Equation(getDeclarationExtensions().getConcreteName(nm));
		eq.setNamespace(getDeclarationExtensions().getConceptNamespace(nm));
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
		// put equation in context for sub-processing
		setCurrentEquation(eq);
		Object bdyobj = processExpression(bdy);
		if (bdyobj instanceof List<?>) {
			eq.setBody((List<GraphPatternElement>) bdyobj);
		} else if (bdyobj instanceof GraphPatternElement) {
			eq.addBodyElement((GraphPatternElement) bdyobj);
		}
		if (getModelValidator() != null) {
			// check return type against body expression
			StringBuilder errorMessageBuilder = new StringBuilder();
			if (!getModelValidator().validateBinaryOperationByParts(element, rtype, bdy, "function return",
					errorMessageBuilder, false)) {
				addIssueToAcceptor(errorMessageBuilder.toString(), bdy);
			}
		}
		setCurrentEquation(null); // clear
		logger.debug("Equation: " + eq.toFullyQualifiedString());
		return eq;
	}

	public void addIssueToAcceptor(String message, EObject expr) {
		if (isTypeCheckingWarningsOnly()) {
			issueAcceptor.addWarning(message, expr);
		} else {
			issueAcceptor.addError(message, expr);
		}
	}

	public void addIssueToAcceptor(String message, Severity severity, EObject expr) {
		if (severity.equals(Severity.WARNING)) {
			issueAcceptor.addWarning(message, expr);
		} else if (severity.equals(Severity.INFO)) {
			issueAcceptor.addInfo(message, expr);
		} else if (severity.equals(Severity.ERROR)) {
			issueAcceptor.addError(message, expr);
		}
		// otherwise must be IGNORE
	}

	protected void processStatement(ExternalEquationStatement element)
			throws JenaProcessorException, InvalidNameException, InvalidTypeException, TranslationException {
		String uri = element.getUri();
		// if(uri.equals(SadlConstants.SADL_BUILTIN_FUNCTIONS_URI)){
		// return;
		// }
		SadlResource nm = element.getName();
		EList<SadlParameterDeclaration> params = element.getParameter();
		SadlTypeReference rtype = element.getReturnType();
		String location = element.getLocation();
		Equation eq = createExternalEquation(nm, uri, rtype, params, location);
		addEquation(element.eResource(), eq, nm);
		Individual eqinst = getTheJenaModel().createIndividual(getDeclarationExtensions().getConceptUri(nm),
				getTheJenaModel().getOntClass(SadlConstants.SADL_BASE_MODEL_EXTERNAL_URI));
		DatatypeProperty dtp = getTheJenaModel().getDatatypeProperty(SadlConstants.SADL_BASE_MODEL_EXTERNALURI_URI);
		Literal literal = uri != null ? getTheJenaModel().createTypedLiteral(uri) : null;
		if (eqinst != null && dtp != null && literal != null) {
			// these can be null if a resource is open in the editor and a clean/build is
			// performed
			eqinst.addProperty(dtp, literal);
			if (location != null && location.length() > 0) {
				DatatypeProperty dtp2 = getTheJenaModel()
						.getDatatypeProperty(SadlConstants.SADL_BASE_MODEL_EXTERNALURI_LOCATIOIN);
				Literal literal2 = getTheJenaModel().createTypedLiteral(location);
				eqinst.addProperty(dtp2, literal2);
			}
		}
	}

	protected Equation createExternalEquation(SadlResource nm, String uri, SadlTypeReference rtype,
			EList<SadlParameterDeclaration> params, String location)
			throws JenaProcessorException, TranslationException, InvalidNameException {
		Equation eq = new Equation(getDeclarationExtensions().getConcreteName(nm));
		eq.setNamespace(getDeclarationExtensions().getConceptNamespace(nm));
		eq.setExternal(true);
		eq.setUri(uri);
		if (location != null) {
			eq.setLocation(location);
		}
		if (rtype != null) {
			Node rtnode = sadlTypeReferenceToNode(rtype);
			eq.setReturnType(rtnode);
		}
		if (params != null && params.size() > 0) {
			if (params.get(0).getUnknown() == null) {
				List<Node> args = new ArrayList<Node>();
				List<Node> argtypes = new ArrayList<Node>();
				for (int i = 0; i < params.size(); i++) {
					SadlParameterDeclaration param = params.get(i);
					SadlResource pr = param.getName();
					if (pr != null) {
						Object pn = processExpression(pr);
						args.add((Node) pn);
						SadlTypeReference prtype = param.getType();
						Node prtnode = sadlTypeReferenceToNode(prtype);
						argtypes.add(prtnode);
					}
				}
				eq.setArguments(args);
				eq.setArgumentTypes(argtypes);
			}
		}

		logger.debug("External Equation: " + eq.toFullyQualifiedString());
		return eq;
	}

	private NamedNode sadlTypeReferenceToNode(SadlTypeReference rtype)
			throws JenaProcessorException, InvalidNameException, TranslationException {
		ConceptName cn = sadlSimpleTypeReferenceToConceptName(rtype);
		if (cn == null) {
			return null;
		}
		return conceptNameToNamedNode(cn);
		// com.hp.hpl.jena.rdf.model.Resource rtobj =
		// sadlTypeReferenceToResource(rtype);
		// if (rtobj == null) {
		//// throw new JenaProcessorException("SadlTypeReference was not resolved to a
		// model resource.");
		// return null;
		// }
		// if (rtobj.isURIResource()) {
		// NamedNode rtnn = new
		// NamedNode(((com.hp.hpl.jena.rdf.model.Resource)rtobj).getLocalName());
		// rtnn.setNamespace(((com.hp.hpl.jena.rdf.model.Resource)rtobj).getNameSpace());
		// return rtnn;
		// }
	}

	public NamedNode conceptNameToNamedNode(ConceptName cn) throws TranslationException, InvalidNameException {
		NamedNode rtnn = new NamedNode(cn.getUri());
		rtnn.setNodeType(conceptTypeToNodeType(cn.getType()));
		return rtnn;
	}

	public ConceptName namedNodeToConceptName(NamedNode nn) throws TranslationException, InvalidNameException {
		if (nn == null)
			return null;
		ConceptName cn = new ConceptName(nn.toFullyQualifiedString());
		cn.setType(nodeTypeToConceptType(nn.getNodeType()));
		return cn;
	}

	protected void addEquation(Resource resource, Equation eq, EObject nm) {
		if (getEquations() == null) {
			setEquations(new ArrayList<Equation>());
			OntModelProvider.addOtherContent(resource, getEquations());
		}
		String newEqName = eq.getName();
		List<Equation> eqlist = getEquations();
		for (int i = 0; i < eqlist.size(); i++) {
			if (eqlist.get(i).getName().equals(newEqName)) {
				if (!namespaceIsImported(eq.getNamespace(), resource)) {
					getIssueAcceptor()
							.addError("Name '" + newEqName + "' is already used. Please provide a unique name.", nm);
				} else {
					return;
				}
			}
		}
		getEquations().add(eq);
	}

	private boolean namespaceIsImported(String namespace, Resource resource) {
		String currentNamespace = namespace.replace("#", "");
		if (currentNamespace.equals(SadlConstants.SADL_BUILTIN_FUNCTIONS_URI)
				|| currentNamespace.equals(SadlConstants.SADL_IMPLICIT_MODEL_URI)) {
			return true;
		}

		TreeIterator<EObject> it = resource.getAllContents();
		while (it.hasNext()) {
			EObject eObj = it.next();
			if (eObj instanceof SadlImport) {
				SadlModel sadlModel = ((SadlImport) eObj).getImportedResource();
				if (sadlModel.getBaseUri().equals(currentNamespace)) {
					return true;
				} else if (namespaceIsImported(namespace, sadlModel.eResource())) {
					return true;
				}
			}
		}

		return false;
	}

	private boolean namespaceIsImported(String namespace, OntModel currentModel) {
		OntModel importedModel = currentModel.getImportedModel(namespace.replace("#", ""));
		if (importedModel != null) {
			return true;
		}
		return false;
	}

	public List<Equation> getEquations(Resource resource) {
		List<Object> other = OntModelProvider.getOtherContent(resource);
		return equations;
	}

	private void processStatement(RuleStatement element)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		clearCruleVariables();
		String ruleName = getDeclarationExtensions().getConcreteName(element.getName());
		Rule rule = new Rule(ruleName);
		setTarget(rule);
		EList<Expression> ifs = element.getIfs();
		EList<Expression> thens = element.getThens();
		setRulePart(RulePart.PREMISE);
		for (int i = 0; ifs != null && i < ifs.size(); i++) {
			Expression expr = ifs.get(i);
			Object result = postProcessTranslationResult(processExpression(expr));
			if (result instanceof GraphPatternElement) {
				rule.addIf((GraphPatternElement) result);
			} else {
				addError(SadlErrorMessages.IS_NOT_A.get("If Expression (" + result + ")", "GraphPatternElement"), expr);
			}
		}
		setRulePart(RulePart.CONCLUSION);
		for (int i = 0; thens != null && i < thens.size(); i++) {
			Expression expr = thens.get(i);
			Object result = postProcessTranslationResult(processExpression(expr));
			if (result instanceof GraphPatternElement) {
				rule.addThen((GraphPatternElement) result);
			} else {
				addError(SadlErrorMessages.IS_NOT_A.get("Then Expression (" + result + ")", "GraphPatternElement"),
						expr);
			}
		}
		getIfTranslator().setTarget(rule);
		rule = getIfTranslator().postProcessRule(rule, element);
		if (rules == null) {
			rules = new ArrayList<Rule>();
		}
		rules.add(rule);
		String uri = declarationExtensions.getConceptUri(element.getName());
		OntClass rcls = getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_RULE_CLASS_URI);
		if (rcls != null) {
			Individual rl = getTheJenaModel().createIndividual(uri, rcls);
			// Add annotations, if any
			EList<NamedStructureAnnotation> annotations = element.getAnnotations();
			if (annotations != null && annotations.size() > 0) {
				addNamedStructureAnnotations(rl, annotations);
			}
		}
		setTarget(null);
	}

	protected void addSadlCommand(SadlCommand sadlCommand) {
		if (getSadlCommands() == null) {
			setSadlCommands(new ArrayList<SadlCommand>());
		}
		getSadlCommands().add(sadlCommand);
	}

	/**
	 * Get the SadlCommands generated by the processor. Used for testing purposes.
	 * 
	 * @return
	 */
	public List<SadlCommand> getSadlCommands() {
		return sadlCommands;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.ge.research.sadl.jena.IJenaBasedModelProcessor#getIntermediateFormResults
	 * (boolean, boolean)
	 */
	@Override
	public List<Object> getIntermediateFormResults() throws InvalidNameException,
			InvalidTypeException, TranslationException, IOException, PrefixNotFoundException, ConfigurationException {
		return intermediateFormResults;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ge.research.sadl.jena.IJenaBasedModelProcessor#
	 * expandNodesInIntermediateForm(java.lang.Object, boolean)
	 */
	@Override
	public GraphPatternElement expandNodesInIntermediateForm(Object rawIntermediateForm, boolean treatAsConclusion)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		getIfTranslator().resetIFTranslator();
		Object expansion = getIfTranslator().expandProxyNodes(rawIntermediateForm, treatAsConclusion, true);
		if (expansion instanceof GraphPatternElement) {
			return (GraphPatternElement) expansion;
		} else
			throw new TranslationException(
					"Expansion failed to return a GraphPatternElement (returned '" + expansion.toString() + "')");
	}

	protected void addIntermediateFormResult(Object result) {
		if (intermediateFormResults == null) {
			intermediateFormResults = new ArrayList<Object>();
		}
		intermediateFormResults.add(result);
	}

	@Override
	public IntermediateFormTranslator getIfTranslator() {
		if (intermediateFormTranslator == null) {
			intermediateFormTranslator = new IntermediateFormTranslator(this, getTheJenaModel());
		}
		return intermediateFormTranslator;
	}

	// @Override
	// public Object processExpression(Expression expr) throws InvalidNameException,
	// InvalidTypeException, TranslationException {
	// return processExpression(expr);
	// }
	//
	public Object processExpression(final EObject expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		if (expr instanceof BinaryOperation) {
			return processExpression((BinaryOperation) expr);
		} else if (expr instanceof BooleanLiteral) {
			return processExpression((BooleanLiteral) expr);
		} else if (expr instanceof Constant) {
			return processExpression((Constant) expr);
		} else if (expr instanceof Declaration) {
			return processExpression((Declaration) expr);
		} else if (expr instanceof Name) {
			return processExpression((Name) expr);
		} else if (expr instanceof NumberLiteral) {
			return processExpression((NumberLiteral) expr);
		} else if (expr instanceof PropOfSubject) {
			return processExpression((PropOfSubject) expr);
		} else if (expr instanceof StringLiteral) {
			return processExpression((StringLiteral) expr);
		} else if (expr instanceof SubjHasProp) {
			if (SadlASTUtils.isUnitExpression(expr)) {
				return processSubjHasPropUnitExpression((SubjHasProp) expr);
			}
			return processExpression((SubjHasProp) expr);
		} else if (expr instanceof SadlResource) {
			return processExpression((SadlResource) expr);
		} else if (expr instanceof UnaryExpression) {
			return processExpression((UnaryExpression) expr);
		} else if (expr instanceof Sublist) {
			return processExpression((Sublist) expr);
		} else if (expr instanceof ValueTable) {
			return processExpression((ValueTable) expr);
		} else if (expr instanceof SelectExpression) {
			return processExpression((SelectExpression) expr);
		} else if (expr instanceof AskExpression) {
			return processExpression((AskExpression) expr);
		} else if (expr instanceof ConstructExpression) {
			return processExpression((ConstructExpression) expr);
		} else if (expr instanceof UnitExpression) {
			return processExpression((UnitExpression) expr);
		} else if (expr instanceof ElementInList) {
			return processExpression((ElementInList) expr);
		} else if (expr != null) {
			throw new TranslationException("Unhandled rule expression type: " + expr.getClass().getCanonicalName());
		}
		return expr;
	}

	public Object processExpression(ValueTable expr) {
		ValueRow row = ((ValueTable) expr).getRow();
		if (row == null) {
			EList<ValueRow> rows = ((ValueTable) expr).getRows();
			if (rows == null || rows.size() == 0) {
				ValueTable vtbl = ((ValueTable) expr).getValueTable();
				return processExpression(vtbl);
			}
			return null;
		}
		return null;
	}

	public Object processExpression(BinaryOperation expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		// is this a variable definition?
		boolean isLeftVariableDefinition = false;
		Name leftVariableName = null;
		Expression leftVariableDefn = null;

		// math operations can be a variable on each side of operand
		boolean isRightVariableDefinition = false;
		Name rightVariableName = null;
		Expression rightVariableDefn = null;
		try {
			if (expr.getLeft() instanceof Name && isVariableDefinition((Name) expr.getLeft())) {
				// left is variable name, right is variable definition
				isLeftVariableDefinition = true;
				leftVariableName = (Name) expr.getLeft();
				leftVariableDefn = expr.getRight();
			}
			if (expr.getRight() instanceof Name && isVariableDefinition((Name) expr.getRight())) {
				// right is variable name, left is variable definition
				isRightVariableDefinition = true;
				rightVariableName = (Name) expr.getRight();
				rightVariableDefn = expr.getLeft();
				
				if (EcoreUtil2.getContainerOfType(expr, QueryStatement.class) == null
						&& EcoreUtil2.getContainerOfType(expr, RuleStatement.class) == null) {

					addError("Variable definition on right is not supported at this time", expr);
				}

			}
		} catch (CircularDefinitionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		if (isLeftVariableDefinition) {
			try {
				VariableNode leftVar = createVariable(
						getDeclarationExtensions().getConceptUri(leftVariableName.getName()), expr);
				if (leftVar == null) { // this can happen on clean/build when file is open in editor
					return null;
				}
				addVariableNamingEObject(leftVar, leftVariableName.getName());
				Declaration varkey = getLeftDeclaration(leftVariableDefn);
				if (varkey != null) {
					setVariableInDefinition(varkey, leftVar);
				}
				Object rest = null;
				Object leftTranslatedDefn = processExpression(leftVariableDefn);
				if (varkey != null) {
					setVariableInDefinition(varkey, null);
				}
				if (leftTranslatedDefn instanceof Object[] && ((Object[]) leftTranslatedDefn).length == 2) {
					rest = ((Object[]) leftTranslatedDefn)[1];
					if (((Object[]) leftTranslatedDefn)[0].equals(leftVar)) {
						Node vtype = leftVar.getType();
						if (vtype instanceof NamedNode) {
							addVariableDefinition(leftVar, rest, (NamedNode) vtype, expr);
						}
						return rest;
					}
					leftTranslatedDefn = ((Object[]) leftTranslatedDefn)[0];
				}
				NamedNode leftDefnType = null;
				if (leftTranslatedDefn instanceof NamedNode) {
					if (leftTranslatedDefn instanceof VariableNode) {
						leftDefnType = (NamedNode) ((VariableNode) leftTranslatedDefn).getType();
					} else {
						leftDefnType = (NamedNode) leftTranslatedDefn;
					}
					setVarType(leftVar, leftDefnType, (Boolean) null, leftVariableDefn);
					if (isRightVariableDefinition) {
						Object rightTranslatedDefn = processExpression(rightVariableDefn);
						NamedNode rightDefnType = null;
						VariableNode rightVar = createVariable(
								getDeclarationExtensions().getConceptUri(rightVariableName.getName()), expr);
						if (rightVar == null) {
							return null;
						}
						addVariableNamingEObject(rightVar, rightVariableName.getName());
						if (rightTranslatedDefn instanceof NamedNode) {
							rightDefnType = (NamedNode) rightTranslatedDefn;
							setVarType(rightVar, rightDefnType, (Boolean) null, rightVariableDefn);
						}
						return combineRest(createBinaryBuiltin(expr.getOp(), leftVar, rightVar), rest);
					}
					// TODO shouldn't generate triple for type as variable contains all type info.
					// should just return rest?
					// problem is that rest can be null and to just return the variable doesn't work
					// because callers expect GraphPatternElements returned
					// normally the variable will be in a TripleElement and/or a BuiltinElement and
					// could be dropped entirely
					// but for now returning this extra type although for a list range will be
					// incorrect? awc 12/6/2017
					TripleElement trel = new TripleElement(leftVar, new RDFTypeNode(), leftDefnType);
					trel.setSourceType(TripleSourceType.SPV);
					return combineRest(trel, rest);
				} else if (expr.getRight().equals(leftVariableName) && expr.getLeft() instanceof PropOfSubject
						&& leftTranslatedDefn instanceof TripleElement
						&& ((TripleElement) leftTranslatedDefn).getObject() == null) {
					// this is just like a SubjHasProp only the order is reversed
					((TripleElement) leftTranslatedDefn).setObject(leftVar);
					return leftTranslatedDefn;
				} else {
					TypeCheckInfo varType = getModelValidator().getType(leftVariableDefn);
					if (varType != null) {
						if (leftVar.getType() == null) {
							if (varType.getCompoundTypes() != null) {
								Object jct = compoundTypeCheckTypeToNode(varType, leftVariableDefn);
								if (jct != null && jct instanceof Junction) {
									setVarType(leftVar, nodeCheck(jct), varType.isList(), leftVariableDefn);
								} else {
									addError(
											"Compound type check did not process into expected result for variable type",
											leftVariableDefn);
								}
							} else if (varType.getTypeCheckType() != null
									&& varType.getTypeCheckType() instanceof NamedNode) {
								leftDefnType = (NamedNode) varType.getTypeCheckType();
								setVarType(leftVar, leftDefnType, varType.isList(), leftVariableDefn);
							}
						}
						if (varType.getTypeCheckType() != null && varType.getTypeCheckType() instanceof NamedNode) {
							leftDefnType = (NamedNode) varType.getTypeCheckType();
						}
						if (leftTranslatedDefn instanceof GraphPatternElement) {
							leftVar.addDefinition((GraphPatternElement) leftTranslatedDefn);
						}
					} else if (leftTranslatedDefn instanceof GraphPatternElement) {
						leftVar.addDefinition((GraphPatternElement) leftTranslatedDefn);
					} else if (leftTranslatedDefn instanceof List<?>) {
						leftVar.setDefinition((List<GraphPatternElement>) leftTranslatedDefn);
					}
					if (leftTranslatedDefn instanceof TripleElement
							&& ((TripleElement) leftTranslatedDefn).getObject() == null) {
						// this is a variable definition and the definition is a triple and the triple
						// has no object
						((TripleElement) leftTranslatedDefn).setObject(leftVar);
						addVariableDefinition(leftVar, leftTranslatedDefn, leftDefnType, expr);
						return leftTranslatedDefn;
					} else if (leftTranslatedDefn instanceof BuiltinElement) {
						int eArgCnt = ((BuiltinElement) leftTranslatedDefn).getExpectedArgCount();
						int currentArgCnt = ((BuiltinElement) leftTranslatedDefn).getArguments() != null
								? ((BuiltinElement) leftTranslatedDefn).getArguments().size()
								: 0;
						if (eArgCnt == currentArgCnt + 1) {
							((BuiltinElement) leftTranslatedDefn).addArgument(leftVar);
							return leftTranslatedDefn;
						}
					}
					if (leftTranslatedDefn instanceof TripleElement
							&& ((TripleElement) leftTranslatedDefn).getSubject() instanceof VariableNode) {
						replaceVariable((TripleElement) leftTranslatedDefn, leftVar);
						addVariableDefinition(leftVar, leftTranslatedDefn, leftDefnType, expr);
						return leftTranslatedDefn;
					} else {
						Node defn = nodeCheck(leftTranslatedDefn);
						GraphPatternElement bi = createBinaryBuiltin(expr.getOp(), leftVar, defn);
						addVariableDefinition(leftVar, leftTranslatedDefn, leftDefnType, expr);
						return bi;
					}
				}
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (DontTypeCheckException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (CircularDefinitionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (CircularDependencyException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (PropertyWithoutRangeException e) {
				addError("Property does not have a range", leftVariableDefn);
			} catch (PrefixNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else if (isRightVariableDefinition) { // only, left is not variable definition
			Object rightTranslatedDefn = processExpression(rightVariableDefn);
			NamedNode rightDefnType = null;
			VariableNode rightVar;
			try {
				rightVar = createVariable(getDeclarationExtensions().getConceptUri(rightVariableName.getName()), expr);
				if (rightVar == null) {
					return null;
				}
				addVariableNamingEObject(rightVar, rightVariableName.getName());
				if (rightTranslatedDefn instanceof NamedNode) {
					rightDefnType = (NamedNode) rightTranslatedDefn;
					setVarType(rightVar, rightDefnType, (Boolean) null, rightVariableDefn);
				} else {
					TypeCheckInfo varType = getModelValidator().getType(rightVariableDefn);
					if (varType != null) {
						if (rightVar.getType() == null) {
							if (varType.getCompoundTypes() != null) {
								Object jct = compoundTypeCheckTypeToNode(varType, rightVariableDefn);
								if (jct != null && jct instanceof Junction) {
									setVarType(rightVar, nodeCheck(jct), (Boolean) null, rightVariableDefn);
								} else {
									addError(
											"Compound type check did not process into expected result for variable type",
											leftVariableDefn);
								}
							} else if (varType.getTypeCheckType() != null
									&& varType.getTypeCheckType() instanceof NamedNode) {
								rightDefnType = (NamedNode) varType.getTypeCheckType();
								setVarType(rightVar, rightDefnType, (Boolean) null, rightVariableDefn);
							}
						}
					} else if (rightTranslatedDefn instanceof GraphPatternElement) {
						if (rightVar.getDefinition() != null) {
							rightVar.getDefinition().add((GraphPatternElement) rightTranslatedDefn);
						} else {
							List<GraphPatternElement> defnLst = new ArrayList<GraphPatternElement>(1);
							defnLst.add((GraphPatternElement) rightTranslatedDefn);
							rightVar.setDefinition(defnLst);
						}
					} else if (rightTranslatedDefn instanceof List<?>) {
						rightVar.setDefinition((List<GraphPatternElement>) rightTranslatedDefn);
					}
				}
				Object lobj = processExpression(expr.getLeft());
				if (lobj instanceof TripleElement && ((TripleElement) lobj).getObject() == null) {
					((TripleElement) lobj).setObject(rightVar);
					return lobj;
				} else {
					return createBinaryBuiltin(expr.getOp(), rightVar, lobj);
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (PrefixNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (DontTypeCheckException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (CircularDefinitionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (CircularDependencyException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (PropertyWithoutRangeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		String op = expr.getOp();

		Expression lexpr = expr.getLeft();
		Expression rexpr = expr.getRight();

		Object result = processBinaryExpressionByParts(expr, op, lexpr, rexpr);
		checkForArticleForNameInTriple(lexpr, result);
		checkForArticleForNameInTriple(rexpr, result);
		return result;
	}

	private void setVariableInDefinition(EObject decl, VariableNode var) {
		if (var != null) {
			if (variablesInDefinition == null) {
				variablesInDefinition = new HashMap<EObject, VariableNode>();
			}
			if (!variablesInDefinition.containsKey(decl)) {
				variablesInDefinition.put(decl, var);
			}
		} else if (variablesInDefinition != null) {
			variablesInDefinition.remove(decl);
		}
	}

	private VariableNode getVariableInDefinition(EObject decl) {
		if (variablesInDefinition != null) {
			return variablesInDefinition.get(decl);
		}
		return null;
	}

	protected void addVariableNamingEObject(VariableNode leftVar, SadlResource name) {
		// TODO Auto-generated method stub
	}

	protected boolean replaceVariable(TripleElement triple, VariableNode var)
			throws IOException, PrefixNotFoundException, InvalidNameException, InvalidTypeException,
			TranslationException, ConfigurationException {
		// TODO Auto-generated method stub
		return false;
	}

	// private VariableNode setListTypeInfo(VariableNode var, NamedNode type,
	// TypeCheckInfo varTci) throws TranslationException {
	// var.setType(type);
	// var.setListLength(type.getListLength());
	// var.setMinListLength(type.getMinListLength());
	// var.setMaxListLength(type.getMaxListLength());
	// return var;
	// }
	//
	private void setVarType(VariableNode var, Node vartype, Boolean isList, EObject defn) throws TranslationException {
		// if it hasn't been set before just set it
		if (vartype instanceof NamedNode) {
			vartype = validateNamedNode((NamedNode) vartype);
		}
		if (var.getType() == null) {
			if (isList(null, defn)) {
				var.setType(vartype);
				int[] lenRest = getLengthRestrictions(defn);
				if (lenRest != null) {
					if (lenRest.length == 1) {
						var.setListLength(lenRest[0]);
					} else if (lenRest.length == 2) {
						var.setMinListLength(lenRest[0]);
						var.setMaxListLength(lenRest[1]);
					}
				}
			} else {
				var.setType(vartype);
				if (isList != null && isList) {
					var.setList(true);
				}
			}
			return;
		}

		// it has been set before so we need to do some type checking
		List<Node> typesOfType = new ArrayList<Node>();
		if (vartype instanceof NamedNode && ((NamedNode) vartype).getNodeType().equals(NodeType.InstanceNode)) {
			Individual typeInst = getTheJenaModel().getIndividual(((NamedNode) vartype).toFullyQualifiedString());
			ExtendedIterator<com.hp.hpl.jena.rdf.model.Resource> typeitr = typeInst.listRDFTypes(false);
			while (typeitr.hasNext()) {
				com.hp.hpl.jena.rdf.model.Resource typ = typeitr.next();
				if (typ.isURIResource()) {
					typesOfType.add(validateNamedNode(new NamedNode(typ.getURI(), NodeType.ClassNode)));
				}
			}
		} else if (vartype instanceof ProxyNode && ((ProxyNode) vartype).getProxyFor() instanceof Junction) {
			Junction jct = (Junction) ((ProxyNode) vartype).getProxyFor();
			typesOfType.addAll(disjunctionToNodeList(jct));
		} else {
			typesOfType.add(vartype);
		}
		for (Node type : typesOfType) {
			if (isList(null, defn)) {
				int[] lenRest = getLengthRestrictions(defn);
				int len = -1;
				int minLen = -1;
				int maxLen = -1;
				if (lenRest != null) {
					if (lenRest.length == 1) {
						len = lenRest[0];
					} else if (lenRest.length == 2) {
						minLen = lenRest[0];
						maxLen = lenRest[1];
					}
				}

				if (var.getType().toFullyQualifiedString().equals(type.toFullyQualifiedString())
						&& var.getListLength() == len && var.getMinListLength() == minLen
						&& var.getMaxListLength() == maxLen) {
					return; // same
				} else {
					Node current = var.getType();
					if (current instanceof NamedNode && type instanceof NamedNode) {
						if (isNamedNodeSubclassOfNamedNode((NamedNode) current, (NamedNode) type)) {
							return; // We're in a loop--OK to return when only one matches? Assumes disjuction?
						}
					} else if (current instanceof ProxyNode
							&& ((ProxyNode) current).getProxyFor() instanceof Junction) {
						Junction jct = (Junction) ((ProxyNode) current).getProxyFor();
						List<Node> jctnodes = disjunctionToNodeList(jct);
						for (Node jctnode : jctnodes) {
							if (jctnode instanceof NamedNode && type instanceof NamedNode) {
								if (isNamedNodeSubclassOfNamedNode((NamedNode) jctnode, (NamedNode) type)) {
									return; // We're in a loop--OK to return when only one matches? Assumes disjuction?
								}
							}
						}
					} else {
						throw new TranslationException("Unhandled Node type");
					}
				}
			} else {
				if (var.getType().toFullyQualifiedString().equals(type.toFullyQualifiedString())) {
					return; // same
				} else {
					Node current = var.getType();
					if (current instanceof NamedNode && type instanceof NamedNode) {
						if (isNamedNodeSubclassOfNamedNode((NamedNode) current, (NamedNode) type)) {
							return; // We're in a loop--OK to return when only one matches? Assumes disjuction?
						}
					} else if (current instanceof ProxyNode
							&& ((ProxyNode) current).getProxyFor() instanceof Junction) {
						Junction jct = (Junction) ((ProxyNode) current).getProxyFor();
						List<Node> jctnodes = disjunctionToNodeList(jct);
						for (Node jctnode : jctnodes) {
							if (jctnode instanceof NamedNode && type instanceof NamedNode) {
								if (isNamedNodeSubclassOfNamedNode((NamedNode) jctnode, (NamedNode) type)) {
									return; // We're in a loop--OK to return when only one matches? Assumes disjuction?
								}
							}
						}
					} else {
						throw new TranslationException("Unhandled Node type");
					}
				}
			}
		}
		addError("Changing type of variable '" + var.getName() + "' from '" + var.getType().toString() + "' to '"
				+ vartype.toString() + "' not allowed.", defn);
	}

	public NamedNode validateNamedNode(NamedNode namedNode) {
		if (namedNode.getPrefix() == null) {
			namedNode.setPrefix(getConfigMgr().getGlobalPrefix(namedNode.getNamespace()));
		}
		return namedNode;
	}

	private boolean isNamedNodeSubclassOfNamedNode(NamedNode cls, NamedNode subcls) {
		OntResource curCls = getTheJenaModel().getOntResource(((NamedNode) cls).toFullyQualifiedString());
		OntResource newCls = getTheJenaModel().getOntResource(((NamedNode) subcls).toFullyQualifiedString());
		if (curCls.canAs(OntClass.class) && newCls.canAs(OntClass.class)) {
			try {
				if (SadlUtils.classIsSubclassOf(newCls.as(OntClass.class), curCls.as(OntClass.class), true, null)) {
					return true; // OK if subclass
				}
			} catch (CircularDependencyException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return false;
	}

	// private VariableNode setListTypeInfo(VariableNode var, NamedNode type,
	// Expression variableDefn) throws TranslationException {
	// var.setType(type);
	// int[] lenRest = getLengthRestrictions(variableDefn);
	// if (lenRest != null) {
	// if (lenRest.length == 1) {
	// var.setListLength(lenRest[0]);
	// }
	// else if (lenRest.length == 2) {
	// var.setMinListLength(lenRest[0]);
	// var.setMaxListLength(lenRest[1]);
	// }
	// }
	// return var;
	// }
	//
	protected boolean isVariableDefinition(Name expr) throws CircularDefinitionException {
		if (expr instanceof Name && getDeclarationExtensions().getOntConceptType(((Name) expr).getName())
				.equals(OntConceptType.VARIABLE)) {
			if (getDeclarationExtensions().getDeclaration(((Name) expr).getName()).equals((Name) expr)) {
				// addInfo("This is a variable definition of '" +
				// getDeclarationExtensions().getConceptUri(((Name)expr).getName()) + "'",
				// expr);
				return true;
			}
		}
		return false;
	}

	private boolean isVariableDefinition(Declaration decl) {
		if (!isDefiniteArticle(decl.getArticle())
				&& (isDeclInThereExists(decl) || (decl.getType() instanceof SadlSimpleTypeReference))) {
			return true;
		}
		return false;
	}

	private boolean isDeclInThereExists(Declaration decl) {
		if (decl.eContainer() != null && decl.eContainer() instanceof UnaryExpression
				&& ((UnaryExpression) decl.eContainer()).getOp().equals("there exists")) {
			return true;
		} else if (decl.eContainer() != null && decl.eContainer() instanceof SubjHasProp
				&& decl.eContainer().eContainer() != null && decl.eContainer().eContainer() instanceof UnaryExpression
				&& ((UnaryExpression) decl.eContainer().eContainer()).getOp().equals("there exists")) {
			return true;
		}
		return false;
	}

	protected Object processBinaryExpressionByParts(EObject container, String op, Expression lexpr, Expression rexpr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		StringBuilder errorMessage = new StringBuilder();
		if (lexpr != null && rexpr != null) {
			if(!getModelValidator().validateBinaryOperationByParts(container, lexpr, rexpr, op, errorMessage, false)){
				addError(errorMessage.toString(), container);
			}
			else {
				Map<EObject, Property> ip = getModelValidator().getImpliedPropertiesUsed();
				if (ip != null) {
					Iterator<EObject> ipitr = ip.keySet().iterator();
					while (ipitr.hasNext()) {
						EObject eobj = ipitr.next();
						OntModelProvider.addImpliedProperty(lexpr.eResource(), eobj, ip.get(eobj));
					}
					// TODO must add implied properties to rules, tests, etc.
				}
			}
		}
		BuiltinType optype = BuiltinType.getType(op);
		Object lobj = null;
		if (lexpr != null) {
			lobj = applyPulledUpOperations(processExpression(lexpr));
		} else {
			addError("Left side of '" + op + "' is null", lexpr); // TODO Add new error
			return null;
		}
		Object robj = null;
		Object rest = null;
		if (rexpr != null) {
			robj = applyPulledUpOperations(processExpression(rexpr));
			if (robj instanceof Object[] && ((Object[]) robj).length == 2) {
				rest = ((Object[]) robj)[1];
				robj = ((Object[]) robj)[0];
			}
		} else {
			addError("Right side of '" + op + "' is null", rexpr); // TODO Add new error
			return null;
		}

		
		if(lobj != null && robj !=null ) 
		{	if(lobj instanceof TripleElement && robj instanceof TripleElement) {
				boolean flag = false;
				TripleElement tr = (TripleElement)lobj;
				TripleElement tl = (TripleElement)robj;
				Node trnode = tr.getObject();
				Node tlnode = tl.getObject();
				if(trnode!= null && tlnode !=null) {
				OntClass subclassl = theJenaModel.getOntClass((trnode).toFullyQualifiedString());
				OntClass subclassr = theJenaModel.getOntClass((tlnode).toFullyQualifiedString());
				OntResource suprclass = theJenaModel.getOntResource(SadlConstants.SADL_IMPLICIT_MODEL_EVENT_URI);
				if(subclassl != null && subclassr != null) {
			    try {
					if (SadlUtils.classIsSubclassOf(subclassl, suprclass, true, null) && SadlUtils.classIsSubclassOf(subclassr,suprclass,true,null)) {
					    
					    try {
							if (SadlUtils.classIsSubclassOf(subclassr,suprclass,true,null)){
							  flag = true;
							}
						} catch (CircularDependencyException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
				} catch (CircularDependencyException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			    if(flag == true && isConjunction(op)){
			    	addError(SadlErrorMessages.INVALID_CONJUNCTION.toString(), container);    	
			    }
			}
		 }	
		}
	  }
			
		if (optype == BuiltinType.Equal || optype == BuiltinType.NotEqual) {
			// If we're doing an assignment, we can simplify the pattern.
			Node assignedNode = null;
			Object pattern = null;
			if (rexpr instanceof Declaration && !(robj instanceof VariableNode)) {
				if (lobj instanceof Node && robj instanceof Node) {
					TripleElement trel = new TripleElement((Node) lobj, new RDFTypeNode(), (Node) robj);
					trel.setSourceType(TripleSourceType.ITC);
					return applyImpliedAndExpandedProperties(container, lexpr, rexpr, trel);
				} else {
					// throw new TranslationException("Unhandled binary operation condition: left
					// and right are not both nodes.");
					addError(SadlErrorMessages.UNHANDLED.get("binary operation condition. ",
							"Left and right are not both nodes."), container);
				}
			}
			if (lobj instanceof NamedNode && !(lobj instanceof VariableNode) && hasCommonVariableSubject(robj)) {
				TripleElement trel = (TripleElement) robj; // TODO how do we know this is a TripleElement? What happens
															// to it?
				while (trel != null) {
					trel.setSubject((Node) lobj);
					trel = (TripleElement) trel.getNext();
				}
				return robj;
			}
			/*
			if ((lobj instanceof TripleElement || (lobj instanceof com.ge.research.sadl.model.gp.Literal
					&& isSparqlQuery(((com.ge.research.sadl.model.gp.Literal) lobj).toString())))
					&& !(ITranslator.isKnownNode(robj))) {
//				if (robj instanceof com.ge.research.sadl.model.gp.Literal) {
//	
//					if (lobj instanceof TripleElement) {
//						if (((TripleElement) lobj).getObject() == null) {
//							((TripleElement) lobj).setObject((com.ge.research.sadl.model.gp.Literal) robj);
//							lobj = checkForNegation((TripleElement) lobj, rexpr);
//							return applyImpliedAndExpandedProperties(container, lexpr, rexpr, lobj);
//						} else {
//							addError(SadlErrorMessages.UNHANDLED.get("rule conclusion construct ", " "), container);
//						}
//					} else {
//						addError(SadlErrorMessages.UNHANDLED.get("rule conclusion construct ", ""), container);
//					}
//				} else 
				if (robj instanceof VariableNode) {
					if (((TripleElement) lobj).getObject() == null) {
						((TripleElement) lobj).setObject((VariableNode) robj);
						lobj = checkForNegation((TripleElement) lobj, rexpr);
						return applyImpliedAndExpandedProperties(container, lexpr, rexpr, lobj);
					}
//				} else if (robj instanceof NamedNode) {
//					if (((TripleElement) lobj).getObject() == null) {
//						((TripleElement) lobj).setObject((NamedNode) robj);
//						lobj = checkForNegation((TripleElement) lobj, rexpr);
//						return applyImpliedAndExpandedProperties(container, lexpr, rexpr, lobj);
//					}
				} else if (robj instanceof BuiltinElement) {
					if (isModifiedTriple(((BuiltinElement) robj).getFuncType())) {
						assignedNode = ((BuiltinElement) robj).getArguments().get(0);
						optype = ((BuiltinElement) robj).getFuncType();
						pattern = lobj;
					} else if (isComparisonBuiltin(((BuiltinElement) robj).getFuncName())) {
						if (((BuiltinElement) robj).getArguments()
								.get(0) instanceof com.ge.research.sadl.model.gp.Literal) {
							((TripleElement) lobj).setObject(nodeCheck(robj));
							return applyImpliedAndExpandedProperties(container, lexpr, rexpr, lobj);
						} else {
							return applyImpliedAndExpandedProperties(container, lexpr, rexpr,
									createBinaryBuiltin(((BuiltinElement) robj).getFuncName(), lobj,
											((BuiltinElement) robj).getArguments().get(0)));
						}
					}
				} else if (robj instanceof TripleElement) {
					// do nothing
//				} else if (robj instanceof ConstantNode) {
//					String cnst = ((ConstantNode) robj).getName();
//					if (cnst.equals(SadlConstants.CONSTANT_NONE)) {
//						((TripleElement) lobj).setType(TripleModifierType.None);
//						((TripleElement) lobj).setObject((Node) robj);
//						return applyImpliedAndExpandedProperties(container, lexpr, rexpr, lobj);
//					}
				} else if (robj instanceof BuiltinElement) {
					if (isModifiedTriple(((BuiltinElement) robj).getFuncType())) {
						if (((BuiltinElement) robj).getArguments() != null
								&& ((BuiltinElement) robj).getArguments().size() > 0) {
							assignedNode = ((BuiltinElement) robj).getArguments().get(0);
						}
						optype = ((BuiltinElement) robj).getFuncType();
						pattern = lobj;
					} else if (isComparisonBuiltin(((BuiltinElement) robj).getFuncName())) {
						if (((BuiltinElement) robj).getArguments().get(0) instanceof Literal) {
							((TripleElement) lobj).setObject(nodeCheck(robj));
							return applyImpliedAndExpandedProperties(container, lexpr, rexpr, lobj);
						} else {
							return applyImpliedAndExpandedProperties(container, lexpr, rexpr,
									createBinaryBuiltin(((BuiltinElement) robj).getFuncName(), lobj,
											((BuiltinElement) robj).getArguments().get(0)));
						}
					}
				}
				// else {
				// addError(SadlErrorMessages.UNHANDLED.get("assignment construct in rule
				// conclusion", " "), container);
				// }
			} else 
				*/
			if (lobj instanceof Node && robj instanceof TripleElement) {
				assignedNode = validateNode((Node) lobj);
				pattern = (TripleElement) robj;
//			} else if (robj instanceof Node && lobj instanceof TripleElement) {
//				assignedNode = validateNode((Node) robj);
//				pattern = (TripleElement) lobj;
			}
			if (assignedNode != null && pattern != null) {
				// We're expressing the type of a named thing.
				if (pattern instanceof TripleElement && ((TripleElement) pattern).getSubject() == null) {
					if (isModifiedTripleViaBuitin(robj)) {
						optype = ((BuiltinElement) ((TripleElement) pattern).getNext()).getFuncType();
						((TripleElement) pattern).setNext(null);
					}
					((TripleElement) pattern).setSubject(assignedNode);
					if (optype != BuiltinType.Equal) {
						((TripleElement) pattern).setType(getTripleModifierType(optype));
					}
				} else if (pattern instanceof TripleElement && ((TripleElement) pattern).getObject() == null
						&& (((TripleElement) pattern).getSourceType().equals(TripleSourceType.PSnewV)
								|| ((TripleElement) pattern).getSourceType().equals(TripleSourceType.PSV))) {
					if (isModifiedTripleViaBuitin(robj)) {
						optype = ((BuiltinElement) ((TripleElement) pattern).getNext()).getFuncType();
						((TripleElement) pattern).setNext(null);
					}
					((TripleElement) pattern).setObject(assignedNode);
					if (optype != BuiltinType.Equal) {
						((TripleElement) pattern).setType(getTripleModifierType(optype));
					}
				} else if (pattern instanceof TripleElement
						&& ((TripleElement) pattern).getSourceType().equals(TripleSourceType.SPV)
						&& assignedNode instanceof NamedNode
						&& getProxyWithNullSubject(((TripleElement) pattern)) != null) {
					TripleElement proxyFor = getProxyWithNullSubject(((TripleElement) pattern));
					assignNullSubjectInProxies(((TripleElement) pattern), proxyFor, assignedNode);
					if (optype != BuiltinType.Equal) {
						proxyFor.setType(getTripleModifierType(optype));
					}
				} else if (isModifiedTriple(optype)
						|| (optype.equals(BuiltinType.Equal) && pattern instanceof TripleElement
								&& (((TripleElement) pattern).getObject() == null
										|| ((TripleElement) pattern).getObject() instanceof NamedNode
										|| ((TripleElement) pattern)
												.getObject() instanceof com.ge.research.sadl.model.gp.Literal))) {
					if (pattern instanceof TripleElement && isModifiedTripleViaBuitin(robj)) {
						optype = ((BuiltinElement) ((TripleElement) pattern).getNext()).getFuncType();
						((TripleElement) pattern).setObject(assignedNode);
						((TripleElement) pattern).setNext(null);
						((TripleElement) pattern).setType(getTripleModifierType(optype));
					} else if (isComparisonViaBuiltin(robj, lobj)) {
						BuiltinElement be = (BuiltinElement) ((TripleElement) robj).getNext();
						be.addMissingArgument((Node) lobj);
						return applyImpliedAndExpandedProperties(container, lexpr, rexpr, pattern);
					} else if (pattern instanceof TripleElement) {
						TripleElement lastPattern = (TripleElement) pattern;
						// this while may need additional conditions to narrow application to nested
						// triples?
						while (lastPattern.getNext() != null && lastPattern instanceof TripleElement) {
							lastPattern = (TripleElement) lastPattern.getNext();
						}
						if (getEncapsulatingTarget() instanceof Test) {
							((Test) getEncapsulatingTarget()).setRhs(assignedNode);
							((Test) getEncapsulatingTarget()).setCompName(optype);
						} else if (getEncapsulatingTarget() instanceof Query && getTarget() instanceof Test) {
							((Test) getTarget()).setRhs(getEncapsulatingTarget());
							((Test) getTarget()).setLhs(assignedNode);
							((Test) getTarget()).setCompName(optype);
						} else if (getTarget() instanceof Test && assignedNode != null) {
							((Test) getTarget()).setLhs(pattern);
							((Test) getTarget()).setRhs(assignedNode);
							((Test) getTarget()).setCompName(optype);
							((TripleElement) pattern).setType(TripleModifierType.None);
							optype = BuiltinType.Equal;
						} else {
							lastPattern.setObject(assignedNode);
						}
						if (!optype.equals(BuiltinType.Equal)) {
							((TripleElement) pattern).setType(getTripleModifierType(optype));
						}
					} else {
						if (getTarget() instanceof Test) {
							((Test) getTarget()).setLhs(lobj);
							((Test) getTarget()).setRhs(assignedNode);
							((Test) getTarget()).setCompName(optype);
						}
					}
				} else if (getEncapsulatingTarget() instanceof Test) {
					((Test) getEncapsulatingTarget()).setRhs(assignedNode);
					((Test) getEncapsulatingTarget()).setCompName(optype);
				} else if (getTarget() instanceof Rule && pattern instanceof TripleElement
						&& ((TripleElement) pattern).getSourceType().equals(TripleSourceType.ITC)
						&& ((TripleElement) pattern).getSubject() instanceof VariableNode
						&& assignedNode instanceof VariableNode) {
					// in a rule of this type we just want to replace the pivot node variable
					doVariableSubstitution(((TripleElement) pattern),
							(VariableNode) ((TripleElement) pattern).getSubject(), (VariableNode) assignedNode);
				}
				return applyImpliedAndExpandedProperties(container, lexpr, rexpr, pattern);
			}
			BuiltinElement bin = null;
			boolean binOnRight = false;
			Object retObj = null;
			if (lobj instanceof Node && robj instanceof BuiltinElement) {
				assignedNode = validateNode((Node) lobj);
				bin = (BuiltinElement) robj;
				retObj = robj;
				binOnRight = true;
			} else if (robj instanceof Node && lobj instanceof BuiltinElement) {
				assignedNode = validateNode((Node) robj);
				bin = (BuiltinElement) lobj;
				retObj = lobj;
				binOnRight = false;
			}
			if (bin != null && assignedNode != null) {
				if ((assignedNode instanceof VariableNode || (assignedNode instanceof NamedNode
						&& ((NamedNode) assignedNode).getNodeType().equals(NodeType.VariableNode)))) {
					if (getTarget() instanceof Rule && containsDeclaration(robj)) {
						return replaceDeclarationWithVariableAndAddUseDeclarationAsDefinition(lexpr, lobj, rexpr, robj);
					} else {
						while (bin.getNext() instanceof BuiltinElement) {
							bin = (BuiltinElement) bin.getNext();
						}
						if (bin.isCreatedFromInterval()) {
							bin.addArgument(0, assignedNode);
						} else {
							bin.addArgument(assignedNode);
						}
					}
					return applyImpliedAndExpandedProperties(container, lexpr, rexpr, retObj);
				} else if (assignedNode instanceof Node && isComparisonBuiltin(bin.getFuncName())) {
					// this is a comparison with an extra "is"
					if (bin.getArguments().size() == 1) {
						if (bin.isCreatedFromInterval() || binOnRight) {
							bin.addArgument(0, assignedNode);
						} else {
							bin.addArgument(assignedNode);
						}
						return applyImpliedAndExpandedProperties(container, lexpr, rexpr, bin);
					}
				}
			}
			// We're describing a thing with a graph pattern.
			Set<VariableNode> vars = pattern instanceof TripleElement ? getSelectVariables(((TripleElement) pattern))
					: null;
			if (vars != null && vars.size() == 1) {
				// Find where the unbound variable occurred in the pattern
				// and replace each place with the assigned node.
				VariableNode var = vars.iterator().next();
				GraphPatternElement gpe = ((TripleElement) pattern);
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
				return applyImpliedAndExpandedProperties(container, lexpr, rexpr, pattern);
			}
		}
		// if we get to here we want to actually create a BuiltinElement for the
		// BinaryOpExpression
		// However, if the type is equal ("is", "equal") and the left side is a
		// VariableNode and the right side is a literal
		// and the VariableNode hasn't already been bound, change from type equal to
		// type assign.
		if (optype == BuiltinType.Equal && getTarget() instanceof Rule && lobj instanceof VariableNode
				&& robj instanceof com.ge.research.sadl.model.gp.Literal
				&& !variableIsBound((Rule) getTarget(), null, (VariableNode) lobj)) {
			return applyImpliedAndExpandedProperties(container, lexpr, rexpr,
					createBinaryBuiltin("assign", robj, lobj));
		}
		if (op.equals("and") || op.equals("or")) {
			Junction jct = new Junction();
			jct.setJunctionName(op);
			jct.setLhs(nodeCheck(postProcessTranslationResult(lobj)));
			jct.setRhs(nodeCheck(robj instanceof NamedNode && rest != null ? rest : robj));
			return jct;
		} else {
			boolean isNegated = false;
			boolean reverseArgs = false;
			if (op.equals("does not contain")) {
				isNegated = true;
				op = "contains";
			} else if (op.equals("is not unique in")) {
				isNegated = true;
				reverseArgs = true;
				op = "unique";
			} else if (op.equals("is unique in")) {
				reverseArgs = true;
				op = "unique";
			}
			GraphPatternElement bi;
			if (reverseArgs) {
				bi = createBinaryBuiltin(op, robj, postProcessTranslationResult(lobj));
			} else {
				bi = createBinaryBuiltin(op, postProcessTranslationResult(lobj), robj);
			}
			if (isNegated) {
				bi = (GraphPatternElement) createUnaryBuiltin(container, "not", bi);
			}
			return combineRest(applyImpliedAndExpandedProperties(container, lexpr, rexpr, bi), rest);
		}
	}

	private Object applyImpliedAndExpandedProperties(EObject binobj, EObject lobj, EObject robj, Object maybeGpe) {
		try {
			Map<EObject, Property> ip = getModelValidator().getImpliedPropertiesUsed();
			List<EObject> toBeRemoved = null;
			if (ip != null) {
				if (maybeGpe instanceof TripleElement || maybeGpe instanceof BuiltinElement) {
					Iterator<EObject> ipitr = ip.keySet().iterator();
					boolean matched = false;
					while (ipitr.hasNext()) {
						EObject eobj = ipitr.next();
						Property implProp = ip.get(eobj);
						if (eobj.equals(lobj)) {
							((GraphPatternElement) maybeGpe).setLeftImpliedPropertyUsed(
									validateNamedNode(new NamedNode(implProp.getURI(), NodeType.PropertyNode)));
							matched = true;
							if (toBeRemoved == null) {
								toBeRemoved = new ArrayList<EObject>();
							}
							toBeRemoved.add(lobj);
						}
						else if (eobj.equals(robj)) {
							((GraphPatternElement)maybeGpe).setRightImpliedPropertyUsed(validateNamedNode(new NamedNode(implProp.getURI(), NodeType.PropertyNode)));
							matched = true;
							if (toBeRemoved == null) {
								toBeRemoved = new ArrayList<EObject>();
							}
							toBeRemoved.add(robj);
						}
					}
					if (!matched) {
						// throw new TranslationException("EObject key of impliedProperty is neither
						// left nor right of binary operation for any implied Property");
						int i = 0;
					}
					// TODO must add implied properties to rules, tests, etc.
				} else {
					throw new TranslationException("Unexpected type to which to apply implied and expanded properties");
				}
			}
			if (toBeRemoved != null) {
				for (int i = 0; i < toBeRemoved.size(); i++) {
					getModelValidator().removeImpliedPropertyUsed(toBeRemoved.get(i));
				}
			}

			Map<EObject, List<String>> ep = getModelValidator().getApplicableExpandedProperties();
			if (ep != null) {
				if (maybeGpe instanceof TripleElement || maybeGpe instanceof BuiltinElement) {
					Iterator<EObject> epitr = ep.keySet().iterator();
					boolean match = false;
					while (epitr.hasNext()) {
						EObject eobj = epitr.next();
						if (eobj.equals(binobj)) {
							List<String> propuris = ep.get(eobj);
							for (String propuri : propuris) {
								Property prop = getTheJenaModel().getProperty(propuri);
								if (prop != null) {
									((GraphPatternElement) maybeGpe).addExpandedPropertyToBeUsed(
											validateNamedNode(new NamedNode(propuri, NodeType.PropertyNode)));
									match = true;
								} else {
									throw new TranslationException(
											"Expanded property '" + propuri + "' not found in ontology");
								}
							}
						}
					}
					if (!match) {
						int i = 0;
					}
				} else {
					throw new TranslationException("Unexpected type to which to apply implied and expanded properties");
				}
				getModelValidator().clearApplicableExpandedProperties();
			}
		} catch (TranslationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return maybeGpe;
	}

	private TripleElement checkForNegation(TripleElement lobj, Expression rexpr) throws InvalidTypeException {
		if (isOperationPullingUp(rexpr) && isNegation(rexpr)) {
			lobj.setType(TripleModifierType.Not);
			getOperationPullingUp();
		}
		return lobj;
	}

	private boolean isNegation(Expression expr) {
		if (expr instanceof UnaryExpression && ((UnaryExpression) expr).getOp().equals("not")) {
			return true;
		}
		return false;
	}

	private Object replaceDeclarationWithVariableAndAddUseDeclarationAsDefinition(Expression lexpr, Object lobj,
			Expression rexpr, Object robj) throws TranslationException, InvalidTypeException, InvalidNameException {
		if (lobj instanceof VariableNode) {
			Object[] declAndTrans = getDeclarationAndTranslation(rexpr);
			if (declAndTrans != null) {
				Object rtrans = declAndTrans[1];
				if (rtrans instanceof NamedNode) {
					if (((NamedNode) rtrans).getNodeType().equals(NodeType.ClassNode)) {
						if (replaceDeclarationInRightWithVariableInLeft((Node) lobj, robj, rtrans)) {
							TripleElement newTriple = new TripleElement((Node) lobj,
									validateNamedNode(new NamedNode(RDF.type.getURI(), NodeType.ObjectProperty)),
									(Node) rtrans);
							Junction jct = createJunction(rexpr, "and", newTriple, robj);
							return jct;
						}
					}
				}
			}
		}
		return null;
	}

	private boolean replaceDeclarationInRightWithVariableInLeft(Node lobj, Object robj, Object rtrans) {
		if (robj instanceof BuiltinElement) {
			Iterator<Node> argitr = ((BuiltinElement) robj).getArguments().iterator();
			while (argitr.hasNext()) {
				Node arg = argitr.next();
				if (replaceDeclarationInRightWithVariableInLeft(lobj, arg, rtrans)) {
					return true;
				}
			}
		} else if (robj instanceof ProxyNode) {
			if (replaceDeclarationInRightWithVariableInLeft(lobj, ((ProxyNode) robj).getProxyFor(), rtrans)) {
				return true;
			}
		} else if (robj instanceof TripleElement) {
			Node subj = ((TripleElement) robj).getSubject();
			if (subj.equals(rtrans)) {
				((TripleElement) robj).setSubject(lobj);
				return true;
			} else if (replaceDeclarationInRightWithVariableInLeft(lobj, subj, rtrans)) {
				return true;
			}
		}
		return false;
	}

	private Object[] getDeclarationAndTranslation(Expression expr) throws TranslationException {
		Declaration decl = getLeftDeclaration(expr);
		if (decl != null) {
			Object declprocessed = processExpression(decl);
			if (declprocessed != null) {
				Object[] result = new Object[2];
				result[0] = decl;
				result[1] = declprocessed;
				return result;
			}
		}
		return null;
	}

	private Declaration getLeftDeclaration(Expression rexpr) throws TranslationException {
		if (rexpr instanceof SubjHasProp) {
			return getDeclarationFromSubjHasProp((SubjHasProp) rexpr);
		} else if (rexpr instanceof BinaryOperation) {
			Declaration decl = getLeftDeclaration(((BinaryOperation) rexpr).getLeft());
			if (decl != null) {
				return decl;
			}
			// decl = getDeclaration(((BinaryOperation)rexpr).getRight());
			// if (decl != null) {
			// return decl;
			// }
		}
		return null;
	}

	protected boolean isList(NamedNode typenode, EObject expr) {
		if (typenode != null && typenode.isList()) {
			return true;
		}
		if (expr instanceof Declaration) {
			return isList(null, ((Declaration) expr).getType());
		}
		if (expr instanceof SadlSimpleTypeReference && ((SadlSimpleTypeReference) expr).isList()) {
			return true;
		}
		if (expr instanceof SubjHasProp) {
			return isList(null, ((SubjHasProp) expr).getLeft());
		}
		return false;
	}

	/**
	 * Method to return, if any, the length restrictions on a List declaration
	 * 
	 * @param eObject
	 * @return
	 */
	public int[] getLengthRestrictions(EObject eObject) {
		if (eObject instanceof Declaration) {
			String len = ((Declaration) eObject).getLen();
			String maxLen = ((Declaration) eObject).getMaxlen();
			int lenL = -1;
			int minL = -1;
			int maxL = -1;
			if (len != null) {
				lenL = Integer.parseInt(len);
			}
			if (maxLen != null) {
				if (lenL >= 0) {
					minL = lenL;
					lenL = -1;
				}
				if (maxLen.trim().equals("*")) {
					maxL = -1; // no limit
				} else {
					maxL = Integer.parseInt(maxLen);
				}
			}
			if (lenL >= 0) {
				int[] result = new int[1];
				result[0] = lenL;
				return result;
			} else if (minL >= 0) {
				int[] result = new int[2];
				result[0] = minL;
				result[1] = maxL;
				return result;
			}
		}
		return null;
	}

	protected boolean isDeclaration(EObject expr) {
		if (expr instanceof SubjHasProp) {
			return isDeclaration(((SubjHasProp) expr).getLeft());
		} else if (expr instanceof BinaryOperation) {
			if (isDeclaration(((BinaryOperation) expr).getLeft())) {
				return true;
			}
			if (isDeclaration(((BinaryOperation) expr).getRight())) {
				return true;
			}
		} else if (expr instanceof BinaryOperation) {
			if (isDeclaration(((BinaryOperation) expr).getLeft())) {
				return true;
			}
			if (isDeclaration(((BinaryOperation) expr).getRight())) {
				return true;
			}
		} else if (expr instanceof UnaryExpression && ((UnaryExpression) expr).getExpr() instanceof Declaration) {
			return true;
		} else if (expr instanceof Declaration) {
			return true;
		}
		return false;
	}

	private void validateTripleTypes(EObject subjeo, EObject predeo, EObject objeo, TripleElement tr, Expression expr)
			throws TranslationException, InvalidTypeException, CircularDependencyException, InvalidNameException {
		if (getModelValidator() != null) {
			Node nsubj = tr.getSubject();
			boolean isCrVar = nsubj instanceof VariableNode ? ((VariableNode) nsubj).isCRulesVariable() : false;
			String varName = nsubj instanceof NamedNode ? ((NamedNode) nsubj).getName() : null;
			OntResource subj = null;
			if (nsubj == null) {
				addError("Triple has null subject in validation", expr);
				return;
			} else if (nsubj instanceof ProxyNode) {
				Object pf = ((ProxyNode) nsubj).getProxyFor();
				if (pf instanceof TripleElement) {
					Node pfPred = ((TripleElement) pf).getPredicate();
					if (pfPred instanceof NamedNode) {

						TypeCheckInfo pfpredtci;
						try {
							pfpredtci = getModelValidator().getTypeInfoFromRange(
									namedNodeToConceptName((NamedNode) pfPred),
									getTheJenaModel().getProperty(((NamedNode) pfPred).toFullyQualifiedString()),
									subjeo);
							if (pfpredtci.getTypeCheckType() != null) {
								Node pfpredrngtype = pfpredtci.getTypeCheckType();
								if (pfpredrngtype instanceof NamedNode) {
									subj = getTheJenaModel()
											.getOntResource(((NamedNode) pfpredrngtype).toFullyQualifiedString());
								}
							}
						} catch (DontTypeCheckException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
				}
			} else {
				subj = getOntResource(tr.getSubject());
			}
			Node pnode = tr.getPredicate();
			OntProperty pred = getTheJenaModel().getOntProperty(tr.getPredicate().toFullyQualifiedString());
			if (pred == null) {
				if (tr.getPredicate() instanceof VariableNode) {
					if (EcoreUtil2.getContainerOfType(expr, QueryStatement.class) != null) {
						return; // variables as property in queries is OK
					}
					addError("Property '" + ((VariableNode) tr.getPredicate()).toDescriptiveString()
							+ "' is a variable, unable to validate", expr);
				} else {
					addError("Unexpected error finding property '" + tr.getPredicate().toDescriptiveString()
							+ "' in ontology, cannot validate", expr);
				}
				return;
			}
			getModelValidator().checkPropertyDomain(getTheJenaModel(), subj, pred, expr, true,
					isCrVar ? varName : null);
			NodeType pnodetype;
			if (pnode instanceof NamedNode) {
				pnodetype = ((NamedNode) pnode).getNodeType();
			} else {
				if (pred.isObjectProperty()) {
					pnodetype = NodeType.ObjectProperty;
				} else if (pred.isDatatypeProperty()) {
					pnodetype = NodeType.DataTypeProperty;
				} else if (pred.isAnnotationProperty()) {
					pnodetype = NodeType.AnnotationProperty;
				} else {
					pnodetype = NodeType.PropertyNode;
				}
			}
			if (pnodetype.equals(NodeType.AnnotationProperty)) {
				return; // can't check annotation property as there should be no range specified (OWL
						// 1). What about local restrictions on annotation properties?
			}
			Node obj = tr.getObject();
			if (obj != null) {
				handleLocalRestriction(expr, tr);
				checkTripleRange(subjeo, predeo, objeo, expr, tr.getSubject(), tr.getPredicate(), pred, pnodetype, obj,
						false);
			}
		}
	}

	private void checkTripleRange(EObject subjeo, EObject predeo, EObject objeo, Expression expr, Node subjNode,
			Node predNode, OntProperty pred, NodeType pnodetype, Node obj, boolean isKnownToBeList)
			throws InvalidTypeException, TranslationException, CircularDependencyException {
		if (obj instanceof ConstantNode) {
			String cnstval = ((ConstantNode) obj).getName();
			double dval = 0;
			if (cnstval.equals(SadlConstants.CONSTANT_NONE)) {
				// None is ok with any subject and predicate
				return;
			} else if (cnstval.equals(SadlConstants.CONSTANT_PI)) {
				dval = Math.PI;
			} else if (cnstval.equals(SadlConstants.CONSTANT_E)) {
				dval = Math.E;
			}
			if (pnodetype.equals(NodeType.DataTypeProperty)) {
				try {
					Literal litval = SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), pred, dval);
					if (!valueInDatatypePropertyRange(pred, litval, expr)) {
						addError("Value '" + nodeToString(obj) + "' is not in the range of property '"
								+ rdfNodeToString(pred) + "'", expr);
					}
				} catch (Throwable t) {
					addError("Value '" + nodeToString(obj) + "' is not in the range of property '"
							+ rdfNodeToString(pred) + "'", expr);
				}
			} else if (predNode instanceof NamedNode && isProperty(((NamedNode) predNode).getNodeType())) {
				try {
					TypeCheckInfo ptci = getModelValidator().getTypeInfoFromRange(
							namedNodeToConceptName((NamedNode) predNode),
							getTheJenaModel().getProperty(((NamedNode) predNode).toFullyQualifiedString()), predeo);
					TypeCheckInfo otci = getModelValidator().getType(objeo);
					List<String> operations = new ArrayList<String>(1);
					operations.add("is");
					if (getModelValidator().compareTypesUsingImpliedPropertiesRecursively(operations, predeo, objeo,
							ptci, otci, ImplicitPropertySide.LEFT)) {
						return;
					}
				} catch (DontTypeCheckException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (InvalidNameException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (URISyntaxException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (CircularDefinitionException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (PropertyWithoutRangeException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			} else if (predNode instanceof VariableNode) {
				throw new TranslationException("Unhandled condition");
			}
		} else if (ITranslator.isKnownNode(obj)) {
			// no type checking for "known"
			return;
		} else if (obj instanceof com.ge.research.sadl.model.gp.Literal) {
			if (pnodetype.equals(NodeType.DataTypeProperty)) {
				try {
					Literal litval = SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), pred,
							((com.ge.research.sadl.model.gp.Literal) obj).getValue());
					if (!valueInDatatypePropertyRange(pred, litval, expr)) {
						addError("Value '" + nodeToString(obj) + "' is not in the range of property '"
								+ rdfNodeToString(pred) + "'", expr);
					}
				} catch (Throwable t) {
					addError("Value '" + nodeToString(obj) + "' is not in the range of property '"
							+ rdfNodeToString(pred) + "'", expr);
				}
			} else {
				if (predNode instanceof NamedNode && isProperty(((NamedNode) predNode).getNodeType())) {
					try {
						TypeCheckInfo ptci = getModelValidator().getTypeInfoFromRange(
								namedNodeToConceptName((NamedNode) predNode),
								getTheJenaModel().getProperty(((NamedNode) predNode).toFullyQualifiedString()), predeo);
						TypeCheckInfo otci = getModelValidator().getType(objeo);
						List<String> operations = new ArrayList<String>(1);
						operations.add("is");
						if (getModelValidator().compareTypesUsingImpliedPropertiesRecursively(operations, predeo, objeo,
								ptci, otci, ImplicitPropertySide.LEFT)) {
							return;
						}
					} catch (DontTypeCheckException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (InvalidNameException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (URISyntaxException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (ConfigurationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (CircularDefinitionException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (PropertyWithoutRangeException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				} else if (predNode instanceof VariableNode) {
					throw new TranslationException("Unhandled condition");
				}
				addError("Value '" + nodeToString(obj) + "' is not in the range of property '" + rdfNodeToString(pred)
						+ "'", expr);
			}
		} else if (obj instanceof NamedNode) {
			OntResource objrsrc = getOntResource(obj);
			boolean isList = false;
			NodeType objtype = ((NamedNode) obj).getNodeType();
			if (objtype.equals(NodeType.ClassNode)) {
				classInObjectTypePropertyRange(pred, objrsrc, isList, expr);
			} else if (objtype.equals(NodeType.ClassListNode)) {
				isList = true;
				if (isKnownToBeList) {
					classInObjectTypePropertyRange(pred, objrsrc, isList, expr);
				} else if (getModelValidator().isSadlTypedList(objrsrc) && objrsrc.canAs(OntClass.class)) {
					com.hp.hpl.jena.rdf.model.Resource listtype = getModelValidator()
							.getSadlTypedListType(objrsrc.as(OntClass.class));
					isList = true;
					if (listtype.canAs(OntResource.class)) {
						objrsrc = listtype.as(OntResource.class);
						classInObjectTypePropertyRange(pred, objrsrc, isList, expr);
					} else {
						throw new TranslationException("Unexpected error: the list type of object list '" + ((NamedNode)obj).toFullyQualifiedString() + "' cannot be morphed to an OntResource");
					}
				} else {
					throw new TranslationException("Unexpected error: the object list '" + ((NamedNode)obj).toFullyQualifiedString() + "' cannot be morphed to an OntClass");
				}
			} else if (objtype.equals(NodeType.DataTypeNode)) {
				datatypeInDatatypePropertyRange(pred, obj, isList, expr);
			} else if (objtype.equals(NodeType.DataTypeListNode)) {
				isList = true;
				if (isKnownToBeList) {
					datatypeInDatatypePropertyRange(pred, objrsrc, isList, expr);
				} else if (getModelValidator().isSadlTypedList(objrsrc) && objrsrc.canAs(OntClass.class)) {
					com.hp.hpl.jena.rdf.model.Resource listtype = getModelValidator()
							.getSadlTypedListType(objrsrc.as(OntClass.class));
					isList = true;
					if (listtype.canAs(OntResource.class)) {
						objrsrc = listtype.as(OntResource.class);
						datatypeInDatatypePropertyRange(pred, objrsrc, isList, expr);
					} else {
						throw new TranslationException("Unexpected error: the list type of data list '" + ((NamedNode)obj).toFullyQualifiedString() + "' cannot be morphed to an OntResource");
					}
				} else {
					throw new TranslationException("Unexpected error: the data list '" + ((NamedNode)obj).toFullyQualifiedString() + "' cannot be morphed to an OntClass");
				}
			} else if (objtype.equals(NodeType.InstanceNode)) {
				instanceInObjectTypePropertyRange(pred, obj, isList, expr);
			} else if (objtype.equals(NodeType.VariableNode)) {
				if (obj instanceof VariableNode) {
					Node vartype = ((VariableNode) obj).getType();
					if (((VariableNode) obj).isList()) {
						isList = true;
					}
					checkTripleRange(subjeo, predeo, null, expr, subjNode, predNode, pred, pnodetype, vartype, isList);
				} else {
					throw new TranslationException("Unexpected error, disagreement between objtype and actual obj");
				}
			} else {
				throw new TranslationException("Unexpected error: the type of '" + ((NamedNode)obj).toFullyQualifiedString() + "' (" + objtype.toString() + ") is not handled.");
			}
		} else if (obj instanceof ProxyNode) {
			Object pf = ((ProxyNode) obj).getProxyFor();
			if (pf instanceof TripleElement) {
				// need range of predicate of nested triple
				Node pfpn = ((TripleElement) pf).getPredicate();
				if (pfpn instanceof ProxyNode && ((ProxyNode) pfpn).getProxyFor() instanceof BuiltinElement
						&& ((BuiltinElement) ((ProxyNode) pfpn).getProxyFor()).getFuncName().equals("previous")) {
					pfpn = ((BuiltinElement) ((ProxyNode) pfpn).getProxyFor()).getArguments().get(0);
				}
				if (getModelValidator() != null) {
					if (pfpn instanceof NamedNode) {
						try {
							TypeCheckInfo pftci = getModelValidator().getTypeInfoFromRange(
									namedNodeToConceptName((NamedNode) pfpn),
									getTheJenaModel().getProperty(((NamedNode) pfpn).toFullyQualifiedString()), expr);
							Node rngnn = pftci.getTypeCheckType();
							if (rngnn instanceof NamedNode) {
								boolean isList = ((NamedNode) rngnn).isList();
								checkTripleRange(subjeo, predeo, (EObject) null, expr, subjNode, predNode, pred,
										pnodetype, (NamedNode) rngnn, isList);
							} else {
								throw new TranslationException("Unexpected error: range of property '" + ((NamedNode)pfpn).toFullyQualifiedString() + "' is not a named node (unions are not yet handled)");
							}
						} catch (DontTypeCheckException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						} catch (InvalidNameException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					} else {
						throw new TranslationException("Unexpected error: property in triple is not a named node.");
					}
				}
			} else if (pf instanceof BuiltinElement) {
				String pseudoObj = null;
				String op = ((BuiltinElement) pf).getFuncName();
				if (isNumericOperator(op)) {
					pseudoObj = XSD.decimal.getURI();
				} else if (isBooleanOperator(op)) {
					pseudoObj = XSD.xboolean.getURI();
				}
				if (pseudoObj != null) {
					NamedNode poNode = new NamedNode(pseudoObj);
					poNode.setNodeType(NodeType.DataTypeNode);
					checkTripleRange(subjeo, predeo, (EObject) null, expr, subjNode, predNode, pred, pnodetype, poNode,
							poNode.isList());
				} else {
					try {
						TypeCheckInfo bitci = getModelValidator().getType(objeo);
						if (bitci != null && bitci.getTypeCheckType() != null) {
							Node bitype = bitci.getTypeCheckType();
							if (bitype instanceof NamedNode) {
								checkTripleRange(subjeo, predeo, objeo, null, subjNode, predNode, pred, pnodetype,
										bitype, ((NamedNode) bitype).isList());
							} else {
								throw new TranslationException("Unexpected error: the return type of the builtin '" + ((BuiltinElement)pf).getFuncName() + "' isn't a NamedNode.");
							}
						}
					} catch (InvalidNameException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (URISyntaxException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (ConfigurationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (DontTypeCheckException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (CircularDefinitionException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (PropertyWithoutRangeException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			} else if (pf instanceof Junction) {
				// the type of any Junction is boolean
				String pseudoObj = XSD.xboolean.getURI();
				NamedNode poNode = new NamedNode(pseudoObj);
				poNode.setNodeType(NodeType.DataTypeNode);
				checkTripleRange(subjeo, predeo, (EObject) null, expr, subjNode, predNode, pred, pnodetype, poNode,
						poNode.isList());		
			} else {
				throw new TranslationException("Unexpected error: the object of the triple is a ProxyNode but the proxyFor type isn't handled (" + pf.getClass().getCanonicalName() + ")");
			}
		} else {
			throw new TranslationException("Unexpected error: the object of the triple is not a node of known type ('" + obj.getClass().getCanonicalName() + ")");
		}
	}

	public void handleLocalRestriction(EObject expr, TripleElement tr) {
		// TODO Auto-generated method stub
		int i = 0;
	}

	private void datatypeInDatatypePropertyRange(OntProperty pred, OntResource objrsrc, boolean isList, Expression expr)
			throws TranslationException, CircularDependencyException {
		if (getModelValidator() != null) {
			getModelValidator().checkPropertyRange(getTheJenaModel(), pred, objrsrc, isList, expr);
		}
	}

	private void datatypeInDatatypePropertyRange(OntProperty pred, Node obj, boolean isList, Expression expr)
			throws TranslationException, CircularDependencyException {
		if (obj instanceof NamedNode) {
			OntResource objrsrc = getTheJenaModel().getOntResource(((NamedNode) obj).toFullyQualifiedString());
			if (objrsrc != null) {
				datatypeInDatatypePropertyRange(pred, objrsrc, isList, expr);
			}
			else {
				addError("'" + obj.toString() + "' is not in range of property '" + pred.getLocalName() + "'", expr);
			}
		}
	}

	private void instanceInObjectTypePropertyRange(OntProperty pred, Node obj, boolean isList, Expression expr)
			throws CircularDependencyException, TranslationException {
		if (getModelValidator() != null) {
			if (obj instanceof NamedNode) {
				OntResource objrsrc = getTheJenaModel().getIndividual(((NamedNode) obj).toFullyQualifiedString());
				getModelValidator().checkObjectPropertyRange(getTheJenaModel(), pred, objrsrc, isList, expr);
			}
		}
	}

	private void classInObjectTypePropertyRange(OntProperty pred, OntResource obj, boolean isList, Expression expr)
			throws TranslationException, CircularDependencyException {
		if (getModelValidator() != null) {
			if (!getModelValidator().checkObjectPropertyRange(getTheJenaModel(), pred, obj, isList, expr)) {
				addError("Class '" + rdfNodeToString(obj) + "' is not in the range of property '"
						+ rdfNodeToString(pred) + "'", expr);
			}
		}
	}

	private OntResource getOntResource(Node node) {
		if (node instanceof VariableNode) {
			if (((VariableNode) node).getType() != null) {
				return getTheJenaModel().getOntResource(((VariableNode) node).getType().toFullyQualifiedString());
			} else {
				return null;
			}
		}
		return getTheJenaModel().getOntResource(node.toFullyQualifiedString());
	}

	protected VariableNode declarationToImplicitVariable(Declaration decl) throws TranslationException {
		Object var = processExpression(decl);
		if (var instanceof VariableNode) {
			return (VariableNode) var;
		}
		return null;
	}

	protected boolean containsDeclaration(Object obj) {
		if (obj instanceof BuiltinElement) {
			Iterator<Node> argitr = ((BuiltinElement) obj).getArguments().iterator();
			while (argitr.hasNext()) {
				Node n = argitr.next();
				if (n instanceof ProxyNode) {
					if (containsDeclaration(((ProxyNode) n).getProxyFor())) {
						return true;
					}
				}
			}
		} else if (obj instanceof TripleElement) {
			Node s = ((TripleElement) obj).getSubject();
			if (s instanceof NamedNode && ((NamedNode) s).getNodeType().equals(NodeType.ClassNode)) {
				return true;
			}
			if (containsDeclaration(((TripleElement) obj).getSubject())) {
				return true;
			}
		} else if (obj instanceof ProxyNode) {
			if (containsDeclaration(((ProxyNode) obj).getProxyFor())) {
				return true;
			}
		}
		return false;
	}

	protected Object processFunction(Name expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		EList<Expression> arglist = expr.getArglist();
		Node fnnode = processExpression(expr.getName());
		BuiltinElement builtin = new BuiltinElement();
		if (fnnode instanceof NamedNode) {
			builtin.setFuncName(((NamedNode) fnnode).getName());
			builtin.setFuncPrefix(((NamedNode)fnnode).getPrefix());
			builtin.setFuncUri(((NamedNode)fnnode).toFullyQualifiedString());
		} else if (fnnode == null) {
			addError("Function not found", expr);
			return null;
		} else {
			builtin.setFuncName(fnnode.toString());
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
						((GraphPatternElement) arg).setEmbedded(true);
					}
				}
			}
		}
		return builtin;
	}

	private boolean hasCommonVariableSubject(Object robj) {
		if (robj instanceof TripleElement && (((TripleElement) robj).getSubject() instanceof VariableNode
				&& (((TripleElement) robj).getSourceType().equals(TripleSourceType.SPV))
				|| ((TripleElement) robj).getSourceType().equals(TripleSourceType.ITC))) {
			VariableNode subjvar = (VariableNode) ((TripleElement) robj).getSubject();
			Object trel = robj;
			while (trel != null && trel instanceof TripleElement) {
				if (!(trel instanceof TripleElement) || (((TripleElement) trel).getSubject() != null
						&& !(((TripleElement) trel).getSubject().equals(subjvar)))) {
					return false;
				}
				trel = ((TripleElement) trel).getNext();
			}
			if (trel == null) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the bottom triple whose subject was replaced.
	 * 
	 * @param pattern
	 * @param proxyFor
	 * @param assignedNode
	 * @return
	 */
	private TripleElement assignNullSubjectInProxies(TripleElement pattern, TripleElement proxyFor, Node assignedNode) {
		if (pattern.getSubject() instanceof ProxyNode) {
			Object proxy = ((ProxyNode) pattern.getSubject()).getProxyFor();
			if (proxy instanceof TripleElement) {
				// ((ProxyNode)pattern.getSubject()).setReplacementNode(assignedNode);
				if (((TripleElement) proxy).getSubject() == null) {
					// this is the bottom of the recursion
					((TripleElement) proxy).setSubject(assignedNode);
					return (TripleElement) proxy;
				} else {
					// recurse down
					TripleElement bottom = assignNullSubjectInProxies(((TripleElement) proxy), proxyFor, assignedNode);
					// make the proxy next and reassign this subject as assignedNode
					((ProxyNode) ((TripleElement) proxy).getSubject()).setReplacementNode(assignedNode);
					((TripleElement) proxy).setSubject(assignedNode);
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
			Object proxy = ((ProxyNode) pattern.getSubject()).getProxyFor();
			if (proxy instanceof TripleElement) {
				if (((TripleElement) proxy).getSubject() == null) {
					return (TripleElement) proxy;
				} else {
					return getProxyWithNullSubject(((TripleElement) proxy));
				}
			}
		}
		return null;
	}

	private boolean isComparisonViaBuiltin(Object robj, Object lobj) {
		if (robj instanceof TripleElement && lobj instanceof Node
				&& ((TripleElement) robj).getNext() instanceof BuiltinElement) {
			BuiltinElement be = (BuiltinElement) ((TripleElement) robj).getNext();
			if (isComparisonBuiltin(be.getFuncName()) && be.getArguments().size() == 1) {
				return true;
			}
		}
		return false;
	}

	private boolean isModifiedTripleViaBuitin(Object robj) {
		if (robj instanceof TripleElement && ((TripleElement) robj).getNext() instanceof BuiltinElement) {
			BuiltinElement be = (BuiltinElement) ((TripleElement) robj).getNext();
			if (((TripleElement) robj).getPredicate() instanceof RDFTypeNode) {
				if (isModifiedTriple(be.getFuncType())) {
					Node subj = ((TripleElement) robj).getSubject();
					Node arg = (be.getArguments() != null && be.getArguments().size() > 0) ? be.getArguments().get(0)
							: null;
					if (subj == null && arg == null) {
						return true;
					}
					if (subj != null && arg != null && subj.equals(arg)) {
						return true;
					}
				}
			} else {
				if (isModifiedTriple(be.getFuncType())
						&& ((TripleElement) robj).getObject().equals(be.getArguments().get(0))) {
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
				if (((TripleElement) gpe).getSubject().equals(v1)) {
					((TripleElement) gpe).setSubject(v2);
					retval = true;
				} else if (((TripleElement) gpe).getObject().equals(v1)) {
					((TripleElement) gpe).setObject(v2);
					retval = true;
				}
			} else if (gpe instanceof BuiltinElement) {
				List<Node> args = ((BuiltinElement) gpe).getArguments();
				for (int j = 0; j < args.size(); j++) {
					if (args.get(j).equals(v1)) {
						args.set(j, v2);
						retval = true;
					}
				}
			} else if (gpe instanceof Junction) {
				logger.error("Not yet handled");
			}
			gpe = gpe.getNext();
		} while (gpe != null);
		return retval;
	}

	/**
	 * This method returns true if the argument node is bound in some other element
	 * of the rule
	 * 
	 * @param rule
	 * @param gpe
	 * @param v
	 * @return
	 */
	public static boolean variableIsBound(Rule rule, GraphPatternElement gpe, Node v) {
		if (v instanceof NamedNode) {
			if (((NamedNode) v).getNodeType() != null
					&& !(((NamedNode) v).getNodeType().equals(NodeType.VariableNode))) {
				return true;
			}
		}
		// Variable is bound if it appears in a triple or as the return argument of a
		// built-in
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

	private GraphPatternElement createBinaryBuiltin(String name, Object lobj, Object robj)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		if (name.equals(JunctionType.AND_ALPHA) || name.equals(JunctionType.AND_SYMBOL)
				|| name.equals(JunctionType.OR_ALPHA) || name.equals(JunctionType.OR_SYMBOL)) {
			Junction jct = new Junction();
			jct.setJunctionName(name);
			jct.setLhs(nodeCheck(lobj));
			jct.setRhs(nodeCheck(robj));
			return jct;
		} else {
			BuiltinElement builtin = new BuiltinElement();
			builtin.setFuncName(transformOpName(name));
			if (lobj != null) {
				builtin.addArgument(nodeCheck(lobj));
			}
			if (robj != null) {
				builtin.addArgument(nodeCheck(robj));
			}
			return builtin;
		}
	}

	protected Junction createJunction(Expression expr, String name, Object lobj, Object robj) throws InvalidNameException, InvalidTypeException, TranslationException {
		Junction junction = new Junction();
		junction.setJunctionName(name);
		junction.setLhs(nodeCheck(lobj));
		junction.setRhs(nodeCheck(robj));
		return junction;
	}

	public TripleModifierType getTripleModifierType(BuiltinType btype) {
		if (btype.equals(BuiltinType.Not) || btype.equals(BuiltinType.NotEqual)) {
			return TripleModifierType.Not;
		} else if (btype.equals(BuiltinType.Only)) {
			return TripleModifierType.Only;
		} else if (btype.equals(BuiltinType.NotOnly)) {
			return TripleModifierType.NotOnly;
		}
		return null;
	}

	public Object processExpression(BooleanLiteral expr) {
		Object lit = super.processExpression(expr);
		return lit;
	}

	public Node processExpression(Constant expr) throws InvalidNameException {
		// System.out.println("processing " + expr.getClass().getCanonicalName() + ": "
		// + expr.getConstant());
		if (expr.getConstant().equals("known")) {
			return new ConstantNode("known");
		}
		if (expr.getConstant().equals(SadlConstants.CONSTANT_NONE)) {
			return new ConstantNode(SadlConstants.CONSTANT_NONE);
		}
		return new ConstantNode(expr.getConstant());
	}

	public Object processExpression(Declaration expr) throws TranslationException {
		// String nn = expr.getNewName();
		SadlTypeReference type = expr.getType();
		String article = expr.getArticle();
		String ordinal = expr.getOrdinal();
		Object typenode = processExpression(type);
		if (article != null && isInstance(typenode)) {
			addError("An article (e.g., '" + article
					+ "') should not be used in front of the name of an instance of a class.", expr);
		} else if (article != null && isVariable(typenode)
				&& EcoreUtil2.getContainerOfType(expr, QueryStatement.class) == null) {
			addError("An article (e.g., '" + article + "') should not be used in front of the name of a variable.",
					expr);
		} else if (article != null && !isProperty(typenode) && !isDefinitionOfExplicitVariable(expr)) {
			// article should never be null, otherwise it wouldn't be a declaration
			int ordNum = 1;
			if (ordinal != null) {
				ordNum = getOrdinalNumber(ordinal);
			} else if (article.equals("another")) {
				ordNum = 2;
			}

			if (isUseArticlesInValidation() && !isDefiniteArticle(article) && typenode instanceof NamedNode
					&& (((NamedNode) typenode).getNodeType().equals(NodeType.ClassNode)
							|| ((NamedNode) typenode).getNodeType().equals(NodeType.ClassListNode))) {
				if (!isCruleVariableDefinitionPossible(expr)) {
					if (ordinal != null) {
						addError("Did not expect an indefinite article reference with ordinality in rule conclusion",
								expr);
					}
					return typenode;
				}

				// create a CRule variable
				String nvar = getNewVar(expr);
				VariableNode var = addCruleVariable((NamedNode) typenode, ordNum, nvar, expr, getHostEObject());
				// System.out.println("Added crule variable: " + typenode.toString() + ", " +
				// ordNum + ", " + var.toString());
				return var;
			} else if (typenode != null && typenode instanceof NamedNode) {
				if (isList((NamedNode) typenode, type)) {
					((NamedNode) typenode).setList(true);
					if (expr.getLen() != null) {
						if (expr.getMaxlen() != null) {
							((NamedNode) typenode).setMinListLength(Integer.parseInt(expr.getLen()));
							if (!expr.getMaxlen().equals("*")) {
								((NamedNode) typenode).setMaxListLength(Integer.parseInt(expr.getMaxlen()));
							}
						} else {
							((NamedNode) typenode).setListLength(Integer.parseInt(expr.getLen()));
						}
					}
				}
				VariableNode var = null;
				if (isUseArticlesInValidation()) {
					var = getCruleVariable((NamedNode) typenode, ordNum);
				} else {
					var = getVariableInDefinition(expr);
					try {
						if (var == null) {
							String nvar = getNewVar(expr);
							var = createVariable(nvar, expr);
						}
						setVarType(var, (NamedNode) typenode, isList((NamedNode) typenode, type), expr);
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (PrefixNotFoundException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (InvalidNameException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (InvalidTypeException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (ConfigurationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				if (var == null) {
					addError("Did not find crule variable for type '" + ((NamedNode) typenode).toString()
							+ "', definite article, ordinal " + ordNum, expr);
				} else {
					// System.out.println("Retrieved crule variable: " + typenode.toString() + ", "
					// + ordNum + ", " + var.toString());
					return var;
				}
			} else {
				addError("No type identified", expr);
			}
		} else if (isUseArticlesInValidation() && article == null) {
			if (isClass(typenode)) {
				addError(
						"A class name should be preceded by either an indefinite (e.g., 'a' or 'an') or a definite (e.g., 'the') article.",
						expr);
			}
		}
		return typenode;
	}

	protected EObject getHostEObject() {
		return hostEObject;
	}

	protected void setHostEObject(EObject host) {
		if (hostEObject != null) {
			clearCruleVariablesForHostObject(hostEObject);
		}
		hostEObject = host;
	}

	public Object processExpression(ElementInList expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		// create a builtin for this
		if (expr.getElement() != null) {
			if (expr.getElement() instanceof PropOfSubject) {
				Expression predicate = ((PropOfSubject) expr.getElement()).getLeft();
				Expression subject = ((PropOfSubject) expr.getElement()).getRight();
				Object lst = processExpression(subject);
				Object element = processExpression(predicate);
				BuiltinElement bi = new BuiltinElement();
				if (expr.isBefore()) {
					bi.setFuncName("elementBefore");
				} else if (expr.isAfter()) {
					bi.setFuncName("elementAfter");
				} else {
					bi.setFuncName("elementInList");
				}
				bi.addArgument(nodeCheck(lst));
				bi.addArgument(nodeCheck(element));
				return bi;
			} else {
				return processExpression(expr.getElement());
			}
		}
		return null;
	}

	private boolean isVariable(Object node) {
		if (node instanceof NamedNode) {
			if (((NamedNode) node).getNodeType() != null
					&& ((NamedNode) node).getNodeType().equals(NodeType.VariableNode)) {
				return true;
			}
		}
		return false;
	}

	private boolean isClass(Object node) {
		if (node instanceof NamedNode) {
			if (((NamedNode) node).getNodeType() != null
					&& ((NamedNode) node).getNodeType().equals(NodeType.ClassNode)) {
				return true;
			}
		}
		return false;
	}

	private boolean isInstance(Object node) {
		if (node instanceof NamedNode) {
			if (((NamedNode) node).getNodeType() != null
					&& ((NamedNode) node).getNodeType().equals(NodeType.InstanceNode)) {
				return true;
			}
		}
		return false;
	}

	public boolean isProperty(Object node) {
		if (node instanceof NamedNode) {
			return isProperty(((NamedNode) node).getNodeType());
		}
		return false;
	}

	private boolean isCruleVariableDefinitionPossible(Declaration expr) {
		if (getRulePart().equals(RulePart.CONCLUSION) && !isDeclInThereExists(expr)) {
			// this can't be a crule variable unless there is no rule body
			if (getTarget() != null && getTarget() instanceof Rule
					&& (((Rule) getTarget()).getIfs() != null || ((Rule) getTarget()).getGivens() != null)) {
				return false;
			}
		}
		if (expr.eContainer() != null && expr.eContainer() instanceof BinaryOperation
				&& isEqualOperator(((BinaryOperation) expr.eContainer()).getOp())
				&& !((BinaryOperation) expr.eContainer()).getLeft().equals(expr)
				&& ((BinaryOperation) expr.eContainer()).getLeft() instanceof Declaration) {
			return false;
		}
		return true;
	}

	private boolean isDefinitionOfExplicitVariable(Declaration expr) {
		EObject cont = expr.eContainer();
		try {
			if (cont instanceof BinaryOperation && ((BinaryOperation) cont).getLeft() instanceof SadlResource
					&& getDeclarationExtensions().getOntConceptType((SadlResource) ((BinaryOperation) cont).getLeft())
							.equals(OntConceptType.VARIABLE)) {
				return true;
			}
		} catch (CircularDefinitionException e) {
			addError(e.getMessage(), expr);
		}
		return false;
	}

	private int getOrdinalNumber(String ordinal) throws TranslationException {
		if (ordinal == null) {
			throw new TranslationException("Unexpected null ordinal on call to getOrdinalNumber");
		}
		if (ordinal.equals("first"))
			return 1;
		if (ordinal.equals("second") || ordinal.equals("other"))
			return 2;
		if (ordinal.equals("third"))
			return 3;
		if (ordinal.equals("fourth"))
			return 4;
		if (ordinal.equals("fifth"))
			return 5;
		if (ordinal.equals("sixth"))
			return 6;
		if (ordinal.equals("seventh"))
			return 7;
		if (ordinal.equals("eighth"))
			return 8;
		if (ordinal.equals("ninth"))
			return 9;
		if (ordinal.equals("tenth"))
			return 10;
		throw new TranslationException("Unexpected ordinal '" + ordinal + "'; can't handle.");
	}

	private String nextOrdinal(int ordinalNumber) throws TranslationException {
		if (ordinalNumber == 0)
			return "first";
		if (ordinalNumber == 1)
			return "second";
		if (ordinalNumber == 2)
			return "third";
		if (ordinalNumber == 3)
			return "fourth";
		if (ordinalNumber == 4)
			return "fifth";
		if (ordinalNumber == 5)
			return "sixth";
		if (ordinalNumber == 6)
			return "seventh";
		if (ordinalNumber == 7)
			return "eighth";
		if (ordinalNumber == 8)
			return "ninth";
		if (ordinalNumber == 9)
			return "tenth";
		throw new TranslationException(
				"Unexpected ordinal number '" + ordinalNumber + "'is larger than is handled at this time.");
	}

	private boolean isDefiniteArticle(String article) {
		if (article != null && article.equalsIgnoreCase("the")) {
			return true;
		}
		return false;
	}

	private Object processExpression(SadlTypeReference type) throws TranslationException {

		if (type instanceof SadlSimpleTypeReference) {
			Object typeobj = processExpression(((SadlSimpleTypeReference) type).getType());
			if (((SadlSimpleTypeReference) type).isList()) {
				if (typeobj instanceof NamedNode) {
					((NamedNode) typeobj).setList(true);
				} else {
                    logger.error("Error:", new TranslationException("Unhandled case: type returned from simple type reference is not type NamedNode"));
                    return null;
				}
			}
			return typeobj;
		} else if (type instanceof SadlPrimitiveDataType) {
			SadlDataType pt = ((SadlPrimitiveDataType) type).getPrimitiveType();
			Object typeobj = sadlDataTypeToNamedNode(pt);
				
			// Show an warning if primitive type is not compatible with downstream projects
			if(!isSupportedByDownstreamProjects(pt)) {
				issueAcceptor.addWarning(SadlErrorMessages.TYPE_UNSUPPORTED_DOWNSTREAM.get(pt.getLiteral()), type);
			}	
			
			if (((SadlPrimitiveDataType) type).isList()) {
				if (typeobj instanceof NamedNode) {
					((NamedNode) typeobj).setList(true);
				} else {
					throw new TranslationException("Unhandled case: type returned from primitive data type List is '" + typeobj.getClass().getCanonicalName() + "'");
				}
			}
			return typeobj;
		} else if (type instanceof SadlUnionType) {
			try {
				Object unionObj = sadlTypeReferenceToObject(type);
				return unionObj;
			} catch (JenaProcessorException e) {
				throw new TranslationException("Error processing union", e);
			}
		} else {
			throw new TranslationException(
					"Unhandled type of SadlTypeReference: " + type.getClass().getCanonicalName());
		}
	}

	private Object sadlDataTypeToNamedNode(SadlDataType pt) {
		/*
		 * string | boolean | decimal | int | long | float | double | duration |
		 * dateTime | time | date | gYearMonth | gYear | gMonthDay | gDay | gMonth |
		 * hexBinary | base64Binary | anyURI | integer | negativeInteger |
		 * nonNegativeInteger | positiveInteger | nonPositiveInteger | byte |
		 * unsignedByte | unsignedInt | anySimpleType;
		 */

		String typeStr = pt.getLiteral();
		if (typeStr.equals("string")) {
			return validateNamedNode(new NamedNode(XSD.xstring.getURI(), NodeType.DataTypeNode));
		}
		if (typeStr.equals("boolean")) {
			return validateNamedNode(new NamedNode(XSD.xboolean.getURI(), NodeType.DataTypeNode));
		}
		if (typeStr.equals("byte")) {
			return validateNamedNode(new NamedNode(XSD.xbyte.getURI(), NodeType.DataTypeNode));
		}
		if (typeStr.equals("int")) {
			return validateNamedNode(new NamedNode(XSD.xint.getURI(), NodeType.DataTypeNode));
		}
		if (typeStr.equals("long")) {
			return validateNamedNode(new NamedNode(XSD.xlong.getURI(), NodeType.DataTypeNode));
		}
		if (typeStr.equals("float")) {
			return validateNamedNode(new NamedNode(XSD.xfloat.getURI(), NodeType.DataTypeNode));
		}
		if (typeStr.equals("double")) {
			return validateNamedNode(new NamedNode(XSD.xdouble.getURI(), NodeType.DataTypeNode));
		}
		if (typeStr.equals("short")) {
			return validateNamedNode(new NamedNode(XSD.xshort.getURI(), NodeType.DataTypeNode));
		}
		return validateNamedNode(new NamedNode(XSD.getURI() + "#" + typeStr, NodeType.DataTypeNode));
	}
	
	/**
	 * Checks an incoming primitive type from .sreq files and displays
	 * a warning within the editor on that primitive type if it is not supported by downstream projects
	 * such as RAE and XLR. Legals types are boolean, decimal, int/integer, float, double, enum, list, anything
	 * else is illegal.
	 * 
	 * @param primitiveType The type to check
	 * @return
	 */
	private boolean isSupportedByDownstreamProjects(SadlDataType primitiveType) {
		
		// get the current translator for the resource
		ITranslator translator = null;
		try {
			translator = getConfigMgr(getCurrentResource(), null).getTranslator();
		} catch (ConfigurationException e) {
			e.printStackTrace();
			return true;
		}

		// check the supported data types of the translator against the primitive type
		if(translator != null) {
			int numDataTypes = translator.getSupportedDataTypes().size();
			
			/* if no specific data types have been declared supported we assume all 
			 types are supported, waiting on supported data types to be implemented
			 by responsible parties */
			if(numDataTypes == 0) {
				return true;
			}
			
			String typeStr = primitiveType.getLiteral();
			for(int i = 0; i < numDataTypes; i++){
				if(typeStr.equals(translator.getSupportedDataTypes().get(i))) {
					return true;
				}
			}
			// primitive type is not supported by the translator
			return false;
		}
		// no available translator so don't show a warning
		return true;
			
	}
	

	public Object processExpression(Name expr) throws TranslationException, InvalidNameException, InvalidTypeException {
		if (expr.isFunction()) {
			return processFunction(expr);
		}
		SadlResource qnm = expr.getName();
		String nm = getDeclarationExtensions().getConcreteName(qnm);
		if (nm == null) {
			SadlResource srnm = qnm.getName();
			if (srnm != null) {
				return processExpression(srnm);
			}
			addError(SadlErrorMessages.TRANSLATE_NAME_SADLRESOURCE.toString(), expr);
			// throw new InvalidNameException("Unable to resolve SadlResource to a name");
		} else if (qnm.equals(expr) && expr.eContainer() instanceof BinaryOperation
				&& ((BinaryOperation) expr.eContainer()).getRight() != null
				&& ((BinaryOperation) expr.eContainer()).getRight().equals(qnm)) {
			addError("It appears that '" + nm + "' is not defined.", expr);
		}
			
		  else if (qnm.equals(expr) && expr.eContainer() instanceof BinaryOperation
				      && ((BinaryOperation) expr.eContainer()).getLeft() != null
				    && ((BinaryOperation) expr.eContainer()).getLeft().equals(qnm)) {
				    addError("It appears that '" + nm + "' is not defined.", expr);
				  }
			
			
		 else {
			return processExpression(qnm);
		}
		return null;
	}

	public Object processExpression(NumberLiteral expr) {
		Object lit = super.processExpression(expr);
		return lit;
	}

	public Object processExpression(StringLiteral expr) {
		return super.processExpression(expr);
	}

	public Object processExpression(PropOfSubject expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		Expression predicate = expr.getLeft();
		if (predicate == null) {
			addError("Predicate in expression is null. Are parentheses needed?", expr);
			return null;
		}
		Expression subject = expr.getRight();
		Object trSubj = null;
		Object trPred = null;
		Node subjNode = null;
		Node predNode = null;
		String constantBuiltinName = null;
		int numBuiltinArgs = 0;
		if (predicate instanceof Constant) {
			// this is a pseudo PropOfSubject; the predicate is a constant
			String cnstval = ((Constant) predicate).getConstant();
			predicate = null;
			if (cnstval.equals("length") || cnstval.equals("the length")) {
				constantBuiltinName = "length";
				numBuiltinArgs = 1;
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject) subject).getLeft();
					subject = ((PropOfSubject) subject).getRight();
				}
			} else if (cnstval.equals("count")) {
				constantBuiltinName = cnstval;
				numBuiltinArgs = 2;
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject) subject).getLeft();
					subject = ((PropOfSubject) subject).getRight();
				}
			} else if (cnstval.endsWith("index")) {
				constantBuiltinName = cnstval;
				numBuiltinArgs = 2;
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject) subject).getLeft();
					subject = ((PropOfSubject) subject).getRight();
				}
			} else if (cnstval.equals("first element")) {
				constantBuiltinName = "firstElement";
				numBuiltinArgs = 1;
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject) subject).getLeft();
					subject = ((PropOfSubject) subject).getRight();
				}
			} else if (cnstval.equals("last element")) {
				constantBuiltinName = "lastElement";
				numBuiltinArgs = 1;
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject) subject).getLeft();
					subject = ((PropOfSubject) subject).getRight();
				}
			} else if (cnstval.endsWith("element")) {
				constantBuiltinName = cnstval;
				numBuiltinArgs = 2;
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject) subject).getLeft();
					subject = ((PropOfSubject) subject).getRight();
				}
			} else {
				System.err.println("Unhandled constant property in translate PropOfSubj: " + cnstval);
			}
		} else if (predicate instanceof ElementInList) {
			trSubj = processExpression(subject);
//			trPred = processExpression(predicate);
			Expression element = ((ElementInList)predicate).getElement();
			Object elObj = processExpression(element);
			BuiltinElement bi = new BuiltinElement();
			if (((ElementInList) predicate).isBefore()) {
				bi.setFuncName("elementBefore");
			} else if (((ElementInList) predicate).isAfter()) {
				bi.setFuncName("elementAfter");
			} else {
				bi.setFuncName("elementInList");
			}
			bi.addArgument(nodeCheck(trSubj));
			bi.addArgument(nodeCheck(elObj));
			return bi;

		}

		Object rest = null;
		if (subject != null) {
			trSubj = processExpression(subject);
			if (trSubj instanceof Object[] && ((Object[]) trSubj).length == 2) {
				rest = ((Object[]) trSubj)[1];
				trSubj = ((Object[]) trSubj)[0];
			}
			if (isUseArticlesInValidation() && subject instanceof Name && trSubj instanceof NamedNode
					&& ((NamedNode) trSubj).getNodeType().equals(NodeType.ClassNode)) {
				// we have a class in a PropOfSubject that does not have an article (otherwise
				// it would have been a Declaration)
				addError(SadlErrorMessages.NEEDS_ARTICLE.get(),subject);
			}
		}
		boolean isPreviousPredicate = false;
		if (predicate != null) {
			trPred = processExpression(predicate);

			// Check for cardinality of property on this particular class hierarchy
			if (constantBuiltinName == null) {
				if (Boolean.parseBoolean(getProcessorContext().getPreferenceValues()
						.getPreference(SadlPreferences.CHECK_FOR_CARDINALITY_OF_PROPERTY_IN_DOMAIN))) {
					checkForExistenceOfCardinality(predicate, subject);
				}
			}
		}
		if (trPred != null && (constantBuiltinName == null || numBuiltinArgs == 1)) {
			TripleElement returnTriple = null;
			if (trPred instanceof Node) {
				predNode = (Node) trPred;
			} else {
				predNode = new ProxyNode(trPred);
			}
			if (trSubj instanceof Node) {
				subjNode = (Node) trSubj;
			} else if (trSubj != null) {
				subjNode = new ProxyNode(trSubj);
			}
			if (predNode != null && predNode instanceof Node) {
				returnTriple = new TripleElement(subjNode, predNode, null);
				returnTriple.setSourceType(TripleSourceType.PSV);
				if (constantBuiltinName == null) {
					return combineRest(returnTriple, rest);
				}
			}
			if (numBuiltinArgs == 1) {
				Object bi = createUnaryBuiltin(expr, constantBuiltinName, new ProxyNode(returnTriple));
				return combineRest(bi, rest);
			} else {
				predNode = new RDFTypeNode();
				Node variable = getVariableNode(expr, null, predNode, subjNode);
				returnTriple = new TripleElement();
				returnTriple.setSubject(variable);
				returnTriple.setPredicate(predNode);
				returnTriple.setObject(subjNode);
				if (subjNode instanceof NamedNode && !((NamedNode) subjNode).getNodeType().equals(NodeType.ClassNode)) {
					addError(SadlErrorMessages.IS_NOT_A.get(subjNode.toString(), "class"), subject);
				}
				returnTriple.setSourceType(TripleSourceType.ITC);
				return combineRest(returnTriple, rest);
			}
		} else { // none of these create more than 2 arguments
			GraphPatternElement bi = null;
			if (numBuiltinArgs == 1) {
				bi = new BuiltinElement();
				((BuiltinElement) bi).setFuncName(constantBuiltinName);
				((BuiltinElement) bi).addArgument(nodeCheck(trSubj));
			} else {
				bi = createBinaryBuiltin(constantBuiltinName, nodeCheck(trSubj), trPred);
			}
			return combineRest(bi, rest);
		}
	}

	private void checkForExistenceOfCardinality(Expression property, Expression subject)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		if (property == null || subject == null) {
			return;
		}

		if (!(property instanceof SadlResource) || !(subject instanceof SadlResource)) {
			return;
		}

		TypeCheckInfo tci = null;
		try {
			tci = getModelValidator().getType((SadlResource) subject);
		} catch (CircularDependencyException | DontTypeCheckException | CircularDefinitionException | URISyntaxException
				| IOException | ConfigurationException | PropertyWithoutRangeException e) {
			e.printStackTrace();
		}
		if (tci == null) {
			return;
		}
		List<OntClass> subjClasses = getModelValidator().getTypeCheckTypeClasses(tci);

		String propURI = getDeclarationExtensions().getConceptUri((SadlResource) property);
		Property prop = getTheJenaModel().getProperty(propURI);
		StmtIterator sitr = getTheJenaModel().listStatements(null, OWL.onProperty, prop);
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			com.hp.hpl.jena.rdf.model.Resource sr = stmt.getSubject();
			for (OntClass subjClass : subjClasses) {
				if (checkForCardinalityExistence(sr, subjClass, prop, (SadlResource) property)) {
					return;
				}

				if (checkForSubClassCardinalityExistence(sr, subjClass, prop, (SadlResource) property)) {
					return;
				}
			}
		}
		String propName = getDeclarationExtensions().getConcreteName((SadlResource) property);
		String domName = getDeclarationExtensions().getConcreteName((SadlResource) subject);
		addWarning("This property (" + propName + ") has no cardinality in this domain (" + domName + ")", property);
	}

	private boolean checkForSubClassCardinalityExistence(com.hp.hpl.jena.rdf.model.Resource sr, OntClass subjClass,
			Property prop, SadlResource propResource) throws InvalidTypeException {
		ExtendedIterator<OntClass> ei = subjClass.listSubClasses();
		while (ei.hasNext()) {
			OntClass subClass = ei.next();
			if (checkForCardinalityExistence(sr, subClass, prop, propResource)) {
				return true;
			}
			if (checkForSubClassCardinalityExistence(sr, subClass, prop, propResource)) {
				return true;
			}
		}

		return false;
	}

	private boolean checkForCardinalityExistence(com.hp.hpl.jena.rdf.model.Resource sr, OntClass subjClass,
			Property prop, SadlResource propResource) throws InvalidTypeException {
		if (sr.canAs(OntClass.class) && subjClass.as(OntClass.class).hasSuperClass(sr.as(OntClass.class))) {
			Restriction restriction = sr.as(OntClass.class).asRestriction();

			if (restriction.isMinCardinalityRestriction()) {
				MinCardinalityRestriction cr = restriction.asMinCardinalityRestriction();
				if (cr.getOnProperty().equals(prop)) {
					return true;
				}
			} else if (restriction.isCardinalityRestriction()) {
				CardinalityRestriction cr = restriction.asCardinalityRestriction();
				if (cr.getOnProperty().equals(prop)) {
					return true;
				}
			} else if (restriction.isMaxCardinalityRestriction()) {
				MaxCardinalityRestriction cr = restriction.asMaxCardinalityRestriction();
				if (cr.getOnProperty().equals(prop)) {
					return true;
				}
			}
		}

		return false;
	}

	private Object getSubjectOfFirstTriple(Object stuff) {
		if (stuff instanceof TripleElement) {
			return ((TripleElement) stuff).getSubject();
		} else if (stuff instanceof Junction && ((Junction) stuff).getJunctionType().equals(JunctionType.Conj)) {
			return getSubjectOfFirstTriple(((Junction) stuff).getLhs());
		}
		return null;
	}

	/**
	 * Method to take an immediate result (gpe) and add the previous results (rest)
	 * into an array to be returned
	 * 
	 * @param gpe
	 * @param rest
	 * @return
	 * @throws InvalidNameException
	 * @throws InvalidTypeException
	 * @throws TranslationException
	 */
	private Object combineRest(Object gpe, Object rest)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		if (rest == null) {
			return gpe;
		}
		if (gpe instanceof Node) {
			Object[] result = new Object[2];
			result[0] = gpe;
			if (rest instanceof List<?>) {
				if (((List<?>)rest).size() == 1) {
					rest = ((List<?>)rest).get(0);
				}
				else {
					rest = listToSingleJunction(rest);
				}
			}
			result[1] = rest;
			return result;
		}
		if (gpe instanceof TripleElement && ((TripleElement) gpe).getObject() == null) {
			Object[] result = new Object[2];
			result[0] = gpe;
			result[1] = rest;
			return result;
		}
		Junction jct = new Junction();
		jct.setJunctionName("and");
		jct.setLhs(nodeCheck(gpe));
		if (rest instanceof List<?>) {
			jct.setRhs(nodeCheck(listToSingleJunction((List<GraphPatternElement>) rest)));
		} else {
			jct.setRhs(nodeCheck(rest));
		}
		return jct;
	}

	protected Object postProcessTranslationResult(Object result) throws TranslationException, InvalidNameException, InvalidTypeException {
		if (result instanceof Object[] && ((Object[]) result).length == 2) {
			if (((Object[]) result)[0] instanceof NamedNode) {
				result = ((Object[]) result)[1];
				if (result instanceof List<?>) {
					result = listToSingleJunction(result);
				}
			} else if (((Object[]) result)[0] instanceof ProxyNode) {
				// this node came from rest, return the rest
				return listToSingleJunction(((Object[]) result)[1]);
			}
		}
		if (result instanceof GraphPatternElement) {
			return (GraphPatternElement) result;
		} else if (result instanceof Query) {
			return result;
		} else if (result instanceof Node) {
			return result;
		} else if (result instanceof List<?>) {
			return listToSingleJunction(result);
		} else if (result != null) {
			// can be null when processing partial expressions
			throw new TranslationException("Unexpected result in postProcessTranslationResult");
		}
		return result;
	}

	private Object listToSingleJunction(Object result) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (result instanceof List<?>) {
			if (((List<?>) result).size() == 1) {
				result = ((List<?>) result).get(0);
			} else {
				result = ((List<?>) getIfTranslator().listToAnd((List<GraphPatternElement>) result)).get(0);
			}
		}
		return result;
	}

	public Node processExpression(SadlResource expr) throws TranslationException {
		String nm = getDeclarationExtensions().getConcreteName(expr);
		String ns = getDeclarationExtensions().getConceptNamespace(expr);
		String prfx = getDeclarationExtensions().getConceptPrefix(expr);
		OntConceptType type;
		try {
			type = getDeclarationExtensions().getOntConceptType(expr);
		} catch (CircularDefinitionException e) {
			type = e.getDefinitionType();
			addError(e.getMessage(), expr);
		}
		if (type.equals(OntConceptType.VARIABLE) && nm != null) {
			VariableNode vn = null;
			try {
				vn = createVariable(expr);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (PrefixNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvalidNameException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvalidTypeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			return vn;
		} else if (nm != null) {
			NamedNode n = new NamedNode(nm, ontConceptTypeToNodeType(type));
			n.setNamespace(ns);
			n.setPrefix(prfx);
			return n;
		}
		return null;
	}

	protected Object processSubjHasPropUnitExpression(SubjHasProp expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		Expression valexpr = expr.getLeft();
		Object valarg = processExpression(valexpr);
		if (ignoreUnittedQuantities) {
			return valarg;
		}
		String unit = SadlASTUtils.getUnitAsString(expr);
		if (valarg instanceof com.ge.research.sadl.model.gp.Literal) {
			((com.ge.research.sadl.model.gp.Literal) valarg).setUnits(unit);
			return valarg;
		}
		com.ge.research.sadl.model.gp.Literal unitLiteral = new com.ge.research.sadl.model.gp.Literal();
		unitLiteral.setValue(unit);
		return createBinaryBuiltin("unittedQuantity", valarg, unitLiteral);
	}

	public Object processExpression(SubjHasProp expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		// System.out.println("processing " + expr.getClass().getCanonicalName() + ": "
		// + expr.getProp().toString());
		if (expr.isComma() && expr.getRight() == null) {
			addError("This is an invalid subject-has-property construct using a comma", expr);
			return null;
		}
		Expression subj = expr.getLeft();
		SadlResource pred = expr.getProp();
		Expression obj = expr.getRight();
		List<GraphPatternElement> shpTriples = new ArrayList<GraphPatternElement>();
		shpTriples = processSubjHasProp(subj, pred, obj, shpTriples, expr);

		if (shpTriples.size() > 0 && shpTriples.get(0) instanceof TripleElement) {
			return combineRest(((TripleElement) shpTriples.get(0)).getSubject(), shpTriples);
		}
		return shpTriples;
	}

	private List<GraphPatternElement> processSubjHasProp(Expression subj, SadlResource pred, Expression obj,
			List<GraphPatternElement> shpTriples, Expression expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		// Create a triple for this SubjHasProp and put it in the list (only has
		// predicate at this point)
		Object predObj = processExpression(pred);
		TripleElement tr = new TripleElement();
		tr.setPredicate(nodeCheck(predObj));

		// check for negation
		boolean negatedSubject = subj instanceof UnaryExpression && ((UnaryExpression) subj).getOp().equals("not");
		if (negatedSubject) {
			subj = ((UnaryExpression) subj).getExpr();
		}
		boolean negatedObject = obj instanceof UnaryExpression && ((UnaryExpression) obj).getOp().equals("not");
		if (negatedObject) {
			obj = ((UnaryExpression) obj).getExpr();
		}
		if (negatedObject || negatedSubject) {
			tr.setType(TripleModifierType.Not);
		}

		if (subj instanceof Declaration || subj instanceof Name) {
			tr.setSubject(nodeCheck(processExpression(subj)));
			shpTriples.add(tr);
		}
		if (obj instanceof Declaration || obj instanceof Name) {
			tr.setObject(nodeCheck(processExpression(obj)));
		}
		if (tr.getSubject() == null) {
			boolean subjectFound = false;
			if (subj instanceof SubjHasProp) {
				int preCallListSize = shpTriples.size();
				shpTriples = processSubjHasProp(((SubjHasProp) subj).getLeft(), ((SubjHasProp) subj).getProp(),
						((SubjHasProp) subj).getRight(), shpTriples, subj);
				// must get subject of current triple; should be the subject of the first triple
				// added by call on previous line
				if (shpTriples.size() > preCallListSize) {
					if (shpTriples.get(preCallListSize) instanceof TripleElement) {
						TripleElement informingTriple = (TripleElement) shpTriples.get(preCallListSize);
						tr.setSubject(informingTriple.getSubject());
						shpTriples.add(tr);
						subjectFound = true;
					}
				}
			} else {
				Object subjObj = processExpression(subj);
				if (subjObj instanceof Node) {
					tr.setSubject((Node) subjObj);
					shpTriples.add(tr);
					subjectFound = true;
				} else if (subjObj instanceof BuiltinElement && ((BuiltinElement) subjObj).getArguments() != null
						&& ((BuiltinElement) subjObj).getArguments().size() > 0) {
					tr.setSubject(nodeCheck(((BuiltinElement) subjObj).getArguments().get(0)));
					shpTriples.add((GraphPatternElement) subjObj);
					shpTriples.add(tr);
					subjectFound = true;
				} else {
					tr.setSubject(nodeCheck(subjObj));
					shpTriples.add(tr);
					subjectFound = true;
				}
			}
			if (!subjectFound) {
				throw new TranslationException(
						"Unhandled SubjHasProp subject type: " + subj.getClass().getCanonicalName());
			}
		}
		if (tr.getObject() == null) {
			boolean objectFound = false;
			if (obj instanceof SubjHasProp) {
				int preCallListSize = shpTriples.size();
				shpTriples = processSubjHasProp(((SubjHasProp) obj).getLeft(), ((SubjHasProp) obj).getProp(),
						((SubjHasProp) obj).getRight(), shpTriples, obj);
				// must get object of current triple; should be the subject of the first triple
				// added by call on previous line
				if (shpTriples.size() > preCallListSize) {
					if (shpTriples.get(preCallListSize) instanceof TripleElement) {
						TripleElement informingTriple = (TripleElement) shpTriples.get(preCallListSize);
						tr.setObject(informingTriple.getSubject());
						objectFound = true;
					}
				}
			} else {
				Object objObj = processExpression(obj);
				if (objObj instanceof Node) {
					tr.setObject((Node) objObj);
					objectFound = true;
				} else if (objObj instanceof BuiltinElement
						&& ((BuiltinElement) objObj).getFuncType().equals(BuiltinType.Equal)
						&& ((BuiltinElement) objObj).getArguments() != null
						&& ((BuiltinElement) objObj).getArguments().size() == 1) {
					tr.setObject(nodeCheck(((BuiltinElement) objObj).getArguments().get(0)));
					objectFound = true;
				} else {
					tr.setObject(nodeCheck(objObj));
					objectFound = true;
				}
			}
			if (!objectFound) {
				throw new TranslationException(
						"Unable to find object for triple: " + obj.getClass().getCanonicalName());
			}
		}
		try {
			validateTripleTypes(subj, pred, obj, tr, expr);
		} catch (CircularDependencyException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return shpTriples;
	}

	protected Junction compoundTypeCheckTypeToNode(TypeCheckInfo dtci, EObject expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		Iterator<TypeCheckInfo> ctitr = dtci.getCompoundTypes().iterator();
		Junction last = null;
		Junction jct = null;
		while (ctitr.hasNext()) {
			Object current = null;
			TypeCheckInfo tci = ctitr.next();
			if (tci.getCompoundTypes() != null) {
				current = compoundTypeCheckTypeToNode(tci, expr);
			} else if (tci.getTypeCheckType() != null) {
				if (tci.getTypeCheckType() instanceof NamedNode) {
					current = tci.getTypeCheckType();
				} else {
					addError("Type check info doesn't have expected ConceptName type", expr);
				}
			} else {
				addError("Type check info doesn't have valid type", expr);
			}
			if (current != null) {
				if (jct == null) {
					if (ctitr.hasNext()) {
						// there is more so new junction
						jct = new Junction();
						jct.setJunctionName("or");
						if (last != null) {
							// this is a nested junction
							jct.setLhs(last);
							jct.setRhs(current);
						} else {
							// this is not a nested junction so just set the LHS to current, RHS will be set
							// on next iteration
							jct.setLhs(current);
						}
					} else {
						jct = new Junction();
						jct.setJunctionName("or");
						if (last != null) {
							jct.setLhs(last);
							jct.setRhs(current);
						} else {
							// this shouldn't happen
							addError("Unexpected problem in compoundTypeCheckTypeToNode", expr);
						}
					}
				} else {
					// this finishes off the RHS of the first junction
					jct.setRhs(current);
					last = jct;
					jct = null; // there should always be a final RHS that will set last, which is returned
				}
			}
		}
		return jct;
	}

	protected List<Node> disjunctionToNodeList(Junction disjunct) throws TranslationException {
		List<Node> results = new ArrayList<Node>();
		if (disjunct.getJunctionType().equals(JunctionType.Disj)) {
			Object lhs = disjunct.getLhs();
			if (lhs instanceof Node) {
				if (!results.contains((Node) lhs)) {
					results.add((Node) lhs);
				}
			} else if (lhs instanceof Junction) {
				results = disjunctionToNodeList((Junction) lhs, results);
			}
			Object rhs = disjunct.getRhs();
			if (rhs instanceof Node) {
				if (!results.contains((Node) rhs)) {
					results.add((Node) rhs);
				}
			} else if (rhs instanceof Junction) {
				results = disjunctionToNodeList((Junction) rhs, results);
			}
			return results;
		} else {
			throw new TranslationException("Invalid Junction type encountered converting disjunction of Nodes to list");
		}
	}

	protected List<Node> disjunctionToNodeList(Junction disjunct, List<Node> results) throws TranslationException {
		if (disjunct.getJunctionType().equals(JunctionType.Disj)) {
			Object lhs = disjunct.getLhs();
			if (lhs instanceof Node) {
				if (!results.contains((Node) lhs)) {
					results.add((Node) lhs);
				}
			} else if (lhs instanceof Junction) {
				results = disjunctionToNodeList((Junction) lhs, results);
			}
			Object rhs = disjunct.getRhs();
			if (rhs instanceof Node) {
				if (!results.contains((Node) rhs)) {
					results.add((Node) rhs);
				}
			} else if (rhs instanceof Junction) {
				results = disjunctionToNodeList((Junction) rhs, results);
			}
			return results;
		} else {
			throw new TranslationException("Invalid Junction type encountered converting disjunction of Nodes to list");
		}
	}

	public Object processExpression(Sublist expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		Expression list = expr.getList();
		Expression where = expr.getWhere();
		Object lobj = processExpression(list);
		Object wobj = processExpression(where);

		// addError("Processing of sublist construct not yet implemented: " +
		// lobj.toString() + ", " + wobj.toString(), expr);

		BuiltinElement builtin = new BuiltinElement();
		builtin.setFuncName("sublist");
		builtin.addArgument(nodeCheck(lobj));
		if (lobj instanceof GraphPatternElement) {
			((GraphPatternElement) lobj).setEmbedded(true);
		}
		builtin.addArgument(nodeCheck(wobj));
		if (wobj instanceof GraphPatternElement) {
			((GraphPatternElement) wobj).setEmbedded(true);
		}
		return builtin;
	}

	public Object processExpression(UnaryExpression expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		Object eobj = processExpression(expr.getExpr());
		// if (eobj instanceof VariableNode && ((VariableNode)eobj).isCRulesVariable()
		// && ((VariableNode)eobj).getType() != null) {
		// TripleElement trel = new TripleElement((VariableNode)eobj, new RDFTypeNode(),
		// ((VariableNode)eobj).getType());
		// trel.setSourceType(TripleSourceType.SPV);
		// eobj = trel;
		// }
		String op = expr.getOp();
		Object val = null;
		if (eobj instanceof com.ge.research.sadl.model.gp.Literal) {
			val = ((com.ge.research.sadl.model.gp.Literal) eobj).getValue();
		}
		if (op.equals("-") && val instanceof Number) {
			if (val instanceof BigDecimal) {
				val = ((BigDecimal) val).negate();
			} else {
				val = -1.0 * ((Number) val).doubleValue();
			}
			((com.ge.research.sadl.model.gp.Literal) eobj).setValue(val);
			((com.ge.research.sadl.model.gp.Literal) eobj)
					.setOriginalText(op + ((com.ge.research.sadl.model.gp.Literal) eobj).getOriginalText());
			return eobj;
		} else if (op.equals("not")) {
			if (val instanceof Boolean) {
				try {
					boolean bval = ((Boolean) val).booleanValue();
					if (bval) {
						((com.ge.research.sadl.model.gp.Literal) eobj).setValue(false);
					} else {
						((com.ge.research.sadl.model.gp.Literal) eobj).setValue(true);
					}
					((com.ge.research.sadl.model.gp.Literal) eobj).setOriginalText(
							op + " " + ((com.ge.research.sadl.model.gp.Literal) eobj).getOriginalText());
					return eobj;
				} catch (Exception e) {

				}
			} else {
				// this is a not before a non-boolean value so we want to pull the negation up
				pullOperationUp(expr);
				return eobj;
			}
		}
		BuiltinElement bi = new BuiltinElement();
		bi.setFuncName(transformOpName(op));
		if (eobj instanceof Node) {
			bi.addArgument((Node) eobj);
		} else if (eobj instanceof GraphPatternElement) {
			bi.addArgument(new ProxyNode(eobj));
		} else if (eobj == null) {
			addError("Unary operator '" + op + "' has no argument. Perhaps parentheses are needed.", expr);
		} else {
			throw new TranslationException("Expected node, got '" + eobj.getClass().getCanonicalName() + "'");
		}
		return bi;
	}

	private String transformOpName(String op) {
		if (op.equals("there exists")) {
			return THERE_EXISTS;
		} else if (op.equals("=") || op.equals("==")) {
			return "is";
		}
		return op;
	}

	protected void pullOperationUp(Expression expr) {
		if (expr != null) {
			if (operationsPullingUp == null) {
				operationsPullingUp = new ArrayList<EObject>();
			}
			operationsPullingUp.add(expr);
		}
	}

	private EObject getOperationPullingUp() {
		if (operationsPullingUp != null && operationsPullingUp.size() > 0) {
			EObject removed = operationsPullingUp.remove(operationsPullingUp.size() - 1);
			return removed;
		}
		return null;
	}

	protected boolean isOperationPullingUp(EObject expr) {
		if (operationsPullingUp != null && operationsPullingUp.size() > 0) {
			if (operationsPullingUp.get(operationsPullingUp.size() - 1).equals(expr)) {
				return true;
			}
		}
		return false;
	}

	protected Object applyPulledUpOperations(Object result)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		while (operationsPullingUp != null && operationsPullingUp.size() > 0) {
			result = applyPulledUpOperation(getOperationPullingUp(), result);
		}
		return result;
	}

	protected Object applyPulledUpOperation(EObject operationPullingUp, Object result)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		if (operationPullingUp instanceof UnaryExpression
				&& ((UnaryExpression) operationPullingUp).getOp().equals("not")) {
			return wrapInNot(result);
		}
		return result;
	}

	private Object wrapInNot(Object result) throws InvalidNameException, InvalidTypeException, TranslationException {
		BuiltinElement bi = new BuiltinElement();
		bi.setFuncName("not");
		bi.addArgument(nodeCheck(result));
		return bi;
	}

	public Object processExpression(UnitExpression expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		String unit = expr.getUnit();
		Expression value = expr.getLeft();
		Object valobj = null;
		valobj = processExpression(value);
		if (ignoreUnittedQuantities) {
			return valobj;
		}
		if (valobj instanceof com.ge.research.sadl.model.gp.Literal) {
			((com.ge.research.sadl.model.gp.Literal) valobj).setUnits(unit);
			return valobj;
		}
		com.ge.research.sadl.model.gp.Literal unitLiteral = new com.ge.research.sadl.model.gp.Literal();
		unitLiteral.setValue(unit);
		return createBinaryBuiltin("unittedQuantity", valobj, unitLiteral);
	}

	private void processSadlSameAs(SadlSameAs element) throws JenaProcessorException {
		SadlResource sr = element.getNameOrRef();
		String uri = getDeclarationExtensions().getConceptUri(sr);
		OntResource rsrc = getTheJenaModel().getOntResource(uri);
		SadlTypeReference smas = element.getSameAs();
		OntConceptType sameAsType;
		if (rsrc == null) {
			// concept does not exist--try to get the type from the sameAs
			sameAsType = getSadlTypeReferenceType(smas);

		} else {
			try {
				sameAsType = getDeclarationExtensions().getOntConceptType(sr);
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
				cls = createOntClass(getDeclarationExtensions().getConcreteName(sr), (String) null, null);
			}
			if (element.isComplement()) {
				ComplementClass cc = getTheJenaModel().createComplementClass(cls.getURI(), smasCls);
				logger.debug("New complement class '" + cls.getURI() + "' created");
			} else {
				cls.addEquivalentClass(smasCls);
				logger.debug("Class '" + cls.toString() + "' given equivalent class '" + smasCls.toString() + "'");
			}
		} else if (sameAsType.equals(OntConceptType.INSTANCE)) {
			OntResource smasInst = sadlTypeReferenceToOntResource(smas);
			rsrc.addSameAs(smasInst);
			logger.debug("Instance '" + rsrc.toString() + "' declared same as '" + smas.toString() + "'");
		} else {
			throw new JenaProcessorException("Unexpected concept type for same as statement: " + sameAsType.toString());
		}
	}

	private List<OntResource> processSadlClassOrPropertyDeclaration(SadlClassOrPropertyDeclaration element)
			throws JenaProcessorException, TranslationException {
		if (isEObjectPreprocessed(element)) {
			return null;
		}
		// Get the names of the declared concepts and store in a list
		List<String> newNames = new ArrayList<String>();
		Map<String, EList<SadlAnnotation>> nmanns = null;
		EList<SadlResource> clses = element.getClassOrProperty();
		if (clses != null) {
			Iterator<SadlResource> citer = clses.iterator();
			while (citer.hasNext()) {
				SadlResource sr = citer.next();
				String nm = getDeclarationExtensions().getConceptUri(sr);
				SadlResource decl = getDeclarationExtensions().getDeclaration(sr);
				if (!(decl.equals(sr))) {
					// defined already
					try {
						if (getDeclarationExtensions().getOntConceptType(decl).equals(OntConceptType.STRUCTURE_NAME)) {
							addError("This is already a Named Structure", sr);
						}
					} catch (CircularDefinitionException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				newNames.add(nm);
				EList<SadlAnnotation> anns = sr.getAnnotations();
				if (anns != null && anns.size() > 0) {
					if (nmanns == null) {
						nmanns = new HashMap<String, EList<SadlAnnotation>>();
					}
					nmanns.put(nm, anns);
				}
			}
		}

		if (newNames.size() < 1) {
			throw new JenaProcessorException("No names passed to processSadlClassOrPropertyDeclaration");
		}
		List<OntResource> rsrcList = new ArrayList<OntResource>();
		// The declared concept(s) will be of type class, property, or datatype.
		// Determining which will depend on the structure, including the
		// superElement....
		// Get the superElement
		SadlTypeReference superElement = element.getSuperElement();
		boolean isList = typeRefIsList(superElement);
		// 1) if superElement is null then it is a top-level class declaration
		if (superElement == null) {
			OntClass cls = createOntClass(newNames.get(0), (OntClass) null);
			if (nmanns != null && nmanns.get(newNames.get(0)) != null) {
				addAnnotationsToResource(cls, nmanns.get(newNames.get(0)));
			}
			rsrcList.add(cls);
		}
		// 2) if superElement is not null then the type of the new concept is the same
		// as the type of the superElement
		// the superElement can be:
		// a) a SadlSimpleTypeReference
		else if (superElement instanceof SadlSimpleTypeReference) {
			SadlResource superSR = ((SadlSimpleTypeReference) superElement).getType();
			String superSRUri = getDeclarationExtensions().getConceptUri(superSR);
			OntConceptType superElementType;
			try {
				superElementType = getDeclarationExtensions().getOntConceptType(superSR);
				if (isList) {
					superElementType = OntConceptType.CLASS_LIST;
				}
			} catch (CircularDefinitionException e) {
				superElementType = e.getDefinitionType();
				addError(SadlErrorMessages.CIRCULAR_IMPORT.get(superSRUri), superElement);
			}
			if (superElementType.equals(OntConceptType.CLASS)) {
				for (int i = 0; i < newNames.size(); i++) {
					OntClass cls = createOntClass(newNames.get(i), superSRUri, superSR);
					if (nmanns != null && nmanns.get(newNames.get(i)) != null) {
						addAnnotationsToResource(cls, nmanns.get(newNames.get(i)));
					}
					rsrcList.add(cls);
				}
			} else if (superElementType.equals(OntConceptType.CLASS_LIST)
					|| superElementType.equals(OntConceptType.DATATYPE_LIST)) {
				for (int i = 0; i < newNames.size(); i++) {
					rsrcList.add(getOrCreateListSubclass(newNames.get(i), superSRUri, superSR.eResource(), null));
				}
			} else if (superElementType.equals(OntConceptType.CLASS_PROPERTY)) {
				for (int i = 0; i < newNames.size(); i++) {
					OntProperty prop = createObjectProperty(newNames.get(i), superSRUri);
					if (nmanns != null && nmanns.get(newNames.get(i)) != null) {
						addAnnotationsToResource(prop, nmanns.get(newNames.get(i)));
					}
					rsrcList.add(prop);
				}
			} else if (superElementType.equals(OntConceptType.DATATYPE_PROPERTY)) {
				for (int i = 0; i < newNames.size(); i++) {
					DatatypeProperty prop = createDatatypeProperty(newNames.get(i), superSRUri);
					if (nmanns != null && nmanns.get(newNames.get(i)) != null) {
						addAnnotationsToResource(prop, nmanns.get(newNames.get(i)));
					}
					rsrcList.add(prop);
				}
			} else if (superElementType.equals(OntConceptType.ANNOTATION_PROPERTY)) {
				for (int i = 0; i < newNames.size(); i++) {
					AnnotationProperty prop = createAnnotationProperty(newNames.get(i), superSRUri);
					if (nmanns != null && nmanns.get(newNames.get(i)) != null) {
						addAnnotationsToResource(prop, nmanns.get(newNames.get(i)));
					}
					rsrcList.add(prop);
				}
			} else if (superElementType.equals(OntConceptType.RDF_PROPERTY)) {
				for (int i = 0; i < newNames.size(); i++) {
					OntProperty prop = createRdfProperty(newNames.get(i), superSRUri);
					if (nmanns != null && nmanns.get(newNames.get(i)) != null) {
						addAnnotationsToResource(prop, nmanns.get(newNames.get(i)));
					}
					rsrcList.add(prop);
				}
			}
		}
		// b) a SadlPrimitiveDataType
		else if (superElement instanceof SadlPrimitiveDataType) {
			if (isList) {
				com.hp.hpl.jena.rdf.model.Resource spdt = getSadlPrimitiveDataTypeResource(
						(SadlPrimitiveDataType) superElement);
				rsrcList.add(getOrCreateListSubclass(newNames.get(0), spdt.getURI(), superElement.eResource(), null));
			} else {
				com.hp.hpl.jena.rdf.model.Resource spdt = processSadlPrimitiveDataType(element,
						(SadlPrimitiveDataType) superElement, newNames.get(0));
				if (spdt instanceof OntClass) {
					rsrcList.add((OntClass) spdt);
				} else if (spdt.canAs(OntResource.class)) {
					rsrcList.add(spdt.as(OntResource.class));
				} else {
					throw new JenaProcessorException("Expected OntResource to be returned"); // .add(spdt);
				}
			}
		}
		// c) a SadlPropertyCondition
		else if (superElement instanceof SadlPropertyCondition) {
			OntClass propCond = processSadlPropertyCondition((SadlPropertyCondition) superElement);
			rsrcList.add(propCond);
		}
		// d) a SadlTypeReference
		else if (superElement instanceof SadlTypeReference) {
			// this can only be a class; can't create a property as a SadlTypeReference
			Object superClsObj = sadlTypeReferenceToObject(superElement);
			if (superClsObj instanceof List) {
				// must be a union of xsd datatypes; create RDFDatatype
				OntClass unionCls = createRdfsDatatype(newNames.get(0), (List) superClsObj, null, null);
				rsrcList.add(unionCls);
			} else if (superClsObj instanceof OntResource) {
				OntResource superCls = (OntResource) superClsObj;
				if (superCls != null) {
					if (superCls instanceof UnionClass) {
						ExtendedIterator<? extends com.hp.hpl.jena.rdf.model.Resource> itr = ((UnionClass) superCls)
								.listOperands();
						while (itr.hasNext()) {
							com.hp.hpl.jena.rdf.model.Resource cls = itr.next();
							// System.out.println("Union member: " + cls.toString());
						}
					} else if (superCls instanceof IntersectionClass) {
						ExtendedIterator<? extends com.hp.hpl.jena.rdf.model.Resource> itr = ((IntersectionClass) superCls)
								.listOperands();
						while (itr.hasNext()) {
							com.hp.hpl.jena.rdf.model.Resource cls = itr.next();
							// System.out.println("Intersection member: " + cls.toString());
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
					//
					EList<SadlExplicitValue> instances = ((SadlMustBeOneOf) rest).getValues();
					if (instances != null) {
						Iterator<SadlExplicitValue> iitr = instances.iterator();
						List<Individual> individuals = new ArrayList<Individual>();
						while (iitr.hasNext()) {
							SadlExplicitValue inst = iitr.next();
							if (inst instanceof SadlResource) {
								for (int i = 0; i < rsrcList.size(); i++) {
									individuals.add(createIndividual((SadlResource) inst, rsrcList.get(i).asClass()));
								}
							} else {
								throw new JenaProcessorException(
										"Unhandled type of SadlExplicitValue: " + inst.getClass().getCanonicalName());
							}
						}
						// create equivalent class
						RDFList collection = getTheJenaModel().createList();
						Iterator<Individual> iter = individuals.iterator();
						while (iter.hasNext()) {
							RDFNode dt = iter.next();
							collection = collection.with(dt);
						}
						EnumeratedClass enumcls = getTheJenaModel().createEnumeratedClass(null, collection);
						OntResource cls = rsrcList.get(0);
						if (cls.canAs(OntClass.class)) {
							cls.as(OntClass.class).addEquivalentClass(enumcls);
						}
					}
				} else if (rest instanceof SadlCanOnlyBeOneOf) {
					EList<SadlExplicitValue> instances = ((SadlCanOnlyBeOneOf) rest).getValues();
					if (instances != null) {
						Iterator<SadlExplicitValue> iitr = instances.iterator();
						List<Individual> individuals = new ArrayList<Individual>();
						while (iitr.hasNext()) {
							SadlExplicitValue inst = iitr.next();
							if (inst instanceof SadlResource) {
								for (int i = 0; i < rsrcList.size(); i++) {
									individuals.add(createIndividual((SadlResource) inst, rsrcList.get(i).asClass()));
								}
							} else {
								throw new JenaProcessorException(
										"Unhandled type of SadlExplicitValue: " + inst.getClass().getCanonicalName());
							}
						}
						// create equivalent class
						RDFList collection = getTheJenaModel().createList();
						Iterator<Individual> iter = individuals.iterator();
						while (iter.hasNext()) {
							RDFNode dt = iter.next();
							collection = collection.with(dt);
						}
						EnumeratedClass enumcls = getTheJenaModel().createEnumeratedClass(null, collection);
						OntResource cls = rsrcList.get(0);
						if (cls.canAs(OntClass.class)) {
							cls.as(OntClass.class).addEquivalentClass(enumcls);
						}
					}
				}
			}
		}
		for (int i = 0; i < rsrcList.size(); i++) {
			Iterator<SadlProperty> dbiter = element.getDescribedBy().iterator();
			while (dbiter.hasNext()) {
				SadlProperty sp = dbiter.next();
				// if this is an assignment of a range to a property the property will be
				// returned (prop) for domain assignment,
				// but if it is a condition to be added as property restriction null will be
				// returned
				Property prop = processSadlProperty(rsrcList.get(i), sp);
				if (prop != null) {
					addPropertyDomain(prop, rsrcList.get(i), sp); // .eContainer());
				}
			}

		}
		if (isList) {
			addLengthRestrictionsToList(rsrcList.get(0), element.getFacet());

		}
		return rsrcList;
	}

	private Property processSadlProperty(OntResource subject, SadlProperty element) throws JenaProcessorException {
		Property retProp = null;
		// this has multiple forms:
		// 1) <prop> is a property...
		// 2) relationship of <Domain> to <Range> is <prop>
		// 3) <prop> describes <class> with <range info> (1st spr is a
		// SadlTypeAssociation, the domain; 2nd spr is a SadlRangeRestriction, the
		// range)
		// 4) <prop> of <class> <restriction> (1st spr is a SadlTypeAssociation, the
		// class being restricted; 2nd spr is a SadlCondition
		// 5) <prop> of <class> can only be one of {<instances> or <datavalues>} (1st
		// spr is SadlTypeAssociation, 2nd spr is a SadlCanOnlyBeOneOf)
		// 6) <prop> of <class> must be one of {<instances> or <datavalues>} (1st spr is
		// SadlTypeAssociation, 2nd spr is a SadlCanOnlyBeOneOf)
		SadlResource sr = sadlResourceFromSadlProperty(element);
		String propUri = getDeclarationExtensions().getConceptUri(sr);
		OntConceptType propType;
		try {
			propType = getDeclarationExtensions().getOntConceptType(sr);
			if (!isProperty(propType)) {
				addError(SadlErrorMessages.INVALID_USE_OF_CLASS_AS_PROPERTY
						.get(getDeclarationExtensions().getConcreteName(sr)), element);
			}
		} catch (CircularDefinitionException e) {
			propType = e.getDefinitionType();
			addError(e.getMessage(), element);
		}

		Iterator<SadlPropertyRestriction> spitr = element.getRestrictions().iterator();
		if (spitr.hasNext()) {
			SadlPropertyRestriction spr1 = spitr.next();
			if (spr1 instanceof SadlIsAnnotation) {
				retProp = getTheJenaModel().createAnnotationProperty(propUri);
			} else if (spr1 instanceof SadlIsTransitive) {
				OntProperty pr;
				if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
					pr = getOrCreateObjectProperty(propUri);
				} else {
					throw new JenaProcessorException("Only object properties can be transitive");
				}
				if (pr == null) {
					throw new JenaProcessorException("Property '" + propUri + "' not found in ontology.");
				}
				pr.convertToTransitiveProperty();
				retProp = getTheJenaModel().createTransitiveProperty(pr.getURI());
			} else if (spr1 instanceof SadlIsInverseOf) {
				OntProperty pr;
				if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
					pr = getOrCreateObjectProperty(propUri);
				} else {
					throw new JenaProcessorException("Only object properties can have inverses");
				}
				if (pr == null) {
					throw new JenaProcessorException("Property '" + propUri + "' not found in ontology.");
				}
				SadlResource otherProp = ((SadlIsInverseOf) spr1).getOtherProperty();
				String otherPropUri = getDeclarationExtensions().getConceptUri(otherProp);
				OntConceptType optype;
				try {
					optype = getDeclarationExtensions().getOntConceptType(otherProp);
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
			} else if (spr1 instanceof SadlRangeRestriction) {
				SadlTypeReference rng = ((SadlRangeRestriction) spr1).getRange();
				if (rng != null) {
					RangeValueType rngValueType = RangeValueType.CLASS_OR_DT; // default
					boolean isList = typeRefIsList(rng);
					if (isList) {
						rngValueType = RangeValueType.LIST;
					}
					OntProperty prop;
					String rngName = null;
					if (!isList && rng instanceof SadlPrimitiveDataType) {
						rngName = ((SadlPrimitiveDataType) rng).getPrimitiveType().getName();
						RDFNode rngNode = primitiveDatatypeToRDFNode(rngName);
						if (!checkForExistingCompatibleDatatypeProperty(propUri, rngNode)) {
							prop = createDatatypeProperty(propUri, null);
							addPropertyRange(propType, prop, rngNode, rngValueType, rng);
						} else {
							prop = getTheJenaModel().getDatatypeProperty(propUri);
							addPropertyRange(propType, prop, rngNode, rngValueType, rng);
						}
						addPropertyRange(propType, prop, rngNode, rngValueType, rng);
						retProp = prop;
					} else {
						ConceptName rngcn = sadlSimpleTypeReferenceToConceptName(rng);
						if (rngcn != null) {
							rngName = rngcn.toFQString();
						} else {
							addError("No range found in statement", rng);
						}
						OntResource rngRsrc;
						if (isList) {
							rngRsrc = getOrCreateListSubclass(null, rngName, element.eResource(), ((SadlRangeRestriction) spr1).getFacet());
							addLengthRestrictionsToList(rngRsrc, ((SadlRangeRestriction) spr1).getFacet());
							propType = OntConceptType.CLASS_PROPERTY;
						} else {
							rngRsrc = sadlTypeReferenceToOntResource(rng);
						}
						if (rngRsrc != null) {
							retProp = assignRangeToProperty(propUri, propType, rngRsrc, rngValueType, rng);
						}
					}
				} else {
					// this can happen when the range is just specified as "class", creating an
					// owl:ObjectProperty with no range
					if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
						retProp = getOrCreateObjectProperty(propUri);
					} else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
						retProp = getOrCreateDatatypeProperty(propUri);
					} else if (propType.equals(OntConceptType.RDF_PROPERTY)) {
						retProp = getOrCreateRdfProperty(propUri);
					}
				}
				if (((SadlRangeRestriction) spr1).isSingleValued()) {
					// add cardinality restriction
					addCardinalityRestriction(subject, retProp, 1);
				}
			} else if (spr1 instanceof SadlCondition) {
				OntProperty prop = getTheJenaModel().getOntProperty(propUri);
				if (prop == null) {
					prop = getOrCreateRdfProperty(propUri);
				}
				OntClass condCls = sadlConditionToOntClass((SadlCondition) spr1, prop, propType);
				OntClass cls = null;
				if (subject != null) {
					if (subject.canAs(OntClass.class)) {
						cls = subject.as(OntClass.class);
						cls.addSuperClass(condCls);
						retProp = null;
					} else {
						throw new JenaProcessorException("Unable to convert concept being restricted ("
								+ subject.toString() + ") to an OntClass.");
					}
				} else {
					// I think this is OK... AWC 3/13/2017
				}
			} else if (spitr.hasNext()) {
				SadlPropertyRestriction spr2 = spitr.next();
				if (spitr.hasNext()) {
					StringBuilder sb = new StringBuilder();
					int cntr = 0;
					while (spitr.hasNext()) {
						if (cntr++ > 0)
							sb.append(", ");
						sb.append(spitr.next().getClass().getCanonicalName());
					}
					throw new JenaProcessorException(
							"Unexpected SadlProperty has more than 2 restrictions: " + sb.toString());
				}
				if (spr1 instanceof SadlTypeAssociation && spr2 instanceof SadlRangeRestriction) {
					// this is case 3
					SadlTypeReference domain = ((SadlTypeAssociation) spr1).getDomain();
					OntConceptType domaintype;
					try {
						domaintype = sadlTypeReferenceOntConceptType(domain);
						if (domaintype != null && domaintype.equals(OntConceptType.DATATYPE)) {
							addWarning(SadlErrorMessages.DATATYPE_AS_DOMAIN.get(), domain);
						}
					} catch (CircularDefinitionException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);

					// before creating the property, determine if the range is a sadllistmodel:List
					// as if it is the property type is actually an owl:ObjectProperty
					RangeValueType rngValueType = RangeValueType.CLASS_OR_DT; // default
					SadlTypeReference rng = ((SadlRangeRestriction) spr2).getRange();
					if (rng instanceof SadlPrimitiveDataType) {
						if (((SadlPrimitiveDataType) rng).isList()) {
							rngValueType = RangeValueType.LIST;
							propType = OntConceptType.DATATYPE_LIST;
						}
					} else if (rng instanceof SadlSimpleTypeReference) {
						if (((SadlSimpleTypeReference) rng).isList()) {
							rngValueType = RangeValueType.LIST;
							propType = OntConceptType.CLASS_LIST;
						}
					}

					OntProperty prop;
					if (propType.equals(OntConceptType.CLASS_PROPERTY) || rngValueType.equals(RangeValueType.LIST)) {
						prop = getOrCreateObjectProperty(propUri);
					} else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
						prop = getOrCreateDatatypeProperty(propUri);
					} else if (propType.equals(OntConceptType.RDF_PROPERTY)) {
						prop = getOrCreateRdfProperty(propUri);
					} else if (propType.equals(OntConceptType.ANNOTATION_PROPERTY)) {
						addError(
								"Can't specify domain of an annotation property. Did you want to use a property restriction?",
								sr);
						throw new JenaProcessorException(
								"Invalid property type (" + propType.toString() + ") for '" + propUri + "'");
					} else {
						throw new JenaProcessorException(
								"Invalid property type (" + propType.toString() + ") for '" + propUri + "'");
					}
					addPropertyDomain(prop, domainrsrc, domain);
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

					// RangeValueType rngValueType = RangeValueType.CLASS_OR_DT; // default
					// SadlTypeReference rng = ((SadlRangeRestriction)spr2).getRange();
					if (rng instanceof SadlPrimitiveDataType && !rngValueType.equals(RangeValueType.LIST)) {
						String rngName = ((SadlPrimitiveDataType) rng).getPrimitiveType().getName();
						RDFNode rngNode = primitiveDatatypeToRDFNode(rngName);
						DatatypeProperty prop2 = null;
						if (!checkForExistingCompatibleDatatypeProperty(propUri, rngNode)) {
							// TODO should this ever happen? spr1 should have created the property?
							prop2 = createDatatypeProperty(propUri, null);
							addPropertyRange(propType, prop, rngNode, rngValueType, rng);
						} else {
							prop2 = getTheJenaModel().getDatatypeProperty(propUri);
						}
						retProp = prop2;
					} else if (((SadlRangeRestriction) spr2).getTypeonly() == null) {
						OntResource rngRsrc = sadlTypeReferenceToOntResource(rng);
						if ((rng instanceof SadlSimpleTypeReference && ((SadlSimpleTypeReference) rng).isList())
								|| (rng instanceof SadlPrimitiveDataType && ((SadlPrimitiveDataType) rng).isList())) {
							addLengthRestrictionsToList(rngRsrc, ((SadlRangeRestriction) spr2).getFacet());
						}
						if (rngRsrc == null) {
							addError(SadlErrorMessages.RANGE_RESOLVE.toString(), rng);
						} else {
							retProp = assignRangeToProperty(propUri, propType, rngRsrc, rngValueType, rng);
						}
					} else {
						retProp = prop;
					}
					if (((SadlRangeRestriction) spr2).isSingleValued()) {
						addCardinalityRestriction(domainrsrc, retProp, 1);
					}
				} else if (spr1 instanceof SadlTypeAssociation && spr2 instanceof SadlCondition) {
					// this is case 4
					SadlTypeReference domain = ((SadlTypeAssociation) spr1).getDomain();
					OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
					if (domainrsrc == null) {
						addError(SadlErrorMessages.UNABLE_TO_FIND.get("domain"), domain);
						return null;
					} else if (domainrsrc.canAs(OntClass.class)) {
						OntClass cls = domainrsrc.as(OntClass.class);
						Property prop = getTheJenaModel().getProperty(propUri);
						if (prop != null) {
							OntClass condCls = sadlConditionToOntClass((SadlCondition) spr2, prop, propType);
							if (condCls != null) {
								cls.addSuperClass(condCls);
							} else {
								addError(SadlErrorMessages.UNABLE_TO_ADD.get("restriction",
										"unable to create condition class"), domain);
							}
							retProp = prop;
						} else {
							throw new JenaProcessorException(
									"Unable to convert property '" + propUri + "' to OntProperty.");
						}
					} else {
						throw new JenaProcessorException("Unable to convert concept being restricted ("
								+ domainrsrc.toString() + ") to an OntClass.");
					}
				} else if (spr1 instanceof SadlTypeAssociation && spr2 instanceof SadlCanOnlyBeOneOf) {
					SadlTypeReference domain = ((SadlTypeAssociation) spr1).getDomain();
					OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
					if (domainrsrc == null) {
						addError(SadlErrorMessages.UNABLE_TO_FIND.get("domain"), domain);
						return null;
					} else if (domainrsrc.canAs(OntClass.class)) {
						OntClass cls = domainrsrc.as(OntClass.class);
						Property prop = getTheJenaModel().getProperty(propUri);
						if (prop != null) {
							EList<SadlExplicitValue> values = ((SadlCanOnlyBeOneOf) spr2).getValues();
							if (values != null) {
								EnumeratedClass enumCls = sadlExplicitValuesToEnumeratedClass(values);
								AllValuesFromRestriction avf = getTheJenaModel().createAllValuesFromRestriction(null,
										prop, enumCls);
								if (avf != null) {
									cls.addSuperClass(avf);
								} else {
									addError(SadlErrorMessages.UNABLE_TO_CREATE.get("AllValuesFromRestriction",
											"Unknown reason"), spr2);
								}
							} else {
								addError(SadlErrorMessages.UNABLE_TO_ADD.get("all values from restriction",
										"unable to create oneOf class"), domain);
							}
							retProp = prop;
						} else {
							throw new JenaProcessorException(
									"Unable to convert property '" + propUri + "' to OntProperty.");
						}
					}
				} else if (spr1 instanceof SadlTypeAssociation && spr2 instanceof SadlMustBeOneOf) {
					SadlTypeReference domain = ((SadlTypeAssociation) spr1).getDomain();
					OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
					if (domainrsrc == null) {
						addError(SadlErrorMessages.UNABLE_TO_FIND.get("domain"), domain);
						return null;
					} else if (domainrsrc.canAs(OntClass.class)) {
						OntClass cls = domainrsrc.as(OntClass.class);
						Property prop = getTheJenaModel().getProperty(propUri);
						if (prop != null) {
							EList<SadlExplicitValue> values = ((SadlMustBeOneOf) spr2).getValues();
							if (values != null) {
								EnumeratedClass enumCls = sadlExplicitValuesToEnumeratedClass(values);
								SomeValuesFromRestriction svf = getTheJenaModel().createSomeValuesFromRestriction(null,
										prop, enumCls);
								if (svf != null) {
									cls.addSuperClass(svf);
								} else {
									addError(SadlErrorMessages.UNABLE_TO_CREATE.get("SomeValuesFromRestriction",
											"Unknown reason"), spr2);
								}
							} else {
								addError(SadlErrorMessages.UNABLE_TO_ADD.get("some values from restriction",
										"unable to create oneOf class"), domain);
							}
							retProp = prop;
						} else {
							throw new JenaProcessorException(
									"Unable to convert property '" + propUri + "' to OntProperty.");
						}
					}
				} else if (spr1 instanceof SadlTypeAssociation && spr2 instanceof SadlDefaultValue) {
					SadlExplicitValue dv = ((SadlDefaultValue) spr2).getDefValue();
					int lvl = ((SadlDefaultValue) spr2).getLevel();
					SadlTypeReference domain = ((SadlTypeAssociation) spr1).getDomain();
					OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
					if (domainrsrc == null) {
						addError(SadlErrorMessages.UNABLE_TO_FIND.get("domain"), domain);
						return null;
					} else if (domainrsrc.canAs(OntClass.class)) {
						OntClass cls = domainrsrc.as(OntClass.class);
						Property prop = getTheJenaModel().getProperty(propUri);
						if (prop != null) {
							if (sadlDefaultsModel == null) {
								try {
									importSadlDefaultsModel(element.eResource());
								} catch (Exception e) {
									e.printStackTrace();
									throw new JenaProcessorException("Failed to load SADL Defaults model", e);
								}
							}

							RDFNode defVal = sadlExplicitValueToRdfNode(dv, prop, true);
							Individual seeAlsoDefault = null;
							if (propType.equals(OntConceptType.CLASS_PROPERTY)
									|| (propType.equals(OntConceptType.RDF_PROPERTY) && defVal.isResource())) {
								if (!(defVal.isURIResource()) || !defVal.canAs(Individual.class)) {
									addError("Error creating default for property '" + propUri + "' for class '"
											+ cls.getURI() + "' with value '" + defVal.toString()
											+ "': the value is not a named concept.", spr2);
								} else {
									Individual defInst = defVal.as(Individual.class);
									try {
										seeAlsoDefault = createDefault(cls, prop, defInst, lvl, element);
									} catch (Exception e) {
										addError("Error creating default for property '" + propUri + "' for class '"
												+ cls.getURI() + "' with value '" + defVal.toString() + "': "
												+ e.getMessage(), spr2);
									}
								}
							} else {
								if (propType.equals(OntConceptType.DATATYPE_PROPERTY) && !defVal.isLiteral()) {
									addError(
											"Error creating default for property '" + propUri + "' for class '"
													+ cls.getURI() + "' with value '" + defVal.toString()
													+ "': the value is a named concept but should be a data value.",
											spr2);
								} else {
									try {
										seeAlsoDefault = createDefault(cls, prop, defVal.asLiteral(), lvl, spr2);
									} catch (Exception e) {
										addError("Error creating default for property '" + propUri + "' for class '"
												+ cls.getURI() + "' with value '" + defVal.toString() + "': "
												+ e.getMessage(), spr2);
									}
								}
							}
							if (seeAlsoDefault != null) {
								cls.addSeeAlso(seeAlsoDefault);
							} else {
								addError("Unable to create default for '" + cls.getURI() + "', '" + propUri + "', '"
										+ defVal + "'", element);
							}

						}
					}
				} else {
					throw new JenaProcessorException("Unhandled restriction: spr1 is '" + spr1.getClass().getName()
							+ "', spr2 is '" + spr2.getClass().getName() + "'");
				}
			} else if (spr1 instanceof SadlTypeAssociation) {
				// this is case 3 but with range not present
				SadlTypeReference domain = ((SadlTypeAssociation) spr1).getDomain();
				OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
				ObjectProperty prop = getOrCreateObjectProperty(propUri);
				if (domainrsrc != null) {
					addPropertyDomain(prop, domainrsrc, domain);
				}
			} else if (spr1 instanceof SadlIsSymmetrical) {
				ObjectProperty prop = getOrCreateObjectProperty(propUri);
				if (prop != null) {
					if (!prop.isObjectProperty()) {
						addError(SadlErrorMessages.OBJECT_PROP_SYMMETRY.toString(), spr1);
					} else {
						getTheJenaModel().add(prop, RDF.type, OWL.SymmetricProperty);
					}
				}
			} else {
				throw new JenaProcessorException("Unhandled SadlProperty expression");
			}

			while (spitr.hasNext()) {
				SadlPropertyRestriction spr = spitr.next();
				if (spr instanceof SadlRangeRestriction) {
					RangeValueType rngValueType = RangeValueType.CLASS_OR_DT; // default
					SadlTypeReference rng = ((SadlRangeRestriction) spr).getRange();
					if (rng instanceof SadlPrimitiveDataType) {
						String rngName = ((SadlPrimitiveDataType) rng).getPrimitiveType().getName();
						RDFNode rngNode = primitiveDatatypeToRDFNode(rngName);
						DatatypeProperty prop = null;
						if (!checkForExistingCompatibleDatatypeProperty(propUri, rngNode)) {
							prop = createDatatypeProperty(propUri, null);
							addPropertyRange(propType, prop, rngNode, rngValueType, rng);
						} else {
							prop = getTheJenaModel().getDatatypeProperty(propUri);
						}
						retProp = prop;
					} else {
						OntResource rngRsrc = sadlTypeReferenceToOntResource(rng);
						if (rngRsrc == null) {
							throw new JenaProcessorException("Range failed to resolve to a class or datatype");
						}
						retProp = assignRangeToProperty(propUri, propType, rngRsrc, rngValueType, rng);
					}
				} else if (spr instanceof SadlCondition) {
					if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
						ObjectProperty prop = getOrCreateObjectProperty(propUri);
						OntClass condCls = sadlConditionToOntClass((SadlCondition) spr, prop, propType);
						addPropertyRange(propType, prop, condCls, RangeValueType.CLASS_OR_DT, spr); // use default?
						// TODO don't we need to add this class as superclass??
						retProp = prop;
					} else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
						ObjectProperty prop = getOrCreateObjectProperty(propUri);
						OntClass condCls = sadlConditionToOntClass((SadlCondition) spr, prop, propType);
						// TODO don't we need to add this class as superclass??
						retProp = prop;
						// throw new JenaProcessorException("SadlCondition on data type property not
						// handled");
					} else {
						throw new JenaProcessorException("Invalid property type: " + propType.toString());
					}
				} else if (spr instanceof SadlTypeAssociation) {
					SadlTypeReference domain = ((SadlTypeAssociation) spr).getDomain();
					OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
					ObjectProperty prop = getOrCreateObjectProperty(propUri);
					if (domainrsrc != null) {
						addPropertyDomain(prop, domainrsrc, domain);
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
				} else if (spr instanceof SadlIsAnnotation) {
					retProp = getTheJenaModel().createAnnotationProperty(propUri);
				} else if (spr instanceof SadlIsTransitive) {
					OntProperty pr = getOrCreateObjectProperty(propUri);
					pr.convertToTransitiveProperty();
					retProp = getTheJenaModel().createTransitiveProperty(pr.getURI());
				} else {
					throw new JenaProcessorException(
							"Unhandled SadlPropertyRestriction type: " + spr.getClass().getCanonicalName());
				}
			} // end while
		} else if (element.getFrom() != null && element.getTo() != null) {
			SadlTypeReference fromTypeRef = element.getFrom();
			Object frm;
			try {
				frm = processExpression(fromTypeRef);
				SadlTypeReference toTypeRef = element.getTo();
				Object t = processExpression(toTypeRef);
				if (frm != null && t != null) {
					OntClass dmn;
					OntClass rng;
					if (frm instanceof OntClass) {
						dmn = (OntClass) frm;
					} else if (frm instanceof NamedNode) {
						dmn = getOrCreateOntClass(((NamedNode) frm).toFullyQualifiedString());
					} else {
						throw new JenaTransactionException("Valid domain not identified: " + frm.toString());
					}
					if (t instanceof OntClass) {
						rng = (OntClass) t;
					} else if (t instanceof NamedNode) {
						rng = getOrCreateOntClass(((NamedNode) t).toFullyQualifiedString());
					} else {
						throw new JenaTransactionException("Valid range not identified: " + t.toString());
					}
					OntProperty pr = createObjectProperty(propUri, null);
					addPropertyDomain(pr, dmn, toTypeRef);
					addPropertyRange(OntConceptType.CLASS_PROPERTY, pr, rng, RangeValueType.CLASS_OR_DT, element);
					retProp = pr;
				} else if (frm == null) {
					throw new JenaTransactionException("Valid domian not identified");
				} else if (t == null) {
					throw new JenaTransactionException("Valid range not identified");
				}
			} catch (TranslationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else {
			// No restrictions--this will become an rdf:Property
			retProp = createRdfProperty(propUri, null);
		}
		if (sr != null && retProp != null && sr.getAnnotations() != null && retProp.canAs(OntResource.class)) {
			addAnnotationsToResource(retProp.as(OntResource.class), sr.getAnnotations());
		}
		return retProp;
	}

	private void addLengthRestrictionsToList(OntResource rngRsrc, SadlDataTypeFacet facet) {
		// check for list length restrictions
		if (facet != null && rngRsrc.canAs(OntClass.class)) {
			if (facet.getLen() != null) {
				int len = Integer.parseInt(facet.getLen());
				HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null,
						getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_LENGTH_RESTRICTION_URI),
						getTheJenaModel().createTypedLiteral(len));
				rngRsrc.as(OntClass.class).addSuperClass(hvr);
			}
			if (facet.getMinlen() != null) {
				int minlen = Integer.parseInt(facet.getMinlen());
				HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null,
						getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_MINLENGTH_RESTRICTION_URI),
						getTheJenaModel().createTypedLiteral(minlen));
				rngRsrc.as(OntClass.class).addSuperClass(hvr);
			}
			if (facet.getMaxlen() != null && !facet.getMaxlen().equals("*")) {
				int maxlen = Integer.parseInt(facet.getMaxlen());
				HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null,
						getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_MAXLENGTH_RESTRICTION_URI),
						getTheJenaModel().createTypedLiteral(maxlen));
				rngRsrc.as(OntClass.class).addSuperClass(hvr);
			}
		}
	}

	private void addCardinalityRestriction(OntResource cls, Property retProp, int cardinality) {
		if (cls != null && cls.canAs(OntClass.class)) {
			CardinalityRestriction cr = getTheJenaModel().createCardinalityRestriction(null, retProp, cardinality);
			cls.as(OntClass.class).addSuperClass(cr);
		}
	}

	private String createUniqueDefaultValName(OntClass restricted, Property prop) throws PrefixNotFoundException {
		String nmBase = restricted.getLocalName() + "_" + prop.getLocalName() + "_default";
		String nm = getModelNamespace() + nmBase;
		int cntr = 0;
		while (getTheJenaModel().getIndividual(nm) != null) {
			nm = nmBase + ++cntr;
		}
		return nm;
	}

	private Individual createDefault(OntClass restricted, Property prop, RDFNode defValue, int level, EObject ref)
			throws Exception {
		if (defValue instanceof Individual) {
			OntClass instDefCls = getTheJenaModel().getOntClass(ResourceManager.ACUITY_DEFAULTS_NS + "ObjectDefault");
			if (instDefCls == null) {
				addError("Unable to find ObjectDefault in Defaults model", ref);
				return null;
			}
			Individual def = getTheJenaModel().createIndividual(createUniqueDefaultValName(restricted, prop),
					instDefCls);
			def.addProperty(getTheJenaModel().getOntProperty(ResourceManager.ACUITY_DEFAULTS_NS + "appliesToProperty"),
					prop);
			def.addProperty(getTheJenaModel().getOntProperty(ResourceManager.ACUITY_DEFAULTS_NS + "hasObjectDefault"),
					defValue);
			if (level > 0) {
				String hlpuri = ResourceManager.ACUITY_DEFAULTS_NS + "hasLevel";
				OntProperty hlp = getTheJenaModel().getOntProperty(hlpuri);
				if (hlp == null) {
					addError("Unable to find hasLevel property in Defaults model", ref);
					return null;
				}
				Literal defLvl = getTheJenaModel().createTypedLiteral(level);
				def.addProperty(hlp, defLvl);
			}
			return def;
		} else if (defValue instanceof Literal) {
			OntClass litDefCls = getTheJenaModel().getOntClass(ResourceManager.ACUITY_DEFAULTS_NS + "DataDefault");
			if (litDefCls == null) {
				addError("Unable to find DataDefault in Defaults model", ref);
				return null;
			}
			Individual def = getTheJenaModel()
					.createIndividual(modelNamespace + createUniqueDefaultValName(restricted, prop), litDefCls);
			def.addProperty(getTheJenaModel().getOntProperty(ResourceManager.ACUITY_DEFAULTS_NS + "appliesToProperty"),
					prop);
			def.addProperty(getTheJenaModel().getOntProperty(ResourceManager.ACUITY_DEFAULTS_NS + "hasDataDefault"),
					defValue);
			if (level > 0) {
				String hlpuri = ResourceManager.ACUITY_DEFAULTS_NS + "hasLevel";
				OntProperty hlp = getTheJenaModel().getOntProperty(hlpuri);
				if (hlp == null) {
					addError("Unable to find hasLevel in Defaults model", ref);
					return null;
				}
				Literal defLvl = getTheJenaModel().createTypedLiteral(level);
				def.addProperty(hlp, defLvl);
			}
			return def;
		}
		return null;
	}

	private EnumeratedClass sadlExplicitValuesToEnumeratedClass(EList<SadlExplicitValue> values)
			throws JenaProcessorException {
		List<RDFNode> nodevals = new ArrayList<RDFNode>();
		for (int i = 0; i < values.size(); i++) {
			SadlExplicitValue value = values.get(i);
			RDFNode nodeval = sadlExplicitValueToRdfNode(value, null, true);
			if (nodeval.canAs(Individual.class)) {
				nodevals.add(nodeval.as(Individual.class));
			} else {
				nodevals.add(nodeval);
			}
		}
		RDFNode[] enumedArray = nodevals.toArray(new RDFNode[nodevals.size()]);
		RDFList rdfl = getTheJenaModel().createList(enumedArray);
		EnumeratedClass enumCls = getTheJenaModel().createEnumeratedClass(null, rdfl);
		return enumCls;
	}

	private RDFNode sadlExplicitValueToRdfNode(SadlExplicitValue value, Property prop, boolean literalsAllowed)
			throws JenaProcessorException {
		if (value instanceof SadlResource) {
			String uri = getDeclarationExtensions().getConceptUri((SadlResource) value);
			com.hp.hpl.jena.rdf.model.Resource rsrc = getTheJenaModel().getResource(uri);
			return rsrc;
		}
		else {
			if (prop != null) {
				StmtIterator rngitr = getTheJenaModel().listStatements(prop, RDFS.range, (RDFNode)null);
				while (rngitr.hasNext()) {
					RDFNode rng = rngitr.nextStatement().getObject();
					if (rng instanceof com.hp.hpl.jena.rdf.model.Resource) {
						try {
							Literal litval = sadlExplicitValueToLiteral(value, (com.hp.hpl.jena.rdf.model.Resource) rng);
							rngitr.close();
							return litval;
						}
						catch (Exception e) {
							
						}
					}
				}
				addWarning("Can't find range of property to create typed Literal", value);
				return sadlExplicitValueToLiteral(value, null);
			}
			else {
				Literal litval = sadlExplicitValueToLiteral(value, prop);
				return litval;
			}
		}
	}

	private Property assignRangeToProperty(String propUri, OntConceptType propType, OntResource rngRsrc,
			RangeValueType rngValueType, SadlTypeReference rng) throws JenaProcessorException {
		Property retProp;
		if (propType.equals(OntConceptType.CLASS_PROPERTY) || propType.equals(OntConceptType.CLASS_LIST)
				|| propType.equals(OntConceptType.DATATYPE_LIST)) {
			OntClass rngCls = rngRsrc.asClass();
			ObjectProperty prop = getOrCreateObjectProperty(propUri);
			addPropertyRange(propType, prop, rngCls, rngValueType, rng);
			retProp = prop;
		} else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
			DatatypeProperty prop = getOrCreateDatatypeProperty(propUri);
			addPropertyRange(propType, prop, rngRsrc, rngValueType, rng);
			retProp = prop;
		} else if (propType.equals(OntConceptType.RDF_PROPERTY)) {
			OntProperty prop = getOrCreateRdfProperty(propUri);
			addPropertyRange(propType, prop, rngRsrc, rngValueType, rng);
			// getTheJenaModel().add(prop, RDFS.range, rngRsrc);
			retProp = prop;
		} else {
			throw new JenaProcessorException(
					"Property '" + propUri + "' of unexpected type '" + rngRsrc.toString() + "'");
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

	private void addPropertyRange(OntConceptType propType, OntProperty prop, RDFNode rngNode,
			RangeValueType rngValueType, EObject context) throws JenaProcessorException {
		OntResource rngResource = null;
		if (rngNode instanceof OntClass) {
			rngResource = rngNode.as(OntClass.class);
			if (prop.isDatatypeProperty()) {
				// this happens when the range is a union of Lists of primitive types
				getTheJenaModel().remove(prop, RDF.type, OWL.DatatypeProperty);
				getTheJenaModel().add(prop, RDF.type, OWL.ObjectProperty);
			}
		}
		// If ignoring UnittedQuantity, change any UnittedQuantity range to the range of
		// value and make the property an owl:DatatypeProperty
		// TODO this should probably work for any declared subclass of UnittedQuantity
		// and associated value restriction?
		if (ignoreUnittedQuantities && rngResource != null && rngResource.isURIResource()
				&& rngResource.canAs(OntClass.class)
				&& rngResource.getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
			com.hp.hpl.jena.rdf.model.Resource effectiveRng = getUnittedQuantityValueRange();
			rngNode = effectiveRng;
			rngResource = null;
			getTheJenaModel().remove(prop, RDF.type, OWL.ObjectProperty);
			getTheJenaModel().add(prop, RDF.type, OWL.DatatypeProperty);
		}

		RDFNode propOwlType = null;
		boolean rangeExists = false;
		boolean addNewRange = false;
		StmtIterator existingRngItr = getTheJenaModel().listStatements(prop, RDFS.range, (RDFNode) null);
		if (existingRngItr.hasNext()) {
			RDFNode existingRngNode = existingRngItr.next().getObject();
			rangeExists = true;
			// property already has a range know to this model
			if (rngNode.equals(existingRngNode) || (rngResource != null && rngResource.equals(existingRngNode))) {
				// do nothing-- rngNode is already in range
				return;
			}
			if (prop.isDatatypeProperty()) {
				String existingRange = stmtIteratorToObjectString(
						getTheJenaModel().listStatements(prop, RDFS.range, (RDFNode) null));
				addError(SadlErrorMessages.CANNOT_ASSIGN_EXISTING.get("range", nodeToString(prop), existingRange),
						context);
				return;
			}
			if (!rngNode.isResource()) {
				addError("Proposed range node '" + rngNode.toString()
						+ "' is not a Resource so cannot be added to create a union class as range", context);
				return;
			}
			if (existingRngNode.canAs(OntClass.class)) {
				// is the new range a subclass of the existing range, or vice versa?
				if ((rngResource != null && rngResource.canAs(OntClass.class)
						&& checkForSubclassing(rngResource.as(OntClass.class), existingRngNode.as(OntClass.class),
								context))
						|| rngNode.canAs(OntClass.class) && checkForSubclassing(rngNode.as(OntClass.class),
								existingRngNode.as(OntClass.class), context)) {
					StringBuilder sb = new StringBuilder(
							"This range is a subclass of the range which is already defined");
					String existingRange = nodeToString(existingRngNode);
					if (existingRange != null) {
						sb.append(" (");
						sb.append(existingRange);
						sb.append(") ");
					}
					sb.append(
							"; perhaps you meant to restrict values of this property on this class with an 'only has values of type' restriction?");
					addWarning(sb.toString(), context);
					return;
				}
			}

			boolean rangeInThisModel = false;
			StmtIterator inModelStmtItr = getTheJenaModel().getBaseModel().listStatements(prop, RDFS.range,
					(RDFNode) null);
			if (inModelStmtItr.hasNext()) {
				rangeInThisModel = true;
			}
			if (domainAndRangeAsUnionClasses) {
				// in this case we want to create a union class if necessary
				if (rangeInThisModel) {
					// this model (as opposed to imports) already has a range specified
					addNewRange = false;
					UnionClass newUnionClass = null;
					while (inModelStmtItr.hasNext()) {
						RDFNode rngThisModel = inModelStmtItr.nextStatement().getObject();
						if (rngThisModel.isResource()) {
							if (rngThisModel.canAs(OntResource.class)) {
								if (existingRngNode.toString().equals(rngThisModel.toString())) {
									rngThisModel = existingRngNode;
								}
								newUnionClass = createUnionClass(rngThisModel.as(OntResource.class),
										rngResource != null ? rngResource : rngNode.asResource());
								logger.debug(
										"Range '" + rngNode.toString() + "' added to property '" + prop.getURI() + "'");
								if (!newUnionClass.equals(rngThisModel)) {
									addNewRange = true;
									rngResource = newUnionClass;
								} else {
									rngNode = null;
								}
							} else {
								throw new JenaProcessorException(
										"Encountered non-OntResource in range of '" + prop.getURI() + "'");
							}
						} else {
							throw new JenaProcessorException(
									"Encountered non-Resource in range of '" + prop.getURI() + "'");
						}
					}
					if (addNewRange) {
						getTheJenaModel().remove(
								getTheJenaModel().getBaseModel().listStatements(prop, RDFS.range, (RDFNode) null));
						rngNode = newUnionClass;
					}
				} // end if existing range in this model
				else {
					inModelStmtItr.close();
					// check to see if this is something new
					do {
						if (existingRngNode.equals(rngNode)) {
							existingRngItr.close();
							return; // already in domain, nothing to add
						}
						if (existingRngItr.hasNext()) {
							existingRngNode = existingRngItr.next().getObject();
						} else {
							existingRngNode = null;
						}
					} while (existingRngNode != null);
				}
			} // end if domainAndRangeAsUnionClasses
			else {
				inModelStmtItr.close();
			}
			if (rangeExists && !rangeInThisModel) {
				addWarning(SadlErrorMessages.IMPORTED_RANGE_CHANGE.get(nodeToString(prop)), context);
			}
		} // end if existing range in any model, this or imports
		if (rngNode != null) {
			if (rngResource != null) {
				if (!domainAndRangeAsUnionClasses && rngResource instanceof UnionClass) {
					List<com.hp.hpl.jena.rdf.model.Resource> uclsmembers = getUnionClassMemebers(
							(UnionClass) rngResource);
					for (int i = 0; i < uclsmembers.size(); i++) {
						getTheJenaModel().add(prop, RDFS.range, uclsmembers.get(i));
						logger.debug("Range '" + uclsmembers.get(i).toString() + "' added to property '" + prop.getURI()
								+ "'");
					}
				} else {
					getTheJenaModel().add(prop, RDFS.range, rngResource);
					logger.debug("Range '" + rngResource.toString() + "' added to property '" + prop.getURI() + "'");
				}
				propOwlType = OWL.ObjectProperty;
			} else {
				com.hp.hpl.jena.rdf.model.Resource rngrsrc = rngNode.asResource();
				if (rngrsrc.hasProperty(RDF.type, RDFS.Datatype)) {
					propOwlType = OWL.DatatypeProperty;
				} else if (rngrsrc.canAs(OntClass.class)) {
					propOwlType = OWL.ObjectProperty;
				} else {
					propOwlType = OWL.DatatypeProperty;
				}
				getTheJenaModel().add(prop, RDFS.range, rngNode);
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
			sb.append(uriStringToString(obj.toString()));
		} else if (obj.canAs(UnionClass.class)) {
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
		} else if (obj.canAs(IntersectionClass.class)) {
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
		} else if (obj.isResource() && getTheJenaModel().getOntClass(SadlConstants.SADL_LIST_MODEL_LIST_URI) != null
				&& getTheJenaModel().contains(obj.asResource(), RDFS.subClassOf,
						getTheJenaModel().getOntClass(SadlConstants.SADL_LIST_MODEL_LIST_URI))) {
			NamedNode cn;
			try {
				cn = getTypedListType(obj);
				sb.append(cn.getName());
			} catch (TranslationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				sb.append("<null>");
			}
			sb.append(" List");
		} else {
			sb.append("<blank node>");
		}
		return sb.toString();
	}

	public String uriStringToString(String uri) {
		int sep = uri.lastIndexOf('#');
		if (sep > 0) {
			String ns = uri.substring(0, sep);
			String ln = uri.substring(sep + 1);
			// if the concept is in the current model just return the localname
			if (ns.equals(getModelName())) {
				return ln;
			}
			// get the prefix and if there is one generate qname
			String prefix = getConfigMgr().getGlobalPrefix(ns);
			if (prefix == null) {
				if (ns.equals(SadlConstants.SADL_IMPLICIT_MODEL_URI)) {
					prefix = SadlConstants.SADL_IMPLICIT_MODEL_PREFIX;
				}
			}
			if (prefix != null && prefix.length() > 0) {
				return prefix + ":" + ln;
			}
			return ln;
		}
		return uri;
	}

	private boolean checkForSubclassing(OntClass rangeCls, OntResource existingRange, EObject context)
			throws JenaProcessorException {
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

	public void addError(String msg, EObject context) {
		if (getIssueAcceptor() != null) {
			getIssueAcceptor().addError(msg, context);
			if (isSyntheticUri(null, getCurrentResource())) {
				if (getMetricsProcessor() != null) {
					getMetricsProcessor().addMarker(null, MetricsProcessor.ERROR_MARKER_URI,
							MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
				}
			}
		} else if (!generationInProgress) {
			System.err.println(msg);
		}
	}

	public void addWarning(String msg, EObject context) {
		if (getIssueAcceptor() != null) {
			getIssueAcceptor().addWarning(msg, context);
			if (isSyntheticUri(null, getCurrentResource())) {
				if (getMetricsProcessor() != null) {
					getMetricsProcessor().addMarker(null, MetricsProcessor.WARNING_MARKER_URI,
							MetricsProcessor.WARNING_MARKER_URI);
				}
			}
		} else if (!generationInProgress) {
			System.out.println(msg);
		}
	}

	public void addInfo(String msg, EObject context) {
		if (getIssueAcceptor() != null) {
			getIssueAcceptor().addInfo(msg, context);
			if (isSyntheticUri(null, getCurrentResource())) {
				if (getMetricsProcessor() != null) {
					getMetricsProcessor().addMarker(null, MetricsProcessor.INFO_MARKER_URI,
							MetricsProcessor.INFO_MARKER_URI);
				}
			}
		} else if (!generationInProgress) {
			System.out.println(msg);
		}
	}

	private void processSadlNecessaryAndSufficient(SadlNecessaryAndSufficient element) throws JenaProcessorException {
		OntResource rsrc = sadlTypeReferenceToOntResource(element.getSubject());
		OntClass supercls = null;
		if (rsrc != null) {
			supercls = rsrc.asClass();
		}
		OntClass rolecls = getOrCreateOntClass(getDeclarationExtensions().getConceptUri(element.getObject()));
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
			} else {
				throw new JenaProcessorException("Necessary and sufficient conditions appears to have invalid input.");
			}
		} else {
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
		while (dcitr.hasNext()) {
			SadlClassOrPropertyDeclaration decl = dcitr.next();
			Iterator<SadlResource> djitr = decl.getClassOrProperty().iterator();
			while (djitr.hasNext()) {
				SadlResource sr = djitr.next();
				String declUri = getDeclarationExtensions().getConceptUri(sr);
				if (declUri == null) {
					throw new JenaProcessorException(
							"Failed to get concept URI for SadlResource in processSadlDifferentFrom");
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
			Individual otherInst = getTheJenaModel().getIndividual(getDeclarationExtensions().getConceptUri(sr));
			differentFrom.add(otherInst);
		}
		RDFNode[] nodeArray = null;
		if (differentFrom.size() > 0) {
			nodeArray = differentFrom.toArray(new Individual[differentFrom.size()]);
		} else {
			throw new JenaProcessorException("Unexpect empty array in processSadlDifferentFrom");
		}
		RDFList differentMembers = getTheJenaModel().createList(nodeArray);
		getTheJenaModel().createAllDifferent(differentMembers);
		logger.debug("New all different from created");
	}

	private Individual processSadlInstance(SadlInstance element)
			throws JenaProcessorException, CircularDefinitionException {
		// this has two forms:
		// 1) <name> is a <type> ...
		// 2) a <type> <name> ....
		SadlTypeReference type = element.getType();
		boolean isList = typeRefIsList(type);
		SadlResource sr = sadlResourceFromSadlInstance(element);
		Individual inst = null;
		String instUri = null;
		OntConceptType subjType = null;
		boolean isActuallyClass = false;
		if (sr != null) {
			instUri = getDeclarationExtensions().getConceptUri(sr);
			if (instUri == null) {
				throw new JenaProcessorException("Failed to get concept URI of SadlResource in processSadlInstance");
			}
			subjType = getDeclarationExtensions().getOntConceptType(sr);
			if (subjType.equals(OntConceptType.CLASS)) {
				// This is really a class so don't treat as an instance
				OntClass actualClass = getOrCreateOntClass(instUri);
				isActuallyClass = true;
				inst = actualClass.asIndividual();
			}
		}
		OntClass cls = null;
		if (!isActuallyClass) {
			if (type != null) {
				if (type instanceof SadlPrimitiveDataType) {
					com.hp.hpl.jena.rdf.model.Resource rsrc = sadlTypeReferenceToResource(type);
					if (isList) {
						if (rsrc instanceof OntClass) {
							// List subclass already created
							cls = (OntClass)rsrc;
						}
						else {
							try {
								cls = getOrCreateListSubclass(null, rsrc.getURI(), type.eResource(), null);
							} catch (JenaProcessorException e) {
								addError(e.getMessage(), type);
							}
						}
					} else {
						// AATIM-2306 If not list, generate error when creating instances of primitive
						// data types.
						addError(
								"Invalid to create an instance of a primitive datatype ( \'"
										+ ((SadlPrimitiveDataType) type).getPrimitiveType().toString()
										+ "\' in this case).  Instances of primitive datatypes exist implicitly.",
								type);
					}
				} else {
					OntResource or = sadlTypeReferenceToOntResource(type);
					if (or != null && or.canAs(OntClass.class)) {
						cls = or.asClass();
					} else if (or instanceof Individual) {
						inst = (Individual) or;
					}
				}
			}
			if (inst == null) {
				if (cls != null) {
					inst = createIndividual(instUri, cls);
				} else if (instUri != null) {
					inst = createIndividual(instUri, (OntClass) null);
				} else {
					throw new JenaProcessorException("Can't create an unnamed instance with no class given");
				}
			}
		}

		Iterator<SadlPropertyInitializer> itr = element.getPropertyInitializers().iterator();
		while (itr.hasNext()) {
			SadlPropertyInitializer propinit = itr.next();
			SadlResource prop = propinit.getProperty();
			OntConceptType propType = getDeclarationExtensions().getOntConceptType(prop);
			if (subjType != null && subjType.equals(OntConceptType.CLASS)
					&& !(propType.equals(OntConceptType.ANNOTATION_PROPERTY)) && // only a problem if not an annotation
																					// property
					!getOwlFlavor().equals(SadlConstants.OWL_FLAVOR.OWL_FULL)) {
				addWarning(SadlErrorMessages.CLASS_PROPERTY_VALUE_OWL_FULL.get(), element);
			}
			EObject val = propinit.getValue();
			if (val != null) {
				try {
					if (getModelValidator() != null) {
						StringBuilder error = new StringBuilder();
						if (!getModelValidator().checkPropertyValueInRange(getTheJenaModel(), sr, prop, val, error)) {
							issueAcceptor.addError(error.toString(), propinit);
						}
					}
				} catch (DontTypeCheckException e) {
					// do nothing
				} catch (PropertyWithoutRangeException e) {
					String propUri = getDeclarationExtensions().getConceptUri(prop);
					if (propUri != null) {
						if (!propUri.equals(SadlConstants.SADL_IMPLICIT_MODEL_IMPLIED_PROPERTY_URI)) {
							issueAcceptor.addWarning(SadlErrorMessages.PROPERTY_WITHOUT_RANGE
									.get(getDeclarationExtensions().getConcreteName(prop)), propinit);
						}
					}
				} catch (Exception e) {
					throw new JenaProcessorException("Unexpected error checking value in range", e);
				}
				assignInstancePropertyValue(inst, cls, prop, val);
			} else {
				addError("no value found", propinit);
			}
		}
		SadlValueList listInitializer = element.getListInitializer();
		if (listInitializer != null) {
			if (listInitializer.getExplicitValues().isEmpty()) {
				addError(SadlErrorMessages.EMPTY_LIST_DEFINITION.get(), element);
			} else {
				if (cls == null) {
					NamedNode cn;
					try {
						cn = getTypedListType(inst);
						if (cn != null) {
							cls = getTheJenaModel().getOntClass(cn.toFullyQualifiedString());
						}
					} catch (TranslationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				if (cls != null) {
					addListValues(inst, cls, listInitializer);
				} else {
					throw new JenaProcessorException("Unable to find type of list '" + inst.toString() + "'");
				}
			}
		}
		return inst;
	}

	private OWL_FLAVOR getOwlFlavor() {
		return owlFlavor;
	}

	@Override
	public NamedNode getTypedListType(RDFNode node) throws TranslationException {
		if (node.isResource()) {
			StmtIterator sitr = theJenaModel.listStatements(node.asResource(), RDFS.subClassOf, (RDFNode) null);
			while (sitr.hasNext()) {
				RDFNode supercls = sitr.nextStatement().getObject();
				if (supercls.isResource()) {
					if (supercls.asResource().hasProperty(OWL.onProperty,
							theJenaModel.getResource(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
						Statement avfstmt = supercls.asResource().getProperty(OWL.allValuesFrom);
						if (avfstmt != null) {
							RDFNode type = avfstmt.getObject();
							if (type.isURIResource()) {
								NamedNode tctype = validateNamedNode(new NamedNode(type.asResource().getURI(),
										ontConceptTypeToNodeType(OntConceptType.CLASS_LIST)));
								sitr.close();
								return tctype;
							}
						}
					}
				}
			}
			// maybe it's an instance
			if (node.asResource().canAs(Individual.class)) {
				ExtendedIterator<com.hp.hpl.jena.rdf.model.Resource> itr = node.asResource().as(Individual.class)
						.listRDFTypes(true);
				while (itr.hasNext()) {
					com.hp.hpl.jena.rdf.model.Resource r = itr.next();
					sitr = theJenaModel.listStatements(r, RDFS.subClassOf, (RDFNode) null);
					while (sitr.hasNext()) {
						RDFNode supercls = sitr.nextStatement().getObject();
						if (supercls.isResource()) {
							if (supercls.asResource().hasProperty(OWL.onProperty,
									theJenaModel.getResource(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
								Statement avfstmt = supercls.asResource().getProperty(OWL.allValuesFrom);
								if (avfstmt != null) {
									RDFNode type = avfstmt.getObject();
									if (type.isURIResource()) {
										NamedNode tctype = validateNamedNode(new NamedNode(type.asResource().getURI(),
												ontConceptTypeToNodeType(OntConceptType.CLASS)));
										sitr.close();
										return tctype;
									}
								}
							}
						}
					}
				}
			}
		}
		return null;
	}

	public ConceptName createTypedConceptName(String conceptUri, OntConceptType conceptType) {
		ConceptName cn = new ConceptName(conceptUri);
		if (conceptType.equals(OntConceptType.CLASS)) {
			cn.setType(ConceptType.ONTCLASS);
		} else if (conceptType.equals(OntConceptType.ANNOTATION_PROPERTY)) {
			cn.setType(ConceptType.ANNOTATIONPROPERTY);
		} else if (conceptType.equals(OntConceptType.DATATYPE_PROPERTY)) {
			cn.setType(ConceptType.DATATYPEPROPERTY);
		} else if (conceptType.equals(OntConceptType.INSTANCE)) {
			cn.setType(ConceptType.INDIVIDUAL);
		} else if (conceptType.equals(OntConceptType.CLASS_PROPERTY)) {
			cn.setType(ConceptType.OBJECTPROPERTY);
		} else if (conceptType.equals(OntConceptType.DATATYPE)) {
			cn.setType(ConceptType.RDFDATATYPE);
		} else if (conceptType.equals(OntConceptType.RDF_PROPERTY)) {
			cn.setType(ConceptType.RDFPROPERTY);
		} else if (conceptType.equals(OntConceptType.VARIABLE)) {
			cn.setType(ConceptType.VARIABLE);
		} else if (conceptType.equals(OntConceptType.FUNCTION_DEFN)) {
			cn.setType(ConceptType.FUNCTION_DEFN);
		}
		return cn;
	}

	public NamedNode createNamedNode(String conceptUri, OntConceptType conceptType) {
		NamedNode cn = new NamedNode(conceptUri);
		if (conceptType.equals(OntConceptType.CLASS)) {
			cn.setNodeType(NodeType.ClassNode);
		} else if (conceptType.equals(OntConceptType.ANNOTATION_PROPERTY)) {
			cn.setNodeType(NodeType.AnnotationProperty);
		} else if (conceptType.equals(OntConceptType.DATATYPE_PROPERTY)) {
			cn.setNodeType(NodeType.DataTypeProperty);
		} else if (conceptType.equals(OntConceptType.INSTANCE)) {
			cn.setNodeType(NodeType.InstanceNode);
		} else if (conceptType.equals(OntConceptType.CLASS_PROPERTY)) {
			cn.setNodeType(NodeType.ObjectProperty);
		} else if (conceptType.equals(OntConceptType.DATATYPE)) {
			cn.setNodeType(NodeType.DataTypeNode);
		} else if (conceptType.equals(OntConceptType.RDF_PROPERTY)) {
			cn.setNodeType(NodeType.PropertyNode);
		} else if (conceptType.equals(OntConceptType.VARIABLE)) {
			cn.setNodeType(NodeType.VariableNode);
		} else if (conceptType.equals(OntConceptType.FUNCTION_DEFN)) {
			cn.setNodeType(NodeType.FunctionNode);
		}
		return cn;
	}

	private boolean typeRefIsList(SadlTypeReference type) throws JenaProcessorException {
		boolean isList = false;
		if (type instanceof SadlSimpleTypeReference) {
			isList = ((SadlSimpleTypeReference) type).isList();
		} else if (type instanceof SadlPrimitiveDataType) {
			isList = ((SadlPrimitiveDataType) type).isList();
		}
		if (isList) {
			try {
				importSadlListModel(type.eResource());
			} catch (ConfigurationException e) {
				throw new JenaProcessorException("Unable to load List model", e);
			}
		}
		return isList;
	}

	private void addListValues(Individual inst, OntClass cls, SadlValueList listInitializer) {
		com.hp.hpl.jena.rdf.model.Resource to = null;
		ExtendedIterator<OntClass> scitr = cls.listSuperClasses(true);
		while (scitr.hasNext()) {
			OntClass sc = scitr.next();
			if (sc.isRestriction()
					&& ((sc.as(Restriction.class)).isAllValuesFromRestriction() && sc.as(AllValuesFromRestriction.class)
							.onProperty(getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI)))) {
				to = sc.as(AllValuesFromRestriction.class).getAllValuesFrom();
				break;
			}
		}
		if (to == null) {
			// addError("No 'to' resource found in restriction of List subclass",
			// listInitializer);
		}
		Iterator<SadlExplicitValue> values = listInitializer.getExplicitValues().iterator();
		addValueToList(null, inst, cls, to, values);
	}

	private Individual addValueToList(Individual lastInst, Individual inst, OntClass cls,
			com.hp.hpl.jena.rdf.model.Resource type, Iterator<SadlExplicitValue> valueIterator) {
		if (inst == null) {
			inst = getTheJenaModel().createIndividual(cls);
		}
		SadlExplicitValue val = valueIterator.next();
		if (val instanceof SadlResource) {
			Individual listInst;
			try {
				if (type.canAs(OntClass.class)) {
					listInst = createIndividual((SadlResource) val, type.as(OntClass.class));
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
					getTheJenaModel().add(inst, getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI),
							listInst);
				} else {
					addError("The type of the list could not be converted to a class.", val);
				}
			} catch (JenaProcessorException e) {
				addError(e.getMessage(), val);
			} catch (TranslationException e) {
				addError(e.getMessage(), val);
			}
		} else {
			Literal lval;
			try {
				lval = sadlExplicitValueToLiteral((SadlExplicitValue) val, type);
				getTheJenaModel().add(inst, getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI),
						lval);
			} catch (JenaProcessorException e) {
				addError(e.getMessage(), val);
			}
		}
		if (valueIterator.hasNext()) {
			Individual rest = addValueToList(inst, null, cls, type, valueIterator);
			getTheJenaModel().add(inst, getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_REST_URI), rest);
		}
		return inst;
	}

	private void assignInstancePropertyValue(Individual inst, OntClass cls, SadlResource prop, EObject val)
			throws JenaProcessorException, CircularDefinitionException {
		OntConceptType type;
		try {
			type = getDeclarationExtensions().getOntConceptType(prop);
		} catch (CircularDefinitionException e) {
			type = e.getDefinitionType();
			addError(e.getMessage(), prop);
		}
		String propuri = getDeclarationExtensions().getConceptUri(prop);
		if (type.equals(OntConceptType.CLASS_PROPERTY)) {
			OntProperty oprop = getTheJenaModel().getOntProperty(propuri);
			if (oprop == null) {
				addError(SadlErrorMessages.PROPERTY_NOT_EXIST.get(propuri), prop);
			} else {
				if (val instanceof SadlInstance) {
					Individual instval = processSadlInstance((SadlInstance) val);
					OntClass uQCls = getTheJenaModel()
							.getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
					if (uQCls != null && instval.hasRDFType(uQCls) && ignoreUnittedQuantities) {
						if (val instanceof SadlNestedInstance) {
							Iterator<SadlPropertyInitializer> propinititr = ((SadlNestedInstance) val)
									.getPropertyInitializers().iterator();
							while (propinititr.hasNext()) {
								EObject pval = propinititr.next().getValue();
								if (pval instanceof SadlNumberLiteral) {
									com.hp.hpl.jena.rdf.model.Resource effectiveRng = getUnittedQuantityValueRange();
									Literal lval = sadlExplicitValueToLiteral((SadlNumberLiteral) pval, effectiveRng);
									if (lval != null) {
										addInstancePropertyValue(inst, oprop, lval, val);
									}
								}
							}
						}
					} else {
						addInstancePropertyValue(inst, oprop, instval, val);
					}
				} else if (val instanceof SadlResource) {
					String uri = getDeclarationExtensions().getConceptUri((SadlResource) val);
					com.hp.hpl.jena.rdf.model.Resource rsrc = getTheJenaModel().getResource(uri);
					if (rsrc.canAs(Individual.class)) {
						addInstancePropertyValue(inst, oprop, rsrc.as(Individual.class), val);
					} else {
						throw new JenaProcessorException(
								"unhandled value type SadlResource that isn't an instance (URI is '" + uri + "')");
					}
				} else if (val instanceof SadlExplicitValue) {
					OntResource rng = oprop.getRange();
					if (val instanceof SadlNumberLiteral && ((SadlNumberLiteral) val).getUnit() != null) {
						if (!ignoreUnittedQuantities) {
							String unit = ((SadlNumberLiteral) val).getUnit();
							if (rng != null) {
								if (rng.canAs(OntClass.class)
										&& checkForSubclassing(rng.as(OntClass.class), getTheJenaModel().getOntClass(
												SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI), val)) {
									addUnittedQuantityAsInstancePropertyValue(inst, oprop, rng,
											((SadlNumberLiteral) val).getLiteralNumber(), unit);
								} else {
									addError(SadlErrorMessages.UNITTED_QUANTITY_ERROR.toString(), val);
								}
							} else {
								addUnittedQuantityAsInstancePropertyValue(inst, oprop, rng,
										((SadlNumberLiteral) val).getLiteralNumber(), unit);
							}
						} else {
							com.hp.hpl.jena.rdf.model.Resource effectiveRng = getUnittedQuantityValueRange();
							Literal lval = sadlExplicitValueToLiteral((SadlExplicitValue) val, effectiveRng);
							if (lval != null) {
								addInstancePropertyValue(inst, oprop, lval, val);
							}
						}
					} else {
						if (rng == null) {
							// this isn't really an ObjectProperty--should probably be an rdf:Property
							Literal lval = sadlExplicitValueToLiteral((SadlExplicitValue) val, null);
							addInstancePropertyValue(inst, oprop, lval, val);
						} else {
							addError("A SadlExplicitValue is given to an an ObjectProperty", val);
						}
					}
				} else if (val instanceof SadlValueList) {
					// EList<SadlExplicitValue> vals = ((SadlValueList)val).getExplicitValues();
					addListValues(inst, cls, (SadlValueList) val);
				} else {
					throw new JenaProcessorException("unhandled value type for object property");
				}
			}
		} else if (type.equals(OntConceptType.DATATYPE_PROPERTY)) {
			DatatypeProperty dprop = getTheJenaModel().getDatatypeProperty(propuri);
			if (dprop == null) {
				// dumpModel(getTheJenaModel());
				addError(SadlErrorMessages.PROPERTY_NOT_EXIST.get(propuri), prop);
			} else {
				if (val instanceof SadlValueList) {
					// EList<SadlExplicitValue> vals = ((SadlValueList)val).getExplicitValues();
					addListValues(inst, cls, (SadlValueList) val);
				} else if (val instanceof SadlExplicitValue) {
					Literal lval = sadlExplicitValueToLiteral((SadlExplicitValue) val, dprop.getRange());
					if (lval != null) {
						addInstancePropertyValue(inst, dprop, lval, val);
					}
				} else {
					addError("Invalid value (" + val.getClass().getCanonicalName() + ") for data property", val);
					throw new JenaProcessorException("unhandled value type for data property");
				}
			}
		} else if (type.equals(OntConceptType.ANNOTATION_PROPERTY)) {
			AnnotationProperty annprop = getTheJenaModel().getAnnotationProperty(propuri);
			if (annprop == null) {
				addError(SadlErrorMessages.PROPERTY_NOT_EXIST.get(propuri), prop);
			} else {
				RDFNode rsrcval;
				if (val instanceof SadlResource) {
					String uri = getDeclarationExtensions().getConceptUri((SadlResource) val);
					rsrcval = getTheJenaModel().getResource(uri);
					OntConceptType valueType = getDeclarationExtensions().getOntConceptType((SadlResource)val);
			          if(propuri.equals(SadlConstants.SADL_IMPLICIT_MODEL_URI + "#reference_class") && valueType.equals(OntConceptType.VARIABLE)) 
			          {
			            addError("Undefined class", val);
			              
			          }
				} else if (val instanceof SadlInstance) {
					rsrcval = processSadlInstance((SadlInstance) val);
				} else if (val instanceof SadlExplicitValue) {
					rsrcval = sadlExplicitValueToLiteral((SadlExplicitValue) val, null);
				} else {
					throw new JenaProcessorException(SadlErrorMessages.UNHANDLED.get(val.getClass().getCanonicalName(),
							"unable to handle annotation value"));
				}
				addInstancePropertyValue(inst, annprop, rsrcval, val);
			}
		} else if (type.equals(OntConceptType.RDF_PROPERTY)) {
			Property rdfprop = getTheJenaModel().getProperty(propuri);
			if (rdfprop == null) {
				addError(SadlErrorMessages.PROPERTY_NOT_EXIST.get(propuri), prop);
			}
			RDFNode rsrcval;
			if (val instanceof SadlResource) {
				String uri = getDeclarationExtensions().getConceptUri((SadlResource) val);
				rsrcval = getTheJenaModel().getResource(uri);
			} else if (val instanceof SadlInstance) {
				rsrcval = processSadlInstance((SadlInstance) val);
			} else if (val instanceof SadlExplicitValue) {
				rsrcval = sadlExplicitValueToLiteral((SadlExplicitValue) val, null);
			} else {
				throw new JenaProcessorException(
						"unable to handle rdf property value of type '" + val.getClass().getCanonicalName() + "')");
			}
			addInstancePropertyValue(inst, rdfprop, rsrcval, val);
		} else if (type.equals(OntConceptType.VARIABLE)) {
			// a variable for a property type is only valid in a rule or query.
			if (getTarget() == null || getTarget() instanceof Test) {
				addError("Variable can be used for property only in queries and rules", val);
			}
		} else {
			throw new JenaProcessorException("unhandled property type");
		}
	}

	private com.hp.hpl.jena.rdf.model.Resource getUnittedQuantityValueRange() {
		com.hp.hpl.jena.rdf.model.Resource effectiveRng = getTheJenaModel()
				.getOntProperty(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI).getRange();
		if (effectiveRng == null) {
			effectiveRng = XSD.decimal;
		}
		return effectiveRng;
	}

	private void addInstancePropertyValue(Individual inst, Property prop, RDFNode value, EObject ctx) {
		if (prop.getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_IMPLIED_PROPERTY_URI)) {
			// check for ambiguity through duplication of property range
			if (value.canAs(OntProperty.class)) {
				NodeIterator ipvs = inst.listPropertyValues(prop);
				if (ipvs.hasNext()) {
					List<OntResource> valueRngLst = new ArrayList<OntResource>();
					ExtendedIterator<? extends OntResource> vitr = value.as(OntProperty.class).listRange();
					while (vitr.hasNext()) {
						valueRngLst.add(vitr.next());
					}
					vitr.close();
					boolean overlap = false;
					while (ipvs.hasNext()) {
						RDFNode ipv = ipvs.next();
						if (ipv.canAs(OntProperty.class)) {
							ExtendedIterator<? extends OntResource> ipvitr = ipv.as(OntProperty.class).listRange();
							while (ipvitr.hasNext()) {
								OntResource ipvr = ipvitr.next();
								if (valueRngLst.contains(ipvr)) {
									addError("Ambiguous condition--multiple implied properties ("
											+ value.as(OntProperty.class).getLocalName() + ","
											+ ipv.as(OntProperty.class).getLocalName() + ") have the same range ("
											+ ipvr.getLocalName() + ")", ctx);
								}
							}
						}
					}
				}
			}
			addImpliedPropertyClass(inst);
		} else if (prop.getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_EXPANDED_PROPERTY_URI)) {
			addExpandedPropertyClass(inst);
		}
		inst.addProperty(prop, value);
		logger.debug("added value '" + value.toString() + "' to property '" + prop.toString() + "' for instance '"
				+ inst.toString() + "'");
	}

	private void addUnittedQuantityAsInstancePropertyValue(Individual inst, OntProperty oprop, OntResource rng,
			BigDecimal number, String unit) {
		addUnittedQuantityAsInstancePropertyValue(inst, oprop, rng, number.toPlainString(), unit);
	}

	private void addImpliedPropertyClass(Individual inst) {
		if (!allImpliedPropertyClasses.contains(inst)) {
			allImpliedPropertyClasses.add(inst);
		}
	}

	private void addExpandedPropertyClass(Individual inst) {
		if (!allExpandedPropertyClasses.contains(inst)) {
			allExpandedPropertyClasses.add(inst);
		}
	}

	private void addUnittedQuantityAsInstancePropertyValue(Individual inst, OntProperty oprop, OntResource rng,
			String literalNumber, String unit) {
		Individual unittedVal;
		if (rng != null && rng.canAs(OntClass.class)) {
			unittedVal = getTheJenaModel().createIndividual(rng.as(OntClass.class));
		} else {
			unittedVal = getTheJenaModel().createIndividual(
					getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI));
		}
		// TODO this may need to check for property restrictions on a subclass of
		// UnittedQuantity
		unittedVal.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI),
				getTheJenaModel().createTypedLiteral(literalNumber));
		unittedVal.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI),
				getTheJenaModel().createTypedLiteral(unit));
		inst.addProperty(oprop, unittedVal);
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
			sr = element.getInstance();
		}
		return sr;
	}

	private void processSadlDisjointClasses(SadlDisjointClasses element) throws JenaProcessorException {
		List<OntClass> disjointClses = new ArrayList<OntClass>();
		if (element.getClasses() != null) {
			Iterator<SadlResource> dcitr = element.getClasses().iterator();
			while (dcitr.hasNext()) {
				SadlResource sr = dcitr.next();
				String declUri = getDeclarationExtensions().getConceptUri(sr);
				if (declUri == null) {
					throw new JenaProcessorException(
							"Failed to get concept URI of SadlResource in processSadlDisjointClasses");
				}
				OntClass cls = getTheJenaModel().getOntClass(declUri);
				if (cls == null) {
					throw new JenaProcessorException("Failed to get class '" + declUri + "' from Jena model.");
				}
				disjointClses.add(cls.asClass());
			}
		}
		Iterator<SadlClassOrPropertyDeclaration> dcitr = element.getTypes().iterator();
		while (dcitr.hasNext()) {
			SadlClassOrPropertyDeclaration decl = dcitr.next();
			Iterator<SadlResource> djitr = decl.getClassOrProperty().iterator();
			while (djitr.hasNext()) {
				SadlResource sr = djitr.next();
				String declUri = getDeclarationExtensions().getConceptUri(sr);
				if (declUri == null) {
					throw new JenaProcessorException(
							"Failed to get concept URI of SadlResource in processSadlDisjointClasses");
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
		if (op != null && op.canAs(OntProperty.class)) {
			return op.as(OntProperty.class);
		}
		return createRdfProperty(propUri, null);
	}

	private boolean checkForExistingCompatibleDatatypeProperty(String propUri, RDFNode rngNode) {
		DatatypeProperty prop = getTheJenaModel().getDatatypeProperty(propUri);
		if (prop != null) {
			OntResource rng = prop.getRange();
			if (rng != null && rng.equals(rngNode)) {
				return true;
			}
		}
		return false;
	}

	private void addPropertyDomain(Property prop, OntResource cls, EObject context) throws JenaProcessorException {
		boolean addNewDomain = true;
		StmtIterator sitr = getTheJenaModel().listStatements(prop, RDFS.domain, (RDFNode) null);
		boolean domainExists = false;
		if (sitr.hasNext()) {
			RDFNode existingDomain = sitr.next().getObject();
			domainExists = true;
			// property already has a domain known to this model
			if (cls.equals(existingDomain)) {
				// do nothing--cls is already in domain
				return;
			}

			if (existingDomain.canAs(OntClass.class)) {
				// is the new domain a subclass of the existing domain?
				if (cls.canAs(OntClass.class)
						&& checkForSubclassing(cls.as(OntClass.class), existingDomain.as(OntClass.class), context)) {
					StringBuilder sb = new StringBuilder("This specified domain of '");
					sb.append(nodeToString(prop));
					sb.append("' is a subclass of the domain which is already defined");
					String dmnstr = nodeToString(existingDomain);
					if (dmnstr != null) {
						sb.append(" (");
						sb.append(dmnstr);
						sb.append(") ");
					}
					addWarning(sb.toString(), context);
					return;
				}
			}

			boolean domainInThisModel = false;
			StmtIterator inModelStmtItr = getTheJenaModel().getBaseModel().listStatements(prop, RDFS.domain,
					(RDFNode) null);
			if (inModelStmtItr.hasNext()) {
				domainInThisModel = true;
			}
			if (domainAndRangeAsUnionClasses) {
				// in this case we want to create a union class if necessary
				if (domainInThisModel) {
					// this model (as opposed to imports) already has a domain specified
					addNewDomain = false;
					UnionClass newUnionClass = null;
					while (inModelStmtItr.hasNext()) {
						RDFNode dmn = inModelStmtItr.nextStatement().getObject();
						if (dmn.isResource()) { // should always be a Resource
							if (dmn.canAs(OntResource.class)) {
								if (existingDomain.toString().equals(dmn.toString())) {
									dmn = existingDomain;
								}
								newUnionClass = createUnionClass(dmn.as(OntResource.class), cls);
								logger.debug(
										"Domain '" + cls.toString() + "' added to property '" + prop.getURI() + "'");
								if (!newUnionClass.equals(dmn)) {
									addNewDomain = true;
								}
							} else {
								throw new JenaProcessorException(
										"Encountered non-OntResource in domain of '" + prop.getURI() + "'");
							}
						} else {
							throw new JenaProcessorException(
									"Encountered non-Resource in domain of '" + prop.getURI() + "'");
						}
					}
					if (addNewDomain) {
						getTheJenaModel().remove(
								getTheJenaModel().getBaseModel().listStatements(prop, RDFS.domain, (RDFNode) null));
						cls = newUnionClass;
					}
				} // end if existing domain in this model
				else {
					inModelStmtItr.close();
					// check to see if this is something new
					do {
						if (existingDomain.equals(cls)) {
							sitr.close();
							return; // already in domain, nothing to add
						}
						if (sitr.hasNext()) {
							existingDomain = sitr.next().getObject();
						} else {
							existingDomain = null;
						}
					} while (existingDomain != null);
				}
			} // end if domainAndRangeAsUnionClasses
			else {
				inModelStmtItr.close();
			}
			if (domainExists && !domainInThisModel) {
				addWarning(SadlErrorMessages.IMPORTED_DOMAIN_CHANGE.get(nodeToString(prop)), context);
			}
		} // end if existing domain in any model, this or imports
		if (cls != null) {
			if (!domainAndRangeAsUnionClasses && cls instanceof UnionClass) {
				List<com.hp.hpl.jena.rdf.model.Resource> uclsmembers = getUnionClassMemebers((UnionClass) cls);
				for (int i = 0; i < uclsmembers.size(); i++) {
					getTheJenaModel().add(prop, RDFS.domain, uclsmembers.get(i));
					logger.debug(
							"Domain '" + uclsmembers.get(i).toString() + "' added to property '" + prop.getURI() + "'");
				}
			} else if (addNewDomain) {
				getTheJenaModel().add(prop, RDFS.domain, cls);
				logger.debug("Domain '" + cls.toString() + "' added to property '" + prop.getURI() + "'");
				logger.debug("Domain of '" + prop.toString() + "' is now: " + nodeToString(cls));
			}
		} else {
			logger.debug("Domain is not defined for property '" + prop.toString() + "'");
		}
	}

	private List<com.hp.hpl.jena.rdf.model.Resource> getUnionClassMemebers(UnionClass cls) {
		List<com.hp.hpl.jena.rdf.model.Resource> members = null;
		ExtendedIterator<? extends com.hp.hpl.jena.rdf.model.Resource> itr = ((UnionClass) cls).listOperands();
		while (itr.hasNext()) {
			com.hp.hpl.jena.rdf.model.Resource ucls = itr.next();
			if (ucls instanceof UnionClass || ucls.canAs(UnionClass.class)) {
				List<com.hp.hpl.jena.rdf.model.Resource> nested = getUnionClassMemebers(ucls.as(UnionClass.class));
				if (members == null) {
					members = nested;
				} else {
					members.addAll(nested);
				}
			} else {
				if (members == null)
					members = new ArrayList<com.hp.hpl.jena.rdf.model.Resource>();
				members.add(ucls);
			}
		}
		if (cls.isAnon()) {
			for (int i = 0; i < members.size(); i++) {
				((UnionClass) cls).removeOperand(members.get(i));
			}
			getTheJenaModel().removeAll(cls, null, null);
			getTheJenaModel().removeAll(null, null, cls);
			cls.remove();
		}
		return members;
	}

	private OntResource createUnionOfClasses(OntResource cls, ExtendedIterator<? extends OntResource> ditr)
			throws JenaProcessorException {
		OntResource unionClass = null;
		RDFList classes = null;
		boolean allEqual = true;
		while (ditr.hasNext()) {
			OntResource existingCls = ditr.next();
			if (!existingCls.canAs(OntResource.class)) {
				throw new JenaProcessorException(
						"Unable to '" + existingCls.toString() + "' to OntResource to put into union of classes");
			}
			if (existingCls.equals(cls)) {
				continue;
			} else {
				allEqual = false;
			}
			if (existingCls.as(OntResource.class).canAs(UnionClass.class)) {
				if (classes != null) {
					classes.append(existingCls.as(UnionClass.class).getOperands());
				} else {
					try {
						existingCls.as(UnionClass.class).addOperand(cls);
						unionClass = existingCls.as(UnionClass.class);
					} catch (Exception e) {
						// don't know why this is happening
						logger.error("Union class error that hasn't been resolved or understood.");
						return cls;
					}
				}
			} else {
				if (classes == null) {
					classes = getTheJenaModel().createList();
				}
				classes = classes.with(existingCls.as(OntResource.class));
				classes = classes.with(cls);
			}
		}
		if (allEqual) {
			return cls;
		}
		if (classes != null) {
			unionClass = getTheJenaModel().createUnionClass(null, classes);
		}
		return unionClass;
	}

	private RDFNode primitiveDatatypeToRDFNode(String name) {
		return getTheJenaModel().getResource(XSD.getURI() + name);
	}

	private OntClass getOrCreateOntClass(String name) {
		OntClass cls = getTheJenaModel().getOntClass(name);
		if (cls == null) {
			cls = createOntClass(name, (OntClass) null);
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
		return createOntClass(newName, (OntClass) null);
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

	private OntClass getOrCreateListSubclass(String newName, String typeUri, Resource resource, SadlDataTypeFacet facet)
			throws JenaProcessorException {
		if (sadlListModel == null) {
			try {
				importSadlListModel(resource);
			} catch (Exception e) {
				e.printStackTrace();
				throw new JenaProcessorException("Failed to load SADL List model", e);
			}
		}
		OntClass lstcls = getTheJenaModel().getOntClass(SadlConstants.SADL_LIST_MODEL_LIST_URI);
		ExtendedIterator<OntClass> lscitr = lstcls.listSubClasses();
		while (lscitr.hasNext()) {
			OntClass scls = lscitr.next();
			if (newName != null && scls.isURIResource() && newName.equals(scls.getURI())) {
				// same class
				return scls;
			}
			if (newName == null && scls.isAnon()) {
				String restrictionTypeUri = "";
				int lengthRestriction = -1;
				int lengthMaxRestriction = -1;
				int lengthMinRestriction = -1;
				ExtendedIterator<OntClass> spcitr = scls.listSuperClasses(true);
				while (spcitr.hasNext()) {
					OntClass spcls = spcitr.next();
					if (spcls.isRestriction()) {
						Restriction restriction = spcls.asRestriction();
						OntProperty onprop = restriction.getOnProperty();
						if(restriction.isAllValuesFromRestriction()) {	
							if (onprop.isURIResource() && onprop.getURI().equals(SadlConstants.SADL_LIST_MODEL_FIRST_URI)) {
								com.hp.hpl.jena.rdf.model.Resource avf = restriction.asAllValuesFromRestriction().getAllValuesFrom();
								if(avf.isURIResource()) {
									restrictionTypeUri = avf.getURI();
								}
							}
						}
						if(restriction.isHasValueRestriction()) {
							RDFNode hasValue = restriction.asHasValueRestriction().getHasValue();
							if(onprop.isURIResource() && onprop.getURI().equals(SadlConstants.SADL_LIST_MODEL_LENGTH_RESTRICTION_URI)) {
								lengthRestriction = hasValue.asLiteral().getInt();
							}
							if(onprop.isURIResource() && onprop.getURI().equals(SadlConstants.SADL_LIST_MODEL_MAXLENGTH_RESTRICTION_URI)) {
								lengthMaxRestriction = hasValue.asLiteral().getInt();
							}
							if(onprop.isURIResource() && onprop.getURI().equals(SadlConstants.SADL_LIST_MODEL_MINLENGTH_RESTRICTION_URI)) {
								lengthMinRestriction = hasValue.asLiteral().getInt();
							}
						}
					}
				}
				if (restrictionTypeUri.equals(typeUri)) {
					if(facet == null) {
						spcitr.close();
						lscitr.close();
						return scls;
					}else {
						int length = facet.getLen() != null ? Integer.parseInt(facet.getLen()) : -1;
						int lengthMax = facet.getMaxlen() != null ? Integer.parseInt(facet.getMaxlen()) : -1;
						int lengthMin = facet.getMinlen() != null ? Integer.parseInt(facet.getMinlen()) : -1;
						if(lengthRestriction == length &&
						   lengthMaxRestriction == lengthMax &&
						   lengthMinRestriction == lengthMin) {
							spcitr.close();
							lscitr.close();
							return scls;
						}
					}
				}
			}
		}
		OntClass newcls = createOntClass(newName, lstcls);
		com.hp.hpl.jena.rdf.model.Resource typeResource = getTheJenaModel().getResource(typeUri);
		Property pfirst = getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI);
		AllValuesFromRestriction avf = getTheJenaModel().createAllValuesFromRestriction(null, pfirst, typeResource);
		newcls.addSuperClass(avf);
		Property prest = getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_REST_URI);
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
				// throw new JenaProcessorException("Unable to find super property '" +
				// superSRUri + "'");
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
				// throw new JenaProcessorException("Unable to find super property '" +
				// superSRUri + "'");
				if (superProp == null) {
					getTheJenaModel().createDatatypeProperty(superSRUri);
				}
			}
			newProp.addSuperProperty(superProp);
			logger.debug("    Datatype property '" + newProp.getURI() + "' given super property '" + superSRUri + "'");
		}
		return newProp;
	}

	private Individual createIndividual(SadlResource srsrc, OntClass type)
			throws JenaProcessorException, TranslationException {
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

	private Individual createIndividual(String newName, OntClass supercls) {
		Individual inst = getTheJenaModel().createIndividual(newName, supercls);
		logger.debug("New instance '" + (newName != null ? newName : "(bnode)") + "' created");
		return inst;
	}

	private OntResource sadlTypeReferenceToOntResource(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		com.hp.hpl.jena.rdf.model.Resource obj = sadlTypeReferenceToResource(sadlTypeRef);
		if (obj == null) {
			return null; // this happens when sadlTypeRef is a variable (even if unintended)
		}
		if (obj instanceof OntResource) {
			return (OntResource) obj;
		} else if (obj instanceof RDFNode) {
			if (((RDFNode) obj).canAs(OntResource.class)) {
				return ((RDFNode) obj).as(OntResource.class);
			}
		}
		throw new JenaProcessorException("Unable to convert SadlTypeReference '" + sadlTypeRef + "' to OntResource");
	}

	private com.hp.hpl.jena.rdf.model.Resource sadlTypeReferenceToResource(SadlTypeReference sadlTypeRef)
			throws JenaProcessorException {
		Object obj = sadlTypeReferenceToObject(sadlTypeRef);
		if (obj == null) {
			return null; // this happens when sadlTypeRef is a variable (even if unintended)
		}
		if (obj instanceof com.hp.hpl.jena.rdf.model.Resource) {
			return (com.hp.hpl.jena.rdf.model.Resource) obj;
		} else if (obj instanceof RDFNode) {
			if (((RDFNode) obj).canAs(com.hp.hpl.jena.rdf.model.Resource.class)) {
				return ((RDFNode) obj).as(com.hp.hpl.jena.rdf.model.Resource.class);
			}
		}
		throw new JenaProcessorException("Unable to convert SadlTypeReference '" + sadlTypeRef + "' to OntResource");
	}

	private ConceptName sadlSimpleTypeReferenceToConceptName(SadlTypeReference sadlTypeRef)
			throws JenaProcessorException {
		if (sadlTypeRef instanceof SadlSimpleTypeReference) {
			SadlResource strSR = ((SadlSimpleTypeReference) sadlTypeRef).getType();
			OntConceptType ctype;
			try {
				ctype = getDeclarationExtensions().getOntConceptType(strSR);
			} catch (CircularDefinitionException e) {
				ctype = e.getDefinitionType();
				addError(e.getMessage(), sadlTypeRef);
			}
			String strSRUri = getDeclarationExtensions().getConceptUri(strSR);
			if (strSRUri == null) {
				if (ctype.equals(OntConceptType.VARIABLE)) {
					// throw new JenaProcessorException("Failed to get variable URI of SadlResource
					// in sadlSimpleTypeReferenceToConceptName");
					// be silent? during clean these URIs won't be found
				}
				// throw new JenaProcessorException("Failed to get concept URI of SadlResource
				// in sadlSimpleTypeReferenceToConceptName");
				// be silent? during clean these URIs won't be found
				return null;
			}
			if (ctype.equals(OntConceptType.CLASS)) {
				ConceptName cn = new ConceptName(strSRUri);
				cn.setType(ConceptType.ONTCLASS);
				return cn;
			} else if (ctype.equals(OntConceptType.CLASS_LIST)) {
				ConceptName cn = new ConceptName(strSRUri);
				cn.setType(ConceptType.ONTCLASSLIST);
				return cn;
			} else if (ctype.equals(OntConceptType.DATATYPE_LIST)) {
				ConceptName cn = new ConceptName(strSRUri);
				cn.setType(ConceptType.RDFDATATYPELIST);
				return cn;
			} else if (ctype.equals(OntConceptType.INSTANCE)) {
				ConceptName cn = new ConceptName(strSRUri);
				cn.setType(ConceptType.INDIVIDUAL);
				return cn;
			} else if (ctype.equals(OntConceptType.DATATYPE)) {
				ConceptName cn = new ConceptName(strSRUri);
				cn.setType(ConceptType.RDFDATATYPE);
				return cn;
			} else if (ctype.equals(OntConceptType.CLASS_PROPERTY)) {
				ConceptName cn = new ConceptName(strSRUri);
				cn.setType(ConceptType.OBJECTPROPERTY);
				return cn;
			} else if (ctype.equals(OntConceptType.DATATYPE_PROPERTY)) {
				ConceptName cn = new ConceptName(strSRUri);
				cn.setType(ConceptType.DATATYPEPROPERTY);
				return cn;
			} else {
				throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri
						+ "' was of a type not yet handled: " + ctype.toString());
			}
		} else if (sadlTypeRef instanceof SadlPrimitiveDataType) {
			com.hp.hpl.jena.rdf.model.Resource trr = getSadlPrimitiveDataTypeResource(
					(SadlPrimitiveDataType) sadlTypeRef);
			ConceptName cn = new ConceptName(trr.getURI());
			cn.setType(ConceptType.RDFDATATYPE);
			return cn;
		} else {
			throw new JenaProcessorException("SadlTypeReference is not a URI resource");
		}
	}

	private OntConceptType sadlTypeReferenceOntConceptType(SadlTypeReference sadlTypeRef)
			throws CircularDefinitionException {
		if (sadlTypeRef instanceof SadlSimpleTypeReference) {
			SadlResource strSR = ((SadlSimpleTypeReference) sadlTypeRef).getType();
			return getDeclarationExtensions().getOntConceptType(strSR);
		} else if (sadlTypeRef instanceof SadlPrimitiveDataType) {
			return OntConceptType.DATATYPE;
		} else if (sadlTypeRef instanceof SadlPropertyCondition) {
			SadlResource sr = ((SadlPropertyCondition) sadlTypeRef).getProperty();
			return getDeclarationExtensions().getOntConceptType(sr);
		} else if (sadlTypeRef instanceof SadlUnionType || sadlTypeRef instanceof SadlIntersectionType) {
			return OntConceptType.CLASS;
		}
		return null;
	}

	protected Object sadlTypeReferenceToObject(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		OntResource rsrc = null;
		// TODO How do we tell if this is a union versus an intersection?
		if (sadlTypeRef instanceof SadlSimpleTypeReference) {
			SadlResource strSR = ((SadlSimpleTypeReference) sadlTypeRef).getType();
			// TODO check for proxy, i.e. unresolved references
			OntConceptType ctype;
			try {
				ctype = getDeclarationExtensions().getOntConceptType(strSR);
			} catch (CircularDefinitionException e) {
				ctype = e.getDefinitionType();
				addError(e.getMessage(), sadlTypeRef);
			}
			String strSRUri = getDeclarationExtensions().getConceptUri(strSR);
			if (strSRUri == null) {
				if (ctype.equals(OntConceptType.VARIABLE)) {
					addError("Range should not be a variable.", sadlTypeRef);
					return null;
				}
				throw new JenaProcessorException(
						"Failed to get concept URI of SadlResource in sadlTypeReferenceToObject");
			}
			if (ctype.equals(OntConceptType.CLASS)) {
				if (((SadlSimpleTypeReference) sadlTypeRef).isList()) {
					rsrc = getOrCreateListSubclass(null, strSRUri, sadlTypeRef.eResource(), null);
				} else {
					rsrc = getTheJenaModel().getOntClass(strSRUri);
					if (rsrc == null) {
						return createOntClass(strSRUri, (OntClass) null);
					}
				}
			} else if (ctype.equals(OntConceptType.CLASS_LIST)) {
				rsrc = getTheJenaModel().getOntClass(strSRUri);
				if (rsrc == null) {
					SadlResource declResource = getDeclarationExtensions().getDeclaration(strSR);
					if (declResource.eContainer() instanceof SadlClassOrPropertyDeclaration) {
						try {
							List<OntResource> listResources = processSadlClassOrPropertyDeclaration(
									(SadlClassOrPropertyDeclaration) declResource.eContainer());
							eobjectPreprocessed(declResource.eContainer());
							return listResources.get(0);
						} catch (TranslationException e) {
							e.printStackTrace();
						}
					} else {
						return getOrCreateListSubclass(strSRUri, strSRUri, strSR.eResource(), null);
					}
				}
			} else if (ctype.equals(OntConceptType.DATATYPE_LIST)) {
				rsrc = getTheJenaModel().getOntClass(strSRUri);
				if (rsrc == null) {
					return getOrCreateListSubclass(strSRUri, strSRUri, strSR.eResource(), null);
				}
			} else if (ctype.equals(OntConceptType.INSTANCE)) {
				rsrc = getTheJenaModel().getIndividual(strSRUri);
				if (rsrc == null) {
					// is it OK to create Individual without knowing class??
					return createIndividual(strSRUri, (OntClass) null);
				}
			} else if (ctype.equals(OntConceptType.DATATYPE)) {
				OntResource dt = getTheJenaModel().getOntResource(strSRUri);
				if (dt == null) {
					throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri
							+ "' not found; it should exist as there isn't enough information to create it.");
				}
				return dt;
			} else if (ctype.equals(OntConceptType.CLASS_PROPERTY)) {
				OntProperty otp = getTheJenaModel().getOntProperty(strSRUri);
				if (otp == null) {
					throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri
							+ "' not found; should have found an ObjectProperty");
				}
				return otp;
			} else if (ctype.equals(OntConceptType.DATATYPE_PROPERTY)) {
				OntProperty dtp = getTheJenaModel().getOntProperty(strSRUri);
				if (dtp == null) {
					throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri
							+ "' not found; should have found an DatatypeProperty");
				}
				return dtp;
			} else {
				throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri
						+ "' was of a type not yet handled: " + ctype.toString());
			}
		} else if (sadlTypeRef instanceof SadlPrimitiveDataType) {
			return processSadlPrimitiveDataType(null, (SadlPrimitiveDataType) sadlTypeRef, null);
		} else if (sadlTypeRef instanceof SadlPropertyCondition) {
			return processSadlPropertyCondition((SadlPropertyCondition) sadlTypeRef);
		} else if (sadlTypeRef instanceof SadlUnionType) {
			RDFNode lftNode = null;
			RDFNode rhtNode = null;
			SadlTypeReference lft = ((SadlUnionType) sadlTypeRef).getLeft();
			Object lftObj = sadlTypeReferenceToObject(lft);
			if (lftObj == null) {
				return null;
			}
			if (lftObj instanceof OntResource) {
				lftNode = ((OntResource) lftObj).asClass();
			} else {
				if (lftObj instanceof RDFNode) {
					lftNode = (RDFNode) lftObj;
				} else if (lftObj instanceof List) {
					// carry on: RDFNode list from nested union
				} else {
					throw new JenaProcessorException(
							"Union member of unsupported type: " + lftObj.getClass().getCanonicalName());
				}
			}
			SadlTypeReference rht = ((SadlUnionType) sadlTypeRef).getRight();
			Object rhtObj = sadlTypeReferenceToObject(rht);
			if (rhtObj == null) {
				return null;
			}
			if (rhtObj instanceof OntResource && ((OntResource) rhtObj).canAs(OntClass.class)) {
				rhtNode = ((OntResource) rhtObj).asClass();
			} else {
				if (rhtObj instanceof RDFNode) {
					rhtNode = (RDFNode) rhtObj;
				} else if (rhtObj instanceof List) {
					// carry on: RDFNode list from nested union
				} else {
					throw new JenaProcessorException(
							"Union member of unsupported type: " + rhtObj != null ? rhtObj.getClass().getCanonicalName()
									: "null");
				}
			}
			if (lftNode instanceof OntResource && rhtNode instanceof OntResource) {
				OntClass unionCls = createUnionClass(lftNode, rhtNode);
				return unionCls;
			} else if (lftObj instanceof List && rhtNode instanceof RDFNode) {
				((List) lftObj).add(rhtNode);
				return lftObj;
			} else if (lftObj instanceof RDFNode && rhtNode instanceof List) {
				((List) rhtNode).add(lftNode);
				return rhtNode;
			} else if (lftNode instanceof RDFNode && rhtNode instanceof RDFNode) {
				List<RDFNode> rdfdatatypelist = new ArrayList<RDFNode>();
				rdfdatatypelist.add((RDFNode) lftNode);
				rdfdatatypelist.add((RDFNode) rhtNode);
				return rdfdatatypelist;
			} else {
				throw new JenaProcessorException("Left and right sides of union are of incompatible types: "
						+ lftNode.toString() + " and " + rhtNode.toString());
			}
		} else if (sadlTypeRef instanceof SadlIntersectionType) {
			RDFNode lftNode = null;
			RDFNode rhtNode = null;
			SadlTypeReference lft = ((SadlIntersectionType) sadlTypeRef).getLeft();
			Object lftObj = sadlTypeReferenceToObject(lft);
			if (lftObj == null) {
				return null;
			}
			if (lftObj instanceof OntResource) {
				lftNode = ((OntResource) lftObj).asClass();
			} else {
				if (lftObj instanceof RDFNode) {
					lftNode = (RDFNode) lftObj;
				} else if (lftObj == null) {
					addError("SadlIntersectionType did not resolve to an ontology object (null)", sadlTypeRef);
				} else {
					throw new JenaProcessorException(
							"Intersection member of unsupported type: " + lftObj.getClass().getCanonicalName());
				}
			}
			SadlTypeReference rht = ((SadlIntersectionType) sadlTypeRef).getRight();
			if (rht == null) {
				throw new JenaProcessorException("No right-hand side to intersection");
			}
			Object rhtObj = sadlTypeReferenceToObject(rht);
			if (rhtObj == null) {
				return null;
			}
			if (rhtObj instanceof OntResource) {
				rhtNode = ((OntResource) rhtObj).asClass();
			} else {
				if (rhtObj instanceof RDFNode) {
					rhtNode = (RDFNode) rhtObj;
				} else {
					throw new JenaProcessorException(
							"Intersection member of unsupported type: " + rhtObj.getClass().getCanonicalName());
				}
			}
			if (lftNode instanceof OntResource && rhtNode instanceof OntResource) {
				OntClass intersectCls = createIntersectionClass(lftNode, rhtNode);
				return intersectCls;
			} else if (lftNode instanceof RDFNode && rhtNode instanceof RDFNode) {
				List<RDFNode> rdfdatatypelist = new ArrayList<RDFNode>();
				rdfdatatypelist.add((RDFNode) lftNode);
				rdfdatatypelist.add((RDFNode) rhtNode);
				return rdfdatatypelist;
			} else {
				throw new JenaProcessorException("Left and right sides of union are of incompatible types: "
						+ lftNode.toString() + " and " + rhtNode.toString());
			}
		}
		return rsrc;
	}

	private com.hp.hpl.jena.rdf.model.Resource processSadlPrimitiveDataType(SadlClassOrPropertyDeclaration element,
			SadlPrimitiveDataType sadlTypeRef, String newDatatypeUri) throws JenaProcessorException {
		com.hp.hpl.jena.rdf.model.Resource onDatatype = getSadlPrimitiveDataTypeResource(sadlTypeRef);
		if (sadlTypeRef.isList()) {
			onDatatype = getOrCreateListSubclass(null, onDatatype.toString(), sadlTypeRef.eResource(), null);
		}
		if (newDatatypeUri == null) {
			return onDatatype;
		}
		SadlDataTypeFacet facet = element.getFacet();
		OntClass datatype = createRdfsDatatype(newDatatypeUri, null, onDatatype, facet);
		return datatype;
	}

	private com.hp.hpl.jena.rdf.model.Resource getSadlPrimitiveDataTypeResource(SadlPrimitiveDataType sadlTypeRef)
			throws JenaProcessorException {
		SadlDataType pt = sadlTypeRef.getPrimitiveType();
		String typeStr = pt.getLiteral();
		com.hp.hpl.jena.rdf.model.Resource onDatatype;
		if (typeStr.equals(XSD.xstring.getLocalName()))
			onDatatype = XSD.xstring;
		else if (typeStr.equals(XSD.anyURI.getLocalName()))
			onDatatype = XSD.anyURI;
		else if (typeStr.equals(XSD.base64Binary.getLocalName()))
			onDatatype = XSD.base64Binary;
		else if (typeStr.equals(XSD.xbyte.getLocalName()))
			onDatatype = XSD.xbyte;
		else if (typeStr.equals(XSD.date.getLocalName()))
			onDatatype = XSD.date;
		else if (typeStr.equals(XSD.dateTime.getLocalName()))
			onDatatype = XSD.dateTime;
		else if (typeStr.equals(XSD.decimal.getLocalName()))
			onDatatype = XSD.decimal;
		else if (typeStr.equals(XSD.duration.getLocalName()))
			onDatatype = XSD.duration;
		else if (typeStr.equals(XSD.gDay.getLocalName()))
			onDatatype = XSD.gDay;
		else if (typeStr.equals(XSD.gMonth.getLocalName()))
			onDatatype = XSD.gMonth;
		else if (typeStr.equals(XSD.gMonthDay.getLocalName()))
			onDatatype = XSD.gMonthDay;
		else if (typeStr.equals(XSD.gYear.getLocalName()))
			onDatatype = XSD.gYear;
		else if (typeStr.equals(XSD.gYearMonth.getLocalName()))
			onDatatype = XSD.gYearMonth;
		else if (typeStr.equals(XSD.hexBinary.getLocalName()))
			onDatatype = XSD.hexBinary;
		else if (typeStr.equals(XSD.integer.getLocalName()))
			onDatatype = XSD.integer;
		else if (typeStr.equals(XSD.time.getLocalName()))
			onDatatype = XSD.time;
		else if (typeStr.equals(XSD.xboolean.getLocalName()))
			onDatatype = XSD.xboolean;
		else if (typeStr.equals(XSD.xdouble.getLocalName()))
			onDatatype = XSD.xdouble;
		else if (typeStr.equals(XSD.xfloat.getLocalName()))
			onDatatype = XSD.xfloat;
		else if (typeStr.equals(XSD.xint.getLocalName()))
			onDatatype = XSD.xint;
		else if (typeStr.equals(XSD.xlong.getLocalName()))
			onDatatype = XSD.xlong;
		else if (typeStr.equals(XSD.anyURI.getLocalName()))
			onDatatype = XSD.anyURI;
		else if (typeStr.equals(XSD.anyURI.getLocalName()))
			onDatatype = XSD.anyURI;
		else {
			throw new JenaProcessorException("Unexpected primitive data type: " + typeStr);
		}
		return onDatatype;
	}

	private OntClass createRdfsDatatype(String newDatatypeUri, List<RDFNode> unionOfTypes,
			com.hp.hpl.jena.rdf.model.Resource onDatatype, SadlDataTypeFacet facet) throws JenaProcessorException {
		OntClass datatype = getTheJenaModel().createOntResource(OntClass.class, RDFS.Datatype, newDatatypeUri);
		OntClass equivClass = getTheJenaModel().createOntResource(OntClass.class, RDFS.Datatype, null);
		if (onDatatype != null) {
			equivClass.addProperty(OWL2.onDatatype, onDatatype);
			if (facet != null) {
				com.hp.hpl.jena.rdf.model.Resource restrictions = facetsToRestrictionNode(newDatatypeUri, facet);
				// Create a list containing the restrictions
				RDFList list = getTheJenaModel().createList(new RDFNode[] { restrictions });
				equivClass.addProperty(OWL2.withRestrictions, list);
			}
		} else if (unionOfTypes != null) {
			Iterator<RDFNode> iter = unionOfTypes.iterator();
			RDFList collection = getTheJenaModel().createList();
			while (iter.hasNext()) {
				RDFNode dt = iter.next();
				collection = collection.with(dt);
			}
			equivClass.addProperty(OWL.unionOf, collection);
		} else {
			throw new JenaProcessorException("Invalid arguments to createRdfsDatatype");
		}
		datatype.addEquivalentClass(equivClass);
		TypeMapper.getInstance().getSafeTypeByName(newDatatypeUri);
		return datatype;
	}

	private com.hp.hpl.jena.rdf.model.Resource facetsToRestrictionNode(String newName, SadlDataTypeFacet facet) {
		com.hp.hpl.jena.rdf.model.Resource anon = getTheJenaModel().createResource();

		anon.addProperty(xsdProperty(facet.isMinInclusive() ? "minInclusive" : "minExclusive"), "" + facet.getMin());
		anon.addProperty(xsdProperty(facet.isMaxInclusive() ? "maxInclusive" : "maxExclusive"), "" + facet.getMax());

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

	protected OntClass processSadlPropertyCondition(SadlPropertyCondition sadlPropCond) throws JenaProcessorException {
		OntClass retval = null;
		SadlResource sr = ((SadlPropertyCondition) sadlPropCond).getProperty();
		String propUri = getDeclarationExtensions().getConceptUri(sr);
		if (propUri == null) {
			throw new JenaProcessorException(
					"Failed to get concept URI of SadlResource in processSadlPropertyCondition");
		}
		OntConceptType propType;
		try {
			propType = getDeclarationExtensions().getOntConceptType(sr);
		} catch (CircularDefinitionException e) {
			propType = e.getDefinitionType();
			addError(e.getMessage(), sadlPropCond);
		}
		OntProperty prop = getTheJenaModel().getOntProperty(propUri);
		if (prop == null) {
			if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
				prop = getTheJenaModel().createObjectProperty(propUri);
			} else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
				prop = getTheJenaModel().createDatatypeProperty(propUri);
			} else if (propType.equals(OntConceptType.ANNOTATION_PROPERTY)) {
				prop = getTheJenaModel().createAnnotationProperty(propUri);
			} else {
				prop = getTheJenaModel().createOntProperty(propUri);
			}
		}
		Iterator<SadlCondition> conditer = ((SadlPropertyCondition) sadlPropCond).getCond().iterator();
		while (conditer.hasNext()) {
			SadlCondition cond = conditer.next();
			retval = sadlConditionToOntClass(cond, prop, propType);
			if (conditer.hasNext()) {
				throw new JenaProcessorException("Multiple property conditions not currently handled");
			}
		}
		return retval;
	}

	protected OntClass sadlConditionToOntClass(SadlCondition cond, Property prop, OntConceptType propType)
			throws JenaProcessorException {
		OntClass retval = null;
		if (prop == null) {
			addError(SadlErrorMessages.CANNOT_CREATE.get("restriction", "unresolvable property"), cond);
		} else if (cond instanceof SadlAllValuesCondition) {
			SadlTypeReference type = ((SadlAllValuesCondition) cond).getType();
			if (type instanceof SadlPrimitiveDataType) {
				SadlDataType pt = ((SadlPrimitiveDataType) type).getPrimitiveType();
				String typeStr = pt.getLiteral();
				typeStr = XSD.getURI() + typeStr;
			}
			com.hp.hpl.jena.rdf.model.Resource typersrc = sadlTypeReferenceToResource(type);
			if (typersrc == null) {
				addError(SadlErrorMessages.CANNOT_CREATE.get("all values from restriction",
						"restriction on unresolvable property value restriction"), type);
			} else {
				AllValuesFromRestriction avf = getTheJenaModel().createAllValuesFromRestriction(null, prop, typersrc);
				logger.debug("New all values from restriction on '" + prop.getURI() + "' to values of type '"
						+ typersrc.toString() + "'");
				retval = avf;
			}
		} else if (cond instanceof SadlHasValueCondition) {
			// SadlExplicitValue value = ((SadlHasValueCondition)cond).getRestriction();
			RDFNode val = null;
			EObject restObj = ((SadlHasValueCondition) cond).getRestriction();
			if (restObj instanceof SadlExplicitValue) {
				SadlExplicitValue value = (SadlExplicitValue) restObj;
				if (value instanceof SadlResource) {
					OntConceptType srType;
					try {
						srType = getDeclarationExtensions().getOntConceptType((SadlResource) value);
					} catch (CircularDefinitionException e) {
						srType = e.getDefinitionType();
						addError(e.getMessage(), cond);
					}
					SadlResource srValue = (SadlResource) value;
					if (srType == null) {
						srValue = ((SadlResource) value).getName();
						try {
							srType = getDeclarationExtensions().getOntConceptType(srValue);
						} catch (CircularDefinitionException e) {
							srType = e.getDefinitionType();
							addError(e.getMessage(), cond);
						}
					}
					if (srType == null) {
						throw new JenaProcessorException("Unable to resolve SadlResource value");
					}
					if (srType.equals(OntConceptType.INSTANCE)) {
						String valUri = getDeclarationExtensions().getConceptUri(srValue);
						if (valUri == null) {
							throw new JenaProcessorException("Failed to find SadlResource in Xtext model");
						}
						val = getTheJenaModel().getIndividual(valUri);
						if (val == null) {
							SadlResource decl = getDeclarationExtensions().getDeclaration(srValue);
							if (decl != null && !decl.equals(srValue)) {
								EObject cont = decl.eContainer();
								if (cont instanceof SadlInstance) {
									try {
										val = processSadlInstance((SadlInstance) cont);
									} catch (CircularDefinitionException e) {
										// TODO Auto-generated catch block
										e.printStackTrace();
									}
								} else if (cont instanceof SadlMustBeOneOf) {
									cont = ((SadlMustBeOneOf) cont).eContainer();
									if (cont instanceof SadlClassOrPropertyDeclaration) {
										try {
											processSadlClassOrPropertyDeclaration(
													(SadlClassOrPropertyDeclaration) cont);
											eobjectPreprocessed(cont);
										} catch (TranslationException e) {
											// TODO Auto-generated catch block
											e.printStackTrace();
										}
										val = getTheJenaModel().getIndividual(valUri);
									}
								} else if (cont instanceof SadlCanOnlyBeOneOf) {
									cont = ((SadlCanOnlyBeOneOf) cont).eContainer();
									if (cont instanceof SadlClassOrPropertyDeclaration) {
										try {
											processSadlClassOrPropertyDeclaration(
													(SadlClassOrPropertyDeclaration) cont);
											eobjectPreprocessed(cont);
										} catch (TranslationException e) {
											// TODO Auto-generated catch block
											e.printStackTrace();
										}
										val = getTheJenaModel().getIndividual(valUri);
									}
								}
							}
							if (val == null) {
								throw new JenaProcessorException(
										"Failed to retrieve instance '" + valUri + "' from Jena model");
							}
						}
					} else {
						throw new JenaProcessorException(
								"A has value restriction is to a SADL resource that did not resolve to an instance in the model");
					}
				} else {
					if (prop.canAs(OntProperty.class)) {
						val = sadlExplicitValueToLiteral(value, prop.as(OntProperty.class).getRange());
					} else {
						val = sadlExplicitValueToLiteral(value, null);
					}
				}
			} else if (restObj instanceof SadlNestedInstance) {
				try {
					val = processSadlInstance((SadlNestedInstance) restObj);
				} catch (CircularDefinitionException e) {
					throw new JenaProcessorException(e.getMessage());
				}
			}
			if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
				Individual valInst = val.as(Individual.class);
				if (prop.canAs(OntProperty.class)
						&& valueInObjectTypePropertyRange(prop.as(OntProperty.class), valInst, cond)) {
					HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, prop, valInst);
					logger.debug("New has value restriction on '" + prop.getURI() + "' to value '" + valInst.toString()
							+ "'");
					retval = hvr;
				} else {
					throw new JenaProcessorException(
							SadlErrorMessages.NOT_IN_RANGE.get(valInst.getURI(), prop.getURI()));
				}
			} else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
				if (prop.canAs(OntProperty.class) && val.isLiteral()
						&& valueInDatatypePropertyRange(prop.as(OntProperty.class), val.asLiteral(), cond)) {
					HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, prop, val);
					logger.debug(
							"New has value restriction on '" + prop.getURI() + "' to value '" + val.toString() + "'");
					retval = hvr;
				} else {
					throw new JenaProcessorException(SadlErrorMessages.NOT_IN_RANGE.get(val.toString(), prop.getURI()));
				}
			} else if (propType.equals(OntConceptType.RDF_PROPERTY)) {
				HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, prop, val);
				logger.debug("New has value restriction on '" + prop.getURI() + "' to value '" + val.toString() + "'");
				retval = hvr;
			} else {
				throw new JenaProcessorException(
						"Has value restriction on unexpected property type: " + propType.toString());
			}
		} else if (cond instanceof SadlCardinalityCondition) {
			// Note: SomeValuesFrom is embedded in cardinality in the SADL grammar--an "at
			// least" cardinality with "one" instead of #
			String cardinality = ((SadlCardinalityCondition) cond).getCardinality();
			SadlTypeReference type = ((SadlCardinalityCondition) cond).getType();
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
				logger.debug("New some values from restriction on '" + prop.getURI() + "' to values of type '"
						+ typersrc.toString() + "'");
				retval = svf;
			} else {
				// cardinality restriction
				int cardNum = Integer.parseInt(cardinality);
				String op = ((SadlCardinalityCondition) cond).getOperator();
				if (op == null) {
					CardinalityRestriction cr = getTheJenaModel().createCardinalityRestriction(null, prop, cardNum);
					logger.debug("New cardinality restriction " + cardNum + " on '" + prop.getURI() + "' created");
					if (type != null) {
						cr.removeAll(OWL.cardinality);
						cr.addLiteral(OWL2.qualifiedCardinality, cardNum);
						cr.addProperty(OWL2.onClass, typersrc);
					}
					retval = cr;
				} else if (op.equals("least")) {
					MinCardinalityRestriction cr = getTheJenaModel().createMinCardinalityRestriction(null, prop,
							cardNum);
					logger.debug("New min cardinality restriction " + cardNum + " on '" + prop.getURI() + "' created");
					if (type != null) {
						cr.removeAll(OWL.minCardinality);
						cr.addLiteral(OWL2.minQualifiedCardinality, cardNum);
						cr.addProperty(OWL2.onClass, typersrc);
					}
					retval = cr;
				} else if (op.equals("most")) {
					logger.debug("New max cardinality restriction " + cardNum + " on '" + prop.getURI() + "' created");
					MaxCardinalityRestriction cr = getTheJenaModel().createMaxCardinalityRestriction(null, prop,
							cardNum);
					if (type != null) {
						cr.removeAll(OWL.maxCardinality);
						cr.addLiteral(OWL2.maxQualifiedCardinality, cardNum);
						cr.addProperty(OWL2.onClass, typersrc);
					}
					retval = cr;
				}
				if (logger.isDebugEnabled()) {
					if (type != null) {
						logger.debug("   cardinality is qualified; values must be of type '" + typersrc + "'");
					}
				}
			}
		} else {
			throw new JenaProcessorException("Unhandled SadlCondition type: " + cond.getClass().getCanonicalName());
		}
		return retval;
	}

	private boolean valueInDatatypePropertyRange(OntProperty prop, Literal val, EObject cond) {
		try {
			if (getModelValidator() != null) {
				return getModelValidator().checkDataPropertyValueInRange(getTheJenaModel(), null, prop, val);
			}
		} catch (TranslationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}

	protected Literal sadlExplicitValueToLiteral(SadlExplicitValue value, com.hp.hpl.jena.rdf.model.Resource rng)
			throws JenaProcessorException {
		try {
			boolean isNegated = false;
			if (value instanceof SadlUnaryExpression) {
				String op = ((SadlUnaryExpression) value).getOperator();
				if (op.equals("-")) {
					value = ((SadlUnaryExpression) value).getValue();
					isNegated = true;
				} else {
					throw new JenaProcessorException("Unhandled case of unary operator on SadlExplicitValue: " + op);
				}
			}
			if (value instanceof SadlNumberLiteral) {
				String val = ((SadlNumberLiteral) value).getLiteralNumber().toPlainString();
				if (isNegated) {
					val = "-" + val;
				}
				if (rng != null && rng.getURI() != null) {
					return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), val);
				} else {
					if (val.contains(".")) {
						return getTheJenaModel().createTypedLiteral(Double.parseDouble(val));
					} else {
						return getTheJenaModel().createTypedLiteral(Integer.parseInt(val));
					}
				}
			} else if (value instanceof SadlStringLiteral) {
				String val = ((SadlStringLiteral) value).getLiteralString();
				if (isNegated) {
					val = "-" + val;
				}
				if (rng != null) {
					return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), val);
				} else {
					return getTheJenaModel().createTypedLiteral(val);
				}
			} else if (value instanceof SadlBooleanLiteral) {
				SadlBooleanLiteral val = ((SadlBooleanLiteral) value);
				if (rng != null) {
					return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(),
							val.isTruethy());
				} else {
					return getTheJenaModel().createTypedLiteral(val.isTruethy());
				}
			} else if (value instanceof SadlValueList) {
				throw new JenaProcessorException("A SADL value list cannot be converted to a Literal");
			} else if (value instanceof SadlConstantLiteral) {
				String val = ((SadlConstantLiteral) value).getTerm();
				if (val.equals(SadlConstants.CONSTANT_PI)) {
					double cv = Math.PI;
					if (isNegated) {
						cv = cv * -1.0;
					}
					return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), cv);
				} else if (val.equals(SadlConstants.CONSTANT_E)) {
					double cv = Math.E;
					if (isNegated) {
						cv = cv * -1.0;
					}
					return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), cv);
				} else if (rng != null) {
					if (isNegated) {
						val = "-" + val;
					}
					return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), val);
				} else {
					if (isNegated) {
						val = "-" + val;
					}
					try {
						int ival = Integer.parseInt(val);
						return getTheJenaModel().createTypedLiteral(ival);
					} catch (Exception e) {
						try {
							double dval = Double.parseDouble(val);
							return getTheJenaModel().createTypedLiteral(dval);
						} catch (Exception e2) {
							return getTheJenaModel().createTypedLiteral(val);
						}
					}
				}
			} else if (value instanceof SadlResource) {
				Node nval = processExpression((SadlResource) value);
				throw new JenaProcessorException(
						"Unable to convert concept '" + nval.toFullyQualifiedString() + "to a literal");
			} else {
				throw new JenaProcessorException(
						"Unhandled sadl explicit vaue type: " + value.getClass().getCanonicalName());
			}
		} catch (Throwable t) {
			addError(t.getMessage(), value);
		}
		return null;
	}

	private boolean valueInObjectTypePropertyRange(OntProperty prop, Individual valInst, EObject cond)
			throws JenaProcessorException {
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

	private IntersectionClass createIntersectionClass(RDFNode... members) throws JenaProcessorException {
		RDFList classes = getTheJenaModel().createList(members);
		if (!classes.isEmpty()) {
			IntersectionClass intersectCls = getTheJenaModel().createIntersectionClass(null, classes);
			logger.debug("New intersection class created");
			return intersectCls;
		}
		throw new JenaProcessorException("createIntersectionClass called with empty list of classes");
	}

	private UnionClass createUnionClass(RDFNode... members) throws JenaProcessorException {
		UnionClass existingBnodeUnion = null;
		for (int i = 0; i < members.length; i++) {
			RDFNode mmbr = members[i];
			if ((mmbr instanceof UnionClass || mmbr.canAs(UnionClass.class) && mmbr.isAnon())) {
				existingBnodeUnion = mmbr.as(UnionClass.class);
				break;
			}
		}
		if (existingBnodeUnion != null) {
			for (int i = 0; i < members.length; i++) {
				RDFNode mmbr = members[i];
				if (!mmbr.equals(existingBnodeUnion)) {
					existingBnodeUnion.addOperand(mmbr.asResource());
					logger.debug("Added member '" + mmbr.toString() + "' to existing union class");
				}
			}
			return existingBnodeUnion;
		} else {
			RDFList classes = getTheJenaModel().createList(members);
			if (!classes.isEmpty()) {
				UnionClass unionCls = getTheJenaModel().createUnionClass(null, classes);
				logger.debug("New union class created");
				return unionCls;
			}
		}
		throw new JenaProcessorException("createUnionClass called with empty list of classes");
	}

	private OntConceptType getSadlTypeReferenceType(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		if (sadlTypeRef instanceof SadlSimpleTypeReference) {
			SadlResource sr = ((SadlSimpleTypeReference) sadlTypeRef).getType();
			try {
				return getDeclarationExtensions().getOntConceptType(sr);
			} catch (CircularDefinitionException e) {
				addError(e.getMessage(), sadlTypeRef);
				return e.getDefinitionType();
			}
		} else if (sadlTypeRef instanceof SadlPrimitiveDataType) {
			return OntConceptType.DATATYPE;
		} else if (sadlTypeRef instanceof SadlPropertyCondition) {
			// property conditions => OntClass
			return OntConceptType.CLASS;
		} else if (sadlTypeRef instanceof SadlUnionType) {
			SadlTypeReference lft = ((SadlUnionType) sadlTypeRef).getLeft();
			OntConceptType lfttype = getSadlTypeReferenceType(lft);
			return lfttype;
			// SadlTypeReference rght = ((SadlUnionType)sadlTypeRef).getRight();
		} else if (sadlTypeRef instanceof SadlIntersectionType) {
			SadlTypeReference lft = ((SadlIntersectionType) sadlTypeRef).getLeft();
			OntConceptType lfttype = getSadlTypeReferenceType(lft);
			return lfttype;
			// SadlTypeReference rght = ((SadlIntersectionType)sadlTypeRef).getRight();
		}
		throw new JenaProcessorException(
				"Unexpected SadlTypeReference subtype: " + sadlTypeRef.getClass().getCanonicalName());
	}

	protected String assureNamespaceEndsWithHash(String name) {
		name = name.trim();
		if (!name.endsWith("#")) {
			return name + "#";
		}
		return name;
	}

	public String getModelNamespace() {
		return modelNamespace;
	}

	protected void setModelNamespace(String modelNamespace) {
		this.modelNamespace = modelNamespace;
	}

	public OntDocumentManager getJenaDocumentMgr(OntModelSpec ontModelSpec) {
		if (jenaDocumentMgr == null) {
			if (getMappingModel() != null) {
				setJenaDocumentMgr(new OntDocumentManager(getMappingModel()));
				if (ontModelSpec != null) {
					ontModelSpec.setDocumentManager(jenaDocumentMgr);
				}
			} else {
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
	public boolean instanceBelongsToClass(OntModel m, OntResource inst, OntResource cls) throws JenaProcessorException {
		// The following cases must be considered:
		// 1) The class is a union of other classes. Check to see if the instance is a
		// member of any of
		// the union classes and if so return true.
		// 2) The class is an intersection of other classes. Check to see if the
		// instance is
		// a member of each class in the intersection and if so return true.
		// 3) The class is neither a union nor an intersection. If the instance belongs
		// to the class return true. Otherwise
		// check to see if the instance belongs to a subclass of the class else
		// return false. (Superclasses do not need to be considered because even if the
		// instance belongs to a super
		// class that does not tell us that it belongs to the class.)

		/*
		 * e.g., Internet is a Network. Network is a type of Subsystem. Subsystem is
		 * type of System.
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
		} else if (cls.canAs(IntersectionClass.class)) {
			List<OntResource> uclses = getOntResourcesInIntersectionClass(m, cls.as(IntersectionClass.class));
			for (int i = 0; i < uclses.size(); i++) {
				OntResource ucls = uclses.get(i);
				if (!instanceBelongsToClass(m, inst, ucls)) {
					return false;
				}
			}
			return true;
		} else if (cls.canAs(Restriction.class)) {
			Restriction rest = cls.as(Restriction.class);
			OntProperty ontp = rest.getOnProperty();
			if (rest.isAllValuesFromRestriction()) {
				StmtIterator siter = inst.listProperties(ontp);
				while (siter.hasNext()) {
					Statement stmt = siter.nextStatement();
					RDFNode obj = stmt.getObject();
					if (obj.canAs(Individual.class)) {
						com.hp.hpl.jena.rdf.model.Resource avfc = rest.asAllValuesFromRestriction().getAllValuesFrom();
						if (!instanceBelongsToClass(m, (Individual) obj.as(Individual.class),
								(OntResource) avfc.as(OntResource.class))) {
							return false;
						}
					}
				}
			} else if (rest.isSomeValuesFromRestriction()) {
				if (inst.hasProperty(ontp)) {
					return true;
				}
			} else if (rest.isHasValueRestriction()) {
				RDFNode hval = rest.as(HasValueRestriction.class).getHasValue();
				if (inst.hasProperty(ontp, hval)) {
					return true;
				}
			} else if (rest.isCardinalityRestriction()) {
				throw new JenaProcessorException("Unhandled cardinality restriction");
			} else if (rest.isMaxCardinalityRestriction()) {
				throw new JenaProcessorException("Unhandled max cardinality restriction");
			} else if (rest.isMinCardinalityRestriction()) {
				throw new JenaProcessorException("Unhandled min cardinality restriction");
			}
		} else {
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
						} else if (or.canAs(OntClass.class)) {
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

	public List<OntResource> getOntResourcesInUnionClass(OntModel m, UnionClass ucls) {
		List<OntResource> results = new ArrayList<OntResource>();
		List<RDFNode> clses = ucls.getOperands().asJavaList();
		for (int i = 0; i < clses.size(); i++) {
			RDFNode mcls = clses.get(i);
			if (mcls.canAs(OntResource.class)) {
				if (mcls.canAs(UnionClass.class)) {
					List<OntResource> innerList = getOntResourcesInUnionClass(m, mcls.as(UnionClass.class));
					for (int j = 0; j < innerList.size(); j++) {
						OntResource innerRsrc = innerList.get(j);
						if (!results.contains(innerRsrc)) {
							results.add(innerRsrc);
						}
					}
				} else {
					results.add(mcls.as(OntResource.class));
				}
			}
		}
		return results;
	}

	public List<OntResource> getOntResourcesInIntersectionClass(OntModel m, IntersectionClass icls) {
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

	public ValidationAcceptor getIssueAcceptor() {
		return issueAcceptor;
	}

	protected void setIssueAcceptor(ValidationAcceptor issueAcceptor) {
		this.issueAcceptor = issueAcceptor;
	}

	private CancelIndicator getCancelIndicator() {
		return cancelIndicator;
	}

	protected void setCancelIndicator(CancelIndicator cancelIndicator) {
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

	public String getModelAlias() {
		return modelAlias;
	}

	protected void setModelAlias(String modelAlias) {
		this.modelAlias = modelAlias;
	}

	private void setSpec(OntModelSpec spec) {
		this.spec = spec;
	}

	/**
	 * This method looks in the clauses of a Rule to see if there is already a
	 * triple matching the given pattern. If there is a new variable of the same
	 * name is created (to make sure the count is right) and returned. If not a rule
	 * or no match a new variable (new name) is created and returned.
	 * 
	 * @param expr
	 * @param subject
	 * @param predicate
	 * @param object
	 * @return
	 */
	protected VariableNode getVariableNode(Expression expr, Node subject, Node predicate, Node object) {
		if (getTarget() != null) {
			// Note: when we find a match we still create a new VariableNode with the same
			// name in order to have the right reference counts for the new VariableNode
			if (getTarget() instanceof Rule) {
				VariableNode var = findVariableInTripleForReuse(((Rule) getTarget()).getGivens(), subject, predicate,
						object);
				if (var != null) {
					return new VariableNode(var.getName());
				}
				var = findVariableInTripleForReuse(((Rule) getTarget()).getIfs(), subject, predicate, object);
				if (var != null) {
					return new VariableNode(var.getName());
				}
				var = findVariableInTripleForReuse(((Rule) getTarget()).getThens(), subject, predicate, object);
				if (var != null) {
					return new VariableNode(var.getName());
				}
			}
		}
		return new VariableNode(getNewVar(expr));
	}

	protected String getNewVar(EObject container) {
		IScopeProvider scopeProvider = ((XtextResource) container.eResource()).getResourceServiceProvider()
				.get(IScopeProvider.class);
		IScope scope = scopeProvider.getScope(container, SADLPackage.Literals.SADL_RESOURCE__NAME);
		String proposedName = "v" + vNum;
		while (userDefinedVariables.contains(proposedName)
				|| scope.getSingleElement(QualifiedName.create(proposedName)) != null) {
			vNum++;
			proposedName = "v" + vNum;
		}
		vNum++;
		return proposedName;
	}

	protected int getVariableNumber() {
		return vNum;
	}

	/**
	 * Supporting method for the method above (getVariableNode(Node, Node, Node))
	 * 
	 * @param gpes
	 * @param subject
	 * @param predicate
	 * @param object
	 * @return
	 */
	protected VariableNode findVariableInTripleForReuse(List<GraphPatternElement> gpes, Node subject, Node predicate,
			Node object) {
		if (gpes != null) {
			Iterator<GraphPatternElement> itr = gpes.iterator();
			while (itr.hasNext()) {
				GraphPatternElement gpe = itr.next();
				while (gpe != null) {
					if (gpe instanceof TripleElement) {
						TripleElement tr = (TripleElement) gpe;
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

	private java.nio.file.Path checkImplicitSadlModelExistence(Resource resource, ProcessorContext context)
			throws IOException, ConfigurationException, URISyntaxException, JenaProcessorException {
		UtilsForJena ufj = new UtilsForJena();
		String policyFileUrl = ufj.getPolicyFilename(resource);
		String policyFilename = policyFileUrl != null ? ufj.fileUrlToFileName(policyFileUrl) : null;
		if (policyFilename != null) {
			File projectFolder = new File(policyFilename).getParentFile().getParentFile();
			String relPath = SadlConstants.SADL_IMPLICIT_MODEL_FOLDER + "/"
					+ SadlConstants.SADL_IMPLICIT_MODEL_FILENAME;
			String platformPath = projectFolder.getName() + "/" + relPath;
			String implicitSadlModelFN = projectFolder + "/" + relPath;
			File implicitModelFile = new File(implicitSadlModelFN);
			if (!implicitModelFile.exists()) {
				createSadlImplicitModel(implicitModelFile);
				try {
					Resource newRsrc = resource.getResourceSet()
							.createResource(URI.createPlatformResourceURI(platformPath, false)); // createFileURI(implicitSadlModelFN));
					// newRsrc.load(new StringInputStream(implicitModel),
					// resource.getResourceSet().getLoadOptions());
					newRsrc.load(resource.getResourceSet().getLoadOptions());
					refreshResource(newRsrc);
				} catch (Throwable t) {
				}
			}
			return implicitModelFile.getAbsoluteFile().toPath();
		}
		return null;
	}

	static public File createBuiltinFunctionImplicitModel(String projectRootPath)
			throws IOException, ConfigurationException {
		// First, obtain proper translator for project
		SadlUtils su = new SadlUtils();
		if (projectRootPath.startsWith("file")) {
			projectRootPath = su.fileUrlToFileName(projectRootPath);
		}
		final File mfFolder = new File(projectRootPath + "/" + ResourceManager.OWLDIR);
		final String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
		String fixedModelFolderName = mfFolder.getCanonicalPath().replace("\\", "/");
		IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory
				.getConfigurationManagerForIDE(fixedModelFolderName, format);
		ITranslator translator = configMgr.getTranslator();
		// Second, obtain built-in function implicit model contents
		String builtinFunctionModel = translator.getBuiltinFunctionModel();
		// Third, create built-in function implicit model file
		File builtinFunctionFile = new File(projectRootPath + "/" + SadlConstants.SADL_IMPLICIT_MODEL_FOLDER + "/"
				+ SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME);
		su.stringToFile(builtinFunctionFile, builtinFunctionModel, true);

		return builtinFunctionFile;
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
		sb.append(
				"	    <rdfs:comment xml:lang=\"en\">Base model for SADL. These concepts can be used without importing.</rdfs:comment>\n");
		sb.append("	  </owl:Ontology>\n");
		sb.append("	  <owl:Class rdf:ID=\"Equation\"/>\n");
		sb.append("	  <owl:Class rdf:ID=\"ExternalEquation\"/>\n");
		sb.append("	  <owl:DatatypeProperty rdf:ID=\"expression\">\n");
		sb.append("	    <rdfs:domain rdf:resource=\"#Equation\"/>\n");
		sb.append("	    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#string\"/>\n");
		sb.append("	  </owl:DatatypeProperty>\n");
		sb.append("	  <owl:DatatypeProperty rdf:ID=\"externalURI\">\n");
		sb.append("	    <rdfs:domain rdf:resource=\"#ExternalEquation\"/>\n");
		sb.append("	    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#anyURI\"/>\n");
		sb.append("	  </owl:DatatypeProperty>\n");
		sb.append("	  <owl:DatatypeProperty rdf:ID=\"location\">\n");
		sb.append("	    <rdfs:domain rdf:resource=\"#ExternalEquation\"/>\n");
		sb.append("	    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#string\"/>\n");
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
		sb.append("  <owl:DatatypeProperty rdf:about=\"http://sadl.org/sadllistmodel#lengthRestriction\">\n");
		sb.append("    <rdfs:domain rdf:resource=\"http://sadl.org/sadllistmodel#List\"/>\n");
		sb.append("    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#int\"/>\n");
		sb.append("    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#DatatypeProperty\"/>\n");
		sb.append("  </owl:DatatypeProperty>\n");
		sb.append("  <owl:DatatypeProperty rdf:about=\"http://sadl.org/sadllistmodel#minLengthRestriction\">\n");
		sb.append("    <rdfs:domain rdf:resource=\"http://sadl.org/sadllistmodel#List\"/>\n");
		sb.append("    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#int\"/>\n");
		sb.append("    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#DatatypeProperty\"/>\n");
		sb.append("  </owl:DatatypeProperty>\n");
		sb.append("  <owl:DatatypeProperty rdf:about=\"http://sadl.org/sadllistmodel#maxLengthRestriction\">\n");
		sb.append("    <rdfs:domain rdf:resource=\"http://sadl.org/sadllistmodel#List\"/>\n");
		sb.append("    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#int\"/>\n");
		sb.append("    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#DatatypeProperty\"/>\n");
		sb.append("  </owl:DatatypeProperty>\n");
		sb.append("  <owl:AnnotationProperty rdf:about=\"http://sadl.org/sadllistmodel#listtype\"/>\n");
		sb.append("</rdf:RDF>\n");
		return sb.toString();
	}

	static public String getSadlDefaultsModel() {
		StringBuilder sb = new StringBuilder();
		sb.append("<?xml version=\"1.0\"?>\n");
		sb.append("<rdf:RDF xmlns=\"http://research.ge.com/Acuity/defaults.owl#\" \n");
		sb.append("xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\" \n");
		sb.append("xmlns:owl=\"http://www.w3.org/2002/07/owl#\" \n");
		sb.append("xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" \n");
		sb.append("xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\" \n");
		sb.append("xml:base=\"http://research.ge.com/Acuity/defaults.owl\">\n");
		sb.append("	<owl:Ontology rdf:about=\"\">\n");
		sb.append(
				"	  <rdfs:comment>Copyright 2007, 2008, 2009 - General Electric Company, All Rights Reserved</rdfs:comment>\n");
		sb.append("	  <owl:versionInfo>$Id: defaults.owl,v 1.1 2014/01/23 21:52:26 crapo Exp $</owl:versionInfo>\n");
		sb.append("	</owl:Ontology>\n");
		sb.append("	<owl:Class rdf:ID=\"DataDefault\">\n");
		sb.append(
				"		<rdfs:comment rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">This type of default has a value which is a Literal</rdfs:comment>\n");
		sb.append("		<rdfs:subClassOf>\n");
		sb.append("			<owl:Class rdf:ID=\"DefaultValue\"/>\n");
		sb.append("		</rdfs:subClassOf>\n");
		sb.append("	</owl:Class>\n");
		sb.append("	<owl:Class rdf:ID=\"ObjectDefault\">\n");
		sb.append(
				"		<rdfs:comment rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">This type of default has a value which is an Individual</rdfs:comment>\n");
		sb.append("		<rdfs:subClassOf>\n");
		sb.append("			<owl:Class rdf:about=\"#DefaultValue\"/>\n");
		sb.append("		</rdfs:subClassOf>\n");
		sb.append("	</owl:Class>\n");
		sb.append("	<owl:FunctionalProperty rdf:ID=\"hasLevel\">\n");
		sb.append("		<rdfs:domain rdf:resource=\"#DataDefault\"/>\n");
		sb.append("		<rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#DatatypeProperty\"/>\n");
		sb.append("		<rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#int\"/>\n");
		sb.append("	</owl:FunctionalProperty>\n");
		sb.append("	<owl:FunctionalProperty rdf:ID=\"hasDataDefault\">\n");
		sb.append("		<rdfs:domain rdf:resource=\"#DataDefault\"/>\n");
		sb.append("		<rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#DatatypeProperty\"/>\n");
		sb.append("	</owl:FunctionalProperty>\n");
		sb.append("	<owl:ObjectProperty rdf:ID=\"hasObjectDefault\">\n");
		sb.append("		<rdfs:domain rdf:resource=\"#ObjectDefault\"/>\n");
		sb.append("		<rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#FunctionalProperty\"/>\n");
		sb.append("	</owl:ObjectProperty>\n");
		sb.append("	<owl:ObjectProperty rdf:ID=\"appliesToProperty\">\n");
		sb.append(
				"		<rdfs:comment rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">The value of this Property is the Property to which the default value applies.</rdfs:comment>\n");
		sb.append("		<rdfs:domain rdf:resource=\"#DefaultValue\"/>\n");
		sb.append("		<rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#FunctionalProperty\"/>\n");
		sb.append("		<rdfs:range rdf:resource=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#Property\"/>");
		sb.append("	</owl:ObjectProperty>\n");
		sb.append("</rdf:RDF>\n");
		return sb.toString();
	}

	private boolean importSadlListModel(Resource resource) throws JenaProcessorException, ConfigurationException {
		if (sadlListModel == null) {
			try {
				sadlListModel = getOntModelFromString(resource, getSadlListModel());
				OntModelProvider.setSadlListModel(sadlListModel);
			} catch (Exception e) {
				throw new JenaProcessorException(e.getMessage(), e);
			}
			addImportToJenaModel(getModelName(), SadlConstants.SADL_LIST_MODEL_URI,
					SadlConstants.SADL_LIST_MODEL_PREFIX, sadlListModel);
			return true;
		}
		return false;
	}

	public OntModel getOntModelFromString(Resource resource, String serializedModel)
			throws IOException, ConfigurationException, URISyntaxException, JenaProcessorException {
		OntModel listModel = prepareEmptyOntModel(resource);
		InputStream stream = new ByteArrayInputStream(serializedModel.getBytes());
		listModel.read(stream, null);
		return listModel;
	}

	private boolean importSadlDefaultsModel(Resource resource) throws JenaProcessorException, ConfigurationException {
		if (sadlDefaultsModel == null) {
			try {
				sadlDefaultsModel = getOntModelFromString(resource, getSadlDefaultsModel());
				OntModelProvider.setSadlDefaultsModel(sadlDefaultsModel);
			} catch (Exception e) {
				throw new JenaProcessorException(e.getMessage(), e);
			}
			addImportToJenaModel(getModelName(), SadlConstants.SADL_DEFAULTS_MODEL_URI,
					SadlConstants.SADL_DEFAULTS_MODEL_PREFIX, sadlDefaultsModel);
			return true;
		}
		return false;
	}

	protected IConfigurationManagerForIDE getConfigMgr(Resource resource, String format) throws ConfigurationException {
		if (configMgr == null) {
			String modelFolderPathname = getModelFolderPath(resource);
			if (format == null) {
				format = ConfigurationManager.RDF_XML_ABBREV_FORMAT; // default
			}
			if (isSyntheticUri(modelFolderPathname, resource)) {
				modelFolderPathname = null;
				configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderPathname, format,
						true);
			} else {
				configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderPathname,
						format);
			}
		}
		return configMgr;
	}

	protected boolean isSyntheticUri(String modelFolderPathname, Resource resource) {
		if ((modelFolderPathname == null && resource.getURI().toString().startsWith("synthetic"))
				|| resource.getURI().toString().startsWith(SYNTHETIC_FROM_TEST)) {
			return true;
		}
		return false;
	}

	protected IConfigurationManagerForIDE getConfigMgr() {
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

	protected boolean isConjunction(String op) {
		if (op.equals("and")) {
			return true;
		}
		return false;
	}
	
	protected boolean isDisjunction(String aOp) {
		if("or".equals(aOp)) {
			return true;
		}
		return false;
	}

	protected void resetProcessorState(SadlModelElement element) throws InvalidTypeException {
		try {
			if (getModelValidator() != null) {
				getModelValidator().resetValidatorState(element);
			}
		} catch (TranslationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public List<Equation> getEquations() {
		return equations;
	}

	public void setEquations(List<Equation> equations) {
		this.equations = equations;
	}

	protected boolean isClass(OntConceptType oct) {
		if (oct.equals(OntConceptType.CLASS)) {
			return true;
		}
		return false;
	}

	protected boolean isProperty(NodeType oct) {
		if (oct.equals(NodeType.ObjectProperty) || oct.equals(NodeType.DataTypeProperty)
				|| oct.equals(NodeType.PropertyNode)) {
			return true;
		}
		return false;
	}

	protected boolean isProperty(OntConceptType oct) {
		if (oct.equals(OntConceptType.DATATYPE_PROPERTY) || oct.equals(OntConceptType.CLASS_PROPERTY)
				|| oct.equals(OntConceptType.RDF_PROPERTY) || oct.equals(OntConceptType.ANNOTATION_PROPERTY)) {
			return true;
		}
		return false;
	}

	public SadlCommand getTargetCommand() {
		return targetCommand;
	}

	public ITranslator getTranslator() throws ConfigurationException {
		IConfigurationManagerForIDE cm = getConfigMgr(getCurrentResource(), getOwlModelFormat(getProcessorContext()));
		if (cm.getTranslatorClassName() == null) {
			cm.setTranslatorClassName(translatorClassName);
			cm.setReasonerClassName(reasonerClassName);
		}
		return cm.getTranslator();
	}

	/**
	 * Method to obtain the sadlimplicitmodel:impliedProperty annotation property
	 * values for the given class
	 * 
	 * @param cls
	 *            -- the Jena Resource (nominally a class) for which the values are
	 *            desired
	 * @return -- a List of the ConceptNames of the values
	 */
	public List<ConceptName> getImpliedProperties(com.hp.hpl.jena.rdf.model.Resource cls) {
		List<ConceptName> retlst = null;
		if (cls == null)
			return null;
		if (!cls.isURIResource())
			return null; // impliedProperties can only be given to a named class
		if (!cls.canAs(OntClass.class)) {
			addError("Can't get implied properties of a non-class entity.", null);
			return null;
		}
		List<OntResource> allImplPropClasses = getAllImpliedPropertyClasses();
		if (allImplPropClasses != null) {
			for (OntResource ipcls : allImplPropClasses) {
				try {
					if (SadlUtils.classIsSubclassOf(cls.as(OntClass.class), ipcls, true, null)) {
						StmtIterator sitr = getTheJenaModel().listStatements(ipcls,
								getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_IMPLIED_PROPERTY_URI),
								(RDFNode) null);
						if (sitr.hasNext()) {
							if (retlst == null) {
								retlst = new ArrayList<ConceptName>();
							}
							while (sitr.hasNext()) {
								RDFNode obj = sitr.nextStatement().getObject();
								if (obj.isURIResource()) {
									ConceptName cn = new ConceptName(obj.asResource().getURI());
									if (!retlst.contains(cn)) {
										retlst.add(cn);
									}
								}
							}
						}
					}
				} catch (CircularDependencyException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		return retlst;
	}

	private List<OntResource> getAllImpliedPropertyClasses() {
		return allImpliedPropertyClasses;
	}

	private List<OntResource> getAllExpandedPropertyClasses() {
		return allExpandedPropertyClasses;
	}

	protected boolean addImpliedPropertyClass(OntResource or) {
		if (!allImpliedPropertyClasses.contains(or)) {
			allImpliedPropertyClasses.add(or);
			return true;
		}
		return false;
	}

	protected int initializeAllImpliedPropertyClasses() {
		int cntr = 0;
		allImpliedPropertyClasses = new ArrayList<OntResource>();
		StmtIterator sitr = getTheJenaModel().listStatements(null,
				getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_IMPLIED_PROPERTY_URI), (RDFNode) null);
		if (sitr.hasNext()) {
			while (sitr.hasNext()) {
				com.hp.hpl.jena.rdf.model.Resource subj = sitr.nextStatement().getSubject();
				if (subj.canAs(OntResource.class)) {
					OntResource or = subj.as(OntResource.class);
					addImpliedPropertyClass(or);
					if (!allImpliedPropertyClasses.contains(or)) {
						if (allImpliedPropertyClasses.add(or)) {
							cntr++;
						}
					}
				}
			}
		}
		return cntr;
	}

	protected boolean addExpandedPropertyClass(OntResource or) {
		if (!allExpandedPropertyClasses.contains(or)) {
			allExpandedPropertyClasses.add(or);
			return true;
		}
		return false;
	}

	protected int initializeAllExpandedPropertyClasses() {
		int cntr = 0;
		allExpandedPropertyClasses = new ArrayList<OntResource>();
		StmtIterator sitr = getTheJenaModel().listStatements(null,
				getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXPANDED_PROPERTY_URI), (RDFNode) null);
		if (sitr.hasNext()) {
			while (sitr.hasNext()) {
				com.hp.hpl.jena.rdf.model.Resource subj = sitr.nextStatement().getSubject();
				if (subj.canAs(OntResource.class)) {
					OntResource or = subj.as(OntResource.class);
					if (addExpandedPropertyClass(or)) {
						cntr++;
					}
				}
			}
		}
		return cntr;
	}

	/**
	 * Method to obtain the sadlimplicitmodel:expandedProperty annotation property
	 * values for the given class
	 * 
	 * @param cls
	 *            -- the Jena Resource (nominally a class) for which the values are
	 *            desired
	 * @return -- a List of the URI strings of the values
	 */
	public List<String> getExpandedProperties(com.hp.hpl.jena.rdf.model.Resource cls) {
		List<String> retlst = null;
		if (cls == null)
			return null;
		// check superclasses
		if (cls.canAs(OntClass.class)) {
			OntClass ontcls = cls.as(OntClass.class);
			ExtendedIterator<OntClass> eitr = ontcls.listSuperClasses();
			while (eitr.hasNext()) {
				OntClass supercls = eitr.next();
				List<String> scips = getExpandedProperties(supercls);
				if (scips != null) {
					if (retlst == null) {
						retlst = scips;
					} else {
						for (int i = 0; i < scips.size(); i++) {
							String cn = scips.get(i);
							if (!scips.contains(cn)) {
								retlst.add(scips.get(i));
							}
						}
					}
				}
			}
		}
		StmtIterator sitr = getTheJenaModel().listStatements(cls,
				getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXPANDED_PROPERTY_URI), (RDFNode) null);
		if (sitr.hasNext()) {
			if (retlst == null) {
				retlst = new ArrayList<String>();
			}
			while (sitr.hasNext()) {
				RDFNode obj = sitr.nextStatement().getObject();
				if (obj.isURIResource()) {
					String cn = obj.asResource().getURI();
					if (!retlst.contains(cn)) {
						retlst.add(cn);
					}
				}
			}
			return retlst;
		}
		return retlst;
	}

	protected boolean isLookingForFirstProperty() {
		return lookingForFirstProperty;
	}

	protected void setLookingForFirstProperty(boolean lookingForFirstProperty) {
		this.lookingForFirstProperty = lookingForFirstProperty;
	}

	public Equation getCurrentEquation() {
		return currentEquation;
	}

	protected void setCurrentEquation(Equation currentEquation) {
		this.currentEquation = currentEquation;
	}

	public JenaBasedSadlModelValidator getModelValidator() throws TranslationException {
		return modelValidator;
	}

	protected void setModelValidator(JenaBasedSadlModelValidator modelValidator) {
		this.modelValidator = modelValidator;
	}

	protected void initializeModelValidator() {
		setModelValidator(new JenaBasedSadlModelValidator(issueAcceptor, getTheJenaModel(), getDeclarationExtensions(),
				this, getMetricsProcessor()));
	}

	protected IMetricsProcessor getMetricsProcessor() {
		return metricsProcessor;
	}

	protected void setMetricsProcessor(IMetricsProcessor metricsProcessor) {
		this.metricsProcessor = metricsProcessor;
	}

	protected String rdfNodeToString(RDFNode node) {
		if (node.isLiteral()) {
			return node.asLiteral().getValue().toString();
		} else if (node.isURIResource() && getConfigMgr() != null) {
			String prefix = getConfigMgr().getGlobalPrefix(node.asResource().getNameSpace());
			if (prefix != null) {
				return prefix + ":" + node.asResource().getLocalName();
			}
		}
		return node.toString();
	}

	protected String conceptIdentifierToString(ConceptIdentifier ci) {
		if (ci instanceof ConceptName) {
			if (getConfigMgr() != null && ((ConceptName) ci).getPrefix() == null
					&& ((ConceptName) ci).getNamespace() != null) {
				String ns = ((ConceptName) ci).getNamespace();
				if (ns.endsWith("#")) {
					ns = ns.substring(0, ns.length() - 1);
				}
				String prefix = getConfigMgr().getGlobalPrefix(ns);
				if (prefix == null) { // Note: when running JUnit backend tests this will be null as there is no
										// mapping file.
					return ((ConceptName) ci).getName();
				}
				((ConceptName) ci).setPrefix(prefix);
			}
			return ((ConceptName) ci).toString();
		}
		return ci.toString();
	}

	protected String nodeToString(Node node) {
		if (node instanceof NamedNode) {
			if (((NamedNode) node).getPrefix() != null) {
				return ((NamedNode) node).getPrefix() + ":" + ((NamedNode) node).getName();
			}
			return ((NamedNode) node).toFullyQualifiedString();
		} else if (node instanceof ProxyNode) {
			return ((ProxyNode) node).getProxyFor().toString();
		} else {
			return node.toFullyQualifiedString();
		}
	}

	public boolean isNumericComparisonOperator(String operation) {
		if (numericComparisonOperators.contains(operation)) {
			return true;
		}
		return false;
	}

	public boolean isEqualityInequalityComparisonOperator(String operation) {
		if (equalityInequalityComparisonOperators.contains(operation)) {
			return true;
		}
		return false;
	}

	public boolean isComparisonOperator(String operation) {
		if (comparisonOperators.contains(operation)) {
			return true;
		}
		return false;
	}

	public boolean isBooleanComparison(List<String> operations) {
		if (comparisonOperators.containsAll(operations)) {
			return true;
		}
		return false;
	}

	public boolean canBeNumericOperator(String op) {
		if (canBeNumericOperators.contains(op))
			return true;
		return false;
	}

	public boolean isNumericOperator(String op) {
		if (numericOperators.contains(op))
			return true;
		return false;
	}

	public boolean isNumericOperator(List<String> operations) {
		Iterator<String> itr = operations.iterator();
		while (itr.hasNext()) {
			if (isNumericOperator(itr.next()))
				return true;
		}
		return false;
	}

	public boolean canBeNumericOperator(List<String> operations) {
		Iterator<String> itr = operations.iterator();
		while (itr.hasNext()) {
			if (canBeNumericOperator(itr.next()))
				return true;
		}
		return false;
	}

	public boolean isNumericType(ConceptName conceptName) {
		try {
			if (conceptName.getName().equals("known") && conceptName.getNamespace() == null) {
				// known matches everything
				return true;
			}
			String uri = conceptName.getUri();
			return isNumericType(uri);
		} catch (InvalidNameException e) {
			// OK, some constants don't have namespace and so aren't numeric
		}
		return false;
	}

	public boolean isNumericType(String uri) {
		//uri is exactly a numeric type
		if (uri.equals(XSD.decimal.getURI()) || uri.equals(XSD.integer.getURI()) || uri.equals(XSD.xdouble.getURI())
				|| uri.equals(XSD.xfloat.getURI()) || uri.equals(XSD.xint.getURI()) || uri.equals(XSD.xlong.getURI())) {
			return true;
		}
		//If Unitted Quantities are ignored then they are also considered a numeric type
		if(this.ignoreUnittedQuantities && uri.equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
			return true;
		}
		return false;
	}

	public boolean isBooleanType(String uri) {
		if (uri.equals(XSD.xboolean.getURI())) {
			return true;
		}
		return false;
	}

	protected void setOwlFlavor(OWL_FLAVOR owlFlavor) {
		this.owlFlavor = owlFlavor;
	}

	protected boolean isTypeCheckingWarningsOnly() {
		return typeCheckingWarningsOnly;
	}

	protected void setTypeCheckingWarningsOnly(boolean typeCheckingWarningsOnly) {
		this.typeCheckingWarningsOnly = typeCheckingWarningsOnly;
	}

	protected boolean isBinaryListOperator(String op) {
		if (op.equals("contain") || op.equals("contains") || op.equals("unique")) {
			return true;
		}
		return false;
	}

	protected boolean sharedDisjunctiveContainer(Expression expr1, Expression expr2) {
		if (expr1 != null) {
			EObject cont1 = expr1.eContainer();
			do {
				if (cont1 instanceof BinaryOperation && ((BinaryOperation) cont1).getOp().equals("or")) {
					break;
				}
				cont1 = cont1.eContainer();
			} while (cont1 != null && cont1.eContainer() != null);

			if (expr2 != null) {
				EObject cont2 = expr2;
				do {
					if (cont2 instanceof BinaryOperation && ((BinaryOperation) cont2).getOp().equals("or")) {
						break;
					}
					cont2 = cont2.eContainer();
				} while (cont2 != null && cont2.eContainer() != null);
				if (cont1 != null && cont2 != null && cont1.equals(cont2)) {
					return true;
				}
			}
		}
		return false;
	}

	@Override
	public boolean isTypedListSubclass(RDFNode node) {
		if (node != null && node.isResource()) {
			com.hp.hpl.jena.rdf.model.Resource lstcls = theJenaModel
					.getResource(SadlConstants.SADL_LIST_MODEL_LIST_URI);
			if (lstcls != null && node.asResource().hasProperty(RDFS.subClassOf, lstcls)) { // if model has no lists,
																							// the list model will not
																							// have been imported
				return true;
			}
		}
		return false;
	}

	protected boolean isEqualOperator(String op) {
		BuiltinType optype = BuiltinType.getType(op);
		if (optype.equals(BuiltinType.Equal)) {
			return true;
		}
		return false;
	}

	public DeclarationExtensions getDeclarationExtensions() {
		return declarationExtensions;
	}

	public void setDeclarationExtensions(DeclarationExtensions declarationExtensions) {
		this.declarationExtensions = declarationExtensions;
	}

	protected void addNamedStructureAnnotations(Individual namedStructure, EList<NamedStructureAnnotation> annotations)
			throws TranslationException {
		Iterator<NamedStructureAnnotation> annitr = annotations.iterator();
		if (annitr.hasNext()) {
			while (annitr.hasNext()) {
				NamedStructureAnnotation ra = annitr.next();
				String annuri = getDeclarationExtensions().getConceptUri(ra.getType());
				Property annProp = getTheJenaModel().getProperty(annuri);
				try {
					if (annProp == null || !isProperty(getDeclarationExtensions().getOntConceptType(ra.getType()))) {
						issueAcceptor.addError("Annotation property '" + annuri + "' not found in model", ra);
						continue;
					}
				} catch (CircularDefinitionException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				Iterator<SadlExplicitValue> cntntitr = ra.getContents().iterator();
				StringBuilder sb = new StringBuilder();
				int cntr = 0;
				while (cntntitr.hasNext()) {
					SadlExplicitValue annvalue = cntntitr.next();
					if (annvalue instanceof SadlResource) {
						Node n = processExpression((SadlResource) annvalue);
						OntResource nor = getTheJenaModel().getOntResource(n.toFullyQualifiedString());
						if (nor != null) { // can be null during entry of statement in editor
							getTheJenaModel().add(namedStructure, annProp, nor);
						}
					} else {
						try {
							com.hp.hpl.jena.ontology.OntResource range = annProp.canAs(OntProperty.class)
									? annProp.as(OntProperty.class).getRange()
									: null;
							com.hp.hpl.jena.rdf.model.Literal annLiteral = sadlExplicitValueToLiteral(annvalue, range);
							getTheJenaModel().add(namedStructure, annProp, annLiteral);
							if (cntr > 0)
								sb.append(", ");
							sb.append("\"");
							sb.append(annvalue);
							sb.append("\"");
							cntr++;
						} catch (Exception e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						} // getTheJenaModel().createTypedLiteral(annvalue);
					}
				}
				logger.debug("Named structure annotation: " + getDeclarationExtensions().getConceptUri(ra.getType())
						+ " = " + sb.toString());
			}
		}
	}

	protected Declaration getDeclarationFromSubjHasProp(SubjHasProp subject) {
		Expression left = subject.getLeft();
		if (left instanceof Declaration) {
			return (Declaration) left;
		} else if (left instanceof SubjHasProp) {
			return getDeclarationFromSubjHasProp((SubjHasProp) left);
		} else if (left instanceof UnaryExpression && ((UnaryExpression) left).getExpr() instanceof Declaration) {
			return (Declaration) ((UnaryExpression) left).getExpr();
		}
		return null;
	}

	protected VariableNode getCruleVariable(NamedNode type, int ordNum) {
		List<VariableNode> existingList = cruleVariables != null ? cruleVariables.get(type) : null;
		if (existingList != null && ordNum >= 0 && ordNum <= existingList.size()) {
			return existingList.get(ordNum - 1);
		}
		return null;
	}

	protected VariableNode addCruleVariable(NamedNode type, int ordinalNumber, String name, EObject expr, EObject host)
			throws TranslationException {
		if (cruleVariables == null) {
			cruleVariables = new HashMap<NamedNode, List<VariableNode>>();
		}
		if (!cruleVariables.containsKey(type)) {
			List<VariableNode> newList = new ArrayList<VariableNode>();
			VariableNode var = new VariableNode(name);
			var.setCRulesVariable(true);
			var.setType(validateNamedNode(type));
			var.setHostObject(getHostEObject());
			newList.add(var);
			cruleVariables.put(type, newList);
			return var;
		} else {
			List<VariableNode> existingList = cruleVariables.get(type);
			Iterator<VariableNode> nodeitr = existingList.iterator();
			while (nodeitr.hasNext()) {
				if (nodeitr.next().getName().equals(name)) {
					return null;
				}
			}
			int idx = existingList.size();
			if (idx == ordinalNumber - 1) {
				VariableNode var = new VariableNode(name);
				var.setCRulesVariable(true);
				var.setType(validateNamedNode(type));
				var.setHostObject(getHostEObject());
				existingList.add(var);
				return var;
			} else {
				if (!isInDisjunctiveContainer(expr)) {
					// it would be ok, in some circumstances, to have a conjunctive variable definition, each different
					addError(
						"There is already an implicit variable with ordinality " + ordinalNumber + ". Please use 'a "
								+ nextOrdinal(ordinalNumber) + "' to create another implicit variable or 'the "
								+ nextOrdinal(ordinalNumber - 1) + "' to refer to the existing implicit variable.",
						expr);
				}
				return existingList.get(existingList.size() - 1);
			}
		}
	}

	private boolean isInDisjunctiveContainer(EObject expr) {
		if (expr instanceof BinaryOperation && ((BinaryOperation)expr).getOp().equals("or")) {
			return true;
		}
		else if (expr.eContainer() != null) {
			return isInDisjunctiveContainer(expr.eContainer());
		}
		return false;
	}

	protected void clearCruleVariables() {
		if (cruleVariables != null) {
			cruleVariables.clear();
		}
		vNum = 0; // reset system-generated variable name counter
	}

	protected void clearCruleVariablesForHostObject(EObject host) {
		if (cruleVariables != null) {
			Iterator<NamedNode> crvitr = cruleVariables.keySet().iterator();
			while (crvitr.hasNext()) {
				List<VariableNode> varsOfType = cruleVariables.get(crvitr.next());
				for (int i = varsOfType.size() - 1; i >= 0; i--) {
					if (varsOfType.get(i).getHostObject() != null && varsOfType.get(i).getHostObject().equals(host)) {
						varsOfType.remove(i);
					}
				}
			}
		}
	}

	protected boolean processModelImports(Ontology modelOntology, URI importingResourceUri, SadlModel model)
			throws OperationCanceledError {
		boolean failure = false;
		EList<SadlImport> implist = model.getImports();
		Iterator<SadlImport> impitr = implist.iterator();
		while (impitr.hasNext()) {
			SadlImport simport = impitr.next();
			SadlModel importedResource = simport.getImportedResource();
			if (importedResource != null) {
				// URI importingResourceUri = resource.getURI();
				String importUri = importedResource.getBaseUri();
				String importPrefix = simport.getAlias();
				Resource eResource = importedResource.eResource();
				if (eResource instanceof XtextResource) {
					XtextResource xtrsrc = (XtextResource) eResource;
					URI importedResourceUri = xtrsrc.getURI();
					OntModel importedOntModel = OntModelProvider.find(xtrsrc);
					if (importedOntModel == null) {
						logger.debug("JenaBasedSadlModelProcessor failed to resolve null OntModel for Resource '"
								+ importedResourceUri + "' while processing Resource '" + importingResourceUri + "'");
					} else {
						addImportToJenaModel(modelName, importUri, importPrefix, importedOntModel);
					}
				} else if (eResource instanceof ExternalEmfResource) {
					ExternalEmfResource emfResource = (ExternalEmfResource) eResource;
					addImportToJenaModel(modelName, importUri, importPrefix, emfResource.getOntModel());
				}
				else {
					failure = true;
				}
			}
			else {
				failure = true;
			}
		}
		return !failure;
	}

	// protected Literal sadlExplicitValueToLiteral(SadlExplicitValue value,
	// OntProperty prop) throws JenaProcessorException, TranslationException {
	protected boolean isEObjectPreprocessed(EObject eobj) {
		if (preprocessedEObjects != null && preprocessedEObjects.contains(eobj)) {
			return true;
		}
		return false;
	}

	protected boolean eobjectPreprocessed(EObject eobj) {
		if (preprocessedEObjects == null) {
			preprocessedEObjects = new ArrayList<EObject>();
			preprocessedEObjects.add(eobj);
			return true;
		}
		if (preprocessedEObjects.contains(eobj)) {
			return false;
		}
		preprocessedEObjects.add(eobj);
		return true;
	}

	protected void checkShallForControlledProperty(Expression expr) {
		OntConceptType exprType = null;
		SadlResource sr = null;
		if (expr instanceof Name) {
			sr = ((Name) expr).getName();
		} else if (expr instanceof SadlResource) {
			sr = (SadlResource) expr;
		}
		if (sr != null) {
			try {
				exprType = getDeclarationExtensions().getOntConceptType(sr);
				if (!isProperty(exprType)) {
					addError("Expected a property as controlled variable in a Requirement shall statement", expr);
				}
			} catch (CircularDefinitionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	protected List<SadlResource> getControlledPropertiesForTable(Expression expr) {
		List<SadlResource> results = new ArrayList<SadlResource>();
		if (expr instanceof Name) {
			results.add(((Name) expr).getName());
		} else if (expr instanceof SadlResource) {
			results.add((SadlResource) expr);
		} else if (expr instanceof BinaryOperation && ((BinaryOperation) expr).getOp().equals("and")) {
			List<SadlResource> leftList = getControlledPropertiesForTable(((BinaryOperation) expr).getLeft());
			if (leftList != null) {
				results.addAll(leftList);
			}
			List<SadlResource> rightList = getControlledPropertiesForTable(((BinaryOperation) expr).getRight());
			if (rightList != null) {
				results.addAll(rightList);
			}
		} else if (expr instanceof PropOfSubject) {
			List<SadlResource> propList = getControlledPropertiesForTable(((PropOfSubject) expr).getLeft());
			if (propList != null) {
				results.addAll(propList);
			}
		} else {
			addError("Tabular requirement appears to have an invalid set statement", expr);
		}
		return results;
	}

	public VariableNode getVariable(String name) {
		Object trgt = getTarget();
		if (trgt instanceof Rule) {
			return ((Rule) trgt).getVariable(name);
		} else if (trgt instanceof Query) {
			return ((Query) trgt).getVariable(name);
		}
		return null;
	}

	private void setSadlCommands(List<SadlCommand> sadlCommands) {
		this.sadlCommands = sadlCommands;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.ge.research.sadl.jena.IJenaBasedModelProcessor#compareTranslations(java.
	 * lang.String, java.lang.String)
	 */
	@Override
	public boolean compareTranslations(String result, String evalTo) {
		evalTo = removeNewLines(evalTo);
		evalTo = removeLocation(evalTo);
		result = removeNewLines(result);
		result = removeLocation(result);
		boolean stat = result.equals(evalTo);
		if (!stat) {
			System.err.println("Comparison failed:");
			System.err.println("  " + result);
			System.err.println("  " + evalTo);
		}
		return stat;
	}

	protected String removeNewLines(String evalTo) {
		String cleanString = evalTo.replaceAll("\r", "").replaceAll("\n", "").replaceAll(" ", "");
		cleanString = cleanString.replaceAll("\t", "");
		return cleanString;
	}

	protected String removeLocation(String str) {
		int locloc = str.indexOf("location(");
		while (locloc > 0) {
			int afterloc = str.indexOf("sreq(name(", locloc);
			String before = str.substring(0, locloc);
			int realStart = before.lastIndexOf("sreq(name(");
			if (realStart > 0) {
				before = str.substring(0, realStart);
			}
			if (afterloc > 0) {
				String after = str.substring(afterloc);
				str = before + after;
			} else {
				str = before;
			}
			locloc = str.indexOf("location(");
		}
		return str;
	}

	public boolean isUseArticlesInValidation() {
		return useArticlesInValidation;
	}

	protected void checkForArticleForNameInTriple(Expression value, Object triple) throws InvalidNameException {
		if(triple instanceof TripleElement) {
			Node tripleObject = ((TripleElement)triple).getObject();
			if (tripleObject == null && ((TripleElement)triple).getModifierType().equals(TripleModifierType.None)) {
				tripleObject = new ConstantNode(SadlConstants.CONSTANT_NONE);
				((TripleElement)triple).setObject(tripleObject);
			}
			if (isUseArticlesInValidation() && value instanceof Name && tripleObject instanceof NamedNode
					&& ((NamedNode) tripleObject).getNodeType().equals(NodeType.ClassNode)) {
				addError(SadlErrorMessages.NEEDS_ARTICLE.get(), value);
			}
		}
	}

	public boolean isBuiltinMissingArgument(String funcName, int size) {
		if ((funcName.equals("is") || funcName.equals("assign")) && size == 2) {
			return false;
		}
		if (funcName.equals("+") || funcName.equals("*") || funcName.equals("+") || funcName.equals("/")
				|| funcName.equals("-") || funcName.equals("%") || funcName.equals("^")) {
			if (size == 2) {
				return true;
			} else {
				return false;
			}
		}
		if (isComparisonOperator(funcName) && size == 2) {
			return false;
		}
		if (funcName.equals("unittedQuantity") && size == 2) {
			return true;
		}
		return true; // default? get from builtinfunction signatures?
	}

	protected boolean addVariableDefinition(VariableNode var, Object defn, NamedNode leftDefnType, EObject expr) throws TranslationException {
		// This model processor doesn't need to do anything
		return false;
	}

	public boolean elementIdentificationOperation(String op) {
		if (op.equals("contains") || op.equals("does not contain")) {
			return true;
		}
		if (op.equals("member of list")) {
			return true;
		}
		return false;
	}

	private List<TripleElement> getTriplesOfInterestList(List<TripleElement> found, GraphPatternElement gpe) throws InvalidTypeException {
		if (gpe instanceof TripleElement) {
			if (((TripleElement)gpe).getSubject() != null) {
				found = addTripleElement(found, (TripleElement)gpe);
				if (((TripleElement)gpe).getSubject() instanceof ProxyNode) {
					found = addTripleOfInterest(found, ((TripleElement)gpe).getSubject());
				}
			}
		}
		else if (gpe instanceof BuiltinElement) {
			if (((BuiltinElement)gpe).getArguments() != null) {
				for (int i = 0; i < ((BuiltinElement)gpe).getArguments().size(); i++) {
					found = addTripleOfInterest(found, ((BuiltinElement)gpe).getArguments().get(i));
				}
			}
		}
		else if (gpe instanceof Junction) {
			Object lhs = ((Junction)gpe).getLhs();
			if (lhs instanceof Node) {
				found = addTripleOfInterest(found, (Node) lhs);
			}
			Object rhs = ((Junction)gpe).getRhs();
			if (rhs instanceof Node) {
				found = addTripleOfInterest(found, (Node) rhs);
			}
		}
		return found;
	}

	private List<TripleElement> addTripleElement(List<TripleElement> found, TripleElement tr) {
		found.add(tr);
		return found;
	}

	private List<TripleElement> addTripleOfInterest(List<TripleElement> found, Node node) throws InvalidTypeException {
		if (node instanceof NamedNode && !(node instanceof VariableNode)) {
			if (isProperty((NamedNode)node)) {
				TripleElement newtr = new TripleElement(null, node, null);
				((NamedNode) node).setMissingTripleReplacement(new ProxyNode(newtr));
				found.add(newtr);
			}
			else if (node instanceof NamedNode && ((NamedNode)node).getNodeType().equals(NodeType.InstanceNode)) {
				// do nothing?
			}
			else {
				TripleElement newtr = new TripleElement(node, null, null);
				((NamedNode) node).setMissingTripleReplacement(new ProxyNode(newtr));
				found.add(newtr);
			}
		}
		else if (node instanceof ProxyNode) {
			found = getTriplesOfInterestList(found, (GraphPatternElement) ((ProxyNode) node).getProxyFor());
		}
		return found;
	}

	public boolean isAssignment(EObject expr) {
		// TODO make this smarter
		return false;
	}
}
