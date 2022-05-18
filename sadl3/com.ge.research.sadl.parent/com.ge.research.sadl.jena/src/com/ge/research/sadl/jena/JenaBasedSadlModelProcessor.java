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
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLCARDINALITYCONDITION_CARDINALITY;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLHASVALUECONDITION_RESTRICTION;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLNESTEDINSTANCE_TYPE;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLPROPERTYINITIALIZER_PROPERTY;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLPROPERTYINITIALIZER_VALUE;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLSTATEMENT_CLASSES;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLSTATEMENT_CLASSORPROPERTY;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLSTATEMENT_PROPCONDITIONS;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLSTATEMENT_SAMEAS;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLSTATEMENT_SUPERELEMENT;
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.SADLSTATEMENT_TYPE;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.stream.Collectors;

import org.apache.jena.datatypes.RDFDatatype;
import org.apache.jena.datatypes.TypeMapper;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.ext.xerces.impl.dv.InvalidDatatypeFacetException;
import org.apache.jena.ext.xerces.impl.dv.ValidationContext;
import org.apache.jena.ext.xerces.impl.dv.XSFacets;
import org.apache.jena.ext.xerces.impl.dv.XSSimpleType;
import org.apache.jena.ext.xerces.impl.dv.xs.XSSimpleTypeDecl;
import org.apache.jena.ext.xerces.xs.XSObject;
import org.apache.jena.ontology.AllValuesFromRestriction;
import org.apache.jena.ontology.AnnotationProperty;
import org.apache.jena.ontology.CardinalityRestriction;
import org.apache.jena.ontology.ComplementClass;
import org.apache.jena.ontology.DataRange;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.EnumeratedClass;
import org.apache.jena.ontology.HasValueRestriction;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.IntersectionClass;
import org.apache.jena.ontology.MaxCardinalityRestriction;
import org.apache.jena.ontology.MinCardinalityRestriction;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntDocumentManager;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.ontology.OntProperty;
import org.apache.jena.ontology.OntResource;
import org.apache.jena.ontology.Ontology;
import org.apache.jena.ontology.Restriction;
import org.apache.jena.ontology.SomeValuesFromRestriction;
import org.apache.jena.ontology.UnionClass;
import org.apache.jena.rdf.model.HasNoModelException;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.NodeIterator;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFList;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.RDFWriterI;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.sparql.JenaTransactionException;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.OWL2;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.XSD;
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
import org.eclipse.xtext.nodemodel.SyntaxErrorMessage;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextSyntaxDiagnostic;
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
import com.ge.research.sadl.model.CircularDefinitionException;
import com.ge.research.sadl.model.ConceptIdentifier;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.ConceptName.RangeValueType;
import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.PrefixNotFoundException;
import com.ge.research.sadl.model.SadlIntersectionClass;
import com.ge.research.sadl.model.SadlUnionClass;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.ConstantNode;
import com.ge.research.sadl.model.gp.EndWrite;
import com.ge.research.sadl.model.gp.Equation;
import com.ge.research.sadl.model.gp.Explain;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.Junction.JunctionType;
import com.ge.research.sadl.model.gp.JunctionList;
import com.ge.research.sadl.model.gp.JunctionNode;
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
import com.ge.research.sadl.model.gp.TypedEllipsisNode;
import com.ge.research.sadl.model.gp.UnknownNode;
import com.ge.research.sadl.model.gp.UntypedEllipsisNode;
import com.ge.research.sadl.model.gp.Update;
import com.ge.research.sadl.model.gp.ValueTableNode;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.model.persistence.SadlPersistenceFormat;
import com.ge.research.sadl.owl2sadl.OwlToSadl;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.ISadlOntologyHelper.Context;
import com.ge.research.sadl.processing.OntModelProvider;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SadlConstants.OWL_FLAVOR;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.processing.ValidationAcceptorExt;
import com.ge.research.sadl.reasoner.AmbiguousNameException;
import com.ge.research.sadl.reasoner.CircularDependencyException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
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
import com.ge.research.sadl.sADL.SadlExplicitValueLiteral;
import com.ge.research.sadl.sADL.SadlHasValueCondition;
import com.ge.research.sadl.sADL.SadlImport;
import com.ge.research.sadl.sADL.SadlInstance;
import com.ge.research.sadl.sADL.SadlIntersectionType;
import com.ge.research.sadl.sADL.SadlIsAnnotation;
import com.ge.research.sadl.sADL.SadlIsFunctional;
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
import com.ge.research.sadl.sADL.SadlReturnDeclaration;
import com.ge.research.sadl.sADL.SadlSameAs;
import com.ge.research.sadl.sADL.SadlSimpleTypeReference;
import com.ge.research.sadl.sADL.SadlStringLiteral;
import com.ge.research.sadl.sADL.SadlTableDeclaration;
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
import com.ge.research.sadl.sADL.ThereExistsStatement;
import com.ge.research.sadl.sADL.UnaryExpression;
import com.ge.research.sadl.sADL.UnitExpression;
import com.ge.research.sadl.sADL.UpdateExpression;
import com.ge.research.sadl.sADL.UpdateStatement;
import com.ge.research.sadl.sADL.ValueRow;
import com.ge.research.sadl.sADL.ValueTable;
import com.ge.research.sadl.utils.PathToFileUriConverter;
import com.ge.research.sadl.utils.ResourceManager;
import com.ge.research.sadl.utils.SadlASTUtils;
import com.ge.research.sadl.utils.SadlProjectHelper;
import com.google.common.base.Preconditions;
import com.google.common.base.Stopwatch;
import com.google.common.collect.Iterables;

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
		ALIAS, NOTE, SEE
	}

	private static List<String> sadlTokens = null;

	private List<String> comparisonOperators = Arrays.asList(">=", ">", "<=", "<", "==", "!=", "is", "=", "not",
			"unique", "in", "contains", "does", /* "not", */"contain");
	private List<String> numericOperators = Arrays.asList("*", "+", "/", "-", "%", "^");
	private List<String> numericComparisonOperators = Arrays.asList(">=", ">", "<=", "<");
	private List<String> equalityInequalityComparisonOperators = Arrays.asList("==", "!=", "is", "=");
	private List<String> mAssignmentOperators = Arrays.asList("is","=");
	private List<String> canBeNumericOperators = Arrays.asList(">=", ">", "<=", "<", "==", "!=", "is", "=");

	public enum OPERATORS_RETURNING_BOOLEAN {
		contains, unique, is, gt, ge, lt, le, and, or, not, was, hasBeen
	}

	public enum BOOLEAN_LITERAL_TEST {
		BOOLEAN_TRUE, BOOLEAN_FALSE, NOT_BOOLEAN, NOT_BOOLEAN_NEGATED
	}

	private int vNum = 0; // used to create unique variables
	private List<String> userDefinedVariables = new ArrayList<String>();
	
	private Map<VariableNode, Individual> gpVariableMap = null;	// This maps the Intermediate Form variables to the URIs of the corresponding OWL variables
	private List<VariableNode> variablesTyped = new ArrayList<VariableNode>();	// This keeps track of which variables have a type constraint in the OWL


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
	private OntModel sadlServicesConfigConceptModel = null;

	private OntModel sadlImplicitModel = null;
	private OntModel sadlBuiltinFunctionModel = null;

	protected JenaBasedSadlModelValidator modelValidator = null;
	protected ValidationAcceptor issueAcceptor = null;
	protected CancelIndicator cancelIndicator = null;

	private boolean lookingForFirstProperty = false; // in rules and other constructs, the first property may be
														// significant (the binding, for example)
	private boolean typeCheckingErrorDetected = false;

	protected List<String> importsInOrderOfAppearance = null; // an ordered set of import URIs, ordered by appearance in
																// file.
	private List<Rule> rules = null;
	private Equation currentEquation = null;
	private List<SadlCommand> sadlCommands = null;
	private SadlCommand targetCommand = null;
	public List<TripleElement> eventConj = new ArrayList<TripleElement>();

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

	private Map<String, List<ConceptName>> impliedPropoertiesCache = new HashMap<>();
	private Map<String, Boolean> classIsSubclassOfCache = new HashMap<>();

	/**
	 * Method to determine if subcls is a subclass of cls
	 * @param subcls
	 * @param cls
	 * @param rootCall
	 * @param previousClasses
	 * @return
	 * @throws CircularDependencyException
	 */
	protected boolean classIsSubclassOfCached(OntClass subcls, OntResource cls, boolean rootCall, List<OntResource> previousClasses) throws CircularDependencyException {
		String key = new StringBuilder()
				.append(subcls == null ? "null" : subcls.getURI())
				.append("-")
				.append(cls == null ? "null" : cls.getURI())
				.append("-")
				.append(rootCall)
				.append("-")
				.append(previousClasses == null ? "null" : String.join(",", previousClasses.stream().map(OntResource::getURI).collect(Collectors.toList())))
				.toString();
		Boolean result = classIsSubclassOfCache.get(key);
		if (result == null) {
			result = SadlUtils.classIsSubclassOf(subcls, cls, rootCall, previousClasses);
			classIsSubclassOfCache.put(key, result);
		}
		return result;
	}

	public class DataDescriptor {
		private Node name = null;
		private Node type = null;
		private List<String> units = null;
		private Object augType = null;
		private Individual gpVariable = null;
		
		public DataDescriptor(Node nm, Node typ, EList<String> units, Object augTypeObj) {
			setName(nm);
			setType(typ);
			setUnits(units);
			setAugType(augTypeObj);
		}
		
		public Object getAugType() {
			return augType;
		}
		public void setAugType(Object augType) {
			this.augType = augType;
		}
		public List<String> getUnits() {
			return units;
		}
		public void setUnits(List<String> units) {
			this.units = units;
		}
		public Node getType() {
			return type;
		}
		public void setType(Node type) {
			this.type = type;
		}
		public Node getName() {
			return name;
		}
		public void setName(Node name) {
			this.name = name;
		}

		public Individual getGpVariable() {
			return gpVariable;
		}

		public void setGpVariable(Individual gpVariable) {
			this.gpVariable = gpVariable;
		}

	}
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
	
	public boolean isUseArticlesInValidation() {
		return useArticlesInValidation;
	}

	public void setExpandMissingPatternsInValidation(boolean expandMissingPatternsInValidation) {
		this.expandMissingPatternsInValidation = expandMissingPatternsInValidation;	
	}
	
	public boolean isExpandMissingPatternsInValidation() {
		return expandMissingPatternsInValidation;
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
		// update project dependencies
		try {
			List<java.net.URI> projectDependencies = projectHelper.getReferencedProjectURIs(resource);
			getConfigMgr().addProjectDependencies(projectDependencies);
		} catch (Exception e2) {
			e2.printStackTrace();
		}
		
		generationInProgress = true;
		setCurrentResource(resource);
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
									String translatedQuery = ((Query)cmd).getPreparedQueryString();
									if (translatedQuery == null) {
										try {
											translatedQuery = translator.translateQuery(getTheJenaModel(), getModelName(), (Query) cmd);
										} catch (AmbiguousNameException e) {
											addError(e.getMessage(), (EObject) ((Query)cmd).getContext());
										} catch (UnsupportedOperationException e) {
											IReasoner defaultReasoner = getConfigMgr(resource, format)
													.getOtherReasoner(ConfigurationManager.DEFAULT_REASONER);
											translator = getConfigMgr(resource, format)
													.getTranslatorForReasoner(defaultReasoner);
											try {
												translatedQuery = translator.translateQuery(getTheJenaModel(), getModelName(), (Query) cmd);
											} catch (AmbiguousNameException e1) {
												addError(e.getMessage(), (EObject) ((Query)cmd).getContext());
											}
										}
									}
									Literal queryLit = getTheJenaModel().createTypedLiteral(translatedQuery);
									queryInst.addProperty(RDFS.isDefinedBy, queryLit);
									namedQueryList.add(queryInst.getURI());
								} catch (TranslationException e) {
									e.printStackTrace();
								} catch (InvalidNameException e) {
									e.printStackTrace();
								}
							}
						}
					}
				}
			} catch (ConfigurationException e1) {
				e1.printStackTrace();
			}

			try {
				URI lastSeg = fsa.getURI(resource.getURI().lastSegment());
				String owlFN = getOwlFilename(lastSeg, format);
				SadlUtils su = new SadlUtils();
				String modelFolder = getModelFolderPath(resource);
				// Persist the OWL model
				if (format != null && !format.equals("Jena TDB")) {
					getConfigMgr().cleanTdbFolder();
				}
				generateOwlFile(fsa, modelFolder, owlFN, getTheJenaModel().getBaseModel(), getModelName(), getModelAlias(), format);

				String fn = SadlConstants.SADL_BASE_MODEL_FILENAME + "." + ResourceManager.getOwlFileExtension(format);
				if (!fileExists(fsa, fn)) {
					sadlBaseModel = OntModelProvider.getSadlBaseModel();
					if (sadlBaseModel != null) {
						generateOwlFile(fsa, modelFolder, fn, sadlBaseModel.getBaseModel(), 
								SadlConstants.SADL_BASE_MODEL_URI, 
								SadlConstants.SADL_BASE_MODEL_PREFIX, format);
						addMapping(newMappings, su, modelFolder, fn, 
								SadlConstants.SADL_BASE_MODEL_URI, 
								SadlConstants.SADL_BASE_MODEL_PREFIX);
					}
				}
				else if (!mappingExists(SadlConstants.SADL_BASE_MODEL_URI)) {
					addMapping(newMappings, su, modelFolder, fn, 
							SadlConstants.SADL_BASE_MODEL_URI, 
							SadlConstants.SADL_BASE_MODEL_PREFIX);
				}
				fn = SadlConstants.SADL_LIST_MODEL_FILENAME + "." + ResourceManager.getOwlFileExtension(format);
				if (!fileExists(fsa, fn)) {
					sadlListModel = OntModelProvider.getSadlListModel();
					if (sadlListModel != null) {
						generateOwlFile(fsa, modelFolder, fn, sadlListModel.getBaseModel(), SadlConstants.SADL_LIST_MODEL_URI, SadlConstants.SADL_LIST_MODEL_PREFIX, format);
						addMapping(newMappings, su, modelFolder, fn, 
								SadlConstants.SADL_LIST_MODEL_URI,
								SadlConstants.SADL_LIST_MODEL_PREFIX);
					}
				}
				else if (!mappingExists(SadlConstants.SADL_LIST_MODEL_URI)) {
					addMapping(newMappings, su, modelFolder, fn, 
							SadlConstants.SADL_LIST_MODEL_URI,
							SadlConstants.SADL_LIST_MODEL_PREFIX);
				}
				fn = SadlConstants.SADL_DEFAULTS_MODEL_FILENAME + "." + ResourceManager.getOwlFileExtension(format);
				if (!fileExists(fsa, fn)) {
					sadlDefaultsModel = OntModelProvider.getSadlDefaultsModel();
					if (sadlDefaultsModel != null) {
						generateOwlFile(fsa, modelFolder, fn, sadlDefaultsModel.getBaseModel(), SadlConstants.SADL_DEFAULTS_MODEL_URI, SadlConstants.SADL_DEFAULTS_MODEL_PREFIX, format);
						addMapping(newMappings, su, modelFolder, fn,
								SadlConstants.SADL_DEFAULTS_MODEL_URI,
							SadlConstants.SADL_DEFAULTS_MODEL_PREFIX);
					}
				}
				else if (!mappingExists(SadlConstants.SADL_DEFAULTS_MODEL_URI)) {
					addMapping(newMappings, su, modelFolder, fn,
							SadlConstants.SADL_DEFAULTS_MODEL_URI,
						SadlConstants.SADL_DEFAULTS_MODEL_PREFIX);
				}
				fn = SadlConstants.SADL_SERVICES_CONFIGURATION_FILENAME + "." + ResourceManager.getOwlFileExtension(format);
				if (!fileExists(fsa, fn)) {
					sadlServicesConfigConceptModel = OntModelProvider.getSadlServicesConfigConceptsModel();
					if (sadlServicesConfigConceptModel != null) {
						generateOwlFile(fsa, modelFolder, fn, sadlServicesConfigConceptModel.getBaseModel(), SadlConstants.SADL_SERIVCES_CONFIGURATION_URI, SadlConstants.SADL_SERIVCES_CONFIGURATION_CONCEPTS_PREFIX, format);
						addMapping(newMappings, su, modelFolder, fn,
								SadlConstants.SADL_SERIVCES_CONFIGURATION_CONCEPTS_URI,
								SadlConstants.SADL_SERIVCES_CONFIGURATION_CONCEPTS_PREFIX);
					}
				}
				else if (!mappingExists(SadlConstants.SADL_SERIVCES_CONFIGURATION_CONCEPTS_URI)) {
					addMapping(newMappings, su, modelFolder, fn,
							SadlConstants.SADL_SERIVCES_CONFIGURATION_CONCEPTS_URI,
							SadlConstants.SADL_SERIVCES_CONFIGURATION_CONCEPTS_PREFIX);
				}
				String[] mapping = new String[4];
				mapping[0] = su.fileNameToFileUrl(modelFolder + "/" + owlFN);
				mapping[1] = getModelName();
				mapping[2] = getModelAlias();
				mapping[3] = resource.getURI().toString();

				newMappings.add(mapping);

				// Output the Rules and any other knowledge structures via the specified
				// translator
				List<Object> otherContent = OntModelProvider.getOtherContent(resource);
				if (otherContent != null) {
					for (int i = 0; i < otherContent.size(); i++) {
						Object oc = otherContent.get(i);
						if (oc instanceof List<?>) {
							if (((List<?>) oc).get(0) instanceof Rule) {
								setRules((List<Rule>) oc);
							}
						}
					}
				}
				List<ModelError> results = translateAndSaveModel(resource, owlFN, format, newMappings, "SADL");
				if (results != null) {
					generationInProgress = false; // we need these errors to show up
					modelErrorsToOutput(resource, results, false);
				}
			} catch (Exception e) {
				if (e.getMessage() == null) {
					e.printStackTrace();
				}
				System.err.println(e.getMessage());
			}
		}
		generationInProgress = false;
		logger.debug("onGenerate completed for Resource '" + resource.getURI() + "'");
	}

	/**
	 * Method to add a simple mapping to the ConfiguationManager's mappings model.
	 * @param newMappings
	 * @param su
	 * @param modelFolder
	 * @param fn
	 * @param publicUri
	 * @param prefix
	 * @throws URISyntaxException
	 */
	private void addMapping(List<String[]> newMappings, SadlUtils su, String modelFolder, String fn,
			String publicUri, String prefix)
			throws URISyntaxException {
		String[] mapping = new String[3];
		mapping[0] = su.fileNameToFileUrl(modelFolder + "/" + fn);
		mapping[1] = publicUri;
		mapping[2] = prefix;
		newMappings.add(mapping);
	}

	/**
	 * Method to determine if a model URI is found in the mappings
	 * @param modelUri
	 * @return
	 * @throws ConfigurationException
	 */
	private boolean mappingExists(String modelUri) throws ConfigurationException {
		String altUrl = getConfigMgr().getAltUrlFromPublicUri(modelUri);
		if (altUrl == null || altUrl.equals(modelUri)) {
			return false;
		}
		return true;
	}

	private void generateOwlFile(IFileSystemAccess2 fsa, String modelFolder, String owlFN, Model model, String modelName, String modelAlias, String format) throws TranslationException, IOException {
		if (isBinary(format)) {
			// IFileSystemAccess2.generateFile doesn't appear to handle binary files.
			String ofn = modelFolder + "/" + owlFN;
			getConfigMgr().getSadlModelGetterPutter(format).saveModel(model, modelName, modelName, ofn, format);
		}
		else {
			Charset charset = Charset.forName("UTF-8");
			CharSequence seq = getConfigMgr().getSadlModelGetterPutter(format).getModelAsString(model, modelAlias, modelName, format, charset);
			fsa.generateFile(owlFN, seq);
		}
	}

	private boolean isBinary(String format) {
		if (format.equals("RDF Binary")) {
			return true;
		} else
			try {
				if (SadlPersistenceFormat.getRDFFormat(format).equals(SadlPersistenceFormat.TDB_PseudoFormat) 
					|| SadlPersistenceFormat.getRDFFormat(format).equals(SadlPersistenceFormat.SEMTK_PseudoFormat)) {
					return true;
				}
			} catch (TranslationException e) {
				e.printStackTrace();
			}
		return false;
	}

	private CharSequence serializeModelToString(Model model, String prefix, String modelName, String format, Charset charset) throws TranslationException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		if (format.equals(SadlPersistenceFormat.RDF_XML_FORMAT) || 
				format.equals(SadlPersistenceFormat.RDF_XML_ABBREV_FORMAT)) {
			RDFWriterI w2 = model.getWriter(format);
			w2.setProperty("xmlbase", modelName);
			w2.write(model, out, modelName);
		}
		else {
			if (prefix != null) {
				getTheJenaModel().getBaseModel().setNsPrefix(prefix, modelName);
				if (prefix.length() > 0) {
					// also add the empty string prefix to enable finding the URI of this model from the OWL file
					getTheJenaModel().getBaseModel().setNsPrefix("", modelName);
				}	
			}
			RDFDataMgr.write(out, model, SadlPersistenceFormat.getRDFFormat(format));
		}	
		CharSequence seq = new String(out.toByteArray(), charset);
		return seq;
	}

	protected String getOwlFilename(URI lastSeg, String format) throws TranslationException {
		String owlFN = lastSeg.trimFileExtension().appendFileExtension(SadlPersistenceFormat.getFileExtension(SadlPersistenceFormat.getRDFFormat(format)))
				.lastSegment().toString();
		return owlFN;
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

	protected List<ModelError> translateAndSaveModel(Resource resource, String owlFN, String _repoType,
			List<String[]> newMappings, String source) {
		String modelFolderPathname = getModelFolderPath(resource);
		try {
			if (newMappings != null) {
				getConfigMgr(resource, _repoType).addMappings(newMappings, false, source);
			}
			ITranslator translator = getConfigMgr(resource, _repoType).getTranslator();
			List<ModelError> results = translator.translateAndSaveModel(getTheJenaModel(), getRules(),
					modelFolderPathname, getModelName(), getImportsInOrderOfAppearance(), owlFN);
			if (results != null) {
				modelErrorsToOutput(resource, results, false);
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
		try {
			if (isSyntheticUri(resource)) {
				return null;
			}
			final URI resourceUri = resource.getURI();
			java.net.URI root = this.projectHelper.getRoot(new java.net.URI(resourceUri.toString()));
			// This is for the headless tool-chain.
			if (root == null) {
				IPath lWorkspaceRoot = ResourcesPlugin.getWorkspace().getRoot().getLocation();
				IPath lResourcePath = new Path(resourceUri.toString());
				if(resourceUri.isFile()) {		
					root = new java.net.URI(lResourcePath.removeLastSegments(lResourcePath.segmentCount() - lWorkspaceRoot.segmentCount() - 2).toString());
				}
				if(resourceUri.isPlatformResource()) {
					root = lWorkspaceRoot.append(lResourcePath.segment(1)).toFile().toURI();
				}
			}
			return Paths.get(root).resolve(UtilsForJena.OWL_MODELS_FOLDER_NAME).toString();
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * @deprecated use {@link SadlProjectHelper#getRoot(java.net.URI)} instead.
	 */
	@Deprecated
	static String findProjectPath(URI uri) {
		String modelFolder = findModelFolderPath(uri);
		if (modelFolder != null) {
			return new File(modelFolder).getParent();
		}
		return null;
	}

	/**
	 * @deprecated use {@link SadlProjectHelper#getRoot(java.net.URI)} instead.
	 */
	@Deprecated
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

	protected void modelErrorsToOutput(Resource resource, List<ModelError> errors, boolean useMarkersIfPossible) {
		for (int i = 0; errors != null && i < errors.size(); i++) {
			ModelError err = errors.get(i);
			if (err.getErrorType().equals(ErrorType.ERROR)) {
				if (useMarkersIfPossible && err.getContext() != null && err.getContext() instanceof EObject) {
					addError(err.getErrorMsg(), (EObject) err.getContext());
				}
				else {
					System.err.println(err.getErrorMsg());
				}
			}
			else if (err.getErrorType().equals(ErrorType.WARNING)){
				if (useMarkersIfPossible && err.getContext() != null && err.getContext() instanceof EObject) {
					addWarning(err.getErrorMsg(), (EObject) err.getContext());
				}
				else {
					System.out.println(err.getErrorMsg());
				}
			}
			else {
				if (useMarkersIfPossible && err.getContext() != null && err.getContext() instanceof EObject) {
					addInfo(err.getErrorMsg(), (EObject) err.getContext());
				}
				else {
					System.out.println(err.getErrorMsg());
				}
			}
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
	private boolean ignoreUnittedQuantities;

	private boolean expandUnittedQuantities;

	private boolean useArticlesInValidation;
	
	private boolean expandMissingPatternsInValidation;

	private boolean domainAndRangeAsUnionClasses = true;

	private boolean typeCheckingWarningsOnly;
	
	private boolean typeCheckingRangeRequired = true;
	
	private boolean enableMetricsCollection = false;
	
	private boolean typeUnsupportedDownstreamWarnings;
	
	//-----------END PEFERENCES --------------

	private List<OntResource> allImpliedPropertyClasses = null;

	private List<OntResource> allExpandedPropertyClasses = null;

	private ArrayList<Object> intermediateFormResults = null;

	private EObject hostEObject = null;

	private Map<EObject, VariableNode> variablesInDefinition = null;

	private EObject setDefaultEObject;

	protected Map<String, String> modelProcessorPreferenceMap;

	private List<Class> allowedVariableContainers;

	private List<EObject> undefinedEObjects = new ArrayList<EObject>();

	private Map<EObject, org.apache.jena.rdf.model.Resource> cachedJenaResource = null;

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
		if (getTheJenaModel() == null && ontModel != null) {
			theJenaModel = ontModel;
		}
		if (getConfigMgr() == null) {
			if (currentResource == null) {
				currentResource = candidate.eResource();
			}
			try {
				configMgr = getConfigMgr(currentResource, null);
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		SadlResource subject = context.getSubject();
		String sruri = subject != null ? getDeclarationExtensions().getConceptUri(subject) : null;
		String cruri = getDeclarationExtensions().getConceptUri(candidate);
//		System.out.println("Subject: " + getDeclarationExtensions().getConceptUri(subject));
//		System.out.println("Candidate: " + getDeclarationExtensions().getConceptUri(candidate));
		if (sruri != null && sruri.equals(cruri)) {
			context.getAcceptor().add("candidate and subject the same", candidate, Severity.ERROR);
			return;
		}
		try {
			OntConceptType oct = subject !=  null ? getDeclarationExtensions().getOntConceptType(subject) : null;
			OntConceptType coct = getDeclarationExtensions().getOntConceptType(candidate);
			getModelValidator().resetValidatorState(null);
			if (subject == MISSING_SUBJECT) {
				if (contextId.equals(SADLSTATEMENT_CLASSORPROPERTY)) {
					OntConceptType candType = getDeclarationExtensions().getOntConceptType(candidate);
					String candUri = getDeclarationExtensions().getConceptUri(candidate);
					if (!candType.equals(OntConceptType.CLASS) && !candType.equals(OntConceptType.INSTANCE)) {
						context.getAcceptor().add("SADLSTATEMENT_CLASSORPROPERTY with no subject must be Class or Individual", candidate, Severity.ERROR);
					}
				}
				else if (contextId.equals(SADLSTATEMENT_SAMEAS)) {
					OntConceptType candType = getDeclarationExtensions().getOntConceptType(candidate);
					// if it has a "not" then it can only be a class.
					if (context.getCurrentModel() instanceof SadlSameAs &&
							((SadlSameAs)context.getCurrentModel()).isComplement() &&
							!candType.equals(OntConceptType.CLASS)) {
						context.getAcceptor().add("Only classes can be in same as not", candidate, Severity.ERROR);
					}
					if (candType.equals(OntConceptType.CLASS) || 
						candType.equals(OntConceptType.INSTANCE) ||
						isProperty(candType)) {
						return;
					}
					
					else {
						context.getAcceptor().add("Only classes, instances, and properties can be in same as", candidate, Severity.ERROR);
					}
				}
				else if (contextId.equals(SADLPROPERTYINITIALIZER_PROPERTY)) {
					if (context.getCurrentModel() instanceof SadlModel) {
						context.getAcceptor().add("Property initializer at top level?", candidate, Severity.ERROR);
					}
				}
				else if (contextId.equals(SADLSTATEMENT_TYPE)) {
					OntConceptType candType = getDeclarationExtensions().getOntConceptType(candidate);
					if (!candType.equals(OntConceptType.CLASS)) {
						context.getAcceptor().add("Only classes allowed here", candidate, Severity.ERROR);
					}
				}
				return;
			}
			
			if (logger.isDebugEnabled()) {
				logger.debug("\nSubject: " + sruri + ", " + oct.toString());
				logger.debug("Candidate: " + cruri + ", " + coct.toString());
			}

			switch (contextId) {
			case SADLNESTEDINSTANCE_TYPE: {
				if (!isProperty(oct) || !coct.equals(OntConceptType.CLASS)) {
					context.getAcceptor().add("No", candidate, Severity.ERROR);
					return;
				}
				OntProperty prop = ontModel.getOntProperty(sruri);
				StmtIterator rngitr = ontModel.listStatements(prop, RDFS.range, (RDFNode)null);
				while (rngitr.hasNext()) {
					RDFNode rng = rngitr.nextStatement().getObject();
					if (rng.isResource() && rng.asResource().canAs(OntClass.class)) {
						OntClass rngcls = rng.asResource().as(OntClass.class);
						OntClass candcls = ontModel.getOntClass(cruri);
						if (candcls != null) {
							if (candcls.equals(rngcls) ) {
								return;
							}
							try {
								if (checkForSubclassing(candcls, rngcls, candidate)) {
									return;
								}
							} catch (JenaProcessorException e) {
								context.getAcceptor().add(e.getMessage(), candidate, Severity.ERROR);	
								return;
							}
						}
					}
				}
				context.getAcceptor().add("No", candidate, Severity.ERROR);
				return;
			}
			case SADLPROPERTYINITIALIZER_PROPERTY: {
				OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
				if (!isProperty(candtype)) {
					context.getAcceptor().add("No", candidate, Severity.ERROR);
					return;
				}
				try {
					getModelValidator().checkPropertyDomain(ontModel, subject, candidate, candidate, true, false);
				} catch (CircularDependencyException e) {
					addError(e.getMessage(), subject);
				}
				return;
			}
			case SADLSTATEMENT_PROPCONDITIONS: {
				OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
				if (!isProperty(candtype)) {
					context.getAcceptor().add("No", candidate, Severity.ERROR);
					return;
				}
				try {
					getModelValidator().checkPropertyDomain(ontModel, subject, candidate, candidate, true, false);
				} catch (CircularDependencyException e) {
					addError(e.getMessage(), subject);
				}
				return;
			}
			case SADLPROPERTYINITIALIZER_VALUE: {
				// subject should be the class of the statement subject
				// restriction[0] should be the property
				SadlResource prop = null;
				if (context.getRestrictions() instanceof List<?> &&
						((List<?>)context.getRestrictions()).size() > 0) {
					prop = context.getRestrictions().iterator().next();
				}

				if (prop == null) {
					context.getAcceptor().add("No property found in restrictions", candidate, Severity.ERROR);
					return;
				}
				OntConceptType proptype = getDeclarationExtensions().getOntConceptType(prop);
				if (proptype.equals(OntConceptType.DATATYPE_PROPERTY)) {
					context.getAcceptor().add("No", candidate, Severity.ERROR);
					return;
				}
				if (proptype.equals(OntConceptType.CLASS_PROPERTY)) {
					OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
					if (!candtype.equals(OntConceptType.INSTANCE)) {
						String canduri = getDeclarationExtensions().getConceptUri(candidate);
						context.getAcceptor().add("'" + canduri + "' is not an Instance", candidate, Severity.ERROR);
						return;
					}
				}
//				Iterator<SadlResource> ritr = context.getRestrictions().iterator();
//				while (ritr.hasNext()) {
//					System.out.println("Restriction: " + getDeclarationExtensions().getConceptUri(ritr.next()));
//				}
				try {
					getModelValidator().checkPropertyDomain(ontModel, subject, prop, subject, true, false);
				} catch (CircularDependencyException e) {
					// TODO Auto-generated catch block
					addError(e.getMessage(), subject);
				}
				StringBuilder errorMessageBuilder = new StringBuilder();
				if (!getModelValidator().validateBinaryOperationByParts(subject, prop, candidate, "is",
						errorMessageBuilder, true)) {
					context.getAcceptor().add(errorMessageBuilder.toString(), candidate, Severity.ERROR);
				}
				return;
			}
			case SADLHASVALUECONDITION_RESTRICTION: {
				SadlResource prop = subject;
				OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
				if (!candtype.equals(OntConceptType.INSTANCE)) {
					String canduri = getDeclarationExtensions().getConceptUri(candidate);
					context.getAcceptor().add("'" + canduri + "' is not an Instance", candidate, Severity.ERROR);
					return;
				}
				OntProperty pred = getTheJenaModel().getOntProperty(sruri);
				OntResource obj = getTheJenaModel().getOntResource(cruri);
				getModelValidator().checkObjectPropertyRange(getTheJenaModel(), pred, obj, false, subject);
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
					try {
						getModelValidator().checkPropertyDomain(ontModel, candidate, subject, candidate, true, false);
					} catch (CircularDependencyException e) {
						addError(e.getMessage(), candidate);
					}
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
					try {
						getModelValidator().checkPropertyDomain(ontModel, candidate, subject, candidate, true, false);
					} catch (CircularDependencyException e) {
						addError(e.getMessage(), candidate);
					}
					return;
				}
				context.getAcceptor().add("No", candidate, Severity.ERROR);
				return;
			}
			case SADLSTATEMENT_CLASSES: {
				OntConceptType subjtype = getDeclarationExtensions().getOntConceptType(subject);
				OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
				if (subjtype.equals(OntConceptType.CLASS) &&
						!candtype.equals(OntConceptType.CLASS)) {
					addError("SADLSTATEMENT_CLASSES must continue with a class", candidate);
				}
				String snm = getDeclarationExtensions().getConceptUri(subject);
				String cnm = getDeclarationExtensions().getConceptUri(candidate);
				if (snm != null && cnm != null && snm.equals(cnm)) {
					addError("A class can't be disjoint with itself", candidate);
					return;
				}
				// if the subject is not a root class, the subject and candidate should have a common super class
				OntClass subcls = getTheJenaModel().getOntClass(snm);
				if (subcls != null) {
					List<OntClass> subjscls = new ArrayList<OntClass>();
					subjscls = getAllSuperClasses(subcls, subjscls);
					Iterator<OntClass> sclsitr = subjscls.iterator();
					if (sclsitr.hasNext()) {
						OntClass candcls = getTheJenaModel().getOntClass(cnm);
						if (candcls != null) {
							List<OntClass> candscls = new ArrayList<OntClass>();
							candscls = getAllSuperClasses(candcls, candscls);
							Iterator<OntClass> cclsitr = candscls.iterator();
							while (cclsitr.hasNext()) {
								OntClass ccls = cclsitr.next();
								if (subjscls.contains(ccls)) {
									return;
								}
							}
							addError("Disjoint classes should be subclasses of the same universe", candidate);
						}
					}
				}
				return;
			}
			case SADLSTATEMENT_CLASSORPROPERTY: {
				OntConceptType subjtype = getDeclarationExtensions().getOntConceptType(subject);
				OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
				if (!subjtype.equals(candtype)) {
					addError("SADLSTATEMENT_CLASSORPROPERTY must be of same type", candidate);
				}
				String snm = getDeclarationExtensions().getConceptUri(subject);
				String cnm = getDeclarationExtensions().getConceptUri(candidate);
				if (snm != null && cnm != null && snm.equals(cnm)) {
					addError("Duplicate entries not desirable", candidate);
					return;
				}				
			}
			case SADLCARDINALITYCONDITION_CARDINALITY: {
				OntConceptType subjtype = getDeclarationExtensions().getOntConceptType(subject);
				OntConceptType candtype = getDeclarationExtensions().getOntConceptType(candidate);
				String snm = getDeclarationExtensions().getConceptUri(subject);
				String cnm = getDeclarationExtensions().getConceptUri(candidate);
				if (subjtype.equals(OntConceptType.INSTANCE)) {
					if (!isProperty(candtype)) {
						addError("Expected a property", candidate);
						return;
					}
					Individual subjinst = getTheJenaModel().getIndividual(snm);
					OntProperty candprop = getTheJenaModel().getOntProperty(cnm);
					try {
						getModelValidator().checkPropertyDomain(getTheJenaModel(), subjinst, candprop, subject, true, null, false, false);
					} catch (CircularDependencyException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (TranslationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					return;
				}
				else {
					// TODO
				}
			}
			case SADLSTATEMENT_TYPE: {
//				if (oct !=  null && oct.equals(OntConceptType.CLASS)) {
//					// if the subject is a class they no SadlResource is valid after
//				}
				// must be a class
				if (!coct.equals(OntConceptType.CLASS)) {
					context.getAcceptor().add("must be a class", candidate, Severity.ERROR);
				}
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
		} catch (TranslationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (CircularDependencyException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} finally {
			if (savedIssueAccpetor != null) {
				setIssueAcceptor(savedIssueAccpetor);
			}
		}
	}

	private List<OntClass> getAllSuperClasses(OntClass cls, List<OntClass> superClasses) {
		if (cls != null) {
			StmtIterator cclsitr = getTheJenaModel().listStatements(cls, RDFS.subClassOf, (RDFNode)null);
			while (cclsitr.hasNext()) {
				RDFNode on = cclsitr.nextStatement().getObject();
				if (on.canAs(OntClass.class) ) {
					OntClass onc = on.as(OntClass.class);
					superClasses.add(onc);
					superClasses = getAllSuperClasses(onc, superClasses);
				}
			}
			
			// If cls belongs to a UnionClass which is the same as or a subclass of class X, 
			//	then X is a superclass of cls
			StmtIterator unionItr = getTheJenaModel().listStatements(null, OWL.unionOf, (RDFNode)null);
			while (unionItr.hasNext()) {
				Statement uostmt = unionItr.nextStatement();
				org.apache.jena.rdf.model.Resource subject = uostmt.getSubject();
				RDFNode on = uostmt.getObject();
				if (subject.canAs(OntClass.class)) {
					OntClass subjcls = subject.as(OntClass.class);				
					if (subjcls.isUnionClass()) {
						ExtendedIterator<? extends OntClass> opitr = subjcls.asUnionClass().listOperands();
						List<OntClass> opclses = new ArrayList<OntClass>();
						while (opitr.hasNext()) {
							try {
								OntClass opcls = opitr.next();
								opclses.add(opcls);
							}
							catch (Throwable t) {
								// System.err.println(t.getMessage()); // not sure why errors occur... awc 1/11/2021
							}
						}
						if (opclses.contains(cls)) {
							if (subject.canAs(OntClass.class)) {
								// is this a subclass of something?
								StmtIterator scitr = getTheJenaModel().listStatements(subjcls, RDFS.subClassOf, (RDFNode)null);
								while (scitr.hasNext()) {
									RDFNode scn = scitr.nextStatement().getObject();
									if (scn.isResource() && scn.asResource().canAs(OntClass.class)) {
										if (scn.isURIResource()) {
											superClasses.add(scn.asResource().as(OntClass.class));
										}
										superClasses = getAllSuperClasses(scn.asResource().as(OntClass.class), superClasses);
									}
								}
								// is there an equivalent class?
								StmtIterator eqclsitr = getTheJenaModel().listStatements(subjcls, OWL.equivalentClass, (RDFNode)null);
								while (eqclsitr.hasNext()) {
									RDFNode eqrn = eqclsitr.nextStatement().getObject();
									if (eqrn.canAs(OntClass.class)) {
										OntClass eqcls = eqrn.as(OntClass.class);
										superClasses.add(eqcls);
										superClasses = getAllSuperClasses(eqcls, superClasses);
									}
								}			
								eqclsitr = getTheJenaModel().listStatements(null, OWL.equivalentClass, subjcls);
								while (eqclsitr.hasNext()) {
									org.apache.jena.rdf.model.Resource eqrn = eqclsitr.nextStatement().getSubject();
									if (eqrn.canAs(OntClass.class)) {
										OntClass eqcls = eqrn.as(OntClass.class);
										superClasses.add(eqcls);
										superClasses = getAllSuperClasses(eqcls, superClasses);
									}
								}			
							}
						}
					}
				}
			}
			
		}
		return superClasses;
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

		Stopwatch stopwatch = Stopwatch.createStarted();
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
			theJenaModel = prepareEmptyOntModel(resource, context);
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
		// clear any pre-existing content
		List<Object> oc = OntModelProvider.getOtherContent(resource);
		if (oc != null) {
			Iterator<Object> itr = oc.iterator();
			List<Equation> eqs = new ArrayList<Equation>();
			while (itr.hasNext()) {
				Object nxt = itr.next();
				if (nxt instanceof Equation) {
					eqs.add((Equation)nxt);
				}
			}
			if (eqs.size() > 0) {
				oc.removeAll(eqs);
			}
		}

		try {
			// Add SadlBaseModel to everything except the SadlImplicitModel
			if (!resource.getURI().lastSegment().equals(SadlConstants.SADL_IMPLICIT_MODEL_FILENAME)) {
				addSadlBaseModelImportToJenaModel(resource);
			}
			// Add the SadlImplicitModel to everything except itself
			if (!resource.getURI().lastSegment().equals(SadlConstants.SADL_IMPLICIT_MODEL_FILENAME)) {
				addImplicitSadlModelImportToJenaModel(resource, context);

			}
			// Add the SadlBuilinFunctions to everything except itself and the SadlImplicitModel
			if (!resource.getURI().lastSegment().equals(SadlConstants.SADL_IMPLICIT_MODEL_FILENAME)
					&& !resource.getURI().lastSegment().equals(SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME)) {
				addImplicitBuiltinFunctionModelImportToJenaModel(resource, context);

			}
			if (modelActualUrl.equals(ResourceManager.ServicesConf_SFN)) {
				addSadlServicesConfigConceptsModelImportToJenaModel(resource, context);
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

		if(!processModelImports(modelOntology, resource.getURI(), model)) {
			return;
		}

		initializePreferences(context);

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

		// create validator for expressions
		initializeModelValidator();
		initializeAllImpliedPropertyClasses();
		initializeAllExpandedPropertyClasses();

	    boolean validAST = isAstSyntaxValid(model);	
	    if (!validAST) {
		    Iterable<XtextSyntaxDiagnostic> syntaxErrors = Iterables.<XtextSyntaxDiagnostic>filter(model.eResource().getErrors(), XtextSyntaxDiagnostic.class);
		    for (XtextSyntaxDiagnostic synErr : syntaxErrors) {
		    	String code = synErr.getCode();
		    	String msg = synErr.getMessage();
		    	String[] data = synErr.getData();
		    	int offset = synErr.getOffset();
		    	int len = synErr.getLength();
		    	int i = 0;
		    }
			List<SadlModelElement> elements = model.getElements();
			if (elements != null) {
				Iterator<SadlModelElement> elitr = elements.iterator();
				while (elitr.hasNext()) {
					SadlModelElement element = elitr.next();
					TreeIterator<EObject> allcontents = element.eAllContents();
					while (allcontents.hasNext()) {
						EObject obj = allcontents.next();
						ICompositeNode node = NodeModelUtils.findActualNodeFor(obj);
						if (node != null) {
							String txt = node.getText();
							if (node.getSyntaxErrorMessage() != null) {
								SyntaxErrorMessage synErr = node.getSyntaxErrorMessage();
								if (synErr != null) {
									System.err.println(synErr.getMessage());
								}
							}
						}
					}
				}
			}
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
				processModelElement(element);
			}
		}
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
					if (!isSyntheticUri(null, resource)) {
						// don't do metrics on JUnit tests
						if (getMetricsProcessor() != null) {
							getMetricsProcessor().saveMetrics(SadlPersistenceFormat.RDF_XML_ABBREV_FORMAT);
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
		logger.debug("onValidate completed for resource '" + resource.getURI() + "' [took " + stopwatch + "]");
	}

	@Override
	public void initializePreferences(ProcessorContext context) {
		getPreferences(currentResource);
	}

	protected void processModelElement(SadlModelElement element) {
		setDefaultEObject(element);
		try {
			if (getModelValidator() != null) {
				getModelValidator().clearImpliedPropertiesUsed();
			}
		} catch (TranslationException e1) {
			e1.printStackTrace();
		}
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
			} else if (element instanceof UpdateStatement) {
				processStatement((UpdateStatement) element);
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
						applyPulledUpOperations(processExpression(((ExpressionStatement) element).getExpr())));
				if (isSyntheticUri(null, element.eResource())) {
					// for JUnit tests, do not expand; expansion, if desired, will be done upon retrieval
					addIntermediateFormResult(rawResult);
				} else if (rawResult != null) {
					// for IDE, expand and also add as info marker
					addInfo(rawResult.toString(), element);
					getIfTranslator().setStartingVariableNumber(getVariableNumber());	// make sure IF doesn't duplicate var names
					Object intForm = getIfTranslator().expandProxyNodes(rawResult, false, true);
					setVariableNumber(getIfTranslator().getVariableNumber());  // make sure this processor doesn't duplicate var names
					if (intForm != null) {
						if (intForm instanceof List<?>) {
							if (((List<?>) intForm).size() > 0) {
								addIntermediateFormResult(intForm);
								if (!(((List<?>)intForm).size() == 1 && ((List<?>)intForm).get(0).toString().equals(rawResult.toString()))) {
									addInfo(intForm.toString(), element);
								}
							}
						}
					}
					if (rawResult instanceof VariableNode) {
						addInfo(((VariableNode) rawResult).toDescriptiveString(), element);
					} else {
						addInfo(rawResult.toString(), element);
					}
				}
				if (rawResult instanceof BuiltinElement) {
					String evalResult = evaluateExpression((BuiltinElement)rawResult, ((ExpressionStatement) element).getExpr());
					if (evalResult !=  null) {
						addInfo(evalResult, element);
					}
				}
			} else {
				throw new JenaProcessorException("onValidate for element of type '"
						+ element.getClass().getCanonicalName() + "' not implemented");
			}
//		} catch (JenaProcessorException e) {
//			addError(e.getMessage(), element);
//		} catch (InvalidNameException e) {
//			e.printStackTrace();
//		} catch (InvalidTypeException e) {
//			e.printStackTrace();
//		} catch (TranslationException e) {
//			e.printStackTrace();
//		} catch (CircularDefinitionException e) {
//			e.printStackTrace();
		} catch (Throwable t) {
			if (logger.isDebugEnabled()) {
				t.printStackTrace();
			}
			addError(t.getMessage(), element);
		}
	}

	/**
	 * Method to evaluate, if possible, a BuiltinElement
	 * @param bi
	 * @param expression 
	 * @return
	 */
	private String evaluateExpression(BuiltinElement bi, Expression expression) {
		if (bi != null && bi.getInModelReferencedEquation() != null) {
			try {
				IReasoner reasoner = getConfigMgr().getReasoner();
				Node evalNode = reasoner.evaluateSadlEquation(bi);
				if (evalNode != null) {
					addInfo("Evaluates to: " + evalNode.toString(), expression);
				}
				List<com.ge.research.sadl.reasoner.ModelError> errLst = reasoner.getErrors();
				if (errLst != null) {
					for (com.ge.research.sadl.reasoner.ModelError err : errLst) {
						if (err.getErrorType().equals(ErrorType.ERROR)) {
							addError(err.getErrorMsg(), expression);
						}
						else if (err.getErrorType().equals(ErrorType.WARNING)) {
							addWarning(err.getErrorMsg(), expression);
						}
						else {
							addInfo(err.getErrorMsg(), expression);
						}
					}
				}
			} catch (ConfigurationException e) {
				addError(e.getMessage(), expression);
			}			
		}
		return null;
	}
	
	private void setDefaultEObject(EObject element) {
		this.setDefaultEObject = element;
	}
	
	private EObject getDefaultEObject() {
		return setDefaultEObject;
	}

	private PathToFileUriConverter getUriConverter(Resource resource) {
		return ((XtextResource) resource).getResourceServiceProvider().get(PathToFileUriConverter.class);
	}

	protected void validateResourcePathAndName(Resource resource, SadlModel model, String modelActualUrl) {
		if (!isReservedFolder(resource, model)) {
			if (isReservedName(resource)) {
				if (!isSyntheticUri(null, resource)) {
					if (modelActualUrl.equals(ResourceManager.ServicesConf_SFN)) {
						addWarning(SadlErrorMessages.RESERVED_NAME_SERVICESCONFIG.get(modelActualUrl), model);
					}
					else {
						addError(SadlErrorMessages.RESERVED_NAME.get(modelActualUrl), model);
					}
				}
			}
		}
	}

	protected void addImplicitBuiltinFunctionModelImportToJenaModel(Resource resource, ProcessorContext context)
			throws ConfigurationException, IOException, URISyntaxException, JenaProcessorException {
		if (isSyntheticUri(null, resource)) {
			// test case: get SadlImplicitModel OWL model from the OntModelProvider
			URI simTestUri = URI.createURI(IReasoner.SADL_BUILTIN_FUNCTIONS_SYNTHETIC_URI);
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
									IReasoner.SADL_BUILTIN_FUNCTIONS_URI,
									IReasoner.SADL_BUILTIN_FUNCTIONS_ALIAS);
						} else {
							throw new JenaProcessorException("When does this happen? Not sure it ever will...");
						}
					}
				}
			}
		}
		if (sadlBuiltinFunctionModel != null) {
			addImportToJenaModel(getModelName(), IReasoner.SADL_BUILTIN_FUNCTIONS_URI,
					IReasoner.SADL_BUILTIN_FUNCTIONS_ALIAS, sadlBuiltinFunctionModel);
		}
	}

	private void addSadlServicesConfigConceptsModelImportToJenaModel(Resource resource, ProcessorContext context) {
		if (isSyntheticUri(null, resource)) {
			return;	// no tests currently so don't need to create
		}
		try {
			String content = getSadlServicesConfigConceptsModel();
			String desiredPath = getModelFolderPath(resource) + File.separator + ResourceManager.ServicesConfigurationConcepts_FN;
			File file = new File(desiredPath);
			if (file.exists()) {
				return;
			}
			org.eclipse.xtext.util.Files.writeStringIntoFile(Files.createFile(file.toPath()).toString(), content);

			String platformPath = (new File(getModelFolderPath(resource)).getParentFile().getName() + "/OwlModels/" + ResourceManager.ServicesConfigurationConcepts_FN);
			try {
				Resource newRsrc = resource.getResourceSet()
						.createResource(URI.createPlatformResourceURI(platformPath, false)); // createFileURI(implicitSadlModelFN));
				newRsrc.load(resource.getResourceSet().getLoadOptions());
				refreshResource(newRsrc);
			} catch (Throwable t) {
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	protected void addImplicitSadlModelImportToJenaModel(Resource resource, ProcessorContext context)
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
							throw new JenaProcessorException("When does this happen? Not sure it ever will...");
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

	protected void addSadlBaseModelImportToJenaModel(Resource resource)
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

	protected void addAnnotationsToResource(OntResource resource, EList<SadlAnnotation> anns) {
		Iterator<SadlAnnotation> iter = anns.iterator();
		while (iter.hasNext()) {
			SadlAnnotation ann = iter.next();
			String anntype = ann.getType();
			EList<String> annContents = ann.getContents();
			Iterator<String> anniter = annContents.iterator();
			while (anniter.hasNext()) {  
				String annContent = anniter.next();
				annContent = cleanTextForUTF8(annContent);	
				if (anntype.equalsIgnoreCase(AnnType.ALIAS.toString())) {
					resource.addLabel(annContent, "en");
				} else if (anntype.equalsIgnoreCase(AnnType.NOTE.toString())) {
					resource.addComment(annContent, "en");
				} else if (anntype.equalsIgnoreCase(AnnType.SEE.toString())) {
					if (validURI(annContent)) {
						resource.addSeeAlso(getTheJenaModel().getResource(annContent));
					}
					else {
						addWarning("A 'see' annotation must refer to a valid URI.", ann);
					}
				}
			}
		}
	}

	/**
	 * Method to make sure that the URI has at least rudimentary parts.
	 * @param s
	 * @return
	 */
    public boolean validURI(String s) {
    	try {
    		java.net.URI uri = new java.net.URI(s);
    		String sch = uri.getScheme();
    		if (sch == null) {
    			return false;
    		}
    		if (!sch.equals("file")) {
    			String hst = uri.getHost();
        		if (hst == null) {
        			return false;
        		}
    		}
    	} catch (Exception e) {
    		return false;
    	}
    	return true;
    }
    
    protected String getUriScheme(String s) throws URISyntaxException {
		java.net.URI uri = new java.net.URI(s);
		return uri.getScheme();
    }

	private String cleanTextForUTF8(String aContent) {
		aContent = aContent.replaceAll("[^\\x00-\\x7F]", "");
		aContent = aContent.replaceAll("[\\p{Cntrl}&&[^\r\n\t]]", "");
		return aContent;
	}

	public OntModel prepareEmptyOntModel(Resource resource, ProcessorContext context) throws ConfigurationException {
		try {
			IConfigurationManagerForIDE cm = getConfigMgr(resource, getOwlModelFormat(getProcessorContext()));
			OntDocumentManager owlDocMgr = cm.getJenaDocumentMgr();
			OntModelSpec spec = new OntModelSpec(OntModelSpec.OWL_MEM);
			setSpec(spec);
			String modelFolderPathname = getModelFolderPath(resource);
			if (modelFolderPathname != null && !modelFolderPathname.startsWith(SYNTHETIC_FROM_TEST)) {
				File mff = new File(modelFolderPathname);
				mff.mkdirs();
				spec.setImportModelGetter(getConfigMgr().getSadlModelGetterPutter(getOwlModelFormat(context)));
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

	/**
	 * Method to add a model to the current model processor Jena Model
	 * @param modelName -- URI of the current model processor model 
	 * @param importUri -- URI of the model being imported (added)
	 * @param importPrefix -- alias for the URI of the model being imported
	 * @param importedOntModel -- the OntModel being imported 
	 */
	private void addImportToJenaModel(String modelName, String importUri, String importPrefix, Model importedOntModel) {
		OntModel theModel = getTheJenaModel();
		addImportToJenaModel(theModel, modelName, importUri, importPrefix, importedOntModel);
	}
	
	/**
	 * Method to add a Jena model to another Jena model
	 * @param modelToAddTo -- the OntModel to which an import is being added
	 * @param modelNameOfModelToAddTo -- URI of the model to which an import is being added
	 * @param importUri -- URI of the model being imported (added)
	 * @param importPrefix -- alias for the URI of the model being imported
	 * @param importedOntModel -- the OntModel being imported
	 */
	private void addImportToJenaModel(OntModel modelToAddTo, String modelNameOfModelToAddTo, String importUri,
				String importPrefix, Model importedOntModel) {
		modelToAddTo.getDocumentManager().addModel(importUri, importedOntModel, true);
		Ontology modelOntology = modelToAddTo.getOntology(modelName);
		if (modelOntology == null) {
			modelOntology = modelToAddTo.createOntology(modelName);
		}
		if (importPrefix == null) {
			try {
				importPrefix = getConfigMgr(getCurrentResource(), getOwlModelFormat(getProcessorContext()))
						.getGlobalPrefix(importUri);
			} catch (ConfigurationException e) {
				e.printStackTrace();
			}
		}
		if (importPrefix != null && importPrefix.length() > 0) {
			modelToAddTo.setNsPrefix(importPrefix, assureNamespaceEndsWithHash(importUri));
		}
		org.apache.jena.rdf.model.Resource importedOntology = modelToAddTo.createResource(importUri);
		modelOntology.addImport(importedOntology);
		modelToAddTo.addSubModel(importedOntModel);
		modelToAddTo.addLoadedImport(importUri);
		if (getModelName().equals(modelNameOfModelToAddTo)) {
			// only add if direct import
			addOrderedImport(importUri);
		}
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
							getIfTranslator().setStartingVariableNumber(getVariableNumber());	// make sure IF doesn't duplicate var names
							Object lhsObj = getIfTranslator().expandProxyNodes(args.get(0), false, true);
							Object rhsObj = getIfTranslator().expandProxyNodes(args.get(1), false, true);
							setVariableNumber(getIfTranslator().getVariableNumber());  // make sure this processor doesn't duplicate var names
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
						getIfTranslator().setStartingVariableNumber(getVariableNumber());	// make sure IF doesn't duplicate var names
						Object trans = getIfTranslator().expandProxyNodes(testtrans, false, true);
						setVariableNumber(getIfTranslator().getVariableNumber());  // make sure this processor doesn't duplicate var names
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
				else if (testtrans instanceof Query) {
					GraphPatternElement lp = ((Query)testtrans).getLastPattern();
					GraphPatternElement realLp = null;
					if (lp instanceof Junction) {
						JunctionList plst = IntermediateFormTranslator.junctionToList((Junction)lp);
						if (plst.size() > 0) {
							realLp = plst.get(plst.size() - 1);
							if (realLp instanceof BuiltinElement && 
									isComparisonBuiltin(((BuiltinElement)realLp).getFuncName())) {
								if (((BuiltinElement)realLp).getArguments().get(0) instanceof VariableNode) {
									GraphPatternElement prior = plst.get(plst.size() - 2);
									if (prior instanceof TripleElement && 
											((TripleElement)prior).getObject().equals(((BuiltinElement)realLp).getArguments().get(0))) {
										plst.remove(plst.size() - 1);
										plst.remove(plst.size() - 1);
										plst.add(prior);
										test.setRhs(((BuiltinElement)realLp).getArguments().get(1));
										((Query)testtrans).setPatterns(plst);
									}
								}
								else if (((BuiltinElement)realLp).getArguments().get(0) instanceof ProxyNode && 
										((ProxyNode)((BuiltinElement)realLp).getArguments().get(0)).getProxyFor() instanceof TripleElement) {
									plst.remove(plst.size() - 1);
									plst.add(((ProxyNode) ((BuiltinElement)realLp).getArguments().get(0)).getProxyFor());
									test.setRhs(((BuiltinElement)realLp).getArguments().get(1));
									((Query)testtrans).setPatterns(plst);
								}
								test.setCompName(((BuiltinElement)realLp).getFuncType());
							}
						}
					}
					if (!done) {
						test.setLhs(testtrans);
						done = true;
					}
					generatedTests = new Test[1];
					generatedTests[0] = test;
				}

				if (!done) {
					// expand ProxyNodes and see what we can do with the expanded form
					List<Object> expanded = new ArrayList<Object>();
					getIfTranslator().setStartingVariableNumber(getVariableNumber());	// make sure IF doesn't duplicate var names
					Object testExpanded = getIfTranslator().expandProxyNodes(testtrans, false, true);
					setVariableNumber(getIfTranslator().getVariableNumber());  // make sure this processor doesn't duplicate var names
					boolean treatAsMultipleTests = false;
					{
						if (testExpanded instanceof List<?> && ((List<?>)testExpanded).size() == 1) {
							testExpanded = ((List<?>)testExpanded).get(0);
						}
						if (testExpanded instanceof Junction) {
							testExpanded = getIfTranslator().junctionToList((Junction) testExpanded);
						}
						if (testExpanded instanceof List<?>) {
							treatAsMultipleTests = containsMultipleTests((List<GraphPatternElement>) testExpanded);
						}
					}
					if (treatAsMultipleTests && testExpanded instanceof List<?>) {
						for (int i = 0; i < ((List<?>) testExpanded).size(); i++) {
							expanded.add(((List<?>) testExpanded).get(i));
						}
					} else if (!((testExpanded instanceof List<?>) && ((List<?>)testExpanded).size() == 0)) {
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
		if (isInAllowedVariableContainer(defnContainer)) {
			return var;
		}
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
				EObject augtype = ((SadlParameterDeclaration) defnContainer).getAugtype();
				// TODO
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
				} else if (((SubjHasProp) defnContainer).getProp().equals(decl) && 
						((SubjHasProp) defnContainer).getLeft() instanceof Declaration) {
					// this is an in-line variable declaration
					Declaration expr = (Declaration) ((SubjHasProp) defnContainer).getLeft();
					SadlTypeReference type = expr.getType();
					Object typenode = processExpression(type);
					if (typenode instanceof Node) {
						setVarType(var, (Node) typenode, false, expr);
					}
					else {
						addError("Unexpected type", expr);
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
				if (EcoreUtil2.getContainerOfType(sr, QueryStatement.class) == null
						&& EcoreUtil2.getContainerOfType(sr, SelectExpression.class)== null 
						&& EcoreUtil2.getContainerOfType(sr, RuleStatement.class) == null) {
					addWarning("Variable '" + var.getName() + "' is not defined", sr);
				}
			} else if (EcoreUtil2.getContainerOfType(sr, EquationStatement.class) != null) {
				Equation cureq = getCurrentEquation();
				if (cureq != null) {
					cureq.addVariable(var);
					tci = getModelValidator().getType(getDeclarationExtensions().getDeclaration(sr));
				}
			} else if (EcoreUtil2.getContainerOfType(sr, QueryStatement.class) == null
					&& EcoreUtil2.getContainerOfType(sr, SelectExpression.class)== null 
					&& EcoreUtil2.getContainerOfType(sr, RuleStatement.class) == null
					&& EcoreUtil2.getContainerOfType(sr, ExpressionStatement.class)== null ) {
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
//			e.printStackTrace();
		} catch (CircularDefinitionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (CircularDependencyException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (PropertyWithoutRangeException e) {
			addPropertyWithoutRangeError(defnContainer, null, e);
		}
		return var;
	}

	private boolean isInAllowedVariableContainer(EObject defnContainer) {
		if (getAllowedVariableContainers() != null) {
			for (Class allowed : getAllowedVariableContainers()) {
				String nm1 = allowed.getCanonicalName();
				String nm2 = defnContainer.getClass().getCanonicalName();
				if (nm1.equals(nm2)) {
					return true;
				}
				for (Class incls : defnContainer.getClass().getInterfaces()) {
					if (incls.equals(allowed)) {
						return true;
					}
				}
			}
			if (defnContainer.eContainer() != null){
				return isInAllowedVariableContainer(defnContainer.eContainer());
			}
		}
		return false;
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
			if (((Rule) trgt).getVariable(variableNameToUri(name)) != null) {
				return ((Rule) trgt).getVariable(variableNameToUri(name));
			}
		} else if (trgt instanceof Query) {
			if (((Query) trgt).getVariable(variableNameToUri(name)) != null) {
				return ((Query) trgt).getVariable(variableNameToUri(name));
			}
		} else if (trgt instanceof Test) {
			// TODO
		}
		else if (getCurrentEquation() != null) {
			if (getCurrentEquation().getVariable(variableNameToUri(name)) != null) {
				return getCurrentEquation().getVariable(variableNameToUri(name));
			}
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
		if (testtrans.size() == 1 || testtrans.size() == 0) {
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
		Object rhs = test.getRhs();
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
		else if (gpe instanceof Junction) {
			try {
				JunctionList plst = IntermediateFormTranslator.junctionToList((Junction)gpe);
				for (GraphPatternElement el : plst) {
					numErrors += validateGraphPatternElement(object, el);
				}
			} catch (TranslationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
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
			} else if (result instanceof Object[] && ((Object[])result).length > 1 &&
					((Object[])result)[1] instanceof GraphPatternElement) {
				Explain cmd = new Explain((GraphPatternElement) ((Object[])result)[1]);
				addSadlCommand(cmd);
			} else if (result != null) {
//				throw new TranslationException("Unhandled ExplainStatement: " + result.toString());
				addError("Unhandled ExplainStatement: " + result.toString(), element);
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

	private Query processStatement(UpdateStatement element) throws JenaProcessorException, TranslationException, InvalidNameException, InvalidTypeException, CircularDefinitionException {
		Expression qexpr = element.getExpr();
		if (qexpr != null) {
			Update theQuery = new Update();
			theQuery.setContext(qexpr);
			theQuery.setUpdate(true);
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
			if (qobj instanceof Update) {
				query = (Update) qobj;
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
				query = processQuery(postProcessTranslationResult(qobj), qexpr);
			}
			if (query != null) {
				if (element.getName() != null) {
					String uri = declarationExtensions.getConceptUri(element.getName());
					query.setFqName(uri);
					OntClass nqcls = getTheJenaModel()
							.getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_NAMEDQUERY_CLASS_URI);
					if (nqcls != null) {
						Individual nqry = createIndividual(element.getName(), nqcls);
						// Add annotations, if any
						EList<NamedStructureAnnotation> annotations = element.getAnnotations();
						if (annotations != null && annotations.size() > 0) {
							addNamedStructureAnnotations(nqry, annotations);
						}
					}
				}
				final ICompositeNode node = NodeModelUtils.findActualNodeFor(element);
				if (node != null) {
					query.setOffset(node.getOffset() - 1);
					query.setLength(node.getLength());
				}
				query = addExpandedPropertiesToQuery(query, qexpr);
				addSadlCommand(query);
			}
			return query;
		}
		return null;
	}

	public Query processStatement(QueryStatement element) throws JenaProcessorException, InvalidNameException,
			InvalidTypeException, TranslationException, CircularDefinitionException {
		clearCruleVariables();
		Expression qexpr = element.getExpr();
		if (qexpr == null) {
			qexpr = element.getSrname();
		}
		SadlResource elementName = element.getName();
		EList<NamedStructureAnnotation> annotations = element.getAnnotations();
		boolean isGraph = element.getStart().equals("Graph");
		Query query = processQueryExpression(element, qexpr, elementName, annotations, isGraph);
		if (element.getParameterizedValues() != null) {
			EList<Expression> pvs = element.getParameterizedValues().getExplicitValues();
			if (pvs != null) {
				List<Object> pvobjs = new ArrayList<Object>();
				for (Expression pv : pvs) {
					Object pvobj = processExpression(pv);
					if (pvobj != null) {
						pvobjs.add(pvobj);
					}
				}
				query.setParameterizedValues(pvobjs);
			}
		}
		return query;
	}

	protected Query processQueryExpression(SadlModelElement element, Expression qexpr,
			SadlResource elementName, EList<NamedStructureAnnotation> annotations, boolean isGraph) throws CircularDefinitionException,
			InvalidNameException, InvalidTypeException, TranslationException, JenaProcessorException {
		Query query = null;
		if (qexpr != null && !(qexpr instanceof ValueTable)) {
			boolean returnOnError = false;
			Query theQuery = new Query();
			theQuery.setContext(qexpr);
			setTarget(theQuery);
			if (qexpr instanceof SadlResource) {
				OntConceptType qntype = getDeclarationExtensions().getOntConceptType(((SadlResource) qexpr).getName());
				if (qntype.equals(OntConceptType.STRUCTURE_NAME)) {
					// this is just a named query declared elsewhere
					SadlResource qdecl = getDeclarationExtensions().getDeclaration(((SadlResource) qexpr).getName());
					EObject qdeclcont = qdecl.eContainer();
					if (qdeclcont instanceof QueryStatement) {
						qexpr = ((QueryStatement) qdeclcont).getExpr();
					} else {
						addError("Unexpected named structure name whose definition is not a query statement", qexpr);
						returnOnError = true;
					}
				}
			}
			if (!returnOnError) {
				Object qobj = processAsk(qexpr);
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
					query = processQuery(postProcessTranslationResult(qobj), qexpr);
				}
				if (query != null) {
					if (elementName != null) {
						String uri = declarationExtensions.getConceptUri(elementName);
						query.setFqName(uri);
						OntClass nqcls = getTheJenaModel()
								.getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_NAMEDQUERY_CLASS_URI);
						if (nqcls != null) {
							Individual nqry = createIndividual(elementName, nqcls);
							// Add NamedStructureAnnotations, if any
							if (annotations != null && annotations.size() > 0) {
								addNamedStructureAnnotations(nqry, annotations);
							}
						}
					}
					if (query.getKeyword() == null) {
						if (query.getVariables() == null) {
							query.setKeyword("ask");
						}
						else {
							query.setKeyword("select");
						}
					}
					if (isGraph) {
						query.setGraph(true);
					}
					final ICompositeNode node = NodeModelUtils.findActualNodeFor(element);
					if (node != null) {
						query.setOffset(node.getOffset() - 1);
						query.setLength(node.getLength());
					}
					query = addExpandedPropertiesToQuery(query, qexpr);
					addSadlCommand(query);
				}
			}
		} else {
			// this is a reference to a named query defined elsewhere
			SadlResource sr = elementName;
			SadlResource sr2 = declarationExtensions.getDeclaration(sr);
			if (sr2 != null) {
				EObject cont = sr2.eContainer();
				if (cont instanceof QueryStatement && ((QueryStatement) cont).getExpr() != null) {
					query = processStatement((QueryStatement) cont);
				}
				if (qexpr instanceof ValueTable) {
					Object vtobj = processExpression(qexpr);
					query.setParameterizedValues((List<Object>) vtobj);
				}
			}
		}
		return query;
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
						obj = new VariableNode(getNewVar(expr));
						((TripleElement) e).setObject(obj);
						implicitObject = true;
					}
					if (implicitObject || obj instanceof VariableNode) {
						VariableNode vn = (VariableNode) ((TripleElement) e).getObject();
						// if (vars != null && vars.contains(vn.getName())) {
						Node pred = ((TripleElement) e).getPredicate();
						if (pred != null) {
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
											else {
												NamedNode nn = new NamedNode(rngcls.getURI());
												nn.setNodeType(NodeType.ClassNode);
												vn.setType(nn);
											}
										}
									}
								}
							} catch (DontTypeCheckException e1) {
								e1.printStackTrace();
							} catch (InvalidTypeException e1) {
								e1.printStackTrace();
							}
							// }
							catch (TranslationException e2) {
								e2.printStackTrace();
							} catch (InvalidNameException e1) {
								e1.printStackTrace();
							}
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
		else if (expr == null) {
			addError("Query has no body", expr);
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
		} else if (predProp.canAs(ObjectProperty.class)) {
			predcn.setType(ConceptType.OBJECTPROPERTY);
		} else if (predProp.canAs(DatatypeProperty.class)) {
			predcn.setType(ConceptType.DATATYPEPROPERTY);
		}
		else if (predProp.canAs(AnnotationProperty.class)) {
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

	public Object processExpression(SelectExpression expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		return processAsk(expr);
	}

	public Object processExpression(ConstructExpression expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		return processAsk(expr);
	}

	public Object processExpression(AskExpression expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		return processAsk(expr);
	}
	
	public Update processExpression(UpdateExpression expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		return processUpdate(expr);
	}

	private Update processUpdate(UpdateExpression expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		Update query = (Update) getTarget();
		query.setUpdate(true);
		boolean ddata = expr.getDData() != null;
		boolean idata = expr.getIData() != null;
		Expression dExpr = expr.getDeleteExpression();
		Expression iExpr = expr.getInsertExpression();
		Expression wExpr = expr.getWhereExpression();
		Object dObj = null;
		Object iObj = null;
		Object wObj = null;
		if (dExpr != null) {
			dObj = processExpression(dExpr);
			query.setKeyword("delete");
			if (ddata) {
				query.setDeleteData(true);
			}
			if (dObj instanceof Object[]) {
				if (((Object[])dObj)[1] instanceof GraphPatternElement) {
					query.addDeletePattern((GraphPatternElement) ((Object[])dObj)[1]);
				}
				else if (((Object[])dObj)[1] instanceof List<?>) {
					query.getDeletePatterns().addAll((Collection<? extends GraphPatternElement>) ((Object[])dObj)[1]);
				}
			}
			else if (dObj instanceof Junction) {
				query.addDeletePattern((Junction)dObj);
			}
			else if (dObj != null){
				throw new TranslationException("Unhandled delete expression: " + dObj.getClass().getCanonicalName());
			}
		}
		if (iExpr != null) {
			iObj = processExpression(iExpr);
			query.setSecondKeyword("insert");
			if (idata) {
				query.setInsertData(true);
			}
			if (iObj instanceof Object[]) {
				if (((Object[])iObj)[1] instanceof GraphPatternElement) {
					query.addInsertPattern((GraphPatternElement) ((Object[])iObj)[1]);
				}
				else if (((Object[])iObj)[1] instanceof List<?>) {
					query.getInsertPatterns().addAll((Collection<? extends GraphPatternElement>) ((Object[])iObj)[1]);
				}
			}
			else if (iObj instanceof Junction) {
				query.addInsertPattern((Junction)iObj);
			}
			else if (iObj != null) {
				throw new TranslationException("Unhandled insert expression: " + iObj.getClass().getCanonicalName());
			}
		}
		if (wExpr != null) {
			wObj = postProcessTranslationResult(processExpression(wExpr));
			if (wObj instanceof Object[]) {
				query.addVariable((VariableNode) ((Object[])wObj)[0]);
				query.addPattern((GraphPatternElement) ((Object[])wObj)[1]);
			}
			else if (wObj instanceof Junction) {
				query.addPattern((Junction)wObj);
			}
			else if (wObj != null) {
				throw new TranslationException("Unhandled where expression: " + wObj.getClass().getCanonicalName());
			}
		}
		
		return query;
	}

	protected Object processAsk(EObject thenExpr) {
		Object trgt = getTarget();
		Query query = null;
		if (trgt instanceof Query) {
			query = (Query) getTarget();
		}
		else if (trgt instanceof Test) {
			query = new Query();
			setTarget(query);
			addInfo("Creating query as part of test", thenExpr);
		}
		else {
			addError("Unhandled construct with target of type " + thenExpr.getClass().getCanonicalName(), thenExpr);
		}
		// if (parent != null) {
		// getIfTranslator().setEncapsulatingTarget(parent);
		// }

		// get variables and other information from the SelectExpression
		EList<SadlResource> varList = null; // the specified list of variables to be returned
		List<VariableNode> names = null; // the actual names generated from the varList
		EObject whexpr = null;
		Object qobj;
		if (thenExpr instanceof SelectExpression) {
			whexpr = ((SelectExpression) thenExpr).getWhereExpression();
			query.setKeyword("select");
			if (((SelectExpression) thenExpr).isDistinct()) {
				query.setDistinct(true);
			}
			varList = ((SelectExpression) thenExpr).getSelectFrom();
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
						tci = getModelValidator().getType(varList.get(i));
						if (tci != null && tci.getImplicitProperties() != null) {
							if (var instanceof VariableNode) {
								if (tci.getImplicitProperties().size() == 1) {
									ConceptName ipn = tci.getImplicitProperties().get(0);
									((VariableNode)var).setImpliedPropertyNode(conceptNameToNamedNode(ipn));
								}
								else {
									addWarning("Multiple implied properties on variable not supported", varList.get(i));
								}
							}
						}
					} catch (DontTypeCheckException e1) {
						// OK to not type check
					} catch (CircularDefinitionException e1) {
						e1.printStackTrace();
					} catch (URISyntaxException e1) {
						e1.printStackTrace();
					} catch (IOException e1) {
						e1.printStackTrace();
					} catch (ConfigurationException e1) {
						e1.printStackTrace();
					} catch (CircularDependencyException e) {
						e.printStackTrace();
					} catch (PropertyWithoutRangeException e) {
						e.printStackTrace();
					} catch (InvalidNameException e) {
						e.printStackTrace();
					} catch (TranslationException e) {
						e.printStackTrace();
					} catch (InvalidTypeException e) {
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
							addError(SadlErrorMessages.QUERY_ISNT_VARIABLE.get(var.toString()), thenExpr);
						}
					} else {
						names.add(((VariableNode) var));
					}
				}
			}
			if (((SelectExpression) thenExpr).getOrderby() != null) {
				EList<OrderElement> ol = ((SelectExpression) thenExpr).getOrderList();
				List<OrderingPair> orderingPairs = new ArrayList<OrderingPair>();
				for (int i = 0; i < ol.size(); i++) {
					OrderElement oele = ol.get(i);
					SadlResource ord = oele.getOrderBy();
					orderingPairs.add(query.new OrderingPair(getDeclarationExtensions().getConcreteName(ord),
							(oele.isDesc() ? Order.DESC : Order.ASC)));
				}
				query.setOrderBy(orderingPairs);
			}
		} else if (thenExpr instanceof ConstructExpression) {
			whexpr = ((ConstructExpression) thenExpr).getWhereExpression();
			query.setKeyword("construct");
			names = new ArrayList<VariableNode>();
			try {
				Object result = processExpression(((ConstructExpression) thenExpr).getSubj());
				if (result instanceof VariableNode) {
					names.add(((VariableNode) result));
				} else {
					names.add(createVariable(result.toString(), thenExpr));
				}
				result = processExpression(((ConstructExpression) thenExpr).getPred());
				if (result instanceof VariableNode) {
					names.add((VariableNode) result);
				} else {
					names.add(createVariable(result.toString(), thenExpr));
				}
				result = processExpression(((ConstructExpression) thenExpr).getObj());
				if (result instanceof VariableNode) {
					names.add(((VariableNode) result));
				} else {
					names.add(createVariable(result.toString(), thenExpr));
				}
				if (names.size() != 3) {
					addWarning("A 'construct' statement should have 3 variables so as to be able to generate a graph.",
							thenExpr);
				}
			} catch (InvalidNameException e) {
				e.printStackTrace();
			} catch (InvalidTypeException e) {
				e.printStackTrace();
			} catch (TranslationException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			} catch (PrefixNotFoundException e) {
				e.printStackTrace();
			} catch (ConfigurationException e) {
				e.printStackTrace();
			}
		} else if (thenExpr instanceof AskExpression) {
			whexpr = ((AskExpression) thenExpr).getWhereExpression();
			query.setKeyword("ask");
		} 				
		else if (thenExpr instanceof StringLiteral || thenExpr instanceof SadlResource) {
			try {
				return processExpression(thenExpr);
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
		else {
			// this is just some expression...
			whexpr = thenExpr;
		}
		

		// Translate the query to the resulting intermediate form.
		if (modelValidator != null) {
			try {
				TypeCheckInfo tct = getModelValidator().getType(whexpr);
				if (tct != null && tct.getImplicitProperties() != null) {
					List<ConceptName> ips = tct.getImplicitProperties();
					int i = 0;
				}
			} catch (URISyntaxException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			} catch (ConfigurationException e) {
				e.printStackTrace();
			} catch (DontTypeCheckException e) {
				// OK to not be able to type check
			} catch (CircularDefinitionException e) {
				e.printStackTrace();
			} catch (CircularDependencyException e) {
				e.printStackTrace();
			} catch (PropertyWithoutRangeException e) {
				addTypeCheckingError(e.getMessage(), whexpr);
			} catch (InvalidNameException e) {
				e.printStackTrace();
			} catch (TranslationException e) {
				e.printStackTrace();
			} catch (InvalidTypeException e) {
				e.printStackTrace();
			}
		}
		Object pattern = null;
		try {
			pattern = postProcessTranslationResult(processExpression(whexpr));
		} catch (InvalidNameException e1) {
			e1.printStackTrace();
		} catch (InvalidTypeException e1) {
			e1.printStackTrace();
		} catch (TranslationException e1) {
			addError(e1.getMessage(), whexpr);
		}

		boolean explicitNames = false;
		if (names != null && names.size() > 0) {
			query.setVariables(names); // this will replace implicitly added variables if there are explicit ones
			explicitNames = true;
		}

		Object expandedPattern = null;
		try {
			getIfTranslator().setStartingVariableNumber(getVariableNumber());	// make sure IF doesn't duplicate var names
			expandedPattern = getIfTranslator().expandProxyNodes(pattern, false, true);
			setVariableNumber(getIfTranslator().getVariableNumber());  // make sure this processor doesn't duplicate var names
		} catch (InvalidNameException e) {
			addError(SadlErrorMessages.INVALID_NAME.get("query", pattern.toString()), thenExpr);
			e.printStackTrace();
		} catch (InvalidTypeException e) {
			addError(SadlErrorMessages.INVALID_TYPE.get("query", pattern.toString()), thenExpr);
			e.printStackTrace();
		} catch (TranslationException e) {
			addError(SadlErrorMessages.TRANSLATION_ERROR.get("query", pattern.toString()), thenExpr);
			e.printStackTrace();
		}
		if (expandedPattern != null && expandedPattern instanceof List<?> && ((List<?>) expandedPattern).size() > 0) {
			pattern = expandedPattern;
		}

		if (pattern instanceof List<?>) {
			if (query.getVariables() == null || !explicitNames) {
				Set<VariableNode> nodes = getIfTranslator().getSelectVariables((List<GraphPatternElement>) pattern);
				if (nodes != null && nodes.size() > 0) {
					names = new ArrayList<VariableNode>(1);
					for (VariableNode node : nodes) {
						if (!names.contains(node)) {
							names.add(node);
						}
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
			pattern = getIfTranslator().removeDuplicates((List<?>) pattern);
			query.setPatterns((List<GraphPatternElement>) pattern);
		} else if (pattern instanceof GraphPatternElement) {
			query.addPattern((GraphPatternElement)pattern);
		} else if (pattern instanceof Literal) {
			// this must be a SPARQL query
			query.setSparqlQueryString(((Literal) pattern).getValue().toString());
		}
		logger.debug("Ask translation: {}", query);
		if (trgt != null && !(trgt.equals(query))) {
			setTarget(trgt);
		}
		return query;
	}

	private Query processQuery(Object qobj, EObject context) throws JenaProcessorException {
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
				VariableNode sn = new VariableNode(getNewVar(context));
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
		if (q.getSparqlQueryString() != null) {
			ITranslator translator;
			try {
				translator = getTranslator();
				if (translator != null) {
					try {
						String preparedQuery = translator.prepareQuery(getTheJenaModel(), q.getSparqlQueryString());
						q.setPreparedQueryString(preparedQuery);
					} catch (Exception e) {
						addError(e.getMessage(), context);
					}
				}
			} catch (ConfigurationException e1) {
				e1.printStackTrace();
			}

		}
		setTarget(null);
		return q;
	}

	public void processStatement(EquationStatement element)
			throws JenaProcessorException, InvalidNameException, InvalidTypeException, TranslationException {
		setHostEObject(element);
		clearCruleVariables();
		SadlResource nm = element.getName();
		EList<SadlParameterDeclaration> params = element.getParameter();
		EList<SadlReturnDeclaration> rtype = element.getReturnType();
		Expression bdy = element.getBody();
		Expression retVal = element.getRetval();
		Expression whrExpr = element.getWhere();
		if (bdy == null && retVal == null && whrExpr == null) {
			String equri = getDeclarationExtensions().getConceptUri(nm);
			addError("Equation '" + equri + "' has empty body", element);
			return;
		}
		Individual eqinst = getTheJenaModel().createIndividual(getDeclarationExtensions().getConceptUri(nm),
				getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_EQUATION_CLASS_URI));
		// Add annotations, if any
		EList<NamedStructureAnnotation> annotations = element.getAnnotations();
		if (annotations != null && annotations.size() > 0) {
			addNamedStructureAnnotations(eqinst, annotations);
		}
		Equation eq = createEquation(element, eqinst, nm, rtype, params, bdy, retVal, whrExpr);
		addEquation(element.eResource(), eq, nm);
		setHostEObject(null);
	}

	/**
	 * Method to add the properties of an Equation to the OWL (Jena) model.
	 * @param nm  -- the SadlResource (grammar element) identifying the equation
	 * @param eq -- The instance of the Java class Equation which is being built
	 * @param eqinst -- The OWL model Individual representing the equation
	 * @param paramInstances -- the DataDescriptor instances created for the arguments
	 * @param retInstances -- the DataDescriptor instances created for the return values
	 * @throws JenaProcessorException 
	 * @throws TranslationException 
	 */
	private void addEquationPropertiesToJenaModel(SadlResource nm, Equation eq, Individual eqinst, List<Individual> paramInstances, List<Individual> retInstances) throws JenaProcessorException, TranslationException {
		// add the expression, if any
		String expr = eq.toString();
		if (expr != null && expr.length() > 0) {
			Literal literal = getTheJenaModel().createTypedLiteral(expr);
			ObjectProperty exprProp = getTheJenaModel().getObjectProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXPRESSTION_PROPERTY_URI);
			if (exprProp != null) {
				// these can be null during clean/build with resource open in editor
				Individual scriptInst = getTheJenaModel().createIndividual(getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_SCRIPT_CLASS_URI));
				DatatypeProperty scriptProp = getTheJenaModel().getDatatypeProperty(SadlConstants.SADL_IMPLICIT_MODEL_SCRIPT_PROPERTY_URI);
				scriptInst.addProperty(scriptProp, literal);
				Property langProp = getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_LANGUAGE_PROPERTY_URI);
				scriptInst.addProperty(langProp, getTheJenaModel().getIndividual(SadlConstants.SADL_IMPLICIT_MODEL_TEXT_LANGUAGE_INST_URI));
				eqinst.addProperty(exprProp, scriptInst);
			}
		}
		
		// get the meta model resources needed to create the OWL triples
		ObjectProperty argsProp = getTheJenaModel().getObjectProperty(SadlConstants.SADL_IMPLICIT_MODEL_ARGUMENTS_PROPERTY_URI);
		ObjectProperty returnTypesProp = getTheJenaModel().getObjectProperty(SadlConstants.SADL_IMPLICIT_MODEL_RETURN_TYPES_PROPERTY_URI);
		if (argsProp == null || returnTypesProp == null) {
			addError("Model doesn't contain Equation metamodel. Do you need to update the SadlImplicitModel?", nm);
		}
		else {
			OntClass lsttyp = getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_DATA_DESCRIPTOR_CLASS_URI);
			if (paramInstances != null && paramInstances.size() > 0) {
				OntClass cls = getOrCreateListSubclass(null, lsttyp.getURI(), getCurrentResource(), null);
				Individual argInstList = addMembersToSadlList(getTheJenaModel(), null, cls, lsttyp, paramInstances.iterator());
//				RDFList argInstList = getTheJenaModel().createList(paramInstances.iterator());
				eqinst.addProperty(argsProp, argInstList);
			}
			if (retInstances != null && retInstances.size() > 0) {
				OntClass cls = getOrCreateListSubclass(null, lsttyp.getURI(), getCurrentResource(), null);
				Individual retTypeList = addMembersToSadlList(getTheJenaModel(), null, cls,  lsttyp, retInstances.iterator());
//				RDFList retTypeList = getTheJenaModel().createList(retInstances.iterator());
				eqinst.addProperty(returnTypesProp, retTypeList);
			}
		}
		// add annotations
		EList<SadlAnnotation> anns = nm.getAnnotations();
		if (anns != null) {
			addAnnotationsToResource(eqinst, anns);
		}
	}

	protected Equation createEquation(EquationStatement element, Individual eqinst, SadlResource nm, EList<SadlReturnDeclaration> rtype,
			EList<SadlParameterDeclaration> params, Expression bdy, Expression retVal, Expression whrExpr)
			throws JenaProcessorException, TranslationException, InvalidNameException, InvalidTypeException {
		Equation eq = new Equation(getDeclarationExtensions().getConcreteName(nm));
		eq.setNamespace(getDeclarationExtensions().getConceptNamespace(nm));
		List<DataDescriptor> paramDataDescriptors = null;
		if (params != null && params.size() > 0) {
			List<Node> args = new ArrayList<Node>();
			List<Node> argtypes = new ArrayList<Node>();
			paramDataDescriptors = new ArrayList<DataDescriptor>();
			for (int i = 0; i < params.size(); i++) {
				SadlParameterDeclaration param = params.get(i);
				SadlResource pr = param.getName();
				Object pn = processExpression(pr);
				if (pn != null) {
					args.add((Node) pn);
				}
				SadlTypeReference prtype = param.getType();
				Node prtnode = sadlTypeReferenceToNode(prtype);
				if (prtnode != null) {
					argtypes.add(prtnode);
				}
			}
			eq.setArguments(args);
			eq.setArgumentTypes(argtypes);
			// put equation in context for sub-processing
			setCurrentEquation(eq);
			for (int i = 0; i < params.size(); i++) {
				if (i < args.size()) {
					Node pn = args.get(i);
					Node prtnode = argtypes.get(i);
					SadlParameterDeclaration param = params.get(i);
					EObject augtype = param.getAugtype();
					Object augTypeObj = null;
					if (augtype != null) {
						augTypeObj = processExpression(augtype);
					}
					paramDataDescriptors.add(new DataDescriptor((Node) pn, prtnode, param.getUnits(), augTypeObj));
				}
			}
			// clear current equation
			setCurrentEquation(null);
		}
		List<Node> rtypes = new ArrayList<Node>();
		List<DataDescriptor> retDataDescriptors = new ArrayList<DataDescriptor>();
		for (SadlReturnDeclaration srd : rtype) {
			SadlTypeReference rt = srd.getType();
			EObject augtype = srd.getAugtype();
			Object augTypeObject = null;
			if (augtype != null) {
				augTypeObject = processExpression(augtype);
			}
			Node rtnode = null;
			if (rt != null) {
				rtnode = sadlTypeReferenceToNode(rt);
				rtypes.add(rtnode);
			}
			else {
				rtypes.add(null);
			}
			retDataDescriptors.add(new DataDescriptor(null, rtnode, srd.getUnits(), augTypeObject));
		}
		eq.setReturnTypes(rtypes);
		// put equation in context for sub-processing
		setCurrentEquation(eq);
		
		// do where's first so that variables defined there will exist when processing body and return
		if (whrExpr != null) {
			Object whrObj = processExpression(whrExpr);
			if (whrObj instanceof List<?>) {
				eq.setWheres((List<GraphPatternElement>) whrObj);
			}
			else if (whrObj instanceof GraphPatternElement) {
				eq.addWhereElement((GraphPatternElement) whrObj);
			}
			else {
				throw new TranslationException("Equation where translation not of expected type.");
			}
		}
		
		Object bdyobj = processExpression(bdy);
		if (bdyobj instanceof List<?>) {
			eq.setBody((List<GraphPatternElement>) bdyobj);
		} else if (bdyobj instanceof GraphPatternElement) {
			eq.addBodyElement((GraphPatternElement) bdyobj);
		}
		if (retVal != null) {
			Object retObj = processExpression(retVal);
			if (retObj instanceof List<?>) {
				for (Object obj : (List<?>)retObj) {
					if (obj instanceof Node) {
						eq.addReturnNode((Node)obj);
					}
					else {
						throw new TranslationException("Equation return not of expected type.");
					}
				}
			}
			else if (retObj instanceof Object[]) {
				for (Object elObj : (Object[])retObj) {
					if (elObj instanceof Node) {
						eq.addReturnNode((Node)elObj);
					}
				}
			}
			else if (retObj instanceof Node) {
				eq.addReturnNode((Node)retObj);
			}
			else if (retObj instanceof GraphPatternElement) {
				eq.addReturnNode(nodeCheck(retObj));
			}
			else {
				throw new TranslationException("Equation return not of expected type.");
			}
		}
		if (getModelValidator() != null) {
			// check return type against body expression
			StringBuilder errorMessageBuilder = new StringBuilder();
			if (retVal != null) {
				if (!getModelValidator().validateBinaryOperationByParts(element, rtype, retVal, "function return",
						errorMessageBuilder, false)) {
					addTypeCheckingError(errorMessageBuilder.toString(), bdy);
				}
			}
			else if (bdy != null) {
				if (!getModelValidator().validateBinaryOperationByParts(element, rtype, bdy, "function return",
						errorMessageBuilder, false)) {
					addTypeCheckingError(errorMessageBuilder.toString(), bdy);
				}
			}
		}
		// set return Nodes in reDataDescriptors
		if (eq.getReturnNodes() != null) {
			for (int i = 0; i < eq.getReturnNodes().size(); i++) {
				Node retNode = eq.getReturnNodes().get(i);
				if (retNode instanceof NamedNode) {
					DataDescriptor rdd = retDataDescriptors.get(i);
					rdd.setName(retNode);
				}
			}
		}
		equationToOwl(nm, eqinst, eq, retDataDescriptors, paramDataDescriptors);
		setCurrentEquation(null); // clear
		logger.debug("Equation: " + eq.toFullyQualifiedString());
		return eq;
	}

	private Individual equationToOwl(SadlResource nm, Individual eqinst, Equation eq, List<DataDescriptor> retDataDescriptors,
			List<DataDescriptor> paramDataDescriptors) throws TranslationException, JenaProcessorException {
		List<Individual> paramInstances = new ArrayList<Individual>();
		if (paramDataDescriptors != null) {
			for (DataDescriptor pdd : paramDataDescriptors) {
				Individual pddinst = dataDescriptorToOwl(nm, pdd, false);
				if (pddinst != null) {
					paramInstances.add(pddinst);
				}
			}
		}
		List<Individual> retInstances = new ArrayList<Individual>();
		if (retDataDescriptors != null) {
			for (DataDescriptor rdd : retDataDescriptors) {
				Individual rddinst = dataDescriptorToOwl(nm, rdd, true);
				if (rddinst != null) {
					retInstances.add(rddinst);
				}
			}
		}
		if (eqinst != null) {
			addEquationPropertiesToJenaModel(nm, eq, eqinst, paramInstances, retInstances);
		}
		
		return eqinst;
	}
	
	private Individual dataDescriptorToOwl(EObject context, DataDescriptor dd, boolean isReturnValueDescriptor) throws TranslationException {
		Individual ddInst = null;
		OntClass argcls = getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_DATA_DESCRIPTOR_CLASS_URI);
		DatatypeProperty nameProp = getTheJenaModel().getDatatypeProperty(SadlConstants.SADL_IMPLICIT_MODEL_DESCRIPTOR_NAME_PROPERTY_URI);
//		ObjectProperty specfiedUnits = getTheJenaModel().getObjectProperty(SadlConstants.SADL_IMPLICIT_MODEL_SPECIFIED_UNITS_PROPERTY_URI);
		Property typeProp = getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_DATATYPE_PROPERTY_URI);
		if (argcls == null || nameProp == null || typeProp == null) {
			addError("Model doesn't contain all of the Equation metamodel. Do you need to update the SadlImplicitModel?", context);
		}
		else {
			ddInst = getTheJenaModel().createIndividual(argcls);
			boolean useLastTriplesObjectAsDescriptorVariable = false;
			GraphPatternElement lastTriple = null;
			Node nameNode = dd.getName();
			if (nameNode != null && nameNode instanceof NamedNode) {
				ddInst.addProperty(nameProp, nameNode.getName());
				if (nameNode instanceof VariableNode && dd.getGpVariable() == null) {
					dd.setGpVariable(getOrCreateGPVariableNode((VariableNode) nameNode, dd.getType(), true, true, context));
					ddInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_DESCRIPTOR_VARIABLE_PROPERTY_URI), dd.getGpVariable());
				}
				if (nameNode instanceof TypedEllipsisNode) {
					Property varnumargs = getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_VARIABLE_NUM_ARGUMENTS_PROPERTY_URI);
					ddInst.addProperty(varnumargs, getTheJenaModel().createTypedLiteral(true));
				}
			}
			if (nameNode == null && isReturnValueDescriptor) {
				// this is a return DataDescriptor and has no name.
				useLastTriplesObjectAsDescriptorVariable = true;
			}
			Node typeNode = dd.getType();
			if (typeNode == null) {
				addError("Argument has no type", context);
			}
			else if (typeNode instanceof UntypedEllipsisNode) {
				Property varnumargs = getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_VARIABLE_NUM_ARGUMENTS_PROPERTY_URI);
				ddInst.addProperty(varnumargs, getTheJenaModel().createTypedLiteral(true));
			}
			else if (!(typeNode instanceof UnknownNode)){
				ddInst.addProperty(typeProp, typeNode.getURI());
			}
			if (dd.getUnits() != null) {
				Iterator<String> unititr = dd.getUnits().iterator();
				List<Literal> unitList = new ArrayList<Literal>();
				while (unititr.hasNext()) {
					String unit = unititr.next();
					unitList.add(getTheJenaModel().createTypedLiteral(unit));
				}
				if (!unitList.isEmpty()) {
					try {
						OntClass cls = getOrCreateListSubclass(null, XSD.xstring.getURI(), getCurrentResource(), null);
						Individual theList = addMembersToSadlList(getTheJenaModel(), null, cls, XSD.xstring, unitList.iterator());
						if (theList != null) {
							ddInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_SPECIFIED_UNITS_PROPERTY_URI), theList);				
						}
						else {
							addError("Failed to create a SADL typed list for units", context);
						}
					} catch (JenaProcessorException e) {
						e.printStackTrace();
					}
				}
			}
			if (dd.getAugType() != null) {
				List<GraphPatternElement> gpes = null;
				List<Individual> constraints = new ArrayList<Individual>();
				if (dd.getAugType() instanceof NamedNode) {
					Individual semTypeInst = getTheJenaModel().createIndividual(getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_SEMANTIC_TYPE_CLASS_URI));
					semTypeInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_SEM_TYPE_PROPERTY_URI), getTheJenaModel().getResource(((NamedNode)dd.getAugType()).getURI()));
					ddInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_AUGMENTED_TYPE_PROPERTY_URI), semTypeInst);
				}
				else if (dd.getAugType() instanceof SadlUnionClass ||
						(dd.getAugType() instanceof JunctionNode &&
								((JunctionNode)dd.getAugType()).getType().equals(JunctionType.Disj))) {
					addWarning("Union of classes as augmented type not currently supported", context);
				}
				else if (dd.getAugType() instanceof SadlIntersectionClass ||
						(dd.getAugType() instanceof JunctionNode &&
								((JunctionNode)dd.getAugType()).getType().equals(JunctionType.Conj))) {
					addWarning("Intersection of classes as augmented type not currently supported", context);
				}
				else if (dd.getAugType() instanceof Object[]) {
					gpes = new ArrayList<GraphPatternElement>();
					for (int i = 0; i < ((Object[])dd.getAugType()).length; i++) {
						Object o = ((Object[])dd.getAugType())[i];
						if (o instanceof GraphPatternElement) {
							if (o instanceof TripleElement && ((TripleElement)o).getObject() == null) {
								if (isReturnValueDescriptor && nameNode == null) {
									if (getCurrentEquation() != null) {
										List<Node> retNodes = getCurrentEquation().getReturnNodes();
										if (retNodes != null) {
											if (retNodes.size() > 1) {
												addWarning("Semantic constraints on multiple return values not yet implemented", context);
											}
											((TripleElement)o).setObject(retNodes.get(0));
										}
									}
									else {
										addError("Processing augmented type but no current equation; this shouldn't happen", context);
									}
								}
								else {
									((TripleElement)o).setObject(nameNode);
								}
							}
							gpes.add((GraphPatternElement)o);
						}
						else {
							addError("Data descriptor has a list but not all elements are graph patterns (" + o.getClass().getCanonicalName() + ")", context);
						}
					}
					addConstraintsAsAssumptions(context, ddInst, nameNode, typeNode, gpes, constraints, isReturnValueDescriptor);
				}
				else if (dd.getAugType() instanceof GraphPatternElement) {
					GraphPatternElement gpe = (GraphPatternElement) dd.getAugType();
					if (gpe instanceof Junction) {
						try {
							gpes = IntermediateFormTranslator.junctionToList((Junction) gpe);
						} catch (TranslationException e) {
							e.printStackTrace();
						}
					}
					else {
						gpes = new ArrayList<GraphPatternElement>();
						gpes.add(gpe);
					}				
					if (gpes != null && gpes.size() > 0 && (gpes.get(0) instanceof TripleElement) && ((TripleElement)gpes.get(0)).getObject() == null) {
						if (isReturnValueDescriptor && nameNode == null) {
							if (getCurrentEquation() != null) {
								List<Node> retNodes = getCurrentEquation().getReturnNodes();
								if (retNodes != null && retNodes.size() > 0 && retNodes.get(0) instanceof NamedNode) {
									if (retNodes.size() > 1) {
										addWarning("Semantic constraints on multiple return values not yet implemented", context);
									}
									((TripleElement)gpes.get(0)).setObject(retNodes.get(0));
								}
								else if (retNodes == null || retNodes.size() == 0) {
									retNodes = new ArrayList<Node>();
									// this return has name; create a typed VariableNode
									String nvar = getNewVar(context);
									try {
										VariableNode obj = addCruleVariable((NamedNode) typeNode, 1, nvar, context, getHostEObject());
										retNodes.add(obj);
									} catch (TranslationException e) {
										e.printStackTrace();
									} catch (InvalidNameException e) {
										e.printStackTrace();
									}
									getCurrentEquation().setReturnNodes(retNodes);
								}
								else {
									int i = gpes.size() - 1;
									do {
										lastTriple = gpes.get(i--);
									} while (i >= 0 && !(lastTriple instanceof TripleElement));
								}
							}
							else {
								addError("Processing augmented type but no current equation; this shouldn't happen", context);
							}
						}
						else {
							((TripleElement)gpes.get(0)).setObject(nameNode);
						}
					}
					addConstraintsAsAssumptions(context, ddInst, nameNode, typeNode, gpes, constraints, isReturnValueDescriptor);
					if (useLastTriplesObjectAsDescriptorVariable && lastTriple != null) {
						Node lastTriplesObject = ((TripleElement)lastTriple).getObject();
						dd.setName(lastTriplesObject);
						Individual gpv = gpVariableMap.get(lastTriplesObject);
						dd.setGpVariable(gpv);
						ddInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_DESCRIPTOR_VARIABLE_PROPERTY_URI), gpv);
					}
				}
				else {
					addError("Augmented type is not a graph pattern nor a name, not handled (" + dd.getAugType().getClass().getCanonicalName() + ")", context);
				}
			}		
		}
		return ddInst;
	}

	private void addConstraintsAsAssumptions(EObject context, Individual ddInst, Node nameNode, Node typeNode,
			List<GraphPatternElement> gpes, List<Individual> constraints, boolean isReturnValueDescriptor) throws TranslationException {
		try {
			Object gpesOut;
			getIfTranslator().setStartingVariableNumber(getVariableNumber());	// make sure IF doesn't duplicate var names
			if (!isReturnValueDescriptor) {
				gpesOut = getIfTranslator().cook(gpes, true);	// this prevents the adding of notEqual(vx, vy)
			}
			else {
				gpesOut = getIfTranslator().cook(gpes);	// t
			}
			setVariableNumber(getIfTranslator().getVariableNumber());  // make sure this processor doesn't duplicate var names
			if (nameNode instanceof ProxyNode) {
				if (((ProxyNode)nameNode).getProxyFor() instanceof BuiltinElement) {
					List<Node> args = ((BuiltinElement)((ProxyNode)nameNode).getProxyFor()).getArguments();
					Node newNamedNode = args.get(args.size() - 1);
					if (newNamedNode instanceof VariableNode) {
						((VariableNode)newNamedNode).setType(typeNode);
					}
					nameNode = newNamedNode;
				}
				else if (((ProxyNode)nameNode).getProxyFor() instanceof TripleElement) {
					nameNode = ((TripleElement)((ProxyNode)nameNode).getProxyFor()).getObject();
				}
			}
			if (gpesOut instanceof List<?> && ((List<?>)gpesOut).get(0) instanceof Junction) {
				gpes = IntermediateFormTranslator.junctionToList((Junction) ((List<?>)gpesOut).get(0));
			}
		} catch (TranslationException e) {
			e.printStackTrace();
		} catch (InvalidNameException e) {
			e.printStackTrace();
		} catch (InvalidTypeException e) {
			e.printStackTrace();
		}
		for (GraphPatternElement gpe2 : gpes) {
			if (gpe2 instanceof TripleElement) {
				addTriplePatternConstraint((TripleElement) gpe2, ddInst, typeNode, nameNode, constraints, context);
			}
			else if (gpe2 instanceof BuiltinElement) {
				addFunctionPatternConstraint((BuiltinElement)gpe2, ddInst, typeNode, nameNode, constraints, context);
			}
			else {
				addError("Unexpected augmented type constraint: " + gpe2.getClass().getCanonicalName(), context);
			}
		}
		RDFList constList = getTheJenaModel().createList(constraints.iterator());
		Individual assumptInst = getTheJenaModel().createIndividual(getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_SEMANTIC_CONSTRAINT_CLASS_URI));
		assumptInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_CONSTRAINTS_PROPERTY_URI), constList);
		ddInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_AUGMENTED_TYPE_PROPERTY_URI), assumptInst);
	}

	private void addFunctionPatternConstraint(BuiltinElement fp, Individual ddInst, Node typeNode, Node nameNode,
			List<Individual> constraints, EObject context) {
		OntClass functionPatternClass = getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_FUNCTION_PATTERN_CLASS_URI);	// get FunctionPattern class
		Individual fpInst = getTheJenaModel().createIndividual(functionPatternClass);			// create new FunctionPattern 
		String bieqUri;
		try {
			bieqUri = IReasoner.SADL_BUILTIN_FUNCTIONS_URI + "#" + getConfigMgr().getTranslator().builtinTypeToString(fp);
			Individual bieq = getTheJenaModel().getIndividual(bieqUri);			// get the referenced equation
			if (bieq == null) {
				// this might be an implicit builtin
				if (fp.getFuncUri() != null) {
					bieq = getTheJenaModel().createIndividual(fp.getFuncUri(), getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNAL_EQUATION_CLASS_URI));
				}
				else {
					bieqUri = getConfigMgr().getTranslator().getBuiltinClassName(fp.getFuncName());
					if (bieqUri != null) {
						bieq = getTheJenaModel().createIndividual(bieqUri, getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNAL_EQUATION_CLASS_URI));
					}
				}
			}
			if (bieq == null) {
				addError("function " + fp.getFuncName() + " not found in any model.", context);
			}
			else {
				fpInst.addProperty(getTheJenaModel().getOntProperty(SadlConstants.SADL_IMPLICIT_MODEL_BUILTIN_PROPERTY_URI), bieq);
			}
			Iterator<Node> argitr = fp.getArguments().iterator();
			List<org.apache.jena.rdf.model.Resource> argValues = argitr.hasNext() ? new ArrayList<org.apache.jena.rdf.model.Resource>() : null;
			while (argitr.hasNext()) {
				Node argNode = argitr.next();
				org.apache.jena.rdf.model.Resource argNodeRsrc = null;
				if (argNode instanceof VariableNode) {
					argNodeRsrc = getOrCreateGPVariableNode((VariableNode)argNode, typeNode, true, false, context);
				}
				else if (argNode instanceof com.ge.research.sadl.model.gp.Literal) {
					OntClass litCls = getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_GP_LITERAL_VALUE_CLASS_URI);
					argNodeRsrc = getTheJenaModel().createIndividual(litCls);
					Property gpLitValProp = getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_GP_LITERAL_VLAUE_PROPERTYS_URI);
					 Literal litvalval = getTheJenaModel().createTypedLiteral(((com.ge.research.sadl.model.gp.Literal)argNode).getValue());
					argNodeRsrc.addProperty(gpLitValProp, litvalval); 
				}
				else {
					OntClass litCls = getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_GP_RESOURCE_CLASS_URI);
					argNodeRsrc = getTheJenaModel().getResource(argNode.getURI());
				}
				argValues.add(argNodeRsrc);
			}
			if (argValues != null && argValues.size() > 0) {
				RDFList argInstList = getTheJenaModel().createList(argValues.iterator());
				fpInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_ARG_VALUES_PROPERTY_URI), argInstList);
				constraints.add(fpInst);
			}
		} catch (Exception e) {
			addError(e.getMessage(), context);
		}
	}

	private Individual getOrCreateGPVariableNode(VariableNode var, Node type, boolean userNamed, boolean useVariableNodeName, EObject container) {
		if (gpVariableMap == null) {
			gpVariableMap = new HashMap<VariableNode, Individual>();
		}
		if (gpVariableMap.containsKey(var)) {
			return gpVariableMap.get(var);
		}
		return createGPVariable(var, type, useVariableNodeName, container);
	}
	
	private Individual createGPVariable(VariableNode var, Node type, boolean useVariableNodeName, EObject container) {
		OntClass gpVarCls = getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_GP_VARIABLE_CLASS_URI);		// get Variable Class
		useVariableNodeName = true;
		String varName = useVariableNodeName ? var.getName() : getNewVar(container);
		String uniquifier;
		if (getCurrentEquation() != null) {
			uniquifier = getCurrentEquation().getName();
		}
		else if (getTarget() instanceof Individual) {
			uniquifier = ((Individual)getTarget()).getLocalName();
		}
		else {
			uniquifier = "errorFindingUniqueString";
		}
		String uniqueInNamespace = uniquifier + "_" + varName;
		Individual varInstance = getTheJenaModel().createIndividual(createUri(uniqueInNamespace), gpVarCls);
		org.apache.jena.rdf.model.Resource typeCls = getTheJenaModel().getOntClass(type.toFullyQualifiedString());
		if (typeCls != null) {
			varInstance.addRDFType(typeCls);
		}
		gpVariableMap.put(var, varInstance);
		return varInstance;
	}
	
	private void addTriplePatternConstraint(TripleElement tp, Individual ddInst, Node typeNode, Node nameNode,
			List<Individual> constraints, EObject context) throws TranslationException {
		OntClass triplePatternClass = getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_TRIPLE_PATTERN_CLASS_URI);	// get TriplePattern class
		Node subj = tp.getSubject();	
		Individual gpVarSubjInst = null;
		Individual gpSubjNamedNode = null;
		if (subj instanceof VariableNode) {
			boolean varExists = gpVariableExists((VariableNode) subj);
			// the Variable Individual in the ontology must be a unique, system-named node, of the correct type as well as of GPVariable type
			//	so that if it is used in another equation there is no issue (types, semantic constrains can be different, so variables with 
			//	same name in different scope contexts can't have the same URI)
			gpVarSubjInst = getOrCreateGPVariableNode((VariableNode) subj, typeNode, true, false, context);				// create new instance of Variable for subject
			if (!varExists) {
				addGpVariableTypeTriples((VariableNode) subj, gpVarSubjInst, constraints, triplePatternClass);
			}
			
		}
		else if (subj instanceof NamedNode) {
			gpSubjNamedNode = getTheJenaModel().getIndividual(((NamedNode)subj).getURI());
		}
		else {
			throw new TranslationException("Unexpected non-variable, non-UriResource triple constraint subject (" + subj.toString());
		}
		
		Node obj = tp.getObject();
		Individual gpVarObject = null;
		org.apache.jena.rdf.model.Resource gpObjNamedNode = null;
		Literal gpLitObject = null;
		if (obj instanceof VariableNode) {
			boolean varExists = gpVariableExists((VariableNode)obj);
			gpVarObject = getOrCreateGPVariableNode((VariableNode) obj, typeNode, true, false, context);
			if (!varExists) {
				addGpVariableTypeTriples((VariableNode) obj, gpVarObject, constraints, triplePatternClass);
			}
		}
		else if (obj instanceof NamedNode) {
			gpObjNamedNode = getTheJenaModel().getResource(((NamedNode)obj).getURI());
		}
		else if (obj instanceof com.ge.research.sadl.model.gp.Literal) {
			gpLitObject = getTheJenaModel().createTypedLiteral(((com.ge.research.sadl.model.gp.Literal)nameNode).getValue());
		}
		
		Individual subjInst = gpVarSubjInst != null ? gpVarSubjInst : (gpSubjNamedNode != null ? gpSubjNamedNode : null);
		if (subjInst != null) {
			Node predNode = tp.getPredicate();
			Property prop = predNode instanceof NamedNode ? getTheJenaModel().getProperty(((NamedNode)predNode).getURI()) : null;
			if (prop != null) {
				RDFNode objrsrc = gpVarObject != null ? gpVarObject : gpObjNamedNode != null ? gpObjNamedNode : gpLitObject != null ? gpLitObject : null;
				if (objrsrc == null) {
					// this object has no value; create a typed VariableNode
					String nvar = getNewVar(context);
					try {
						obj = addCruleVariable((NamedNode) typeNode, 1, nvar, context, getHostEObject());
						objrsrc = gpVarObject = getOrCreateGPVariableNode((VariableNode) obj, typeNode, true, false, context);
						ddInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_DESCRIPTOR_NAME_PROPERTY_URI), obj.getName());
					} catch (TranslationException e) {
						e.printStackTrace();
					} catch (InvalidNameException e) {
						e.printStackTrace();
					}
				}
				if (objrsrc != null && (!variablesTyped.contains(tp.getSubject()) || !prop.equals(RDF.type))) {
					createTripleSemanticConstraint(triplePatternClass, subjInst, prop, objrsrc, constraints);
				}
			}
			else {
				throw new TranslationException("No property found for triple pattern semantic constraint: " + tp.toString());
			}
		}
		else {
			throw new TranslationException("No subject found for triple pattern semantic constraint: " + tp.toString());
		}
	}

	private Individual createTripleSemanticConstraint(OntClass triplePatternClass, Individual subjInst, Property predNode,
			RDFNode objrsrc, List<Individual> constraints) {
		Individual tpInst = getTheJenaModel().createIndividual(triplePatternClass);												// create new TriplePattern 
		tpInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_GP_SUBJECT_PROPERTY_URI), subjInst);
		tpInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_GP_PREDICATE_PROPERTY_URI), predNode);
		tpInst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_GP_OBJECT_PROPERTY_URI), objrsrc);
		constraints.add(tpInst);
		return tpInst;
	}

	private void addGpVariableTypeTriples(VariableNode subj, Individual gpVarSubjInst, List<Individual> constraints, OntClass triplePatternClass) {
		// add type info of variable
		if (subj != null) {
			if (!variablesTyped.contains(subj) && subj.getType() != null) {
				org.apache.jena.rdf.model.Resource typ = getTheJenaModel().getResource(subj.getType().toFullyQualifiedString());
				createTripleSemanticConstraint(triplePatternClass, gpVarSubjInst, RDF.type, typ, constraints);
				variablesTyped.add(subj);
			}
		}
	}

	private boolean gpVariableExists(VariableNode obj) {
		if (gpVariableMap != null && gpVariableMap.containsKey(obj)) {
			return true;
		}
		return false;
	}

	public void addTypeCheckingError(String message, EObject expr) {
		if (isTypeCheckingWarningsOnly()) {
			issueAcceptor.addWarning(message, expr);
		} else {
			issueAcceptor.addError(message, expr);
		}
		this.setTypeCheckingErrorDetected(true);
	}

	/**
	 * Method to provide special handling for type checking errors so that they can be reported as warnings only
	 * 
	 * @param msg
	 * @param context
	 */
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
		setHostEObject(element);
		clearCruleVariables();
		SadlResource nm = element.getName();
		EList<SadlParameterDeclaration> params = element.getParameter();
		EList<SadlReturnDeclaration> rtype = element.getReturnType();
		String location = element.getLocation();
		Expression whrExpr = element.getWhere();
		Individual eqinst = getTheJenaModel().createIndividual(getDeclarationExtensions().getConceptUri(nm),
				getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNAL_EQUATION_CLASS_URI));
		// Add annotations, if any
		EList<NamedStructureAnnotation> annotations = element.getAnnotations();
		if (annotations != null && annotations.size() > 0) {
			addNamedStructureAnnotations(eqinst, annotations);
		}
		Equation eq = createExternalEquation(element, eqinst, nm, eqinst.getURI(), uri, rtype, params, location, whrExpr);
		addEquation(element.eResource(), eq, nm);
		if (eqinst != null) {
			DatatypeProperty dtp = getTheJenaModel().getDatatypeProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNALURL_PROPERTY_URI);
			Literal literal = uri != null ? getTheJenaModel().createTypedLiteral(uri) : null;
			if (eqinst != null && dtp != null && literal != null) {
				// these can be null if a resource is open in the editor and a clean/build is
				// performed
				eqinst.addProperty(dtp, literal);
				if (location != null && location.length() > 0) {
					DatatypeProperty dtp2 = getTheJenaModel()
							.getDatatypeProperty(SadlConstants.SADL_IMPLICIT_MODEL_LOCATION_PROPERTY_URI);
					Literal literal2 = getTheJenaModel().createTypedLiteral(location);
					eqinst.addProperty(dtp2, literal2);
				}
			}
		}
		setHostEObject(null);
	}

	protected Equation createExternalEquation(ExternalEquationStatement element, Individual eqinst, SadlResource nm, String internalUri, String externalUri, EList<SadlReturnDeclaration> rtype,
			EList<SadlParameterDeclaration> params, String location, Expression whrExpr)
			throws JenaProcessorException, TranslationException, InvalidNameException, InvalidTypeException {
		Equation eq = new Equation(getDeclarationExtensions().getConcreteName(nm));
		eq.setNamespace(getDeclarationExtensions().getConceptNamespace(nm));
		eq.setExternal(true);
		eq.setExternalUri(externalUri);
		eq.setUri(internalUri);
		if (location != null) {
			eq.setLocation(location);
		}
		if (whrExpr != null) {
			Object whrObj = processExpression(whrExpr);
			if (whrObj instanceof List<?>) {
				eq.setWheres((List<GraphPatternElement>) whrObj);
			} 
			else if (whrObj instanceof GraphPatternElement){
				eq.addWhereElement((GraphPatternElement) whrObj);
			}
			else {
				throw new TranslationException("External equation where translation not of expected type.");
			}
		}
		List<DataDescriptor> paramDataDescriptors = null;
		if (params != null && params.size() > 0) {
//			if (params.get(0).getUnknown() == null) {
				List<Node> args = new ArrayList<Node>();
				List<Node> argtypes = new ArrayList<Node>();
				paramDataDescriptors = new ArrayList<DataDescriptor>();
				for (int i = 0; i < params.size(); i++) {
					SadlParameterDeclaration param = params.get(i);
					SadlResource pr = param.getName();
					Node pn = null;
					Node prtnode = null;
					if (pr != null) {
						pn = processExpression(pr);
						args.add((Node) pn);
						SadlTypeReference prtype = param.getType();
						prtnode = sadlTypeReferenceToNode(prtype);				
						if (prtnode != null) {
							if (param.getTypedEllipsis() != null) {
								argtypes.add(createTypedEllipsisNodeFromVariableNode(pn, prtnode));
							}
							else {
								argtypes.add(prtnode);
							}
						}
					}
					else if (param.getEllipsis() != null) {
						Node uten = new UntypedEllipsisNode();
						args.add(uten);
						argtypes.add(uten);
					}
					else if (param.getUnknown() != null) {
						Node ukn = new UnknownNode();
						args.add(ukn);
						argtypes.add(ukn);
					}
				}
				eq.setArguments(args);
				eq.setArgumentTypes(argtypes);
				// put equation in context for sub-processing
				setCurrentEquation(eq);
				for (int i = 0; i < params.size(); i++) {
					if (i < args.size()) {
						Node pn = args.get(i);
						Node prtnode = null;
						if (i > argtypes.size() - 1) {
							addError("Error in argument types", params.get(i));
						}
						else {
							prtnode = argtypes.get(i);
						}
						SadlParameterDeclaration param = params.get(i);
						EObject augtype = param.getAugtype();
						Object augTypeObj = null;
						if (augtype != null) {
							augTypeObj = processExpression(augtype);
						}
						paramDataDescriptors.add(new DataDescriptor((Node) pn, prtnode, param.getUnits(), augTypeObj));
				}
			}
				// clear current equation
				setCurrentEquation(null);
			}
//		}
		List<Node> rtypes = new ArrayList<Node>();
		List<DataDescriptor> retDataDescriptors = new ArrayList<DataDescriptor>();
		for (SadlReturnDeclaration srd : rtype) {
			SadlTypeReference rt = srd.getType();
			EObject augtype = srd.getAugtype();
			Object augTypeObject = null;
			if (augtype != null) {
				augTypeObject = processExpression(augtype);
			}
			Node rtnode = null;
			if (rt != null) {
				rtnode = sadlTypeReferenceToNode(rt);
				rtypes.add(rtnode);
			}
			else if (srd.getUnknown() != null) {
				rtypes.add(new UnknownNode());
			}
			else {
				rtypes.add(null);
			}
			if (rtnode != null) {
				retDataDescriptors.add(new DataDescriptor(null, rtnode, null, augTypeObject));
			}
		}
		eq.setReturnTypes(rtypes);
		setCurrentEquation(eq);
		equationToOwl(nm, eqinst, eq, retDataDescriptors, paramDataDescriptors);
		logger.debug("External Equation: " + eq.toFullyQualifiedString());
		setCurrentEquation(null);
		return eq;
	}

	private Node createTypedEllipsisNodeFromVariableNode(Node pn, Node type) throws TranslationException {
		TypedEllipsisNode tevn = new TypedEllipsisNode(type.getURI());
		if (type instanceof VariableNode) {
			tevn.setHostObject(((VariableNode)type).getHostObject());
		}
		return tevn;
	}

	private NamedNode sadlTypeReferenceToNode(SadlTypeReference rtype)
			throws JenaProcessorException, InvalidNameException, TranslationException {
		ConceptName cn = sadlSimpleTypeReferenceToConceptName(rtype);
		if (cn == null) {
			return null;
		}
		NamedNode nn = conceptNameToNamedNode(cn);
		nn.setContext(rtype);
		return nn;
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
		OntModelProvider.addOtherContent(resource, eq);;
	}

	private boolean namespaceIsImported(String namespace, Resource resource) {
		String currentNamespace = namespace.replace("#", "");
		if (currentNamespace.equals(IReasoner.SADL_BUILTIN_FUNCTIONS_URI)
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
		List<Equation> equations = new ArrayList<Equation>();
		List<Object> other = OntModelProvider.getOtherContent(resource);
		if(other != null) {
			for(Object obj: other) {
				if(obj instanceof Equation) {
					equations.add((Equation)obj);
				}
			}
		}
		return equations;
	}

	private void processStatement(RuleStatement element)
			throws InvalidNameException, InvalidTypeException, TranslationException, JenaProcessorException {
		clearCruleVariables();
		String ruleName = getDeclarationExtensions().getConcreteName(element.getName());
		Rule rule = new Rule(ruleName);
		setTarget(rule);
		int stage = element.getStage();
		rule.setStage(stage);
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
		getIfTranslator().setStartingVariableNumber(getVariableNumber());
		try {
			if (isExpandMissingPatternsInValidation()) {
				rule = getIfTranslator().cook(rule);				
			}
			else {
				rule = getIfTranslator().postProcessRule(rule, element);
			}
			setVariableNumber(getIfTranslator().getVariableNumber());  // make sure this processor doesn't duplicate var names
		}
		catch (Exception e) {
			addError("Fatal error post-processing rule. " + e.getMessage(), element);
		}
		if (rules == null) {
			rules = new ArrayList<Rule>();
		}
		rules.add(rule);
		String uri = declarationExtensions.getConceptUri(element.getName());
		OntClass rcls = getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_RULE_CLASS_URI);
		if (rcls != null) {
			Individual rl = createIndividual(element.getName(), rcls);
			// Add annotations, if any
			EList<NamedStructureAnnotation> annotations = element.getAnnotations();
			if (annotations != null && annotations.size() > 0) {
				addNamedStructureAnnotations(rl, annotations);
			}
		}
		try {
			List<ModelError> errlist = getConfigMgr().getTranslator().validateRule(rule);
			if (errlist != null && errlist.size() > 0) {
				modelErrorsToOutput(getCurrentResource(), errlist, true);
			}
		} catch (ConfigurationException e) {
			e.printStackTrace();
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
		getIfTranslator().resetIFTranslator();	// this resets varNum to 0
		Object expansion = getIfTranslator().expandProxyNodes(rawIntermediateForm, treatAsConclusion, true);
		setVariableNumber(getIfTranslator().getVariableNumber());  // make sure this processor doesn't duplicate var names
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
			try {
				return processExpression((SubjHasProp) expr);
			} catch (TranslationException e) {
				throw e;
			}
			catch (Exception e) {
				addError(e.getMessage(), expr);
			} 
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
		} else if (expr instanceof UpdateExpression) {
			return processExpression((UpdateExpression) expr);
		} else if (expr instanceof ThereExistsStatement) {
			return processExpression((ThereExistsStatement) expr);
		} else if (expr != null) {
			throw new TranslationException("Unhandled rule expression type: " + expr.getClass().getCanonicalName());
		}
		return expr;
	}
	
	public Object processExpression(ValueTable expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		ValueRow row = ((ValueTable) expr).getRow();
		if (row == null) {
			EList<ValueRow> rows = ((ValueTable) expr).getRows();
			if (rows == null || rows.size() == 0) {
				ValueTable vtbl = ((ValueTable) expr).getValueTable();
				return processExpression(vtbl);
			}
			List<List<Node>> tbl = new ArrayList<List<Node>>();
			for (ValueRow vr : rows) {
				EList<Expression> rowValues = vr.getExplicitValues();
				List<Node> rowObjects = new ArrayList<Node>();
				for (Expression val : rowValues) {
					Object valObj = processExpression(val);
					rowObjects.add(nodeCheck(valObj));
				}
				tbl.add(rowObjects);
			}
			ValueTableNode vtn = new ValueTableNode();
			vtn.setRows(tbl);
			return vtn;
		}
		else {
			EList<Expression> rowvals = row.getExplicitValues();
			List<Object> rowObjects = new ArrayList<Object>();
			for (Expression val : rowvals) {
				Object valObj = processExpression(val);
				rowObjects.add(valObj);
			}
			return convertToValueTableNode(rowObjects);
		}
	}
	
	public Object processExpression(ThereExistsStatement expr) throws InvalidTypeException, InvalidNameException, TranslationException {
		Expression matchExpr = expr.getMatch();
		// the first expression in the match should be the class and may be:
		//	a Declaration as subject and a complete SubjHasProp (derived from "there exists a <Class> with <pred> <obj> ....)
		//	a Declaration wrapped in a SubjHasProp with null right (derived from "there exists a <Class> <varName> and ...)
		Expression first = matchExpr;
		List<Expression> restExpressions = new ArrayList<Expression>();
		while (first instanceof BinaryOperation) {
			Expression rest = ((BinaryOperation)first).getRight();
			if (rest != null) {
				restExpressions.add(0, rest);
			}
			else {
				addError("Unexpected null right-hand side of binary operation", first);
			}
			first = ((BinaryOperation)first).getLeft();
		}
		
		Object cls = processExpression(first);
		List<Object> restObjs = new ArrayList<Object>();
		for (Expression rexpr : restExpressions) {
			restObjs.add(processExpression(rexpr));
		}
		NamedNode thereExistsIdentity = null;
		if (cls instanceof VariableNode) {
			thereExistsIdentity = (VariableNode) cls;
			cls = ((VariableNode)cls).getType();
		}
		else {
			if (cls instanceof Object[] && 
					((Object[])cls)[0] instanceof NamedNode &&
					((Object[])cls)[1] instanceof GraphPatternElement) {
				if (((Object[])cls)[1] instanceof TripleElement) {
					restObjs.add(0, ((Object[])cls)[1]);
				}
				else if (((Object[])cls)[1] instanceof Junction) {
					JunctionList expanded = IntermediateFormTranslator.junctionToList((Junction)((Object[])cls)[1]);
					if (expanded.getJunctionType().equals(JunctionType.Disj)) {
						addError("Disjunction in 'there exists' is not valid", matchExpr);
					}
					int insertAt = 0;
					for (GraphPatternElement gpe : expanded) {
						if (gpe instanceof TripleElement) {
							restObjs.add(insertAt++, gpe);
						}
						else {
							addError("Expected a triple pattern but found '" + gpe.toString(), matchExpr);
						}
					}
				}
				cls = ((Object[])cls)[0];
				thereExistsIdentity = (NamedNode) cls;
				if (cls instanceof VariableNode) {
					cls = ((VariableNode)cls).getType();
				}
			}
			else {
				addError("Did not find expected beginning of there exists.", first);
			}
		}
		Expression plusExpr = expr.getPlus();
		Object plusObj = null;
		if (plusExpr != null) {
			plusObj = processExpression(plusExpr);
		}
		BuiltinElement tebi = new BuiltinElement();
		tebi.setFuncName("thereExists");
		tebi.addArgument(nodeCheck(cls));
		for (Object robj : restObjs) {
			TripleElement rtr = null;
			if (robj instanceof TripleElement) {
				rtr = (TripleElement) robj;
			}
			else if (robj instanceof Object[] && ((Object[])robj)[1] instanceof TripleElement) {
				rtr = (TripleElement) ((Object[])robj)[1];
			}
			else {
				addError("failed to find match triple", matchExpr);
			}
			if (rtr != null) {
				if (rtr.getSubject().equals(thereExistsIdentity)) {
					tebi.addArgument(rtr.getPredicate());
					tebi.addArgument(rtr.getObject());
				}
				else if (rtr.getObject() != null && rtr.getObject().equals(thereExistsIdentity)){
					tebi.addArgument(rtr.getSubject());
					tebi.addArgument(rtr.getPredicate());
				}
				else {
					String msg ="It appears that there is a triple in 'there exists' that does not reference the matching instance.";
					if (!useArticlesInValidation) {
						msg += " Are you using articles in translation?";
					}
					msg += " Does the triple belong in the 'plus' portion?";
					addError(msg, rtr.getContext() != null ? (EObject)rtr.getContext() : expr);
				}
			}
		}
		if (plusObj != null) {
			NamedNode plusNode = new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_URI + "#Plus");
			plusNode.setNodeType(NodeType.InstanceNode);
			tebi.addArgument(plusNode);
			List<TripleElement> plusObjs = new ArrayList<TripleElement>();
			if (plusObj instanceof Junction) {
				JunctionList plusGpes = getIfTranslator().junctionToList(((Junction)plusObj));
				if (plusGpes.getJunctionType().equals(JunctionType.Disj)) {
					addError("Disjunction in 'there exists' is not valid", plusExpr);
				}
				for (GraphPatternElement gpe : plusGpes) {
					if (gpe instanceof TripleElement) {
						plusObjs.add((TripleElement) gpe);
					}
					else {
						addError("Non-graph pattern found in plus statements", plusExpr);
					}
				}
			}
			else if (plusObj instanceof TripleElement) {
				plusObjs.add((TripleElement) plusObj);
			}
			else if (plusObj instanceof Object[] && ((Object[])plusObj)[1] instanceof TripleElement) {
				plusObjs.add((TripleElement) ((Object[])plusObj)[1]);
			}
			else {
				addError("failed to find plus triple", matchExpr);
			}
			for (TripleElement rtr : plusObjs) {
				if (rtr.getSubject().equals(thereExistsIdentity)) {
					tebi.addArgument(rtr.getPredicate());
					tebi.addArgument(rtr.getObject());
				}
				else {
					tebi.addArgument(rtr.getSubject());
					tebi.addArgument(rtr.getPredicate());
				}
			}
		}
		
		return tebi;
	}

	private ValueTableNode convertToValueTableNode(List<Object> rowObjects) throws TranslationException {
		List<Node> row = new ArrayList<Node>();
		for (Object robj : rowObjects) {
			if (robj instanceof Node) {
				row.add((Node)robj);
			}
			else {
				throw new TranslationException("Unexpected non-Node in value table content");
			}
		}
		List<List<Node>> tbl = new ArrayList<List<Node>>();
		tbl.add(row);
		ValueTableNode vtn = new ValueTableNode();
		vtn.setRows(tbl);
		return vtn;
	}

	public Object processExpression(BinaryOperation expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		// is this a variable definition?
		boolean isLeftVariableDefinition = false;
		Name leftVariableName = null;
		ValueTable leftVariableNames = null;
		Expression leftVariableDefn = null;
		TypeCheckInfo leftVariableDefnTci = null;
		boolean leftVariableDefnTripleMissingObject = false;
		boolean leftVariableDefnTripleMissingSubject = false;

		// math operations can be a variable on each side of operand
		boolean isRightVariableDefinition = false;
		Name rightVariableName = null;
		Expression rightVariableDefn = null;
		try {
			if (isBinaryOpVariableDefinition(expr.getLeft())) {
//			if (expr.getLeft() instanceof Name && isVariableDefinition((Name) expr.getLeft())) {
				// left is variable name, right is variable definition
				isLeftVariableDefinition = true;
				if (expr.getLeft() instanceof Name) {
					leftVariableName = (Name) expr.getLeft();
				}
				else if (expr.getLeft() instanceof ValueTable) {
					leftVariableNames = (ValueTable)expr.getLeft();
				}
				leftVariableDefn = expr.getRight();
			}
			if (isBinaryOpVariableDefinition(expr.getRight())) {
//			if (expr.getRight() instanceof Name && isVariableDefinition((Name) expr.getRight())) {
				// right is variable name, left is variable definition
				isRightVariableDefinition = true;
				if (expr.getRight() instanceof Name) {
					rightVariableName = (Name) expr.getRight();
				}
				else if (expr.getRight() instanceof ValueTable) {
					addError("Multi-variable definition on right is not supported at this time", expr);
				}
				rightVariableDefn = expr.getLeft();
				
				if (!isContainedByQuery(expr)
						&& EcoreUtil2.getContainerOfType(expr, RuleStatement.class) == null) {

					addError("Variable definition on right is not supported at this time", expr);
				}

			}
		} catch (CircularDefinitionException e) {
			e.printStackTrace();
		}

		if (isLeftVariableDefinition) {
			List<Name> variableNames = new ArrayList<Name>();
			if (leftVariableName != null) {
				variableNames.add(leftVariableName);
			}
			else if (leftVariableNames != null) {
				variableNames.addAll(getNamesFromValueTable(leftVariableNames));
			}
			int leftMultiVarIndex = 0;
			Object leftTranslatedDefn = null;
			for (Name varnm : variableNames) {
				try {
					VariableNode leftVar = createVariable(
							getDeclarationExtensions().getConceptUri(varnm.getName()), expr);
					if (leftVar == null) { // this can happen on clean/build when file is open in editor
						return null;
					}
					addVariableNamingEObject(leftVar, varnm.getName());
					Declaration varkey = getLeftDeclaration(leftVariableDefn);
					if (varkey != null) {
						setVariableInDefinition(varkey, leftVar);
					}
					Object rest = null;
					if (leftTranslatedDefn == null) {
						leftTranslatedDefn = applyPulledUpOperations(processExpression(leftVariableDefn));
						if (expressionIsNegatedClass(leftVariableDefn, leftTranslatedDefn)) {
							// this can only be true if leftTranslatedDefn is a BuiltinElement so case on next line is OK
							TripleElement trel = negatedClassAndVariableToTripleElement(leftVar, (BuiltinElement) leftTranslatedDefn);
							return trel;
						}
					}
					if (varkey != null) {
						setVariableInDefinition(varkey, null);
					}
					if (leftTranslatedDefn instanceof Object[] && ((Object[]) leftTranslatedDefn).length == 2) {
						rest = ((Object[]) leftTranslatedDefn)[1];
						if (((Object[]) leftTranslatedDefn)[0].equals(leftVar)) {
							Node vtype = leftVar.getType();
							if (vtype instanceof NamedNode) {
								leftVariableDefnTripleMissingObject = checkForMissingObjectVariableInTriple(leftVar, leftTranslatedDefn, leftVariableDefn);
								if (!leftVariableDefnTripleMissingObject) {
									leftVariableDefnTripleMissingSubject = checkForMissingSubjectVariableInTriple(leftVar, leftTranslatedDefn, leftVariableDefn);
								}
								addVariableDefinition(leftVar, rest, (NamedNode) vtype, expr);
							}
							return rest;
						}
						leftTranslatedDefn = ((Object[]) leftTranslatedDefn)[0];
					}
					else {
						Node vtype = leftVar.getType();
						if (vtype == null && leftVariableDefn != null) {
							leftVariableDefnTci = getModelValidator().getType(leftVariableDefn);
							if (leftVariableNames != null && leftVariableDefnTci.getCompoundTypes() != null) {
								leftVariableDefnTci = leftVariableDefnTci.getCompoundTypes().get(leftMultiVarIndex);
							}
							if (leftVariableDefnTci != null && leftVariableDefnTci.getTypeCheckType() != null) {
								Node lvdttype = leftVariableDefnTci.getTypeCheckType();
								if (lvdttype != null && !(lvdttype instanceof ConstantNode)) {
									vtype = lvdttype;
									validateNode(vtype);
								}
							}
						}
						if (vtype instanceof NamedNode) {
							leftVar.setType(vtype);
							leftVariableDefnTripleMissingObject = checkForMissingObjectVariableInTriple(leftVar, leftTranslatedDefn, leftVariableDefn);
							if (!leftVariableDefnTripleMissingObject) {
								leftVariableDefnTripleMissingSubject = checkForMissingSubjectVariableInTriple(leftVar, leftTranslatedDefn, leftVariableDefn);
							}
						}
						else if (vtype != null) {
							throw new TranslationException("This shouldn't happen!");
						}
					}
					NamedNode leftDefnType = null;
					if (leftTranslatedDefn instanceof NamedNode) {
						if (leftTranslatedDefn instanceof VariableNode) {
							leftDefnType = (NamedNode) ((VariableNode) leftTranslatedDefn).getType();
						}else if(((NamedNode) leftTranslatedDefn).getLocalizedType() != null) {
							leftDefnType = (NamedNode) ((NamedNode) leftTranslatedDefn).getLocalizedType(); 
						}else {
							leftDefnType = (NamedNode) leftTranslatedDefn;
						}
						setVarType(leftVar, leftDefnType, (Boolean) null, leftVariableDefn);
						if (isRightVariableDefinition) {
							// 11/14/2018 awc: this can't happen--we're already in a isLeftVariableDefinition true so this can't be true
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
						if((((NamedNode) leftTranslatedDefn).isList() && ((NamedNode) leftTranslatedDefn).getListLiterals() != null) ||
								((NamedNode) leftTranslatedDefn).getNodeType().equals(NodeType.DataTypeProperty)) {
							addVariableDefinition(leftVar, leftTranslatedDefn, leftDefnType, expr);
							GraphPatternElement bi = createBinaryBuiltin(expr.getOp(), leftVar, leftTranslatedDefn);
							((BuiltinElement)bi).setFuncName("assign");
							return combineRest(bi, rest);
						}
						// TODO shouldn't generate triple for type as variable contains all type info.
						// should just return rest?
						// problem is that rest can be null and to just return the variable doesn't work
						// because callers expect GraphPatternElements returned
						// normally the variable will be in a TripleElement and/or a BuiltinElement and
						// could be dropped entirely
						// but for now returning this extra type although for a list range will be
						// incorrect? awc 12/6/2017
						//Still uncertain if these triple elements should still be applicable
						//NGB 4-4-2019
						TripleElement trel;
						if (leftVariableDefn instanceof Declaration) {
							trel = new TripleElement(leftVar, new RDFTypeNode(), (Node) leftTranslatedDefn);
						}
						else {
							trel = new TripleElement(leftVar, new RDFTypeNode(), leftDefnType);
						}
						addVariableDefinition(leftVar, leftTranslatedDefn, leftDefnType, expr);
						trel.setSourceType(TripleSourceType.SPV);
						return combineRest(trel, rest);
					} else {
						if (leftVariableDefnTci == null) {
							leftVariableDefnTci = getModelValidator().getType(leftVariableDefn);
						}
						if (leftVariableDefnTci != null) {
							if (leftVar.getType() == null) {
								// can only be multiple variables on left if the right is a multi-return value function, which will be a BuiltinElement as the translation
								if (leftVariableDefnTci.getCompoundTypes() != null) {
									Object jct = compoundTypeCheckTypeToNode(leftVariableDefnTci, leftVariableDefn);
									if (jct != null && jct instanceof Junction) {
										setVarType(leftVar, nodeCheck(jct), leftVariableDefnTci.isList(), leftVariableDefn);
									} else {
										addTypeCheckingError(
												"Compound type check did not process into expected result for variable type",
												leftVariableDefn);
									}
								} else if (leftVariableDefnTci.getTypeCheckType() != null
										&& leftVariableDefnTci.getTypeCheckType() instanceof NamedNode) {
									leftDefnType = (NamedNode) leftVariableDefnTci.getTypeCheckType();
									setVarType(leftVar, leftDefnType, leftVariableDefnTci.isList(), leftVariableDefn);
								}
							}
							if (leftVariableDefnTci.getTypeCheckType() != null && leftVariableDefnTci.getTypeCheckType() instanceof NamedNode) {
								leftDefnType = (NamedNode) leftVariableDefnTci.getTypeCheckType();
							}
							if (leftTranslatedDefn instanceof GraphPatternElement) {
								leftVar.addDefinition(nodeCheck((GraphPatternElement) leftTranslatedDefn));
							}
						} else if (leftTranslatedDefn instanceof GraphPatternElement) {
							leftVar.addDefinition(nodeCheck((GraphPatternElement) leftTranslatedDefn));
						} else if (leftTranslatedDefn instanceof List<?>) {
	//						leftVar.setDefinition((List<GraphPatternElement>) leftTranslatedDefn);
							throw new TranslationException("This shouldn't happen!");
						}
						
						boolean iterationComplete = false;
						if (leftVariableDefnTripleMissingObject) {
							// this is a variable definition and the definition is a triple and the triple
							// had no object
	//						((TripleElement) leftTranslatedDefn).setObject(leftVar);
	//						addVariableDefinition(leftVar, leftTranslatedDefn, leftDefnType, expr);
							return leftTranslatedDefn;
						} else if (leftVariableDefnTripleMissingSubject) {
							return leftTranslatedDefn;
						} else if (leftTranslatedDefn instanceof BuiltinElement) {
							int eArgCnt = ((BuiltinElement) leftTranslatedDefn).getExpectedArgCount();
							int currentArgCnt = ((BuiltinElement) leftTranslatedDefn).getArguments() != null
									? ((BuiltinElement) leftTranslatedDefn).getArguments().size()
									: 0;
							if (eArgCnt > currentArgCnt) {
								((BuiltinElement) leftTranslatedDefn).addArgument(leftVar);
								if (leftMultiVarIndex == variableNames.size() - 1) {
									// all variables processed
									return leftTranslatedDefn;
								}
								iterationComplete = true;
							}
						}
						if (!iterationComplete) {
							if (leftTranslatedDefn instanceof TripleElement
									&& (((TripleElement) leftTranslatedDefn).getSubject() instanceof VariableNode ||
											((TripleElement)leftTranslatedDefn).getSubject() instanceof ProxyNode)) {
								replaceVariable((TripleElement) leftTranslatedDefn, leftVar);
								addVariableDefinition(leftVar, leftTranslatedDefn, leftDefnType, expr);
								if (rest != null) {
									List<GraphPatternElement> ret = new ArrayList<GraphPatternElement>();
									ret.add((GraphPatternElement) rest);
									ret.add((TripleElement) leftTranslatedDefn);
									return ret;
								}
								else {
									return leftTranslatedDefn;
								}
							} else {
								Node defn = nodeCheck(leftTranslatedDefn);
								GraphPatternElement bi = createBinaryBuiltin(expr.getOp(), leftVar, defn);
								if (bi instanceof BuiltinElement && (defn instanceof com.ge.research.sadl.model.gp.Literal || defn instanceof ConstantNode || 
										(defn instanceof NamedNode && ((NamedNode)defn).getNodeType().equals(NodeType.InstanceNode)))) {
									((BuiltinElement)bi).setFuncName("assign");
								}
								addVariableDefinition(leftVar, leftTranslatedDefn, leftDefnType, expr);
								if (leftMultiVarIndex == variableNames.size() - 1) {
									// all variables processed
									if (rest != null) {
										List<GraphPatternElement> ret = new ArrayList<GraphPatternElement>();
										ret.add((GraphPatternElement) rest);
										ret.add(bi);
										return ret;
									}
									else {
										return bi;
									}
								}
							}
						}
					}
				} catch (URISyntaxException e) {
					e.printStackTrace();
				} catch (IOException e) {
					e.printStackTrace();
				} catch (ConfigurationException e) {
					e.printStackTrace();
				} catch (DontTypeCheckException e) {
				} catch (CircularDefinitionException e) {
					e.printStackTrace();
				} catch (CircularDependencyException e) {
					e.printStackTrace();
				} catch (PropertyWithoutRangeException e) {
					addPropertyWithoutRangeError(leftVariableDefn, null, e);
				} catch (PrefixNotFoundException e) {
					e.printStackTrace();
				}
				if (leftMultiVarIndex < variableNames.size() - 1) {
					leftVariableDefnTci = null;
				}
				leftMultiVarIndex++;
			}
		} else if (isRightVariableDefinition) { // only, left is not variable definition
			/*
			 * Currently "Variable definition on right is not supported at this time"
			 * However, this is needed for Rule statements
			 */
			
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
									addTypeCheckingError(
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
						rightVar.addDefinition(nodeCheck((GraphPatternElement) rightTranslatedDefn));
					} else if (rightTranslatedDefn instanceof List<?>) {
//						rightVar.setDefinition((List<GraphPatternElement>) rightTranslatedDefn);
						throw new TranslationException("This shouldn't happen!");
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
				e.printStackTrace();
			} catch (PrefixNotFoundException e) {
				e.printStackTrace();
			} catch (ConfigurationException e) {
				e.printStackTrace();
			} catch (URISyntaxException e) {
				e.printStackTrace();
			} catch (DontTypeCheckException e) {
				if (!e.getMessage().startsWith("OK")) {
					e.printStackTrace();
				}
			} catch (CircularDefinitionException e) {
				e.printStackTrace();
			} catch (CircularDependencyException e) {
				e.printStackTrace();
			} catch (PropertyWithoutRangeException e) {
				e.printStackTrace();
			}
		}

		String op = expr.getOp();

		Expression lexpr = expr.getLeft();
		Expression rexpr = expr.getRight();

		Object result = processBinaryExpressionByParts(expr, op, lexpr, rexpr);
		if(result instanceof TripleElement) {
			checkForArticleForNameInTriple(lexpr, result, SIDE.LEFT);
			checkForArticleForNameInTriple(rexpr, result, SIDE.RIGHT);
		}else if(result instanceof BuiltinElement) {
			checkForArticleForNameInBuiltinElement(lexpr, result);
			checkForArticleForNameInBuiltinElement(rexpr, result);
		}
		return result;
	}

	/**
	 * Method to get all of the Names contained in a ValueTable
	 * @param expr
	 * @return
	 */
	private Collection<? extends Name> getNamesFromValueTable(ValueTable expr) {
		List<Name> names = new ArrayList<Name>();
		ValueRow row = ((ValueTable) expr).getRow();
		if (row == null) {
			EList<ValueRow> rows = ((ValueTable) expr).getRows();
			if (rows == null || rows.size() == 0) {
				ValueTable vtbl = ((ValueTable) expr).getValueTable();
				if (vtbl != null) {
					row = vtbl.getRow();
				}
				
			}
		}
		if (row != null) {
			EList<Expression> vals = row.getExplicitValues();
			if (vals != null) {
				if (vals.size() > 1) {
					for (Expression val : vals) {
						if (val instanceof Name) {
							names.add((Name) val);
						}
					}	
				}
			}
		}
		return names;
	}

	/**
	 * Method to look at an expression and decide if it is the variable(s) side of a binary operation defining the variable(s).
	 * Note that multiple variables only occur in the context of a rule with variables accepting the return values of an equation
	 * returning multiple values.
	 * @param expr
	 * @return
	 * @throws CircularDefinitionException
	 */
	private boolean isBinaryOpVariableDefinition(Expression expr) throws CircularDefinitionException {
		if (expr instanceof Name) {
			if(isVariableDefinition((Name) expr)) {
				return true;
			}
		}
		if (EcoreUtil2.getContainerOfType(expr, RuleStatement.class) != null) {
			// in a rule
			if (expr instanceof ValueTable) {
				ValueRow row = ((ValueTable) expr).getRow();
				if (row == null) {
					EList<ValueRow> rows = ((ValueTable) expr).getRows();
					if (rows == null || rows.size() == 0) {
						ValueTable vtbl = ((ValueTable) expr).getValueTable();
						if (vtbl != null) {
							row = vtbl.getRow();
						}
						
					}
				}
				if (row != null) {
					EList<Expression> vals = row.getExplicitValues();
					if (vals != null) {
						if (vals.size() > 1) {
							boolean allVariableDefs = true;
							for (Expression val : vals) {
								if (val instanceof Name && getDeclarationExtensions().getOntConceptType(((Name) val).getName())
										.equals(OntConceptType.VARIABLE)) {
									if (!getDeclarationExtensions().getDeclaration(((Name) val).getName()).equals((Name) val)) {
										allVariableDefs = false;
									}
								}
							}	
							return allVariableDefs;
						}
					}
					
				}
			}
		}
		return false;
	}

	/**
	 * Method to set the object of a defining triple to the variable being defined when appropriate
	 * @param var
	 * @param defn
	 * @param defnExpr
	 * @return
	 */
	private boolean checkForMissingObjectVariableInTriple(VariableNode var, Object defn,
			Expression defnExpr) {
		if (defnExpr instanceof PropOfSubject && defn instanceof TripleElement && ((TripleElement)defn).getObject() == null) {
			((TripleElement)defn).setObject(var);
			return true;
		}
		return false;
	}

	/**
	 * Method to set the subject of a defining triple to the variable being defined when appropriate
	 * @param var
	 * @param defn
	 * @param defnExpr
	 * @return
	 */
	private boolean checkForMissingSubjectVariableInTriple(VariableNode var, Object defn,
			Expression defnExpr) {
		if (defnExpr instanceof PropOfSubject && defn instanceof TripleElement && ((TripleElement)defn).getSubject() == null) {
			((TripleElement)defn).setSubject(var);
			return true;
		}
		return false;
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

	protected void addVariableNamingEObject(VariableNode var, SadlResource name) {
		var.setHostObject(name);
	}

	protected boolean replaceVariable(TripleElement triple, VariableNode var)
			throws IOException, PrefixNotFoundException, InvalidNameException, InvalidTypeException,
			TranslationException, ConfigurationException {
		if (triple.getObject() == null) {
			triple.setObject(var);
			return true;
		}
		return false;
	}

	public void setVarType(VariableNode var, Node vartype, Boolean isList, EObject defn) throws TranslationException, InvalidNameException, DontTypeCheckException, InvalidTypeException {
		// if it hasn't been set before just set it
		if (vartype instanceof NamedNode) {
			vartype = validateNode(vartype);
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
		else if (vartype.equals(var.getType())) {
			return;
		}
		else if (vartype instanceof NamedNode && var.getType() instanceof NamedNode && 
				isNamedNodeSubclassOfNamedNode((NamedNode)var.getType(), (NamedNode)vartype)) {
			return;
		}
		// it has been set before so we need to do some type checking
		List<Node> typesOfType = new ArrayList<Node>();
		if (vartype instanceof NamedNode) {
			if (((NamedNode) vartype).getNodeType().equals(NodeType.InstanceNode)) {
				Individual typeInst = getTheJenaModel().getIndividual(((NamedNode) vartype).toFullyQualifiedString());
				ExtendedIterator<org.apache.jena.rdf.model.Resource> typeitr = typeInst.listRDFTypes(false);
				while (typeitr.hasNext()) {
					org.apache.jena.rdf.model.Resource typ = typeitr.next();
					if (typ.isURIResource()) {
						NamedNode nn = new NamedNode(typ.getURI(), NodeType.ClassNode);
						typesOfType.add(validateNamedNode(nn));
					}
				}
			}
			else if (isProperty(((NamedNode)vartype).getNodeType())) {
				TypeCheckInfo ptci = getModelValidator().getTypeInfoFromRange(namedNodeToConceptName((NamedNode)vartype), 
						getTheJenaModel().getProperty(((NamedNode) vartype).toFullyQualifiedString()), defn);
				typesOfType.add(ptci.getTypeCheckType());
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

	@Override
	public NamedNode validateNamedNode(NamedNode namedNode) {
		return UtilsForJena.validateNamedNode(getConfigMgr(), getModelNamespace(), namedNode);
	}

	private boolean isNamedNodeSubclassOfNamedNode(NamedNode cls, NamedNode subcls) {
		OntResource curCls = getTheJenaModel().getOntResource(((NamedNode) cls).toFullyQualifiedString());
		OntResource newCls = getTheJenaModel().getOntResource(((NamedNode) subcls).toFullyQualifiedString());
		if (curCls != null && newCls != null && curCls.canAs(OntClass.class) && newCls.canAs(OntClass.class)) {
			try {
				if (classIsSubclassOfCached(newCls.as(OntClass.class), curCls.as(OntClass.class), true, null)) {
					return true; // OK if subclass
				}
			} catch (CircularDependencyException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return false;
	}

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
		EObject eobj = decl.eContainer();
		while (eobj != null) {
			if (eobj instanceof ThereExistsStatement) {
				return true;
			}
			else if (eobj.eContainer() != null) {
				eobj = eobj.eContainer();
			}
			else {
				break;
			}
		}
		return false;
	}

	protected Object processBinaryExpressionByParts(EObject container, String op, Expression lexpr, Expression rexpr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		StringBuilder errorMessage = new StringBuilder();
		boolean isDefiningDeclaration = false;
		if (lexpr != null && rexpr != null) {
			isDefiningDeclaration = isDefiningDeclaration(lexpr, rexpr, op);
			// no need to do type checking if this is a defining declaration
			if(!isDefiningDeclaration && !isNegation(rexpr) &&
					!getModelValidator().validateBinaryOperationByParts(container, lexpr, rexpr, op, errorMessage, false)){
				addTypeCheckingError(errorMessage.toString(), container);
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
			if (lobj instanceof NamedNode) {
				((NamedNode)lobj).setContext(lexpr);
			}
		} else {
			addError("Left side of '" + op + "' is null", lexpr); // TODO Add new error
			return null;
		}
		Object robj = null;
		Object rest = null;
		if (rexpr != null) {
			robj = applyPulledUpOperations(processExpression(rexpr));
			if (robj instanceof NamedNode) {
				((NamedNode)robj).setContext(rexpr);
			}
			if (op != null && op.equals("and") && getTarget() instanceof Query) {
				// check for special case where there is a conjunction of properties or property chains that is an abbreviated form
				if (rexpr instanceof PropOfSubject && leftSideIncompletePropertyChain(lexpr)) {
					if (robj instanceof Object[] && ((Object[]) robj).length == 2) {
						rest = ((Object[]) robj)[1];
						robj = ((Object[]) robj)[0];
					}
					lobj = deabbreviateLeftOfConjunction(lexpr, rexpr, lobj, robj, rest);
				}
			}
		} else {
			addError("Right side of '" + op + "' is null", rexpr); // TODO Add new error
			return null;
		}

		if(lobj != null && robj !=null ) {
			//Check to see if we need to add implied property to entire built in element
			// Data has Implied property _number (int) and _valid (bool) dataList is list of type Data
			// shall set _number of data1 to element data2 of dataList
			// The implied property here is on the right had side of _number of returned list type Data
			if(robj instanceof BuiltinElement) {
				//check for implied properties
				Map<EObject, Property> ip = getModelValidator().getImpliedPropertiesUsed();
				if(ip != null) {
					Iterator<EObject> ipitr = ip.keySet().iterator();
					while (ipitr.hasNext()) {
						EObject eobj = ipitr.next();
						Property implProp = ip.get(eobj);
						try {
							TypeCheckInfo lPropTci = getModelValidator().getType((EObject)eobj);
							
							TypeCheckInfo ipNN = getMatchingImpliedPropertyBaseTci(lPropTci, implProp, eobj);
							NamedNode impliedPropertyNode = (validateNamedNode(new NamedNode(implProp.getURI(), NodeType.PropertyNode)));
        					addLocalizedTypeToNode(impliedPropertyNode, ipNN);		
                        
							if (eobj.equals(rexpr)) {
								((BuiltinElement) robj).setImpliedPropertyNode(impliedPropertyNode);								
								
								//Handle a prop of subject statement with "constant" OP "PropOfSubject"
								//ex. data1 to index of data2 in intList
								//data2 here need to be integer type
							}else if (rexpr instanceof PropOfSubject &&
									((PropOfSubject) rexpr).getRight() instanceof PropOfSubject &&
									((PropOfSubject) rexpr).getLeft() instanceof Constant) {
								PropOfSubject propOfSubj = (PropOfSubject) ((PropOfSubject) rexpr).getRight();
								if(eobj.equals(propOfSubj.getLeft())) {
									((NamedNode) ((BuiltinElement) robj).getArguments().get(1)).setImpliedPropertyNode(impliedPropertyNode);								
									((NamedNode) ((BuiltinElement) robj).getArguments().get(1)).setLocalizedType(lPropTci.getTypeCheckType());								
								}
							}
						} catch (URISyntaxException | IOException | ConfigurationException | DontTypeCheckException
								| CircularDefinitionException | CircularDependencyException
								| PropertyWithoutRangeException e) {
							e.printStackTrace();
						}
					}
				}
			}
			
			if(lobj instanceof TripleElement && robj instanceof TripleElement) {
			
				eventConj.clear();
				TripleElement tr = (TripleElement)lobj;
				TripleElement tl = (TripleElement)robj;
				Node trnode = tr.getObject();
				Node tlnode = tl.getObject();
				if(trnode!= null && tlnode !=null) {
					OntResource resourceL = theJenaModel.getOntResource(trnode.toFullyQualifiedString());
					OntResource resourceR = theJenaModel.getOntResource(tlnode.toFullyQualifiedString());
					
					OntClass subclassl = null;
					if(resourceL != null) {
						if(resourceL.canAs(OntClass.class)) {
							subclassl = resourceL.as(OntClass.class);
							
						}else if(resourceL.canAs(Individual.class)) {
							subclassl = resourceL.as(Individual.class).getOntClass();
						}
					}
					
					OntClass subclassr = null;
					if(resourceR != null) {
						if(resourceR.canAs(OntClass.class)) {
							subclassr = resourceR.as(OntClass.class);
							
						}else if(resourceR.canAs(Individual.class)) {
							subclassr = resourceR.as(Individual.class).getOntClass();
						}
					}
		
					OntResource suprclass = theJenaModel.getOntResource(SadlConstants.SADL_IMPLICIT_MODEL_EVENT_URI);
					if(subclassl != null && subclassr != null) {
						try {
							if (classIsSubclassOfCached(subclassl, suprclass, true, null) && classIsSubclassOfCached(subclassr,suprclass,true,null)) {

								try {
									if (classIsSubclassOfCached(subclassr,suprclass,true,null)){
										if(isConjunction(op)){
											
											eventConj.add(tr);
											eventConj.add(tl);
											  	
										}
										
									}
								} catch (CircularDependencyException e) {
									e.printStackTrace();
								}
							}
						} catch (CircularDependencyException e) {
							e.printStackTrace();
						}
					
					}
				}	
			}
		}

		if (optype == BuiltinType.Equal || optype == BuiltinType.NotEqual) {
			// If we're doing an assignment, we can simplify the pattern.
			Node assignedNode = null;
			Object pattern = null;
			if (lobj instanceof VariableNode && robj instanceof VariableNode &&
					(!((VariableNode)lobj).isCRulesVariable() || !((VariableNode)robj).isCRulesVariable()) &&
					lexpr instanceof Declaration && rexpr instanceof Declaration) {
				// this is a narrower type declaration; the left is a variable, the right is a type
				assignedNode = validateNode((Node)lobj);
				pattern = new TripleElement(assignedNode, new RDFTypeNode(), ((VariableNode) robj).getType());
			}
			else if (expressionIsNegatedClass(rexpr, robj)) {
				// this is like the case below only negated
				if (robj instanceof BuiltinElement && ((BuiltinElement)robj).getArguments().size() == 1) {
//					this should always be true, error checking only
					TripleElement trel = negatedClassAndVariableToTripleElement((Node)lobj, (BuiltinElement)robj);
					if (lobj instanceof VariableNode && robj instanceof NamedNode && 
							(((NamedNode)robj).getNodeType().equals(NodeType.ClassNode) ||
									((NamedNode)robj).getNodeType().equals(NodeType.ClassListNode))) {
						// this is a restriction on the variable type
						((VariableNode)lobj).addDefinition(nodeCheck(trel));
						try {
							applyRestrictionToVariableType((VariableNode)lobj, (NamedNode) robj, rexpr);
						} catch (CircularDependencyException e) {
							e.printStackTrace();
						}
					}
					return applyImpliedAndExpandedProperties(container, lexpr, rexpr, trel, false);
				}
				else {
					addError("Something is going wrong with translation, please report", rexpr);
				}
			}
			else if (op.equals("is") && rexpr instanceof Declaration) {
				if (robj instanceof VariableNode && lobj instanceof NamedNode && 
						(((NamedNode)lobj).getNodeType().equals(NodeType.InstanceNode) ||
								((NamedNode)lobj).getNodeType().equals(NodeType.VariableNode))) {
					// this is a Declaration with a variable on the right and an individual on the left
					TripleElement trel = new TripleElement((Node) lobj, new RDFTypeNode(), (Node) robj);
					trel.setSourceType(TripleSourceType.ITC);
					if (isDefiningDeclaration) {
						trel.setObject(((VariableNode)robj).getType());
					}
					return applyImpliedAndExpandedProperties(container, lexpr, rexpr, trel, false);
				}
				else if (!(robj instanceof VariableNode)) {
					if (lobj instanceof Node && robj instanceof Node) {
						TripleElement trel = new TripleElement((Node) lobj, new RDFTypeNode(), (Node) robj);
						trel.setSourceType(TripleSourceType.ITC);
						if (lobj instanceof VariableNode && robj instanceof NamedNode && ((NamedNode)robj).getNodeType().equals(NodeType.ClassNode)) {
							// this is a restriction on the variable type
							((VariableNode)lobj).addDefinition(nodeCheck(trel));
							try {
								applyRestrictionToVariableType((VariableNode)lobj, (NamedNode) robj, rexpr);
							} catch (CircularDependencyException e) {
								e.printStackTrace();
							}
						}
						return applyImpliedAndExpandedProperties(container, lexpr, rexpr, trel, false);
					} else {
						// throw new TranslationException("Unhandled binary operation condition: left
						// and right are not both nodes.");
						addError(SadlErrorMessages.UNHANDLED.get("binary operation condition. ",
								"Left and right are not both nodes."), container);
					}
				}
			}
			else if (lobj instanceof VariableNode && rexpr instanceof Declaration && robj instanceof VariableNode &&
					!((VariableNode)robj).isCRulesVariable()) {
				// this condition happens when a variable is being given a type which is also a variable, e.g. x is a y
				assignedNode = validateNode((Node)lobj);
				pattern = new TripleElement(assignedNode, new RDFTypeNode(), (Node) robj);
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
			if (lobj instanceof Node && robj instanceof TripleElement) {
				assignedNode = validateNode((Node) lobj);
				pattern = (TripleElement) robj;
			}
			else if (lobj instanceof TripleElement && robj instanceof Node) {
				assignedNode = validateNode((Node) robj);
				pattern = (TripleElement) lobj;
			}
			else if (getTarget() != null && getTarget() instanceof Test &&
					lobj instanceof NamedNode && ((NamedNode)lobj).getNodeType().equals(NodeType.InstanceNode) &&
					robj instanceof VariableNode && rest instanceof GraphPatternElement) {
				// this is a test with type and properties
				TripleElement tr1 = new TripleElement((Node) lobj, new RDFTypeNode(), ((VariableNode)robj).getType());
				if (rest instanceof Junction) {
					rest = getIfTranslator().junctionToList((Junction)rest);
				}
				else if (rest instanceof TripleElement) {
					List<GraphPatternElement> lst = new ArrayList<GraphPatternElement>();
					lst.add((GraphPatternElement) rest);
					rest = lst;
				}
				else {
					addError("Unhandled Test pattern", container);
				}
				List<TripleElement> results = new ArrayList<TripleElement>();
				results.add(tr1);
				for (int i = 0; i < ((List<?>)rest).size(); i++) {
					GraphPatternElement gpe = (GraphPatternElement) ((List<?>)rest).get(i);
					((TripleElement)gpe).setSubject((Node) lobj);
					results.add((TripleElement) gpe);
				}
				return results;
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
						&& ((TripleElement) pattern).getSourceType() != null
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
					else if (getRulePart().equals(RulePart.CONCLUSION)) {
						((TripleElement)pattern).setType(TripleModifierType.Assignment);
					}
				} else if (pattern instanceof TripleElement && ((TripleElement)pattern).getSourceType() != null
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
						return applyImpliedAndExpandedProperties(container, lexpr, rexpr, pattern, false);
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
						} else if (lastPattern.getObject() == null) {
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
						&& ((TripleElement) pattern).getSourceType() != null
						&& ((TripleElement) pattern).getSourceType().equals(TripleSourceType.ITC)
						&& ((TripleElement) pattern).getSubject() instanceof VariableNode
						&& assignedNode instanceof VariableNode) {
					// in a rule of this type we just want to replace the pivot node variable
					doVariableSubstitution(((TripleElement) pattern),
							(VariableNode) ((TripleElement) pattern).getSubject(), (VariableNode) assignedNode);
				}
				return applyImpliedAndExpandedProperties(container, lexpr, rexpr, pattern, false);
			}
			BuiltinElement bin = null;
			boolean binOnRight = false;
			Object retObj = null;
			if (lobj instanceof Node && robj instanceof BuiltinElement) {

				//Check to "pull up" not operator for Node Objects and rebuild "is" object
				BuiltinElement right = (BuiltinElement) robj;
				if(right.getFuncName() == "not") {
					//Pull up the not to the outside operator with the "is" operator nested     			
					Node right_arg = right.getArguments().get(0);
					GraphPatternElement bi = createBinaryBuiltin(op, lobj, right_arg); 
					Object biWithImpliedProperties = applyImpliedAndExpandedProperties(container, lexpr, rexpr, bi, false);
					Object ubi = createUnaryBuiltin(container, "not", biWithImpliedProperties);
					return combineRest(ubi, rest);
				}

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
					return applyImpliedAndExpandedProperties(container, lexpr, rexpr, retObj, false);
				} else if (assignedNode instanceof Node && isComparisonBuiltin(bin.getFuncName())) {
					// this is a comparison with an extra "is"
					if (bin.getArguments().size() == 1) {
						if (bin.isCreatedFromInterval() || binOnRight) {
							bin.addArgument(0, assignedNode);
						} else {
							bin.addArgument(assignedNode);
						}
						return applyImpliedAndExpandedProperties(container, lexpr, rexpr, bin, false);
					}
				}
			}

			//Check to "pull up" not operator for TripleElements and rebuild "is" object
			if( lobj instanceof TripleElement && robj instanceof BuiltinElement) {
				BuiltinElement right = (BuiltinElement) robj;
				if(right.getFuncName() == "not") {
					TripleElement left = (TripleElement) lobj;
					Node arg = right.getArguments().get(0);
					//Pull up the not to the outside operator with the "is" operator nested   
					if (arg instanceof ConstantNode && ((ConstantNode)arg).getName().equals(SadlConstants.CONSTANT_KNOWN)) {
						left.setObject(arg);
						left.setType(TripleModifierType.Not);
						return left;
					}
					else {
						applyImpliedAndExpandedProperties(container, lexpr, rexpr, left, false);
						GraphPatternElement bi = createBinaryBuiltin(op, left, arg); 
						if (bi == null && getTarget() instanceof Test) {
							Test tst = (Test) getTarget();
							tst.setCompName(BuiltinType.NotEqual);
							return null;
						}
						else {
							Object ubi = createUnaryBuiltin(container, "not", bi);
							return combineRest(ubi, rest);
						}
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
				return applyImpliedAndExpandedProperties(container, lexpr, rexpr, pattern, false);
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
					createBinaryBuiltin("assign", robj, lobj), false);
		}

		if (op.equals("and") || op.equals("or")) {
			boolean simpleUnionOrIntersection = true;
			if (robj instanceof ProxyNode || robj instanceof GraphPatternElement || 
					robj instanceof ArrayList<?> || robj instanceof Object[]) {
				simpleUnionOrIntersection = false;
			}
			else if (lobj instanceof ProxyNode || lobj instanceof GraphPatternElement || 
					lobj instanceof ArrayList<?> || lobj instanceof Object[]) {
				simpleUnionOrIntersection = false;
			}
			if (simpleUnionOrIntersection) {
				// this is a special case--union or intersection of two Nodes
				JunctionNode ci = null;
				if (lobj != null && robj != null) {
					if (op.equals("or")) {
						ci = new JunctionNode(nodeCheck(lobj), nodeCheck(robj), JunctionType.Disj);
					}
					else {
						ci = new JunctionNode(nodeCheck(lobj), nodeCheck(robj), JunctionType.Conj);
					}
				}
				return ci;
			}
			Junction jct = new Junction();
			jct.setJunctionName(op);
			jct.setContext(container);
			
			//check for implied properties on junctions with top level named nodes
			if(robj instanceof NamedNode || robj instanceof BuiltinElement) {
				Map<EObject, Property> ip = getModelValidator().getImpliedPropertiesUsed();
				if(ip != null) {
					Iterator<EObject> ipitr = ip.keySet().iterator();
					while (ipitr.hasNext()) {
						EObject eobj = ipitr.next();
						Property implProp = ip.get(eobj);
					
						try {
							TypeCheckInfo lPropTci = getModelValidator().getType((EObject)eobj);
							TypeCheckInfo ipNN = getMatchingImpliedPropertyBaseTci(lPropTci, implProp, eobj);
							NamedNode impliedPropertyNode = (validateNamedNode(new NamedNode(implProp.getURI(), NodeType.PropertyNode)));
        					addLocalizedTypeToNode(impliedPropertyNode, ipNN);									
							
							if (eobj.equals(rexpr)) {
								if(robj instanceof NamedNode) {
		        					addLocalizedTypeToNode(((NamedNode) robj), lPropTci);	
									((NamedNode) robj).setImpliedPropertyNode(impliedPropertyNode);
								}else {
		        					addLocalizedTypeToNode(((NamedNode) robj), lPropTci);
									((BuiltinElement) robj).setImpliedPropertyNode(impliedPropertyNode);
								}
							}
						} catch (URISyntaxException | IOException | ConfigurationException | DontTypeCheckException
								| CircularDefinitionException | CircularDependencyException
								| PropertyWithoutRangeException e) {
							e.printStackTrace();
						}
					}	
				}
			}
	
			if(lobj instanceof NamedNode) {
				//check for implied properties
				Map<EObject, Property> ip = getModelValidator().getImpliedPropertiesUsed();
				if(ip != null) {
					Iterator<EObject> ipitr = ip.keySet().iterator();
					while (ipitr.hasNext()) {
						EObject eobj = ipitr.next();
						Property implProp = ip.get(eobj);
						try {
							TypeCheckInfo lPropTci = getModelValidator().getType((EObject)eobj);
							
							TypeCheckInfo ipNN = getMatchingImpliedPropertyBaseTci(lPropTci, implProp, eobj);
							NamedNode impliedPropertyNode = (validateNamedNode(new NamedNode(implProp.getURI(), NodeType.PropertyNode)));
        					addLocalizedTypeToNode(impliedPropertyNode, ipNN);	
                        
							if (eobj.equals(lexpr)) {
								if(lobj instanceof NamedNode) {
		        					addLocalizedTypeToNode(((NamedNode) lobj), lPropTci);
									((NamedNode) lobj).setImpliedPropertyNode(impliedPropertyNode);								
								}
	                        }
						} catch (URISyntaxException | IOException | ConfigurationException | DontTypeCheckException
								| CircularDefinitionException | CircularDependencyException
								| PropertyWithoutRangeException e) {
							e.printStackTrace();
						}
						
	
					}
				}
			}
			jct.setLhs(nodeCheck(postProcessTranslationResult(lobj)));
//			if (rest != null) {
//				Object[] newRobj = new Object[2];
//				newRobj[0] = robj;
//				newRobj[1] = rest;
//				robj = newRobj;
//			}
//			jct.setRhs(nodeCheck(robj instanceof NamedNode && rest != null ? rest : robj));
			jct.setRhs(nodeCheck(postProcessTranslationResult(robj)));
			
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
			bi.setContext(container);
			return combineRest(applyImpliedAndExpandedProperties(container, lexpr, rexpr, bi, false), rest);
		}
	}

	/**
	 * Method to add missing (abbreviated) content to the left side of a conjunction, 
	 * drawing from the right side.
	 * @param lobj
	 * @param robj
	 * @param rest 
	 * @return
	 */
	private Object deabbreviateLeftOfConjunction(Expression lexpr, Expression rexpr, Object lobj, Object robj, Object rest) {
		// find matching domain to expand abbreviated form
		// find the end of the left chain
		try {
			if (lobj instanceof NamedNode && isProperty((NamedNode)lobj)) {
				lobj = new TripleElement(null, (Node) lobj, null);
			}
		} catch (TranslationException e1) {
			addError(e1.getMessage(), lexpr);
		}
		if (!(lobj instanceof TripleElement)) {
			addError("Left of conjunctions not a triple as expected", lexpr);
			return lobj;
		}
		if (!(robj instanceof TripleElement)) {
			addError("Right of conjunctions not a triple as expected", rexpr);
			return lobj;
		}
		TripleElement leftEndTriple = (lobj instanceof TripleElement) ? (TripleElement)lobj : null;
		while (leftEndTriple.getSubject() != null) {
			if (leftEndTriple.getSubject() instanceof ProxyNode) {
				GraphPatternElement gpe = ((ProxyNode)leftEndTriple.getSubject()).getProxyFor();
				if (gpe instanceof TripleElement) {
					leftEndTriple = (TripleElement) gpe;
				}
				else {
					// this shouldn't happen
					addError("Left of conjunctions contains unexpected non-triple", rexpr);
				}
			}
		}
		if (leftEndTriple != null) {
			Node pred1 = leftEndTriple.getPredicate();
			if (pred1 instanceof NamedNode) {
				Property prop1 = getTheJenaModel().getProperty(((NamedNode)pred1).getURI());
				List<OntClass> domainMembers = getPropertyDomainClasses(prop1);
				if (domainMembers !=  null) {
					boolean done = false;
					TripleElement robjtriple = (TripleElement)robj;
					TripleElement newLobjAccumulatorElement = null;
					TripleElement lastNewLobjElement =  null;
					for (OntClass member : domainMembers) {
						while (robjtriple != null) {
							Node pred2 = robjtriple.getPredicate();
							if (pred2 instanceof NamedNode) {
								Property prop2 = getTheJenaModel().getProperty(((NamedNode)pred2).getURI());
								List<OntClass> domainMembers2 = getPropertyDomainClasses(prop2);
								if (domainsMatch(member, domainMembers2, (EObject)robjtriple.getContext())) {
									if (lastNewLobjElement == null) {
										leftEndTriple.setSubject(robjtriple.getSubject());
									}
									else {
										leftEndTriple.setSubject(lastNewLobjElement.getSubject());
									}
									done = true;
									break;
								}
								else {
									if (robjtriple.getSubject() instanceof ProxyNode) {
										Object gpe = ((ProxyNode) robjtriple.getSubject()).getProxyFor();
										if (gpe instanceof TripleElement) {
											robjtriple = (TripleElement) gpe;
											if (newLobjAccumulatorElement == null) {
												newLobjAccumulatorElement = new TripleElement(robjtriple.getSubject(), robjtriple.getPredicate(), robjtriple.getObject());
												lastNewLobjElement = newLobjAccumulatorElement;
											}
											else {
												TripleElement newTriple = new TripleElement(robjtriple.getSubject(), robjtriple.getPredicate(), robjtriple.getObject());
												try {
													lastNewLobjElement.setObject(new ProxyNode(newTriple));
													lastNewLobjElement = newTriple;
												} catch (InvalidTypeException e) {
													addError(e.getMessage(), (EObject) robjtriple.getContext());
													return lobj;
												}
											}
										}
										else {
											addError("Unexpected non-triple", (EObject)robjtriple.getContext());
											return lobj;
										}
									}
									else {
										addError("Unexpected non-proxy", (EObject)robjtriple.getContext());
										return lobj;
									}
								}
							}
						}
						if (done) {
							break;
						}
					}
				}
			}
		}
		if (rest != null) {
			// rest needs to be applied to both lobj and robj
			Object[] larr = new Object[2];
			larr[0] = lobj;
			larr[1] = rest;
			return larr;
		}
		return lobj;
	}
	
	private boolean domainsMatch(OntClass member, List<OntClass> domainMembers2, EObject context) {
		for (OntClass member2 : domainMembers2) {
			if (domainMembers2 != null && domainMembers2.contains(member)) {
				return true;
			}
			try {
				if (SadlUtils.classIsSubclassOf(member, member2, true, null)) {
					return true;										
				}
			} catch (CircularDependencyException e) {
				addError(e.getMessage(), context);
			}
		}
		return false;
	}

	/**
	 * Method to get a list of OntClass members of a property's domain.
	 * @param p
	 * @return
	 */
	private List<OntClass> getPropertyDomainClasses(Property p) {
		List<OntClass> domainMembers = null;
		StmtIterator sitr = getTheJenaModel().listStatements(p, RDFS.domain, (RDFNode)null);
		while (sitr.hasNext()) {
			RDFNode obj = sitr.nextStatement().getObject();
			if (obj.isResource()) {
				org.apache.jena.rdf.model.Resource rsrc = obj.asResource();
				if (rsrc.canAs(OntClass.class)) {
					OntClass cls = rsrc.as(OntClass.class);
					if (domainMembers == null) {
						domainMembers = new ArrayList<OntClass>();
					}
					if (cls.isUnionClass()) {
						RDFList members = cls.asUnionClass().getOperands();
						List<RDFNode> javaList = members.asJavaList();
						for (RDFNode member : javaList) {
							if (member.isResource() && member.asResource().canAs(OntClass.class)) {
								domainMembers.add(member.asResource().as(OntClass.class));
							}
						}
					}
					else {
						domainMembers.add(cls);
					}
				}
			}
		}
		return domainMembers;
	}

	/**
	 * Method to analyze an Expression in a query and determine if it is an incomplete property
	 * chain, that is a property chain that ends with a property rather than a class or instance.
	 * @param expr
	 * @return
	 */
	private boolean leftSideIncompletePropertyChain(Expression expr) {
		try {
			if (expr instanceof Declaration) {
				SadlTypeReference decltype = ((Declaration)expr).getType();
				if (decltype instanceof SadlSimpleTypeReference) {
					SadlResource declSR = ((SadlSimpleTypeReference)decltype).getType().getName();
					if (isProperty(getDeclarationExtensions().getOntConceptType(declSR))) {
						return true;
					}
				}
			}
			else if (expr instanceof Name && isProperty(getDeclarationExtensions().getOntConceptType((SadlResource) expr))) {
				return true;
			}
			else if (expr instanceof PropOfSubject) { // && ((PropOfSubject) lexpr).getRight() == null) {
				while (((PropOfSubject)expr).getRight() instanceof PropOfSubject) {
					expr = ((PropOfSubject)expr).getRight();
				}
				if (((PropOfSubject)expr).getRight() instanceof Name || 
						((PropOfSubject)expr).getRight() instanceof Declaration) {
					return leftSideIncompletePropertyChain(((PropOfSubject)expr).getRight());
				}
				else if (((PropOfSubject)expr).getRight() instanceof SubjHasProp) {
					return false;
				}
				return leftSideIncompletePropertyChain(((PropOfSubject)expr).getLeft());
			}
		} catch (CircularDefinitionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return false;
	}

	/**
	 * Method to take a Node (lobj) and a negated class (robj) and create a corresponding TripleElement
	 * @param lobj
	 * @param robj
	 * @return
	 * @throws InvalidTypeException
	 */
	private TripleElement negatedClassAndVariableToTripleElement(Node lobj, BuiltinElement robj) throws InvalidTypeException {
		Node effectiveRobj = ((BuiltinElement)robj).getArguments().get(0);
		TripleElement trel = new TripleElement((Node) lobj, new RDFTypeNode(), effectiveRobj);
		trel.setType(TripleElement.TripleModifierType.Not);
		trel.setSourceType(TripleSourceType.ITC);
		return trel;
	}

	/**
	 * Method to determine if the Expression rexpr, with translation 
	 * (negation ignored in translationto robj, to be pulled up)
	 * is a negated class ("not a Someclass")
	 * @param rexpr
	 * @param robj
	 * @return
	 */
	private boolean expressionIsNegatedClass(Expression rexpr, Object robj) {
		if (rexpr instanceof UnaryExpression && ((UnaryExpression)rexpr).getOp().equals("not") && 
		((UnaryExpression)rexpr).getExpr() instanceof Declaration && 
		robj instanceof BuiltinElement && ((BuiltinElement)robj).getArguments() != null &&
		((BuiltinElement)robj).getArguments().size() == 1 &&
		((BuiltinElement)robj).getArguments().get(0) instanceof NamedNode &&
		(((NamedNode)((BuiltinElement)robj).getArguments().get(0)).getNodeType().equals(NodeType.ClassNode) || 
				((NamedNode)((BuiltinElement)robj).getArguments().get(0)).getNodeType().equals(NodeType.ClassListNode))) {
			return true;
		}
		return false;
	}

	private boolean isDefiningDeclaration(Expression lexpr, Expression rexpr, String op) {
		if (op.equals("is") && lexpr instanceof Name && declarationExtensions.getDeclaration((Name)lexpr).equals(lexpr) &&
				lexpr.eContainer().equals(rexpr.eContainer())) {
			return true;
		}
		return false;
	}

	private void applyRestrictionToVariableType(VariableNode vobj, NamedNode restrictionType, EObject expr) throws CircularDependencyException, TranslationException {
		if (((VariableNode)vobj).getType() != null) {
			// this variable already has a type so the restriction should be a narrowing of type to be valid
			Node oldType = ((VariableNode)vobj).getType();
			if (oldType instanceof NamedNode) {
				if (!oldType.equals(restrictionType)) {
					OntResource oldRsrc = getTheJenaModel().getOntResource(((NamedNode)oldType).getURI());
					OntClass newRsrc = getTheJenaModel().getOntClass(restrictionType.getURI());
					if (oldRsrc != null && newRsrc != null && !classIsSubclassOfCached(newRsrc, oldRsrc, true, null)) {
						if (!(getTarget() instanceof Rule) || !getRulePart().equals(RulePart.CONCLUSION)) {
							// this is not consistent	this is OK to have in a rule conclusion--it is concluding, not conditioning
// TODO need to allow "x ... and x ..." or "x... or x ...."
							if (!variableRestrictionsInJunction(expr, vobj, restrictionType, (NamedNode) oldType)) {
								addTypeCheckingError("Restriction on variable type must be a subclass of type from definition.", expr);
							}
						}
					}
				}
			}
		}
	}
	
	private boolean variableRestrictionsInJunction(EObject expr, VariableNode vobj, NamedNode restrictionType, NamedNode oldType) {
		BinaryOperation bocont = EcoreUtil2.getContainerOfType(expr, BinaryOperation.class);
		List<Node> defns = vobj.getDefinitions();
		if (defns != null) {
			Node defn = defns.get(0);
			if (defn instanceof ProxyNode) {
				GraphPatternElement gpe = ((ProxyNode)defn).getProxyFor();
				if (gpe instanceof TripleElement && 
						((TripleElement)gpe).getSubject() instanceof VariableNode &&
						((VariableNode)((TripleElement)gpe).getSubject()).equals(vobj)) {
					return true;
				}
			}
		}
		return false;
	}

	public TypeCheckInfo getMatchingImpliedPropertyBaseTci(TypeCheckInfo aImpliedBaseTci, Property aPropertyToMatch, EObject aEObj) throws DontTypeCheckException, InvalidTypeException, TranslationException, InvalidNameException{
		Iterator<ConceptName> litr = aImpliedBaseTci.getImplicitProperties().iterator();
		TypeCheckInfo baseImpliedPropertyTci = null;
		while (litr.hasNext()) {
			ConceptName cn = litr.next();
			Property prop = theJenaModel.getProperty(cn.getUri());
			if(aPropertyToMatch.equals(prop)){
				if (cn.getType() == null) {
					setPropertyConceptNameType(cn, prop);
				}
				baseImpliedPropertyTci = getModelValidator().getTypeInfoFromRange(cn, prop, aEObj);
			}
		}
		
		return baseImpliedPropertyTci;
	}

	/**
	 * Check and add implied and expanded properties on a GraphPatternElement at
	 * the NamedNode level within the object for implied properties and at the 
	 * GraphPatternElement level for expanded properties
	 * @param binobj
	 * @param lobj
	 * @param robj
	 * @param maybeGpe
	 * @param recursiveCall 
	 * @return Object
	 */
	private Object applyImpliedAndExpandedProperties(EObject binobj, EObject lobj, EObject robj, Object maybeGpe, boolean recursiveCall) {
		try {
			Map<EObject, Property> ip = getModelValidator().getImpliedPropertiesUsed();
			List<EObject> toBeRemoved = null;
			if (ip != null && ip.size() > 0) {
				if (maybeGpe instanceof TripleElement || maybeGpe instanceof BuiltinElement) {
					Iterator<EObject> ipitr = ip.keySet().iterator();
					boolean matched = false;
					while (ipitr.hasNext()) {
						EObject eobj = ipitr.next();
						Property implProp = ip.get(eobj);
						
						try {
							TypeCheckInfo lPropTci = getModelValidator().getType((EObject)eobj);
							TypeCheckInfo ipNN = getMatchingImpliedPropertyBaseTci(lPropTci, implProp, eobj);
							NamedNode impliedPropertyNode = (validateNamedNode(new NamedNode(implProp.getURI(), NodeType.PropertyNode)));
        					addLocalizedTypeToNode(impliedPropertyNode, ipNN);
							
							if (eobj.equals(lobj)) {
								//((GraphPatternElement) maybeGpe).setLeftImpliedPropertyUsed(validateNamedNode(new NamedNode(implProp.getURI(), NodeType.PropertyNode)));
								//set the implied property node for the underlying named node instead of GPE
								if(maybeGpe instanceof TripleElement) {
									NamedNode predicate = (NamedNode) ((TripleElement) maybeGpe).getPredicate();
		        					addLocalizedTypeToNode(((NamedNode) predicate), lPropTci);
									predicate.setImpliedPropertyNode(impliedPropertyNode);

								}else {
									//left is the first argument of a BuiltinElement
									List<Node> args = ((BuiltinElement) maybeGpe).getArguments();
									if(args.get(0) instanceof NamedNode ) {
										NamedNode arg1 = (NamedNode) args.get(0);
										Node arg2 = args.get(1);
										if (!recursiveCall && ((BuiltinElement)maybeGpe).getFuncType().equals(BuiltinType.Equal) && arg1 instanceof NamedNode) {
											TripleElement newTr = new TripleElement(arg1, impliedPropertyNode, arg2);
											maybeGpe = newTr;
										}
			        					addLocalizedTypeToNode(arg1, lPropTci);
										arg1.setImpliedPropertyNode(impliedPropertyNode);

									}else if (args.get(0) instanceof ProxyNode && args.get(1) instanceof ProxyNode) {
										GraphPatternElement lGPE = ((ProxyNode)args.get(0)).getProxyFor();
										if(lGPE instanceof BuiltinElement) {
											BuiltinElement bie = (BuiltinElement) lGPE;
											bie.setImpliedPropertyNode(impliedPropertyNode);
										}else if(lGPE instanceof TripleElement){
											NamedNode predicate = (NamedNode) ((TripleElement) lGPE).getPredicate();
				        					addLocalizedTypeToNode(((NamedNode) predicate), lPropTci);
											predicate.setImpliedPropertyNode(impliedPropertyNode);
										}
									}
								}
								
								matched = true;
								if (toBeRemoved == null) {
									toBeRemoved = new ArrayList<EObject>();
								}
								toBeRemoved.add(lobj);
							}
							else if (eobj.equals(robj)) {
								//((GraphPatternElement)maybeGpe).setRightImpliedPropertyUsed(validateNamedNode(new NamedNode(implProp.getURI(), NodeType.PropertyNode)));
								//set the implied property node for the underlying named node  instead of GPE
	
								if(maybeGpe instanceof TripleElement) {
									NamedNode object = (NamedNode) ((TripleElement) maybeGpe).getObject();
									addLocalizedTypeToNode(object, lPropTci);
									object.setImpliedPropertyNode(impliedPropertyNode);						

								}else {
									//right is the second argument of a BuildinElement
									List<Node> args = ((BuiltinElement) maybeGpe).getArguments();
									if(args.get(1) instanceof NamedNode) {
									addLocalizedTypeToNode(((NamedNode) args.get(1)), lPropTci);
									((NamedNode) args.get(1)).setImpliedPropertyNode(impliedPropertyNode);					

									}else if (args.get(1) instanceof ProxyNode) {
										GraphPatternElement lGPE = ((ProxyNode)args.get(1)).getProxyFor();
										if(lGPE instanceof BuiltinElement) {
											BuiltinElement bie = (BuiltinElement) lGPE;
											bie.setImpliedPropertyNode(impliedPropertyNode);
										}else if(lGPE instanceof TripleElement){
											NamedNode predicate = (NamedNode) ((TripleElement) lGPE).getPredicate();
				        					addLocalizedTypeToNode(((NamedNode) predicate), lPropTci);
											predicate.setImpliedPropertyNode(impliedPropertyNode);
										}
									}
								}
								
								matched = true;
								if (toBeRemoved == null) {
									toBeRemoved = new ArrayList<EObject>();
								}
								toBeRemoved.add(robj);
							}
						} catch (URISyntaxException | IOException | ConfigurationException | DontTypeCheckException
								| CircularDefinitionException | CircularDependencyException
								| PropertyWithoutRangeException | InvalidNameException | InvalidTypeException e) {
							e.printStackTrace();
						}
					}
					if (!matched) {
						// throw new TranslationException("EObject key of impliedProperty is neither
						// left nor right of binary operation for any implied Property");
						int i = 0;
					}
					// TODO must add implied properties to rules, tests, etc.
				} 
				else if (maybeGpe == null && getTarget() != null) {
					if (getTarget() instanceof Test) {
						// the two sides of the test are treated as if they were two sides of an "is" BuiltinElement
						BuiltinElement bi = new BuiltinElement();
						bi.setFuncName("is");
						Object lhsArg = ((Test)getTarget()).getLhs();
						Object rhsArg = ((Test)getTarget()).getRhs();
						try {
							bi.addArgument(lhsArg instanceof Node ? (Node)lhsArg : new ProxyNode((GraphPatternElement) lhsArg));
							bi.addArgument(rhsArg instanceof Node ? (Node)rhsArg : new ProxyNode((GraphPatternElement) rhsArg));
							Object result = applyImpliedAndExpandedProperties(binobj, lobj, robj, bi, true);
							if (result instanceof BuiltinElement) {
								((Test)getTarget()).setLhs(((BuiltinElement)bi).getArguments().get(0));
								((Test)getTarget()).setRhs(((BuiltinElement)bi).getArguments().get(1));
								return null;
							}
							else {
								throw new TranslationException("Unexpected error applying implied properties to target Test");
							}
						} catch (InvalidTypeException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
					else {
						addError("implied properties not yet implemented for " + getTarget().getClass().getSimpleName(), binobj);
					}
				}
				else {
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

	private Object[] getDeclarationAndTranslation(Expression expr) throws TranslationException, InvalidNameException {
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
		else if (rexpr instanceof Declaration) {
			return (Declaration) rexpr;
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
			
			if(len == null && maxLen == null && ((Declaration)eObject).getArglist() != null && ((Declaration)eObject).getArglist().size() > 0) {
				int[] result = new int[1];
				result[0] = ((Declaration)eObject).getArglist().size();
				return result;
			}
			
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
		} else if (expr instanceof UnaryExpression && ((UnaryExpression) expr).getExpr() instanceof Declaration) {
			return true;
		} else if (expr instanceof Declaration) {
			return true;
		}
		return false;
	}

	/**
	 * Method to type check a triple's type relative to other triples with variable connections
	 * @param subjeo
	 * @param predeo
	 * @param objeo
	 * @param tr
	 * @param expr
	 * @throws TranslationException
	 * @throws InvalidTypeException
	 * @throws CircularDependencyException
	 * @throws InvalidNameException
	 */
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
							e.printStackTrace();
						}
					}
				}
			} else {
				subj = getOntResource(tr.getSubject());
			}
			Node pnode = tr.getPredicate();
			OntProperty pred = tr.getPredicate() != null ? getTheJenaModel().getOntProperty(tr.getPredicate().toFullyQualifiedString()) : null;
			if (pred == null) {
				if (tr.getPredicate() instanceof VariableNode) {
					if (isContainedByQuery(expr)) {
						return; // variables as property in queries is OK
					}
					addTypeCheckingError("Property '" + ((VariableNode) tr.getPredicate()).toDescriptiveString()
							+ "' is a variable, unable to validate", expr);
				} else if (tr.getPredicate() != null){
					if (tr.getPredicate() instanceof NamedNode && 
							((NamedNode)tr.getPredicate()).getContext() != null && 
							((NamedNode)tr.getPredicate()).getContext() instanceof SadlResource) {
						NamedNode prnode = (NamedNode)tr.getPredicate();
						SadlResource predDecl = getDeclarationExtensions().getDeclaration((SadlResource)((NamedNode)tr.getPredicate()).getContext());
						EObject container = predDecl.eContainer();
						while (container.eContainer() != null && !(container.eContainer() instanceof SadlModel)) {
							container = container.eContainer();
						}
						if (container instanceof SadlModelElement) {
							processModelElement((SadlModelElement) container);
						}
						else {
							processExpression(container);
						}
						eobjectPreprocessed(container);
						pred = tr.getPredicate() != null ? getTheJenaModel().getOntProperty(tr.getPredicate().toFullyQualifiedString()) : null;
					}
					if (pred == null) {
						addTypeCheckingError("Unexpected error finding property '" + tr.getPredicate().toDescriptiveString()
							+ "' in ontology, cannot validate", expr);
					}
				} else {
					addTypeCheckingError("Property not identified", expr);
				}
				if (pred == null) {
					return;
				}
			}
			if (!(tr.getSubject() instanceof VariableNode) || !isContainedByQuery(expr)) {
				try {
					getModelValidator().checkPropertyDomain(getTheJenaModel(), subj, pred, expr, true,
							isCrVar ? varName : null, false, true);
				} 
				catch (CircularDependencyException e) {
					addError(e.getMessage(), subjeo);
				}
			}
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

	protected boolean isContainedByQuery(Expression expr) {
		if (EcoreUtil2.getContainerOfType(expr, QueryStatement.class) != null) {
			return true;
		}
		return false;
	}

	protected boolean isVariableInDeclarationInRuleOrQuery(EObject expr) throws CircularDefinitionException {
		if (expr instanceof Declaration && 
			((Declaration)expr).getType() instanceof SadlSimpleTypeReference &&
			declarationExtensions.getOntConceptType(((SadlSimpleTypeReference)((Declaration)expr).getType()).getType()).equals(OntConceptType.VARIABLE) &&
			(EcoreUtil2.getContainerOfType(expr, QueryStatement.class) != null
			|| EcoreUtil2.getContainerOfType(expr, SelectExpression.class) != null 
			|| EcoreUtil2.getContainerOfType(expr, RuleStatement.class) != null)) {
				return true;
		}
		return false;
	}


	protected boolean isVariableTypeConditionInRuleOrQuery(EObject expression) throws CircularDefinitionException {
		if (EcoreUtil2.getContainerOfType(expression, QueryStatement.class) != null
				|| EcoreUtil2.getContainerOfType(expression, SelectExpression.class) != null 
				|| EcoreUtil2.getContainerOfType(expression, RuleStatement.class) != null) {
			// this is in a rule or query
			if (expression instanceof BinaryOperation &&
					((BinaryOperation)expression).getOp().equals("is") &&
					((BinaryOperation)expression).getRight() instanceof Declaration &&
					((BinaryOperation)expression).getLeft() instanceof Name &&
					declarationExtensions.getOntConceptType(((Name)((BinaryOperation)expression).getLeft()).getName()).equals(OntConceptType.VARIABLE)) {
				return true;
			}
			
		}
		return false;
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
						addTypeCheckingError("Value '" + nodeToString(obj) + "' is not in the range of property '"
								+ rdfNodeToString(pred) + "'", expr);
					}
				} catch (Throwable t) {
					addTypeCheckingError("Value '" + nodeToString(obj) + "' is not in the range of property '"
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
					e.printStackTrace();
				} catch (InvalidNameException e) {
					e.printStackTrace();
				} catch (URISyntaxException e) {
					e.printStackTrace();
				} catch (IOException e) {
					e.printStackTrace();
				} catch (ConfigurationException e) {
					e.printStackTrace();
				} catch (CircularDefinitionException e) {
					e.printStackTrace();
				} catch (PropertyWithoutRangeException e) {
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
						addTypeCheckingError("Value '" + nodeToString(obj) + "' is not in the range of property '"
								+ rdfNodeToString(pred) + "'", expr);
					}
				} catch (Throwable t) {
					addTypeCheckingError("Value '" + nodeToString(obj) + "' is not in the range of property '"
							+ rdfNodeToString(pred) + "'", expr);
				}
			} else {
				if (predNode instanceof NamedNode && isProperty(((NamedNode) predNode).getNodeType())) {
					try {
						TypeCheckInfo ptci = getModelValidator().getTypeInfoFromRange(
								namedNodeToConceptName((NamedNode) predNode),
								getTheJenaModel().getProperty(((NamedNode) predNode).toFullyQualifiedString()), predeo);
						TypeCheckInfo otci = null;
						if (obj instanceof com.ge.research.sadl.model.gp.Literal && 
								((com.ge.research.sadl.model.gp.Literal)obj).getUnits() != null) {
							if (ptci.getTypeCheckType().getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
								return;
							}
						}
						if (otci == null) {
							otci = getModelValidator().getType(objeo);
						}
						List<String> operations = new ArrayList<String>(1);
						operations.add("is");
						if (getModelValidator().compareTypesUsingImpliedPropertiesRecursively(operations, predeo, objeo,
								ptci, otci, ImplicitPropertySide.LEFT)) {
							return;
						}
					} catch (DontTypeCheckException e) {
						e.printStackTrace();
					} catch (InvalidNameException e) {
						e.printStackTrace();
					} catch (URISyntaxException e) {
						e.printStackTrace();
					} catch (IOException e) {
						e.printStackTrace();
					} catch (ConfigurationException e) {
						e.printStackTrace();
					} catch (CircularDefinitionException e) {
						e.printStackTrace();
					} catch (PropertyWithoutRangeException e) {
						e.printStackTrace();
					}
				} else if (predNode instanceof VariableNode) {
					throw new TranslationException("Unhandled condition");
				}
				addTypeCheckingError("Value '" + nodeToString(obj) + "' is not in the range of property '" + rdfNodeToString(pred)
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
					org.apache.jena.rdf.model.Resource listtype = getModelValidator()
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
					org.apache.jena.rdf.model.Resource listtype = getModelValidator()
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
					Node vartype = null;
					if (getTarget() != null) {
						if (getTarget() instanceof Rule) {
							vartype = getLocalRestrictionOnVariableTypeFromRule((Rule) getTarget(), subjNode, predNode, (VariableNode)obj);
						}
					}
					if (vartype == null) {
						vartype = ((VariableNode) obj).getType();
					}
					if (((VariableNode) obj).isList()) {
						isList = true;
					}
//					if (EcoreUtil2.getContainerOfType(objeo, RuleStatement.class) == null && EcoreUtil2.getContainerOfType(objeo, QueryStatement.class) == null) {
					if (vartype != null) {
						checkTripleRange(subjeo, predeo, null, expr, subjNode, predNode, pred, pnodetype, vartype, isList);
					}
//					}
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
							e.printStackTrace();
						} catch (InvalidNameException e) {
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
					poNode.setContext(pf);
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
						e.printStackTrace();
					} catch (URISyntaxException e) {
						e.printStackTrace();
					} catch (IOException e) {
						e.printStackTrace();
					} catch (ConfigurationException e) {
						e.printStackTrace();
					} catch (DontTypeCheckException e) {
						e.printStackTrace();
					} catch (CircularDefinitionException e) {
						e.printStackTrace();
					} catch (PropertyWithoutRangeException e) {
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
		} else if (obj == null) {
			throw new TranslationException("Unexpected error: the object of the triple is not identifiable");
		} else {
			throw new TranslationException("Unexpected error: the object of the triple is not a node of known type ('" + obj.getClass().getCanonicalName() + ")");
		}
	}

	private Node getLocalRestrictionOnVariableTypeFromRule(Rule rule, Node subjNode, Node predNode, VariableNode obj) {
		Node result = null;
		if (rule.getGivens() != null) {
			for (GraphPatternElement gpe : rule.getGivens()) {
				result = getLocalRestrictionOnVariableTypeFromGpe(gpe, subjNode, predNode, obj);
				if (result != null) {
					break;
				}
			}
		}
		if (result == null && rule.getIfs() != null) {
			for (GraphPatternElement gpe : rule.getIfs()) {
				result = getLocalRestrictionOnVariableTypeFromGpe(gpe, subjNode, predNode, obj);
				if (result != null) {
					break;
				}
			}
		}
		return result;
	}

	private Node getLocalRestrictionOnVariableTypeFromGpe(GraphPatternElement gpe, Node subjNode, Node predNode,
			VariableNode obj) {
		Node result = null;
		if (gpe instanceof TripleElement) {
			if (((TripleElement)gpe).getSubject().equals(obj) && ((TripleElement)gpe).getPredicate() instanceof RDFTypeNode) {
				result = ((TripleElement)gpe).getObject();
			}
		}
		else if (gpe instanceof Junction) {
			if (((Junction)gpe).getLhs() instanceof ProxyNode) {
				result = getLocalRestrictionOnVariableTypeFromGpe(((ProxyNode) ((Junction)gpe).getLhs()).getProxyFor(), subjNode, predNode, obj);
			}
			if (result == null && ((Junction)gpe).getRhs() instanceof ProxyNode) {
				result = getLocalRestrictionOnVariableTypeFromGpe(((ProxyNode) ((Junction)gpe).getRhs()).getProxyFor(), subjNode, predNode, obj);
			}
		}
		return result;
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
				addTypeCheckingError("'" + obj.toString() + "' is not in range of property '" + pred.getLocalName() + "'", expr);
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
				addTypeCheckingError("Class '" + rdfNodeToString(obj) + "' is not in the range of property '"
						+ rdfNodeToString(pred) + "'", expr);
			}
		}
	}

	private OntResource getOntResource(Node node) {
		if (node instanceof VariableNode) {
			if (((VariableNode)node).getDefinitions() != null) {
				for (Node defn : ((VariableNode)node).getDefinitions()) {
					if (defn instanceof ProxyNode && ((ProxyNode)defn).getProxyFor() instanceof TripleElement) {
						TripleElement defnTr = (TripleElement) ((ProxyNode)defn).getProxyFor();
						if (defnTr.getSubject().equals(node) && defnTr.getPredicate() instanceof RDFTypeNode && defnTr.getObject() instanceof NamedNode) {
							OntResource restrictionClass = getTheJenaModel().getOntResource((defnTr.getObject()).toFullyQualifiedString());
							if (restrictionClass != null) {
								return restrictionClass;
							}
						}
					}
				}
			}
			if (((VariableNode) node).getType() != null) {
				return getTheJenaModel().getOntResource(((VariableNode) node).getType().toFullyQualifiedString());
			} else {
				return null;
			}
		}
		return getTheJenaModel().getOntResource(node.toFullyQualifiedString());
	}

	protected VariableNode declarationToImplicitVariable(Declaration decl) throws TranslationException, InvalidNameException {
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
		// This method is only called when a function is referenced, not when an equation is declared.
		
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
		Equation eq = getEquation(expr.getName());		// Retrieve the equation if it was declared in this or an imported model.
		builtin.setInModelReferencedEquation(eq);
		if (arglist != null && arglist.size() > 0) {
			List<Object> args = new ArrayList<Object>();
			for (int i = 0; i < arglist.size(); i++) {
				args.add(processExpression(arglist.get(i)));
			}
			if (args != null) {
				int i = 0;
				for (Object arg : args) {
					try {
						builtin.addArgument(nodeCheck(arg));
						if (arg instanceof GraphPatternElement) {
							((GraphPatternElement) arg).setEmbedded(true);
						}
					}
					catch (TranslationException e) {
						addError("Invalid argument ", arglist.get(i));
					}
					i++;
				}
			}
		}
		if (expr.getName() != null) {
			EObject contr = expr.getName().eContainer();
			EList<SadlParameterDeclaration> argtypes = null;
			EList<SadlReturnDeclaration> rtypes = null;
			if (contr instanceof ExternalEquationStatement) {
				argtypes = ((ExternalEquationStatement)contr).getParameter();
				rtypes = ((ExternalEquationStatement)contr).getReturnType();
			}
			else if (contr instanceof EquationStatement) {
				argtypes = ((EquationStatement)contr).getParameter();
				rtypes = ((EquationStatement)contr).getReturnType();
			}
			if (argtypes != null) {
				List<Node> argTypeNodes = new ArrayList<Node>();
				for (SadlParameterDeclaration spd : argtypes) {
					if (spd.getUnknown() != null) {
						ConstantNode typnode = new ConstantNode(SadlConstants.CONSTANT_NONE);
						argTypeNodes.add(typnode);
					}
					else {
						try {
							SadlTypeReference typ = spd.getType();
							EObject augtype = spd.getAugtype();
							if (augtype != null) {
								addError("unhandled parameter augmented data descriptor in processFunction", contr);
							}
							if (typ != null) {
								NamedNode typnode = sadlTypeReferenceToNode(typ);
								if (typnode != null) {
									if (spd.getTypedEllipsis() != null) {
										argTypeNodes.add(createTypedEllipsisNodeFromVariableNode(null, typnode));
									}
									else {
										argTypeNodes.add(typnode);										
									}
								}	
							}
							else if (spd.getEllipsis()!= null) {
								argTypeNodes.add(new UntypedEllipsisNode());
							}
						} catch (JenaProcessorException e) {
							e.printStackTrace();
						}
					}
				}
				builtin.setArgumentTypes(argTypeNodes);
				builtin.setExpectedArgCount(argTypeNodes.size());
			}
			if (rtypes != null) {
				List<Node> rTypeNodes = new ArrayList<Node>();
				for (SadlReturnDeclaration srd : rtypes) {
					if (srd.getUnknown() != null) {
						ConstantNode typnode = new ConstantNode(SadlConstants.CONSTANT_NONE);
						rTypeNodes.add(typnode);
					}
					else {
						try {
							SadlTypeReference typ = srd.getType();
							EObject augtype = srd.getAugtype();
							if (augtype != null) {
								addError("unhandled return augmented data descriptor in processFunction", srd);
							}
							if (typ != null) {
								NamedNode typnode = sadlTypeReferenceToNode(typ);
								if (typnode != null) {
									rTypeNodes.add(typnode);
								}
							}
						} catch (JenaProcessorException e) {
							e.printStackTrace();
						}
					}
				}
				builtin.setReturnTypes(rTypeNodes);
			}
		}
		try {
			getModelValidator().getFunctionTypeCheckInfoAndCheckArguments(expr, expr.getName(), true); // this will do argment checks on function
		} catch (InvalidNameException e) {
			throw e;
		} catch (InvalidTypeException e) {
			throw e;
		} catch (TranslationException e) {
			throw e;
		} catch (Exception e) {
			throw new TranslationException(e.getMessage(), e);
		}	
		return builtin;
	}

	private boolean hasCommonVariableSubject(Object robj) {
		if (robj instanceof TripleElement && 
				((TripleElement) robj).getSubject() instanceof VariableNode
				&& ((TripleElement) robj).getSourceType() != null
				&& ((((TripleElement) robj).getSourceType().equals(TripleSourceType.SPV))
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

	public GraphPatternElement createBinaryBuiltin(String name, Object lobj, Object robj)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		if (name.equals(JunctionType.AND_ALPHA) || name.equals(JunctionType.AND_SYMBOL)
				|| name.equals(JunctionType.OR_ALPHA) || name.equals(JunctionType.OR_SYMBOL)) {
			Junction jct = new Junction();
			jct.setJunctionName(name);
			jct.setLhs(nodeCheck(lobj));
			jct.setRhs(nodeCheck(robj));
			return jct;
		} else if (name.equals("is") && getTarget() instanceof Test) {
			if (lobj instanceof NamedNode && ((NamedNode)lobj).getNodeType().equals(NodeType.InstanceNode) &&
					robj instanceof VariableNode && ((VariableNode)robj).getType() instanceof NamedNode &&
					((NamedNode) ((VariableNode)robj).getType()).getNodeType().equals(NodeType.ClassNode) &&
					((VariableNode)robj).isCRulesVariable()) {
				// the right was a Declaration so this is of the form <inst> is a <class>
				TripleElement te = new TripleElement((Node) lobj, new RDFTypeNode(), ((VariableNode)robj).getType());
				return te;
			}
			else {
				((Test)getTarget()).setLhs(lobj);
				((Test)getTarget()).setRhs(robj);
				((Test)getTarget()).setCompName(name);
				return null;
				}
		} else {
			BuiltinElement builtin = new BuiltinElement();
			builtin.setFuncName(transformOpName(name));
			if (lobj != null) {
				if (lobj instanceof Object[] && ((Object[])lobj).length == 2 && 
						((Object[])lobj)[0] instanceof VariableNode && 
						((Object[])lobj)[1] instanceof GraphPatternElement) {
					builtin.addArgument(nodeCheck(((Object[])lobj)[1]));
					
				}
				else {
					builtin.addArgument(nodeCheck(lobj));
				}
			}
			if (robj != null) {
				if (robj instanceof Object[] && ((Object[])robj).length == 2 && 
						((Object[])robj)[0] instanceof VariableNode && 
						((Object[])robj)[1] instanceof GraphPatternElement) {
					builtin.addArgument(nodeCheck(((Object[])robj)[1]));
					
				}
				else {
					builtin.addArgument(nodeCheck(robj));
				}
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
		if (expr.getConstant().equals(SadlConstants.CONSTANT_KNOWN)) {
			return new ConstantNode(SadlConstants.CONSTANT_KNOWN);
		}
		if (expr.getConstant().equals(SadlConstants.CONSTANT_NONE)) {
			return new ConstantNode(SadlConstants.CONSTANT_NONE);
		}
		return new ConstantNode(expr.getConstant());
	}

	public Object processExpression(Declaration expr) throws TranslationException, InvalidNameException {
		// String nn = expr.getNewName();
		SadlTypeReference type = expr.getType();
		String article = expr.getArticle();
		String ordinal = expr.getOrdinal();
		Object typenode = processExpression(type);
		if (typenode == null) {
			// this is malformed
			addError("Invalid declaration", expr);
			addUndefinedEObject(type);
			return null;
		}
		if (article != null && isInstance(typenode)) {
			addError("An article (e.g., '" + article
					+ "') should not be used in front of the name of an instance of a class.", expr);
		} else if (article != null && isVariable(typenode)
				&& !isContainedByQuery(expr)
				&& EcoreUtil2.getContainerOfType(expr, RuleStatement.class) == null) {
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
			
			if (!isUseArticlesInValidation() && 
					(ordNum > 1 || isDefiniteArticle(article))) {
				// grammar has content only valid if article use is turned on in preferences
				addError("The use of articles is not enabled in preferences but the content is only valid when enabled.", expr);
			}

			if (isUseArticlesInValidation() && !isDefiniteArticle(article) && typenode instanceof NamedNode
					&& (((NamedNode) typenode).getNodeType().equals(NodeType.ClassNode)
							|| ((NamedNode) typenode).getNodeType().equals(NodeType.ClassListNode))) {
				if (!isCruleVariableDefinitionPossible(expr)) {
					if (ordinal != null) {
						addError("Did not expect an indefinite article reference with ordinality in rule conclusion",
								expr);
					}
					else if (!isDefiniteArticle(article) && getRulePart().equals(RulePart.CONCLUSION) && 
							!isClassificationDeclaration(expr) && !isDeclInThereExists(expr)) {
						addError("Did not expect an indefinite article reference in rule conclusion",
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
					processListDeclaration(expr, (NamedNode)typenode);
				}
				try {
					if (expr.eContainer() instanceof BinaryOperation &&
							((BinaryOperation)expr.eContainer()).getOp().equals("is") &&
							(((NamedNode)typenode).getNodeType().equals(NodeType.ClassNode) ||
									((NamedNode)typenode).getNodeType().equals(NodeType.ClassListNode)) &&
							((BinaryOperation)expr.eContainer()).getLeft() instanceof Name &&
							(getDeclarationExtensions().getOntConceptType(((Name)((BinaryOperation)expr.eContainer()).getLeft())).equals(OntConceptType.INSTANCE) ||
							getDeclarationExtensions().getOntConceptType(((Name)((BinaryOperation)expr.eContainer()).getLeft())).equals(OntConceptType.VARIABLE))) {
						// this is of the form "something is a class" where so return the class (typenode)
						//	where the something must be an instance or an explicit variable
						return typenode;
					}
					else if ((!isUseArticlesInValidation() || !isDefiniteArticle(article)) &&
							expr.eContainer() instanceof UnaryExpression &&
							((UnaryExpression)expr.eContainer()).getOp().equals("not") &&
							(((NamedNode)typenode).getNodeType().equals(NodeType.ClassNode) ||
									((NamedNode)typenode).getNodeType().equals(NodeType.ClassListNode))) {
						// this is the negation of a class or class list node, e.g., "not a SomeClass"
						return typenode;
					}
				} catch (CircularDefinitionException e1) {
					e1.printStackTrace();
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
						e.printStackTrace();
					} catch (PrefixNotFoundException e) {
						e.printStackTrace();
					} catch (InvalidNameException e) {
						e.printStackTrace();
					} catch (InvalidTypeException e) {
						e.printStackTrace();
					} catch (ConfigurationException e) {
						e.printStackTrace();
					} catch (DontTypeCheckException e) {
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
		} else if(isDefinitionOfExplicitVariable(expr) && typenode instanceof NamedNode && isList((NamedNode) typenode, type)) {
			processListDeclaration(expr, (NamedNode) typenode);
		}
		else if (typenode instanceof VariableNode) {
			EObject cont = expr.eContainer();
			try {
				if (cont instanceof BinaryOperation && 
						((BinaryOperation)cont).getOp().equals("is") &&
						((BinaryOperation)cont).getLeft() instanceof SadlResource &&
						(getDeclarationExtensions().getOntConceptType((SadlResource)((BinaryOperation)cont).getLeft()).equals(OntConceptType.INSTANCE) ||
								getDeclarationExtensions().getOntConceptType((SadlResource)((BinaryOperation)cont).getLeft()).equals(OntConceptType.VARIABLE))) {
					// the type is a variable so must be of form instance is a variable
					TypeCheckInfo varTCI = getModelValidator().getType((SadlResource)((BinaryOperation)cont).getLeft());
					((VariableNode)typenode).setType(varTCI.getTypeCheckType());
				}
			} catch (Exception e) {
				addError(e.getMessage(), cont);
			} 
		}
		return typenode;
	}

	private boolean isClassificationDeclaration(Declaration expr) {
		if (expr.eContainer() instanceof BinaryOperation && 
				((BinaryOperation)expr.eContainer()).getOp().equals("is") &&
				((BinaryOperation)expr.eContainer()).getRight().equals(expr)) {
			return true;
		}
		return false;
	}

	protected void processListDeclaration(Declaration aExpression, NamedNode aListNode) throws InvalidNameException, TranslationException {
		aListNode.setList(true);
		if(aExpression.getArglist() != null && aExpression.getArglist().size() > 0) {
			for(Expression lArgument : aExpression.getArglist()) {
				try {
					aListNode.addListLiteral(processExpression(lArgument));
				} catch (InvalidTypeException e) {
					logger.error(e.getMessage());
				}
			}
			aListNode.setListLength(aExpression.getArglist().size());
			
		} else if (aExpression.getLen() != null) {
			if (aExpression.getMaxlen() != null) {
				aListNode.setMinListLength(Integer.parseInt(aExpression.getLen()));
				if (!aExpression.getMaxlen().equals("*")) {
					aListNode.setMaxListLength(Integer.parseInt(aExpression.getMaxlen()));
				}
			} else {
				aListNode.setListLength(Integer.parseInt(aExpression.getLen()));
			}
		}
		try {
			addLocalizedTypeToNode(aListNode,getModelValidator().getType(aExpression));
		} catch (CircularDefinitionException | InvalidNameException | URISyntaxException
				| IOException | ConfigurationException | InvalidTypeException | CircularDependencyException
				| PropertyWithoutRangeException e) {
			e.printStackTrace();
		} catch (DontTypeCheckException e) {
			//this is acceptable
		}
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

	public boolean isProperty(Object node) throws TranslationException {
		if (node instanceof NamedNode) {
			return isProperty(((NamedNode) node).getNodeType());
		}
		else if (node instanceof ConceptName) {
			return isProperty(conceptTypeToNodeType(((ConceptName)node).getType()));
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
		if (expr.eContainer() != null && expr.eContainer() instanceof UnaryExpression && 
				((UnaryExpression)expr.eContainer()).getOp().equals("not")) {
			return false;
		}
		return true;
	}

	private boolean isDefinitionOfExplicitVariable(Declaration expr) {
		EObject cont = expr.eContainer();
		try {
			if (cont instanceof BinaryOperation && ((BinaryOperation)cont).getOp().equals("is")) {
				if (((BinaryOperation) cont).getLeft() instanceof SadlResource && 
						getDeclarationExtensions().getOntConceptType((SadlResource) ((BinaryOperation) cont).getLeft()).equals(OntConceptType.VARIABLE)) {
					// of form variable is a class
					return true;
				}
				else if (((BinaryOperation)cont).getRight() instanceof Declaration && 
						((Declaration)((BinaryOperation)cont).getRight()).getType() instanceof SadlSimpleTypeReference &&
						((SadlSimpleTypeReference)((Declaration)((BinaryOperation)cont).getRight()).getType()).getType() instanceof SadlResource &&
						getDeclarationExtensions().getOntConceptType((SadlResource)((SadlSimpleTypeReference)((Declaration)((BinaryOperation)cont).getRight()).getType()).getType()).equals(OntConceptType.VARIABLE)) {
					// of form instance is a variable (Note: grammar doesn't identify this as the declaration of the variable because it can't--the variable is an explicit reference
					return true;
				}
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

	protected Object processExpression(SadlTypeReference type) throws TranslationException {

		if (type instanceof SadlSimpleTypeReference) {
			Object typeobj = processExpression(((SadlSimpleTypeReference) type).getType());
			if (((SadlSimpleTypeReference) type).isList()) {
				if (typeobj instanceof NamedNode) {
					((NamedNode) typeobj).setList(true);
				} else {
					if (typeobj != null) {
						addError("Unhandled case: type returned from simple type reference is not type NamedNode", type);
					}
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
		return validateNamedNode(new NamedNode(XSD.getURI() + typeStr, NodeType.DataTypeNode));
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
	

	public Object processExpression(Name aExpr) throws TranslationException, InvalidNameException, InvalidTypeException {
		
		// check if the expression is a function
		if (aExpr.isFunction()) {
			return processFunction(aExpr);
		}
		
		SadlResource lQName = aExpr.getName();
		String lName = getDeclarationExtensions().getConcreteName(lQName);
		
		// make sure we have a name
		if (lName == null) {
			
			SadlResource srnm = lQName.getName();
			if (srnm != null) {
				return processExpression(srnm);
			}
			else {
				srnm = aExpr;
				lName = getDeclarationExtensions().getConcreteName(srnm);
				if (lName != null) {
					return processExpression(srnm);
				}
			}
			addError(SadlErrorMessages.TRANSLATE_NAME_SADLRESOURCE.toString(), aExpr);
		}
		else if (getTarget() != null && getTarget() instanceof Rule && 
				((Rule)getTarget()).getVariable(getModelNamespace() + lName) != null) {
			VariableNode var = ((Rule)getTarget()).getVariable(getModelNamespace() + lName);
			Property ip = OntModelProvider.getImpliedProperty(getCurrentResource(), aExpr);
			if (ip != null) {
				NamedNode propnn = new NamedNode(ip.getURI());
				NodeType ptype;
				if (ip instanceof DatatypeProperty) {
					ptype = NodeType.DataTypeProperty;
				}
				else if (ip instanceof ObjectProperty) {
					ptype = NodeType.ObjectProperty;
				}
				else if (ip instanceof AnnotationProperty) {
					ptype = NodeType.AnnotationProperty;
				}
				else {
					ptype = NodeType.PropertyNode;
				}
				propnn.setNodeType(ptype);
				propnn = validateNamedNode(propnn);
				TripleElement tr = new TripleElement(var, propnn, null);
				// remove so it isn't applied again
				getModelValidator().removeImpliedPropertyUsed(aExpr);
				return tr;
			}
			else {
				return var;
			}
		}			// make sure the right side is defined for binary operation
		else if (lQName.equals(aExpr) && aExpr.eContainer() instanceof BinaryOperation
				&& ((BinaryOperation) aExpr.eContainer()).getRight() != null
				&& ((BinaryOperation) aExpr.eContainer()).getRight().equals(lQName)) {

			addError("It appears that '" + lName + "' is not defined.", aExpr);

			// make sure the left side is defined for binary operation
		} else if (lQName.equals(aExpr) && aExpr.eContainer() instanceof BinaryOperation
				&& ((BinaryOperation) aExpr.eContainer()).getLeft() != null
				&& ((BinaryOperation) aExpr.eContainer()).getLeft().equals(lQName)) {

			addError("It appears that '" + lName + "' is not defined.", aExpr);
		} else {
			return processExpression(lQName);
		}
		
		// fall out to null
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
		
        boolean specialCntIdxProcessing = false;
        boolean lIsConstantExpression = false;
        
        boolean predicateIsNegatedType = false;
        if (predicate instanceof UnaryExpression && 
        		((UnaryExpression)predicate).getOp().contentEquals("not") &&
        		((UnaryExpression)predicate).getExpr() instanceof Constant &&
        		((Constant)((UnaryExpression)predicate).getExpr()).getConstant().equals("a type")) {
        	// negated
        	predicateIsNegatedType = true;
        	predicate = ((UnaryExpression)predicate).getExpr();
        }
        
        	
		if (predicate instanceof Constant) {
			// this is a pseudo PropOfSubject; the predicate is a constant
			lIsConstantExpression = true;
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
				
                //Check if we need to do special processing
                if (expr instanceof PropOfSubject && ((PropOfSubject) expr).getOf() != "in") {
                    specialCntIdxProcessing = true;
                }

			} else if (cnstval.endsWith("index")) {
				constantBuiltinName = cnstval;
				numBuiltinArgs = 2;
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject) subject).getLeft();
					subject = ((PropOfSubject) subject).getRight();
				}
				
				//Can't contain further constants
				if(predicate instanceof Constant || subject instanceof Constant) {
					addError(SadlErrorMessages.INVALID_ARGUMENT_FOR_OPERATOR.get(cnstval), expr);
				}
				
                //Check if we need to do special processing
                if (expr instanceof PropOfSubject && ((PropOfSubject) expr).getOf() != "in") {
                    specialCntIdxProcessing = true;
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
			} else if (cnstval.equals("a type")) {
				trSubj = processExpression(subject);
				trPred = new NamedNode(RDFS.subClassOf.getURI());  // new RDFTypeNode();
				((NamedNode)trPred).setContext(predicate);
				((NamedNode)trPred).setNodeType(NodeType.ObjectProperty);
				TripleElement tr = new TripleElement((Node)null, (Node)trPred, (Node)trSubj);
				if (predicateIsNegatedType) {
					tr.setType(TripleModifierType.Not);
				}
				return tr;
			} else if (cnstval.equals("value") && getTarget() instanceof Query) {
				// this is to be a delayed evaluation: don't use the expression as the query but evaluate
				// the expression at runtime and use the value as the query
				Query qry = (Query) getTarget();
				qry.setToBeEvaluated(true);
				return processExpression(subject);
			} else {
				System.err.println("Unhandled constant property in translate PropOfSubj: " + cnstval);
			}
		} else if (predicate instanceof ElementInList) {
			trSubj = processExpression(subject);
//			trPred = processExpression(predicate);
			Expression element = ((ElementInList)predicate).getElement();
			Object elObj = processExpression(element);
            
            if(elObj instanceof NamedNode) {
                //check for implied properties
                Map<EObject, Property> ip = getModelValidator().getImpliedPropertiesUsed();
                if(ip != null) {
                    Iterator<EObject> ipitr = ip.keySet().iterator();
                    while (ipitr.hasNext()) {
                        EObject eobj = ipitr.next();
                        Property implProp = ip.get(eobj);
                        TypeCheckInfo lPropTci;
						try {
							lPropTci = getModelValidator().getType((EObject)eobj);
							TypeCheckInfo ipNN = getMatchingImpliedPropertyBaseTci(lPropTci, implProp, eobj);
							NamedNode impliedPropertyNode = (validateNamedNode(new NamedNode(implProp.getURI(), NodeType.PropertyNode)));
        					addLocalizedTypeToNode(impliedPropertyNode, ipNN);
        					
	                        if (eobj.equals(predicate)) {
	        					addLocalizedTypeToNode(((NamedNode) elObj), lPropTci);
	                            ((NamedNode) elObj).setImpliedPropertyNode(impliedPropertyNode);
	                        }
						} catch (URISyntaxException | IOException | ConfigurationException | DontTypeCheckException
								| CircularDefinitionException | CircularDependencyException
								| PropertyWithoutRangeException e) {
							e.printStackTrace();
						}

                    }	
                }
            }


			
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
			if (trSubj == null) {
				tryToAddUndefinedEObject(subject);
			}
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
			if (trPred == null) {
				tryToAddUndefinedEObject(predicate);
			}

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
			} else if (trPred instanceof GraphPatternElement){
				predNode = new ProxyNode((GraphPatternElement) trPred);
			}
			else {
				throw new TranslationException("Predicate is neither Node nor GraphPatternElement: " + trPred.getClass().getCanonicalName());
			}
			if (trSubj instanceof Node) {
				subjNode = (Node) trSubj;
				if (trSubj instanceof NamedNode && isProperty(((NamedNode)subjNode).getNodeType())) {
					// this needs another TripleElement
					trSubj = new TripleElement(null, (Node) trSubj, null);
					subjNode = nodeCheck(trSubj);
				}
			} else if (trSubj instanceof GraphPatternElement) {
				subjNode = new ProxyNode((GraphPatternElement) trSubj);
			}
			if (subjNode == null) {
				addError("Subject is neither Node nor GraphPatternElement", subject);
			}
			if (predNode != null && predNode instanceof Node) {
				//Add range information to predNode based on domain restriction
				try {
					EObject lTciExpression = expr;
					if(lIsConstantExpression) {
						lTciExpression = expr.getRight();
					}
					TypeCheckInfo lTci = getModelValidator().getType(lTciExpression);
					addLocalizedTypeToNode(predNode,lTci);
					
				} catch (DontTypeCheckException e) {
					// do nothing
				} catch (PropertyWithoutRangeException e) {
					getModelValidator().handleValidationException(predicate, e);
				} catch (URISyntaxException | IOException | ConfigurationException
						| CircularDefinitionException | CircularDependencyException e) {
					addError(e.getMessage(), subject);
				}
				returnTriple = new TripleElement(subjNode, predNode, null);
				returnTriple.setSourceType(TripleSourceType.PSV);
				returnTriple.setContext(expr);
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
			if (constantBuiltinName == null) {
//				addError("Unable to process predicate name", predicate);
				return null;
			}
			GraphPatternElement bi = null;
			if (numBuiltinArgs == 1) {
				bi = new BuiltinElement();
				((BuiltinElement) bi).setFuncName(constantBuiltinName);
				((BuiltinElement) bi).addArgument(nodeCheck(trSubj));
			} else {
				bi = createBinaryBuiltin(constantBuiltinName, nodeCheck(trSubj), trPred);
			}
            
            // special processing for index/count on <> of <> in <> of <> 
            if(specialCntIdxProcessing) {
                
                // de-construct the elements from the previous builtinElement
                // then build correctly formed builtinElement
                List<Node> larguements = ((BuiltinElement) bi).getArguments();
                if(larguements.size() < 2) {
                    // form is not what we are expecting, return old buildinElement
                    return combineRest(bi, rest);
                }
                
                Object arguement1 = larguements.get(0);
                if(arguement1 instanceof ProxyNode) {
                    ProxyNode leftBi = (ProxyNode) arguement1;
                    Object baseObject = leftBi.getProxyFor();
                    
                    if(baseObject != null && baseObject instanceof TripleElement) {
                        TripleElement baseObjectTe = (TripleElement) baseObject;
                        
                        if(baseObjectTe.getSubject() != null && baseObjectTe.getSubject() instanceof ProxyNode) {
                            // build the left and right sides of the new builtinElement
                            ProxyNode modifiedSubject = (ProxyNode) baseObjectTe.getSubject();
                            TripleElement modifiedPredicate = new TripleElement();
    
                            NamedNode rObjectSubject = (NamedNode) baseObjectTe.getPredicate();                        
                            ((TripleElement) modifiedPredicate).setSubject(rObjectSubject);
                            
                            NamedNode rObjectPredicate = (NamedNode) larguements.get(1);
                            ((TripleElement) modifiedPredicate).setPredicate(rObjectPredicate);
                            
                            // create and returned the correctly formed builtinElement
                            GraphPatternElement formedBi = createBinaryBuiltin(constantBuiltinName, nodeCheck(modifiedSubject), 
                                    nodeCheck(modifiedPredicate));
                            return combineRest(formedBi, rest);
                        }
                    }
                }
            }

			
			return combineRest(bi, rest);
		}
	}

	private void tryToAddUndefinedEObject(EObject eobj) {
//		if (eobj instanceof SadlResource) {
			addUndefinedEObject(eobj);
//		}
//		else if (eobj instanceof Declaration && ((Declaration)eobj).getType() instanceof SadlSimpleTypeReference) {
//			addUndefinedEObject(((Declaration)eobj).getType());
//		}
	}

	protected void addUndefinedEObject(EObject eobj) {
		if (!undefinedEObjects.contains(eobj)) {
			undefinedEObjects .add(eobj);
		}
	}

	protected List<EObject> getUndefinedEObjects() {
		return undefinedEObjects;
	}
	
	protected void clearUndefinedEObjects() {
		undefinedEObjects.clear();
	}

	protected void addLocalizedTypeToNode(Node predNode, TypeCheckInfo lTci) throws TranslationException {
		if(predNode instanceof NamedNode && lTci != null) {
			if(isIgnoreUnittedQuantities() && lTci.getTypeCheckType() != null && 
					lTci.getTypeCheckType().getURI() != null &&
					lTci.getTypeCheckType().getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
				((NamedNode) predNode).setLocalizedType(validateNamedNode(new NamedNode(XSD.decimal.getURI(),NodeType.DataTypeNode)));
			}else {
				((NamedNode) predNode).setLocalizedType(lTci.getTypeCheckType());
			}
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
			org.apache.jena.rdf.model.Resource sr = stmt.getSubject();
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

	private boolean checkForSubClassCardinalityExistence(org.apache.jena.rdf.model.Resource sr, OntClass subjClass,
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

	private boolean checkForCardinalityExistence(org.apache.jena.rdf.model.Resource sr, OntClass subjClass,
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
		if (gpe == null) {
			if (rest instanceof List<?>) {
				return listToSingleJunction((List<GraphPatternElement>) rest);
			} else {
				return rest;
			}
		}
		else {
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
			else if (((Object[])result)[0] instanceof GraphPatternElement &&
					((Object[])result)[1] instanceof GraphPatternElement) {
				List<GraphPatternElement> lst = new ArrayList<GraphPatternElement>();
				// is there any circumstance where we don't want to put "rest" before "result"?
				lst.add((GraphPatternElement) ((Object[])result)[1]);
				lst.add((GraphPatternElement) ((Object[])result)[0]);
				return listToSingleJunction(lst);
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
		checkForInvalidAnnotation(expr);
		String nm = getDeclarationExtensions().getConcreteName(expr);
		String uri = getDeclarationExtensions().getConceptUri(expr);
		String ns = getDeclarationExtensions().getConceptNamespace(expr);
		String prfx = getDeclarationExtensions().getConceptPrefix(expr);
//		if (nm.indexOf(':') > 0) {
//			// in this case the returned name has a prefix (must be an import) so get the name from the declaration
//			nm = getDeclarationExtensions().getConcreteName(getDeclarationExtensions().getDeclaration(expr));
//		}
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
				e.printStackTrace();
			} catch (PrefixNotFoundException e) {
				e.printStackTrace();
			} catch (InvalidNameException e) {
				e.printStackTrace();
			} catch (InvalidTypeException e) {
				e.printStackTrace();
			} catch (ConfigurationException e) {
				e.printStackTrace();
			}
			return vn;
		} else if (nm != null) {
			NamedNode n = new NamedNode(uri, ontConceptTypeToNodeType(type));
			n.setContext(expr);
			n.setNamespace(ns);
			n.setPrefix(prfx);
			try {
				addLocalizedTypeToNode(n,getModelValidator().getType(expr));
			} catch (CircularDefinitionException | InvalidNameException | URISyntaxException
					| IOException | ConfigurationException | InvalidTypeException | CircularDependencyException e) {
				e.printStackTrace();
			} catch (PropertyWithoutRangeException e) {
				addPropertyWithoutRangeError(expr, null, e);
			} catch (DontTypeCheckException e) {
				//this is acceptablee
			}
			return n;
		}
		return null;
	}

	private void checkForInvalidAnnotation(SadlResource sr) {
		if (sr != null) {
			EList<SadlAnnotation> anns = sr.getAnnotations();
			if (anns != null && anns.size() > 0) {
				SadlResource dsr = declarationExtensions.getDeclaration(sr);
				if (!dsr.equals(sr)) {
					String sruri = declarationExtensions.getConceptNamespace(sr);
					if (sruri != null && !sruri.equals(getModelNamespace())) {
						addError("Annotations are only allowed in the model where a concept is defined", sr);
					}
					else {
						OntResource rsrc = getTheJenaModel().getOntResource(sruri);
						if (rsrc != null) {
							addAnnotationsToResource(rsrc, anns);
						}
					}
				}
			}

		}
	}

	protected Object processSubjHasPropUnitExpression(SubjHasProp expr)
			throws InvalidNameException, InvalidTypeException, TranslationException {
		Expression valexpr = expr.getLeft();
		Object valarg = processExpression(valexpr);
		if (isIgnoreUnittedQuantities()) {
			return valarg;
		}
		String unit = SadlASTUtils.getUnitAsString(expr);
		if (valarg instanceof com.ge.research.sadl.model.gp.Literal) {
			((com.ge.research.sadl.model.gp.Literal) valarg).setUnits(unit);
			return valarg;
		}
		com.ge.research.sadl.model.gp.Literal unitLiteral = new com.ge.research.sadl.model.gp.Literal();
		unitLiteral.setValue(unit);
		if (!(valarg instanceof Node || valarg instanceof GraphPatternElement)) {
			// this is the kind of error that happens with invalid syntax that makes a construct that appears like a unitted quantity
			addError("Invalid syntax", expr);
			return valarg;
		}
		else {
			return createBinaryBuiltin("unittedQuantity", valarg, unitLiteral);
		}
	}

	public Object processExpression(SubjHasProp expr)
			throws InvalidNameException, InvalidTypeException, TranslationException, CircularDefinitionException, IOException, PrefixNotFoundException, ConfigurationException {
		// System.out.println("processing " + expr.getClass().getCanonicalName() + ": "
		// + expr.getProp().toString());
		if (expr.isComma() && expr.getRight() == null) {
			addError("This is an invalid subject-has-property construct using a comma", expr);
			return null;
		}
		if (expr.getRight() == null && expr.getLeft() instanceof Declaration &&
				getDeclarationExtensions().getOntConceptType(expr.getProp()).equals(OntConceptType.VARIABLE)) {
			// this is an in-line variable declaration
			String vname = getDeclarationExtensions().getConcreteName(expr.getProp());
			VariableNode vn = getVariable(vname);
			if (vn == null) {
				vn = createVariable(expr.getProp());
			}
			return vn;
		}
		Expression subj = expr.getLeft();
		SadlResource pred = expr.getProp();
		Expression obj = expr.getRight();
		List<GraphPatternElement> shpTriples = new ArrayList<GraphPatternElement>();
		shpTriples = processSubjHasProp(subj, pred, obj, null, shpTriples, expr);

		if (shpTriples.size() > 0 && shpTriples.get(0) instanceof TripleElement) {
			return combineRest(((TripleElement) shpTriples.get(0)).getSubject(), shpTriples);
		}
		return shpTriples;
	}

	/**
	 * Method to process SubjHasProp expression in the context of a possible list of triples
	 * @param subj--subject expression
	 * @param pred--predicate expression
	 * @param obj--object expression
	 * @param objUnit--unit on the object if a unitted quatity
	 * @param shpTriples--the list of triples
	 * @param expr--the host expression for error reporting
	 * @return
	 * @throws InvalidNameException
	 * @throws InvalidTypeException
	 * @throws TranslationException
	 */
	private List<GraphPatternElement> processSubjHasProp(Expression subj, SadlResource pred, Expression obj,
			String objUnit, List<GraphPatternElement> shpTriples, Expression expr)
					throws InvalidNameException, InvalidTypeException, TranslationException {
		// Create a triple for this SubjHasProp and put it in the list (only has
		// predicate at this point)
		Object predObj = processExpression(pred);
		TripleElement tr = new TripleElement();
		tr.setPredicate(nodeCheck(predObj));
		tr.setContext(pred.eContainer());

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
				boolean thisSubjHasPropIsUnitsOnly = false;
				String unit = null;
				try {
					// If the current SubjHasProp has right (obj) null and prop type is variable (not really a SadlResource)
					//	and obj of the informingTriple is a Number literal
					//	then the prop is a unit to be placed on the Number literal 
					if (obj == null && ((SubjHasProp) subj).getRight() instanceof NumberLiteral &&
							pred instanceof SadlResource &&
							declarationExtensions.getOntConceptType(pred).equals(OntConceptType.VARIABLE)) {
						thisSubjHasPropIsUnitsOnly = true;
						unit = declarationExtensions.getConcreteName(pred);
					}
				} catch (CircularDefinitionException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				int preCallListSize = shpTriples.size();
				shpTriples = processSubjHasProp(((SubjHasProp) subj).getLeft(), ((SubjHasProp) subj).getProp(),
						((SubjHasProp) subj).getRight(), unit, shpTriples, subj);
				// must get subject of current triple; should be the subject of the first triple
				// added by call on previous line
				if (shpTriples.size() > preCallListSize) {
					if (shpTriples.get(preCallListSize) instanceof TripleElement) {
						TripleElement informingTriple = (TripleElement) shpTriples.get(preCallListSize);
						tr.setSubject(informingTriple.getSubject());
						if (informingTriple.getPredicate() == null && informingTriple.getObject() == null) {
							shpTriples.set(preCallListSize, tr);
						}
						else if (thisSubjHasPropIsUnitsOnly) {
							tr = (TripleElement) shpTriples.get(shpTriples.size() - 1);	// the last element of list
						}
						else {
							shpTriples.add(tr);
						}
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
				} else if (subjObj != null) {
					if (subjObj instanceof TripleElement) {
						((TripleElement)subjObj).setEmbedded(true);
					}
					tr.setSubject(nodeCheck(subjObj));
					shpTriples.add(tr);
					subjectFound = true;
				}
			}
			// if (!subjectFound) {
			// throw new TranslationException(
			// "Unhandled SubjHasProp subject type: " + subj.getClass().getCanonicalName());
			// }
		}
		if (tr.getObject() == null) {
			boolean objectFound = false;
			if (obj instanceof SubjHasProp) {
				// TODO could this be a unit only?
				int preCallListSize = shpTriples.size();
				shpTriples = processSubjHasProp(((SubjHasProp) obj).getLeft(), ((SubjHasProp) obj).getProp(),
						((SubjHasProp) obj).getRight(), null, shpTriples, obj);
				// must get object of current triple; should be the subject of the first triple
				// added by call on previous line
				if (shpTriples.size() > preCallListSize) {
					if (shpTriples.get(preCallListSize) instanceof TripleElement) {
						TripleElement informingTriple = (TripleElement) shpTriples.get(preCallListSize);
						tr.setObject(informingTriple.getSubject());
						objectFound = true;
					}
				}
			} else if (obj == null) {
				if (EcoreUtil2.getContainerOfType(expr, EquationStatement.class) != null && subj instanceof Declaration && tr.getSubject() instanceof VariableNode && predObj instanceof VariableNode) {
					VariableNode unDeclaredTypedVariable = (VariableNode) tr.getSubject();
					VariableNode declaredTypedVariable = (VariableNode) predObj;
					if (getCurrentEquation() != null) {
						getCurrentEquation().addVariable(declaredTypedVariable);
					}
					try {
						declaredTypedVariable.setType(unDeclaredTypedVariable.getType());
					}
					catch (Exception e) {
						addError(e.getMessage(), pred);
					}
					tr.setSubject(declaredTypedVariable);
					tr.setPredicate(null);
					return shpTriples;
				}
				else if (subj.eContainer() instanceof SubjHasProp && 
						subj.eContainer().eContainer() instanceof BinaryOperation) {
//					if (getTarget() == null) {
//						addError("Triple pattern as part of binary operation is incomplete", subj.eContainer());
//					}
					return shpTriples;
				}
				else {
					addError("Object for triple is null, which was not expected.", obj);
				}
			} else {
				Object objObj = processExpression(obj);
				if (objObj instanceof Node) {
					if (objUnit != null) {
						if (objObj instanceof com.ge.research.sadl.model.gp.Literal) {
							((com.ge.research.sadl.model.gp.Literal)objObj).setUnits(objUnit);
						}
						else {
							addWarning("Trying to set units of a non-numeric value", obj);
						}
					}
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
				addError("Unable to find object for triple", obj);
			}
		}
		try {
			validateTripleTypes(subj, pred, obj, tr, expr);
		} catch (CircularDependencyException e) {
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
					addTypeCheckingError("Type check info doesn't have expected ConceptName type", expr);
				}
			} else {
				addTypeCheckingError("Type check info doesn't have valid type", expr);
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
							addTypeCheckingError("Unexpected problem in compoundTypeCheckTypeToNode", expr);
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
		Object eobj = expr.getExpr() != null ? processExpression(expr.getExpr()) : null;
		if (eobj == null) {
			return null;
		}
		// if (eobj instanceof VariableNode && ((VariableNode)eobj).isCRulesVariable()
		// && ((VariableNode)eobj).getType() != null) {
		// TripleElement trel = new TripleElement((VariableNode)eobj, new RDFTypeNode(),
		// ((VariableNode)eobj).getType());
		// trel.setSourceType(TripleSourceType.SPV);
		// eobj = trel;
		// }
		String op = expr.getOp();
		Object val = null;
		if (eobj instanceof ConstantNode && 
				(((ConstantNode)eobj).getName().equals("PI") || ((ConstantNode)eobj).getName().equals("e"))) {
			eobj = ITranslator.constantToLiteral((ConstantNode)eobj);
		}
		if (eobj != null && eobj instanceof com.ge.research.sadl.model.gp.Literal) {
			val = ((com.ge.research.sadl.model.gp.Literal) eobj).getValue();
		}
		if (op.equals("-") && val instanceof Number) {
			if (val instanceof BigDecimal) {
				val = ((BigDecimal) val).negate();
			} else if (val instanceof Double) {
				val = -1.0 * ((Double) val).doubleValue();
			} else if (val instanceof Float) {
				val = -1.0 * ((Float)val).floatValue();
			}
			else if (val instanceof Long) {
				val = -1 * ((Long)val).longValue();
			}
			else if (val instanceof Integer) {
				val = -1 * ((Integer)val).intValue();
			}
			else {
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
			bi.addArgument(new ProxyNode((GraphPatternElement) eobj));
		} else if (eobj instanceof Object[] && ((Object[])eobj).length == 2 && ((Object[])eobj)[0] instanceof NamedNode) {
			bi.addArgument((Node) ((Object[])eobj)[0]);
			Junction jct = new Junction();
			jct.setJunctionName("and");
			jct.setLhs(SadlModelProcessor.nodeCheck(bi));
			jct.setRhs(SadlModelProcessor.nodeCheck(((Object[])eobj)[1]));
			return jct;
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

	protected EObject getOperationPullingUp() {
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
		if (isIgnoreUnittedQuantities()) {
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
		if (rsrc == null && smas != null) {
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
				cls = createOntClass(getDeclarationExtensions().getConceptUri(sr), (String) null, null);
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
		} else if (isProperty(sameAsType)) {
			OntProperty smasProp = sadlTypeReferenceToOntResource(smas).asProperty();
			if (smasProp == null) {
				addError("Unable to find property", smas);
			}
			else {
				OntProperty prop = getTheJenaModel().getOntProperty(uri);
				if (prop == null) {
					if (sameAsType.equals(OntConceptType.CLASS_PROPERTY)) {
						prop = createObjectProperty(uri, null);
					}
					else if (sameAsType.equals(OntConceptType.DATATYPE_PROPERTY)){
						prop = createDatatypeProperty(uri, null);
					}
					else {
						prop = createRdfProperty(uri, null);
					}
					// set domain and range to the same as smasProp
					StmtIterator ditr = getTheJenaModel().listStatements(smasProp, RDFS.domain, (RDFNode)null);
					while (ditr.hasNext()) {
						prop.addDomain(ditr.nextStatement().getObject().asResource());
					}
					StmtIterator ritr = getTheJenaModel().listStatements(smasProp, RDFS.range, (RDFNode)null);
					while (ritr.hasNext()) {
						RDFNode rn = ritr.nextStatement().getObject();
						if (rn.isResource()) {
							prop.addRange(rn.asResource());
						}
						else {
							addWarning("Unable to set range to equivalent property", sr);
						}
					}
				}
				smasProp.addEquivalentProperty(prop);
			}
		} else if (sameAsType.equals(OntConceptType.VARIABLE)) {
			// do nothing--this will happen when the same as isn't complete
			
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
					redeclarationHandler(sr, decl);
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
			if (superSR != null && superSRUri == null) {
				if (element.getOftype() != null && element.getOftype().equals(SadlConstants.OF_TYPE_INSTANCES)) {
					addError(SadlErrorMessages.OF_CLASS_NOT_FOUND.get("class"), superElement);
				}
				else {
					addError(SadlErrorMessages.OF_CLASS_NOT_FOUND.get("superclass"), superElement);
				}
			}
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
					OntResource or;
					if (element.getOftype() != null && element.getOftype().equals(SadlConstants.OF_TYPE_INSTANCES)) {
						or = createIndividual(newNames.get(i), getOrCreateOntClass(superSRUri));
					}
					else {
						or = createOntClass(newNames.get(i), superSRUri, superSR);
					}
					if (nmanns != null && nmanns.get(newNames.get(i)) != null) {
						addAnnotationsToResource(or, nmanns.get(newNames.get(i)));
					}
					rsrcList.add(or);
				}
			} else if (superElementType.equals(OntConceptType.CLASS_LIST)
					|| superElementType.equals(OntConceptType.DATATYPE_LIST)) {
				for (int i = 0; i < newNames.size(); i++) {
					rsrcList.add(getOrCreateListSubclass(newNames.get(i), superSRUri, superSR.eResource(), element.getFacet()));
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
				org.apache.jena.rdf.model.Resource spdt = getSadlPrimitiveDataTypeResource(
						(SadlPrimitiveDataType) superElement);
				rsrcList.add(getOrCreateListSubclass(newNames.get(0), spdt.getURI(), superElement.eResource(), element.getFacet()));
			} else {
				org.apache.jena.rdf.model.Resource spdt = processSadlPrimitiveDataType(element,
						(SadlPrimitiveDataType) superElement, newNames.get(0));
				if (spdt instanceof OntClass) {
					rsrcList.add((OntClass) spdt);
				} else if (spdt != null) {
					if (spdt.canAs(OntResource.class)) {
						rsrcList.add(spdt.as(OntResource.class));
					} else {
						throw new JenaProcessorException("Expected OntResource to be returned"); // .add(spdt);
					}
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
					boolean isRDFDatatype = false;
					List<RDFNode> unionClasses = null;
					if (superCls instanceof UnionClass) {
						ExtendedIterator<? extends org.apache.jena.rdf.model.Resource> itr = ((UnionClass) superCls)
								.listOperands();
						unionClasses = new ArrayList<RDFNode>();
						while (itr.hasNext()) {
							org.apache.jena.rdf.model.Resource cls = itr.next();
							if (cls.canAs(OntResource.class) && conceptIsRDFDatatype(cls.as(OntResource.class))) {
								isRDFDatatype = true;
								unionClasses.add(cls.as(OntResource.class));
							}
							// System.out.println("Union member: " + cls.toString());
						}
					} else if (superCls instanceof IntersectionClass) {
						ExtendedIterator<? extends org.apache.jena.rdf.model.Resource> itr = ((IntersectionClass) superCls)
								.listOperands();
						while (itr.hasNext()) {
							org.apache.jena.rdf.model.Resource cls = itr.next();
							// System.out.println("Intersection member: " + cls.toString());
						}
					}
					
					if (isRDFDatatype) {
						OntClass newCls = createRdfsDatatype(newNames.get(0), unionClasses, null, null);
						newCls.addProperty(RDF.type, RDFS.Datatype);
					}
					else {
						OntClass newCls = createOntClass(newNames.get(0), superCls.as(OntClass.class));
						rsrcList.add(newCls);
					}
				}
			}
		}
		EList<SadlPropertyRestriction> restrictions = element.getRestrictions();
		if (restrictions != null && rsrcList != null && rsrcList.size() > 0) {
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
				else if (rest instanceof SadlRangeRestriction) {
					SadlTypeReference rng = ((SadlRangeRestriction)rest).getRange();
					RangeValueType rngValueType = RangeValueType.CLASS_OR_DT; // default
					boolean isRngLst = typeRefIsList(rng);
					if (isRngLst) {
						rngValueType = RangeValueType.LIST;
					}
					OntProperty prop;
					String propUri = null;
					if (rsrcList != null && rsrcList.size() > 0 && rsrcList.get(0) instanceof Property &&
							((Property)rsrcList.get(0)).isURIResource()) {
						propUri = ((Property)rsrcList.get(0)).getURI();
					}
					String rngName = null;
					if (!isRngLst && rng instanceof SadlPrimitiveDataType) {
						rngName = ((SadlPrimitiveDataType) rng).getPrimitiveType().getName();
						RDFNode rngNode = primitiveDatatypeToRDFNode(rngName);
						if (!checkForExistingCompatibleDatatypeProperty(propUri, rngNode)) {
							prop = createDatatypeProperty(propUri, null);
							addPropertyRange(OntConceptType.DATATYPE_PROPERTY, prop, rngNode, rngValueType, rng);
						} else {
							prop = getTheJenaModel().getDatatypeProperty(propUri);
							addPropertyRange(OntConceptType.DATATYPE_PROPERTY, prop, rngNode, rngValueType, rng);
						}
						addPropertyRange(OntConceptType.DATATYPE_PROPERTY, prop, rngNode, rngValueType, rng);
					} else {
						OntResource rngRsrc = null;
						OntConceptType propType;
						if (rng instanceof SadlUnionType || rng instanceof SadlIntersectionType) {
							Object rngObj = sadlTypeReferenceToObject(rng);
							propType = OntConceptType.CLASS_PROPERTY;
							if (rngObj instanceof OntResource) {
								rngRsrc = (OntResource) rngObj;
							}
							else {
								addError("Unable to resolve to a range", rng);
							}
						}
						else {
							ConceptName rngcn = sadlSimpleTypeReferenceToConceptName(rng);
							if (rngcn != null) {
								rngName = rngcn.toFQString();
							} else {
								addError("No range found in statement", rng);
							}
							if (isRngLst) {
								rngRsrc = getOrCreateListSubclass(null, rngName, element.eResource(), ((SadlRangeRestriction) rest).getFacet());
								propType = OntConceptType.CLASS_LIST;

							} else {
								rngRsrc = sadlTypeReferenceToOntResource(rng);
								propType = OntConceptType.CLASS_PROPERTY;

							}
						}
						if (propUri != null && rngRsrc != null) {
							Property propOther = assignRangeToProperty(propUri, propType, rngRsrc, rngValueType, rng);
						}
					}
					
				}
				else {
					addError("Invalid property restriction", rest);
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

		return rsrcList;
	}

	protected void redeclarationHandler(SadlResource sr, SadlResource decl) {
		try {
			if (getDeclarationExtensions().getOntConceptType(decl).equals(OntConceptType.STRUCTURE_NAME)) {
				addError("This is already a Named Structure", sr);
			}
			if (!getDeclarationExtensions().getConceptNamespace(sr).equals(getModelNamespace())) {
				addError("Declaration of concepts in another namespace not supported", sr);
			}
		} catch (CircularDefinitionException e) {
			e.printStackTrace();
		}
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
		// 7) <prop> of <prop> of .... of <class> has [level n] default value <value>.
		SadlResource sr = sadlResourceFromSadlProperty(element);
		String propUri = getDeclarationExtensions().getConceptUri(sr);
		checkForInvalidAnnotation(sr);
		OntConceptType propType;
		try {
			propType = getDeclarationExtensions().getOntConceptType(sr);
			if (!isProperty(propType)) {
				if (sr != null) {
					addError(SadlErrorMessages.INVALID_USE_OF_CLASS_AS_PROPERTY
							.get(getDeclarationExtensions().getConcreteName(sr)), element);
				}
				else {
					addError("Property not found", element);
				}
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
				RDFNode rngNode = null;
				OntResource rngRsrc = null;
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
						rngNode = primitiveDatatypeToRDFNode(rngName);
						if (!checkForExistingCompatibleDatatypeProperty(propUri, rngNode)) {
							prop = createDatatypeProperty(propUri, null);
						} else {
							prop = getTheJenaModel().getDatatypeProperty(propUri);
						}
						addPropertyRange(propType, prop, rngNode, rngValueType, rng);
						retProp = prop;
						if (rngNode.isResource()) {
							rngRsrc = getTheJenaModel().getOntResource(rngNode.asResource());
						}
					} else {
						if (rng instanceof SadlUnionType || rng instanceof SadlIntersectionType) {
							Object rngObj = sadlTypeReferenceToObject(rng);
							if (rngObj instanceof OntResource) {
								rngRsrc = (OntResource) rngObj;
							}
							else {
								addError("Unable to resolve to a range", rng);
							}
						}
						else {
							ConceptName rngcn = sadlSimpleTypeReferenceToConceptName(rng);
							if (rngcn != null) {
								rngName = rngcn.toFQString();
							} else {
								addError("No range found in statement", rng);
							}
							if (isList) {
								rngRsrc = getOrCreateListSubclass(null, rngName, element.eResource(), ((SadlRangeRestriction) spr1).getFacet());
								propType = OntConceptType.CLASS_PROPERTY;
							} else {
								rngRsrc = sadlTypeReferenceToOntResource(rng);
							}
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
					if (rngRsrc !=  null) {
						addQualifiedCardinalityRestrictionForSingleValue(retProp, propType, subject, rng, rngRsrc);
					}					
					else {
						addCardinalityRestriction(subject, retProp, 1);
					}
				}
			} else if (spr1 instanceof SadlCondition) {
				OntProperty prop = getTheJenaModel().getOntProperty(propUri);
				if (prop == null) {
					if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
						prop = getOrCreateObjectProperty(propUri);
					}
					else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
						prop = getOrCreateDatatypeProperty(propUri);
					}
					else {
						prop = getOrCreateRdfProperty(propUri);
					}
				}
				OntClass condCls = sadlConditionToOntClass((SadlCondition) spr1, prop, propType);
				OntClass cls = null;
				if (subject != null) {
					if (subject.canAs(OntClass.class)) {
						cls = subject.as(OntClass.class);
						cls.addSuperClass(condCls);
						retProp = prop;	// awc 3/12/21: this was set to null but that failed case GH509
					} else {
						throw new JenaProcessorException("Unable to convert concept being restricted ("
								+ subject.toString() + ") to an OntClass.");
					}
				} else {
					addError("A property restriction requires specifying the class to which it applies.", element);
				}
			} else if (spitr.hasNext()) {
				// there is more than one restriction in the element
				SadlPropertyRestriction spr2 = spitr.next();
				boolean isDefVal = false;
				List<SadlResource> srs = new ArrayList<SadlResource>();
				SadlDefaultValue sdv = null;
				RDFNode defValue = null;
				int dvlvl = 0;
				srs.add(element.getNameOrRef());
				EList<SadlPropertyRestriction> restrictions = element.getRestrictions();
				int restrictionCount = 0;
				for (SadlPropertyRestriction spr : restrictions) {
					restrictionCount++;
					if (spr instanceof SadlTypeAssociation) {
						SadlTypeReference sprd = ((SadlTypeAssociation)spr).getDomain();
						if (sprd instanceof SadlSimpleTypeReference) {
							srs.add(((SadlSimpleTypeReference)sprd).getType());
						}
						else if (!(sprd instanceof SadlUnionType) && !(sprd instanceof SadlIntersectionType)) {
							// union and intersection classes handled below
							addError("Unhandled restriction type reference: " + sprd.getClass().getCanonicalName(), sprd);
						}
					}
					else if (spr instanceof SadlDefaultValue) {
						sdv = (SadlDefaultValue)spr;
						dvlvl = sdv.getLevel();
						SadlExplicitValue dv = sdv.getDefValue();
						if (dv instanceof SadlResource){
							defValue = getTheJenaModel().getResource(declarationExtensions.getConceptUri((SadlResource)dv));
						}
						else if (dv instanceof SadlExplicitValueLiteral) {
							defValue = sadlExplicitValueToRdfNode(dv, getTheJenaModel().getProperty(declarationExtensions.getConceptUri(srs.get(0))), true);
						}
						isDefVal = true;
					}
					
				}
				if (isDefVal) {
					// process the default value and return (don't keep going)					
					OntClass restricted = null;
					List<Property> props = new ArrayList<Property>();	
					for (int i = srs.size(); i > 0; i--) {
						SadlResource chsr = srs.get(i-1);
						try {
							OntConceptType chsrtype = declarationExtensions.getOntConceptType(chsr);
							if (chsrtype.equals(OntConceptType.CLASS)) {
								restricted = getTheJenaModel().getOntClass(declarationExtensions.getConceptUri(chsr));
							}
							else {
								props.add(getTheJenaModel().getProperty(declarationExtensions.getConceptUri(chsr)));
							}
						} catch (CircularDefinitionException e) {
							throw new JenaProcessorException(e.getMessage(), e);
						}
					}
					if (spr1 instanceof SadlTypeAssociation) {
						String dmnkw = ((SadlTypeAssociation)spr1).getDmnkw();
						if (restricted != null && dmnkw!= null && dmnkw.equals("describes")) {
							addPropertyDomain(props.get(0), restricted, spr1);
						}
					}
					try {
						if (sadlDefaultsModel == null) {
							try {
								importSadlDefaultsModel(element.eResource());
							} catch (Exception e) {
								e.printStackTrace();
								throw new JenaProcessorException("Failed to load SADL Defaults model", e);
							}
						}
						Individual seeAlsoDefault = createDefault(restricted, props, defValue, dvlvl, element);
						if (seeAlsoDefault != null) {
							restricted.addSeeAlso(seeAlsoDefault);
							return props.get(props.size() - 1);
						} else {
							addError("Unable to create default for '" + restricted.getURI() + "'", element);
						}
					} catch (Exception e) {
						throw new JenaProcessorException(e.getMessage(), e);
					}
					return null;
				}
				if (restrictionCount > 2) {
					throw new JenaProcessorException(
							"Unexpected SadlProperty has more than 2 restrictions");
				}
				if (spr1 instanceof SadlTypeAssociation && spr2 instanceof SadlRangeRestriction) {
					// this is case 3
					SadlTypeReference domain = ((SadlTypeAssociation) spr1).getDomain();
					OntConceptType domaintype;
					try {
						domaintype = sadlTypeReferenceOntConceptType(domain);
						if (domaintype != null) {
							if (domaintype.equals(OntConceptType.DATATYPE)) {
								addWarning(SadlErrorMessages.DATATYPE_AS_DOMAIN.get(), domain);
							}
							else if (!domaintype.equals(OntConceptType.CLASS) &&
									!domaintype.equals(OntConceptType.CLASS_LIST) &&
									propType != null &&
									!propType.equals(OntConceptType.ANNOTATION_PROPERTY)) {
								addError(SadlErrorMessages.DOMAIN_NOT_CLASS.get(), domain);
							}
						}
					} catch (CircularDefinitionException e) {
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
					OntResource rngRsrc = null;
					RDFNode rngNode = null;
					if (rng instanceof SadlPrimitiveDataType && !rngValueType.equals(RangeValueType.LIST)) {
						String rngName = ((SadlPrimitiveDataType) rng).getPrimitiveType().getName();
						rngNode = primitiveDatatypeToRDFNode(rngName);
						if (((SadlRangeRestriction)spr2).getFacet() != null) {
							// this is an in-line restriction 
							if (rngNode.isURIResource() && rngNode.asResource().getNameSpace().equals(XSD.getURI())) {
								// give this a system-generated name and create user-defined datatype
								//	this is the property definition, and it's a datatype property so appending "_range" to the
								//	property URI should be unique
								String uddtUri = propUri + "_range";
								rngNode = createRdfsDatatype(uddtUri, null, rngNode.asResource(), ((SadlRangeRestriction)spr2).getFacet());
							}							
						}
						DatatypeProperty prop2 = null;
						if (!checkForExistingCompatibleDatatypeProperty(propUri, rngNode)) {
							// TODO should this ever happen? spr1 should have created the property?
							prop2 = createDatatypeProperty(propUri, null);
							addPropertyRange(propType, prop, rngNode, rngValueType, rng);
						} else {
							prop2 = getTheJenaModel().getDatatypeProperty(propUri);
						}
						if (rngNode.isResource()) {
							rngRsrc = getTheJenaModel().getOntResource(rngNode.asResource());
						}
						retProp = prop2;
					} else if (((SadlRangeRestriction) spr2).getTypeonly() == null) {
						rngRsrc = sadlTypeReferenceToOntResource(rng);
						if (rngRsrc == null) {
							addError(SadlErrorMessages.RANGE_RESOLVE.toString(), rng);
						} else {
							retProp = assignRangeToProperty(propUri, propType, rngRsrc, rngValueType, rng);
						}
					} else {
						retProp = prop;
					}
					if (((SadlRangeRestriction) spr2).isSingleValued()) {
						if (rngRsrc !=  null) {
							addQualifiedCardinalityRestrictionForSingleValue(retProp, propType, domainrsrc,
									rng, rngRsrc);
						}					
						else {
							addCardinalityRestriction(domainrsrc, retProp, 1);
						}
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
						Property prop;
						if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
							prop = getOrCreateObjectProperty(propUri);
						}
						else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
							prop = getOrCreateDatatypeProperty(propUri);
						}
						else {
							prop = getTheJenaModel().getProperty(propUri);
						}
						if (prop != null) {
							String dmnkw = ((SadlTypeAssociation)spr1).getDmnkw();
							if (dmnkw != null && dmnkw.equals("describes")) {
								addPropertyDomain(prop, cls, domain);
							}
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
							String dmnkw = ((SadlTypeAssociation)spr1).getDmnkw();
							if (dmnkw != null && dmnkw.equals("describes")) {
								addPropertyDomain(prop, cls, domain);
							}
							EList<SadlExplicitValue> values = ((SadlCanOnlyBeOneOf) spr2).getValues();
							if (values != null) {
								EnumeratedClass enumCls = sadlExplicitValuesToEnumeratedClass(values, prop);
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
							String dmnkw = ((SadlTypeAssociation)spr1).getDmnkw();
							if (dmnkw != null && dmnkw.equals("describes")) {
								addPropertyDomain(prop, cls, domain);
							}
							EList<SadlExplicitValue> values = ((SadlMustBeOneOf) spr2).getValues();
							if (values != null) {
								EnumeratedClass enumCls = sadlExplicitValuesToEnumeratedClass(values, prop);
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
							String dmnkw = ((SadlTypeAssociation)spr1).getDmnkw();
							if (dmnkw != null && dmnkw.equals("describes")) {
								addPropertyDomain(prop, cls, domain);
							}
							if (sadlDefaultsModel == null) {
								try {
									importSadlDefaultsModel(element.eResource());
								} catch (Exception e) {
									e.printStackTrace();
									throw new JenaProcessorException("Failed to load SADL Defaults model", e);
								}
							}
							RDFNode defVal = null;
							try {
								if (propType.equals(OntConceptType.CLASS_PROPERTY) && 
										!(dv instanceof SadlResource)) {
									addError("Object property cannot be given a literal value as default", spr2);
								}
								else {
									defVal = sadlExplicitValueToRdfNode(dv, prop, true);
								}
							}
							catch (Exception e) {
								Throwable cause = e.getCause();
								if (cause instanceof JenaProcessorException) {
									
								}
							}
							Individual seeAlsoDefault = null;
							List<Property> props = new ArrayList<Property>();
							props.add(prop);
							if (propType.equals(OntConceptType.CLASS_PROPERTY)
									|| (propType.equals(OntConceptType.RDF_PROPERTY) && defVal.isResource())) {
								if (!(defVal.isURIResource()) || !defVal.canAs(Individual.class)) {
									addError("Error creating default for property '" + propUri + "' for class '"
											+ cls.getURI() + "' with value '" + defVal.toString()
											+ "': the value is not a named concept.", spr2);
								} else {
									Individual defInst = defVal.as(Individual.class);
									try {
										seeAlsoDefault = createDefault(cls, props, defInst, lvl, element);
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
										seeAlsoDefault = createDefault(cls, props, defVal.asLiteral(), lvl, spr2);
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
			} else if (spr1 instanceof SadlIsFunctional) {
				boolean isInverse = ((SadlIsFunctional) spr1).isInverse();
				if (isInverse) {
					if (!propType.equals(OntConceptType.CLASS_PROPERTY)) {
						addError("Only object properties can be inverse functional", spr1);
					}
					else {
						ObjectProperty prop = getOrCreateObjectProperty(propUri);
						if (prop != null) {
							getTheJenaModel().add(prop, RDF.type, OWL.InverseFunctionalProperty);
						}
					}
				}
				else {
					Property prop = null;
					if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
						prop = getOrCreateObjectProperty(propUri);
					}
					else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
						prop = getOrCreateDatatypeProperty(propUri);
					}
					else if (propType.equals(OntConceptType.RDF_PROPERTY)) {
						prop = getOrCreateRdfProperty(propUri);
					}
					else {
						addError("Property type '" + propType.toString() + "' can't be made functional", spr1);
					}
					if (prop != null) {
						getTheJenaModel().add(prop, RDF.type, OWL.FunctionalProperty);
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

	private void addQualifiedCardinalityRestrictionForSingleValue(Property retProp, OntConceptType propType,
			OntResource domainrsrc, SadlTypeReference rng, OntResource rngRsrc) {
		Property onProp = null;
		CardinalityRestriction restrict = null;
		if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
			onProp = OWL2.onClass;
		}
		else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)){	
			onProp = OWL2.onDataRange;
		}
		else if (rngRsrc.canAs(DataRange.class)) {
			onProp = OWL2.onDataRange;
		}
		else if (rngRsrc.canAs(OntClass.class)) {
			onProp = OWL2.onClass;
		}
		else {
			addError("Unable to determine whether qualified cardinality is to a class or a data range", rng);
		}
		if (onProp != null) {
			restrict = createQualifiedCardinalityRestriction(retProp, rng, rngRsrc, OWL2.qualifiedCardinality, onProp, 1);
		}
		if (restrict != null && domainrsrc != null) {
			domainrsrc.as(OntClass.class).addSuperClass(restrict);
		}
		else {
			addError("Failed to create qualified cardinality restriction.", rng.eContainer());
		}
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
		if (cls != null && cls.canAs(OntClass.class) && retProp != null) {
			CardinalityRestriction cr = getTheJenaModel().createCardinalityRestriction(null, retProp, cardinality);
			cls.as(OntClass.class).addSuperClass(cr);
		}
	}

	private String createUniqueDefaultValName(OntClass restricted, List<Property> props, int level) throws PrefixNotFoundException {
		StringBuilder sb = new StringBuilder(restricted.getLocalName());
		for (Property prop : props) {
			sb.append("_");
			sb.append(prop.getLocalName());
		}
		sb.append("_default");
		sb.append("_lvl");
		sb.append(level);
		String nmBase = sb.toString();
		String nm = getModelNamespace() + nmBase;
		int cntr = 0;
		while (getTheJenaModel().getIndividual(nm) != null) {
			nm = nmBase + ++cntr;
		}
		return nm;
	}

	private Individual createDefault(OntClass restricted, List<Property> props, RDFNode defValue, int level, EObject ref)
			throws Exception {
		OntClass instDefCls = getTheJenaModel().getOntClass(ResourceManager.ACUITY_DEFAULTS_NS + "DefaultValue");
		if (instDefCls == null) {
			addError("Unable to find DefaultValue in Defaults model", ref);
			return null;
		}
		OntClass pceCls = getTheJenaModel().getOntClass(ResourceManager.ACUITY_DEFAULTS_NS + "PropertyChainElement");
		if (pceCls == null) {
			addError("Unable to find PropertyChainElement in Defaults model", ref);
			return null;
		}
		Individual def = getTheJenaModel().createIndividual(createUniqueDefaultValName(restricted, props, level),
				instDefCls);
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
		def.addProperty(getTheJenaModel().getOntProperty(ResourceManager.ACUITY_DEFAULTS_NS + "hasDefault"), defValue);
		
		Individual lastPce = null;
		for (Property prop : props) {
			Individual pce = getTheJenaModel().createIndividual(pceCls);
			pce.addProperty(getTheJenaModel().getOntProperty(ResourceManager.ACUITY_DEFAULTS_NS + "propertyElement"), prop);
			if (lastPce == null) {
				def.addProperty(getTheJenaModel().getOntProperty(ResourceManager.ACUITY_DEFAULTS_NS + "appliesToPropertyChain"), pce);
			}
			else {
				lastPce.addProperty(getTheJenaModel().getOntProperty(ResourceManager.ACUITY_DEFAULTS_NS + "nextPropertyChainElement"), pce);
			}
			lastPce = pce;
		}
		return def;
	}

	private EnumeratedClass sadlExplicitValuesToEnumeratedClass(EList<SadlExplicitValue> values, Property prop)
			throws JenaProcessorException {
		List<RDFNode> nodevals = new ArrayList<RDFNode>();
		for (int i = 0; i < values.size(); i++) {
			SadlExplicitValue value = values.get(i);
			if (value instanceof SadlResource) {
				try {
					OntConceptType vtype = getDeclarationExtensions().getOntConceptType((SadlResource)value);
					if (!vtype.equals(OntConceptType.INSTANCE)) {
						addError("Expected an instance in the enumeration of the class", value);
					}
				} catch (CircularDefinitionException e) {
					throw new JenaProcessorException("Unexpected error, please report: " + e.getMessage());
				}
			}
			RDFNode nodeval = sadlExplicitValueToRdfNode(value, prop, true);
			if (nodeval != null) {
				if (nodeval.canAs(Individual.class)) {
					nodevals.add(nodeval.as(Individual.class));
				} else {
					nodevals.add(nodeval);
				}
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
			Individual theInst = getTheJenaModel().getIndividual(uri);
			if (theInst != null) {
				return theInst;
			}
			else {
				// Indiividual doesn't exist; don't create an Individual unless we can determine a class.
				if (prop != null) {
					StmtIterator rngitr = getTheJenaModel().listStatements(prop, RDFS.range, (RDFNode)null);
					int cntr = 0;
					if (rngitr.hasNext()) {
						while (rngitr.hasNext()) {
							RDFNode objnode = rngitr.nextStatement().getObject();
							if (objnode.canAs(OntClass.class)) {
								if (theInst == null) {
									theInst = getTheJenaModel().createIndividual(uri, objnode.as(OntClass.class));								
								}
								else if (theInst != null){
									theInst.addProperty(RDF.type, objnode.as(OntClass.class));
								}
							}
							else {
								addError("Unable to determine type of instance from property range", value);
							}
						}
					}
					else {
						addError("Unable to determine type of instance, the property has no range", value);
					}
				}
				else {
					addError("Unable to determine type of instance, no property and range defined", value);
				}
			}
			return theInst;
		}
		else {
			if (prop != null) {
				StmtIterator rngitr = getTheJenaModel().listStatements(prop, RDFS.range, (RDFNode)null);
				while (rngitr.hasNext()) {
					RDFNode rng = rngitr.nextStatement().getObject();
					if (rng instanceof org.apache.jena.rdf.model.Resource) {
						if (rng.asResource().getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
							org.apache.jena.rdf.model.Resource effectiveRng = getUnittedQuantityValueRange();
							if (isIgnoreUnittedQuantities()) {
								Literal lval = sadlExplicitValueToLiteral((SadlNumberLiteral) value, effectiveRng);
								if (lval != null) {
									return lval;
								}
							}
							else {
								Individual unittedVal = getTheJenaModel().createIndividual(
											getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI));
								
								// TODO this may need to check for property restrictions on a subclass of
								// UnittedQuantity
								unittedVal.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI),
										getTheJenaModel().createTypedLiteral(((SadlNumberLiteral)value).getLiteralNumber(), XSD.decimal.getURI()));
								if (((SadlNumberLiteral)value).getUnit() != null) {
									unittedVal.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI),
											getTheJenaModel().createTypedLiteral(((SadlNumberLiteral)value).getUnit(), XSD.xstring.getURI()));
								}
								return unittedVal;
							}
						}
						try {
							Literal litval = sadlExplicitValueToLiteral(value, (org.apache.jena.rdf.model.Resource) rng);
							rngitr.close();
							return litval;
						}
						catch (Exception e) {
							throw new JenaProcessorException(e.getMessage(), e);
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
		if (sr == null && element.getNameDeclarations() != null) {
			Iterator<SadlResource> itr = element.getNameDeclarations().iterator();
			if (itr.hasNext()) {
				sr = itr.next();
			}
		}
		return sr;
	}

	private void addPropertyRange(OntConceptType propType, OntProperty prop, RDFNode rngNode,
			RangeValueType rngValueType, EObject context) throws JenaProcessorException {
		OntResource rngResource = null;
		if (rngNode instanceof OntClass) {
			rngResource = rngNode.as(OntClass.class);
			if (prop.isDatatypeProperty()) {
				boolean changePropType = true;
				if (context instanceof SadlTypeReference) {
					ConceptName cn = sadlSimpleTypeReferenceToConceptName((SadlTypeReference)context);
					if (cn != null && cn.getType().equals(ConceptType.RDFDATATYPE)) {
						// if the range is a user-defined datatype, do not change to an ObjectProperty
						changePropType = false;
					}
				}
				if (changePropType) {
					// this happens when the range is a union of Lists of primitive types
					getTheJenaModel().remove(prop, RDF.type, OWL.DatatypeProperty);
					getTheJenaModel().add(prop, RDF.type, OWL.ObjectProperty);
				}
			}
		}
		// If ignoring UnittedQuantity, change any UnittedQuantity range to the range of
		// value and make the property an owl:DatatypeProperty
		// TODO this should probably work for any declared subclass of UnittedQuantity
		// and associated value restriction?
		if (isIgnoreUnittedQuantities() && rngResource != null && rngResource.isURIResource()
				&& rngResource.canAs(OntClass.class)
				&& rngResource.getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
			org.apache.jena.rdf.model.Resource effectiveRng = getUnittedQuantityValueRange();
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
			// property already has a range known to this model
			if (rngNode.equals(existingRngNode) || (rngResource != null && rngResource.equals(existingRngNode))) {
				// do nothing-- rngNode is already in range
				return;
			}
			if (prop.isDatatypeProperty()) {
				String existingRange = stmtIteratorToObjectString(
						getTheJenaModel().listStatements(prop, RDFS.range, (RDFNode) null));
				addError(SadlErrorMessages.CANNOT_ASSIGN_EXISTING.get("range", 
						UtilsForJena.nodeToString(theJenaModel, getConfigMgr(), getModelNamespace(), prop), existingRange),
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
					String existingRange = UtilsForJena.nodeToString(theJenaModel, getConfigMgr(), getModelNamespace(), existingRngNode);
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
			if (isDomainAndRangeAsUnionClasses()) {
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
				addWarning(SadlErrorMessages.IMPORTED_RANGE_CHANGE.get(
						UtilsForJena.nodeToString(theJenaModel, getConfigMgr(), getModelNamespace(), prop)), context);
			}
		} // end if existing range in any model, this or imports
		if (rngNode != null) {
			if (rngResource != null) {
				if (!isDomainAndRangeAsUnionClasses() && rngResource instanceof UnionClass) {
					List<org.apache.jena.rdf.model.Resource> uclsmembers = getUnionClassMemebers(
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
				org.apache.jena.rdf.model.Resource rngrsrc = rngNode.asResource();
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
			sb.append(UtilsForJena.nodeToString(theJenaModel, getConfigMgr(), getModelNamespace(), obj));
		}
		return sb.toString();
	}

	private boolean checkForSubclassing(OntClass rangeCls, OntResource existingRange, EObject context)
			throws JenaProcessorException {
		// this is changing the range of a property defined in a different model
		try {
			if (classIsSubclassOfCached((OntClass) rangeCls, existingRange, true, null)) {
				return true;
			}
		} catch (CircularDependencyException e) {
			throw new JenaProcessorException(e.getMessage(), e);
		}
		return false;
	}
	
	public void addError(String msg, EObject context) {
		addError(msg, context, null);
	}

	public void addError(String msg, EObject context, String issueCode, String... issueData) {
		if (getIssueAcceptor() != null) {
			getIssueAcceptor().add(msg, context, Severity.ERROR, issueCode, issueData);
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
		addWarning(msg, context, null);
	}

	public void addWarning(String msg, EObject context, String issueCode, String... issueData) {
		if (getIssueAcceptor() != null) {
			getIssueAcceptor().add(msg, context, Severity.WARNING, issueCode, issueData);
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
		addInfo(msg, context, null);
	}

	public void addInfo(String msg, EObject context, String issueCode, String... issueData) {
		if (getIssueAcceptor() != null) {
			getIssueAcceptor().add(msg, context, Severity.INFO, issueCode, issueData);
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
				OntConceptType type = null;
				try {
					type = getDeclarationExtensions().getOntConceptType(sr);
				} catch (CircularDefinitionException e) {
					e.printStackTrace();
				}
				if (type != null && !type.equals(OntConceptType.INSTANCE)) {
					addTypeCheckingError("Only instances can be different from each other.", element);
				}
				else {
					Individual inst;
					try {
						inst = getOrCreateIndividual(sr);
						if (inst != null) {
							differentFrom.add(inst);
						}
					} catch (CircularDefinitionException e) {
						e.printStackTrace();
					}
				}
			}
		}
		SadlTypeReference nsas = element.getNotTheSameAs();
		if (nsas != null) {
			OntResource nsasrsrc = sadlTypeReferenceToOntResource(nsas);
			if (nsasrsrc != null) {
				differentFrom.add(nsasrsrc.asIndividual());
			}
			else {
				addError("Could not resolve reference", nsas);
			}
			SadlResource sr = element.getNameOrRef();
			Individual otherInst = getTheJenaModel().getIndividual(getDeclarationExtensions().getConceptUri(sr));
			if (otherInst != null) {
				differentFrom.add(otherInst);
			}
			else {
				addError("Could not resolve reference", sr);
			}
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
		if (isEObjectPreprocessed(element)) {
			return null;
		}
		// this has multiple forms:
		// 1) <name> is a <type> ...
		// 2) a <type> <name> ....
		// 3) <name> is a <property> of <instance>
		// 4) <property> of <instance> is <value>
		SadlTypeReference type = element.getType();
		boolean isList = typeRefIsList(type);
		SadlResource sr = sadlResourceFromSadlInstance(element);
		checkForInvalidAnnotation(sr);
		Individual inst = null;
		EObject subjCtx = null;
		String instUri = null;
		OntConceptType subjType = null;
		boolean isActuallyClass = false;
		boolean isActuallyProperty = false;
		if (sr != null) {
			subjCtx = sr;	// might change later but this is default
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
			else if (isProperty(subjType)) {
				// this must be of the form "<property> of <subject> is <value>"
				//	so sr is the property, propertyInitializer.property is the subject, propertyIntitalizer.value is the value
				isActuallyProperty = true;
			}
		}
		OntClass cls = null;
		if (!isActuallyClass) {
			if (type == null && subjType != null && subjType.equals(OntConceptType.INSTANCE)) {
				SadlResource theDecl = getDeclarationExtensions().getDeclaration(element.getNameOrRef());
				if (theDecl.eContainer() != null && theDecl.eContainer() instanceof SadlInstance) {
					type = ((SadlInstance)theDecl.eContainer()).getType();
				}
				if (type == null && theDecl != null && theDecl.equals(sr)) { // element.getPropertyInitializers() != null) {
					// this is the declaration but it has no type
					addTypeCheckingError("No type is declared", element);
				}
			}
			if (type != null) {
				if (type instanceof SadlPrimitiveDataType) {
					org.apache.jena.rdf.model.Resource rsrc = sadlTypeReferenceToResource(type);
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
				} 
				else if (type instanceof SadlTableDeclaration) {
					setHostEObject(element);
					OntClass tdClass = getTheJenaModel().getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_DATA_TABLE_CLASS_URI);
					if (tdClass != null) {
						inst = getTheJenaModel().createIndividual(instUri, tdClass);
						String location = ((SadlTableDeclaration)type).getLocation();
						if (location != null) {
						    try {
						        URL url = new URL(location);
						    }
						    catch (Exception e1) {
						    	addWarning("'" + location + "' doesn't apprear to be a valid URL", type);
						    }

							Property dlprop = getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_DATA_CONTENT_LOCATION_PROPERY_URI);
							if (dlprop != null) {
								Literal locUri = getTheJenaModel().createTypedLiteral(location, XSDDatatype.XSDanyURI);
								inst.addProperty(dlprop, locUri);
							}
							else {
								addError("Model doesn't contain DataTable metamodel. Do you need to update the SadlImplicitModel?", type);
							}
						}
					}
					else {
						addError("Model doesn't contain DataTable metamodel. Do you need to update the SadlImplicitModel?", type);
					}
					
					EList<SadlParameterDeclaration> columnDescriptors = ((SadlTableDeclaration)type).getParameter();
					List<DataDescriptor> columns = new ArrayList<DataDescriptor>();
					for (SadlParameterDeclaration coldesc : columnDescriptors) {
						try {
							Node nm = processExpression(coldesc.getName());
							Object typ = processExpression(coldesc.getType());
							EList<String> units = coldesc.getUnits();
							if (units.size() > 1) {
								addError("A table column should only have a single unit specification", coldesc);
							}
							Object augtype = null;
							if (coldesc.getAugtype() != null) {
								augtype = processExpression(coldesc.getAugtype());
								if (augtype instanceof GraphPatternElement) {
									updateAugmentedTypePatterns((GraphPatternElement)augtype, nm, coldesc);
								}
							}
							DataDescriptor dd = new DataDescriptor(nm, (Node)typ, units, augtype);
							columns.add(dd);
							
						} catch (TranslationException e) {
							e.printStackTrace();
						} catch (InvalidNameException e) {
							e.printStackTrace();
						} catch (InvalidTypeException e) {
							e.printStackTrace();
						}
						
					}
					try {
						Object oldtarget = getTarget();
						setTarget(inst);
						addColumnDescriptorsToTable(inst, columns, type);
						setTarget(oldtarget);
					} catch (TranslationException e) {
						e.printStackTrace();
					}
					setHostEObject(null);
				}
				else {
					if (type instanceof SadlSimpleTypeReference) {
						SadlResource typeSR = ((SadlSimpleTypeReference)type).getType();
						if (typeSR.equals(sr)) {
							// being declared an instance of itself
							addError(getDeclarationExtensions().getConcreteName(sr) + " can't be an instance of itself.", sr);
							return null;
						}
					}
					OntResource or = sadlTypeReferenceToOntResource(type);
					if (or != null && or.canAs(OntClass.class)) {
						cls = or.asClass();
					} else if (or instanceof Individual) {
						inst = (Individual) or;
					} else if (or instanceof Property) {
						isActuallyProperty = true;
						sr = ((SadlSimpleTypeReference)type).getType();
					}				
				}
			}
			if (!isActuallyProperty) {
				if (inst == null) {
					if (cls != null) {
						inst = createIndividual(instUri, cls);
					} else if (instUri != null) {
						inst = getTheJenaModel().getIndividual(instUri);
						if (inst != null) {
							// instance already exists
						}
						else {
							inst = createIndividual(instUri, (OntClass) null);
						}
					} else {
						throw new JenaProcessorException("Can't create an unnamed instance with no class given");
					}
				}
			}
		}
		if (inst != null && sr != null && sr.getAnnotations() != null) {
			addAnnotationsToResource(inst, sr.getAnnotations());
		}

		Iterator<SadlPropertyInitializer> itr = element.getPropertyInitializers().iterator();
		while (itr.hasNext()) {
			SadlPropertyInitializer propinit = itr.next();
			SadlResource prop = propinit.getProperty();
			if (isActuallyProperty) {
				if (propinit.getFirstConnective() != null && propinit.getFirstConnective().equals("of")) {
					SadlResource subj = prop;
					subjCtx = subj;
					// this is of the form "<prop> of <subj> is <value>"
					//	so sr is the actual property, subj is the subject
					prop = sr;
					sr = subj;
					instUri = getDeclarationExtensions().getConceptUri(sr);
					if (instUri == null) {
						addError("instance does not exist", element);
						return null;
					}
					else {
						inst = getTheJenaModel().getIndividual(instUri);
					}
					subjType = getDeclarationExtensions().getOntConceptType(sr);
				}
				else if (inst == null) {
					inst = getTheJenaModel().getIndividual(instUri);
				}
			}
			OntConceptType propType = getDeclarationExtensions().getOntConceptType(prop);
			if (subjType != null && subjType.equals(OntConceptType.CLASS)
					&& !(propType.equals(OntConceptType.ANNOTATION_PROPERTY)) && // only a problem if not an annotation
																					// property
					!getOwlFlavor().equals(SadlConstants.OWL_FLAVOR.OWL_FULL)) {
				addWarning(SadlErrorMessages.CLASS_PROPERTY_VALUE_OWL_FULL.get(), element);
			}
			EObject val = propinit.getValue();
			if (val == null) {
				val = propinit.getType();
			}
			if (val != null) {
				try {
					if (getModelValidator() != null && !propType.equals(OntConceptType.ANNOTATION_PROPERTY)) {
						// don't type check annotation properties--they have no range (OWL 1)
						StringBuilder error = new StringBuilder();
						if (val instanceof Expression && propinit.getFirstConnective() != null && propinit.getFirstConnective().equals("is")) {
							if (!getModelValidator().checkPropertyValueInRange(getTheJenaModel(), (Expression) val, prop, sr, error)) {
								addTypeCheckingError(error.toString(), propinit);
							}
						}
						else {
							if (!getModelValidator().checkPropertyValueInRange(getTheJenaModel(), sr, prop, val, error)) {
								addTypeCheckingError(error.toString(), propinit);
							}
						}
					}
				} catch (DontTypeCheckException e) {
					// do nothing
				} catch (PropertyWithoutRangeException e) {
					String propUri = getDeclarationExtensions().getConceptUri(prop);
					if (propUri != null) {
						if (!propUri.equals(SadlConstants.SADL_IMPLICIT_MODEL_IMPLIED_PROPERTY_URI)) {
							addPropertyWithoutRangeError(propinit, prop, e);
						}
					}
				} catch (Exception e) {
					throw new JenaProcessorException("Unexpected error checking value in range", e);
				}
				if (val instanceof SadlResource && propinit.getFirstConnective() != null && propinit.getFirstConnective().equals("is")) {
					String valinstUri = getDeclarationExtensions().getConceptUri((SadlResource) val);
					Individual valinst = getTheJenaModel().getIndividual(valinstUri);
					assignInstancePropertyValue(valinst, cls, prop, subjCtx, sr);
				}
				else {
					EObject save = getHostEObject();
					setHostEObject(element);
					assignInstancePropertyValue(inst, cls, prop, subjCtx, val);
					setHostEObject(save);
				}
			} else {
				addError("no value found", propinit);
			}
		}
		SadlValueList listInitializer = element.getListInitializer();
		if (listInitializer != null) {
			if (listInitializer.getExplicitValues().isEmpty()) {
				addWarning(SadlErrorMessages.EMPTY_LIST_DEFINITION.get(), element);
			} else {
				if (cls == null) {
					NamedNode cn;
					try {
						cn = getTypedListType(inst);
						if (cn != null) {
							cls = getTheJenaModel().getOntClass(cn.toFullyQualifiedString());
						}
					} catch (TranslationException e) {
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

	private void addPropertyWithoutRangeError(EObject host, SadlResource prop,
			PropertyWithoutRangeException e) {
		String propID = e.getPropID();
		String msg = "";
		if (propID == null) {
			if (prop != null) {
				propID = getDeclarationExtensions().getConcreteName(prop);
			}
			else {
				propID = "<unknown>";
			}
		}
		msg += SadlErrorMessages.PROPERTY_WITHOUT_RANGE
				.get(propID);
		addTypeCheckingError(msg, host);
	}

	private void addColumnDescriptorsToTable(Individual inst, List<DataDescriptor> columns, EObject context) throws TranslationException {
		List<Individual> cddInstances = new ArrayList<Individual>();
		for (DataDescriptor cdd : columns) {
			Individual cddInst = dataDescriptorToOwl(context, cdd, false);
			if (cddInst != null) {
				cddInstances.add(cddInst);
			}
		}
		if (cddInstances.size() > 0) {
			RDFList cddInstList = getTheJenaModel().createList(cddInstances.iterator());
			inst.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_DATA_COLUMN_DESCRIPTORS_PROPERY_URI), cddInstList);
		}
	}

	private void updateAugmentedTypePatterns(GraphPatternElement augtype, Node nm, EObject context) {
		if (augtype instanceof Junction) {
			updateAugmentedTypePatterns(((ProxyNode)((Junction)augtype).getLhs()).getProxyFor(), nm, context);
			updateAugmentedTypePatterns(((ProxyNode)((Junction)augtype).getRhs()).getProxyFor(), nm, context);
		}
		else if (augtype instanceof TripleElement) {
			if (((TripleElement)augtype).getObject() == null) {
				((TripleElement)augtype).setObject(nm);
			}
		}
		else {
			addError("unhandled GraphPatternElement", context);
		}
	}

	private OWL_FLAVOR getOwlFlavor() {
		return owlFlavor;
	}

	@Override
	public NamedNode getTypedListType(RDFNode node) throws TranslationException {
		return UtilsForJena.getTypedListType(theJenaModel, getConfigMgr(), getModelNamespace(), node);
	}

	public ConceptName createTypedConceptName(String conceptUri, OntConceptType conceptType) throws InvalidTypeException {
		ConceptName cn = new ConceptName(conceptUri);
		setConceptNameType(cn, conceptType);
		return cn;
	}

	private ConceptName setConceptNameType(ConceptName cn, OntConceptType conceptType) throws InvalidTypeException {
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
		else {
			throw new InvalidTypeException("OntConceptType " + conceptType.toString() + " is not recognized.");
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
		if(type != null && type.eContainer() instanceof SadlInstance && !isList) {
			isList = ((SadlInstance)type.eContainer()).getListInitializer() != null;
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
		org.apache.jena.rdf.model.Resource to = null;
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
//		if (to == null) {
			// addError("No 'to' resource found in restriction of List subclass",
			// listInitializer);
//		}
		Iterator<SadlExplicitValue> values = listInitializer.getExplicitValues().iterator();
		addValueToList(inst, cls, to, values);
	}
	
	/**
	 * Method to add a value from a SadlExplicitValue Iterator to a SADL typed list
	 * @param lastInst -- the list to which a value is being added
	 * @param cls -- the Sadl list class 
	 * @param type -- the type of each element of the list
	 * @param valueIterator -- the Iterator providing values to be added
	 * @return
	 */
	private Individual addValueToList(Individual lastInst, OntClass cls,
			org.apache.jena.rdf.model.Resource type, Iterator<SadlExplicitValue> valueIterator) {
		if (lastInst == null) {
			lastInst = getTheJenaModel().createIndividual(cls);
		}
		SadlExplicitValue val = valueIterator.next();
		if (val instanceof SadlResource) {
			Individual listInst;
			try {
				if (type.canAs(OntClass.class)) {
					listInst = createIndividual((SadlResource) val, type.as(OntClass.class));
					ExtendedIterator<org.apache.jena.rdf.model.Resource> itr = listInst.listRDFTypes(false);
					boolean match = false;
					while (itr.hasNext()) {
						org.apache.jena.rdf.model.Resource typ = itr.next();
						if (typ.equals(type)) {
							match = true;
						}
					}
					if (!match) {
						addTypeCheckingError("The Instance '" + listInst.toString() + "' doesn't match the List type.", val);
					}
					getTheJenaModel().add(lastInst, getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI),
							listInst);
				} else {
					addTypeCheckingError("The type of the list could not be converted to a class.", val);
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
				getTheJenaModel().add(lastInst, getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI),
						lval);
			} catch (JenaProcessorException e) {
				addError(e.getMessage(), val);
			}
		}
		if (valueIterator.hasNext()) {
			Individual rest = addValueToList(null, cls, type, valueIterator);
			getTheJenaModel().add(lastInst, getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_REST_URI), rest);
		}
		return lastInst;
	}

	private void assignInstancePropertyValue(Individual inst, OntClass cls, SadlResource prop, EObject subjCtx, EObject val)
			throws JenaProcessorException, CircularDefinitionException {
		OntConceptType type;
		try {
			type = getDeclarationExtensions().getOntConceptType(prop);
		} catch (CircularDefinitionException e) {
			type = e.getDefinitionType();
			addError(e.getMessage(), prop);
		}
		String propuri = getDeclarationExtensions().getConceptUri(prop);
		if (propuri == null) {
			ICompositeNode node = NodeModelUtils.findActualNodeFor(prop);
			if (node == null) {
				EObject contr = prop != null ? prop.eContainer() : null;
				if (contr == null && val != null) {
					contr = val.eContainer();
				}
				while (node == null && contr != null) {
					node = NodeModelUtils.findActualNodeFor(contr);
				}
				if (node != null) {
					String txt = node.getText();
					String preTxt = null;
					if (contr instanceof SadlPropertyInitializer) {
						preTxt = ((SadlPropertyInitializer)contr).getFirstConnective();
					}
					if (preTxt != null && txt.trim().startsWith(preTxt.trim())) {
						txt = txt.trim().substring(preTxt.trim().length()).trim();
					}
					if (txt != null) {
						Literal lit = getTheJenaModel().createLiteral( txt, "en" );  	// this is how the literals are created in JenaBasedSadlModelProcessor; 
						StmtIterator stmtitr = getTheJenaModel().listStatements(null, RDFS.label, lit);
						while (stmtitr.hasNext()) {
							org.apache.jena.rdf.model.Resource replacement = stmtitr.nextStatement().getSubject();
							String[] data = new String[] { txt, replacement.getLocalName() };
							addWarning("Consider replacing '" + txt + "' with '" + replacement.getLocalName() + "'", contr, "REPLACE_ALIAS", data);
						}
					}
			    	String psrc = getSourceText(prop);
			    	String vsrc = getSourceText(val);
					if (node.getSyntaxErrorMessage() != null) {
						SyntaxErrorMessage synErr = node.getSyntaxErrorMessage();
						int i = 0;
					}
				}
			}
		}
		if (type.equals(OntConceptType.CLASS_PROPERTY)) {
			OntProperty oprop = getTheJenaModel().getOntProperty(propuri);
			if (oprop == null) {
				addError(SadlErrorMessages.PROPERTY_NOT_EXIST.get(propuri), prop);
			} else {
				if (val instanceof SadlInstance) {
					Individual instval = processSadlInstance((SadlInstance) val);
					OntClass uQCls = getTheJenaModel()
							.getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
					if (uQCls != null && instval != null && instval.hasRDFType(uQCls) && isIgnoreUnittedQuantities()) {
						if (val instanceof SadlNestedInstance) {
							Iterator<SadlPropertyInitializer> propinititr = ((SadlNestedInstance) val)
									.getPropertyInitializers().iterator();
							while (propinititr.hasNext()) {
								EObject pval = propinititr.next().getValue();
								if (pval instanceof SadlNumberLiteral) {
									org.apache.jena.rdf.model.Resource effectiveRng = getUnittedQuantityValueRange();
									Literal lval = sadlExplicitValueToLiteral((SadlNumberLiteral) pval, effectiveRng);
									if (lval != null) {
										addInstancePropertyValue(inst, oprop, lval, subjCtx, val);
									}
								}
							}
						}
					} else if (instval != null){
						addInstancePropertyValue(inst, oprop, instval, subjCtx, val);
					}
				} else if (val instanceof SadlResource) {
					String uri = getDeclarationExtensions().getConceptUri((SadlResource) val);
					org.apache.jena.rdf.model.Resource rsrc = getTheJenaModel().getResource(uri);
					if (rsrc.canAs(Individual.class)) {
						addInstancePropertyValue(inst, oprop, rsrc.as(Individual.class), subjCtx, val);
					} else {
						throw new JenaProcessorException(
								"unhandled value type SadlResource that isn't an instance (URI is '" + uri + "')");
					}
				} else if (val instanceof SadlValueList) {
					// EList<SadlExplicitValue> vals = ((SadlValueList)val).getExplicitValues();
					// convert to SADL Typed List
					try {
						TypeCheckInfo vtct = getModelValidator().getType(val);
						if (vtct != null) {
							if (vtct.getTypeCheckType() != null) {
								String typstr = vtct.getTypeCheckType().getURI();
								OntClass lstcls = getOrCreateListSubclass(null, typstr, prop.eResource(), null);
								Individual lval = getTheJenaModel().createIndividual(lstcls);
								addListValues(lval, lstcls, (SadlValueList) val);
								addInstancePropertyValue(inst, oprop, lval, subjCtx, val);
								RDFNode nv = inst.getPropertyValue(oprop);								
							}
							else {
								//create a list of the range type
								StmtIterator existingRngItr = getTheJenaModel().listStatements(oprop, RDFS.range, (RDFNode) null);
								if (existingRngItr.hasNext()) {
									RDFNode existingRngNode = existingRngItr.next().getObject();
									if (existingRngNode.isResource() && existingRngNode.canAs(OntClass.class)) {
										OntClass lstcls = existingRngNode.as(OntClass.class);
										Individual lval = getTheJenaModel().createIndividual(lstcls);
										addListValues(lval, lstcls, (SadlValueList) val);
										addInstancePropertyValue(inst, oprop, lval, subjCtx, val);
										RDFNode nv = inst.getPropertyValue(oprop);								
									}
								}
							}
						}
					} catch (InvalidNameException e) {
						e.printStackTrace();
					} catch (TranslationException e) {
						e.printStackTrace();
					} catch (URISyntaxException e) {
						e.printStackTrace();
					} catch (IOException e) {
						e.printStackTrace();
					} catch (ConfigurationException e) {
						e.printStackTrace();
					} catch (DontTypeCheckException e) {
						e.printStackTrace();
					} catch (InvalidTypeException e) {
						e.printStackTrace();
					} catch (CircularDependencyException e) {
						e.printStackTrace();
					} catch (PropertyWithoutRangeException e) {
						e.printStackTrace();
					}
//					addListValues(inst, cls, (SadlValueList) val);  // This was replaced by the code above because the
																	// list is the value of a triple, not elements of the subject (inst)
				} else if (val instanceof SadlExplicitValue) {
					OntResource rng = oprop.getRange();
					if (val instanceof SadlNumberLiteral && ((SadlNumberLiteral) val).getUnit() != null) {
						if (!isIgnoreUnittedQuantities()) {
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
							org.apache.jena.rdf.model.Resource effectiveRng = getUnittedQuantityValueRange();
							Literal lval = sadlExplicitValueToLiteral((SadlExplicitValue) val, effectiveRng);
							if (lval != null) {
								addInstancePropertyValue(inst, oprop, lval, subjCtx, val);
							}
						}
					} else {
						if (rng == null) {
							// this isn't really an ObjectProperty--should probably be an rdf:Property
							Literal lval = sadlExplicitValueToLiteral((SadlExplicitValue) val, null);
							addInstancePropertyValue(inst, oprop, lval, subjCtx, val);
						} else {
							addError("A SadlExplicitValue is given to an ObjectProperty", val);
						}
					}
				} else {
					throw new JenaProcessorException("unhandled value type for object property");
				}
			}
		} else if (type.equals(OntConceptType.DATATYPE_PROPERTY)) {
			OntProperty oprop = getTheJenaModel().getDatatypeProperty(propuri);
			if (oprop == null) {
				// maybe this is actually a list range, which means it's really an ObjectProperty
				oprop = getTheJenaModel().getObjectProperty(propuri);
				if (oprop != null) {
					try {
						TypeCheckInfo ptci = getModelValidator().getType(prop);
						if (!ptci.isList()) {
							addError("Something is wrong with this property; there is confusion about its type", prop);
						}
					} catch (DontTypeCheckException e) {
						e.printStackTrace();
					} catch (InvalidNameException e) {
						e.printStackTrace();
					} catch (TranslationException e) {
						e.printStackTrace();
					} catch (URISyntaxException e) {
						e.printStackTrace();
					} catch (IOException e) {
						e.printStackTrace();
					} catch (ConfigurationException e) {
						e.printStackTrace();
					} catch (InvalidTypeException e) {
						e.printStackTrace();
					} catch (CircularDependencyException e) {
						e.printStackTrace();
					} catch (PropertyWithoutRangeException e) {
						e.printStackTrace();
					}
				}
			}
			if (oprop == null) {
//				dumpModel(getTheJenaModel());
				addError(SadlErrorMessages.PROPERTY_NOT_EXIST.get(propuri), prop);
			} else {
				if (val instanceof SadlValueList) {
					// EList<SadlExplicitValue> vals = ((SadlValueList)val).getExplicitValues();
					// convert to SADL Typed List
					String typstr;
					try {
						typstr = getModelValidator().getType(val).getTypeCheckType().getURI();
						OntClass lstcls = getOrCreateListSubclass(null, typstr, prop.eResource(), null);
						Individual lval = getTheJenaModel().createIndividual(lstcls);
						addListValues(lval, lstcls, (SadlValueList) val);
						addInstancePropertyValue(inst, oprop, lval, subjCtx, val);
					} catch (InvalidNameException e) {
						e.printStackTrace();
					} catch (TranslationException e) {
						e.printStackTrace();
					} catch (URISyntaxException e) {
						e.printStackTrace();
					} catch (IOException e) {
						e.printStackTrace();
					} catch (ConfigurationException e) {
						e.printStackTrace();
					} catch (DontTypeCheckException e) {
						e.printStackTrace();
					} catch (InvalidTypeException e) {
						e.printStackTrace();
					} catch (CircularDependencyException e) {
						e.printStackTrace();
					} catch (PropertyWithoutRangeException e) {
						e.printStackTrace();
					}
//					addListValues(inst, cls, (SadlValueList) val);	// This was replaced by the code above because the
																	// list is the value of a triple, not elements of the subject (inst)
				} else if (val instanceof SadlResource) {
					try {
						Node nval = processExpression((SadlResource)val);
						if (nval != null && nval instanceof VariableNode) {
							
						}
					} catch (TranslationException e) {
						e.printStackTrace();
					}
				} else if (val instanceof SadlExplicitValue) {
					Literal lval = sadlExplicitValueToLiteral((SadlExplicitValue) val, oprop.getRange());
					if (inst != null && lval != null) {
						try {
							lval.getValue();
						}
						catch (Throwable t) {
							addError(t.getMessage(), val);
						}
						addInstancePropertyValue(inst, oprop, lval, subjCtx, val);
					}
				} else if (val instanceof SadlNestedInstance) {
// TODO missing case must be implemented.	
					addWarning("Unhandled SadlNestedInstance", val);
//					Iterator<SadlPropertyInitializer> propinititr = ((SadlNestedInstance) val)
//							.getPropertyInitializers().iterator();
//					while (propinititr.hasNext()) {
//						EObject pval = propinititr.next().getValue();
//						if (pval instanceof SadlNumberLiteral) {
//							org.apache.jena.rdf.model.Resource effectiveRng = getUnittedQuantityValueRange();
//							Literal lval = sadlExplicitValueToLiteral((SadlNumberLiteral) pval, effectiveRng);
//							if (lval != null) {
//								addInstancePropertyValue(inst, oprop, lval, val);
//							}
//						}
//					}
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
				addInstancePropertyValue(inst, annprop, rsrcval, subjCtx, val);
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
			addInstancePropertyValue(inst, rdfprop, rsrcval, subjCtx, val);
		} else if (type.equals(OntConceptType.VARIABLE)) {
			// a variable for a property type is only valid in a rule or query.
			if (getTarget() == null || getTarget() instanceof Test) {
				addError("Variable can be used for property only in queries and rules", val);
			}
		} else {
			throw new JenaProcessorException("unhandled property type");
		}
	}

	private org.apache.jena.rdf.model.Resource getUnittedQuantityValueRange() {
		org.apache.jena.rdf.model.Resource effectiveRng = getTheJenaModel()
				.getOntProperty(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI).getRange();
		if (effectiveRng == null) {
			effectiveRng = XSD.decimal;
		}
		return effectiveRng;
	}

	private void addInstancePropertyValue(Individual inst, Property prop, RDFNode value, EObject subjCtx, EObject valCtx) {
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
							if (value.equals(ipv)) {
								// they are the same implied property (maybe repeated in models)
								continue;
							}
							ExtendedIterator<? extends OntResource> ipvitr = ipv.as(OntProperty.class).listRange();
							while (ipvitr.hasNext()) {
								OntResource ipvr = ipvitr.next();
								if (valueRngLst.contains(ipvr)) {
									addError("Ambiguous condition--multiple implied properties ("
											+ value.as(OntProperty.class).getLocalName() + ","
											+ ipv.as(OntProperty.class).getLocalName() + ") have the same range ("
											+ ipvr.getLocalName() + ")", valCtx);
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
		try {
			
			getModelValidator().checkPropertyDomain(getTheJenaModel(), inst, prop, null, false, null, false, true);
		} catch (InvalidTypeException e) {
			e.printStackTrace();
		} catch (CircularDependencyException e) {
			addTypeCheckingError(e.getMessage(), subjCtx);
		} catch (TranslationException e) {
			e.printStackTrace();
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
		if ((unit !=  null && unit.startsWith("\"") && unit.endsWith("\"")) ||
				(unit !=  null && unit.startsWith("'") && unit.endsWith("'"))) {
			unit = unit.substring(1, unit.length() - 1);
		}
		unittedVal.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI),
				getTheJenaModel().createTypedLiteral(literalNumber, XSD.decimal.getURI()));
		unittedVal.addProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI),
				getTheJenaModel().createTypedLiteral(unit, XSD.xstring.getURI()));
		try {
			
			getModelValidator().checkPropertyDomain(getTheJenaModel(), inst, oprop, null, false, null, false, true);
		} catch (InvalidTypeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (CircularDependencyException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TranslationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

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

	protected SadlResource sadlResourceFromSadlInstance(SadlInstance element) throws JenaProcessorException {
		SadlResource sr = element.getNameOrRef();
		if (sr == null) {
			sr = element.getInstance();
		}
		return sr;
	}

	private void processSadlDisjointClasses(SadlDisjointClasses element) throws JenaProcessorException {
		List<OntClass> disjointClses = new ArrayList<OntClass>();
		if (element.getClasses() != null && !element.getClasses().isEmpty()) {
			Iterator<SadlResource> dcitr = element.getClasses().iterator();
			while (dcitr.hasNext()) {
				SadlResource sr = dcitr.next();
				String declUri = getDeclarationExtensions().getConceptUri(sr);
				if (declUri == null) {
					addError("Failed to get concept URI of SadlResource in processSadlDisjointClasses", sr);
				}
				OntClass cls = getTheJenaModel().getOntClass(declUri);
				if (cls == null) {
//					getTheJenaModel().write(System.err);
					addError("Failed to get class '" + declUri + "' from Jena model.", sr);
				}
				disjointClses.add(cls.asClass());
			}
		}
		else if (element.getTypes() != null && element.getTypes().size() > 0 &&
				element.getTypes().get(0) instanceof SadlClassOrPropertyDeclaration) {
			EList<SadlResource> cplst = ((SadlClassOrPropertyDeclaration)element.getTypes().get(0)).getClassOrProperty();
			if (cplst != null) {
				Iterator<SadlResource> cpitr = cplst.iterator();
				while (cpitr.hasNext()) {
					SadlResource sr = cpitr.next();
					String declUri = getDeclarationExtensions().getConceptUri(sr);
					if (declUri == null) {
						addError("Failed to get concept URI of SadlResource in processSadlDisjointClasses", sr);
					}
					OntClass cls = getTheJenaModel().getOntClass(declUri);
					if (cls == null) {
//						getTheJenaModel().write(System.err);
						addError("Failed to get class '" + declUri + "' from Jena model.", sr);
					}
					disjointClses.add(cls.asClass());
				}
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
	
	private void checkForExistingRangeOnPropertyForCondition(Property aProperty, RDFNode aRangeNode, EObject aCondition) {
		OntResource lRange = null;
		if (aProperty.canAs(DatatypeProperty.class)) {
			lRange = aProperty.as(DatatypeProperty.class).getRange();
		}else if(aProperty.canAs(ObjectProperty.class)) {
			lRange = aProperty.as(ObjectProperty.class).getRange();
		}
		if (lRange != null) {
			//Are ranges directly equivalent?
			if(lRange.equals(aRangeNode)) {
				return;
			}
			
			if(isIgnoreUnittedQuantities()) {
				if(lRange.isURIResource()) {
					if(lRange.getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI) && 
							(aRangeNode.canAs(OntResource.class) && aRangeNode.as(OntResource.class).getURI().equals(XSD.decimal.getURI()))) {
						return;
					}
					if(lRange.getURI().equals(XSD.decimal.getURI()) && aRangeNode.isURIResource() &&
							(aRangeNode.canAs(OntResource.class) && aRangeNode.as(OntResource.class).getURI().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI))) {
						return;
					}
				}
			}
			//If not list types, is new restriction a sub-type of original restriction?
			try {
				if(!isTypedListSubclass(lRange) && !isTypedListSubclass(aRangeNode)) {
					if(lRange.canAs(OntClass.class) && aRangeNode.canAs(OntClass.class) && SadlUtils.classIsSuperClassOf(lRange.as(OntClass.class), aRangeNode.as(OntClass.class))) {
						return;
					}
				}
			}catch(HasNoModelException e) {
				//Continue from here
			}
			//Union class comparisons
			if(lRange.canAs(UnionClass.class)) {
				for(OntClass lRangeOperand : lRange.as(UnionClass.class).listOperands().toList()) {
					if(lRangeOperand.equals(aRangeNode)) {
						return;
					}
					if(lRangeOperand.canAs(OntClass.class) && aRangeNode.canAs(OntClass.class) && SadlUtils.classIsSuperClassOf(lRangeOperand.as(OntClass.class), aRangeNode.as(OntClass.class))) {
						return;
					}
				}
			}
			if(aRangeNode.canAs(UnionClass.class)) {
				for(OntClass lRangeNodeOperand : aRangeNode.as(UnionClass.class).listOperands().toList()) {
					if(lRange.equals(lRangeNodeOperand)) {
						return;
					}
					if(lRange.canAs(OntClass.class) && lRangeNodeOperand.canAs(OntClass.class) && SadlUtils.classIsSuperClassOf(lRange.as(OntClass.class), lRangeNodeOperand.as(OntClass.class))) {
						return;
					}
				}
			}
			//Intersection class comparisons
			if(lRange.canAs(IntersectionClass.class)) {
				for(OntClass lRangeOperand : lRange.as(IntersectionClass.class).listOperands().toList()) {
					if(lRangeOperand.equals(aRangeNode)) {
						return;
					}
					if(lRangeOperand.canAs(OntClass.class) && aRangeNode.canAs(OntClass.class) && SadlUtils.classIsSuperClassOf(lRangeOperand.as(OntClass.class), aRangeNode.as(OntClass.class))) {
						return;
					}
				}
			}
			if(aRangeNode.canAs(IntersectionClass.class)) {
				for(OntClass lRangeNodeOperand : aRangeNode.as(IntersectionClass.class).listOperands().toList()) {
					if(lRange.equals(lRangeNodeOperand)) {
						return;
					}
					if(lRange.canAs(OntClass.class) && lRangeNodeOperand.canAs(OntClass.class) && SadlUtils.classIsSuperClassOf(lRange.as(OntClass.class), lRangeNodeOperand.as(OntClass.class))) {
						return;
					}
				}
			}
			//List types comparison
			if(isTypedListSubclass(lRange) && isTypedListSubclass(aRangeNode)) {
				try {
					NamedNode lPropertyRangeNN = getTypedListType(lRange);
					NamedNode lInputRangeNN = getTypedListType(aRangeNode);
					if(lPropertyRangeNN != null && lInputRangeNN != null) {
						if(lPropertyRangeNN.equals(lInputRangeNN)) {
							return;
						}
					}
				} catch (TranslationException e) {
					e.printStackTrace();
				}
			}
			
			for(Statement stmt : getTheJenaModel().listStatements(null, OWL.onProperty, aProperty).toList()) {
				org.apache.jena.rdf.model.Resource subjectResource = stmt.getSubject();
				if(subjectResource.canAs(AllValuesFromRestriction.class)) {
					org.apache.jena.rdf.model.Resource avf = subjectResource.as(AllValuesFromRestriction.class).getAllValuesFrom();
					if(avf.equals(aRangeNode)) {
						return;
					}
					if(avf.canAs(OntClass.class) && aRangeNode.canAs(OntClass.class) && SadlUtils.classIsSuperClassOf(avf.as(OntClass.class), aRangeNode.as(OntClass.class))) {
						return;
					}
				}
			}
			
			addWarning(SadlErrorMessages.RANGE_MAY_BE_INAPPLICABLE.get(
					UtilsForJena.nodeToString(theJenaModel, getConfigMgr(), getModelNamespace(), aProperty), 
					UtilsForJena.nodeToString(theJenaModel, getConfigMgr(), getModelNamespace(), lRange)), aCondition);
		}
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
					sb.append(UtilsForJena.nodeToString(theJenaModel, getConfigMgr(), getModelNamespace(), prop));
					sb.append("' is a subclass of the domain which is already defined");
					String dmnstr = UtilsForJena.nodeToString(theJenaModel, getConfigMgr(), getModelNamespace(), existingDomain);
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
			if (isDomainAndRangeAsUnionClasses()) {
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
				addWarning(SadlErrorMessages.IMPORTED_DOMAIN_CHANGE.get(
						UtilsForJena.nodeToString(theJenaModel, getConfigMgr(), getModelNamespace(), prop)), context);
			}
		} // end if existing domain in any model, this or imports
		if (cls != null) {
			if (!isDomainAndRangeAsUnionClasses() && cls instanceof UnionClass) {
				List<org.apache.jena.rdf.model.Resource> uclsmembers = getUnionClassMemebers((UnionClass) cls);
				for (int i = 0; i < uclsmembers.size(); i++) {
					getTheJenaModel().add(prop, RDFS.domain, uclsmembers.get(i));
					logger.debug(
							"Domain '" + uclsmembers.get(i).toString() + "' added to property '" + prop.getURI() + "'");
				}
			} else if (addNewDomain) {
				getTheJenaModel().add(prop, RDFS.domain, cls);
				logger.debug("Domain '" + cls.toString() + "' added to property '" + prop.getURI() + "'");
				logger.debug("Domain of '" + prop.toString() + "' is now: " + 
				UtilsForJena.nodeToString(theJenaModel, getConfigMgr(), getModelNamespace(), cls));
			}
		} else {
			logger.debug("Domain is not defined for property '" + prop.toString() + "'");
		}
	}

	private List<org.apache.jena.rdf.model.Resource> getUnionClassMemebers(UnionClass cls) {
		List<org.apache.jena.rdf.model.Resource> members = null;
		ExtendedIterator<? extends org.apache.jena.rdf.model.Resource> itr = ((UnionClass) cls).listOperands();
		while (itr.hasNext()) {
			org.apache.jena.rdf.model.Resource ucls = itr.next();
			if (ucls instanceof UnionClass || ucls.canAs(UnionClass.class)) {
				List<org.apache.jena.rdf.model.Resource> nested = getUnionClassMemebers(ucls.as(UnionClass.class));
				if (members == null) {
					members = nested;
				} else {
					members.addAll(nested);
				}
			} else {
				if (members == null)
					members = new ArrayList<org.apache.jena.rdf.model.Resource>();
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

	/**
	 * Method to find a SADL List subclass or, if none if found, create a new one.
	 * If a matching subclass is an anonymous class, it must be in the current model (not an imported model).
	 * Otherwise create a new subclass in this model.
	 * @param newName
	 * @param typeUri
	 * @param resource
	 * @param facet
	 * @return
	 * @throws JenaProcessorException
	 */
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
		String length = "-1";
		String lengthMax = "-1";
		String lengthMin = "-1";
		if(facet != null) {
			length = facet.getLen() != null ? facet.getLen() : "-1";
			lengthMax = facet.getMaxlen() != null ? facet.getMaxlen() : "-1";
			lengthMin = facet.getMinlen() != null ? facet.getMinlen() : "-1";
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
				// we are matching anonymous classes
				// to keep looking, the scls must be in the current (base) model
				if (!getTheJenaModel().getBaseModel().containsResource(scls)) {
					continue;
				}
				String restrictionTypeUri = "";
				String lengthRestriction = "-1";
				String lengthMaxRestriction = "-1";
				String lengthMinRestriction = "-1";
				ExtendedIterator<OntClass> spcitr = scls.listSuperClasses(true);
				while (spcitr.hasNext()) {
					OntClass spcls = spcitr.next();
					if (spcls.isRestriction()) {
						Restriction restriction = spcls.asRestriction();
						OntProperty onprop = restriction.getOnProperty();
						if(restriction.isAllValuesFromRestriction()) {	
							if (onprop.isURIResource() && onprop.getURI().equals(SadlConstants.SADL_LIST_MODEL_FIRST_URI)) {
								org.apache.jena.rdf.model.Resource avf = restriction.asAllValuesFromRestriction().getAllValuesFrom();
								if(avf.isURIResource()) {
									restrictionTypeUri = avf.getURI();
								}
							}
						}
						if(restriction.isHasValueRestriction()) {
							RDFNode hasValue = restriction.asHasValueRestriction().getHasValue();
							if(onprop.isURIResource() && onprop.getURI().equals(SadlConstants.SADL_LIST_MODEL_LENGTH_RESTRICTION_URI)) {
								lengthRestriction = String.valueOf(hasValue.asLiteral().getInt());
							}
							if(onprop.isURIResource() && onprop.getURI().equals(SadlConstants.SADL_LIST_MODEL_MAXLENGTH_RESTRICTION_URI)) {
								lengthMaxRestriction = String.valueOf(hasValue.asLiteral().getInt());
							}
							if(onprop.isURIResource() && onprop.getURI().equals(SadlConstants.SADL_LIST_MODEL_MINLENGTH_RESTRICTION_URI)) {
								lengthMinRestriction = String.valueOf(hasValue.asLiteral().getInt());
							}
						}
					}
				}
				if (restrictionTypeUri.equals(typeUri)) {
					if(facet == null) {
						if(typeUri.contains(XSD.getURI())) {
							if(lengthRestriction.equals("-1") &&
							   lengthMaxRestriction.equals("-1") &&
							   lengthMinRestriction.equals("-1")) {
								spcitr.close();
								lscitr.close();
								return scls;
							}	
						}else {
							spcitr.close();
							lscitr.close();
							return scls;
						}
					}else {
						if(lengthRestriction.equals(length) &&
						   lengthMaxRestriction.equals(lengthMax) &&
						   lengthMinRestriction.equals(lengthMin)) {
							spcitr.close();
							lscitr.close();
							return scls;
						}
					}
				}
			}
		}
		OntClass newcls = createOntClass(newName, lstcls);
		org.apache.jena.rdf.model.Resource typeResource = getTheJenaModel().getResource(typeUri);
		Property pfirst = getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI);
		AllValuesFromRestriction avf = getTheJenaModel().createAllValuesFromRestriction(null, pfirst, typeResource);
		newcls.addSuperClass(avf);
		Property prest = getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_REST_URI);
		AllValuesFromRestriction avf2 = getTheJenaModel().createAllValuesFromRestriction(null, prest, newcls);
		newcls.addSuperClass(avf2);
		if(facet != null) {
			addLengthRestrictionsToList(newcls, facet);
		}
		
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
				superProp = getTheJenaModel().createObjectProperty(superSRUri);
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
		logger.debug("New RDF property '" + newProp.getURI() + "' created");
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
	
	private Individual getOrCreateIndividual(SadlResource isr) throws CircularDefinitionException, JenaProcessorException {
		Individual inst = null;
		String declUri = getDeclarationExtensions().getConceptUri(isr);
		if (declUri != null) {
			OntConceptType type = getDeclarationExtensions().getOntConceptType(isr);
			if (type != null && !type.equals(OntConceptType.INSTANCE)) {
				addTypeCheckingError("Expected an instance.", isr);
			}
			else {
				inst = getTheJenaModel().getIndividual(declUri);
				if (inst == null) {
					SadlResource nsr = isr.getName();
					if (nsr != isr) {
						inst = getOrCreateIndividual(nsr);
					}
					else {
						if (nsr.eContainer() instanceof SadlInstance) {
							inst = processSadlInstance((SadlInstance) nsr.eContainer());
						}
					}
				}
			}
		}
		if (inst == null) {
			addError("Failed to find instance", isr);
		}
		return inst;
	}

	private Individual createIndividual(String newName, OntClass supercls) {
		Individual inst = getTheJenaModel().createIndividual(newName, supercls);
		logger.debug("New instance '" + (newName != null ? newName : "(bnode)") + "' created");
		return inst;
	}

	private OntResource sadlTypeReferenceToOntResource(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		org.apache.jena.rdf.model.Resource obj = sadlTypeReferenceToResource(sadlTypeRef);
		if (obj == null) {
			return null; // this happens when sadlTypeRef is a variable (even if unintended)
		}
		if (obj instanceof OntResource) {
			return (OntResource) obj;
		} else if (obj instanceof RDFNode) {
			if (((RDFNode) obj).canAs(OntResource.class)) {
				return ((RDFNode) obj).as(OntResource.class);
			}
			else if (obj.isURIResource()){
				return getTheJenaModel().getOntResource(obj.getURI());
			}
		}
		throw new JenaProcessorException("Unable to convert SadlTypeReference '" + sadlTypeRef + "' to OntResource");
	}

	private org.apache.jena.rdf.model.Resource sadlTypeReferenceToResource(SadlTypeReference sadlTypeRef)
			throws JenaProcessorException {
		if (getCachedJenaResource(sadlTypeRef) != null) {
			return getCachedJenaResource(sadlTypeRef);
		}
		Object obj = sadlTypeReferenceToObject(sadlTypeRef);
		if (obj == null) {
			return null; // this happens when sadlTypeRef is a variable (even if unintended)
		}
		org.apache.jena.rdf.model.Resource newJenaResource = null;
		if (obj instanceof org.apache.jena.rdf.model.Resource) {
			newJenaResource = (org.apache.jena.rdf.model.Resource) obj;
		} else if (obj instanceof RDFNode) {
			if (((RDFNode) obj).canAs(org.apache.jena.rdf.model.Resource.class)) {
				newJenaResource = ((RDFNode) obj).as(org.apache.jena.rdf.model.Resource.class);
			}
		}
		if (newJenaResource != null) {
			addCachedJenaResource(sadlTypeRef, newJenaResource);
			return newJenaResource;
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
			org.apache.jena.rdf.model.Resource trr = getSadlPrimitiveDataTypeResource(
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
			SadlDataTypeFacet lFacet = getSadlDataTypeFacetFromSadlTypeReference(sadlTypeRef);
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
					rsrc = getOrCreateListSubclass(null, strSRUri, sadlTypeRef.eResource(), lFacet);
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
						return getOrCreateListSubclass(strSRUri, strSRUri, strSR.eResource(), lFacet);
					}
				}
			} else if (ctype.equals(OntConceptType.DATATYPE_LIST)) {
				rsrc = getTheJenaModel().getOntClass(strSRUri);
				if (rsrc == null) {
					return getOrCreateListSubclass(strSRUri, strSRUri, strSR.eResource(), lFacet);
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
			} else if (ctype.equals(OntConceptType.RDF_PROPERTY)) {
				OntProperty rdfp = getTheJenaModel().getOntProperty(strSRUri);
				if (rdfp == null) {
					throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri
							+ "' not found; should have found an RDFProperty");
				}
				return rdfp;
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
				((List<RDFNode>) lftObj).add(rhtNode);
				return lftObj;
			} else if (lftObj instanceof RDFNode && rhtNode instanceof List) {
				((List<RDFNode>) rhtNode).add(lftNode);
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
			if (rhtObj instanceof OntResource && ((OntResource)rhtObj).isClass()) {
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
				// check to see if either is a subclass of the other. If so give a warning and use only the 
				// more restrictive of the two
				OntClass intersectCls = null;
				if (lftNode.isURIResource() && rhtNode.isURIResource() && 
						lftNode.asResource().getURI().equals(rhtNode.asResource().getURI())) {
					addError("Intersection of a class with itself is invalid", sadlTypeRef);
				} else {
					try {
						if (lftNode.canAs(OntClass.class) && 
								classIsSubclassOfCached(lftNode.as(OntClass.class), (OntResource) rhtNode, true, null)) {
							addWarning("Left class of intersection is a subclass of right class of intersection--using left class", sadlTypeRef);
							intersectCls = lftNode.as(OntClass.class);
						}
						else if (rhtNode.canAs(OntClass.class) && 
								classIsSubclassOfCached(rhtNode.as(OntClass.class), (OntResource) lftNode, true, null)) {
							addWarning("Right class of intersection is a subclass of left class of intersection--using right class", sadlTypeRef);
							intersectCls = rhtNode.as(OntClass.class);
						}
					} catch (CircularDependencyException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				if (intersectCls == null) {
					intersectCls = createIntersectionClass(lftNode, rhtNode);
				}
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
		else if (sadlTypeRef instanceof SadlTableDeclaration) {
			if (((SadlTableDeclaration)sadlTypeRef).getLocation() != null) {
				addError("A table subclass declaration cannot have a location; only table instances may have a location.", sadlTypeRef);
			}
			else if (((SadlTableDeclaration)sadlTypeRef).getValueTable() != null) {
				addError("A table subclass declaration cannot have data; only table instances may have data.", sadlTypeRef);
			}
			addError("Table subclass not yet handled", sadlTypeRef);
		}
		return rsrc;
	}

	/**
	 * Method to get the Facet from a SadlSimpleTypeReference or SadlPrimitiveDataType, both subclasses of SadlTypeReference
	 * @param SadlTypeReference object
	 * @return SadlDataTypeFacet object
	 */
	private SadlDataTypeFacet getSadlDataTypeFacetFromSadlTypeReference(SadlTypeReference aTypeRef) {
		EObject lContainer = aTypeRef.eContainer();
		if(lContainer instanceof SadlRangeRestriction) {
			return ((SadlRangeRestriction)lContainer).getFacet();
		}else if(lContainer instanceof SadlAllValuesCondition) {
			return ((SadlAllValuesCondition)lContainer).getFacet();
		}else if(lContainer instanceof SadlCardinalityCondition) {
			return ((SadlCardinalityCondition)lContainer).getFacet();
		}else if(lContainer instanceof SadlClassOrPropertyDeclaration) {
			return ((SadlClassOrPropertyDeclaration)lContainer).getFacet();
		}
		return null;
	}

	private org.apache.jena.rdf.model.Resource processSadlPrimitiveDataType(SadlClassOrPropertyDeclaration element,
			SadlPrimitiveDataType sadlTypeRef, String newDatatypeUri) throws JenaProcessorException {
		org.apache.jena.rdf.model.Resource onDatatype = getSadlPrimitiveDataTypeResource(sadlTypeRef);
		if (sadlTypeRef.isList()) {
			SadlDataTypeFacet lFacet = null;
			if(element == null) {
				lFacet = getSadlDataTypeFacetFromSadlTypeReference(sadlTypeRef);
			}else {
				lFacet = element.getFacet();
			}
			
			onDatatype = getOrCreateListSubclass(null, onDatatype.toString(), sadlTypeRef.eResource(), lFacet);
		}
		if (newDatatypeUri == null) {
			return onDatatype;
		}
		SadlDataTypeFacet facet = element.getFacet();
		try {
			OntClass datatype = createRdfsDatatype(newDatatypeUri, null, onDatatype, facet);
			return datatype;
		}
		catch (JenaProcessorException e) {
			addError(e.getMessage(), element);
			return null;
		}
	}

	private org.apache.jena.rdf.model.Resource getSadlPrimitiveDataTypeResource(SadlPrimitiveDataType sadlTypeRef)
			throws JenaProcessorException {
		SadlDataType pt = sadlTypeRef.getPrimitiveType();
		String typeStr = pt.getLiteral();
		org.apache.jena.rdf.model.Resource onDatatype = primitiveDataTypeLocalnameToJenaResource(typeStr);
		return onDatatype;
	}

	public static org.apache.jena.rdf.model.Resource primitiveDataTypeLocalnameToJenaResource(String typeStr)
			throws JenaProcessorException {
		org.apache.jena.rdf.model.Resource onDatatype;
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
		else if (typeStr.equals(XSD.xshort.getLocalName()))
			onDatatype = XSD.xshort;
		else if (typeStr.equals(XSD.anyURI.getLocalName()))
			onDatatype = XSD.anyURI;
		else if (typeStr.equals(XSD.anyURI.getLocalName()))
			onDatatype = XSD.anyURI;
		else if (typeStr.equals(XSD.positiveInteger.getLocalName()))
			onDatatype = XSD.positiveInteger;
		else if (typeStr.equals(XSD.negativeInteger.getLocalName()))
			onDatatype = XSD.negativeInteger;
		else if (typeStr.equals(XSD.nonPositiveInteger.getLocalName()))
			onDatatype = XSD.nonPositiveInteger;
		else if (typeStr.equals(XSD.nonNegativeInteger.getLocalName()))
			onDatatype = XSD.nonNegativeInteger;
		else if (typeStr.equals(XSD.normalizedString.getLocalName()))
			onDatatype = XSD.normalizedString;
		else if (typeStr.equals(XSD.unsignedByte.getLocalName())) 
			onDatatype = XSD.unsignedByte;
		else if (typeStr.equals(XSD.unsignedInt.getLocalName()))
			onDatatype = XSD.unsignedInt;
		else if (typeStr.equals(XSD.unsignedLong.getLocalName()))
			onDatatype = XSD.unsignedLong;
		else if (typeStr.equals(XSD.unsignedShort.getLocalName()))
			onDatatype = XSD.unsignedShort;
		else if (typeStr.equals(XSD.language.getLocalName()))
			onDatatype = XSD.language;
		else {
			throw new JenaProcessorException("Unexpected primitive data type: " + typeStr);
		}
		return onDatatype;
	}

	private OntClass createRdfsDatatype(String newDatatypeUri, List<RDFNode> unionOfTypes,
			org.apache.jena.rdf.model.Resource onDatatype, SadlDataTypeFacet facet) throws JenaProcessorException {
		OntClass datatype = getTheJenaModel().createOntResource(OntClass.class, RDFS.Datatype, newDatatypeUri);
		OntClass equivClass = getTheJenaModel().createOntResource(OntClass.class, RDFS.Datatype, null);
		if (onDatatype != null) {
			equivClass.addProperty(OWL2.onDatatype, onDatatype);
			if (facet != null) {
				RDFList list = getTheJenaModel().createList(facetsToRestrictionNodes(newDatatypeUri, facet, onDatatype));
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
		// The above code added the RDFDatatype to the OntModel but not to the TypeMapper
		addRDFDatatypeToTypeMapper(newDatatypeUri, onDatatype, unionOfTypes, facet);
		return datatype;
	}

	/**
	 * Method to determine if an OntResource is an RDFDatatype 
	 * 	(which property value should have been set when defined) 
	 * @param range
	 * @return
	 */
	protected boolean conceptIsRDFDatatype(OntResource range) {
		StmtIterator itr = getTheJenaModel().listStatements(range, RDF.type, RDFS.Datatype);
		if (itr.hasNext()) {
			itr.close();
			return true;
		}
		return false;
	}

	private void addRDFDatatypeToTypeMapper(String newDatatypeUri, org.apache.jena.rdf.model.Resource onDatatype,
			List<RDFNode> unionOfTypes, SadlDataTypeFacet facet) throws JenaProcessorException {
		RDFDatatype type = null;
		String ns;
		String ln;
		if (newDatatypeUri.indexOf('#') > 0) {
			ns = newDatatypeUri.substring(0, newDatatypeUri.indexOf('#'));
			ln = newDatatypeUri.substring(newDatatypeUri.indexOf('#') + 1);
		}
		else {
			throw new JenaProcessorException("Invalid user-defined URI");
		}
		if (onDatatype != null) {
			RDFDatatype basetype = TypeMapper.getInstance().getSafeTypeByName(onDatatype.getURI());
			Object basesimpletype = basetype.extendedTypeDefinition();
			
			XSSimpleType nst = org.apache.jena.ext.xerces.impl.dv.xs.BaseDVFactory.getInstance().createTypeRestriction(ln, ns,
                    (short)0, (org.apache.jena.ext.xerces.impl.dv.XSSimpleType) basesimpletype, null);
			if (facet == null) {
				throw new JenaProcessorException("A new datatype is expected to have facets to distinguish it from the base type");
			}
			Object[] facetInfo = sadlFacetsToXSFacets(facet);
			XSFacets facets = null;
			short presentFacet = 0;
			short fixedFacet = 0;
			ValidationContext context = null;
			if (facetInfo != null && facetInfo.length == 2) {
				facets = (XSFacets) facetInfo[0];
				presentFacet = (short) facetInfo[1];
			}
			try {
				nst.applyFacets(facets, presentFacet, fixedFacet, context);
				type = new SadlXSDDatatype(nst, ns);
			} catch (InvalidDatatypeFacetException e) {
				throw new JenaProcessorException(e.getMessage(), e);
			}
		}
		else if (unionOfTypes != null && unionOfTypes.size() > 0) {
			XSSimpleTypeDecl[] xsstdtypes = new XSSimpleTypeDecl[unionOfTypes.size()];
			for (int i = 0; i < unionOfTypes.size(); i++) {
				RDFNode umember = unionOfTypes.get(i);
				if (umember.isURIResource()) {
					Object memetd = TypeMapper.getInstance().getSafeTypeByName(umember.asResource().getURI()).extendedTypeDefinition();
					if (memetd instanceof XSSimpleTypeDecl) {
						xsstdtypes[i] = (XSSimpleTypeDecl)memetd;
					}
				}
			}
			XSSimpleType nst = org.apache.jena.ext.xerces.impl.dv.xs.BaseDVFactory.getInstance().createTypeUnion(ln, ns, (short)0, xsstdtypes, null);
			type = new SadlXSDDatatype(nst, ns);
		}
		if (type != null) {
			TypeMapper.getInstance().registerDatatype(type);
		}
		else {
			if (onDatatype != null) {
				throw new JenaProcessorException("Base type '" + onDatatype.getURI() + "' not handled");
			}
		}
	}

	private RDFNode[] facetsToRestrictionNodes(String newName, SadlDataTypeFacet facet, org.apache.jena.rdf.model.Resource onDatatype) {
		List<RDFNode> restrictions = new ArrayList<RDFNode>();
		if (facet.getMin() != null) {
			Literal minlit;
			if (onDatatype.isURIResource()) {
				minlit = getTheJenaModel().createTypedLiteral(facet.getMin(), onDatatype.getURI());
			}
			else {
				minlit = getTheJenaModel().createLiteral(facet.getMin());
			}
			org.apache.jena.rdf.model.Resource anon = getTheJenaModel().createResource();
			anon.addProperty(xsdProperty(facet.isMinInclusive() ? "minInclusive" : "minExclusive"), minlit); // "" +  facet.getMin());
			restrictions.add(anon);
		}
		if (facet.getMax() != null) {
			Literal maxlit;
			if (onDatatype.isURIResource()) {
				maxlit = getTheJenaModel().createTypedLiteral(facet.getMax(), onDatatype.getURI());
			}
			else {
				maxlit = getTheJenaModel().createLiteral(facet.getMax());
			}
			org.apache.jena.rdf.model.Resource anon = getTheJenaModel().createResource();
			anon.addProperty(xsdProperty(facet.isMaxInclusive() ? "maxInclusive" : "maxExclusive"), maxlit); // "" + facet.getMax());
			restrictions.add(anon);
		}
		
		if (facet.getLen() != null) {
			org.apache.jena.rdf.model.Resource anon = getTheJenaModel().createResource();
			anon.addProperty(xsdProperty("length"), "" + facet.getLen());
			restrictions.add(anon);
		}
		if (facet.getMinlen() != null) {
			org.apache.jena.rdf.model.Resource anon = getTheJenaModel().createResource();
			anon.addProperty(xsdProperty("minLength"), "" + facet.getMinlen());
			restrictions.add(anon);
		}
		if (facet.getMaxlen() != null && !facet.getMaxlen().equals("*")) {
			org.apache.jena.rdf.model.Resource anon = getTheJenaModel().createResource();
			anon.addProperty(xsdProperty("maxLength"), "" + facet.getMaxlen());
			restrictions.add(anon);
		}
		if (facet.getRegex() != null) {
			org.apache.jena.rdf.model.Resource anon = getTheJenaModel().createResource();
			anon.addProperty(xsdProperty("pattern"), "" + facet.getRegex());
			restrictions.add(anon);
		}
		if (facet.getValues() != null) {
			Iterator<String> iter = facet.getValues().iterator();
			org.apache.jena.rdf.model.Resource anon = null;
			while (iter.hasNext()) {
				if (anon == null) {
					anon = getTheJenaModel().createResource();
				}
				anon.addProperty(xsdProperty("enumeration"), iter.next());
			}
			if (anon != null) {
				restrictions.add(anon);
			}
		}
		return restrictions.toArray(new RDFNode[0]);	
	}

	/**
	 * Method to convert the SADL facets on a user-defined data type to an XSFacets
	 * @param facet
	 * @return
	 */
	private Object[] sadlFacetsToXSFacets(SadlDataTypeFacet facet) {
		XSFacets xsfacets = new XSFacets();
		short facetsPresent = 0;
		if (facet == null) {
			return null;
		}
		if (facet.getMin() != null) {
			if (facet.isMinInclusive()) {
				xsfacets.minInclusive = facet.getMin();
				facetsPresent = (short) (facetsPresent | org.apache.jena.ext.xerces.xs.XSSimpleTypeDefinition.FACET_MININCLUSIVE);
			}
			else {
				xsfacets.minExclusive = facet.getMin();
				facetsPresent = (short) (facetsPresent | org.apache.jena.ext.xerces.xs.XSSimpleTypeDefinition.FACET_MINEXCLUSIVE);
			}
		}
		
		if (facet.getMax() != null) {
			if (facet.isMaxInclusive()) {
				xsfacets.maxInclusive = facet.getMax();
				facetsPresent = (short) (facetsPresent | org.apache.jena.ext.xerces.xs.XSSimpleTypeDefinition.FACET_MAXINCLUSIVE);
			}
			else {
				xsfacets.maxExclusive = facet.getMax();
				facetsPresent = (short) (facetsPresent | org.apache.jena.ext.xerces.xs.XSSimpleTypeDefinition.FACET_MAXEXCLUSIVE);
			}
		}
		
		if (facet.getLen() != null) {
			xsfacets.length = Integer.parseInt(facet.getLen());
			facetsPresent = (short) (facetsPresent | org.apache.jena.ext.xerces.xs.XSSimpleTypeDefinition.FACET_LENGTH);
		}
		if (facet.getMinlen() != null) {
			xsfacets.minLength = Integer.parseInt(facet.getMinlen());
			facetsPresent = (short) (facetsPresent | org.apache.jena.ext.xerces.xs.XSSimpleTypeDefinition.FACET_MINLENGTH);
		}
		if (facet.getMaxlen() != null && !facet.getMaxlen().equals("*")) {
			xsfacets.maxLength = Integer.parseInt(facet.getMaxlen());
			facetsPresent = (short) (facetsPresent | org.apache.jena.ext.xerces.xs.XSSimpleTypeDefinition.FACET_MAXLENGTH);
		}
		if (facet.getRegex() != null) {
			xsfacets.pattern = facet.getRegex();
			facetsPresent = (short) (facetsPresent | org.apache.jena.ext.xerces.xs.XSSimpleTypeDefinition.FACET_PATTERN);
		}
		if (facet.getValues() != null) {
			Iterator<String> iter = facet.getValues().iterator();
			Vector<String> vector = new Vector<String>();
			while (iter.hasNext()) {
				String val = iter.next();
				// for some reason these values end up with quotes as part of the string????
				val = SadlUtils.stripQuotes(val);
				vector.add(val);
			}
			if (vector.size() > 0) {
				xsfacets.enumeration = vector;
				facetsPresent = (short) (facetsPresent | org.apache.jena.ext.xerces.xs.XSSimpleTypeDefinition.FACET_ENUMERATION);
			}
		}
		Object[] results = new Object[2];
		results[0] = xsfacets;
		results[1] = facetsPresent;
		return results;
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
			if(type instanceof SadlSimpleTypeReference) {
				SadlResource sr = ((SadlSimpleTypeReference)type).getType();
				if(sr.eContainer() instanceof SadlClassOrPropertyDeclaration) {
					try {
						if(!isEObjectPreprocessed((SadlClassOrPropertyDeclaration)sr.eContainer())) {
							processSadlClassOrPropertyDeclaration((SadlClassOrPropertyDeclaration)sr.eContainer());
							eobjectPreprocessed((SadlClassOrPropertyDeclaration)sr.eContainer());
						}
					} catch (TranslationException e) {
						e.printStackTrace();
					}
				}
			}
			org.apache.jena.rdf.model.Resource typersrc = sadlTypeReferenceToResource(type);
			if (typersrc == null) {
				addError(SadlErrorMessages.CANNOT_CREATE.get("all values from restriction",
						"restriction on unresolvable property value restriction"), type);
			} else {
				checkForExistingRangeOnPropertyForCondition(prop, typersrc, cond);
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
					String valUri = getDeclarationExtensions().getConceptUri(srValue);
					if (srType.equals(OntConceptType.INSTANCE)) {
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
					} else if (isProperty(srType)) {
						if (isTypeCheckingWarningsOnly()) {
							addWarning("Value restriction is not expected to be a property", cond);
						}
						else {
							addError("Value restriction is not expected to be a property", cond);
						}
						val = getTheJenaModel().getProperty(valUri);
					} else {
						if (isTypeCheckingWarningsOnly()){
							addWarning("Value restriction is expected to resolve to an instance in the model", cond);
						}
						else {
							addError(
								"Value restriction is expected to resolve to an instance in the model", cond);
						}
						val = getTheJenaModel().getResource(valUri);
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
//				Individual valInst = val.as(Individual.class);
				if (prop.canAs(OntProperty.class)
						&& valueInObjectTypePropertyRange(prop.as(OntProperty.class), val, cond)) {
					HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, prop, val);
					logger.debug("New has value restriction on '" + prop.getURI() + "' to value '" + val.toString()
							+ "'");
					retval = hvr;
				} else {
					throw new JenaProcessorException(
							SadlErrorMessages.NOT_IN_RANGE.get(val.toString(), prop.getURI()));
				}
			} else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
				if (prop != null && val != null && prop.canAs(OntProperty.class) && val.isLiteral()
						&& valueInDatatypePropertyRange(prop.as(OntProperty.class), val.asLiteral(), cond)) {
					HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, prop, val);
					logger.debug(
							"New has value restriction on '" + prop.getURI() + "' to value '" + val.toString() + "'");
					retval = hvr;
				} else if (val != null) {
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
			if (type != null && typersrc == null) {
				addError("The type could not be resolved", type);
			}
			else if (cardinality == null) {
				// this is probably an incomplete statement, 
				addError("Invalid cardinality", cond);
			}
			else if (cardinality.equals("one")) {
				// this is interpreted as a someValuesFrom restriction
				SomeValuesFromRestriction svf = null;
				if (type == null) {
					throw new JenaProcessorException("'one' means some value from class so a type must be given");
				}
				else if (((SadlCardinalityCondition)cond).getOperator().equals("most")) {
					addError("From the keyword 'most', it appears that a cardinality restriction is desired. Please use '1' rather than 'one' for cardinality.", cond);
				}
				else {
					svf = getTheJenaModel().createSomeValuesFromRestriction(null, prop, typersrc);
					logger.debug("New some values from restriction on '" + prop.getURI() + "' to values of type '"
							+ typersrc.toString() + "'");
				}
				retval = svf;
			} else {
				// cardinality restriction
				int cardNum = Integer.parseInt(cardinality);
				String op = ((SadlCardinalityCondition) cond).getOperator();
				Property cardProp = null;
				Property onProp = null;
				if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
					onProp = OWL2.onClass;
				}
				else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)){
					onProp = OWL2.onDataRange;
				}
				else if (typersrc != null){
					if (typersrc.canAs(DataRange.class)) {
						onProp = OWL2.onDataRange;
					}
					else if (typersrc.canAs(OntClass.class)) {
						onProp = OWL2.onClass;
					}
					else {
						addError("Unable to determine whether qualified cardinality is to a class or a data range", type);
					}
				}
				
				if (op == null) {
					if (type != null) {
						cardProp = OWL2.qualifiedCardinality;
					}
					else {
						cardProp = OWL.cardinality;
					}
				} else if (op.equals("least")) {
					if (type != null) {
						cardProp = OWL2.minQualifiedCardinality;
					}
					else {
						cardProp = OWL.minCardinality;
					}
				} else if (op.equals("most")) {
					if (type != null) {
						cardProp = OWL2.maxQualifiedCardinality;
					}
					else {
						cardProp = OWL.maxCardinality;
					}
				}
				if (logger.isDebugEnabled()) {
					if (type != null) {
						logger.debug("   cardinality is qualified; values must be of type '" + typersrc + "'");
					}
				}
				if (cardProp != null && onProp != null && type != null) {
					retval = createQualifiedCardinalityRestriction(prop, type, typersrc, cardProp, onProp, cardNum);
				}
				else if (cardProp != null) {
					retval = createCardinalityRestriction(prop, cardProp, cardNum);
				}
				else {
					addError("Failed to set qualified cardinality", cond);
				}
			}
		} else {
			throw new JenaProcessorException("Unhandled SadlCondition type: " + cond.getClass().getCanonicalName());
		}
		return retval;
	}

	/**
	 * Method to create a CardinalityRestriction
	 * @param prop -- property restricted
	 * @param cardProp -- cardinality restriction property
	 * @param cardNum -- cardinality
	 * @return
	 */
	private OntClass createCardinalityRestriction(Property prop, Property cardProp, int cardNum) {
		if (cardProp.equals(OWL.cardinality)) {
			return getTheJenaModel().createCardinalityRestriction(null, prop, cardNum);
		}
		else if (cardProp.equals(OWL.minCardinality)) {
			return getTheJenaModel().createMinCardinalityRestriction(null, prop, cardNum);
		}
		else if (cardProp.equals(OWL.maxCardinality)) {
			return getTheJenaModel().createMaxCardinalityRestriction(null, prop, cardNum);
		}
		return null;
	}

	/**
	 * Method to create a QualifiedCardinalityRestriction
	 * @param prop
	 * @param typeRef
	 * @param typeOntRsrc
	 * @param cardNum
	 * @return
	 */
	private CardinalityRestriction createQualifiedCardinalityRestriction(Property prop, SadlTypeReference typeRef,
			OntResource typeOntRsrc, Property cardProperty, Property toProperty, int cardNum) {
		CardinalityRestriction cr = getTheJenaModel().createCardinalityRestriction(null, prop, cardNum);
		logger.debug("New cardinality restriction " + cardNum + " on '" + prop.getURI() + "' created");
		if (typeRef != null) {
			cr.removeAll(OWL.cardinality);
			cr.addLiteral(cardProperty, cardNum);
			cr.addProperty(toProperty, typeOntRsrc);
		}
		return cr;
	}

	private boolean valueInDatatypePropertyRange(OntProperty prop, Literal val, EObject cond) {
		try {
			if (getModelValidator() != null) {
				return getModelValidator().checkDataPropertyValueInRange(getTheJenaModel(), null, prop, val);
			}
		} catch (TranslationException e) {
			e.printStackTrace();
		}
		return true;
	}

	protected Literal sadlExplicitValueToLiteral(SadlExplicitValue value, org.apache.jena.rdf.model.Resource rng)
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
				if (rng != null && rng.getURI().equals(XSD.duration.getURI()) && 
						((SadlNumberLiteral) value).getUnit() != null) {
					val = val + " " + ((SadlNumberLiteral)value).getUnit();
				}
				if (isNegated) {
					val = "-" + val;
				}
				if (rng != null && rng.getURI() != null) {
					try {
						return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), val);
					}
					catch (TranslationException e) {
						addTypeCheckingError(e.getMessage(), value);
					}
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
					if (rng != null) {
						return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), cv);
					}
					else {
						return getTheJenaModel().createTypedLiteral(cv);
					}
				} else if (val.equals(SadlConstants.CONSTANT_E)) {
					double cv = Math.E;
					if (isNegated) {
						cv = cv * -1.0;
					}
					if (rng != null) {
						return SadlUtils.getLiteralMatchingDataPropertyRange(getTheJenaModel(), rng.getURI(), cv);
					}
					else {
						return getTheJenaModel().createTypedLiteral(cv);
					}
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
				throw new JenaProcessorException(SadlErrorMessages.CONVERSION_TO_LITERAL_ERROR.get(nval.toFullyQualifiedString()));
			} else {
				throw new JenaProcessorException(
						"Unhandled sadl explicit vaue type: " + value.getClass().getCanonicalName());
			}
		} catch (Throwable t) {
			addError(t.getMessage(), value);
		}
		return null;
	}

	private boolean valueInObjectTypePropertyRange(OntProperty prop, RDFNode val, EObject cond)
			throws JenaProcessorException {
		ExtendedIterator<? extends OntResource> itr = prop.listRange();
		if (itr.hasNext()) {
			if (val.isResource() && val.canAs(Individual.class)) { 
				Individual valInst = val.as(Individual.class);
				while (itr.hasNext()) {
					OntResource nxt = itr.next();
					if (nxt.isClass()) {
						if (instanceBelongsToClass(getTheJenaModel(), valInst, nxt)) {
							return true;
						}
					}
				}
			}
			return false;
		}
		return true;
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
						org.apache.jena.rdf.model.Resource avfc = rest.asAllValuesFromRestriction().getAllValuesFrom();
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
				ExtendedIterator<org.apache.jena.rdf.model.Resource> eitr = inst.asIndividual().listRDFTypes(false);
				while (eitr.hasNext()) {
					org.apache.jena.rdf.model.Resource r = eitr.next();
					OntResource or = m.getOntResource(r);
					try {
						if (or.isURIResource()) {
							OntClass oc = m.getOntClass(or.getURI());
							if (classIsSubclassOfCached(oc, cls, true, null)) {
								eitr.close();
								return true;
							}
						} else if (or.canAs(OntClass.class)) {
							if (classIsSubclassOfCached(or.as(OntClass.class), cls, true, null)) {
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
	
	protected void setVariableNumber(int num) {
		vNum = num;
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
	
	protected void setRules(List<Rule> rls) {
		rules = rls;
	}

	static public File createBuiltinFunctionImplicitModel(String projectRootPath)
			throws IOException, ConfigurationException {
		// First, obtain proper translator for project
		SadlUtils su = new SadlUtils();
		if (projectRootPath.startsWith("file")) {
			projectRootPath = su.fileUrlToFileName(projectRootPath);
		}
		final File mfFolder = new File(projectRootPath + "/" + ResourceManager.OWLDIR);
		final String format = SadlPersistenceFormat.RDF_XML_ABBREV_FORMAT;
		String fixedModelFolderName = mfFolder.getCanonicalPath().replace("\\", "/");
		IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory
				.getConfigurationManagerForIDE(fixedModelFolderName, format);
		ITranslator translator = configMgr.getTranslator();
		// Second, obtain built-in function implicit model contents
		String builtinFunctionModel = translator.getBuiltinFunctionModel(getSadlKeywords());
		// Third, create built-in function implicit model file
		File builtinFunctionFile = new File(projectRootPath + "/" + SadlConstants.SADL_IMPLICIT_MODEL_FOLDER + "/"
				+ SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME);
		su.stringToFile(builtinFunctionFile, builtinFunctionModel, true);

		return builtinFunctionFile;
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

	public static List<String> getSadlKeywords() {
		return OwlToSadl.getSadlKeywords();
	}
	
	static public String getSadlBaseModel() {
		StringBuilder sb = new StringBuilder();
		sb.append("<rdf:RDF\n");
		sb.append("	    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n");
		sb.append("	    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"\n");
		sb.append("	    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\n");
		sb.append("	    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\n");
		sb.append("	    xmlns=\"http://sadl.org/sadlbasemodel#\"\n");
		sb.append("	    xmlns:sadlbasemodel=\"http://sadl.org/sadlbasemodel#\"\n");
		sb.append("	    xml:base=\"http://sadl.org/sadlbasemodel\">\n");
		sb.append("	  <owl:Ontology rdf:about=\"\">\n");
		sb.append(
				"	    <rdfs:comment xml:lang=\"en\">Base model for SADL. These concepts can be used without importing.</rdfs:comment>\n");
		sb.append("	  </owl:Ontology>\n");
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
		sb.append("    xmlns:sadllistmodel=\"http://sadl.org/sadllistmodel#\"\n");
		sb.append("    xml:base=\"http://sadl.org/sadllistmodel\" > \n");
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

	static public String getSadlServicesConfigConceptsModel() {
		String ssccContent = 
				"<rdf:RDF\n" + 
				"    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n" + 
				"    xmlns:builtinfunctions=\"http://sadl.org/builtinfunctions#\"\n" + 
				"    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"\n" + 
				"    xmlns:sadlimplicitmodel=\"http://sadl.org/sadlimplicitmodel#\"\n" + 
				"    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\n" + 
				"    xmlns:sscc=\"http://com.ge.research.sadl/sadlserver/Services#\"\n" + 
				"    xmlns:sadlbasemodel=\"http://sadl.org/sadlbasemodel#\"\n" + 
				"    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\n" + 
				"  xml:base=\"http://com.ge.research.sadl/sadlserver/Services\">\n" + 
				"  <owl:Ontology rdf:about=\"\">\n" + 
				"    <rdfs:comment xml:lang=\"en\">This ontology contains the concepts for Services Configuration in SADL and should not be edited.</rdfs:comment>\n" + 
				"  </owl:Ontology>\n" + 
				"  <owl:Class rdf:ID=\"NamedValuePair\">\n" + 
				"    <rdfs:subClassOf>\n" + 
				"      <owl:Restriction>\n" + 
				"        <owl:cardinality rdf:datatype=\"http://www.w3.org/2001/XMLSchema#int\"\n" + 
				"        >1</owl:cardinality>\n" + 
				"        <owl:onProperty>\n" + 
				"          <owl:DatatypeProperty rdf:about=\"#sscc:value\"/>\n" + 
				"        </owl:onProperty>\n" + 
				"      </owl:Restriction>\n" + 
				"    </rdfs:subClassOf>\n" + 
				"    <rdfs:subClassOf>\n" + 
				"      <owl:Restriction>\n" + 
				"        <owl:cardinality rdf:datatype=\"http://www.w3.org/2001/XMLSchema#int\"\n" + 
				"        >1</owl:cardinality>\n" + 
				"        <owl:onProperty>\n" + 
				"          <owl:DatatypeProperty rdf:ID=\"name\"/>\n" + 
				"        </owl:onProperty>\n" + 
				"      </owl:Restriction>\n" + 
				"    </rdfs:subClassOf>\n" + 
				"  </owl:Class>\n" + 
				"  <owl:Class rdf:ID=\"ConfigurationItem\">\n" + 
				"    <rdfs:subClassOf>\n" + 
				"      <owl:Restriction>\n" + 
				"        <owl:cardinality rdf:datatype=\"http://www.w3.org/2001/XMLSchema#int\"\n" + 
				"        >1</owl:cardinality>\n" + 
				"        <owl:onProperty>\n" + 
				"          <owl:DatatypeProperty rdf:ID=\"category\"/>\n" + 
				"        </owl:onProperty>\n" + 
				"      </owl:Restriction>\n" + 
				"    </rdfs:subClassOf>\n" + 
				"  </owl:Class>\n" + 
				"  <owl:Class rdf:ID=\"KnowledgeBase\">\n" + 
				"    <rdfs:subClassOf>\n" + 
				"      <owl:Restriction>\n" + 
				"        <owl:cardinality rdf:datatype=\"http://www.w3.org/2001/XMLSchema#int\"\n" + 
				"        >1</owl:cardinality>\n" + 
				"        <owl:onProperty>\n" + 
				"          <owl:DatatypeProperty rdf:ID=\"url\"/>\n" + 
				"        </owl:onProperty>\n" + 
				"      </owl:Restriction>\n" + 
				"    </rdfs:subClassOf>\n" + 
				"  </owl:Class>\n" + 
				"  <owl:Class rdf:ID=\"NamedService\">\n" + 
				"    <rdfs:subClassOf>\n" + 
				"      <owl:Restriction>\n" + 
				"        <owl:cardinality rdf:datatype=\"http://www.w3.org/2001/XMLSchema#int\"\n" + 
				"        >1</owl:cardinality>\n" + 
				"        <owl:onProperty>\n" + 
				"          <owl:DatatypeProperty rdf:ID=\"modelName\"/>\n" + 
				"        </owl:onProperty>\n" + 
				"      </owl:Restriction>\n" + 
				"    </rdfs:subClassOf>\n" + 
				"  </owl:Class>\n" + 
				"  <owl:ObjectProperty rdf:ID=\"item\">\n" + 
				"    <rdfs:domain rdf:resource=\"#ConfigurationItem\"/>\n" + 
				"    <rdfs:range rdf:resource=\"#NamedValuePair\"/>\n" + 
				"  </owl:ObjectProperty>\n" + 
				"  <owl:ObjectProperty rdf:ID=\"configurationOverride\">\n" + 
				"    <rdfs:domain rdf:resource=\"#NamedService\"/>\n" + 
				"    <rdfs:range rdf:resource=\"#ConfigurationItem\"/>\n" + 
				"  </owl:ObjectProperty>\n" + 
				"  <owl:ObjectProperty rdf:ID=\"entryPoint\">\n" + 
				"    <rdfs:domain rdf:resource=\"#KnowledgeBase\"/>\n" + 
				"    <rdfs:range rdf:resource=\"#NamedService\"/>\n" + 
				"  </owl:ObjectProperty>\n" + 
				"  <owl:DatatypeProperty rdf:about=\"#modelName\">\n" + 
				"    <rdfs:domain rdf:resource=\"#NamedService\"/>\n" + 
				"    <rdfs:comment xml:lang=\"en\">public URI of model</rdfs:comment>\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#anyURI\"/>\n" + 
				"  </owl:DatatypeProperty>\n" + 
				"  <owl:DatatypeProperty rdf:about=\"#name\">\n" + 
				"    <rdfs:domain rdf:resource=\"#NamedValuePair\"/>\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#string\"/>\n" + 
				"  </owl:DatatypeProperty>\n" + 
				"  <owl:DatatypeProperty rdf:about=\"#category\">\n" + 
				"    <rdfs:domain rdf:resource=\"#ConfigurationItem\"/>\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#string\"/>\n" + 
				"  </owl:DatatypeProperty>\n" + 
				"  <owl:DatatypeProperty rdf:about=\"#url\">\n" + 
				"    <rdfs:domain rdf:resource=\"#KnowledgeBase\"/>\n" + 
				"    <rdfs:comment xml:lang=\"en\">only needed for absolute URLs</rdfs:comment>\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#anyURI\"/>\n" + 
				"  </owl:DatatypeProperty>\n" + 
				"  <owl:DatatypeProperty rdf:about=\"#sscc:value\">\n" + 
				"    <rdfs:domain rdf:resource=\"#NamedValuePair\"/>\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#string\"/>\n" + 
				"  </owl:DatatypeProperty>\n" + 
				"</rdf:RDF>\n";
		return ssccContent;
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
				"	  <rdfs:comment>Copyright 2007, 2008, 2009, 2021 - General Electric Company, All Rights Reserved</rdfs:comment>\n");
		sb.append("	  <owl:versionInfo>$Id: defaults.owl,v 1.1 2014/01/23 21:52:26 crapo Exp $</owl:versionInfo>\n");
		sb.append("	</owl:Ontology>\n");
		sb.append("  <owl:Class rdf:ID=\"DefaultValue\">\n" + 
				"    <rdfs:comment xml:lang=\"en\">A default value is associated with a class by the rdfs:seeAlso annotation property</rdfs:comment>\n" + 
				"  </owl:Class>\n" + 
				"  <owl:Class rdf:ID=\"PropertyChainElement\"/>\n" + 
				"  <owl:FunctionalProperty rdf:ID=\"nextPropertyChainElement\">\n" + 
				"    <rdfs:domain rdf:resource=\"#PropertyChainElement\"/>\n" + 
				"    <rdfs:range rdf:resource=\"#PropertyChainElement\"/>\n" + 
				"    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#ObjectProperty\"/>\n" + 
				"  </owl:FunctionalProperty>\n" + 
				"  <owl:FunctionalProperty rdf:ID=\"appliesToPropertyChain\">\n" + 
				"    <rdfs:domain rdf:resource=\"#DefaultValue\"/>\n" + 
				"    <rdfs:range rdf:resource=\"#PropertyChainElement\"/>\n" + 
				"    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#ObjectProperty\"/>\n" + 
				"  </owl:FunctionalProperty>\n" + 
				"  <owl:FunctionalProperty rdf:ID=\"propertyElement\">\n" + 
				"    <rdfs:domain rdf:resource=\"#PropertyChainElement\"/>\n" + 
				"    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#ObjectProperty\"/>\n" + 
				"  </owl:FunctionalProperty>\n" + 
				"  <owl:FunctionalProperty rdf:ID=\"hasLevel\">\n" + 
				"    <rdfs:domain rdf:resource=\"#DefaultValue\"/>\n" + 
				"    <rdfs:range rdf:resource=\"http://www.w3.org/2001/XMLSchema#int\"/>\n" + 
				"    <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#DatatypeProperty\"/>\n" + 
				"  </owl:FunctionalProperty>\n" + 
				"  <owl:FunctionalProperty rdf:ID=\"hasDefault\">\n" + 
				"    <rdfs:domain rdf:resource=\"#DefaultValue\"/>\n" + 
				"    <rdf:type rdf:resource=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#Property\"/>\n" + 
				"  </owl:FunctionalProperty>\n" + 
				"</rdf:RDF>\n");
		return sb.toString();
	}
	
	protected boolean importSadlServicesConfigConceptsModel(Resource resource) throws JenaProcessorException, ConfigurationException {
		if (sadlServicesConfigConceptModel == null) {
			try {
				sadlServicesConfigConceptModel = getOntModelFromString(resource, getSadlServicesConfigConceptsModel());
				OntModelProvider.setSadlServicesConfigConceptsModel(sadlServicesConfigConceptModel);
			} catch (Exception e) {
				throw new JenaProcessorException(e.getMessage(), e);
			}
			addImportToJenaModel(getModelName(), SadlConstants.SADL_SERIVCES_CONFIGURATION_CONCEPTS_URI,
					SadlConstants.SADL_SERIVCES_CONFIGURATION_CONCEPTS_PREFIX, sadlServicesConfigConceptModel);
			return true;
		}
		return false;
	}

	protected boolean importSadlListModel(Resource resource) throws JenaProcessorException, ConfigurationException {
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
		OntModel listModel = prepareEmptyOntModel(resource, getProcessorContext());
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
				format = SadlPersistenceFormat.RDF_XML_ABBREV_FORMAT; // default
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

	protected boolean isSyntheticUri(Resource resource) {
		return resource.getURI().toString().startsWith("synthetic")
			|| resource.getURI().toString().startsWith(SYNTHETIC_FROM_TEST);
	}
	
	protected boolean isSyntheticUri(String modelFolderPathname, Resource resource) {
		return modelFolderPathname == null && isSyntheticUri(resource);
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
			e.printStackTrace();
		}
	}

	public List<Equation> getEquations() {
		return getEquations(getCurrentResource());
	}

	/**
	 * Method to get an Equation. First attempt is to get it from the OntModelProvider cache. 
	 * That failing, one is constructed from the OWL model.
	 * @param eqSr
	 * @return
	 */
	private Equation getEquation(SadlResource eqSr) {
		Resource rsrc = eqSr.eResource();
		if (rsrc.getContents() != null && !rsrc.getContents().isEmpty() &&
				rsrc.getContents().get(0) instanceof SadlModel) {
			SadlModel sm = (SadlModel) rsrc.getContents().get(0);	
			if (sm.getBaseUri().equals(SadlConstants.SADL_BUILTIN_FUNCTIONS_URI)) {
				return null;
			}
		}
		List<Equation> eqns = getEquations(rsrc);
		String eqUri = getDeclarationExtensions().getConceptUri(eqSr);
		Equation eqcached = null;
		for (Equation eq : eqns) {
			if (eq.getUri().equals(eqUri)) {
				return eq;
//				eqcached = eq;
//				break;
			}
		}
//		return getEquationFromOwl(eqSr);
		Equation eqFromOwl = getEquationFromOwl(eqSr);
		
		return eqcached;
	}

	/**
	 * Method to get an Equation instance by constructing it from the OWL models
	 * @param eqUri
	 * @return
	 */
	private Equation getEquationFromOwl(SadlResource sr) {
		String eqUri = getDeclarationExtensions().getConceptUri(sr);
		String eqName =  getDeclarationExtensions().getConcreteName(sr);
		Individual eqInst = getTheJenaModel().getIndividual(eqUri);
		if (eqInst !=  null) {
			return getEquationFromOwl(eqName, eqInst);
		}
		return null;
	}

	protected Equation getEquationFromOwl(String eqName, Individual eqInst) {
		boolean isExternal = false;
		Equation eq = new Equation(eqName);
		eq.setNamespace(eqInst.getNameSpace());
		eq.setUri(eqInst.getURI());
		RDFNode exturi = eqInst.getPropertyValue(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNALURL_PROPERTY_URI));
		if (exturi != null) {
			isExternal = true;
			eq.setExternal(true);
			eq.setExternalUri(exturi.asLiteral().getValue().toString());
			RDFNode loc = eqInst.getPropertyValue(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_LOCATION_PROPERTY_URI));
			if (loc != null) {
				eq.setLocation(loc.asLiteral().getValue().toString());
			}
		}
		List<Node> arguments = new ArrayList<Node>();
		List<Node> argumentTypes = new ArrayList<Node>();
		RDFNode argsNode = eqInst.getPropertyValue(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_ARGUMENTS_PROPERTY_URI));
		while (argsNode != null) {
			if (argsNode.isResource()) {
				org.apache.jena.rdf.model.Resource frst = argsNode.asResource().getPropertyResourceValue(getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI));
				Statement dtstmt = frst.getProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_DATATYPE_PROPERTY_URI));
				if (dtstmt != null) {
					RDFNode dtnode = dtstmt.getObject();
					if (dtnode != null && dtnode.isLiteral()) {
						String dtstr = dtnode.asLiteral().getValue().toString();
						Node nnode = new NamedNode(dtstr, NodeType.DataTypeNode);
						argumentTypes.add(nnode);
						Statement dnstmt = frst.getProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_DESCRIPTOR_NAME_PROPERTY_URI));
						if (dnstmt != null) {
							RDFNode dnnode = dnstmt.getObject();
							if (dnnode != null && dnnode.isLiteral()) {
								String dnstr = dnnode.asLiteral().getValue().toString();
								VariableNode vNode = new VariableNode(dnstr);
								try {
									vNode.setType(nnode);
								} catch (TranslationException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								}
								vNode.setNamespace(eqInst.getNameSpace());
								arguments.add(vNode);
							}
						}
						Statement varargsstmt = frst.getProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_ARG_VALUES_PROPERTY_URI));
						if (varargsstmt != null) {
							if (varargsstmt.getObject().asLiteral().getBoolean()) {
//								eq.setVarArgs(true);
							}
						}

					}
				}
				
				org.apache.jena.rdf.model.Resource rst = argsNode.asResource().getPropertyResourceValue(getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_REST_URI));
				if (rst != null && rst.isResource()) {
					argsNode = rst.asResource();
				}
				else {
					argsNode = null;
				}
			}
		}
		eq.setArguments(arguments);
		eq.setArgumentTypes(argumentTypes);
		List<Node> returnTypes = new ArrayList<Node>();
		RDFNode retTypesNode = eqInst.getPropertyValue(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_RETURN_TYPES_PROPERTY_URI));
		while (retTypesNode != null) {
			if (retTypesNode.isResource()) {
				org.apache.jena.rdf.model.Resource frst = retTypesNode.asResource().getPropertyResourceValue(getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI));
				Statement dtstmt = frst.getProperty(getTheJenaModel().getProperty(SadlConstants.SADL_IMPLICIT_MODEL_DATATYPE_PROPERTY_URI));
				if (dtstmt != null) {
					RDFNode dtnode = dtstmt.getObject();
					if (dtnode != null && dtnode.isLiteral()) {
						String dtstr = dtnode.asLiteral().getValue().toString();
						Node nnode = new NamedNode(dtstr, NodeType.DataTypeNode);
						returnTypes.add(nnode);
					}
				}
				org.apache.jena.rdf.model.Resource rst = retTypesNode.asResource().getPropertyResourceValue(getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_REST_URI));
				if (rst != null && rst.isResource()) {
					retTypesNode = rst.asResource();
				}
				else {
					retTypesNode = null;
				}
			}
		}
		eq.setReturnTypes(returnTypes);
		return eq;
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
	public List<ConceptName> getImpliedProperties(org.apache.jena.rdf.model.Resource cls) {
		List<ConceptName> retlst = null;
		if (cls == null)
			return null;
		if (!cls.isURIResource())
			return null; // impliedProperties can only be given to a named class
		if (!cls.canAs(OntClass.class)) {
			addTypeCheckingError("Can't get implied properties of a non-class entity. Perhaps this can't be used before it is declared?", getDefaultEObject());
			return null;
		}
		List<ConceptName> cached = impliedPropoertiesCache.get(cls.getURI());
		if (cached != null && cached.size() > 0) {
			return cached;
		}
		List<OntResource> allImplPropClasses = getAllImpliedPropertyClasses();
		if (allImplPropClasses != null) {
			for (OntResource ipcls : allImplPropClasses) {
				try {
					if (classIsSubclassOfCached(cls.as(OntClass.class), ipcls, true, null)) {
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
									ConceptName cn = (new SadlUtils()).getConceptByUri(getTheJenaModel(), obj.asResource().getURI());
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
		impliedPropoertiesCache.put(cls.getURI(), retlst);
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
				org.apache.jena.rdf.model.Resource subj = sitr.nextStatement().getSubject();
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
				org.apache.jena.rdf.model.Resource subj = sitr.nextStatement().getSubject();
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
	public List<String> getExpandedProperties(org.apache.jena.rdf.model.Resource cls) {
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
		if (modelValidator == null) {
			initializeModelValidator();
		}
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
		if (node == null) return "null";
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

	public boolean isAssignmentOperator(List<String> aOperators) {
		for(String lOperator : aOperators) {
			if(isAssignmentOperator(lOperator)) {
				return true;
			}
		}
		return false;
	}
	
	public boolean isAssignmentOperator(String aOperator) {
		return mAssignmentOperators.contains(aOperator);
	}
	
	public boolean isNumericComparisonOperator(String operation) {
		if (numericComparisonOperators.contains(operation)) {
			return true;
		}
		return false;
	}
	
	public boolean isNumericComparisonOperator(List<String> aOperations) {
		for(String lOperator : aOperations) {
			if(isNumericComparisonOperator(lOperator)) {
				return true;
			}
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
				|| uri.equals(XSD.xfloat.getURI()) || uri.equals(XSD.xint.getURI()) || uri.equals(XSD.xlong.getURI())
				|| uri.equals(XSD.negativeInteger.getURI()) || uri.equals(XSD.nonNegativeInteger.getURI())
				|| uri.equals(XSD.nonPositiveInteger.getURI()) || uri.equals(XSD.positiveInteger.getURI())
				|| uri.equals(XSD.unsignedInt.getURI()) || uri.equals(XSD.unsignedLong.getURI())
				|| uri.equals(XSD.unsignedShort.getURI()) || uri.equals(XSD.xshort.getURI())) {
			return true;
		}
		if (uri.equals(XSD.duration.getURI())) {
			// xsd:duration will be considered numeric for type checking.
			return true;
		}
		//If Unitted Quantities are ignored then they are also considered a numeric type
		if(this.isIgnoreUnittedQuantities() && uri.equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
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
		return UtilsForJena.isTypedListSubclass(theJenaModel, node);
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
							org.apache.jena.ontology.OntResource range = annProp.canAs(OntProperty.class)
									? annProp.as(OntProperty.class).getRange()
									: null;
							org.apache.jena.rdf.model.Literal annLiteral = sadlExplicitValueToLiteral(annvalue, range);
							getTheJenaModel().add(namedStructure, annProp, annLiteral);
							if (cntr > 0)
								sb.append(", ");
							sb.append("\"");
							sb.append(annvalue);
							sb.append("\"");
							cntr++;
						} catch (Exception e) {
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
			throws TranslationException, InvalidNameException {
		if (cruleVariables == null) {
			cruleVariables = new HashMap<NamedNode, List<VariableNode>>();
		}
		if (!cruleVariables.containsKey(type)) {
			List<VariableNode> newList = new ArrayList<VariableNode>();
			VariableNode var = new VariableNode(createUri(name));
			var.setCRulesVariable(true);
			var.setType(validateNode(type));
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
				var.setType(validateNode(type));
				var.setHostObject(host != null ? host : getHostEObject());
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

	/**
	 * Method to make sure that name has a URI namespace in front; if not add one for this namespace
	 * @param name
	 * @return
	 */
	private String createUri(String name) {
		if (!(name.indexOf('#') > 0)) {
			name = getModelNamespace() + name;
		}
		return name;
	}
	
	private String createUri(String ns, String name) {
		if (ns.endsWith("#")) {
			return ns+name;
		}
		return ns + "#" + name;
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
		vNum = 0; 	// reset system-generated variable name counter (where uniqueness across an OWL namespace is required this is achieved 
					//  via special check to create unique OWL variable)
		if (gpVariableMap != null) {
			gpVariableMap.clear();
		}
		if (variablesTyped != null) {
			variablesTyped.clear();
		}
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
						addError("Unable to import model with URI '" + importUri + "'", simport);
					} else {
						addImportToJenaModel(modelName, importUri, importPrefix, importedOntModel);
					}
				} else if (eResource instanceof ExternalEmfResource) {
					ExternalEmfResource emfResource = (ExternalEmfResource) eResource;
					OntModel impcmom;
					try {
						impcmom = emfResource.getOntModel();
						if (impcmom != null) {
							addImportToJenaModel(modelName, importUri, importPrefix, impcmom);
							// we need to load any imports of the external resource
							ExtendedIterator<Ontology> ontologyItr = emfResource.getOntModel().listOntologies();	// getting ConcurrentModificationException on this for some reason so putting in list first. awc 9/29/2020
							List<Ontology> ontologies = null;
							if (ontologyItr.hasNext()) {
								ontologies = new ArrayList<Ontology>();
								while (ontologyItr.hasNext()) {
									Ontology impont = ontologyItr.next();
									ontologies.add(impont);
								}
							}
							if (ontologies != null) {
								Iterator<Ontology> ontitr = ontologies.iterator();
								while (ontitr.hasNext()) {
									Ontology impont = ontitr.next();
									ExtendedIterator<OntResource> imports = impont.listImports();
									while (imports.hasNext()) {
										OntResource imprsrc = imports.next();
										String imprsrcuri = imprsrc.getURI();
										try {
											URI impUri = URI.createURI(imprsrcuri);
											URI impSadlUri = getConfigMgr().getSadlUriFromPublicUri(getCurrentResource().getResourceSet(), impUri);
											if (impSadlUri != null) {
												// according to the mapping file, this should be a SADL model so get it as such so it will be built if necessary
												Resource impRsrc = getCurrentResource().getResourceSet().getResource(impSadlUri, true);
												if (impRsrc instanceof XtextResource) {
													XtextResource xtrsrc = (XtextResource) impRsrc;
													URI importedResourceUri = xtrsrc.getURI();
													OntModel importedOntModel = OntModelProvider.find(xtrsrc);
													String importedPrefix = OntModelProvider.getModelPrefix(xtrsrc);
													if (importedOntModel == null) {
														logger.debug("JenaBasedSadlModelProcessor failed to find OntModel for SADL Resource '"
																+ importedResourceUri + "' while processing Resource '" + importingResourceUri + "'");
														addError("Unable to import model with URI '" + importUri + "'", simport);
													} else {
														addImportToJenaModel(impcmom, importUri, imprsrcuri, importedPrefix, importedOntModel);
													}
												}
												else {
													logger.debug("JenaBasedSadlModelProcessor failed to find import for SADL resource '"
															+ imprsrcuri + "' (mapping SADL URI '" + impSadlUri + "') while processing Resource '" + importingResourceUri + "'");
													addError("Unable to import indirect import with URI '" + impUri + "'", simport);
												}
											}
											else {
												OntModel impom = getConfigMgr().getOntModel(imprsrcuri, Scope.INCLUDEIMPORTS);
												if (impom == null) {
													logger.debug("JenaBasedSadlModelProcessor failed to find import for non-SADL OWL resource '"
															+ imprsrcuri + "' while processing Resource '" + importingResourceUri + "'");
													addError("Unable to import indirect import with URI '" + impUri + "'", simport);
												}
												else {
													getTheJenaModel().addSubModel(impom);
													getTheJenaModel().addLoadedImport(imprsrcuri);
													addOrderedImport(imprsrcuri);
												}
											}
										} catch (ConfigurationException e) {
											e.printStackTrace();
										} catch (IOException e) {
											e.printStackTrace();
										}
									}
								}
							}
						}	
						else {
							logger.debug("JenaBasedSadlModelProcessor failed to find OWL model '"
									+ importUri + "' while processing Resource '" + importingResourceUri + "'");
							addError("Unable to import indirect import with URI '" + importUri + "'", simport);
						}
					} catch (Exception e1) {
						logger.debug("JenaBasedSadlModelProcessor failed to find OWL model '"
								+ importUri + "' while processing Resource '" + importingResourceUri + "'");
						addError("Unable to import indirect import with URI '" + importUri + "'", simport);
					}
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

	public VariableNode getVariable(String name) throws TranslationException {
		Object trgt = getTarget();
		if (trgt instanceof Rule) {
			return ((Rule) trgt).getVariable(variableNameToUri(name));
		} else if (trgt instanceof Query) {
			return ((Query) trgt).getVariable(variableNameToUri(name));
		}
		else if (getCurrentEquation() != null) {
			return getCurrentEquation().getVariable(variableNameToUri(name));
		}
		return null;
	}

	private String variableNameToUri(String name) throws TranslationException {
		if (name.contains("#")) {
			// must be a complete URI
			return name;
		}
		else if (!name.contains(":")) {
			// must be a local name only
			return getModelNamespace() + name;
		}
		else {
			// by elimination, must be a QName but this should not happen?
			int sep = name.lastIndexOf(':');
			if (sep > 0) {
				String prefix = name.substring(0, sep);
				String ln = name.substring(sep + 1);
				if (prefix.equals(getModelAlias())) {
					return getModelNamespace() + ln;
				}
			}
		}
		throw new TranslationException("Invalid variable name '" + name + "'");
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

	enum SIDE {LEFT, RIGHT};
	
	protected void checkForArticleForNameInTriple(Expression value, Object triple, SIDE side) throws InvalidNameException {
		if(triple instanceof TripleElement) {
			Node tripleNode = side.equals(SIDE.LEFT) ? ((TripleElement)triple).getSubject() : ((TripleElement)triple).getObject();
			if (side.equals(SIDE.RIGHT) && tripleNode == null && ((TripleElement)triple).getModifierType().equals(TripleModifierType.None)) {
				tripleNode = new ConstantNode(SadlConstants.CONSTANT_NONE);
				((TripleElement)triple).setObject(tripleNode);
			}
			if (isUseArticlesInValidation() && value instanceof Name && tripleNode instanceof NamedNode
					&& ((NamedNode) tripleNode).getNodeType().equals(NodeType.ClassNode)) {
				addError(SadlErrorMessages.NEEDS_ARTICLE.get(), value);
			}
		}
	}
	
	protected void checkForArticleForNameInBuiltinElement(Expression aValue, Object aBuiltinElement) throws InvalidNameException {
		if(aBuiltinElement instanceof BuiltinElement) {
			List<Node> lNodeList = ((BuiltinElement) aBuiltinElement).getArguments();
			if(lNodeList != null && lNodeList.size() == 2) {
				Node lNode = lNodeList.get(1);
				if(isUseArticlesInValidation() && aValue instanceof Name && lNode instanceof NamedNode && 
						!(lNode instanceof VariableNode) && !(((NamedNode)lNode).getNodeType().equals(NodeType.InstanceNode))) {
					addError(SadlErrorMessages.NEEDS_ARTICLE.get(), aValue);
				}
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

	private List<TripleElement> getTriplesOfInterestList(List<TripleElement> found, GraphPatternElement gpe) throws InvalidTypeException, TranslationException {
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

	private List<TripleElement> addTripleOfInterest(List<TripleElement> found, Node node) throws InvalidTypeException, TranslationException {
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

	public boolean isTypeCheckingRangeRequired() {
		return typeCheckingRangeRequired;
	}

	private void setTypeCheckingRangeRequired(boolean typeCheckingRangeRequired) {
		this.typeCheckingRangeRequired = typeCheckingRangeRequired;
	}
	
	/**
     * Method to get the length of an instance of a SADL typed list.
     * @param stl -- an instance of a SADL typed list
     * @return -- number of elements in the list else 0 if stl is not a SADL typed list
     */
     public int getSadlTypedListLength(org.apache.jena.rdf.model.Resource stl) {
            Property fprop = getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI);
            Property rprop = getTheJenaModel().getProperty(SadlConstants.SADL_LIST_MODEL_REST_URI);
            int cnt = 0;
            org.apache.jena.rdf.model.Resource lst = stl;
            while (lst != null) {
                   if (lst.getProperty(fprop) != null) {
                         cnt++;
                         Statement nxt = lst.getProperty(rprop);
                         lst = (nxt != null) ? nxt.getObject().asResource() : null;
                   } else {
                	   break;
                   }
            }
            return cnt;
     }

 	/**
 	 * Method to convert an Iterator over a List of values to a SADL Typed List in the provided model
 	 * @param lastInst -- the list to which to add members or null to begin a new list
 	 * @param cls -- the class of the SADL list
 	 * @param type --  the type of the members of the list
 	 * @param memberIterator -- Iterator over the values to add
 	 * @return -- the list instance
 	 * @throws JenaProcessorException
 	 * @throws TranslationException
 	 */
 	protected Individual addMembersToSadlList(OntModel model, Individual lastInst, OntClass cls,
 			org.apache.jena.rdf.model.Resource type, Iterator<?> memberIterator) throws JenaProcessorException, TranslationException {
 		if (lastInst == null) {
 			lastInst = model.createIndividual(cls);
 		}
 		Object val = memberIterator.next();
 		if (val instanceof Individual) {
 			Individual listInst = (Individual) val;
 			if (type.canAs(OntClass.class)) {
 				ExtendedIterator<org.apache.jena.rdf.model.Resource> itr = listInst.listRDFTypes(false);
 				boolean match = false;
 				while (itr.hasNext()) {
 					org.apache.jena.rdf.model.Resource typ = itr.next();
 					if (typ.equals(type)) {
 						match = true;
 					} else {
 						try {
 							if (typ.canAs(OntClass.class) && classIsSubclassOfCached(typ.as(OntClass.class), type.as(OntClass.class), true, null)) {
 								match = true;
 							}
 						} catch (CircularDependencyException e) {
 							// TODO Auto-generated catch block
 							e.printStackTrace();
 						}
 					}
 					if (match) {
 						break;
 					}
 				}
 				if (!match) {
 					throw new JenaProcessorException("The Instance '" + listInst.toString() + "' doesn't match the List type.");
 				}
 				model.add(lastInst, model.getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI),
 						listInst);
 			} else {
 				throw new JenaProcessorException("The type of the list could not be converted to a class.");
 			}
 		} else {
 			Literal lval;
 			if (val instanceof Literal) {
 				lval = (Literal) val;
 			}
 			else {
 				lval = SadlUtils.getLiteralMatchingDataPropertyRange(model,type.getURI(), val);
 			}
 			model.add(lastInst, model.getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI), lval);
 		}
 		if (memberIterator.hasNext()) {
 			Individual rest = addMembersToSadlList(model, null, cls, type, memberIterator);
 			model.add(lastInst, model.getProperty(SadlConstants.SADL_LIST_MODEL_REST_URI), rest);
 		}
 		return lastInst;
 	}

	protected void markVariableDefinionTriplesAsDefinition(VariableNode var, Object defn) {
		if (defn instanceof TripleElement) {
			if (((TripleElement)defn).getSubject().equals(var)) {
				((TripleElement)defn).setSourceType(TripleSourceType.VariableDefinition);
			}
		}
		else if (defn instanceof Junction) {
			markVariableDefinionTriplesAsDefinition(var, ((ProxyNode)((Junction)defn).getLhs()).getProxyFor());
			markVariableDefinionTriplesAsDefinition(var, ((ProxyNode)((Junction)defn).getRhs()).getProxyFor());
		}
		else if (defn instanceof List<?>) {
			for (Object dfn : (List<?>)defn) {
				markVariableDefinionTriplesAsDefinition(var, dfn);
			}
		}
	}

	@Override
	public Map<String, String> getPreferences(Resource resource) {
		if (modelProcessorPreferenceMap != null) {
			return modelProcessorPreferenceMap;
		}
		IPreferenceValuesProvider pvp = ((XtextResource)resource).getResourceServiceProvider().get(IPreferenceValuesProvider.class);
		IPreferenceValues preferenceValues = pvp.getPreferenceValues(resource);
		if (preferenceValues != null) {
			modelProcessorPreferenceMap = new HashMap<String, String>();
			String typechecking = preferenceValues.getPreference(SadlPreferences.TYPE_CHECKING_WARNING_ONLY);
			if (typechecking == null) {
				typechecking = "true";
			}
			setTypeCheckingWarningsOnly(Boolean.parseBoolean(typechecking));
			modelProcessorPreferenceMap.put(SadlPreferences.TYPE_CHECKING_WARNING_ONLY.getId(), typechecking);

			String ignoreUnits = preferenceValues.getPreference(SadlPreferences.IGNORE_UNITTEDQUANTITIES);
			if (ignoreUnits == null) {
				ignoreUnits = "true";
			}
			setIgnoreUnittedQuantities(Boolean.parseBoolean(ignoreUnits));
			modelProcessorPreferenceMap.put(SadlPreferences.IGNORE_UNITTEDQUANTITIES.getId(), ignoreUnits);
	
			String expandUnittedQuantities = preferenceValues.getPreference(SadlPreferences.EXPAND_UNITTEDQUANTITIES);
			if (expandUnittedQuantities == null) {
				expandUnittedQuantities = "true";
			}
			setExpandUnittedQuantities(Boolean.parseBoolean(expandUnittedQuantities));
			modelProcessorPreferenceMap.put(SadlPreferences.EXPAND_UNITTEDQUANTITIES.getId(), expandUnittedQuantities);
			
			setUseArticlesInValidation(false);
			String useArticles = preferenceValues.getPreference(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION);
			if (useArticles == null) {
				useArticles = "false";
			}
			setUseArticlesInValidation(Boolean.parseBoolean(useArticles));
			modelProcessorPreferenceMap.put(SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.getId(), useArticles);

			String expandMissingPatterns = preferenceValues.getPreference(SadlPreferences.FIND_AND_EXPAND_MISSING_PATTERNS);
			if (expandMissingPatterns == null) {
				expandMissingPatterns = "false";
			}
			setExpandMissingPatternsInValidation(Boolean.parseBoolean(expandMissingPatterns));
			modelProcessorPreferenceMap.put(SadlPreferences.FIND_AND_EXPAND_MISSING_PATTERNS.getId(), expandMissingPatterns);
			
			String domainAndRangeAsUnionClassesStr = preferenceValues
					.getPreference(SadlPreferences.CREATE_DOMAIN_AND_RANGE_AS_UNION_CLASSES);
			if (domainAndRangeAsUnionClassesStr == null) {
				domainAndRangeAsUnionClassesStr = "true";
			}
			setDomainAndRangeAsUnionClasses(Boolean.parseBoolean(domainAndRangeAsUnionClassesStr));
			modelProcessorPreferenceMap.put(SadlPreferences.CREATE_DOMAIN_AND_RANGE_AS_UNION_CLASSES.getId(), domainAndRangeAsUnionClassesStr);

			String typeCheckingRangeRequiredStr = preferenceValues
					.getPreference(SadlPreferences.TYPE_CHECKING_RANGE_REQUIRED);
			if (typeCheckingRangeRequiredStr == null) {
				typeCheckingRangeRequiredStr = "true";
			}
			setTypeCheckingRangeRequired(Boolean.parseBoolean(typeCheckingRangeRequiredStr));
			modelProcessorPreferenceMap.put(SadlPreferences.TYPE_CHECKING_RANGE_REQUIRED.getId(), typeCheckingRangeRequiredStr);
			
			String enableMetricsCollectionStr = preferenceValues.getPreference(SadlPreferences.ENABLE_METRICS_COLLECTION);
			if (enableMetricsCollectionStr == null) {
				enableMetricsCollectionStr = "false";
			}
			setEnableMetricsCollection(Boolean.parseBoolean(enableMetricsCollectionStr));
			modelProcessorPreferenceMap.put(SadlPreferences.ENABLE_METRICS_COLLECTION.getId(), enableMetricsCollectionStr);
		}
		return modelProcessorPreferenceMap;
	}

	protected void setExpandUnittedQuantities(boolean expandUnittedQuantities) {
		this.expandUnittedQuantities = expandUnittedQuantities;
		
	}

	protected boolean isIgnoreUnittedQuantities() {
		return ignoreUnittedQuantities;
	}

	protected void setIgnoreUnittedQuantities(boolean ignoreUnittedQuantities) {
		this.ignoreUnittedQuantities = ignoreUnittedQuantities;
	}

	protected boolean isDomainAndRangeAsUnionClasses() {
		return domainAndRangeAsUnionClasses;
	}

	protected void setDomainAndRangeAsUnionClasses(boolean domainAndRangeAsUnionClasses) {
		this.domainAndRangeAsUnionClasses = domainAndRangeAsUnionClasses;
	}

	protected List<Class> getAllowedVariableContainers() {
		return allowedVariableContainers;
	}

	protected void addVariableAllowedInContainerType(Class<? extends EObject> class1) {
		if (getAllowedVariableContainers() == null) {
			setAllowedVariableContainers(new ArrayList<Class>());
		}
		getAllowedVariableContainers().add(class1);
	}

	protected void setAllowedVariableContainers(List<Class> allowedVariableContainers) {
		this.allowedVariableContainers = allowedVariableContainers;
	}

	private boolean isEnableMetricsCollection() {
		return enableMetricsCollection;
	}

	private void setEnableMetricsCollection(boolean enableMetricsCollection) {
		this.enableMetricsCollection = enableMetricsCollection;
	}

	public void clearTypeCheckingErrorDetected() {
		setTypeCheckingErrorDetected(false);
	}

	public boolean isTypeCheckingErrorDetected() {
		return typeCheckingErrorDetected;
	}

	protected void setTypeCheckingErrorDetected(boolean typeCheckingErrorDetected) {
		this.typeCheckingErrorDetected = typeCheckingErrorDetected;
	}

	@Override
	public String getDatatypePropertyContentAssistSuggestion(SadlResource prop) {
		OntConceptType oct;
		String propuri = null;
		try {
			oct = getDeclarationExtensions().getOntConceptType(prop);
			if (oct.equals(OntConceptType.DATATYPE_PROPERTY)) {
	            propuri = getDeclarationExtensions().getConceptUri(prop);
	            OntProperty ontprop = getTheJenaModel().getOntProperty(propuri);
	            if (ontprop != null) {
	                StmtIterator rngitr = getTheJenaModel().listStatements(ontprop, RDFS.range, (RDFNode)null);
	                RDFNode rng = null;
	                String proposal = null;
	                if (rngitr.hasNext()) {
	                        rng = rngitr.nextStatement().getObject();
	                        switch(rng.toString()) {
	                        case XSD.NS + "string":
	                        	String pnm = getDeclarationExtensions().getConcreteName(prop);
	                            proposal = "\"<" + pnm + "-value>\"";
	                        break;
	                        case XSD.NS + "date":
	                            proposal = "\"MM/DD/YYYY\"";
	                        break;
	                        case XSD.NS + "dateTime":
	                        	proposal = "\"MM/DD/YYYY hh:mm:ss PM TZD\"";
	                        break;
	                        case XSD.NS + "float":
	                        case XSD.NS + "double":
	                            proposal = "123.4";
	                        break;
	                        case XSD.NS + "int":
	                        case XSD.NS + "integer":
	                        case XSD.NS + "long":
	                            proposal = "123";
	                        break;
	                        default:
	                        	addInfo("Content assist suggestion not provided for datatype '" + rng.toString() + "'", prop);
	                        }       
	                }
	                return proposal;
	            }
			}
		} catch (CircularDefinitionException e) {
			addError("Unexpected error getting content assist example for property '" + propuri + "'", prop);
		}
		return null;
	}

	/**
	 * Method to obtain a cached Jena Resource for a given EObject
	 * @param key
	 * @return
	 */
	protected org.apache.jena.rdf.model.Resource getCachedJenaResource(EObject key) {
		if (cachedJenaResource != null) {
			return cachedJenaResource.get(key);
		}
		return null;
	}

	/**
	 * Method to cache a Jena Resource for a given EObject
	 * @param key
	 * @param jenaResource
	 */
	protected void addCachedJenaResource(EObject key, org.apache.jena.rdf.model.Resource jenaResource) {
		if(cachedJenaResource == null) {
			cachedJenaResource = new HashMap<EObject, org.apache.jena.rdf.model.Resource>();
		}
		cachedJenaResource.put(key, jenaResource);
	}
	
}
