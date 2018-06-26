package com.ge.research.sadl.jena;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.diagnostics.Severity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.errorgenerator.generator.SadlErrorMessages;
import com.ge.research.sadl.model.CircularDefinitionException;
import com.ge.research.sadl.model.ConceptIdentifier;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.ConceptName.RangeValueType;
import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.PrefixNotFoundException;
import com.ge.research.sadl.model.SadlUnionClass;
import com.ge.research.sadl.model.gp.ConstantNode;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.ProxyNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.processing.ISadlModelValidator;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.reasoner.CircularDependencyException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.AskExpression;
import com.ge.research.sadl.sADL.BinaryOperation;
import com.ge.research.sadl.sADL.BooleanLiteral;
import com.ge.research.sadl.sADL.Constant;
import com.ge.research.sadl.sADL.ConstructExpression;
import com.ge.research.sadl.sADL.Declaration;
import com.ge.research.sadl.sADL.ElementInList;
import com.ge.research.sadl.sADL.EquationStatement;
import com.ge.research.sadl.sADL.Expression;
import com.ge.research.sadl.sADL.ExternalEquationStatement;
import com.ge.research.sadl.sADL.Name;
import com.ge.research.sadl.sADL.NumberLiteral;
import com.ge.research.sadl.sADL.PropOfSubject;
import com.ge.research.sadl.sADL.QueryStatement;
import com.ge.research.sadl.sADL.SadlBooleanLiteral;
import com.ge.research.sadl.sADL.SadlCanOnlyBeOneOf;
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration;
import com.ge.research.sadl.sADL.SadlConstantLiteral;
import com.ge.research.sadl.sADL.SadlDataType;
import com.ge.research.sadl.sADL.SadlInstance;
import com.ge.research.sadl.sADL.SadlIntersectionType;
import com.ge.research.sadl.sADL.SadlModel;
import com.ge.research.sadl.sADL.SadlModelElement;
import com.ge.research.sadl.sADL.SadlMustBeOneOf;
import com.ge.research.sadl.sADL.SadlNestedInstance;
import com.ge.research.sadl.sADL.SadlNumberLiteral;
import com.ge.research.sadl.sADL.SadlParameterDeclaration;
import com.ge.research.sadl.sADL.SadlPrimitiveDataType;
import com.ge.research.sadl.sADL.SadlProperty;
import com.ge.research.sadl.sADL.SadlPropertyCondition;
import com.ge.research.sadl.sADL.SadlPropertyInitializer;
import com.ge.research.sadl.sADL.SadlResource;
import com.ge.research.sadl.sADL.SadlSimpleTypeReference;
import com.ge.research.sadl.sADL.SadlStringLiteral;
import com.ge.research.sadl.sADL.SadlTypeReference;
import com.ge.research.sadl.sADL.SadlUnaryExpression;
import com.ge.research.sadl.sADL.SadlUnionType;
import com.ge.research.sadl.sADL.SelectExpression;
import com.ge.research.sadl.sADL.StringLiteral;
import com.ge.research.sadl.sADL.SubjHasProp;
import com.ge.research.sadl.sADL.Sublist;
import com.ge.research.sadl.sADL.TestStatement;
import com.ge.research.sadl.sADL.UnaryExpression;
import com.ge.research.sadl.sADL.UnitExpression;
import com.ge.research.sadl.sADL.ValueTable;
import com.ge.research.sadl.sADL.impl.ElementInListImpl;
import com.ge.research.sadl.sADL.impl.ExternalEquationStatementImpl;
import com.ge.research.sadl.sADL.impl.PropOfSubjectImpl;
import com.ge.research.sadl.sADL.impl.TestStatementImpl;
import com.ge.research.sadl.utils.SadlASTUtils;
import com.hp.hpl.jena.datatypes.DatatypeFormatException;
import com.hp.hpl.jena.ontology.AllValuesFromRestriction;
import com.hp.hpl.jena.ontology.AnnotationProperty;
import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.HasValueRestriction;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.IntersectionClass;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Restriction;
import com.hp.hpl.jena.ontology.UnionClass;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFList;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.OWL2;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;
import com.hp.hpl.jena.vocabulary.XSD;

public class JenaBasedSadlModelValidator implements ISadlModelValidator {
	public static final String RANGE = "range";
	public static final String RESTRICTED_TO = "restriction to";
	private static final int MIN_INT = -2147483648;
	private static final int MAX_INT = 2147483647;
	private static final long MIN_LONG = -9223372036854775808L;
	private static final long MAX_LONG = 9223372036854775807L;
	protected OntModel theJenaModel = null;
	protected DeclarationExtensions declarationExtensions = null;
	private EObject defaultContext;
	
	protected Map<EObject, TypeCheckInfo> expressionsTypeCheckCache = new HashMap<EObject,TypeCheckInfo>();
	private Map<EObject, Property> impliedPropertiesUsed = null;
	private Map<EObject, List<String>> applicableExpandedProperties = null;
	
	private IMetricsProcessor metricsProcessor = null;
	protected JenaBasedSadlModelProcessor modelProcessor = null;
	private List<ConceptName> binaryOpLeftImpliedProperties;
	private List<ConceptName> binaryOpRightImpliedProperties;
	protected Object lastSuperCallExpression = null;
	private Map<EObject,Boolean> mEObjectsValidated = null; 

   	public enum ExplicitValueType {RESTRICTION, VALUE}
   	
   	public enum ImplicitPropertySide {LEFT, RIGHT, NONE, BOTH}
   	
    private static final Logger logger = LoggerFactory.getLogger(JenaBasedSadlModelValidator.class);

	/**
	 * This inner class captures the information about the left or right hand side of an expression that is subject
	 * to type checking, e.g., and assignment, a comparison, etc.
	 * 
	 */
	public class TypeCheckInfo {
		private EObject context = null;							// the parsetree element from which this is derived, 
																//	used to add an error, warning, or info so that a marker can be placed in the editor
    	private ConceptIdentifier expressionType = null;		// the identity, type, etc. of the concept that determines the type, 
    															//	e.g., the property of a "<property> of <subject>" expression
    	private Node typeCheckType = null;						// the type of the TypeCheckInfo which must match the other side of the expression,
    															//	e.g., the range of the property of a "<property> of <subject>" expression
    															// Note: this is a ProxyNode can be used for Unions and Intersections
    	private RDFNode explicitValue = null;					// the explicit value that is allowed, as in a hasValue restriction
    	private ExplicitValueType explicitValueType;			// The type of the explicit value
     	private RangeValueType rangeValueType = RangeValueType.CLASS_OR_DT;	
    															// the range type, one of RangeValueType.CLASS_OR_DT (Class or RDFDataType)
    															//	or LIST (a subclass of http://sadl.org/sadllistmodel#List)

     	private List<ConceptName> implicitProperties = null;	// Implied properties, if any, that apply to this expressionType
 
    	private List<TypeCheckInfo> compoundTypes = null;		// If this is a disjunction of multiple types, this contains the next
    															//	lower level of TypeCheckInfos in the hierarchy, e.g., a property whose
    															//	range is a union of classes or which is given range in multiple imports
    	
    	private String typeToExprRelationship = RANGE;		// the relationship between the typeCheckType and the expressionType, e.g., range (the default)
    															//	for explicit UnittedQuantity this will be the units 
    	private String additionalInformation = null;			// any additional information to explain the contents of the instance of the class.
    	private Severity severity = null;						// Guidance offered on the severity of any type mismatch involving this instance of the class.
    	
    	public TypeCheckInfo(ConceptIdentifier eType) {
    		setExpressionType(eType);
    	}
    	
    	/* Constructor for compound types (union, e.g. range) */
    	public TypeCheckInfo(ConceptIdentifier eType, JenaBasedSadlModelValidator validator, EObject ctx) {
    		setExpressionType(eType);
    		setContext(validator, ctx);
    	}
    	
    	/**
    	 * Constructor
    	 * @param eType Expression type, e.g., the property
    	 * @param tcType the type of the values which are valid
    	 * @param validator the model validator
    	 * @param ctx the EObject from which this is generated
    	 */
    	public TypeCheckInfo(ConceptIdentifier eType, Node tcType, JenaBasedSadlModelValidator validator, EObject ctx) {
    		setExpressionType(eType);
    		setTypeCheckType(tcType);
    		setContext(validator, ctx);
     	}
    	 	
		public TypeCheckInfo(ConceptIdentifier eType, Node tcType, JenaBasedSadlModelValidator validator, List<ConceptName> impliedProps, EObject ctx) {
			this(eType, tcType, validator, ctx);
    		if (impliedProps != null) {
   				implicitProperties = impliedProps;
    		}
		}

    	public TypeCheckInfo(ConceptName eType, RDFNode valueRestriction, ExplicitValueType valueType, JenaBasedSadlModelValidator validator, EObject ctx) {
    		setExpressionType(eType);
    		setExplicitValueType(valueType);
    		setContext(validator, ctx);
    		setExplicitValue(valueRestriction);
    		if (valueType.equals(ExplicitValueType.RESTRICTION)) {
    			setTypeToExprRelationship(RESTRICTED_TO);
    		}
    		else {
    			setTypeToExprRelationship("explicit value");
    		}
		}

    	public TypeCheckInfo(ConceptIdentifier eType, Node tcType, List<ConceptName> impliedProps, JenaBasedSadlModelValidator validator, EObject ctx) {
    		setExpressionType(eType);
    		setTypeCheckType(tcType);
			implicitProperties = impliedProps;
    		setContext(validator, ctx);
    	}
    	
		public boolean equals(Object o) {
			if (o instanceof TypeCheckInfo) {
				TypeCheckInfo other = (TypeCheckInfo) o;
				try {
				return context == other.context // Identity check should be fine for EObjects
					&& Objects.equals(getExpressionType(), other.getExpressionType())
					&& Objects.equals(getRangeValueType(), other.getRangeValueType())
					&& Objects.equals(getTypeCheckType(), other.getTypeCheckType());
				} catch (InvalidTypeException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
			}
			}
    		return false;
    	}
		
		@Override
		public int hashCode() {
			try {
			return Objects.hash(context, getExpressionType(), getRangeValueType(), getTypeCheckType());
			} catch (InvalidTypeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
		}
			return 0;
		}

		public ConceptIdentifier getExpressionType() {
			return expressionType;
		}

		public void setExpressionType(ConceptIdentifier expressionType) {
			this.expressionType = expressionType;
		}

		public Node getTypeCheckType() {
			return typeCheckType;
		}

		public void setTypeCheckType(Node typeCheckType) {
			this.typeCheckType = typeCheckType;
			if (typeCheckType instanceof NamedNode && ((NamedNode)typeCheckType).isList()) {
				setRangeValueType(RangeValueType.LIST);
			}
		}
		
		public RangeValueType getRangeValueType() throws InvalidTypeException {
			if(this.getCompoundTypes() != null){
				return getCompoundRangeValueType(this);
			}else{
				return rangeValueType;
			}
		}
		
		private RangeValueType getCompoundRangeValueType(TypeCheckInfo tci) throws InvalidTypeException{
			List<TypeCheckInfo> types = tci.getCompoundTypes();
			Iterator<TypeCheckInfo> iter = types.iterator();
			RangeValueType rvt = null;
			while(iter.hasNext()){
				TypeCheckInfo type = iter.next();
				RangeValueType rvt2 = null;
				if(type.getCompoundTypes() != null){
					rvt2 = getCompoundRangeValueType(type);
				}else{
					rvt2 = type.getRangeValueType();
				}
				
				if(rvt != null){
					if(rvt != rvt2){
						getModelProcessor().addTypeCheckingError("Property '" + tci.getExpressionType() + "' has incompatable Range Types, '" + rvt.toString() + "' and '" + rvt2.toString() + "'", tci.context); //TODO add new error message
					}
				}else{
					rvt = rvt2;
				}
			}
			
			return rvt;
		}
		
		protected void setContext(JenaBasedSadlModelValidator validator, EObject ctx) {
			this.context = ctx;
		}
		
		public EObject getContext() {
			return context;
		}

		public void setRangeValueType(RangeValueType rangeValueType) {
			this.rangeValueType = rangeValueType;
		}
		
		public String toString() {
			if (compoundTypes != null) {
				StringBuffer sb = new StringBuffer("Compound TypeCheckInfo([");
				for (int i = 0; i < compoundTypes.size(); i++) {
					if (i > 0) {
						sb.append(",");
					}
					sb.append(compoundTypes.get(i).toString());
				}
				sb.append("]");
				return sb.toString();
			}
			else {
				StringBuffer sb = new StringBuffer("TypeCheckInfo(");
//				if (getRangeValueType() != null && !getRangeValueType().equals(RangeValueType.CLASS_OR_DT)) {
//					sb.append(getRangeValueType().toString());
//					sb.append(" of values of type, ");
//				}
				sb.append(expressionType.toString());
				sb.append(", ");
				sb.append(typeCheckType != null ? typeCheckType.toString() : "unknown type");
				if (getExplicitValue() != null) {
					if (getExplicitValueType().equals(ExplicitValueType.RESTRICTION)) {
						sb.append(", restricted to explicit value '");
					}
					else {
						sb.append(", is the explicit value '");
					}
					sb.append(getExplicitValue().toString());
					sb.append("'");
				}
				sb.append(")");
				if (getImplicitProperties() != null) {
					if (getImplicitProperties().size() > 1)
						sb.append(" (has implied properties ");
					else 
						sb.append(" (has implied property ");
					sb.append(implicitPropertiesToString(getImplicitProperties()));
					sb.append(")");
				}
				return sb.toString();
			}
		}
		
		private Object implicitPropertiesToString(List<ConceptName> iprops) {
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < iprops.size(); i++) {
				if (i > 0) sb.append(", ");
				sb.append(getImplicitProperties().get(i).toFQString());
			}
			return sb.toString();
		}

		public void addImplicitProperty(ConceptName implicitProp) {
			if (implicitProp != null) {
				if (implicitProperties == null) {
					implicitProperties = new ArrayList<ConceptName>();
				}
				if (!implicitProperties.contains(implicitProp)) {
					implicitProperties.add(implicitProp);
				}
			}
		}
		
		public void addImplicitProperties(List<ConceptName> implicitProps) {
			if (implicitProperties == null) {
				implicitProperties = implicitProps;
			}
			else {
				implicitProperties.addAll(implicitProps);
			}
		}
		
		public List<ConceptName> getImplicitProperties() {
			return implicitProperties;
		}

		public List<TypeCheckInfo> getCompoundTypes() {
			return compoundTypes;
		}

		private void addCompoundType(TypeCheckInfo additionalType) {
			if (compoundTypes == null) {
				compoundTypes= new ArrayList<TypeCheckInfo>();
			}
			if (!compoundTypes.contains(additionalType)) {
				compoundTypes.add(additionalType);
			}
		}

		private String getTypeToExprRelationship() {
			return typeToExprRelationship;
		}

		public void setTypeToExprRelationship(String typeToExprRelationship) {
			this.typeToExprRelationship = typeToExprRelationship;
		}

		public RDFNode getExplicitValue() {
			return explicitValue;
		}

		public void setExplicitValue(RDFNode explicitValue) {
			this.explicitValue = explicitValue;
		}

		public ExplicitValueType getExplicitValueType() {
			return explicitValueType;
		}

		public void setExplicitValueType(ExplicitValueType explicitValueType) {
			this.explicitValueType = explicitValueType;
		}

		private String getAdditionalInformation() {
			return additionalInformation;
		}

		private void setAdditionalInformation(String additionalInformation) {
			this.additionalInformation = additionalInformation;
		}

		public Severity getSeverity() {
			return severity;
		}

		public void setSeverity(Severity severity) {
			this.severity = severity;
		}

		public boolean isList() throws InvalidTypeException {
			return getRangeValueType().equals(RangeValueType.LIST);
		}
    }
	
	public JenaBasedSadlModelValidator(ValidationAcceptor issueAcceptor, OntModel theJenaModel, JenaBasedSadlModelProcessor processor) {
		this(issueAcceptor, theJenaModel, new DeclarationExtensions(), processor, null);
	}
	
	public JenaBasedSadlModelValidator(ValidationAcceptor issueAcceptor, OntModel theJenaModel, DeclarationExtensions declarationExtensions, JenaBasedSadlModelProcessor processor, IMetricsProcessor metricsProcessor){
		this.theJenaModel = theJenaModel;
		this.declarationExtensions = declarationExtensions;
		this.setModelProcessor(processor) ;
		this.metricsProcessor  = metricsProcessor;
	}
	
	public boolean validate(Expression expr, String xsdType, String op, StringBuilder errorMessageBuilder) {
		List<String> operations = Arrays.asList(op.split("\\s+"));
		TypeCheckInfo exprTypeCheckInfo;
		try {
			exprTypeCheckInfo = getType(expr);
			NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.xint.getURI(), NodeType.DataTypeNode));
			ConceptName numberLiteralConceptName = getModelProcessor().namedNodeToConceptName(tctype);
			numberLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			TypeCheckInfo xsdTypeCheckInfo =  new TypeCheckInfo(numberLiteralConceptName, tctype, this, null);				
			if(!compareTypes(operations, expr, null, exprTypeCheckInfo, xsdTypeCheckInfo, ImplicitPropertySide.NONE)){
				if (createErrorMessage(errorMessageBuilder, exprTypeCheckInfo, xsdTypeCheckInfo, op, true, expr)) {
					return false;
				}
			}
		} catch (Throwable t) {
			return handleValidationException(expr, t);
		}
		return true;
	}
	
	public boolean validate(BinaryOperation expression, StringBuilder errorMessageBuilder) {
		setDefaultContext(expression);
		Expression leftExpression = expression.getLeft();
		Expression rightExpression = expression.getRight();
		String op = expression.getOp();
		
		return validateBinaryOperationByParts(expression, leftExpression, rightExpression, op,
				errorMessageBuilder, false);
	}

	public boolean validateBinaryOperationByParts(EObject expression, EObject leftExpression,
			EObject rightExpression, String op, StringBuilder errorMessageBuilder, boolean forceValidation) {
		List<String> operations = Arrays.asList(op.split("\\s+"));
		boolean errorsFound = false;
		boolean lIsLeftListType = false;
		boolean lIsRightListType = false;
		if (!registerEObjectValidateCalled(expression) && !forceValidation) {
			// if there were errors they were reported on the first call
			return !errorsFound;
		}
		
		if(skipOperations(operations)){
			return true;
		}
		if (skipNonCheckedExpressions(leftExpression, rightExpression)) {
			return true;
		}
		if (!passLiteralConstantComparisonCheck(expression, leftExpression, rightExpression, op, errorMessageBuilder)) {
			errorsFound = true;
		}
		try {	
			boolean dontTypeCheck = false;
			TypeCheckInfo leftTypeCheckInfo = null;
			try {
				leftTypeCheckInfo = getType(leftExpression);
				if (getModelProcessor().isConjunction(op) || getModelProcessor().isDisjunction(op)) {
					// this can be treated as a boolean only (maybe even larger criteria?)
					if(leftTypeCheckInfo.getImplicitProperties() != null) {
						//property is implied
						Iterator<ConceptName> ipitr = leftTypeCheckInfo.getImplicitProperties().iterator();
						while (ipitr.hasNext()) {
							ConceptName cn = ipitr.next();
							Property prop = theJenaModel.getProperty(cn.getUri());
							if (prop.canAs(ObjectProperty.class)) {
								cn.setType(ConceptType.OBJECTPROPERTY);
							}
							else if (prop.canAs(DatatypeProperty.class)) {
								cn.setType(ConceptType.DATATYPEPROPERTY);
							}
							else {
								cn.setType(ConceptType.RDFPROPERTY);
							}
							TypeCheckInfo newltci = getTypeInfoFromRange(cn, prop, leftExpression);
							if (newltci.getTypeCheckType().getName().equals("boolean")) {
								issueAcceptor.addInfo("Implied property '" + getModelProcessor().conceptIdentifierToString(cn) + "' used (left side of '" + op + "') to pass type check", leftExpression);
								addImpliedPropertiesUsed(leftExpression, prop);
								break;
							}
						}
					}
					leftTypeCheckInfo = createBooleanTypeCheckInfo(leftExpression);						 
				} else if (getModelProcessor().elementIdentificationOperation(op)) {
					if(leftTypeCheckInfo.getRangeValueType().equals(RangeValueType.LIST)) {
						lIsLeftListType = true;
					}
					leftTypeCheckInfo = convertListTypeToElementOfListType(leftTypeCheckInfo);
				}
				
				if(leftExpression instanceof PropOfSubjectImpl) {
					Expression pred = ((PropOfSubjectImpl) leftExpression).getLeft();
					Expression subj = ((PropOfSubjectImpl) leftExpression).getRight();
					TypeCheckInfo predTci = getType(pred);
					TypeCheckInfo subjTci = getType(subj);
					if(predTci != null && 
							subjTci != null &&
							subjTci.getRangeValueType().equals(RangeValueType.LIST) &&
							pred instanceof ElementInListImpl) {
		                    
						if(predTci.getImplicitProperties() != null) {
		                        //property is implied
		                        Iterator<ConceptName> ipitr = predTci.getImplicitProperties().iterator();
		                        while (ipitr.hasNext()) {
		                            ConceptName cn = ipitr.next();
		                            Property prop = theJenaModel.getProperty(cn.getUri());
		                            if (prop.canAs(ObjectProperty.class)) {
		                                cn.setType(ConceptType.OBJECTPROPERTY);
		                            }
		                            else if (prop.canAs(DatatypeProperty.class)) {
		                                cn.setType(ConceptType.DATATYPEPROPERTY);
		                            }
		                            else {
		                                cn.setType(ConceptType.RDFPROPERTY);
		                            }
		                            TypeCheckInfo newltci = getTypeInfoFromRange(cn, prop, pred);
		                            if (newltci.getTypeCheckType().getName().equals(subjTci.getTypeCheckType().getName())) {
		                                issueAcceptor.addInfo("Implied property '" + getModelProcessor().conceptIdentifierToString(cn) + "' used (left side of 'of') to pass type check", pred);
		                                addImpliedPropertiesUsed((ElementInListImpl)pred, prop);
		                                break;
		                            }
		                        }
			                }
			            }
				}
					
				
				
			} catch (DontTypeCheckException e) {
				dontTypeCheck = true;
			}
			if (useImpliedProperties(op)) {
				setBinaryOpLeftImpliedProperties(leftTypeCheckInfo != null ? leftTypeCheckInfo.getImplicitProperties() : null);
			}
			
			TypeCheckInfo rightTypeCheckInfo = null;
			try {
				rightTypeCheckInfo = getType(rightExpression);
				if (getModelProcessor().isConjunction(op) || getModelProcessor().isDisjunction(op)) {
					// this can be treated as a boolean only (maybe even larger criteria?)
					if(rightTypeCheckInfo.getImplicitProperties() != null) {
						//property is implied
						Iterator<ConceptName> ipitr = rightTypeCheckInfo.getImplicitProperties().iterator();
						while (ipitr.hasNext()) {
							ConceptName cn = ipitr.next();
							Property prop = theJenaModel.getProperty(cn.getUri());
							if (prop.canAs(ObjectProperty.class)) {
								cn.setType(ConceptType.OBJECTPROPERTY);
							}
							else if (prop.canAs(DatatypeProperty.class)) {
								cn.setType(ConceptType.DATATYPEPROPERTY);
							}
							else {
								cn.setType(ConceptType.RDFPROPERTY);
							}
							TypeCheckInfo newltci = getTypeInfoFromRange(cn, prop, rightExpression);
							if (newltci.getTypeCheckType().getName().equals("boolean")) {
								issueAcceptor.addInfo("Implied property '" + getModelProcessor().conceptIdentifierToString(cn) + "' used (right side of '" + op + "') to pass type check", rightExpression);
								addImpliedPropertiesUsed(rightExpression, prop);
								break;
							}
						}
					}
					rightTypeCheckInfo = createBooleanTypeCheckInfo(rightExpression);
				}
				else if (getModelProcessor().elementIdentificationOperation(op)) {
					if(rightTypeCheckInfo.getRangeValueType().equals(RangeValueType.LIST)) {
						lIsRightListType = true;
					}
					rightTypeCheckInfo = convertListTypeToElementOfListType(rightTypeCheckInfo);
					
					
					TypeCheckInfo predTci = getType(leftExpression);
					TypeCheckInfo subjTci = getType(rightExpression);
					if(!predTci.getTypeCheckType().getName().equals(subjTci.getTypeCheckType().getName()) &&
							predTci.getRangeValueType().equals(RangeValueType.LIST) &&  
							subjTci.getImplicitProperties() != null) {
		                   
	                        //property is implied
	                        Iterator<ConceptName> ipitr = subjTci.getImplicitProperties().iterator();
	                        while (ipitr.hasNext()) {
	                            ConceptName cn = ipitr.next();
	                            Property prop = theJenaModel.getProperty(cn.getUri());
	                            if (prop.canAs(ObjectProperty.class)) {
	                                cn.setType(ConceptType.OBJECTPROPERTY);
	                            }
	                            else if (prop.canAs(DatatypeProperty.class)) {
	                                cn.setType(ConceptType.DATATYPEPROPERTY);
	                            }
	                            else {
	                                cn.setType(ConceptType.RDFPROPERTY);
	                            }
	                            TypeCheckInfo newltci = getTypeInfoFromRange(cn, prop, leftExpression);
	                            if (newltci.getTypeCheckType().getName().equals(predTci.getTypeCheckType().getName())) {
	                                issueAcceptor.addInfo("Implied property '" + getModelProcessor().conceptIdentifierToString(cn) + "' used (right side of '" + op + "')to pass type check", rightExpression);
	                                addImpliedPropertiesUsed(leftExpression, prop);
	                                return true;
	                            }
	                        }
			                
			            }
					
				}
			} catch (DontTypeCheckException e) {
				dontTypeCheck = true;
			}
			if (useImpliedProperties(op)) {
				setBinaryOpRightImpliedProperties(rightTypeCheckInfo != null ? rightTypeCheckInfo.getImplicitProperties() : null);
			}
			
			if (leftTypeCheckInfo == null && rightTypeCheckInfo == null) {
				// this condition happens when a file is loaded in the editor and clean/build is invoked
				return true;
			}
			if (modelProcessor.isComparisonOperator(op) && (rightExpression instanceof NumberLiteral || 
					rightExpression instanceof UnaryExpression && ((UnaryExpression)rightExpression).getExpr() instanceof NumberLiteral)) {
				checkNumericRangeLimits(op, leftTypeCheckInfo, rightTypeCheckInfo);
			}
			if(getModelProcessor().elementIdentificationOperation(op)) {
				if(!applicableListToNonListType(leftTypeCheckInfo, rightTypeCheckInfo, lIsLeftListType, lIsRightListType)){
					if (createErrorMessage(errorMessageBuilder, leftTypeCheckInfo, rightTypeCheckInfo, op, false, null)) {
						setEObjectInError(expression);
						return false;
					}
				}
			}
			if(!dontTypeCheck && !compareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo, ImplicitPropertySide.NONE)){
				if (expression.eContainer() instanceof TestStatement && isQuery(leftExpression)) {
					// you can't tell what type a query will return
					return !errorsFound;
				}
				if (!rulePremiseVariableAssignment(operations, leftTypeCheckInfo,rightTypeCheckInfo)) {
					String effectiveOp = op;
					if (leftExpression instanceof Constant && ((Constant)leftExpression).getConstant().equals("value")) {
						effectiveOp = "matching value";
//						leftTypeCheckInfo.setRangeValueType(RangeValueType.CLASS_OR_DT);
					}
					if (createErrorMessage(errorMessageBuilder, leftTypeCheckInfo, rightTypeCheckInfo, effectiveOp, false, null)) {
						setEObjectInError(expression);
						return false;
					}
				}
			}
			//It's possible there may be a local type restriction
			if (isLocalTypeRestriction(op, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)) {
				TypeCheckInfo tciSubjSubj = getType(((PropOfSubject)leftExpression).getRight());
				Node propOfSubjSubj = tciSubjSubj.getTypeCheckType();
				NamedNode propnn = getModelProcessor().conceptNameToNamedNode((ConceptName) leftTypeCheckInfo.getExpressionType());
				NamedNode restrictedTo = (NamedNode) rightTypeCheckInfo.getTypeCheckType();
				TripleElement tr = new TripleElement(propOfSubjSubj, propnn, restrictedTo);
				getModelProcessor().handleLocalRestriction(expression, tr);
			}
			if (getModelProcessor().isEqualityInequalityComparisonOperator(op) && !errorsFound) {
				checkForApplicableExpandedProperties(expression, leftTypeCheckInfo, rightTypeCheckInfo);
			}
			TypeCheckInfo binopTci = combineBinaryOperationTypes(operations, expression, leftTypeCheckInfo, rightTypeCheckInfo);
			expressionsTypeCheckCache.put(expression, binopTci);
			return !errorsFound;
		} catch (Throwable t) {
			return handleValidationException(expression, t);
		}
	}
	
	/*
	 * Specific check on operators that require a list and a non-list type
	 */
	private boolean applicableListToNonListType(TypeCheckInfo aLeftTypeCheckInfo, TypeCheckInfo aRightTypeCheckInfo, boolean aIsLeftListType, boolean aIsRightListType) {
		if(aIsLeftListType && !aIsRightListType) {
			return true;
		}
		if(!aIsLeftListType && aIsRightListType) {
			return true;
		}
		
		if(aIsLeftListType) {
			aLeftTypeCheckInfo = convertElementOfListToListType(aLeftTypeCheckInfo);
		}
		if(aIsRightListType) {
			aRightTypeCheckInfo = convertElementOfListToListType(aRightTypeCheckInfo);
		}
		
		return false;
	}

	private boolean isLocalTypeRestriction(String op, EObject leftExpression, EObject rightExpression,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidTypeException {
		if (getModelProcessor().isEqualOperator(op) && leftExpression instanceof PropOfSubject && rightExpression instanceof Declaration) {
			if (leftTypeCheckInfo.getExpressionType() instanceof ConceptName && rightTypeCheckInfo.getTypeCheckType() instanceof NamedNode) {
				return true;
			}
		}
		return false;
	}

	private void checkForApplicableExpandedProperties(EObject expression, TypeCheckInfo leftTypeCheckInfo,
			TypeCheckInfo rightTypeCheckInfo) {
		Node type = null;
		// since type checking has passed to get to this point:
		//	if either side has no impliedProperties then the type can come from either side
		if (leftTypeCheckInfo != null && leftTypeCheckInfo.getImplicitProperties() == null && leftTypeCheckInfo.getTypeCheckType() != null) {
			type = leftTypeCheckInfo.getTypeCheckType();
		}
		else if (rightTypeCheckInfo != null && rightTypeCheckInfo.getImplicitProperties() == null && rightTypeCheckInfo.getTypeCheckType() != null) {
			type = rightTypeCheckInfo.getTypeCheckType();
		}
		// the other possibility is that both sides have implied properties (which should be the same) so the type is the common type
		else if (leftTypeCheckInfo != null && leftTypeCheckInfo.getTypeCheckType() != null && rightTypeCheckInfo != null && rightTypeCheckInfo.getTypeCheckType() != null) {
			Node ltct = leftTypeCheckInfo.getTypeCheckType();
			Node rtct = rightTypeCheckInfo.getTypeCheckType();
			if (ltct.equals(rtct)) {
				type = ltct;
			}
			else {
				if (ltct instanceof NamedNode && rtct instanceof NamedNode) {
					OntClass loc = theJenaModel.getOntClass(ltct.toFullyQualifiedString());
					OntClass roc = theJenaModel.getOntClass(rtct.toFullyQualifiedString());
					try {
						if (SadlUtils.classIsSubclassOf(roc, loc, true, null)) {
							type = ltct;
						}
						else if (SadlUtils.classIsSubclassOf(loc, roc, true, null)) {
							type = rtct;
						}
					} catch (CircularDependencyException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
				}
			}
		}
		else {
			try {
				throw new TranslationException("Unexpected failure to find binary operation type");
			} catch (TranslationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		if (type != null && type instanceof NamedNode && 
				(((NamedNode)type).getNodeType().equals(NodeType.ClassNode) || ((NamedNode)type).getNodeType().equals(NodeType.ClassListNode))) {
			// expanded properties only apply to classes
			Resource rsrc = theJenaModel.getResource(((NamedNode)type).toFullyQualifiedString());
			try {
				List<String> expProps = getModelProcessor().getExpandedProperties(rsrc);
				if (expProps != null) {
					addApplicableExpandedProperties(expression, expProps);
				}
			} catch (InvalidTypeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void addApplicableExpandedProperties(EObject expression, List<String> expProps) {
		if (applicableExpandedProperties == null) {
			applicableExpandedProperties = new HashMap<EObject, List<String>>();
		}
		applicableExpandedProperties.put(expression, expProps);
	}
	
	public Map<EObject, List<String>> getApplicableExpandedProperties() {
		return applicableExpandedProperties;
	}
	
	public void clearApplicableExpandedProperties() {
		if (applicableExpandedProperties != null) {
			applicableExpandedProperties.clear();
		}
	}

	/*
	 * Register EObject being type-checked to Validator.
	 */
	private boolean registerEObjectValidateCalled(EObject aExpression) {
		if (aExpression == null) return true;	// for ease in debugging--set expression to null to allow multiple passes in debugger
		if (mEObjectsValidated  == null) {
			mEObjectsValidated = new HashMap<EObject,Boolean>();
			mEObjectsValidated.put(aExpression,false);
			return true;
		}
		if (mEObjectsValidated.containsKey(aExpression)) {
			return false;
		}
		mEObjectsValidated.put(aExpression,false);
		return true;
	}
	
	/*
	 * Returns true if the Expression has a type-checking error
	 */
	public boolean isEObjectInError(EObject aExpression) {
		if(mEObjectsValidated == null) {
			return false;
		}
		Boolean lError = mEObjectsValidated.get(aExpression);
		if(lError == null) {
			return false;
		}
		return lError;
	}
	
	/*
	 * Sets EObject to an "in error" state but only if EObject has already been 
	 * registered by the type-checking algorithm.
	 */
	private void setEObjectInError(EObject aExpression) {
		if (mEObjectsValidated  == null) {
			return;
		}
		mEObjectsValidated.replace(aExpression,true);
	}

	private boolean passLiteralConstantComparisonCheck(EObject expression, EObject leftExpression,
			EObject rightExpression, String op, StringBuilder errorMessageBuilder) {
		if (modelProcessor.canBeNumericOperator(op)) {
			if (isLiteralOrConstant(leftExpression) && isLiteralOrConstant(rightExpression)) {
				errorMessageBuilder.append(SadlErrorMessages.COMPARISON_LITERALS_CONSTANTS.get(op));
				return false;
			}
		}
		return true;
	}

	private boolean isLiteralOrConstant(EObject expr) {
		if ((expr instanceof Constant && 
				(((Constant)expr).getConstant().equals(SadlConstants.CONSTANT_PI) || 
						((Constant)expr).getConstant().equals(SadlConstants.CONSTANT_E))) 
				|| expr instanceof NumberLiteral) {
			return true;
		}
		return false;
	}

	private boolean skipNonCheckedExpressions(EObject leftExpression, EObject rightExpression) {
		EObject rExpr = rightExpression;
		if (rightExpression instanceof UnaryExpression && ((UnaryExpression)rightExpression).getOp().equals("not")) {
			rExpr = ((UnaryExpression)rightExpression).getExpr();
		}
		if (rExpr instanceof Constant && isSkippedConstant((Constant)rExpr)) {
			return true;
		}
		return false;
	}
	
	@Override
	public boolean isSkippedConstant(Constant expr) {
		if (expr.getConstant().equals(SadlConstants.CONSTANT_KNOWN)) {
			return true;
		}
		return false;
	}
	
	private boolean useImpliedProperties(String op) {
		if (op.equals("contains") || op.equals("does not contain")) {
			return false;
		}
		return true;
	}

	private boolean rulePremiseVariableAssignment(List<String> operations, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidTypeException {
		if (possibleAssignment(operations)) {
			if (getModelProcessor().getRulePart().equals(SadlModelProcessor.RulePart.PREMISE)) {
				if (isVariable(leftTypeCheckInfo)) {
					if (!isAlreadyReferenced(leftTypeCheckInfo.getExpressionType())) {
						return true;
					}
				}
				else if (isVariable(rightTypeCheckInfo)) {
					if (!isAlreadyReferenced(rightTypeCheckInfo.getExpressionType())) {
						return true;
					}
				}
			}
		}
		return false;
	}

	private boolean isAlreadyReferenced(ConceptIdentifier expressionType) {
		// TODO Auto-generated method stub
		return false;
	}

	private boolean possibleAssignment(List<String> operations) {
		if (operations.contains("is") || operations.contains("=")) {
			return true;
		}
		return false;
	}

	private boolean isQuery(EObject expr) {
		if (expr instanceof StringLiteral) {
			String val = ((StringLiteral)expr).getValue();
			if (val.contains("select") && val.contains("where")) {
				return true;
			}
		}
		return false;
	}

	protected boolean handleValidationException(EObject expr, Throwable t) {
		try {
			if (t instanceof InvalidNameException) {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("Invalid Name"), expr);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				t.printStackTrace();
			} else if (t instanceof TranslationException) {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("Translation"), expr);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				t.printStackTrace();
			} else if (t instanceof URISyntaxException) {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("URI Syntax"), expr);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				t.printStackTrace();
			} else if (t instanceof IOException) {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("IO"), expr);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				t.printStackTrace();
			} else if (t instanceof ConfigurationException) {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("Configuration"), expr);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				t.printStackTrace();
			} else if (t instanceof NullPointerException){
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("Null Pointer"), expr);
			} else if (t instanceof DontTypeCheckException) {
				return true;
			} else if (t instanceof PropertyWithoutRangeException) {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.PROPERTY_WITHOUT_RANGE.get(((PropertyWithoutRangeException)t).getPropID()), expr);
				return true;
			} else if (t instanceof CircularDefinitionException) {
				t.printStackTrace();
			}
			else {
				t.printStackTrace();
			}
		}
		catch (Throwable t2) {
			try {
				getModelProcessor().addTypeCheckingError("Unexpected exception (" + t.getClass().getCanonicalName() + ": " + 
					t2.getMessage() + ") displaying validation error : " + t.getMessage(), expr);
			} catch (InvalidTypeException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
		}
		}
		return false;
	}

	protected boolean createErrorMessage(StringBuilder errorMessageBuilder, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo, String operation, boolean addToIssueAcceptor, EObject expr) throws InvalidTypeException {
		Severity specifiedSeverity = null;
		if (leftTypeCheckInfo != null && leftTypeCheckInfo.getSeverity() != null) {
			if (leftTypeCheckInfo.getSeverity().equals(Severity.IGNORE)) {
				return false;
			}
			else {
				specifiedSeverity = leftTypeCheckInfo.getSeverity();
			}
		}
		if (rightTypeCheckInfo != null && rightTypeCheckInfo.getSeverity() != null) {
			if (rightTypeCheckInfo.getSeverity().equals(Severity.IGNORE)) {
				return false;
			}
			else {
				if (specifiedSeverity == null) {
					specifiedSeverity = rightTypeCheckInfo.getSeverity();
				}
				else if (rightTypeCheckInfo.getSeverity().compareTo(specifiedSeverity) > 0) {
					specifiedSeverity = rightTypeCheckInfo.getSeverity();
				}
			}
		}
		String[] leftDesc = getTypeCheckInfoDescription(leftTypeCheckInfo);
		String[] rightDesc = getTypeCheckInfoDescription(rightTypeCheckInfo);
		
		String leftSide = "";
		String rightSide = "";
		String op = "";
		
		if (leftDesc == null) {
			leftSide = "Undetermined left type";
		}
		else {
			if (leftDesc.length > 0) {
				leftSide = leftDesc[0];
			}
			if (leftDesc.length > 1) {
				leftSide = leftSide + " " + leftDesc[1];
			}
		}
		
		if (getModelProcessor().isComparisonOperator(operation)) {
			op = "be compared (" + operation + ")";
		}
		else {
			op = "operate (" + operation + ")";
		}
		
		if (rightDesc == null) {
			rightSide = "Undetermined right type";
		}
		else {
			if (rightDesc.length > 0) {
				rightSide = rightDesc[0];
			}
			if (rightDesc.length > 1) {
				rightSide = rightSide + " " + rightDesc[1];
			}
		}
		if (leftSide.contains(",")) leftSide = leftSide + ",";
		errorMessageBuilder.append(SadlErrorMessages.VALIDATE_BIN_OP_ERROR.get(leftSide, op, rightSide));
		if (addToIssueAcceptor) {
			if (specifiedSeverity != null) {
				getModelProcessor().addIssueToAcceptor(errorMessageBuilder.toString(), specifiedSeverity, expr);
			}
			else {
				getModelProcessor().addTypeCheckingError(errorMessageBuilder.toString(), expr);
			}
		}
		return true;
	}

	private String[] getTypeCheckInfoDescription(TypeCheckInfo typeCheckInfo) throws InvalidTypeException {
		if (typeCheckInfo == null) {
			String[] result = new String[1];
			result[0] = "No type check info generated";
			return result;
		}
		else {
			StringBuilder sb1 = new StringBuilder();;
			StringBuilder sb2 = null;
			ConceptIdentifier typeExpr = typeCheckInfo.getExpressionType();
			if (typeExpr != null) {
				if (typeCheckInfo.getAdditionalInformation() != null) {
					sb1.append(" ");
					sb1.append(typeCheckInfo.getAdditionalInformation());
					sb1.append(" ");
				}
				sb1.append(getModelProcessor().conceptIdentifierToString(typeExpr));
			}
			if (typeExpr instanceof ConceptName) {
				ConceptType ct = ((ConceptName)typeExpr).getType();
				if (ct == null) {
					sb1.append(", type unknown ");
				}
				else if (ct.equals(ConceptType.INDIVIDUAL)) {
					sb1.append(", an instance of type ");
					sb2 = new StringBuilder();				}
				else if (ct.equals(ConceptType.ONTCLASS)) {
					sb1.append(", a class ");
				}
				else if (ct.equals(ConceptType.RDFDATATYPE)) {
					sb1.append(", an RDF datatype ");
					sb2= new StringBuilder();				}
				else if (ct.equals(ConceptType.RDFPROPERTY)) {
					sb1.append(", an RDF property ");
					if (rdfPropertyTypeCheckInfoHasRange(typeCheckInfo)) {
						sb1.append("with ");
						sb1.append(typeCheckInfo.getTypeToExprRelationship());
						sb1.append(" ");
					}
					sb2= new StringBuilder();				}
				else if (ct.equals(ConceptType.ANNOTATIONPROPERTY)) {
					sb1.append(", an  annotation property ");
				}
				else if (ct.equals(ConceptType.DATATYPEPROPERTY)) {
					sb1.append(", a datatype property with ");
					sb1.append(typeCheckInfo.getTypeToExprRelationship());
					sb1.append(" ");
					sb2 = new StringBuilder();				}
				else if (ct.equals(ConceptType.OBJECTPROPERTY)) {
					sb1.append(", an object property with ");
					sb1.append(typeCheckInfo.getTypeToExprRelationship());
					sb1.append(" ");
					sb2= new StringBuilder();				}
				else if (ct.equals(ConceptType.VARIABLE)) {
					sb1.append(", a variable of type ");
					sb2 = new StringBuilder();				}
			}
			if (typeCheckInfo.getCompoundTypes() != null) {
				List<String> compoundTypeList = new ArrayList<String>();
				for (int i = 0; i < typeCheckInfo.getCompoundTypes().size(); i++) {
					TypeCheckInfo tci = typeCheckInfo.getCompoundTypes().get(i);
					String[] tciresult = getTypeCheckInfoDescription(tci);
					if (tciresult != null && tciresult.length > 1 && !compoundTypeList.toString().contains(tciresult[1])) {
						compoundTypeList.add(tciresult[1]);
					}
				}
				if (sb2 == null) sb2 = new StringBuilder();
				for (int i = 0; i < compoundTypeList.size(); i++) {
					if (i > 0) sb2.append(" or ");
					sb2.append(compoundTypeList.get(i));
				}
			}
			else {
				if (typeCheckInfo.getRangeValueType().equals(RangeValueType.LIST) && 
						(typeCheckInfo.getTypeCheckType() instanceof NamedNode && ((NamedNode)typeCheckInfo.getTypeCheckType()).isList())) {
					if (sb2 == null) sb2 = new StringBuilder();
					String lengthOrRange = getListLengthAsString((NamedNode)typeCheckInfo.getTypeCheckType());
					sb2.append("a List " + lengthOrRange + "of values of type ");
				}
				if (sb2 != null && typeCheckInfo.getTypeCheckType() != null) {
					sb2.append(getModelProcessor().nodeToString(typeCheckInfo.getTypeCheckType()));
				}
				else if (typeCheckInfo.getExplicitValue() != null) {
					RDFNode ev = typeCheckInfo.getExplicitValue();
					if (typeCheckInfo.getExplicitValueType().equals(ExplicitValueType.VALUE)) {
						sb1.replace(0, sb1.length(), "explicit value ");
					}
					if (ev.isLiteral()) {
						try {
							sb2.append(getModelProcessor().rdfNodeToString(ev.asLiteral()));
						}
						catch (DatatypeFormatException e) {
							getModelProcessor().addTypeCheckingError(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("Datatype Format"), typeCheckInfo.context);
							//addError(e.getMessage(), typeCheckInfo.context);
						}
					}
					else {
						sb2.append(getModelProcessor().rdfNodeToString(ev));
					}
				}
			}
			String[] result = sb2 != null ? new String[2] : new String[1];
			result[0] = sb1.toString();
			if (sb2 != null) {
				result[1] = sb2.toString();
			}
			return result;
		}
//		String leftName = leftTypeCheckInfo != null ? leftTypeCheckInfo.expressionType != null ? leftTypeCheckInfo.expressionType.toString() : "UNIDENTIFIED" : "UNIDENTIFIED";
//		String rightName = rightTypeCheckInfo != null ? rightTypeCheckInfo.expressionType != null ? rightTypeCheckInfo.expressionType.toString() : "UNIDENTIFIED" : "UNIDENTIFIED";
//		String leftType;
//		String leftRange;
//		if (leftTypeCheckInfo == null) {
//			leftType = "UNIDENTIFIED";
//			leftRange = "UNIDENTIFIED";
//		}
//		else if (leftTypeCheckInfo.compoundTypes == null) {
//			leftType = leftTypeCheckInfo.typeCheckType != null ? leftTypeCheckInfo.typeCheckType.toString() : "UNIDENTIFIED";
//			leftRange = leftTypeCheckInfo.rangeValueType != null ? leftTypeCheckInfo.rangeValueType.toString() : "UNIDENTIFIED";
//		}
//		else {
//			StringBuilder sb = new StringBuilder();
//			for (int i = 0; i < leftTypeCheckInfo.getCompoundType().size(); i++) {
//				if (i > 0) {
//					sb.append(" or ");
//				}
//				TypeCheckInfo tic = leftTypeCheckInfo.getCompoundType().get(i);
//				sb.append(tic != null ? tic.typeCheckType != null ? tic.typeCheckType.toString() : "UNIDENTIFIED" : "UNIDENTIFIED");
//			}
//			leftType = sb.toString();
//			sb = new StringBuilder();
//			for (int i = 0; i < leftTypeCheckInfo.getCompoundType().size(); i++) {
//				if (i > 0) {
//					sb.append(" or ");
//				}
//				TypeCheckInfo tic = leftTypeCheckInfo.getCompoundType().get(i);
//				sb.append(tic != null ? tic.rangeValueType != null ? tic.rangeValueType.toString() : "UNIDENTIFIED" : "UNIDENTIFIED");
//			}
//			leftRange = sb.toString();
//		}
//		return null;
	}

	private String getListLengthAsString(NamedNode node) {
		StringBuilder sb = new StringBuilder();	
		int length = node.getListLength();
		int minLength = node.getMinListLength();
		int maxLength = node.getMaxListLength();
		if(length != -1 || minLength != -1 || maxLength != -1) {		
			sb.append("length ");
			if(minLength != -1 || maxLength != -1) {
				if(minLength == -1) {
					sb.append("0");
				}else {
					sb.append(minLength);
				}
				sb.append("-");
				if(maxLength == -1) {
					sb.append("*");
				}else {
					sb.append(maxLength);
				}
			}else {
				sb.append(length);
			}
			sb.append(" ");
		}
		return sb.toString();
	}

	private boolean rdfPropertyTypeCheckInfoHasRange(TypeCheckInfo typeCheckInfo) {
		if (typeCheckInfo.getTypeCheckType() != null) {
			return true;
		}
		if (typeCheckInfo.getCompoundTypes() != null) {
			for (int i = 0; i < typeCheckInfo.getCompoundTypes().size(); i++) {
				TypeCheckInfo next = typeCheckInfo.getCompoundTypes().get(i);
				boolean b = rdfPropertyTypeCheckInfoHasRange(next);
				if (b) {
					return true;
				}
			}
		}
		return false;
	}

	protected boolean skipOperations(List<String> operations) {
		if(operations.contains("and") || operations.contains("or")){
			return true;
		}
		return false;
	}

	protected TypeCheckInfo getType(EObject expression) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException{
		if (expressionsTypeCheckCache != null && expressionsTypeCheckCache.containsKey(expression)) {
			return expressionsTypeCheckCache.get(expression);
		}
		TypeCheckInfo returnedTci = null;
		
		if(expression instanceof Name){
			returnedTci =  getType((Name)expression);
		}else if(expression instanceof SadlResource){
			returnedTci =  getType((SadlResource)expression);
		}
		else if(expression instanceof Declaration){
			SadlTypeReference decltype = ((Declaration)expression).getType();
			if (decltype != null) {
				returnedTci =  getType(decltype);
			}
			else {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.NULL_TYPE.toString(), expression);
			}
			//Need to return passing case for time being
//			ConceptName declarationConceptName = new ConceptName("todo");
//			return new TypeCheckInfo(declarationConceptName, declarationConceptName);
		}
		else if(expression instanceof StringLiteral || expression instanceof SadlStringLiteral) {
			NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.xstring.getURI(), NodeType.DataTypeNode));
			ConceptName stringLiteralConceptName = getModelProcessor().namedNodeToConceptName(tctype);
			returnedTci =  new TypeCheckInfo(stringLiteralConceptName, tctype, this, expression);
		}
		else if(expression instanceof NumberLiteral || expression instanceof SadlNumberLiteral){
			BigDecimal value = null;
			Literal litval = null;
			if (expression instanceof NumberLiteral) {
				value = ((NumberLiteral)expression).getValue();
			}
			else {	// SadlNumberLiteral
				if (((SadlNumberLiteral)expression).getUnit() != null && !getModelProcessor().ignoreUnittedQuantities) {
					return getUnittedQuantityTypeCheckInfo(expression, ((SadlNumberLiteral)expression).getUnit());
				}
				String strval = ((SadlNumberLiteral)expression).getLiteralNumber().toPlainString();
				if (strval.indexOf('.') >= 0) {
					value = BigDecimal.valueOf(Double.parseDouble(strval));
				}
				else {
					value = BigDecimal.valueOf(Long.parseLong(strval));
				}
			}
			if (expression.eContainer() != null && expression.eContainer() instanceof UnaryExpression && ((UnaryExpression)expression.eContainer()).getOp().equals("-")) {
				value = value.negate();
			}
			ConceptName numberLiteralConceptName = null;
			Node tctype = null;
			if (value.stripTrailingZeros().scale() <= 0 || value.remainder(BigDecimal.ONE).compareTo(BigDecimal.ZERO) == 0) {
				if (value.compareTo(BigDecimal.valueOf(MAX_INT)) <= 0 && value.compareTo(BigDecimal.valueOf(MIN_INT)) >= 0) {
					tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.xint.getURI(), NodeType.DataTypeNode));
					numberLiteralConceptName = getModelProcessor().namedNodeToConceptName((NamedNode) tctype);
					litval = theJenaModel.createTypedLiteral(value.intValue());
				}
				else {
					numberLiteralConceptName = new ConceptName(XSD.xlong.getURI());
					tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.xlong.getURI(), NodeType.DataTypeNode));
					if (value.compareTo(BigDecimal.valueOf(MAX_LONG)) > 0 || value.compareTo(BigDecimal.valueOf(MIN_LONG)) < 0) {
						getModelProcessor().addTypeCheckingError("Error converting to a number", expression);
					}
					else {
						litval = theJenaModel.createTypedLiteral(value.longValue());
					}
				}
			}
			else {
				numberLiteralConceptName = new ConceptName(XSD.decimal.getURI());
				litval = theJenaModel.createTypedLiteral(value.doubleValue());
				tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.decimal.getURI(), NodeType.DataTypeNode));
			}
			numberLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			TypeCheckInfo litTci;
			if (litval != null) {
				litTci = new TypeCheckInfo(numberLiteralConceptName, litval, ExplicitValueType.VALUE, this, expression); 
			}
			else {
				litTci = new TypeCheckInfo(numberLiteralConceptName, createNamedNode(numberLiteralConceptName.toFQString(), OntConceptType.LITERAL), this, expression);
			}
			litTci.setTypeCheckType(tctype);
			returnedTci =  litTci;
		}
		else if (expression instanceof UnitExpression) {
			returnedTci =  getUnittedQuantityTypeCheckInfo(((UnitExpression)expression).getLeft(), ((UnitExpression)expression).getUnit());
		}
		else if(expression instanceof BooleanLiteral || expression instanceof SadlBooleanLiteral){
			returnedTci =  createBooleanTypeCheckInfo(expression);
		}
		else if(expression instanceof Constant){
			returnedTci =  getType((Constant)expression);
		}
		else if (expression instanceof SadlConstantLiteral) {
			returnedTci =  getType((SadlConstantLiteral)expression);
		}
		else if(expression instanceof ValueTable){
			ConceptName declarationConceptName = new ConceptName("TODO");
			returnedTci =  new TypeCheckInfo(declarationConceptName, null, this, expression);
		}
		else if(expression instanceof PropOfSubject){
			returnedTci =  getType((PropOfSubject)expression);
		}
		else if(expression instanceof SubjHasProp){
			returnedTci =  getType((SubjHasProp)expression);
		}
		else if (SadlASTUtils.isCommaSeparatedAbbreviatedExpression(expression)) {
			// validate the property initializations within
			validateCommaSeparatedAbreviatedExpression((SubjHasProp) expression);
			returnedTci =  getType(((SubjHasProp)expression).getLeft());
		}
		else if(expression instanceof UnaryExpression){
			returnedTci =  getType(((UnaryExpression) expression).getExpr());
		}
		else if (expression instanceof SadlUnaryExpression) {
			returnedTci =  getType(((SadlUnaryExpression)expression).getValue());
		}
		else if(expression instanceof ElementInList){
			returnedTci =  getType((ElementInList)expression);
		}
		else if(expression instanceof BinaryOperation){
			returnedTci =  getType((BinaryOperation)expression);
		}
		else if (expression instanceof Sublist) {
			// the type is the type of the list
			TypeCheckInfo listtype = getType((((Sublist)expression).getList()));
			if (listtype != null && !listtype.getRangeValueType().equals(RangeValueType.LIST)) {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.IS_NOT_A.get("this","list"), ((Sublist)expression).getList());
			}
			returnedTci =  listtype;
		}
		else if (expression instanceof SadlPrimitiveDataType)  {
			returnedTci =  getType((SadlPrimitiveDataType)expression);
		}
		else if (expression instanceof SadlSimpleTypeReference) {
			returnedTci =  getType((SadlSimpleTypeReference)expression);
		}
		else if (expression instanceof SadlIntersectionType) {
			returnedTci =  getType((SadlIntersectionType)expression);
		}
		else if (expression instanceof SadlPropertyCondition) {
			returnedTci =  getType((SadlPropertyCondition)expression);
		}
		else if (expression instanceof SadlResource) {
			returnedTci =  getType((SadlResource)expression);
		}
		else if (expression instanceof SadlTypeReference) {
			returnedTci =  getType((SadlTypeReference)expression);
		}
		else if (expression instanceof SadlUnionType) {
			returnedTci =  getType((SadlUnionType)expression);
		}
		else if (expression instanceof SadlInstance) {
			returnedTci =  getType((SadlInstance)expression);
		}
		else if (expression != null) {
			getModelProcessor().addTypeCheckingError(SadlErrorMessages.DECOMPOSITION_ERROR.get(expression.toString()), expression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			throw new TranslationException("Unhandled expression type: " + expression.getClass().getCanonicalName());
		}
		if (returnedTci != null) {
			expressionsTypeCheckCache.put(expression, returnedTci);
		}
		return returnedTci;
	}

	private TypeCheckInfo getType(SadlInstance expression) throws InvalidTypeException, DontTypeCheckException,
			CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException,
			ConfigurationException, CircularDependencyException, PropertyWithoutRangeException {
		SadlResource inst = expression.getInstance();
		if (inst == null) {
			SadlTypeReference typ = expression.getType();
			if (typ != null && typ instanceof SadlSimpleTypeReference) {
				inst = ((SadlSimpleTypeReference)typ).getType();
				if (declarationExtensions.getConceptUri(inst).equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI) && getModelProcessor().ignoreUnittedQuantities) {
					if (expression instanceof SadlNestedInstance) {
						Iterator<SadlPropertyInitializer> pinititr = ((SadlNestedInstance)expression).getPropertyInitializers().iterator();
						while (pinititr.hasNext()) {
							SadlPropertyInitializer spinit = pinititr.next();
							if (declarationExtensions.getConceptUri(spinit.getProperty()).equals(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI)) {
								return getType(spinit.getProperty());
							}
						}
					}
				}
			}
		}
		if (inst != null) {
			TypeCheckInfo insttci = getType(inst);
			if (insttci != null && insttci.getTypeCheckType() == null) {
				SadlTypeReference typ = expression.getType();
				if (typ != null && typ instanceof SadlSimpleTypeReference) {
					SadlResource typsr = ((SadlSimpleTypeReference)typ).getType();
					insttci.setTypeCheckType(getModelProcessor().validateNamedNode(new NamedNode(declarationExtensions.getConceptUri(typsr), NodeType.ClassNode)));
				}
			}
			return insttci;
		}
		else {
			getModelProcessor().addTypeCheckingError("Unhandled condition of SadlInstance", expression);
			return null;
		}
	}

	private TypeCheckInfo getType(ElementInList expression)
			throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException,
			DontTypeCheckException, CircularDefinitionException, InvalidTypeException, CircularDependencyException,
			PropertyWithoutRangeException {
		Expression el = expression.getElement();
		if (el instanceof PropOfSubject) {
			TypeCheckInfo listtype = getType(((PropOfSubject)el).getRight());
			if (listtype == null) {
				getModelProcessor().addTypeCheckingError("Unable to get the List type", el);
			}
			else if (listtype.getRangeValueType() != RangeValueType.LIST) {
				getModelProcessor().addTypeCheckingError("Expected a List", el);
			}
			else {
				// the element's type is the type of the list but not necessarily a list
				listtype = convertListTypeToElementOfListType(listtype);
			}
			return listtype;
		}
		else {
			return getType(el);
		}
//		else {
//			getModelProcessor().addIssueToAcceptor(SadlErrorMessages.UNHANDLED.get("element type in element in list construct. ", el.getClass().getCanonicalName() + "; please report"), expression);
//			if (metricsProcessor != null) {
//				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_ERROR_URI);
//			}
//			return null;
//		}
	}

	private TypeCheckInfo getType(SubjHasProp expression) throws CircularDefinitionException, DontTypeCheckException,
			InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException,
			InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		if (SadlASTUtils.isUnitExpression(expression)) {
			NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI, NodeType.ClassNode));
			ConceptName uqcn = getModelProcessor().namedNodeToConceptName(tctype);
			TypeCheckInfo tci = new TypeCheckInfo(uqcn, tctype, this, expression);
			tci.setTypeToExprRelationship(SadlASTUtils.getUnitAsString(expression));
			return tci;
		}
		else if (expression.isComma() && expression.getRight() == null) {
			getModelProcessor().addTypeCheckingError(SadlErrorMessages.TYPE_COMPARISON.toString(), expression);
			return null;
		}
		else if (!getModelProcessor().isDeclaration(expression) && expression.eContainer() instanceof BinaryOperation || 
				expression.eContainer() instanceof SelectExpression ||
				expression.eContainer() instanceof AskExpression ||
				expression.eContainer() instanceof ConstructExpression) {
			// we are comparing or assigning this to something else so we want the type of the root (if there is a chain) property
			if (expression.getProp() instanceof SadlResource) {
				SadlResource prop = expression.getProp();
				TypeCheckInfo propTci = getType(prop);
				if (propTci != null && propTci.getTypeCheckType() == null && isInQuery(expression) && 
						propTci.getExpressionType() instanceof ConceptName && ((ConceptName)propTci.getExpressionType()).getType().equals(ConceptType.VARIABLE)) {
					throw new DontTypeCheckException();	// OK to not get a type for a property which is a variable in a query
				}
				return propTci;
			}
			else {
				getModelProcessor().addTypeCheckingError("This subject-has-property construct isn't properly validated, please report.", expression);
				return null;
			}
		}
		else {
			if (modelProcessor.getTarget() instanceof Rule) {
				return getType(expression.getProp());
			}
			else {
				Declaration subjHasPropInDeclaration = subjHasPropIsDeclaration((SubjHasProp) expression);  // are we in a Declaration (a real declaration--the type is a class)
				if (subjHasPropInDeclaration != null) {
					return getType(subjHasPropInDeclaration);
				}
				else {
					getModelProcessor().addTypeCheckingError("This appears to be a declaration that isn't fully supported; should it be nested (in parentheses)", expression);
					return null;
				}
			}
		}
	}

	private TypeCheckInfo getType(BinaryOperation expression) throws CircularDependencyException,
			InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException,
			DontTypeCheckException, CircularDefinitionException, InvalidTypeException, PropertyWithoutRangeException {
		List<String> operations = Arrays.asList(expression.getOp().split("\\s+"));
		TypeCheckInfo leftTypeCheckInfo = unifyCompoundTypeCheckInfos(getType(expression.getLeft()));
		TypeCheckInfo rightTypeCheckInfo = unifyCompoundTypeCheckInfos(getType(expression.getRight()));
		if (expression.getLeft() instanceof PropOfSubject && ((BinaryOperation)expression).getRight() instanceof Declaration) {
			TypeCheckInfo subjtype = getType(((PropOfSubject)expression.getLeft()).getRight());
			if (subjtype != null) {
				Node subject = subjtype.getTypeCheckType();
				if (subject != null) {
					ConceptIdentifier ltcetype = leftTypeCheckInfo.getExpressionType();
					if (ltcetype instanceof ConceptName) {
						NamedNode pred = getModelProcessor().conceptNameToNamedNode((ConceptName) ltcetype);
						TripleElement tr = new TripleElement(subject, pred, rightTypeCheckInfo.getTypeCheckType());
						getModelProcessor().handleLocalRestriction(expression, tr);
					}
				}
			}
		}

		TypeCheckInfo binopreturn = combineBinaryOperationTypesWithComparison(operations, expression, expression.getLeft(), expression.getRight(), 
				leftTypeCheckInfo, rightTypeCheckInfo, ImplicitPropertySide.NONE);
		if (getModelProcessor().isNumericOperator(expression.getOp())) {
			TypeCheckInfo uqTci = getTypeCompatibleWithOperationOnUnittedQuantities(((BinaryOperation)expression).getOp(), leftTypeCheckInfo, rightTypeCheckInfo);
			if (uqTci != null) {
				return uqTci;
			}
			if (leftTypeCheckInfo != null && !isNumeric(leftTypeCheckInfo) && !isNumericWithImpliedProperty(leftTypeCheckInfo, ((BinaryOperation)expression).getLeft())) {
				getModelProcessor().addTypeCheckingError("Numeric operator requires numeric arguments", ((BinaryOperation)expression).getLeft());
			}
			if (rightTypeCheckInfo != null && !isNumeric(rightTypeCheckInfo) && !isNumericWithImpliedProperty(rightTypeCheckInfo, ((BinaryOperation)expression).getRight())) {
				getModelProcessor().addTypeCheckingError("Numeric operator requires numeric arguments", ((BinaryOperation)expression).getRight());
			}
			NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.decimal.getURI(), NodeType.DataTypeNode));
			ConceptName decimalLiteralConceptName = getModelProcessor().namedNodeToConceptName(tctype);
			decimalLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			return new TypeCheckInfo(decimalLiteralConceptName, tctype, this, expression);
		}
		if (binopreturn != null) {
			return binopreturn;
		}
		else {
			return createBooleanTypeCheckInfo(expression);
		}
	}

	private TypeCheckInfo createBooleanTypeCheckInfo(EObject expression) throws TranslationException, InvalidNameException, InvalidTypeException {
		NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.xboolean.getURI(), NodeType.DataTypeNode));
		ConceptName booleanLiteralConceptName = getModelProcessor().namedNodeToConceptName(tctype);
		return new TypeCheckInfo(booleanLiteralConceptName, tctype, this, expression);
	}

	private TypeCheckInfo getTypeCompatibleWithOperationOnUnittedQuantities(String op, TypeCheckInfo leftTypeCheckInfo,
			TypeCheckInfo rightTypeCheckInfo) {
		if (!modelProcessor.isNumericOperator(op)) {
			return null;
		}
		if (leftTypeCheckInfo == null || rightTypeCheckInfo == null) {
			return null;
		}
		if (!leftTypeCheckInfo.getTypeCheckType().toFullyQualifiedString().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI) ||
				!rightTypeCheckInfo.getTypeCheckType().toFullyQualifiedString().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
			return null;
		}
		if (op.equals("+") || op.equals("-")) {
			if (leftTypeCheckInfo.getTypeToExprRelationship() != null && rightTypeCheckInfo.getTypeToExprRelationship() != null &&
					leftTypeCheckInfo.getTypeToExprRelationship().equals(rightTypeCheckInfo.getTypeToExprRelationship())) {
				return leftTypeCheckInfo;
			}
			if (leftTypeCheckInfo.getTypeToExprRelationship().equals(RANGE) && !rightTypeCheckInfo.getTypeToExprRelationship().equals(RANGE)) {
				// could issue warning that units may not match
				return leftTypeCheckInfo;
			}
			if (!leftTypeCheckInfo.getTypeToExprRelationship().equals(RANGE) && rightTypeCheckInfo.getTypeToExprRelationship().equals(RANGE)) {
				// could issue warning that units may not match
				return rightTypeCheckInfo;
			}
		}
		if (op.equals("/") || op.equals("*") || op.equals("^")) {
			if (leftTypeCheckInfo.getTypeToExprRelationship() != null && rightTypeCheckInfo.getTypeToExprRelationship() != null) {
				leftTypeCheckInfo.setTypeToExprRelationship(leftTypeCheckInfo.getTypeToExprRelationship() + op + rightTypeCheckInfo.getTypeToExprRelationship());
				return leftTypeCheckInfo;
			}
		}
		return null;
	}

	private Declaration getEmbeddedDeclaration(EObject expr) {
		if (expr instanceof SubjHasProp) {
			return getEmbeddedDeclaration(((SubjHasProp)expr).getLeft());
		}
		else if (expr instanceof BinaryOperation) {
			Declaration decl = getEmbeddedDeclaration(((BinaryOperation)expr).getLeft());
			if (decl != null) {
				return decl;
			}
			decl = getEmbeddedDeclaration(((BinaryOperation)expr).getRight());
			if (decl != null) {
				return decl;
			}
		}
		else if (expr instanceof UnaryExpression && ((UnaryExpression)expr).getExpr() instanceof Declaration) {
			return (Declaration) ((UnaryExpression)expr).getExpr();
		}
		else if (expr instanceof Declaration) {
			return (Declaration) expr;
		}
		return null;
	}

	private TypeCheckInfo getUnittedQuantityTypeCheckInfo(EObject expression, String unit)
			throws InvalidTypeException, InvalidNameException, TranslationException {
		NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI, NodeType.ClassNode));
		ConceptName uqcn = getModelProcessor().namedNodeToConceptName(tctype);
		List<ConceptName> impliedProperties = getImpliedProperties(theJenaModel.getOntResource(uqcn.getUri()));
		if (impliedProperties != null) {
			TypeCheckInfo tci = new TypeCheckInfo(uqcn, tctype, impliedProperties, this, expression);
			tci.setTypeToExprRelationship(unit);
			return tci;
		}
		else {
			TypeCheckInfo tci = new TypeCheckInfo(uqcn, tctype, this, expression);
			tci.setTypeToExprRelationship(unit);
			return tci;
		}
	}
	
	protected void validateCommaSeparatedAbreviatedExpression(SubjHasProp expression) throws DontTypeCheckException, CircularDefinitionException, 
		InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		TypeCheckInfo proptct = getType(expression.getProp());
		TypeCheckInfo rghttct = getType(expression.getRight());
		if (!compareTypesUsingImpliedProperties(Arrays.asList("is"), expression.getProp(), expression.getRight(), proptct, rghttct, ImplicitPropertySide.NONE)) {
			StringBuilder errorMessageBuilder = new StringBuilder();
			createErrorMessage(errorMessageBuilder, proptct, rghttct, "property initialization", true, expression);
		}
	}

	protected Declaration subjHasPropIsDeclaration(SubjHasProp expression) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		if (expression.getLeft() instanceof Declaration) {
			TypeCheckInfo declType = getType(((Declaration)expression.getLeft()).getType());
			if (declType == null) {
//				throw new TranslationException("Declaration has no type");
				return null;	// this happens when a file is open in the editor and there is a clean/build
			}
			Node tct = declType.getTypeCheckType();
			if (tct instanceof NamedNode && ((NamedNode)tct).getNodeType() != null && ((NamedNode)tct).getNodeType().equals(NodeType.ClassNode)) {
				return (Declaration)expression.getLeft();
			}
		}
		if (expression.getLeft() instanceof SubjHasProp) {
			return subjHasPropIsDeclaration((SubjHasProp)expression.getLeft());
		}
		return null;
	}

	private TypeCheckInfo unifyCompoundTypeCheckInfos(TypeCheckInfo tci) throws CircularDependencyException, InvalidNameException, TranslationException, InvalidTypeException {
		// if this is a compound TypeCheckInfo, look to make sure that one isn't a subclass of another and eliminate lower classes
		if (tci != null && tci.getCompoundTypes() != null) {
			int size = tci.getCompoundTypes().size();
			if (size == 1) {
				return tci.getCompoundTypes().get(0);
			}
			List<TypeCheckInfo> considered = new ArrayList<TypeCheckInfo>();
			considered.add(tci.getCompoundTypes().get(0));
			List<TypeCheckInfo> toEliminate = null;
			int index = 1;
			for (int i = index; i < size; i++) {
				TypeCheckInfo newTci = tci.getCompoundTypes().get(i);
				RangeValueType newRvt = newTci.getRangeValueType();
				if (newRvt.equals(RangeValueType.CLASS_OR_DT)) {
					Node newTct = newTci.getTypeCheckType();
					if (newTct instanceof NamedNode) {
						OntClass newOr = theJenaModel.getOntClass(((NamedNode)newTct).toFullyQualifiedString());
						if (newOr != null) {
							for (int j = 0; j < considered.size(); j++) {
								if (newRvt.equals(considered.get(j).getRangeValueType())) {
									Node consideredTct = considered.get(j).getTypeCheckType();
									if (consideredTct instanceof NamedNode) {
										OntClass consideredOr = theJenaModel.getOntClass(((NamedNode)consideredTct).toFullyQualifiedString());
										if (consideredOr != null) {
											if (SadlUtils.classIsSubclassOf(newOr, consideredOr, true, null)) {
												if (toEliminate == null) toEliminate = new ArrayList<TypeCheckInfo>();
												toEliminate.add(newTci);
											}
											else if (SadlUtils.classIsSubclassOf(consideredOr, newOr, true, null)) {
												if (toEliminate == null) toEliminate = new ArrayList<TypeCheckInfo>();
												toEliminate.add(considered.get(j));
											}
										}
									}
									else {
										throw new TranslationException("Non-NamedNode type not handled yet");
									}
								}
							}
						}
					}
					else {
						throw new TranslationException("Non-NamedNode type not handled yet");
					}
				}
			}
			if (toEliminate != null) {
				for (int i = 0; i < toEliminate.size(); i++) {
					if (tci.getCompoundTypes().contains(toEliminate.get(i))) {		
						tci.getCompoundTypes().remove(toEliminate.get(i));
					}
				}
			}
			size = tci.getCompoundTypes().size();
			if (size == 1) {
				return tci.getCompoundTypes().get(0);
			}
		}
		return tci;
	}

	private boolean isNumericWithImpliedProperty(TypeCheckInfo tci, Expression expr) throws DontTypeCheckException, InvalidNameException, InvalidTypeException, TranslationException {
		if (tci.getImplicitProperties() != null) {
			Iterator<ConceptName> litr = tci.getImplicitProperties().iterator();
			while (litr.hasNext()) {
				ConceptName cn = litr.next();
				Property prop = theJenaModel.getProperty(cn.getUri());
				if (prop.canAs(ObjectProperty.class)) {
					cn.setType(ConceptType.OBJECTPROPERTY);
				}
				else if (prop.canAs(DatatypeProperty.class)) {
					cn.setType(ConceptType.DATATYPEPROPERTY);
				}
				else {
					cn.setType(ConceptType.RDFPROPERTY);
				}
				TypeCheckInfo newtci = getTypeInfoFromRange(cn, prop, expr);
				if (isNumeric(newtci)) {
					return true;
				}
			}
		}
		return false;
	}

	private boolean isNumeric(TypeCheckInfo tci) throws InvalidTypeException {
		Node ci;
		if (tci != null) {
			if (tci.getTypeCheckType() != null) {
				ci = tci.getTypeCheckType();
				if (ci instanceof NamedNode) {
					return getModelProcessor().isNumericType(((NamedNode)ci).toFullyQualifiedString());
				}
			}
			else if (tci.getExplicitValueType() != null) {
	//TODO this is incomplete; more examples needed AWC 12/19/2016				
				ExplicitValueType evt = tci.getExplicitValueType();
				if (evt.equals(ExplicitValueType.RESTRICTION)) {
					getModelProcessor().addWarning("Explicit value type is RESTRICTION, which isn't yet handled. Please report with use case.", tci.context);
				}
				else if (tci.getExpressionType() instanceof ConceptName){
					return getModelProcessor().isNumericType((ConceptName) tci.getExpressionType());
				}
			}
		}
		return false;
	}

	private boolean isUnittedQuantity(TypeCheckInfo tci) {
		if (tci != null) {
			Node tctn = tci.getTypeCheckType();
			if (tctn instanceof NamedNode && ((NamedNode)tctn).toFullyQualifiedString().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
				return true;
			}
		}
		return false;
	}

	private TypeCheckInfo convertListTypeToElementOfListType(TypeCheckInfo aListType) {
		TypeCheckInfo lElementType = new TypeCheckInfo(aListType.getExpressionType(), this, aListType.getContext());
		Node tctype = aListType.getTypeCheckType();
		if (tctype instanceof NamedNode) {
			try {
				NamedNode clonedNode = (NamedNode) tctype.clone();
				clonedNode.setList(false);
				lElementType.setTypeCheckType(clonedNode);
			} catch (CloneNotSupportedException e) {
				logger.error(e.getMessage());
			}
		}
		lElementType.setRangeValueType(RangeValueType.CLASS_OR_DT);
		
		if(aListType.getCompoundTypes() != null){
			// compound
			  Iterator<TypeCheckInfo> tci_iter = aListType.getCompoundTypes().iterator();
			  while(tci_iter.hasNext()){
				 lElementType.addCompoundType(convertListTypeToElementOfListType(tci_iter.next()));
			  }
		}
		
		return lElementType;
	}
	
	private TypeCheckInfo convertElementOfListToListType(TypeCheckInfo aElementType) {
		TypeCheckInfo lListType = new TypeCheckInfo(aElementType.getExpressionType(), this, aElementType.getContext());
		Node tctype = aElementType.getTypeCheckType();
		if (tctype instanceof NamedNode) {
			try {
				NamedNode clonedNode = (NamedNode) tctype.clone();
				clonedNode.setList(true);
				lListType.setTypeCheckType(clonedNode);
			} catch (CloneNotSupportedException e) {
				logger.error(e.getMessage());
			}
		}
		lListType.setRangeValueType(RangeValueType.LIST);
		
		if(aElementType.getCompoundTypes() != null){
			// compound
			  Iterator<TypeCheckInfo> tci_iter = aElementType.getCompoundTypes().iterator();
			  while(tci_iter.hasNext()){
				  lListType.addCompoundType(convertListTypeToElementOfListType(tci_iter.next()));
			  }
		}
		
		return lListType;
	}

	protected TypeCheckInfo getType(Constant expression) throws DontTypeCheckException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, CircularDefinitionException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		//What do we do about the rest of the constants?
		/*'--' | 'a'? 'type' ;*/
		String constant = expression.getConstant();	
		if(constant.equals(SadlConstants.CONSTANT_PI) || constant.equals(SadlConstants.CONSTANT_E)){
			NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.decimal.getURI(), NodeType.DataTypeNode));
			ConceptName constantConceptName = getModelProcessor().namedNodeToConceptName(tctype);
			return new TypeCheckInfo(constantConceptName, tctype, this, expression);
		}
		else if(constant.equals("length") || constant.equals("count") ||
				   constant.equals("index")){
					NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.xint.getURI(), NodeType.DataTypeNode));
					ConceptName constantConceptName = getModelProcessor().namedNodeToConceptName(tctype);
					return new TypeCheckInfo(constantConceptName, tctype, this, expression);
				}
		else if(constant.endsWith("element") && (constant.startsWith("first") || constant.startsWith("last"))){
			//Handle list types???
			ConceptName declarationConceptName = new ConceptName(constant);
			return new TypeCheckInfo(declarationConceptName, null, this, expression);
		}
//		else if (constant.endsWith("value")) {
//			throw new DontTypeCheckException();
//		}
		else if (expression instanceof Constant && (((Constant)expression).getConstant().equals("value")
				|| ((Constant)expression).getConstant().equals("type"))) {
			Sublist slexpr = getSublistContainer(expression);
			if (slexpr != null) {
				TypeCheckInfo matchTci = getType(slexpr.getList());
//				matchTci.setRangeValueType(RangeValueType.CLASS_OR_DT);
				return matchTci;
			}
			getModelProcessor().addTypeCheckingError("Unable to get sublist type", expression);
			return getType(expression);
		}
		else if (constant.equals("a type")) {
			NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(RDFS.subClassOf.getURI(), NodeType.ClassNode));
			ConceptName rdfType = getModelProcessor().namedNodeToConceptName(tctype);
			return new TypeCheckInfo(rdfType, tctype, this, expression);
		}
		else if(constant.equals(SadlConstants.CONSTANT_NONE)){
			ConstantNode tctype = new ConstantNode(SadlConstants.CONSTANT_NONE);
			ConceptName constantConceptName = new ConceptName(SadlConstants.CONSTANT_NONE);
			return new TypeCheckInfo(constantConceptName, tctype, this, expression);
		}
		else if (constant.equals("known")) {
			NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(constant, NodeType.InstanceNode));
			ConceptName constantConceptName = getModelProcessor().namedNodeToConceptName(tctype);
			return new TypeCheckInfo(constantConceptName, tctype, this, expression);
		}
		else {
			// let any subclass validators do their thing
			lastSuperCallExpression = expression;
			return getType((Constant)expression);
		}

	}

	private TypeCheckInfo getType(SadlConstantLiteral expression) throws TranslationException, InvalidNameException, InvalidTypeException {
		String term = expression.getTerm();
		Literal litval = null;
		if (term.equals(SadlConstants.CONSTANT_PI)) {
			litval = theJenaModel.createTypedLiteral(Math.PI);
		}
		else if (term.equals(SadlConstants.CONSTANT_E)) {
			litval = theJenaModel.createTypedLiteral(Math.E);
		}
		else {
			throw new TranslationException("Unhandled SadlConstantLiteral type: " + expression.getClass().getCanonicalName());
		}
		NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.decimal.getURI(), NodeType.DataTypeNode));
		ConceptName numberLiteralConceptName = getModelProcessor().namedNodeToConceptName(tctype);
		TypeCheckInfo litTci = new TypeCheckInfo(numberLiteralConceptName, litval, ExplicitValueType.VALUE, this, expression); 
		litTci.setTypeCheckType(tctype);
		return litTci;
	}

	private boolean isVariable(TypeCheckInfo tci) {
		if (tci == null) return false;
		Node ci = tci.getTypeCheckType();
		if (ci instanceof NamedNode && ((NamedNode)ci).getNodeType() != null && ((NamedNode)ci).getNodeType().equals(NodeType.VariableNode)) {
			return true;
		}
		return false;
	}

	private TypeCheckInfo getType(SadlTypeReference expression) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		if (expression instanceof SadlIntersectionType) {
			return getType((SadlIntersectionType)expression);
		}
		else if (expression instanceof SadlPrimitiveDataType) {
			return getType((SadlPrimitiveDataType)expression);			
		}
		else if (expression instanceof SadlPropertyCondition) {
			return getType((SadlPropertyCondition)expression);
		}
		else if (expression instanceof SadlSimpleTypeReference) {
			return getType((SadlSimpleTypeReference)expression);
		}
		else if (expression instanceof SadlUnionType) {
			return getType((SadlUnionType)expression);
		}
		getModelProcessor().addTypeCheckingError(SadlErrorMessages.TYPE_REFERENCE_ERROR.get(expression.getClass().getCanonicalName()), expression);
		if (metricsProcessor != null) {
			metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
		}
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, null, this, expression);
	}
	
	private TypeCheckInfo getType(SadlIntersectionType expression) {
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, null, this, expression);		
	}

	private TypeCheckInfo getType(SadlPrimitiveDataType expression) throws TranslationException, InvalidNameException, InvalidTypeException {
		TypeCheckInfo tci = getType(expression.getPrimitiveType());
		tci.setContext(this, expression);
		return tci;
	}

	private TypeCheckInfo getType(SadlDataType primitiveType) throws TranslationException, InvalidNameException, InvalidTypeException {
		String nm = primitiveType.getName();
		NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.getURI() + nm, NodeType.DataTypeNode));
		ConceptName cn = getModelProcessor().namedNodeToConceptName(tctype);
		return new TypeCheckInfo(cn, tctype, this, null);
	}

	private TypeCheckInfo getType(SadlPropertyCondition expression) {
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, null, this, expression);		
	}
	
	private TypeCheckInfo getType(SadlSimpleTypeReference expression) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		TypeCheckInfo tci = getType(expression.getType());
		if (expression.isList()) {
			tci.setRangeValueType(RangeValueType.LIST);
			int[] lenRest = getModelProcessor().getLengthRestrictions(expression.eContainer());
			Node tctype = tci.getTypeCheckType();
			if (lenRest != null && tctype instanceof NamedNode) {
				if (lenRest.length == 1) {
					((NamedNode)tctype).setListLength(lenRest[0]);
				}
				else if (lenRest.length == 2) {
					((NamedNode)tctype).setMinListLength(lenRest[0]);
					((NamedNode)tctype).setMaxListLength(lenRest[1]);
				}
			}
		}
		return tci;
	}

	private TypeCheckInfo getType(SadlUnionType expression) {
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, null, this, expression);		
	}

	private TypeCheckInfo getType(PropOfSubject expression) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		String ofOp = expression.getOf();
		Expression predicate = expression.getLeft();
		Expression subject = expression.getRight();
		boolean isNegated = false;
		if (predicate instanceof UnaryExpression && ((UnaryExpression)predicate).getOp().equals("not")) {
			predicate = ((UnaryExpression)predicate).getExpr();
			isNegated = true;
		}
		
		if (predicate instanceof Constant) {
			String cnstval = ((Constant)predicate).getConstant();
			TypeCheckInfo subjtype = null;
			if (constantRequiresListNext(cnstval)) {
				subjtype = getType(subject);
				if (subjtype != null && !subjtype.getRangeValueType().equals(RangeValueType.LIST)) {
					getModelProcessor().addTypeCheckingError(SadlErrorMessages.MUST_BE_APPLIED_TO_LIST.get(cnstval, getTypeCheckTypeString(subjtype)), subject);
				}
			}
			else if (constantFollowedByIntThenList(cnstval)) {
				subjtype = getType(subject);
				
			}
			else if (constantFollowedByElementThenList(cnstval)) {
                Expression element = null;
                Expression list = null;
                if(subject instanceof PropOfSubject) {
                    element = ((PropOfSubject)subject).getLeft();
                    list = ((PropOfSubject)subject).getRight();            
                }
                
                // Special processing for the parse tree since it is abnormal in this case
                // ie. count/index of input16a of REQ16 in outputList16 of REQ16 = input16b of REQ16.
                if(element != null && list != null) {
                    if (element instanceof Name && list instanceof PropOfSubject) {
                        Expression innerSubject = ((PropOfSubject)list).getRight();    
                        if(innerSubject != null) {
                            subject = innerSubject;
                        }
                    }
                }
                
                subjtype = getType(subject);
			}
			if (cnstval.endsWith("length") || cnstval.equals("count") || cnstval.endsWith("index")) {
				NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.xint.getURI(), NodeType.DataTypeNode));
				ConceptName nlcn = getModelProcessor().namedNodeToConceptName(tctype);
				
				TypeCheckInfo subjType = null;
				if(subject instanceof PropOfSubject) {
					subjType = getType(((PropOfSubject) subject).getLeft());
				}else {
					//subjType = getType(subject);
				}
				//check for implied property 
				if (useImpliedPropertyToMatchNumericOperator(subject, subjType, "right side of '" + ofOp + "'")) {
					//should we be matching a NumbericOperator here?
				}
				
				
				return new TypeCheckInfo(nlcn, tctype, this, expression);
			}
			else if (subjtype != null && (cnstval.endsWith(" element"))) {
				if (subjtype != null && (cnstval.equals("first element") || cnstval.equals("last element")) ) {
					if (subjtype.getTypeCheckType() instanceof NamedNode) {
						if (((NamedNode)subjtype.getTypeCheckType()).getNodeType().equals(NodeType.ClassListNode)) {
							NamedNode lTcType = getModelProcessor().validateNamedNode(new NamedNode(((NamedNode)subjtype.getTypeCheckType()).toFullyQualifiedString(), NodeType.ClassNode));
							ConceptName lCN = getModelProcessor().namedNodeToConceptName(lTcType);
							TypeCheckInfo lTCI = new TypeCheckInfo(lCN, lTcType, this, expression);
							if(subjtype.getImplicitProperties() != null) {
								lTCI.addImplicitProperties(subjtype.getImplicitProperties());
							}
							lTCI.setRangeValueType(RangeValueType.CLASS_OR_DT);
							return lTCI;
						}
						else if (((NamedNode)subjtype.getTypeCheckType()).getNodeType().equals(NodeType.DataTypeListNode)) {
							NamedNode lTcType = getModelProcessor().validateNamedNode(new NamedNode(((NamedNode)subjtype.getTypeCheckType()).toFullyQualifiedString(), NodeType.DataTypeNode));
							ConceptName lCN = getModelProcessor().namedNodeToConceptName(lTcType);
							TypeCheckInfo lTCI = new TypeCheckInfo(lCN, lTcType, this, expression);
							lTCI.setRangeValueType(RangeValueType.CLASS_OR_DT);
							return lTCI;
						}
						else {
							getModelProcessor().addTypeCheckingError("Expected a list for list element function", expression);
						}
					}
					else {
						throw new TranslationException("unhandled element of list type, type check type not a NamedNode");
					}
					return subjtype;
				}
				String article = cnstval.substring(0, cnstval.indexOf(" "));
				subjtype.setRangeValueType(RangeValueType.CLASS_OR_DT);   	// keep type but change from List to reflect this is an element of the list
				if (subjtype.getTypeCheckType() instanceof NamedNode) {
					if (((NamedNode)subjtype.getTypeCheckType()).getNodeType().equals(NodeType.ClassListNode)) {
						((NamedNode)subjtype.getTypeCheckType()).setNodeType(NodeType.ClassNode);
					}
					else {
						throw new TranslationException("unhandled element of list type check type, NamedNode but not ClassListNode");
					}
				}
				else {
					throw new TranslationException("unhandled element of list type, type check type not a NamedNode");
				}
				return subjtype;
			
			}
			else if (cnstval.equals("a type")) {
				subjtype = getType(subject);
				NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(RDFS.subClassOf.getURI(), NodeType.ClassNode));
				ConceptName rdfType = getModelProcessor().namedNodeToConceptName(tctype);
				subjtype.setExpressionType(rdfType);
//				return new TypeCheckInfo(rdfType, tctype, this, expression);
				return subjtype;
			}
			else {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.UNHANDLED.get("Constant Property", cnstval), expression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
			}
		}
		else if (predicate instanceof ElementInList) {
			// this is like the constant "element"
			TypeCheckInfo subjtype = getType(subject);
			TypeCheckInfo predtype = null;
			if(((ElementInList) predicate).getElement() instanceof PropOfSubject) {
				predtype = getType(((ElementInList) predicate).getElement());
			}else {
				predtype = getType(predicate);
			}
			
			//check for implied property 
			if (useImpliedPropertyToMatchNumericOperator(predicate, predtype, "left side of '" + ofOp + "'")) {
				//should we be matching a NumbericOperator here?
			}
			
			TypeCheckInfo lElementType = convertListTypeToElementOfListType(subjtype);
			lElementType.setAdditionalInformation("element of");
			if(subjtype.getImplicitProperties() != null && subjtype.getImplicitProperties().size() != 0){
				lElementType.addImplicitProperties(subjtype.getImplicitProperties());
			}
			return lElementType;
		}
		boolean isElementInListConstruct = false;
		if (predicate instanceof Name) {
			try {
				OntConceptType predtype = declarationExtensions.getOntConceptType(((Name)predicate).getName());
				if (ofOp != null && ofOp.equals("in")) {
					// this is a list construct: element in list
					isElementInListConstruct = true;
					return null;
				}
				else if (!predtype.equals(OntConceptType.CLASS_PROPERTY) && !predtype.equals(OntConceptType.DATATYPE_PROPERTY) && 
						!predtype.equals(OntConceptType.RDF_PROPERTY) && !predtype.equals(OntConceptType.ANNOTATION_PROPERTY)) {
//					String preduri = declarationExtensions.getConceptUri(((Name)predicate).getName());
					getModelProcessor().addTypeCheckingError(SadlErrorMessages.EXPECTED_A.get("property in property chain"), predicate);
				}
			} catch (CircularDefinitionException e) {
				e.printStackTrace();
			}
		}
		boolean validSubject = true;
		if (subject instanceof Name || subject instanceof Declaration) {
			// check for applicable local restriction first
			TypeCheckInfo predicateType = null;
			try {
				List<Node> lrs = getApplicableLocalRestriction(subject, predicate);
				if (lrs != null) {
					if (predicate instanceof Name) {
						String propuri = declarationExtensions.getConceptUri(((Name)predicate).getName());
						ConceptName pcn = new ConceptName(propuri);
						pcn.setType(getModelProcessor().nodeTypeToConceptType(getModelProcessor().ontConceptTypeToNodeType(declarationExtensions.getOntConceptType(((Name)predicate).getName()))));
						TypeCheckInfo lastTci = null;
						for (int i = 0; i < lrs.size(); i++) {
							TypeCheckInfo lrtci = new TypeCheckInfo(pcn, lrs.get(i), this, expression);
							lrtci.setTypeToExprRelationship(RESTRICTED_TO);
							if (lastTci != null) {
								predicateType = new TypeCheckInfo(pcn, this, expression);
								predicateType.addCompoundType(lastTci);
							}
							lastTci = lrtci;
						}
						if (predicateType != null) {
							predicateType.addCompoundType(lastTci);
						}
						else {
							predicateType = lastTci;
						}
					}

					if(predicateType != null) {
						if (predicateType.getTypeCheckType() != null){
							addEffectiveRange(predicateType, subject);
						}
						return predicateType;
					}
				}
			} catch (PrefixNotFoundException e) {
				e.printStackTrace();
			} catch (InvalidTypeException e) {
				e.printStackTrace();
			}
			// check for AllValuesFrom restriction before defaulting to checking property range
			predicateType = getTypeFromRestriction(subject, predicate);
			if (predicateType != null) {
				if(predicateType.getTypeCheckType() != null){
					addEffectiveRange(predicateType, subject);
				}
				return predicateType;
			}
		}
		else {
//			getModelProcessor().addIssueToAcceptor("Property of subject has unexpected subject type '" + subject.getClass().getCanonicalName() + "'", expression);
			// we don't care about any other types?
			validSubject = false;
		}
		if (predicate != null) {
			TypeCheckInfo predicateType = getType(predicate);
			if (subject instanceof PropOfSubject && predicateType.getExpressionType() instanceof ConceptName) {
				predicateType = checkEmbeddedPropOfSubject(subject, predicate, predicateType);
				getType(subject);
			} else if (validSubject && predicateType != null) {

				if (subject instanceof Name && predicate instanceof Name) {
					Property p = theJenaModel.getProperty(declarationExtensions.getConceptUri((Name) predicate));
					OntResource s = null;
					s = theJenaModel.getOntResource(declarationExtensions.getConceptUri((Name) subject));

					if (s != null && p != null) {
						checkPropertyDomain(theJenaModel, s, p, expression, true, null);
					}

				}

				// add interface range

				if (predicateType.getTypeCheckType() != null) {
					addEffectiveRange(predicateType, subject);
				}
			}
			else if (subject instanceof SubjHasProp && SadlASTUtils.isUnitExpression(subject)) {
				getModelProcessor().addWarning("Units are associated with the subject of this expression; should the expression be in parentheses?", subject);
			}
			return predicateType;
		}
		return null;
	}
	
	protected String generateLocalRestrictionKey(Object transobj) {
		String key = transobj instanceof NamedNode ? ((NamedNode)transobj).toFullyQualifiedString() : transobj.toString();
		return key;
	}

	private void addEffectiveRange(TypeCheckInfo predicateType, Expression subject) throws CircularDefinitionException, InvalidTypeException, CircularDependencyException{
		if(metricsProcessor != null){
			try {
				if (subject instanceof Name) {
					String className = declarationExtensions.getConceptUri(((Name) subject).getName());
					SadlResource cls = ((Name) subject).getName();
					if (!declarationExtensions.getOntConceptType(cls).equals(OntConceptType.CLASS)) {
						// need to convert this to the Class representing the type; use existing type checking functionality
						TypeCheckInfo subjTCI = getType(cls);
						if (subjTCI != null /* && !subjTCI.getTypeCheckType().toString().equals("TODO")*/) {
							addEffectiveRangeByTypeCheckInfo(predicateType, subjTCI);
						}
					}
					else {
	//					cls = ((Name)subject).getName();
						addEffectiveRangeUnit(className, predicateType);
					}
				}
				else if (subject instanceof Declaration) {
					SadlTypeReference str = ((Declaration)subject).getType();
					if (str instanceof SadlSimpleTypeReference) {
						String className = declarationExtensions.getConceptUri(((SadlSimpleTypeReference)str).getType());
						addEffectiveRangeUnit(className, predicateType);
					}
				}
				else if (subject instanceof ElementInList) {
					TypeCheckInfo tci = getType(((ElementInList)subject));
					addEffectiveRangeByTypeCheckInfo(predicateType, tci);
				}
				else if (subject instanceof SubjHasProp) {
					TypeCheckInfo tci;
					tci = getType(((SubjHasProp)subject).getLeft());
					addEffectiveRangeByTypeCheckInfo(predicateType, tci);
				}
				else {
					throw new InvalidNameException("addEffectiveRange given a subject of type '" + subject.getClass().getCanonicalName() + "', not yet handled.");
				}
			} catch (TranslationException e) {
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

			} catch (InvalidNameException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (PropertyWithoutRangeException e) {

			} 
		}
	}

	private void addEffectiveRangeByTypeCheckInfo(TypeCheckInfo predicateType, TypeCheckInfo subjTCI)
			throws InvalidNameException, CircularDefinitionException, InvalidTypeException {
		if (subjTCI.getCompoundTypes() != null) {
			Iterator<TypeCheckInfo> itr = subjTCI.getCompoundTypes().iterator();
			while (itr.hasNext()) {
				TypeCheckInfo nexttci = itr.next();
				addEffectiveRangeByTypeCheckInfo(predicateType, nexttci);
			}
		}
		else {
			Node ci = subjTCI.getTypeCheckType();
			if (ci instanceof NamedNode) {
				// this should be the class name
				String className = ((NamedNode) ci).toFullyQualifiedString();
				addEffectiveRangeUnit(className, predicateType);
			}
			else {
				throw new InvalidNameException("addEffectiveRangeByTypeCheckInfo called with TypeCheckInfo '" + subjTCI.toString() + ", which isn't handled.");
			}
		}
	}

	private void addEffectiveRangeUnit(String className, TypeCheckInfo predicateType) throws InvalidTypeException {
		String propertyName = predicateType.getExpressionType().toString();
		String rangeStr = predicateType.getTypeCheckType().toString();
		boolean isList = predicateType.getRangeValueType().equals(RangeValueType.LIST);
		if (metricsProcessor != null) {
			metricsProcessor.addControlledOrMonitoredProperty(null, propertyName);
			metricsProcessor.addEffectiveRangeAndDomain(null, className, propertyName, rangeStr, isList);
		}

	}
	
	private String getTypeCheckTypeString(TypeCheckInfo tci) {
		if (tci != null) {
			if (tci.typeCheckType != null) {
				return tci.typeCheckType.toString();
			}
			if (tci.getCompoundTypes() != null) {
				StringBuilder sb = new StringBuilder();
				Iterator<TypeCheckInfo> itr = tci.getCompoundTypes().iterator();
				int cntr = 0;
				while (itr.hasNext()) {
					if (cntr > 0) sb.append(" or ");
					sb.append(getTypeCheckTypeString(itr.next()));
					cntr++;
				}
				return sb.toString();
			}
		}
		return "(null)";
	}

	protected List<Node> getApplicableLocalRestriction(Expression subject, Expression predicate) throws IOException, PrefixNotFoundException, InvalidNameException, InvalidTypeException, TranslationException, ConfigurationException, CircularDefinitionException {
		return null;
	}

	private boolean constantFollowedByElementThenList(String cnstval) {
		if (cnstval.equals("index") ||
				cnstval.equals("count")) {
			return true;
		}
		return false;
	}
	
	private boolean constantFollowedByIntThenList(String cnstval) {
		if (cnstval.equals("element")) {
			return true;
		}
		return false;
	}

	private boolean constantRequiresListNext(String cnstval) {
		if (cnstval.equals("length") ||
				cnstval.endsWith(" element") ||
				cnstval.equals("first element") ||
				cnstval.equals("last element")) {
			return true;
		}
		return false;
	}

	private TypeCheckInfo checkEmbeddedPropOfSubject(Expression subject, Expression predicate, TypeCheckInfo predicateType) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		if (predicate instanceof Name) {
			String propuri = declarationExtensions.getConceptUri(((Name)predicate).getName());
			OntConceptType oct = declarationExtensions.getOntConceptType(((Name)predicate).getName());
			if (oct.equals(OntConceptType.ANNOTATION_PROPERTY)) {
				getModelProcessor().addWarning(SadlErrorMessages.DOMAIN_MATCHING.get("annotation property"), predicate);
				return predicateType;
			}
			Property prop = theJenaModel.getProperty(propuri);
			boolean firstprop = false;
			if (modelProcessor.isLookingForFirstProperty()) {
				firstprop = true;
			}
			modelProcessor.setLookingForFirstProperty(false);
			TypeCheckInfo subjType = getType(subject);
			if (firstprop) {
				modelProcessor.setLookingForFirstProperty(true);
			}
		
			List<OntClass> subjClasses = subjType != null ? getTypeCheckTypeClasses(subjType) : null;
			try {
				List<Node> lrs = getApplicableLocalRestriction(subject, predicate);
				if (lrs != null) {
					for (int i = 0; i < lrs.size(); i++) {
						if (lrs.size() > 1) {
							throw new TranslationException("More than one matching local restriction found; this shouldn't happen");
						}
						if (!(lrs.get(0) instanceof NamedNode)) {
							throw new TranslationException("Local restriction isn't instance of NamedNode; this shouldn't happen");
						}
						predicateType.setTypeCheckType((NamedNode)lrs.get(0));
						predicateType.setTypeToExprRelationship(RESTRICTED_TO);
						if (predicateType.getTypeCheckType() != null){
							addEffectiveRangeByTypeCheckInfo(predicateType, subjType);
						}
						return predicateType;
					}
				}
				
				else {
					if (subjType != null && predicate != null && predicateType.getTypeCheckType() != null) {
						addEffectiveRangeByTypeCheckInfo(predicateType, subjType);
					}

				}
			} catch (PrefixNotFoundException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			StmtIterator domainItr = prop.listProperties(RDFS.domain);
			boolean domainMatched = false;
			List<Resource> domainList = new ArrayList<Resource>();
			while (domainItr.hasNext() && !domainMatched) {
				RDFNode dmn = domainItr.next().getObject();
				if (dmn instanceof Resource) {
					if (checkDomain(subjClasses, dmn.asResource(), domainList)) {
						domainItr.close();
						domainMatched = true;
						break;
					}
				}
			}
			boolean possibleMatch = false;
			if (!domainMatched) {
				// there was no direct match
				for (int i = 0; domainList != null && i < domainList.size() && !domainMatched; i++) {
					Resource dmn = domainList.get(i);
					if ((dmn instanceof OntResource || dmn.canAs(OntResource.class)) && subjType != null && subjType.getTypeCheckType() != null) {
						try {
							for (int j = 0; subjClasses != null && j < subjClasses.size(); j++) {
								OntClass subj = subjClasses.get(j);
								if (dmn.canAs(OntClass.class)) { 
									/*
									 * This handles the case of a property chain, e.g., p1 of p2, where p1 has domain D1 and p2 has range R2. 
									 * Normally this would pass type checking if R2 is a subclass of D1, proper or improper. In other words,
									 *   we want whatever can be returned as the value of p2 (R2) to be within the domain of p1. This is the first test.
									 * Note that the domainList is D1, subjClasses is the range R2, and may be a union with elements ranged over by subj
									 * 
									 * However, sometimes the range R2 isn't really a range but rather a local restriction, and in this case D1 may not be a subclass of R2,
									 *    but it will then be the case that 
									 */
									if (SadlUtils.classIsSubclassOf(subj, dmn.as(OntClass.class), true, null)) {
										domainMatched = true;
										break;
									}
									else if (subjType.getTypeToExprRelationship() != null && subjType.getTypeToExprRelationship().equals(RESTRICTED_TO)) {
//										if (SadlUtils.classIsSubclassOf(subj, dmn.as(OntClass.class), true, null)) {
										if (SadlUtils.classIsSubclassOf(dmn.as(OntClass.class), subj, true, null)) {
											domainMatched = true;
											break;
										}
									}
									else if (SadlUtils.classIsSubclassOf(dmn.as(OntClass.class), subj, true, null)) {
										// this could match but isn't guaranteed to do so; generate a warning
										possibleMatch = true;
									}
								}
							}
						} catch (CircularDependencyException e) {
							e.printStackTrace();
						}
					}
				}
			}
			if (!domainMatched && domainList != null) {
				// there wasn't any subclass match either so create an appropriate error message
				try {
					oct = declarationExtensions.getOntConceptType(((Name)predicate).getName());
					StringBuilder errorMessageBuilder = new StringBuilder();
					TypeCheckInfo leftTypeCheckInfo;
					if (domainList.size() == 1) {
						ConceptName cn1 = createTypedConceptName(propuri, oct);
						NamedNode cn2 = null;
						if (domainList.get(0).isURIResource()) {
							cn2 = getModelProcessor().validateNamedNode(new NamedNode(domainList.get(0).getURI(), NodeType.ClassNode));
							leftTypeCheckInfo = new TypeCheckInfo(createTypedConceptName(propuri, oct), cn2, this, predicate);
						}
						else {						
							leftTypeCheckInfo = createTypeCheckInfoForNonUriPropertyRange(domainList.get(0), cn1, predicate, cn1.getType());
						}
						leftTypeCheckInfo.setTypeToExprRelationship("domain");
					}
					else {
						leftTypeCheckInfo = new TypeCheckInfo(createTypedConceptName(propuri, oct), this, predicate);
						for (int i = 0; i < domainList.size(); i++) {
							Resource dmn = domainList.get(i);
							if (dmn.isURIResource()) {
								TypeCheckInfo tci = new TypeCheckInfo(createTypedConceptName(propuri, oct), getModelProcessor().validateNamedNode(new NamedNode(domainList.get(i).getURI(), NodeType.ClassNode)), this, predicate);
								leftTypeCheckInfo.addCompoundType(tci);
								leftTypeCheckInfo.setTypeToExprRelationship("domain");
							}
							else {
								ConceptName cn = createTypedConceptName(propuri, oct);
								TypeCheckInfo tci = createTypeCheckInfoForNonUriPropertyRange(dmn, cn, predicate, cn.getType());
								leftTypeCheckInfo.addCompoundType(tci);
								leftTypeCheckInfo.setTypeToExprRelationship("domain");
							}
						}
					}
					if (possibleMatch) {
						String warningMessage = createSuperClassOnRightWarning(predicate, subjType,
								leftTypeCheckInfo, "chained property");
						getModelProcessor().addWarning(warningMessage, predicate);
					}
					else {
						createErrorMessage(errorMessageBuilder, leftTypeCheckInfo, subjType, "chained property", true, predicate);
					}
				} catch (CircularDefinitionException e) {
					e.printStackTrace();
				}
			}
		}
		return predicateType;
	}

	/**
	 * Method to generate a warning message (rather than an error message) when the assignment value on the right side is
	 * a superclass of what is expected by the left side.
	 * @param expr
	 * @param rightTypeCheckInfo
	 * @param leftTypeCheckInfo
	 * @param operation
	 * @return
	 * @throws InvalidTypeException
	 */
	private String createSuperClassOnRightWarning(EObject expr, TypeCheckInfo rightTypeCheckInfo,
			TypeCheckInfo leftTypeCheckInfo, String operation) throws InvalidTypeException {
		StringBuilder warningMessageBuilder = new StringBuilder();
		createErrorMessage(warningMessageBuilder, leftTypeCheckInfo, rightTypeCheckInfo, operation, false, expr);
		int len = 0;
		int idx = warningMessageBuilder.indexOf("cannot operate");
		if (idx > 0) {
			len = 14;
		}
		else {
			idx = warningMessageBuilder.indexOf("cannot be compared");
			if (idx > 0) {
				len = 18;
			}
			int idx2 = warningMessageBuilder.indexOf("is", idx);
			if (idx2 > 0) {
				len = 3 + idx2 - idx;
			}
		}
		if (idx > 0) {
			warningMessageBuilder.replace(idx, idx + len, "may, but is not guaranteed to (because it is broader), operate");
		}
		return warningMessageBuilder.toString();
	}
	
	protected boolean checkDomain(List<OntClass> subjClasses, Resource dmn, List<Resource> domainList) {
		boolean domainMatched = false;
		if (dmn.isURIResource()) {
			for (int i = 0; subjClasses != null && i < subjClasses.size(); i++) {
				if (subjClasses.get(i).getURI().equals(((Resource) dmn).getURI())) {
					domainMatched = true;		// this is a direct match
					break;
				}
			}
			if (!domainMatched) {
				domainList.add((Resource) dmn);
			}
		}
		else if (dmn.asResource().canAs(OntClass.class)) {
			if (dmn.asResource().as(OntClass.class).isUnionClass()) {
				ExtendedIterator<? extends OntClass> ucitr = dmn.asResource().as(OntClass.class).asUnionClass().listOperands();
				while (ucitr.hasNext() && !domainMatched) {
					OntClass uccls = ucitr.next();
					if (uccls.isURIResource()) {
						for (int i = 0; subjClasses != null && i < subjClasses.size(); i++) {
							if (subjClasses.get(i).getURI().equals(uccls.getURI())) {
								ucitr.close();
								domainMatched = true;		// this is a direct match
								break;
							}
						}
						if (!domainMatched) {
							domainList.add(uccls);
						}
					}
					else {
						if (checkDomain(subjClasses, uccls, domainList)) {
							ucitr.close();
							domainMatched = true;
						}
					}
				}
			}
		}
		return domainMatched;
	}
	
	protected List<OntClass> getTypeCheckTypeClasses(TypeCheckInfo tci) {
		List<OntClass> results = null;
		if (tci.compoundTypes != null) {
			for (int i = 0; i < tci.compoundTypes.size(); i++) {
				TypeCheckInfo subtci = tci.compoundTypes.get(i);
				List<OntClass> subresults = getTypeCheckTypeClasses(subtci);
				if (results == null) {
					results = subresults;
				}
				else {
					results.addAll(subresults);
				}
			}
		}
		else {
			if (tci.getTypeCheckType() != null && tci.getTypeCheckType().toFullyQualifiedString() != null) {
				OntClass result = theJenaModel.getOntClass(tci.getTypeCheckType().toFullyQualifiedString());
				if (result != null) {
					results = new ArrayList<OntClass>();
					results.add(result);
				}
			}
		}
		return results;
	}

	protected TypeCheckInfo getTypeFromRestriction(Expression subject, Expression predicate) throws CircularDefinitionException, InvalidTypeException, DontTypeCheckException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, CircularDependencyException, PropertyWithoutRangeException {
		if (subject instanceof Name && predicate instanceof Name) {
			String subjuri = declarationExtensions.getConceptUri(((Name)subject).getName());
			OntConceptType subjtype = declarationExtensions.getOntConceptType(((Name)subject).getName());
			if (subjtype.equals(OntConceptType.VARIABLE)) {
				TypeCheckInfo varTci = getType(((Name)subject).getName());
				if (varTci != null && varTci.getTypeCheckType() != null) {
					Node varci = varTci.getTypeCheckType();
					if (varci instanceof NamedNode) {
						if (((NamedNode) varci).toString().equals("TODO")) {
							return null;
						}
						if (((NamedNode) varci).getName() == null || ((NamedNode) varci).getNamespace() == null) {
							return null;
						}
						subjuri = ((NamedNode)varci).toFullyQualifiedString();
					}
				}
			}
			String propuri = declarationExtensions.getConceptUri(((Name)predicate).getName());
			if (propuri == null) {
				getModelProcessor().addTypeCheckingError("Predicate name could not be resolved", predicate);
			}
			else {
				OntConceptType proptype = declarationExtensions.getOntConceptType(((Name)predicate).getName());
				return getTypeFromRestriction(subjuri, propuri, proptype, predicate);
			}
		}
		return null;
	}
	
	protected TypeCheckInfo getTypeFromRestriction(String subjuri, String propuri, OntConceptType proptype, Expression predicate) throws InvalidTypeException, TranslationException, InvalidNameException {
		Resource subj = theJenaModel.getResource(subjuri);
		if (subj != null) {
			if (!(subj instanceof OntClass || subj.canAs(OntClass.class)) && subj.canAs(Individual.class)) {
				ExtendedIterator<Resource> subjects = subj.as(Individual.class).listRDFTypes(true);
				while(subjects.hasNext()) {
					TypeCheckInfo type = getTypeFromRestriction(subjects.next(), propuri, proptype, predicate);
					if(type != null) {
						return type;
					}
				}
			}else {
				return getTypeFromRestriction(subj, propuri, proptype, predicate);
			}
		}
		return null;
	}
	
	public TypeCheckInfo getTypeFromRestriction(Resource subj, String propuri, OntConceptType proptype, Expression predicate) throws InvalidTypeException, TranslationException, InvalidNameException {
		if (subj != null && subj.canAs(OntClass.class)){ 
			Property prop = theJenaModel.getProperty(propuri);
			// look for restrictions on "range"
			StmtIterator sitr = theJenaModel.listStatements(null, OWL.onProperty, prop);
			while (sitr.hasNext()) {
				Statement stmt = sitr.nextStatement();
				Resource sr = stmt.getSubject();
				if (sr.canAs(OntClass.class) && subj.as(OntClass.class).hasSuperClass(sr.as(OntClass.class))) {
					if (sr.as(OntClass.class).asRestriction().isAllValuesFromRestriction()) {
						Resource avf = sr.as(OntClass.class).asRestriction().asAllValuesFromRestriction().getAllValuesFrom();
						if (avf.isLiteral()) {
							NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(avf.getURI(), NodeType.ClassNode));
							TypeCheckInfo avftci =  new TypeCheckInfo(createTypedConceptName(propuri, proptype), tctype, this, predicate);
							avftci.setTypeToExprRelationship(RESTRICTED_TO);
							return avftci;
						}else if (avf.isURIResource()){
							List<ConceptName> impliedProperties = getImpliedProperties(avf);
							NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(avf.getURI(), NodeType.ClassNode));
							TypeCheckInfo avftci = new TypeCheckInfo(createTypedConceptName(propuri, proptype), tctype, impliedProperties, this, predicate);
							avftci.setTypeToExprRelationship(RESTRICTED_TO);
							if (isTypedListSubclass(avf)) {
								avftci.setTypeCheckType(getTypedListType(avf));
								avftci.setRangeValueType(RangeValueType.LIST);
							}
							return avftci;
						}else if(isTypedListSubclass(avf)) {
							ConceptName cn = createTypedConceptName(propuri, proptype);
							TypeCheckInfo avftci = getSadlTypedListTypeCheckInfo(avf.as(OntClass.class), cn, predicate, cn.getType());
							avftci.setTypeToExprRelationship(RESTRICTED_TO);
							return avftci;
						}
					}
					else if (sr.as(OntClass.class).asRestriction().isHasValueRestriction()) {
						RDFNode hvr = sr.as(OntClass.class).asRestriction().asHasValueRestriction().getHasValue();
						TypeCheckInfo hvtci = new TypeCheckInfo(createTypedConceptName(propuri, proptype), 
							hvr, ExplicitValueType.RESTRICTION, this, predicate);
						if (isTypedListSubclass(hvr)) {
							hvtci.setTypeCheckType(getTypedListType(hvr));
							hvtci.setRangeValueType(RangeValueType.LIST);
						}
						return hvtci;
					}
				}
			}
		}
		
		StmtIterator itr2 = theJenaModel.listStatements(subj, RDFS.subClassOf, (RDFNode)null);
		while(itr2.hasNext()) {
			Statement stmt = itr2.next();
			RDFNode rdfNode= stmt.getObject();
			if(rdfNode.canAs(OntClass.class)) {
				TypeCheckInfo tci = getTypeFromRestriction(rdfNode.asResource(), propuri, proptype, predicate);
				if(tci != null) {
					return tci;
				}
			}
		}
		
		return null;
	}
	
	private NamedNode getTypedListType(RDFNode node) throws TranslationException {
		return modelProcessor.getTypedListType(node);
	}

	private boolean isTypedListSubclass(RDFNode node) {
		return modelProcessor.isTypedListSubclass(node);
	}

	private TypeCheckInfo getType(Name expression) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		SadlResource qnm =expression.getName();
		if (qnm.eIsProxy()) {
			handleUndefinedFunctions(expression);
		}
		
		//If the expression is a function, find equation definition from name and get the return type
		if(expression.isFunction()){
			try {
				TypeCheckInfo ftci = getFunctionType(qnm);
				if (qnm.eContainer() instanceof ExternalEquationStatement) {
					EList<Expression> args = expression.getArglist();
					EList<SadlParameterDeclaration> params = ((ExternalEquationStatementImpl)qnm.eContainer()).getParameter();
					checkFunctionArguments(params, args, expression);
				}
				else if (qnm.eContainer() instanceof EquationStatement) {
					EList<Expression> args = expression.getArglist();
					EList<SadlParameterDeclaration> params = ((EquationStatement)qnm.eContainer()).getParameter();
					checkFunctionArguments(params, args, expression);
				}
				if (ftci != null) {
					return ftci;
				}
			}
			catch (DontTypeCheckException e) {
				getModelProcessor().addWarning("External equation declaration does not provide type information; can't type check.", expression);
				throw e;
			}
			handleUndefinedFunctions(expression);
		}
		return getType(qnm, expression);
	}

	private void checkFunctionArguments(EList<SadlParameterDeclaration> params, EList<Expression> args, Name expression)
			throws InvalidTypeException, TranslationException {
		boolean variableNumArgs = false;
		int minNumArgs = 0;
		if (args.size() != params.size() || (params.size() > 1 && params.get(params.size() - 1).getEllipsis() != null)) {
			boolean wrongNumArgs = true;
			if (params.get(params.size() - 1).getEllipsis() != null) {
				minNumArgs = params.size() - 1;
				variableNumArgs = true;
				if (args.size() >= minNumArgs) {
					wrongNumArgs = false;
				}
			}
			if (wrongNumArgs) {
				getModelProcessor().addTypeCheckingError("Number of arguments does not match function declaration", expression);
			}
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < args.size(); i++) {
			Expression arg = args.get(i);
			SadlParameterDeclaration param = null;
			if (variableNumArgs) {
				param = (i >= minNumArgs) ? params.get(minNumArgs - 1) : params.get(i);
			}
			else if (i < params.size()) {
				param = params.get(i);
			}
			if (param != null) {
				validateBinaryOperationByParts(expression, arg, param.getType(), "argument", sb, false);
				if (sb.length() > 0) {
					getModelProcessor().addTypeCheckingError(sb.toString(), expression);
				}
			}
		}
	}
	
	private TypeCheckInfo getFunctionType(SadlResource fsr) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		if(fsr.eContainer() instanceof EquationStatement){
			EquationStatement es = (EquationStatement) fsr.eContainer();
			if(es != null){
				return getType(es.getReturnType());
			}
		}else if(fsr.eContainer() instanceof ExternalEquationStatement){
			ExternalEquationStatement ees = (ExternalEquationStatement)fsr.eContainer();
			if(ees != null) {
				if (ees.getUnknown() == null){
					return getType(ees.getReturnType());
				}
				else {
					throw new DontTypeCheckException();
				}
			}
		}
		return null;
	}
	
	private boolean isInQuery(EObject expression) {
		EObject cntr = expression.eContainer();
		if (cntr != null) {
			if (cntr instanceof QueryStatement) {
				return true;
			}
			return isInQuery(cntr);
		}
		return false;
	}

	protected void handleUndefinedFunctions(Name expression) throws ConfigurationException, DontTypeCheckException, InvalidTypeException{
		String expressionName = declarationExtensions.getConcreteName(expression);
		ITranslator translator = getModelProcessor().getTranslator();
		//If only names for built-in functions are available, check the name matches a built-in functions. If not, error.
		if (translator == null) {
			getModelProcessor().addWarning(SadlErrorMessages.TYPE_CHECK_TRANSLATOR_CLASS_NOT_FOUND.get(getModelProcessor().getConfigMgr().getTranslatorClassName()), expression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.WARNING_MARKER_URI, MetricsProcessor.UNDEFINED_FUNCTION_URI);
			}
		}
		else if(translator.isBuiltinFunctionTypeCheckingAvailable() == SadlConstants.SADL_BUILTIN_FUNCTIONS_TYPE_CHECKING_AVAILABILITY.NAME_ONLY){	
			if(translator.isBuiltinFunction(expressionName)){
				getModelProcessor().addWarning(SadlErrorMessages.TYPE_CHECK_BUILTIN_EXCEPTION.get(expressionName), expression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.WARNING_MARKER_URI, MetricsProcessor.UNDEFINED_FUNCTION_URI);
				}
			}else{
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.RETURN_TYPE_WARNING.get("Function " + expressionName), expression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNDEFINED_FUNCTION_URI);
				}
			}				
		}
		//Either the Reasoner/Translator provides full built-in information or provides nothing. 
		//Regardless, if this point is reached, error.
		else {
			getModelProcessor().addTypeCheckingError(SadlErrorMessages.RETURN_TYPE_WARNING.get("Function " + expressionName), expression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNDEFINED_FUNCTION_URI);
			}
		}
//		throw new DontTypeCheckException();	// we've added an error message (presumably) so why throw exception?
	}
	
	protected TypeCheckInfo getType(SadlResource sr) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		return getType(sr, sr);
	}
	
	protected TypeCheckInfo getType(SadlResource sr, EObject reference) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException{
		String conceptUri = declarationExtensions.getConceptUri(sr);
		EObject expression = sr.eContainer();
		if (conceptUri == null) {
			getModelProcessor().addTypeCheckingError(SadlErrorMessages.UNIDENTIFIED.toString(), (reference != null ? reference : sr));
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
			}
		}
		
		OntConceptType conceptType;
		try {
			conceptType = declarationExtensions.getOntConceptType(sr);
		} catch (CircularDefinitionException e) {
			conceptType = e.getDefinitionType();
			getModelProcessor().addTypeCheckingError(e.getMessage(), reference);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
			}
		}
		if(conceptType.equals(OntConceptType.CLASS) || conceptType.equals(OntConceptType.DATATYPE)){
			NamedNode tctype = createNamedNode(conceptUri, conceptType);
			ConceptName conceptName = getModelProcessor().namedNodeToConceptName(tctype);
			if (conceptName.getUri().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI) && getModelProcessor().ignoreUnittedQuantities) {
				if (expression instanceof SadlClassOrPropertyDeclaration) {
					Iterator<SadlProperty> spitr = ((SadlClassOrPropertyDeclaration)expression).getDescribedBy().iterator();
					while (spitr.hasNext()) {
						SadlProperty sp = spitr.next();
						if (declarationExtensions.getConceptUri(sp.getProperty()).equals(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI)) {
							return getType(sr, declarationExtensions.getDeclaration(sp.getProperty()));
						}
					}
				}
				else {
					getModelProcessor().addTypeCheckingError("Can't handle this expression type when ignoring UnittedQuantities",expression);
				}
			}
			else {
				List<ConceptName> impliedProps = getImpliedProperties(theJenaModel.getResource(conceptUri));
				TypeCheckInfo tci = new TypeCheckInfo(conceptName, tctype, this, impliedProps, reference);
				if (conceptType.equals(OntConceptType.CLASS)) {
 					tci.setTypeToExprRelationship("self");
 				}
				return tci;
			}
		}
		else if(conceptType.equals(OntConceptType.DATATYPE_PROPERTY)){
			TypeCheckInfo propcheckinfo = getNameProperty(sr, ConceptType.DATATYPEPROPERTY, conceptUri, reference);
			if (propcheckinfo != null) {
				return propcheckinfo;
			}
			throw new PropertyWithoutRangeException(declarationExtensions.getConcreteName(sr));
		}
		else if(conceptType.equals(OntConceptType.CLASS_PROPERTY)){
			TypeCheckInfo propcheckinfo =  getNameProperty(sr, ConceptType.OBJECTPROPERTY, conceptUri, reference);
			if (propcheckinfo != null) {
				return propcheckinfo;
			}
			throw new PropertyWithoutRangeException(declarationExtensions.getConcreteName(sr));
		}
		else if (conceptType.equals(OntConceptType.RDF_PROPERTY)) {
			TypeCheckInfo rdfpropcheckinfo = getNameProperty(sr, ConceptType.RDFPROPERTY, conceptUri, reference);
			if (rdfpropcheckinfo != null) {
				return rdfpropcheckinfo;
			}
			throw new PropertyWithoutRangeException(declarationExtensions.getConcreteName(sr));
		}
		else if(conceptType.equals(OntConceptType.INSTANCE)){
			// this is an instance--if it is already in the ontology we can get its type. If not maybe we can get it from its declaration
			Individual individual = theJenaModel.getIndividual(conceptUri);
			if(individual == null){
				SadlResource qnmDecl = declarationExtensions.getDeclaration(sr);
				if (qnmDecl != null) {
					if (qnmDecl.eContainer() instanceof SadlInstance) {
						SadlTypeReference typeref = ((SadlInstance)qnmDecl.eContainer()).getType();
						if (typeref != null) {
							return getType(typeref);
						}
						else {
							SadlResource nor = ((SadlInstance)qnmDecl.eContainer()).getNameOrRef();
							if (!nor.equals(qnmDecl)) {
								return getType(nor);
							}
						}
					}
					else if (qnmDecl.eContainer() instanceof SadlMustBeOneOf) {
						if (qnmDecl.eContainer().eContainer() instanceof SadlClassOrPropertyDeclaration) {
							return getType(((SadlClassOrPropertyDeclaration)qnmDecl.eContainer().eContainer()).getClassOrProperty().get(0));
						}
					}else if(qnmDecl.eContainer() instanceof SadlCanOnlyBeOneOf) {
						if (qnmDecl.eContainer().eContainer() instanceof SadlClassOrPropertyDeclaration) {
							return getType(((SadlClassOrPropertyDeclaration)qnmDecl.eContainer().eContainer()).getClassOrProperty().get(0));
						}
					}
				}
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.UNIDENTIFIED.toString(), reference);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				return null;
			}
			ConceptName instConceptName = new ConceptName(conceptUri);
			instConceptName.setType(ConceptType.INDIVIDUAL);
			// TODO could belong to multiple classes
			ExtendedIterator<Resource> typeitr = individual.listRDFTypes(true);
			TypeCheckInfo compoundTci = null;
			TypeCheckInfo tci = null;
			while (typeitr.hasNext()) {
				Resource ontResource = typeitr.next();
				if(!ontResource.isURIResource()){
					if (isSadlTypedList(ontResource) && ontResource.canAs(OntClass.class)) {
						tci = getSadlTypedListTypeCheckInfo(ontResource.as(OntClass.class), null, reference, null);
					}
					else {
						ConceptName declarationConceptName = new ConceptName("TODO");
						declarationConceptName.setType(ConceptType.ONTCLASS);
						tci =  new TypeCheckInfo(instConceptName, null, this, reference);
					}
				}
				else {
					String uriOfTypeToBeReturned = ontResource.getURI();
					NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(uriOfTypeToBeReturned, NodeType.ClassNode));
					ConceptName conceptName = getModelProcessor().namedNodeToConceptName(tctype);
					List<ConceptName> impliedProperties = getImpliedProperties(ontResource);
					tci = new TypeCheckInfo(instConceptName, tctype, this, impliedProperties, reference);
				}
				if (typeitr.hasNext() && compoundTci == null) {
					compoundTci = new TypeCheckInfo(instConceptName, this, reference);
				}
				if (compoundTci != null) {
					if (compoundTci.getCompoundTypes() == null || !compoundTci.getCompoundTypes().contains(tci)) {
						compoundTci.addCompoundType(tci);
					}
				}
			}
			if (compoundTci != null) {
				return compoundTci;
			}
			if (tci == null) {
				tci = new TypeCheckInfo(instConceptName);
			}
			return tci;
		}
		else if(conceptType.equals(OntConceptType.VARIABLE)){
			String nm = declarationExtensions.getConcreteName(sr);
			String uri = declarationExtensions.getConceptUri(sr);
			if (nm == null || uri == null) {
				getModelProcessor().addTypeCheckingError("Unable to resolve name",reference);
				return null;
			}
			TypeCheckInfo tci = getVariableType(ConceptType.VARIABLE, sr, nm, uri, reference);
			if (tci != null) {				// will be null on invalid input, e.g., during clean
				ConceptName et = new ConceptName(uri);
				et.setType(ConceptType.VARIABLE);
				tci.setExpressionType(et);
			}
			return tci;
		}
		else if(conceptType.equals(OntConceptType.ANNOTATION_PROPERTY)){
			//This matches any type.
			ConceptName declarationConceptName = new ConceptName("TODO");
			TypeCheckInfo tci = new TypeCheckInfo(declarationConceptName, null, this, reference);
			tci.setAdditionalInformation("" + conceptUri + "' is an annotation property and has no range");
			tci.setSeverity(Severity.IGNORE);
			return tci;
		}
		else if (conceptType.equals(OntConceptType.FUNCTION_DEFN)) {
			return getFunctionType(sr);
		}
		else if (conceptType.equals(OntConceptType.CLASS_LIST)) {
			if (conceptUri != null) {
				OntClass ontcls = theJenaModel.getOntClass(conceptUri);
				ConceptName typecn = getListClassType(ontcls);
				conceptUri = typecn.toFQString();
			}
			SadlResource declsr = declarationExtensions.getDeclaration(sr);
			NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(conceptUri, NodeType.ClassListNode));
			ConceptName conceptName = getModelProcessor().namedNodeToConceptName(tctype);
			if (conceptName.getUri().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI) && getModelProcessor().ignoreUnittedQuantities) {
				if (expression instanceof SadlClassOrPropertyDeclaration) {
					Iterator<SadlProperty> spitr = ((SadlClassOrPropertyDeclaration)expression).getDescribedBy().iterator();
					while (spitr.hasNext()) {
						SadlProperty sp = spitr.next();
						if (declarationExtensions.getConceptUri(sp.getProperty()).equals(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI)) {
							return getType(sp.getProperty(), declarationExtensions.getDeclaration(sp.getProperty()));
						}
					}
				}
				else {
					getModelProcessor().addTypeCheckingError("Can't handle this expression type when ignoring UnittedQuantities",expression);
				}
			}
			else {
				List<ConceptName> impliedProps = getImpliedProperties(theJenaModel.getResource(conceptUri));
				TypeCheckInfo tci = new TypeCheckInfo(conceptName, tctype, this, impliedProps, reference);
				return tci;
			}
		}
		
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, null, this, reference);
	}
	
	private String getSadlModelUri(EObject eobj) {
		if (eobj.eContainer() instanceof SadlModel) {
			return ((SadlModel)eobj.eContainer()).getBaseUri();
		}
		else if (eobj.eContainer() != null) {
			return getSadlModelUri(eobj.eContainer());
		}
		return null;
	}

	private ConceptName createTypedConceptName(String conceptUri, OntConceptType conceptType) {
		return modelProcessor.createTypedConceptName(conceptUri, conceptType);
	}
	
	private NamedNode createNamedNode(String conceptUri, OntConceptType conceptType) {
		return modelProcessor.createNamedNode(conceptUri, conceptType);
	}

	protected TypeCheckInfo getNameProperty(SadlResource qnm, ConceptType propertyType, String conceptUri, EObject expression) throws DontTypeCheckException, InvalidTypeException, TranslationException, InvalidNameException {
		Property property = theJenaModel.getProperty(conceptUri);

		if(property == null){
			getModelProcessor().addTypeCheckingError(SadlErrorMessages.UNIDENTIFIED.toString(), qnm != null ? qnm : expression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			return null;
		}
		ConceptName propConceptName = new ConceptName(conceptUri);
		propConceptName.setType(propertyType);
		return getTypeInfoFromRange(propConceptName, property, expression);
	}

	private TypeCheckInfo getPropertyDomainType(SadlResource propsr, EObject expr) throws InvalidTypeException {
		String propuri = declarationExtensions.getConceptUri(propsr);
		Property prop = theJenaModel.getProperty(propuri);
		ConceptName pcn = new ConceptName(propuri);
		getModelProcessor().setPropertyConceptNameType(pcn, prop);
		return getTypeInfoFromDomain(pcn, prop, expr);
	}
	
	private TypeCheckInfo getPropertyRangeType(SadlResource propsr, EObject expr) throws DontTypeCheckException, InvalidTypeException, TranslationException, InvalidNameException {
		String propuri = declarationExtensions.getConceptUri(propsr);
		Property prop = theJenaModel.getProperty(propuri);
		ConceptName pcn = new ConceptName(propuri);
		getModelProcessor().setPropertyConceptNameType(pcn, prop);
		return getTypeInfoFromRange(pcn, prop, expr);
	}

	public TypeCheckInfo getTypeInfoFromDomain(ConceptName propConceptName, Property property, EObject expression) throws InvalidTypeException {
		StmtIterator stmtitr = theJenaModel.listStatements(property, RDFS.domain, (RDFNode)null);
		while (stmtitr.hasNext()) {
			RDFNode obj = stmtitr.nextStatement().getObject();
			if (obj.canAs(Resource.class)) {
				try {
					return createTypeCheckInfoForPropertyDomain(obj.asResource(), propConceptName, expression);
				} catch (DontTypeCheckException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}			
			}
		}
		return null;
	}

	public TypeCheckInfo getTypeInfoFromRange(ConceptName propConceptName, Property property,
			EObject expression) throws DontTypeCheckException, InvalidTypeException, TranslationException, InvalidNameException {
		ConceptType propertyType = propConceptName.getType();
		StmtIterator sitr = theJenaModel.listStatements(property, RDFS.range, (RDFNode)null);
		if (sitr.hasNext()) {
			TypeCheckInfo compoundTci = null;
			TypeCheckInfo tci = null;
			while (sitr.hasNext()) {
				RDFNode first = sitr.next().getObject();
				boolean isList = false;
				if(first.isURIResource()){
					tci = createTypeCheckInfoForPropertyRange(first, propConceptName, expression, propertyType);
				}
				else if (isSadlTypedList(first)) {
					// get type restriction on "first" property--this is the type
					tci = getSadlTypedListTypeCheckInfo(first.as(OntClass.class), propConceptName, expression, propertyType);
					isList = true;
				}
				else {
					tci = createTypeCheckInfoForNonUriPropertyRange(first, propConceptName, expression, propertyType);
				}
				if (tci != null) {
					if (sitr.hasNext() && compoundTci == null) {
						compoundTci = new TypeCheckInfo(propConceptName, this, expression);
						if (isList) {
							compoundTci.setRangeValueType(RangeValueType.LIST);
						}
					}
					if (compoundTci != null) {
						if (tci.getCompoundTypes() != null) {
							for (TypeCheckInfo subtci:tci.getCompoundTypes()) {
								compoundTci.addCompoundType(subtci);
							}
						}
						else {
							compoundTci.addCompoundType(tci);
						}
					}
				}
			}
			sitr.close();
			if (compoundTci != null) {
				return compoundTci;
			}
			return tci;
		}
		else {
			// no range on this property, check super properties
			StmtIterator sitr2 = theJenaModel.listStatements(property, RDFS.subPropertyOf, (RDFNode)null);
			while (sitr2.hasNext()) {
				RDFNode psuper = sitr2.next().getObject();
				if (psuper.isResource()) {
					TypeCheckInfo superTCInfo = getNameProperty(null, propertyType, psuper.asResource().getURI(), expression);
					if (superTCInfo != null) {
						sitr2.close();
						return superTCInfo;
					}
				}
			}
		}
		return null;
	}

	private TypeCheckInfo createTypeCheckInfoForNonUriPropertyRange(RDFNode rng, ConceptName propConceptName,
			EObject expression, ConceptType propertyType) throws InvalidTypeException, TranslationException, InvalidNameException {
		TypeCheckInfo tci = null;
		if (isSadlTypedList(rng)) {
			// get type restriction on "first" property--this is the type
			tci = getSadlTypedListTypeCheckInfo(rng.as(OntClass.class), propConceptName, expression, propertyType);
		}
		 else if (rng.canAs(UnionClass.class)){
			UnionClass ucls = rng.as(UnionClass.class);
			try {
				ExtendedIterator<? extends OntClass> eitr = ucls.listOperands();
				if (eitr.hasNext()) {
					tci = new TypeCheckInfo(propConceptName, this, expression);
					while (eitr.hasNext()) {
						OntClass uclsmember = eitr.next();
						if (uclsmember.isURIResource()) {
							TypeCheckInfo utci = createTypeCheckInfoForPropertyRange(uclsmember, propConceptName, expression, propertyType);
							tci.addCompoundType(utci);
						}
						else {
							TypeCheckInfo utci = createTypeCheckInfoForNonUriPropertyRange(uclsmember, propConceptName, expression, propertyType);
							tci.addCompoundType(utci);
						}
					}
				}
			}
			catch (Exception e) {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.UNEXPECTED_TYPE_CHECK_ERROR.get("union range", e.getMessage() ), getDefaultContext());
			}
		}
		else if (rng.canAs(IntersectionClass.class)){
			getModelProcessor().addWarning(SadlErrorMessages.TYPE_CHECK_HANDLE_WARNING.get("intersections"), expression);
		}
		else if (rng.canAs(Restriction.class)){
			getModelProcessor().addWarning(SadlErrorMessages.TYPE_CHECK_HANDLE_WARNING.get("restrictions"), expression);
		}
		return tci;
	}

	private TypeCheckInfo createTypeCheckInfoForNonUriPropertyDomain(Resource domain, ConceptName propConceptName,
			EObject expression) throws InvalidTypeException {
		TypeCheckInfo tci = null;
		if (domain.canAs(UnionClass.class)){
			UnionClass ucls = domain.as(UnionClass.class);
			tci = createTypeCheckInfoForUnion(propConceptName, ucls, true, expression);
		}
		else if (domain.canAs(IntersectionClass.class)){
			getModelProcessor().addWarning(SadlErrorMessages.TYPE_CHECK_HANDLE_WARNING.get("intersections"), expression);
		}
		else {
			getModelProcessor().addTypeCheckingError("Unhandled type of non-URI domain", expression);
		}
		return tci;
	}

	private TypeCheckInfo createTypeCheckInfoForUnion(ConceptName propConceptName, UnionClass ucls, boolean isDomain,
			EObject expression) throws InvalidTypeException {
		TypeCheckInfo tci = null;
		try {
			ExtendedIterator<? extends OntClass> eitr = ucls.listOperands();
			if (eitr.hasNext()) {
				tci = new TypeCheckInfo(propConceptName, this, expression);
				while (eitr.hasNext()) {
					try {
						OntClass uclsmember = eitr.next();
						if (uclsmember.isURIResource()) {
							if (isDomain) {
								TypeCheckInfo utci = createTypeCheckInfoForPropertyDomain(uclsmember, propConceptName, expression);
								tci.addCompoundType(utci);
							}
							else {
								TypeCheckInfo utci = createTypeCheckInfoForPropertyRange(uclsmember, propConceptName, expression, propConceptName.getType());
								tci.addCompoundType(utci);
							}
						}
						else {
							if (isDomain) {
								TypeCheckInfo utci = createTypeCheckInfoForNonUriPropertyDomain(uclsmember, propConceptName, expression);
								tci.addCompoundType(utci);
							}
							else {
								TypeCheckInfo utci = createTypeCheckInfoForNonUriPropertyRange(uclsmember, propConceptName, expression, propConceptName.getType());
								tci.addCompoundType(utci);
							}
						}
					}
					catch (Exception e) {
						RDFNode uv = ucls.getPropertyValue(OWL.unionOf);
						if (ucls.canAs(RDFList.class)) {
							RDFList uclsr = ucls.as(RDFList.class);
							List<RDFNode> jlst = uclsr.asJavaList();
							for (int i = 0; jlst != null && i < jlst.size(); i++) {
								System.out.println(jlst.get(i).toString());
							}
						}
						
					}
				}
			}
		}
		catch (Exception e) {
			getModelProcessor().addTypeCheckingError(SadlErrorMessages.UNEXPECTED_TYPE_CHECK_ERROR.get("union range", e.getMessage() ), getDefaultContext());
		}
		return tci;
	}

	private TypeCheckInfo getSadlTypedListTypeCheckInfo(OntClass lst, ConceptName propConceptName, EObject expression, ConceptType propertyType) throws InvalidTypeException, TranslationException, InvalidNameException {
		Resource avf = getSadlTypedListType(lst);
		if (avf != null && avf.isURIResource()) {
			List<ConceptName> impliedProperties = null;
			if(avf.asResource().canAs(OntClass.class)) {
				impliedProperties = getImpliedProperties(avf.asResource());				
			}
			NamedNode tctype = new NamedNode(avf.getURI());
			tctype.setListLength(getSadlTypedListLengthRestriction(lst));
			tctype.setMaxListLength(getSadlTypedListMaxLengthRestriction(lst));
			tctype.setMinListLength(getSadlTypedListMinLengthRestriction(lst));
			if (propertyType != null && propertyType.equals(ConceptType.DATATYPEPROPERTY)) {
				tctype.setNodeType(NodeType.DataTypeListNode);
			}
			else if (tctype.getNamespace().equals(XSD.getURI())) {
				tctype.setNodeType(NodeType.DataTypeListNode);
			}
			else {
				tctype.setNodeType(NodeType.ClassListNode);
			}
			ConceptName rangeConceptName = getModelProcessor().namedNodeToConceptName(tctype);
			TypeCheckInfo tci = new TypeCheckInfo(propConceptName, tctype, impliedProperties, this, expression);
			tci.setRangeValueType(RangeValueType.LIST);
			return tci;
		}
		return null;
	}

	public boolean isSadlTypedList(RDFNode node) {
		if (node instanceof Resource && ((Resource)node).canAs(OntClass.class)) {
			OntClass cls = ((Resource)node).as(OntClass.class);
			OntResource lstrsrc = theJenaModel.getOntResource(SadlConstants.SADL_LIST_MODEL_LIST_URI);
			if (lstrsrc != null && cls.hasSuperClass(lstrsrc)) {		// if the model doesn't have any lists the list model will not have been imported
				return true;
			}
		}
		return false;
	}
	
	public Resource getSadlTypedListType(OntClass lstcls) {
		ExtendedIterator<OntClass> eitr = ((OntClass)lstcls.as(OntClass.class)).listSuperClasses(true);
		while (eitr.hasNext()) {
			OntClass cls = eitr.next();
			if (cls.isRestriction()) {
				if (cls.canAs(AllValuesFromRestriction.class)) {
					if (((AllValuesFromRestriction)cls.as(AllValuesFromRestriction.class)).onProperty(theJenaModel.getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
						Resource avf = ((AllValuesFromRestriction)cls.as(AllValuesFromRestriction.class)).getAllValuesFrom();
						eitr.close();
						return avf;
					}
				}
			}
		}
		return null;
	}
	
	public int getSadlTypedListLengthRestriction(OntClass lstcls) {
		ExtendedIterator<OntClass> eitr = ((OntClass)lstcls.as(OntClass.class)).listSuperClasses(true);
		while (eitr.hasNext()) {
			OntClass cls = eitr.next();
			if (cls.isRestriction()) {
				if (cls.canAs(HasValueRestriction.class)) {
					if (((HasValueRestriction)cls.as(HasValueRestriction.class)).onProperty(theJenaModel.getProperty(SadlConstants.SADL_LIST_MODEL_LENGTH_RESTRICTION_URI))) {
						int length = ((HasValueRestriction)cls.as(HasValueRestriction.class)).getHasValue().asLiteral().getInt();
						eitr.close();
						return length;
					}
				}
			}
		}
		return -1;
	}
	
	public int getSadlTypedListMaxLengthRestriction(OntClass lstcls) {
		ExtendedIterator<OntClass> eitr = ((OntClass)lstcls.as(OntClass.class)).listSuperClasses(true);
		while (eitr.hasNext()) {
			OntClass cls = eitr.next();
			if (cls.isRestriction()) {
				if (cls.canAs(HasValueRestriction.class)) {
					if (((HasValueRestriction)cls.as(HasValueRestriction.class)).onProperty(theJenaModel.getProperty(SadlConstants.SADL_LIST_MODEL_MAXLENGTH_RESTRICTION_URI))) {
						int length = ((HasValueRestriction)cls.as(HasValueRestriction.class)).getHasValue().asLiteral().getInt();
						eitr.close();
						return length;
					}
				}
			}
		}
		return -1;
	}
	
	public int getSadlTypedListMinLengthRestriction(OntClass lstcls) {
		ExtendedIterator<OntClass> eitr = ((OntClass)lstcls.as(OntClass.class)).listSuperClasses(true);
		while (eitr.hasNext()) {
			OntClass cls = eitr.next();
			if (cls.isRestriction()) {
				if (cls.canAs(HasValueRestriction.class)) {
					if (((HasValueRestriction)cls.as(HasValueRestriction.class)).onProperty(theJenaModel.getProperty(SadlConstants.SADL_LIST_MODEL_MINLENGTH_RESTRICTION_URI))) {
						int length = ((HasValueRestriction)cls.as(HasValueRestriction.class)).getHasValue().asLiteral().getInt();
						eitr.close();
						return length;
					}
				}
			}
		}
		return -1;
	}
	
	private TypeCheckInfo createTypeCheckInfoForPropertyDomain(Resource domain, ConceptName propConceptName, EObject expression) throws DontTypeCheckException, InvalidTypeException {
		TypeCheckInfo tci = null;
		if (!domain.isURIResource()) {
			return createTypeCheckInfoForNonUriPropertyDomain(domain, propConceptName, expression);
		}
		NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(domain.getURI(), NodeType.ClassNode));
		if (tci == null) {
			tci = new TypeCheckInfo(propConceptName, tctype, this, expression);
			tci.setTypeToExprRelationship("domain");
		}
		if (isTypedListSubclass(domain)) {
			tci.setRangeValueType(RangeValueType.LIST);
		}
		return tci;
	}

	private TypeCheckInfo createTypeCheckInfoForPropertyRange(RDFNode first, ConceptName propConceptName,
			EObject expression, ConceptType propertyType) throws InvalidTypeException, TranslationException, InvalidNameException {
		TypeCheckInfo tci = null;
		NamedNode rangeNamedNode = getModelProcessor().validateNamedNode(new NamedNode(first.asResource().getURI(), NodeType.DataTypeNode));
//		ConceptName rangeConceptName = new ConceptName(first.asResource().getURI());
		if (propertyType.equals(ConceptType.DATATYPEPROPERTY)) {
//			rangeConceptName.setType(ConceptType.RDFDATATYPE);
			OntResource range;
			range = theJenaModel.getOntResource(rangeNamedNode.toFullyQualifiedString());
			if (theJenaModel.listStatements(range, RDF.type, RDFS.Datatype).hasNext()) {
				// this is a user-defined datatype
				RDFNode rngEC = range.listPropertyValues(OWL.equivalentClass).next();
				if (rngEC != null && rngEC.canAs(OntResource.class)) {
					try {
						RDFNode baseType = rngEC.as(OntResource.class).listPropertyValues(OWL2.onDatatype).next();
						if (baseType != null && baseType.isURIResource()) {
							NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(baseType.asResource().getURI(), NodeType.DataTypeNode));
							tci = new TypeCheckInfo(propConceptName, tctype, this, expression);
						}
					}
					catch (Exception e) {
						// didn't have a OWL2.onDataType property
						OntResource rngOC = rngEC.as(OntResource.class);
						if (rngOC.canAs(OntClass.class)) {
							OntClass rmgOntC = rngOC.as(OntClass.class);
							if (rmgOntC.isUnionClass()) {
								tci = createTypeCheckInfoForUnion(propConceptName, rmgOntC.asUnionClass(), false, expression);
							}
						}
					}
				}
			}
			else {
//					rangeConceptName.setRangeValueType(propConceptName.getRangeValueType());
				if (propConceptName.getRangeValueType().equals(RangeValueType.LIST)) {
					rangeNamedNode.setNodeType(NodeType.DataTypeListNode);
				}
			}
		}
		else {
			rangeNamedNode.setNodeType(NodeType.ClassNode);
		}
		List<ConceptName> impliedProperties = getImpliedProperties(first.asResource());
		if (tci == null) {
			tci = new TypeCheckInfo(propConceptName, rangeNamedNode, impliedProperties, this, expression);
		}
		else if (impliedProperties != null){
			tci.addImplicitProperties(impliedProperties);
		}
		if (isTypedListSubclass(first)) {
			tci.setRangeValueType(RangeValueType.LIST);
			if (first.isURIResource()) {
				// looks like a named list in which case we probably have the wrong type
				if (!first.asResource().canAs(OntClass.class)){
					getModelProcessor().addTypeCheckingError("Unexpected non-OntClass named list, please report."	, expression); 
				}
				return getSadlTypedListTypeCheckInfo(first.asResource().as(OntClass.class), propConceptName, expression, propertyType);
			}
		}
		return tci;
	}

	protected List<ConceptName> getImpliedProperties(Resource first) throws InvalidTypeException {
		return getModelProcessor().getImpliedProperties(first);
	}

//	private boolean isRangeKlugyDATASubclass(OntResource rsrc) {
//		if (rsrc.getURI().endsWith("#DATA")) {
//			return true;
//		}
//		if (rsrc.canAs(OntClass.class)){
//			ExtendedIterator<OntClass> itr = rsrc.as(OntClass.class).listSuperClasses();
//			while (itr.hasNext()) {
//				OntClass spr = itr.next();
//				if (spr.isURIResource() && spr.getURI().endsWith("#DATA")) {
//					return true;
//				}
//			}
//		}
//		return false;
//	}

	protected TypeCheckInfo getVariableType(ConceptType variable, SadlResource sr, String conceptNm, String conceptUri, EObject reference) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		if (sr == null) {
			return null;	// this might happen when cleaning with editor open
		}
		//  1) get the Name of the SadlResource for the variable
		//	2) get the definition of the Name
		//  3) if the container of the reference is a SubjHasProp:
		//		3a) look at the SubjHasProp left and right
		//			3aa) if the Name matches left, get the type as the domain of the SubjHasProp prop
		//			3ab) if the Name matches right, get the type as the range of the SubjHasProp prop
		
		VariableNode var = getModelProcessor().getVariable(conceptNm);
		if (var != null && var.getType() != null) {
			NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(var.getType().toFullyQualifiedString(), NodeType.VariableNode));
			ConceptName et = getModelProcessor().namedNodeToConceptName(tctype);
			TypeCheckInfo tci = new TypeCheckInfo(et, var.getType(), this, reference);
			return tci;
		}
		SadlResource name = sr.getName();
		if (name != null) {
			SadlResource def = declarationExtensions.getDeclaration(name);
			EObject defContainer = def.eContainer();
			if (defContainer instanceof SubjHasProp) {
				SadlResource propsr = ((SubjHasProp)defContainer).getProp();
				if (((SubjHasProp)defContainer).getLeft().equals(name)) {
					return getPropertyDomainType(propsr, reference);
				}
				else if (((SubjHasProp)defContainer).getRight() != null && ((SubjHasProp)defContainer).getRight().equals(name)) {
					return getPropertyRangeType(propsr, reference);
				}
			}
			else if (defContainer instanceof SadlParameterDeclaration) {
				SadlTypeReference exprType = ((SadlParameterDeclaration)defContainer).getType();
				return getType(exprType);
			}
			else if (defContainer instanceof BinaryOperation) {
				if (((BinaryOperation)defContainer).getLeft() instanceof Name) { // && !((BinaryOperation)defContainer).getLeft().equals(reference)) {
					if (getModelProcessor().isDeclaration(((BinaryOperation)defContainer).getRight())) {
						Declaration decl = getEmbeddedDeclaration(defContainer);  // are we in a Declaration (a real declaration--the type is a class)
						if (decl != null) {
							return getType(decl);
						}
					}
					else {
						TypeCheckInfo ptci = getType(((BinaryOperation)defContainer).getRight());
						return ptci;
					}
				}
				else if (((BinaryOperation)defContainer).getLeft() instanceof PropOfSubject && ((BinaryOperation)defContainer).getRight() instanceof Name) {
					TypeCheckInfo tci = getType(((BinaryOperation)defContainer).getLeft());
					return tci;
				}
				else {
					getModelProcessor().addTypeCheckingError("Unhandled variable container for variable '" + conceptNm + "'.", defContainer);
				}
			}
			else if (defContainer instanceof SadlParameterDeclaration) {
				SadlTypeReference exprType = ((SadlParameterDeclaration)defContainer).getType();
				return getType(exprType);
			}
			else if (defContainer instanceof BinaryOperation) {
				if (((BinaryOperation)defContainer).getLeft() instanceof Name) { // && !((BinaryOperation)defContainer).getLeft().equals(expression)) { // this causes failure in some cases awc 9/27/17
					if (getModelProcessor().isDeclaration(((BinaryOperation)defContainer).getRight())) {
						Declaration decl = getEmbeddedDeclaration(defContainer);  // are we in a Declaration (a real declaration--the type is a class)
						if (decl != null) {
							return getType(decl);
						}
					}
					else {
						TypeCheckInfo ptci = getType(((BinaryOperation)defContainer).getRight());
						return ptci;
					}
				}
			}
		}
		
		if (conceptUri == null) {
			return null;
		}
		EObject refContainer = reference.eContainer();
		if (refContainer instanceof SadlParameterDeclaration) {
			SadlTypeReference exprType = ((SadlParameterDeclaration)refContainer).getType();
			return getType(exprType);
		}
		else if (refContainer instanceof SubjHasProp) {
			SadlResource psr = ((SubjHasProp)refContainer).getProp();
			if (psr.equals(sr)) {
				ConceptName tci = new ConceptName("TODO");
				return new TypeCheckInfo(tci, null, this, refContainer);
			}
			TypeCheckInfo ptci = getType(psr);
			return ptci;
		}
		else if (refContainer instanceof PropOfSubject) {
			Expression pred = ((PropOfSubject)refContainer).getLeft();
			if (pred instanceof SadlResource && ((PropOfSubject)refContainer).getRight().equals(name)) {
				return getPropertyDomainType((SadlResource) pred, reference);
			}
		}
		else if (refContainer instanceof BinaryOperation) {
			if (((BinaryOperation)refContainer).getLeft() instanceof Name && !((BinaryOperation)refContainer).getLeft().equals(reference)) {
				if (getModelProcessor().isDeclaration(((BinaryOperation)refContainer).getRight())) {
					Declaration decl = getEmbeddedDeclaration(refContainer);  // are we in a Declaration (a real declaration--the type is a class)
					if (decl != null) {
						return getType(decl);
					}
				}
				else {
					if (((BinaryOperation)refContainer).getRight().equals(reference)) {
						// this will be an infinite recursion otherwise
						return null;
					}
					TypeCheckInfo ptci = getType(((BinaryOperation)refContainer).getRight());
					return ptci;
				}
			}
		}
		else if (refContainer instanceof SelectExpression) {
			// find name in expression and get type from there
			TypeCheckInfo tci = getTypeFromWhereExpression(sr, conceptUri, ((SelectExpression)refContainer).getWhereExpression());
			if (tci == null) {
				throw new DontTypeCheckException();
			}
		}
		NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(conceptUri, NodeType.VariableNode));
		ConceptName declarationConceptName = getModelProcessor().namedNodeToConceptName(tctype);
		return new TypeCheckInfo(declarationConceptName, tctype, this, reference);
	}
	
	private TypeCheckInfo getTypeFromWhereExpression(SadlResource sr, String uri, Expression expr) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		if (expr instanceof SubjHasProp) {
			Expression sexpr = findDefiningExpression(uri, ((SubjHasProp)expr).getLeft());
			if (sexpr != null && !sexpr.equals(sr)  && !(sexpr instanceof Name && ((Name)sexpr).getName().equals(sr))) {
				return getType(sexpr);
			}
			sexpr = findDefiningExpression(uri, ((SubjHasProp)expr).getProp());
			if (sexpr != null && !sexpr.equals(sr)  && !(sexpr instanceof Name && ((Name)sexpr).getName().equals(sr))) {
				return getType(sexpr);
			}
			sexpr = findDefiningExpression(uri, ((SubjHasProp)expr).getRight());
			if (sexpr != null && !sexpr.equals(sr)  && !(sexpr instanceof Name && ((Name)sexpr).getName().equals(sr))) {
				return getType(sexpr);
			}
		} else if (expr instanceof Name) {
			String nuri = declarationExtensions.getConceptUri(((Name)expr).getName());
			if (nuri != null && nuri.equals(uri)) {
				if (expr != null && !expr.equals(sr)  && !(expr instanceof Name && ((Name)expr).getName().equals(sr))) {
					return getType(expr);
				}
			}
		}
		else if (expr instanceof SadlResource) {
			String nuri = declarationExtensions.getConceptUri((SadlResource)expr);
			if (expr != null && !expr.equals(sr)  && !(expr instanceof Name && ((Name)expr).getName().equals(sr))) {
				return getType(expr);
			}
		}
		return null;
	}

	private Expression findDefiningExpression(String uri, Expression expr) {
		if (expr instanceof SubjHasProp) {
			Expression sexpr = findDefiningExpression(uri, ((SubjHasProp)expr).getLeft());
			if (sexpr != null) {
				return sexpr;
			}
			sexpr = findDefiningExpression(uri, ((SubjHasProp)expr).getProp());
			if (sexpr != null) {
				return sexpr;
			}
			sexpr = findDefiningExpression(uri, ((SubjHasProp)expr).getRight());
			if (sexpr != null) {
				return ((SubjHasProp)expr).getProp();
			}
		} else if (expr instanceof Name) {
			String nuri = declarationExtensions.getConceptUri(((Name)expr).getName());
			if (nuri != null && nuri.equals(uri)) {
				return expr;
			}
		}
		else if (expr instanceof SadlResource) {
			String nuri = declarationExtensions.getConceptUri((SadlResource)expr);
			if (nuri != null && nuri.equals(uri)) {
				return expr;
			}
		}
		return null;
	}

	private TypeCheckInfo combineBinaryOperationTypesWithComparison(List<String> operations, EObject binExpr, EObject leftExpression, EObject rightExpression,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo, ImplicitPropertySide side) throws InvalidNameException, DontTypeCheckException, InvalidTypeException, TranslationException {
		if(!compareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo, side)){
			return null;
		}
		return combineBinaryOperationTypes(operations, binExpr, leftTypeCheckInfo, rightTypeCheckInfo);
	}

	private TypeCheckInfo combineBinaryOperationTypes(List<String> operations, EObject binExpr,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo)
			throws InvalidTypeException, TranslationException, InvalidNameException {
		if (getModelProcessor().isBooleanComparison(operations)) {
			NamedNode tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.xboolean.getURI(), NodeType.DataTypeProperty));
			ConceptName booleanLiteralConceptName = getModelProcessor().namedNodeToConceptName(tctype);
			return new TypeCheckInfo(booleanLiteralConceptName, tctype, this, binExpr);
		}
		else if (getModelProcessor().isNumericOperator(operations)) {
			if (leftTypeCheckInfo != null && isNumeric(leftTypeCheckInfo) && rightTypeCheckInfo != null && isNumeric(rightTypeCheckInfo)) {
				NamedNode lcn = getTypeCheckInfoType(leftTypeCheckInfo);
				NamedNode rcn = getTypeCheckInfoType(rightTypeCheckInfo);
				if (lcn == null || lcn.getNamespace() == null) {
					return leftTypeCheckInfo;
				}
				if (rcn == null || rcn.getNamespace() == null) {
					return rightTypeCheckInfo;
				}
				if (rcn.equals(lcn)) {
					return leftTypeCheckInfo;
				}
				else {
					ConceptName cn = numericalPrecedenceType(getModelProcessor().namedNodeToConceptName(lcn), getModelProcessor().namedNodeToConceptName(rcn));
					return new TypeCheckInfo(cn, getModelProcessor().conceptNameToNamedNode(cn), this, binExpr);
				}
			}
			else {
				NamedNode tctype;
				if (leftTypeCheckInfo != null && isNumeric(leftTypeCheckInfo)) {
					tctype = getTypeCheckInfoType(leftTypeCheckInfo);
				}
				else if (rightTypeCheckInfo != null && isNumeric(rightTypeCheckInfo)) {
					tctype = getTypeCheckInfoType(rightTypeCheckInfo);
				}
				else {
					tctype = getModelProcessor().validateNamedNode(new NamedNode(XSD.decimal.getURI(), NodeType.DataTypeNode));
				}
				ConceptName decimalLiteralConceptName = getModelProcessor().namedNodeToConceptName(tctype);
				decimalLiteralConceptName.setType(ConceptType.RDFDATATYPE);
				return new TypeCheckInfo(decimalLiteralConceptName, tctype, this, binExpr);
			}
		}
		else{
			return leftTypeCheckInfo;
		}
	}

	private ConceptName numericalPrecedenceType(ConceptName lcn, ConceptName rcn) throws InvalidNameException {
		if (lcn.getUri().equals(XSD.decimal.getURI())) {
			return lcn;
		} else if (rcn.getUri().equals(XSD.decimal.getURI())) {
			return rcn;
		}
		else if (lcn.getUri().equals(XSD.xdouble.getURI())) {
			return lcn;
		}
		else if (rcn.getUri().equals(XSD.xdouble.getURI())) {
			return rcn;
		}
		else if (lcn.getUri().equals(XSD.xfloat.getURI())) {
			return lcn;
		}
		else if (rcn.getUri().equals(XSD.xfloat.getURI())) {
			return rcn;
		}
		else if (lcn.getUri().equals(XSD.xlong.getURI())) {
			return lcn;
		}
		else if (rcn.getUri().equals(XSD.xlong.getURI())) {
			return rcn;
		}
		else if (lcn.getUri().equals(XSD.integer.getURI())) {
			return lcn;
		}
		else if (rcn.getUri().equals(XSD.integer.getURI())) {
			return rcn;
		}
		else if (lcn.getUri().equals(XSD.xint.getURI())) {
			return lcn;
		}
		else if (rcn.getUri().equals(XSD.xint.getURI())) {
			return rcn;
		}
		return rcn;
	}

	private NamedNode getTypeCheckInfoType(TypeCheckInfo tci) throws InvalidNameException, TranslationException, InvalidTypeException {
		if (tci.getExplicitValueType() != null && tci.getExplicitValueType().equals(ExplicitValueType.VALUE)) {
			if (tci.getExpressionType() instanceof ConceptName) {
				return getModelProcessor().conceptNameToNamedNode((ConceptName) tci.getExpressionType());
			}
		}
		if (tci.getTypeCheckType() instanceof NamedNode) {
			return (NamedNode) tci.getTypeCheckType();
		}
		if (tci.compoundTypes != null) {
			return null;
		}
		throw new InvalidNameException("Failed to get TypeCheckInfoType");
	}

	/**
	 * Compare two TypeCheckInfo structures
	 * @param operations
	 * @param leftExpression
	 * @param rightExpression
	 * @param leftTypeCheckInfo
	 * @param rightTypeCheckInfo
	 * @return return true if they pass type check comparison else false
	 * @throws InvalidNameException
	 * @throws DontTypeCheckException 
	 * @throws InvalidTypeException 
	 * @throws TranslationException 
	 */
	protected boolean compareTypes(List<String> operations, EObject leftExpression, EObject rightExpression,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo, ImplicitPropertySide side) throws InvalidNameException, DontTypeCheckException, InvalidTypeException, TranslationException {
		boolean listTemporarilyDisabled = false;
		try {
			boolean isNumericOperator = getModelProcessor().isNumericOperator(operations);
			boolean isNumericLeft = isNumeric(leftTypeCheckInfo) || isUnittedQuantity(leftTypeCheckInfo);
			boolean isNumericRight = isNumeric(rightTypeCheckInfo) || isUnittedQuantity(rightTypeCheckInfo);
			if (!isNumericOperator && (isNumericLeft || isNumericRight)) {
				isNumericOperator = getModelProcessor().canBeNumericOperator(operations);
			}
			if(!isNumericOperator) {
				isNumericOperator = getModelProcessor().isNumericComparisonOperator(operations);
			}
			// negated expressions have the type of the expression negated
			if (leftExpression instanceof UnaryExpression && ((UnaryExpression)leftExpression).getOp().equals("not")) {
				leftExpression = ((UnaryExpression)leftExpression).getExpr();
			}
			if (rightExpression instanceof UnaryExpression && ((UnaryExpression)rightExpression).getOp().equals("not")) {
				rightExpression = ((UnaryExpression)rightExpression).getExpr();
			}
			if (rightExpression instanceof Constant && ((Constant)rightExpression).getConstant().equals("known")) {
				//	everything matches "known"
				return true;
			}
			if (leftExpression instanceof Constant && ((Constant)leftExpression).getConstant().equals(SadlConstants.CONSTANT_KNOWN)) {
				//	everything matches "known"
				return true;
			}
			if (rightExpression instanceof Constant && ((Constant)rightExpression).getConstant().equals(SadlConstants.CONSTANT_NONE)) {
				//	everything matches "None"
				return true;
			}
			if (leftExpression instanceof Constant && ((Constant)leftExpression).getConstant().equals(SadlConstants.CONSTANT_NONE)) {
				//	everything matches "None"
				return true;
			}
			if (!isNumericOperator) {
				if (directCompareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo, side)) {
					return true;
				}
			}
			if (leftExpression instanceof Constant && 
					(((Constant)leftExpression).getConstant().equals("value") || ((Constant)leftExpression).getConstant().equals("type"))) {
				listTemporarilyDisabled = true;
				leftTypeCheckInfo = convertListTypeToElementOfListType(leftTypeCheckInfo);
			}
			if (isNumericOperator) {
				if (!isNumericLeft) {
					if (!isNumericRight) {
						side = ImplicitPropertySide.BOTH;
					}
					else {
						side = ImplicitPropertySide.LEFT;
					}
				}
				else if (!isNumericRight) {
					side = ImplicitPropertySide.RIGHT;
				}
			}
			if (side.equals(ImplicitPropertySide.NONE)) {
				// Compare literal types
				if (compareTypesRecursively(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)){
					return true;
				}
			}
			else if (side.equals(ImplicitPropertySide.BOTH)) {
				String opstr = (operations != null && operations.size() > 0) ? operations.get(0) : null;
				if (useImpliedPropertyToMatchNumericOperator(leftExpression, leftTypeCheckInfo, "left side of '" + opstr + "'") && 
						useImpliedPropertyToMatchNumericOperator(rightExpression, rightTypeCheckInfo, "right side of '" + opstr + "'")) {
					return true;
				}
				return false; 	// don't fall through to call below, it doesn't handle BOTH
			}
			//Compare implied property types
			if(compareTypesUsingImpliedPropertiesRecursively(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo, side)){
				return true;
			}
				
			if (directCompareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo, side)) {
				return true;
			}
			return false;
		}
		finally {
			if (listTemporarilyDisabled) {
				leftTypeCheckInfo = convertElementOfListToListType(leftTypeCheckInfo);
			}
		}
	}
	
	private boolean directCompareTypes(List<String> operations, EObject leftExpression, EObject rightExpression,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo, ImplicitPropertySide side) throws InvalidNameException, InvalidTypeException, DontTypeCheckException, TranslationException {
		if (leftTypeCheckInfo != null && leftTypeCheckInfo.getTypeCheckType() != null && rightTypeCheckInfo != null
				&& rightTypeCheckInfo.getTypeCheckType() != null) {
			if (leftTypeCheckInfo.getTypeCheckType().equals(rightTypeCheckInfo.getTypeCheckType())) {
				if (!leftTypeCheckInfo.getRangeValueType().equals(rightTypeCheckInfo.getRangeValueType())) {
					return false;

				}

				return true;
			}
			if (compatibleTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)) {
				return true;
			}
		}
		return false;
	}

	public boolean compareTypesUsingImpliedPropertiesRecursively(List<String> operations, EObject leftExpression,
			EObject rightExpression, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo, ImplicitPropertySide side) throws InvalidNameException, DontTypeCheckException, InvalidTypeException, TranslationException {
		List<TypeCheckInfo> ltciCompound = leftTypeCheckInfo != null ? leftTypeCheckInfo.getCompoundTypes() : null;
		if(ltciCompound != null){
			for (int i = 0; i < ltciCompound.size(); i++) {
				boolean thisResult = compareTypesUsingImpliedProperties(operations, leftExpression, rightExpression, ltciCompound.get(i), rightTypeCheckInfo, side);
				if (thisResult) {
					return true;
				}
			}
			if (rightTypeCheckInfo.getCompoundTypes() == null) {
				return false;
			}
		}
		
		List<TypeCheckInfo> rtciCompound = rightTypeCheckInfo != null ? rightTypeCheckInfo.getCompoundTypes() : null;
		if(rtciCompound != null){
			for (int j = 0; j < rtciCompound.size(); j++) {
				boolean thisResult = compareTypesUsingImpliedProperties(operations, leftExpression, rightExpression, leftTypeCheckInfo, rtciCompound.get(j), side);
				if (thisResult) {
					return true;
				}
			}
			if (leftTypeCheckInfo.getCompoundTypes() == null) {
				return false;
			}
		}
		
		if(compareTypesUsingImpliedProperties(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo, side)){
			return true;
		}
		
		return false;
	}

	public boolean compareTypesRecursively(List<String> operations, EObject leftExpression, EObject rightExpression,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException, DontTypeCheckException, InvalidTypeException, TranslationException {

		List<TypeCheckInfo> ltciCompound = leftTypeCheckInfo != null ? leftTypeCheckInfo.getCompoundTypes() : null;
		if(ltciCompound != null){
			for (int i = 0; i < ltciCompound.size(); i++) {
				boolean thisResult = compareTypesRecursively(operations, leftExpression, rightExpression, ltciCompound.get(i), rightTypeCheckInfo);
				if (thisResult) {
					return true;
				}
			}
			if (rightTypeCheckInfo.getCompoundTypes() == null) {
				return false;
			}
		}
		
		List<TypeCheckInfo> rtciCompound = rightTypeCheckInfo != null ? rightTypeCheckInfo.getCompoundTypes() : null;
		if(rtciCompound != null){
			for (int j = 0; j < rtciCompound.size(); j++) {
				boolean thisResult = compareTypesRecursively(operations, leftExpression, rightExpression, leftTypeCheckInfo, rtciCompound.get(j));
				if (thisResult) {
					return true;
				}
			}
			if (leftTypeCheckInfo.getCompoundTypes() == null) {
				return false;
			}
		}
		
		ConceptIdentifier leftConceptIdentifier = leftTypeCheckInfo != null ? getConceptIdentifierFromTypeCheckInfo(leftTypeCheckInfo): null;
		ConceptIdentifier rightConceptIdentifier = rightTypeCheckInfo != null ? getConceptIdentifierFromTypeCheckInfo(rightTypeCheckInfo) : null; 
		if ((leftConceptIdentifier != null && leftConceptIdentifier.toString().equals(SadlConstants.CONSTANT_NONE)) || 
				(rightConceptIdentifier != null && rightConceptIdentifier.toString().equals(SadlConstants.CONSTANT_NONE)) ||
				(leftConceptIdentifier != null && leftConceptIdentifier.toString().equals("TODO")) || 
				(rightConceptIdentifier != null && rightConceptIdentifier.toString().equals("TODO"))) {
			// Can't type-check on "None" as it represents that it doesn't exist.
			return true;
		}
		else if (leftConceptIdentifier == null) {
			if (leftTypeCheckInfo != null && leftTypeCheckInfo.getSeverity() != null) {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_COMPARISON.toString(), leftTypeCheckInfo.getSeverity(), leftExpression);
			}
			else {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.TYPE_COMPARISON.toString(), leftExpression);
			}
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
			}
			return false;
		}
		else if(rightConceptIdentifier == null){
			if (rightTypeCheckInfo != null && rightTypeCheckInfo != null && rightTypeCheckInfo.getSeverity() != null) {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_COMPARISON.toString(), rightTypeCheckInfo.getSeverity(), rightExpression);
			}
			else {
				getModelProcessor().addTypeCheckingError(SadlErrorMessages.TYPE_COMPARISON.toString(), rightExpression);
			}
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
			}
			return false;
		}
		else if (!compatibleTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)) {
			if (isConjunctiveLocalRestriction(leftExpression, rightExpression)) {
				return true;
			}
			return false;
		}
		
		return true;
	}
	
	private boolean isConjunctiveLocalRestriction(EObject leftExpression, EObject rightExpression) {
		if (rightExpression instanceof Declaration && rightExpression.eContainer() instanceof BinaryOperation && 
		((BinaryOperation)rightExpression.eContainer()).getOp().equals("is") && rightExpression.eContainer().eContainer() instanceof BinaryOperation &&
		((BinaryOperation)rightExpression.eContainer().eContainer()).getOp().equals("or") && contains(rightExpression.eContainer().eContainer(), leftExpression)) {
			return true;
		}
		return false;
	}
	
	/**
	 * Returns true if eobj1 contains eobj2 else false
	 * 
	 * @param eobj1
	 * @param eobj2
	 * @return
	 */
	protected boolean contains(EObject eobj1, EObject eobj2) {
		if (eobj2.eContainer() != null) {
			if (eobj2.eContainer().equals(eobj1)) {
				return true;
			}
			else {
				return contains(eobj1, eobj2.eContainer());
			}
		}
		return false;
	}
	
	protected ConceptIdentifier getConceptIdentifierFromTypeCheckInfo(TypeCheckInfo tci) throws TranslationException, InvalidNameException, InvalidTypeException {
		
		if (tci.getRangeValueType().equals(RangeValueType.LIST)) {
			ConceptName cn = getListType(tci);
			if (cn != null) {
				return cn;
			}
		}
		if (tci.getExplicitValue() != null) {
			RDFNode val = tci.getExplicitValue();
			if (val.isURIResource()) {
				ConceptName cn = new ConceptName(val.asResource().getURI());
				cn.setType(ConceptType.INDIVIDUAL);
				return cn;
			}
			else if (val.isLiteral()) {
				ConceptName literalConceptName = new ConceptName(val.asLiteral().getDatatype().getURI());
				literalConceptName.setType(ConceptType.RDFDATATYPE);
				return literalConceptName;
			}
		}
		Node tcttype = tci.getTypeCheckType();
		if (tcttype == null) {
			return null;
		}
		if (tcttype instanceof ProxyNode) {
			if (((ProxyNode)tcttype).getProxyFor() instanceof Junction) {
				// convert Junction (or) to SadlUnionClass
				return junctionToSadlUntionClass((Junction)((ProxyNode)tcttype).getProxyFor());
			}
		}
		return getModelProcessor().namedNodeToConceptName((NamedNode)tcttype);
	}

	private ConceptIdentifier junctionToSadlUntionClass(Junction proxyFor) throws TranslationException, InvalidNameException, InvalidTypeException {
		List<ConceptIdentifier> cnlist = new ArrayList<ConceptIdentifier>();
		cnlist = junctionToList(proxyFor, cnlist);
		SadlUnionClass union = new SadlUnionClass(cnlist);
		return union;
	}

	private List<ConceptIdentifier> junctionToList(Junction jct, List<ConceptIdentifier> cnlist) throws TranslationException, InvalidNameException, InvalidTypeException {
		Object lhs = jct.getLhs();
		Object rhs = jct.getRhs();
		if (lhs instanceof NamedNode) {
			cnlist.add(getModelProcessor().namedNodeToConceptName((NamedNode) lhs));
		}
		else if (lhs instanceof Junction) {
			cnlist = junctionToList((Junction)lhs, cnlist);
		}
		else {
			throw new TranslationException("unhandled type (" + lhs.getClass().getCanonicalName() + ") in junctionToList");
		}
		if (rhs instanceof NamedNode) {
			cnlist.add(getModelProcessor().namedNodeToConceptName((NamedNode) rhs));
		}
		else if (rhs instanceof Junction) {
			cnlist = junctionToList((Junction)rhs, cnlist);
		}
		else {
			throw new TranslationException("unhandled type (" + rhs.getClass().getCanonicalName() + ") in junctionToList");
		}
		return cnlist;
	}

	private boolean compareTypesUsingImpliedProperties(List<String> operations, EObject leftExpression,
			EObject rightExpression, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo, ImplicitPropertySide side) throws InvalidNameException, DontTypeCheckException, InvalidTypeException, TranslationException {
		if (side.equals(ImplicitPropertySide.BOTH)) {
			throw new TranslationException("This method does not handle looking for implied properties on BOTH sides");
		}
		// There must be a mismatch or we wouldn't have gotten to here, so we know that to pass (return true) we 
		//	need to find an implied property that fixes the mismatch on one side or the other.
		// If the side is explicitly specified, then we won't check the other side. Otherwise we will 
		//	check both sides and if we find a match we should return true.
		boolean leftSideOK = !side.equals(ImplicitPropertySide.RIGHT) ? false : true;
		boolean rightSideOK = !side.equals(ImplicitPropertySide.LEFT) ? false : true;
		
		String opstr = (operations != null && operations.size() > 0) ? operations.get(0) : null;
		if (leftTypeCheckInfo != null && !side.equals(ImplicitPropertySide.RIGHT) && leftTypeCheckInfo.getImplicitProperties() != null) {
			Iterator<ConceptName> litr = leftTypeCheckInfo.getImplicitProperties().iterator();
			while (litr.hasNext()) {
				ConceptName cn = litr.next();
				Property prop = theJenaModel.getProperty(cn.getUri());
				if (prop.canAs(ObjectProperty.class)) {
					cn.setType(ConceptType.OBJECTPROPERTY);
				}
				else if (prop.canAs(DatatypeProperty.class)) {
					cn.setType(ConceptType.DATATYPEPROPERTY);
				}
				else {
					cn.setType(ConceptType.RDFPROPERTY);
				}
				TypeCheckInfo newltci = getTypeInfoFromRange(cn, prop, leftExpression);
				if (compareTypesRecursively(operations, leftExpression, rightExpression, newltci, rightTypeCheckInfo)) {
					getModelProcessor().addInfo("Implied property '" + getModelProcessor().conceptIdentifierToString(cn) + "' used (left side" + (opstr != null ? (" of '" + opstr + "'") : "") + ") to pass type check", leftExpression);
					addImpliedPropertiesUsed(leftExpression, prop);
					leftSideOK = true;
					if (!side.equals(ImplicitPropertySide.BOTH)) {
						rightSideOK = true;
					}
					break;
				}
			}
		}
		if (rightTypeCheckInfo != null && !side.equals(ImplicitPropertySide.LEFT) && rightTypeCheckInfo.getImplicitProperties() != null) {
			Iterator<ConceptName> ritr = rightTypeCheckInfo.getImplicitProperties().iterator();
			while (ritr.hasNext()) {
				ConceptName cn = ritr.next();
				Property prop = theJenaModel.getProperty(cn.getUri());
				if (prop.canAs(ObjectProperty.class)) {
					cn.setType(ConceptType.OBJECTPROPERTY);
				}
				else if (prop.canAs(DatatypeProperty.class)) {
					cn.setType(ConceptType.DATATYPEPROPERTY);
				}
				else {
					cn.setType(ConceptType.RDFPROPERTY);
				}
				TypeCheckInfo newrtci = getTypeInfoFromRange(cn, prop, rightExpression);
				if (compareTypesRecursively(operations, leftExpression, rightExpression, leftTypeCheckInfo, newrtci)) {
					getModelProcessor().addInfo("Implied property '" + getModelProcessor().conceptIdentifierToString(cn) + "' used (right side" + (opstr != null ? (" of '" + opstr + "'") : "") + ") to pass type check", rightExpression);
					addImpliedPropertiesUsed(rightExpression, prop);
					rightSideOK = true;
					if (!side.equals(ImplicitPropertySide.BOTH)) {
						leftSideOK = true;
					}
					break;
				}
			}
		}
		return leftSideOK && rightSideOK;
	}

	private boolean useImpliedPropertyToMatchNumericOperator(EObject expr, TypeCheckInfo tci, String sideId) throws DontTypeCheckException, InvalidTypeException, TranslationException, InvalidNameException {
		if (tci != null && tci.getImplicitProperties() != null) {
			Iterator<ConceptName> ipitr = tci.getImplicitProperties().iterator();
			while (ipitr.hasNext()) {
				ConceptName cn = ipitr.next();
				Property prop = theJenaModel.getProperty(cn.getUri());
				if (prop.canAs(ObjectProperty.class)) {
					cn.setType(ConceptType.OBJECTPROPERTY);
				}
				else if (prop.canAs(DatatypeProperty.class)) {
					cn.setType(ConceptType.DATATYPEPROPERTY);
				}
				else {
					cn.setType(ConceptType.RDFPROPERTY);
				}
				TypeCheckInfo newltci = getTypeInfoFromRange(cn, prop, expr);
				if (isNumeric(newltci)) {
					getModelProcessor().addInfo("Implied property '" + getModelProcessor().conceptIdentifierToString(cn) + "' used (" + sideId + ") to pass type check", expr);
					addImpliedPropertiesUsed(expr, prop);
					return true;
				}
			}
		}
		return false;
	}

	private boolean compatibleTypes(List<String> operations, EObject leftExpression, EObject rightExpression,
									TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException, InvalidTypeException, DontTypeCheckException, TranslationException{
		boolean lCond1 = leftTypeCheckInfo.getRangeValueType() == null && rightTypeCheckInfo.getRangeValueType() != null && !rightTypeCheckInfo.getRangeValueType().equals(RangeValueType.CLASS_OR_DT);		
		boolean lCond2 = leftTypeCheckInfo.getRangeValueType() != null && !leftTypeCheckInfo.getRangeValueType().equals(RangeValueType.CLASS_OR_DT) && rightTypeCheckInfo.getRangeValueType() == null; 
		boolean lCond3 = leftTypeCheckInfo.getRangeValueType() != null && rightTypeCheckInfo.getRangeValueType() != null && !(leftTypeCheckInfo.getRangeValueType().equals(rightTypeCheckInfo.getRangeValueType()));
		if ( lCond1 || lCond2 || lCond3) {
			if (!isQualifyingListOperation(operations, leftTypeCheckInfo, rightTypeCheckInfo)) {
				if (isCompatibleListTypes(operations, leftTypeCheckInfo, rightTypeCheckInfo)) {
					return true;
				}
				return false;
			}
		}
		
		boolean lCond4 = leftTypeCheckInfo.getRangeValueType() != null && leftTypeCheckInfo.getRangeValueType().equals(RangeValueType.LIST);
		boolean lCond5 = rightTypeCheckInfo.getRangeValueType() != null && rightTypeCheckInfo.getRangeValueType().equals(RangeValueType.LIST);
		
		if( lCond4 && lCond5) {
			
			boolean lCond6 = leftTypeCheckInfo.getTypeCheckType() != null && rightTypeCheckInfo.getTypeCheckType() != null;
			boolean lCond7 =  leftTypeCheckInfo.getTypeCheckType().toFullyQualifiedString().equals(rightTypeCheckInfo.getTypeCheckType().toFullyQualifiedString());
			if( lCond6 && lCond7){
				ConceptName leftConceptName = (ConceptName)getConceptIdentifierFromTypeCheckInfo(leftTypeCheckInfo);
				ConceptName rightConceptName = (ConceptName)getConceptIdentifierFromTypeCheckInfo(rightTypeCheckInfo);
				if(leftConceptName.getType().equals(rightConceptName.getType())){
					return leftTypeCheckInfo.getTypeCheckType().equals(rightTypeCheckInfo.getTypeCheckType());
				}
			}
		}
		
		if (getModelProcessor().isDeclaration(leftExpression) && getModelProcessor().isDeclaration(rightExpression) && 
				leftTypeCheckInfo.getTypeToExprRelationship().equals("self") && 
				rightTypeCheckInfo.getTypeToExprRelationship().equals("self")) {
			// this is a test for class membership to be resolved by the reasoner
			return true;
		}
		
		ConceptIdentifier leftConceptIdentifier = getConceptIdentifierFromTypeCheckInfo(leftTypeCheckInfo);
		ConceptIdentifier rightConceptIdentifier = getConceptIdentifierFromTypeCheckInfo(rightTypeCheckInfo);	
		if (leftConceptIdentifier == null || rightConceptIdentifier == null) {
			return false;
		}
		
		if (leftConceptIdentifier instanceof ConceptName && rightConceptIdentifier instanceof ConceptName) {
			ConceptName leftCName = (ConceptName) leftConceptIdentifier;
			if (leftCName.getType().equals(ConceptType.ONTCLASS)
					&& leftTypeCheckInfo.getExpressionType() instanceof ConceptName
					&& rightTypeCheckInfo.getExpressionType() instanceof ConceptName) {

				if (((ConceptName) rightTypeCheckInfo.getExpressionType()).getType().equals(ConceptType.INDIVIDUAL)
						&& ((ConceptName) leftTypeCheckInfo.getExpressionType()).getType()
								.equals(ConceptType.RDFPROPERTY)) {

					ConceptName rexpr = (ConceptName) rightTypeCheckInfo.getExpressionType();

					if (!instanceBelongsToClass(theJenaModel.getIndividual(rexpr.getUri()),
							theJenaModel.getOntClass(leftCName.getUri()))) {
						return false;
					}
				}

			}
			if (leftCName.getNamespace().equals(XSD.getURI())) {
				if (((ConceptName)rightConceptIdentifier).getNamespace().equals(XSD.getURI())) {
					if (xsdTypeToXtextType(leftCName).equals(xsdTypeToXtextType((ConceptName)rightConceptIdentifier))) {
						return true;
					}
				}
			}
		}
				
		List<ConceptIdentifier> leftCIs = null;
		if (leftConceptIdentifier instanceof SadlUnionClass) {
			leftCIs = ((SadlUnionClass)leftConceptIdentifier).getClasses();
		}
		else {
			leftCIs = new ArrayList<ConceptIdentifier>(1);
			leftCIs.add(leftConceptIdentifier);
		}
		List<ConceptIdentifier> rightCIs = null;
		if (rightConceptIdentifier instanceof SadlUnionClass) {
			rightCIs = ((SadlUnionClass)rightConceptIdentifier).getClasses();
		}
		else {
			rightCIs = new ArrayList<ConceptIdentifier>(1);
			rightCIs.add(rightConceptIdentifier);
		}
		for (int leftIdx = 0; leftIdx < leftCIs.size(); leftIdx++) {
			for (int rightIdx = 0; rightIdx < rightCIs.size(); rightIdx++) {
				leftConceptIdentifier = leftCIs.get(leftIdx);
				rightConceptIdentifier = rightCIs.get(rightIdx);
				if (leftConceptIdentifier instanceof ConceptName && rightConceptIdentifier instanceof ConceptName) {
					ConceptName leftConceptName = (ConceptName) leftConceptIdentifier;
					ConceptName rightConceptName = (ConceptName) rightConceptIdentifier;
					
					if (getModelProcessor().isNumericOperator(operations) && ((!isNumeric(leftTypeCheckInfo) && !isNumericWithImpliedProperty(leftTypeCheckInfo,(Expression)leftExpression))||
																			  (!isNumeric(rightTypeCheckInfo) && !isNumericWithImpliedProperty(rightTypeCheckInfo,(Expression)rightExpression)))) {
						if (!leftConceptName.getUri().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI) || 
								!rightConceptName.getUri().equals(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI)) {
							return false;
						}
					}
					if (leftConceptName.equals(rightConceptName) && !getModelProcessor().isNumericOperator(operations)) {
						return true;
					}
					else if ((getModelProcessor().isNumericOperator(operations) || getModelProcessor().canBeNumericOperator(operations)) && 
						(getModelProcessor().isNumericType(leftConceptName) && getModelProcessor().isNumericType(rightConceptName))) {
						return true;
					}
					else if (leftConceptName.getType() == null || rightConceptName.getType() == null) {
						if (rightConceptName.getType() == null && leftConceptName.getType() == null) {
							return true;
						}
//						else {
//							return false;
//						}
					}
					else if (leftConceptName.getType().equals(ConceptType.RDFDATATYPE) &&
							  rightConceptName.getType().equals(ConceptType.RDFDATATYPE)) {
						if(leftConceptName.getUri().equals(rightConceptName.getUri())){
							return true;
						}
						else if (isInteger(leftConceptName) && isInteger(rightConceptName)) {
							return true;
						}
						else if(isDecimal(leftConceptName) && isInteger(rightConceptName)){
							return true;
						}
						else if(isInteger(leftConceptName) && isDecimal(rightConceptName)){
							// TODO does this need to be restricted to certain operators? This should work for numerical comparison...
							return true;
						}
						else if(isDecimal(leftConceptName) && isDecimal(rightConceptName)){
							return true;
						}
						else if (rightConceptName.getUri().equals(XSD.xstring.getURI()) && 
								(leftConceptName.getUri().equals(XSD.dateTime.getURI()) || leftConceptName.getUri().equals(XSD.date.getURI()))) {
							return true;
						}
						else {
							// maybe one or both is a user-defined datatype
							
						}
					}
					else if (leftConceptName.getType().equals(ConceptType.DATATYPEPROPERTY) &&
							  rightConceptName.getType().equals(ConceptType.DATATYPEPROPERTY)) {
						if(leftConceptName.getUri().equals(rightConceptName.getUri())){
							return true;
						}
					}
					else if(leftConceptName.getType().equals(ConceptType.OBJECTPROPERTY) &&
							 rightConceptName.getType().equals(ConceptType.OBJECTPROPERTY)){
						if(leftConceptName.getUri().equals(rightConceptName.getUri())){
							return true;
						}
					}
					
					else if (leftConceptName.getType().equals(ConceptType.ONTCLASS) &&
							rightConceptName.getType().equals(ConceptType.ONTCLASS)) {
						if (partOfTest(leftExpression, rightExpression)) {
							// if we're in a test we don't want to type check as it may fail when not using the inferred model.
							return true;
						}
						//How do we determine if either is a sub/super class of the other?
						if(leftConceptName.getUri().equals(rightConceptName.getUri())){
							return true;
						}
						
						// these next two ifs are a little loose, but not clear how to determine which way the comparison should be? May need tightening... AWC 5/11/2016
						try {
//							if (rightTypeCheckInfo.getExpressionType() instanceof ConceptName && ((ConceptName)rightTypeCheckInfo.getExpressionType()).getType().equals(ConceptType.INDIVIDUAL)) {
//								// here we can do a tighter check
//								if (!getModelProcessor().instanceBelongsToClass(theJenaModel, 
//										theJenaModel.getOntResource(((ConceptName)rightTypeCheckInfo.getExpressionType()).getUri()),
//										theJenaModel.getOntResource(leftConceptName.getUri()))) {
//									return false;
//								}
//							}
							OntClass subcls = theJenaModel.getOntClass(leftConceptName.getUri());
							if (subcls == null) {
								getModelProcessor().addTypeCheckingError("Concept '" + leftConceptName.getUri() + "' not found", leftExpression);
							}
							OntResource supercls = theJenaModel.getOntResource(rightConceptName.getUri());
							if (supercls == null) {
								getModelProcessor().addTypeCheckingError("Concept '" + rightConceptName.getUri() + "' not found", rightExpression);
							}
							if (subcls != null && supercls != null) {
								if (SadlUtils.classIsSubclassOf(subcls, supercls, true, null)) {
									if (getModelProcessor().isAssignment(leftExpression.eContainer())) {
										String wmsg = createSuperClassOnRightWarning(leftExpression.eContainer(), leftTypeCheckInfo, rightTypeCheckInfo, operations.get(0));
										getModelProcessor().addWarning(wmsg, leftExpression.eContainer());
									}
									return true;
								}
								if (SadlUtils.classIsSubclassOf(supercls.as(OntClass.class), subcls, true, null)) {
									return true;
								}
							}
		// TODO handle equivalent classes.					
		//					StmtIterator sitr = theJenaModel.listStatements(theJenaModel.getOntClass(rightConceptName.getUri()), OWL.equivalentClass, (RDFNode)null);
		//					if (sitr.hasNext()) {
		//						System.out.println(sitr.nextStatement().toString());
		//					}
		//					else {
		//						theJenaModel.write(System.out, "N-TRIPLE");
		//					}
						} catch (CircularDependencyException e) {
							e.printStackTrace();
//						} catch (JenaProcessorException e) {
//							e.printStackTrace();
						}
					}
					//Case: a class is being type-checked against a decomposition which is a sub/super-class of the former or
					// a decomposition variable is being type-checked against a class
					else if ((leftConceptName.getType().equals(ConceptType.ONTCLASS) &&
							rightConceptName.getType().equals(ConceptType.VARIABLE)) || 
							(leftConceptName.getType().equals(ConceptType.VARIABLE) &&
							rightConceptName.getType().equals(ConceptType.ONTCLASS))) {
						try {
							if (SadlUtils.classIsSubclassOf(theJenaModel.getOntClass(leftConceptName.getUri()), theJenaModel.getOntResource(rightConceptName.getUri()), true, null)) {
								return true;
							}
							if (SadlUtils.classIsSubclassOf(theJenaModel.getOntClass(rightConceptName.getUri()), theJenaModel.getOntResource(leftConceptName.getUri()), true, null)) {
								return true;
							}
						} catch (CircularDependencyException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
					else if ((leftConceptName.getType().equals(ConceptType.INDIVIDUAL) && rightConceptName.getType().equals(ConceptType.ONTCLASS))) {
						if (instanceBelongsToClass(theJenaModel.getIndividual(leftConceptName.getUri()), theJenaModel.getOntClass(rightConceptName.getUri()))) {
							return true;
						}
					}
					
					else if ((leftConceptName.getType().equals(ConceptType.ONTCLASS) && rightConceptName.getType().equals(ConceptType.INDIVIDUAL))){
						if (instanceBelongsToClass(theJenaModel.getIndividual(rightConceptName.getUri()), theJenaModel.getOntClass(leftConceptName.getUri()))) {
							return true;
						}
					}
									
					else if ((leftConceptName.getType().equals(ConceptType.INDIVIDUAL) && rightConceptName.getType().equals(ConceptType.INDIVIDUAL))){
						// TODO Is this the right way to compare for two individuals? 
						if (instancesHaveCommonType(theJenaModel.getIndividual(leftConceptName.getUri()), theJenaModel.getIndividual(rightConceptName.getUri()))) {
							return true;
						}
					}
					else if (leftConceptName.getType().equals(ConceptType.VARIABLE) && getModelProcessor().isDeclaration(rightExpression)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	
	private String xsdTypeToXtextType(ConceptName cname) throws InvalidNameException {
		if (cname.getUri().equals(XSD.anyURI.getURI())) {
			return XSD.xstring.getURI();
		}
		return cname.getUri();
	}

	private boolean isCompatibleListTypes(List<String> operations, TypeCheckInfo leftTypeCheckInfo,
			TypeCheckInfo rightTypeCheckInfo) throws TranslationException, InvalidNameException, InvalidTypeException {
		if(leftTypeCheckInfo.getRangeValueType() != null && rightTypeCheckInfo.getRangeValueType() != null &&
				leftTypeCheckInfo.getRangeValueType().equals(RangeValueType.LIST) && rightTypeCheckInfo.getRangeValueType().equals(RangeValueType.CLASS_OR_DT)){
			return getConceptIdentifierFromTypeCheckInfo(rightTypeCheckInfo).equals(getConceptIdentifierFromTypeCheckInfo(leftTypeCheckInfo));
		}else {
			return false;
		}
	}

	private ConceptName getListType(TypeCheckInfo tci) throws TranslationException, InvalidTypeException {
		Node tct = tci.getTypeCheckType();
		if (tct != null) {
			if (tct instanceof NamedNode) {
				try {
					OntResource cls = theJenaModel.getOntResource(((NamedNode)tct).toFullyQualifiedString());
					if (tci.getTypeToExprRelationship().equals(RANGE) || tci.getTypeToExprRelationship().equals(RESTRICTED_TO)) {
						if (cls.isURIResource()) {
//							return new ConceptName(cls.getURI());
							return getModelProcessor().namedNodeToConceptName((NamedNode) tct);
						}
					}
					if (cls != null && cls.canAs(OntClass.class)){
						ConceptName listcn = getListClassType(cls);
						if (listcn != null) {
							return listcn;
						}
					}
				} catch (InvalidNameException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		return null;
	}

	protected ConceptName getListClassType(OntResource cls) {
		ExtendedIterator<OntClass> eitr = cls.as(OntClass.class).listSuperClasses();
		while (eitr.hasNext()) {
			OntClass supercls = eitr.next();
			if (supercls.isRestriction() && supercls.hasProperty(OWL.onProperty, theJenaModel.getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
				RDFNode avf = supercls.getPropertyValue(OWL.allValuesFrom);
				if (avf.canAs(Resource.class)) {
					Resource r = avf.as(Resource.class);
					if (r.isURIResource()) {
						eitr.close();
						return new ConceptName(r.getURI());
					}
				}
			}
		}
		return null;
	}

	private boolean partOfTest(EObject leftExpression, EObject rightExpression) {
		if (checkForContainer(leftExpression, TestStatementImpl.class)) {
			return true;
		}
		return checkForContainer(rightExpression, TestStatement.class);
	}
	
	private <X extends EObject> boolean checkForContainer(EObject expr, Class<X> t ) {
		if (expr.eContainer() == null) {
			return false;
		}
		if (expr.eContainer().getClass().equals(t)) {
			return true;
		}
		return checkForContainer(expr.eContainer(), t);
	}

	protected boolean isQualifyingListOperation(List<String> operations, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidTypeException {
		if ((operations.contains("contain") || operations.contains("contains")) && leftTypeCheckInfo != null && 
				leftTypeCheckInfo.getRangeValueType().equals(RangeValueType.LIST)) {
			return true;
		}
		if (operations.contains("unique") && operations.contains("in") && rightTypeCheckInfo != null &&
				rightTypeCheckInfo.getRangeValueType().equals(RangeValueType.LIST)) {
			return true;
		}
		return false;
	}
	
	private boolean instancesHaveCommonType(Individual individualL, Individual individualR) {
		ExtendedIterator<Resource> lcitr = individualL.listRDFTypes(true);
		ExtendedIterator<Resource> rcitr = individualR.listRDFTypes(true);
		while (lcitr.hasNext()) {
			Resource lr = lcitr.next();
			while (rcitr.hasNext()) {
				Resource rr = rcitr.next();
				if (lr.equals(rr)) {
					lcitr.close();
					rcitr.close();
					return true;
				}
			}
		}
		return false;
	}

	private boolean instanceBelongsToClass(Individual individual, OntClass ontClass) {
		ExtendedIterator<Resource> citr = individual.listRDFTypes(false);
		while (citr.hasNext()) {
			Resource cls = citr.next();
			if (cls.isURIResource() && cls.getURI().equals(ontClass.getURI())) {
				return true;
			}
			else {
				// this may be a union or intersection class; how should this be handled?
				// TODO
			}
		}
		return false;
	}
	
	private boolean isInteger(ConceptIdentifier type) throws InvalidNameException {
		if (type instanceof ConceptName) {
			String uri = ((ConceptName)type).getUri();
			if (uri.equals(XSD.integer.getURI())) {
				return true;
			}
			else if (uri.equals(XSD.xint.getURI())) {
				return true;
			}
			else if (uri.equals(XSD.xlong.getURI())) {
				return true;
			}
		}
		return false;
	}

	private boolean isDecimal(ConceptIdentifier type) throws InvalidNameException {
		if (type instanceof ConceptName) {
			String uri = ((ConceptName)type).getUri();
			if (uri.equals(XSD.xfloat.getURI()) || uri.equals(XSD.xdouble.getURI()) || uri.equals(XSD.decimal.getURI())) {
				return true;
			}
		}
		return false;
	}

	private EObject getDefaultContext() {
		return defaultContext;
	}

	private void setDefaultContext(EObject defaultContext) {
		this.defaultContext = defaultContext;
	}

	public Map<EObject, Property> getImpliedPropertiesUsed() {
		return impliedPropertiesUsed;
	}
	
	public boolean clearImpliedPropertiesUsed() {
		if (impliedPropertiesUsed != null) {
			impliedPropertiesUsed.clear();
			return true;
		}
		return false;
	}

	public boolean removeImpliedPropertyUsed(EObject obj) {
		if (impliedPropertiesUsed != null) {
			if (impliedPropertiesUsed.containsKey(obj)) {
				impliedPropertiesUsed.remove(obj);
				return true;
			}
		}
		return false;
	}

	protected boolean addImpliedPropertiesUsed(EObject context, Property impliedPropertyUsed) {
		if (impliedPropertiesUsed == null) {
			impliedPropertiesUsed = new HashMap<EObject, Property>();
			impliedPropertiesUsed.put(context, impliedPropertyUsed);
			return true;
		}
		else {
			if (!impliedPropertiesUsed.containsKey(context)) {
				impliedPropertiesUsed.put(context, impliedPropertyUsed);
				return true;
			}
		}
		return false;
	}

	public void checkPropertyDomain(OntModel ontModel, Expression subject, SadlResource predicate, Expression target, boolean propOfSubjectCheck) throws InvalidTypeException {
		OntConceptType ptype = null;
		try {
			ptype = declarationExtensions.getOntConceptType(predicate);
		} catch (CircularDefinitionException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			getModelProcessor().addTypeCheckingError("Unable to get type. This should not happen. Please report.", predicate);
			return;
		}
		boolean checkDomain = true;
		if (ptype.equals(OntConceptType.VARIABLE) && declarationExtensions.getDeclaration(predicate).equals(predicate)) {
			getModelProcessor().addTypeCheckingError(SadlErrorMessages.VARIABLE_INSTEAD_OF_PROP2.get(declarationExtensions.getConcreteName(predicate)), predicate);
			checkDomain = false;
		}
		if (subject instanceof SadlResource) {
			org.eclipse.emf.ecore.resource.Resource rsrc = subject.eResource();
			if (rsrc != null) {
				if (ontModel != null) {
					try {
						OntConceptType stype = declarationExtensions.getOntConceptType((SadlResource)subject);
						String subjuri = declarationExtensions.getConceptUri((SadlResource) subject);
						String preduri = declarationExtensions.getConceptUri(predicate);
						NamedNode subjnn = new NamedNode(subjuri);
						subjnn.setNodeType(NodeType.ClassNode);
						NamedNode prednn = new NamedNode(preduri);
						prednn.setNodeType(getModelProcessor().ontConceptTypeToNodeType(ptype));
						List<Node> lrs = getApplicableLocalRestrictions(subjnn, prednn);
						
						OntResource subj = null;
						String varName = null;
						if (stype.equals(OntConceptType.VARIABLE)) {
							TypeCheckInfo stci;
							// for now don't do any checking--may be able to do so later with variable definitions
							try {
								varName = declarationExtensions.getConcreteName((SadlResource)subject);
								stci = getType(subject);
								//It's possible that there are local restrictions
//								if (stci != null) {
//									TypeCheckInfo lr = getApplicableLocalRestriction(generateLocalRestrictionKey(varName));
//									if (lr != null && lr.getTypeCheckType() != null) {
//										stci = lr;
//									}
//								}
								if (stci != null && stci.getTypeCheckType() != null) {
									subj = ontModel.getOntResource(stci.getTypeCheckType().toFullyQualifiedString());
									if (subj != null) {
										Property prop = ontModel.getProperty(preduri);
										if (prop != null && checkDomain) {
											checkPropertyDomain(ontModel, subj, prop, target, propOfSubjectCheck, varName);
										}
									}
								}
							} catch (InvalidNameException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							} catch (TranslationException e) {
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
								
							} catch (CircularDependencyException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							} catch (PropertyWithoutRangeException e) {

							}
						}
						else if (stype.equals(OntConceptType.CLASS_PROPERTY)) {
							OntProperty propsubj = ontModel.getOntProperty(declarationExtensions.getConceptUri((SadlResource)subject));
							if (propsubj != null) {
								Property prop = ontModel.getProperty(preduri);
								if (prop != null && checkDomain) {
									checkPropertyDomain(ontModel, propsubj, prop, target, propOfSubjectCheck, varName);
								}
							}
						}
						else {
							subj = ontModel.getOntResource(declarationExtensions.getConceptUri((SadlResource)subject));
							if (subj != null) {
								if (preduri == null) {
									getModelProcessor().addTypeCheckingError("Unable to resolve name", predicate);
								}
								else {
									Property prop = ontModel.getProperty(preduri);
									if (prop != null && checkDomain) {
										checkPropertyDomain(ontModel, subj, prop, target, propOfSubjectCheck, varName);
									}
								}
							}
						}
					} catch (CircularDefinitionException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IOException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					} catch (PrefixNotFoundException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					} catch (InvalidNameException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					} catch (TranslationException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					} catch (ConfigurationException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
				}
			}
		}
	}

	public void checkPropertyDomain(OntModel ontModel, OntResource subj, Property prop, Expression target, boolean propOfSubjectCheck, String varName) throws InvalidTypeException {
		if(subj == null || prop == null || prop.canAs(AnnotationProperty.class)){
			return;
		}
		StmtIterator stmtitr = ontModel.listStatements(prop, RDFS.domain, (RDFNode)null);
		boolean matchFound = false;
		while (stmtitr.hasNext()) {
			RDFNode obj = stmtitr.nextStatement().getObject();
			if (obj.isResource()) {
				matchFound = checkForPropertyDomainMatch(subj, prop, obj.asResource());
			}
			if (matchFound) {
				stmtitr.close();
				return;
			}
		}
		//Check for super properties
		if (prop.canAs(OntProperty.class)) {
			ExtendedIterator<? extends OntProperty> spropitr = prop.as(OntProperty.class).listSuperProperties();
			while (spropitr.hasNext()) {
				OntProperty sprop = spropitr.next();
				stmtitr = ontModel.listStatements(sprop, RDFS.domain, (RDFNode)null);
				while (stmtitr.hasNext()) {
					RDFNode obj = stmtitr.nextStatement().getObject();
					if (obj.isResource()) {
						matchFound = checkForPropertyDomainMatch(subj, prop, obj.asResource());
					}
					if (matchFound) {
						stmtitr.close();
						break;
					}
				}
			}
		}
		if (subj != null && !matchFound) {
			if (varName != null) {
				if(propOfSubjectCheck){
					getModelProcessor().addTypeCheckingError(SadlErrorMessages.VARIABLE_NOT_IN_DOMAIN_OF_PROPERTY.get(varName, subj.getURI(),prop.getURI()), target);
				}else{
					getModelProcessor().addWarning(SadlErrorMessages.VARIABLE_NOT_IN_DOMAIN_OF_PROPERTY.get(varName, getModelProcessor().rdfNodeToString(subj),getModelProcessor().rdfNodeToString(prop)), target);
				}
			}
			else {
				String msg;
				if (subj instanceof OntProperty) {
					msg = SadlErrorMessages.RANGE_OF_NOT_IN_DOMAIN_OF.get(getModelProcessor().rdfNodeToString(subj),getModelProcessor().rdfNodeToString(prop));
				}
				else {
					msg = SadlErrorMessages.SUBJECT_NOT_IN_DOMAIN_OF_PROPERTY.get(getModelProcessor().rdfNodeToString(subj),getModelProcessor().rdfNodeToString(prop));
				}
				if(propOfSubjectCheck){
					getModelProcessor().addTypeCheckingError(msg, target);
				}else{
					getModelProcessor().addWarning(msg, target);
				}
			}
		}
	}
	
	private boolean checkForPropertyDomainMatch(Resource subj, Property prop, Resource obj) throws InvalidTypeException {
		if (obj.isResource()) {
			if (obj.canAs(UnionClass.class)){
				List<OntResource> uclsMembers = getModelProcessor().getOntResourcesInUnionClass(theJenaModel, obj.as(UnionClass.class));
				if (uclsMembers.contains(subj)) {
					return true;
				}
				if (subj.canAs(OntClass.class)){ 
					for (int i = 0; i < uclsMembers.size(); i++) {
						OntResource member = uclsMembers.get(i);
						try {
							if (SadlUtils.classIsSubclassOf(subj.as(OntClass.class), member, true, null)) {
								return true;
							}
						} catch (CircularDependencyException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
				}
				else if (subj.canAs(Individual.class)){
					ExtendedIterator<Resource> titr = subj.as(Individual.class).listRDFTypes(false);
					while (titr.hasNext()) {
						Resource type = titr.next();
						if (uclsMembers.contains(type)) {
							titr.close();
							return true;
						}
					}
				}
			}
			else if (subj != null && obj.isURIResource() && obj.asResource().getURI().equals(subj.getURI())) {
				return true;	
			}
			else {
				if (subj.canAs(OntClass.class)){
					try {
						if ( SadlUtils.classIsSubclassOf(subj.as(OntClass.class), obj.as(OntResource.class),true, null)) {
							return true;
						}
//						if (obj.canAs(OntClass.class) &&  SadlUtils.classIsSuperClassOf(obj.as(OntClass.class), subj.as(OntClass.class))) {
//							return true;
//						}
					} catch (CircularDependencyException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				else if (subj instanceof Property || subj.canAs(OntProperty.class)) {
					// this is a property chain missing an intermediate class or instance: get the range of the property
					StmtIterator stmtitr = getModelProcessor().getTheJenaModel().listStatements(subj, RDFS.range, (RDFNode)null);
					boolean matchFound = false;
					while (stmtitr.hasNext()) {
						RDFNode missingSubject = stmtitr.nextStatement().getObject();
						if (missingSubject.isResource() && missingSubject.canAs(OntClass.class)) {
							try {
								if ( SadlUtils.classIsSubclassOf(missingSubject.as(OntClass.class), obj.as(OntResource.class),true, null)) {
									stmtitr.close();
									return true;
								}
							} catch (CircularDependencyException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
						}
					}
				}
				else if (subj.canAs(Individual.class)){
					Individual inst = subj.as(Individual.class);
					ExtendedIterator<Resource> itr = inst.listRDFTypes(false);
					while (itr.hasNext()) {
						Resource cls = itr.next();
						boolean match = checkForPropertyDomainMatch(cls, prop, obj);
						if (match) {
							itr.close();
							return true;
						}
					}
					
				}
			}
		}
		return false;
	}

	public List<ConceptName> getBinaryOpLeftImplliedProperties() {
		return binaryOpLeftImpliedProperties;
	}

	protected void setBinaryOpLeftImpliedProperties(List<ConceptName> binaryLeftTypeCheckInfo) {
		this.binaryOpLeftImpliedProperties = binaryLeftTypeCheckInfo;
	}

	public List<ConceptName> getBinaryOpRightImpliedProperties() {
		return binaryOpRightImpliedProperties;
	}

	protected void setBinaryOpRightImpliedProperties(List<ConceptName> binaryRightTypeCheckInfo) {
		this.binaryOpRightImpliedProperties = binaryRightTypeCheckInfo;
	}

	/**
	 * Clear old values of left and right impliedProperties 
	 */
	public void clearImpliedProperties() {
		setBinaryOpLeftImpliedProperties(null);
		setBinaryOpRightImpliedProperties(null);
	}

	protected JenaBasedSadlModelProcessor getModelProcessor() throws InvalidTypeException {
		return modelProcessor;
	}

	protected void setModelProcessor(JenaBasedSadlModelProcessor modelProcessor) {
		this.modelProcessor = modelProcessor;
	}

	public void resetValidatorState(SadlModelElement element) {
		if (impliedPropertiesUsed != null) {
			impliedPropertiesUsed.clear();
		}
		if (binaryOpLeftImpliedProperties != null) {
			binaryOpLeftImpliedProperties.clear();
		}
		if (binaryOpRightImpliedProperties != null) {
			binaryOpRightImpliedProperties.clear();
		}
		if (expressionsTypeCheckCache != null) {
			expressionsTypeCheckCache.clear();
		}
	}

	private Sublist getSublistContainer(EObject expression) {
		if (expression instanceof Sublist) {
			return (Sublist) expression;
		}
		if (expression.eContainer() != null) {
			return getSublistContainer(expression.eContainer());
		}
		return null;
	}

	public boolean checkPropertyValueInRange(OntModel theJenaModel, Expression subj, SadlResource pred, EObject val, StringBuilder errorMessageBuilder) throws CircularDefinitionException, DontTypeCheckException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		TypeCheckInfo predType = getType(pred);
		if (predType == null) {
			return true;	// this is an error which will have already been marked in call to getType
		}
		if (val == null && isInQuery(pred)) {
			return true;	// this is OK
		}
		TypeCheckInfo valType = getType(val);
		List<String> operations = Arrays.asList("is");
		if (declarationExtensions.getOntConceptType(pred).equals(OntConceptType.DATATYPE_PROPERTY)) {
			if (!checkNumericRangeLimits("=", predType, valType)) {
				return true;  // return true so as to not generate error at higher level; failure has already created marker
			}
		}
		if (compareTypes(operations , pred, val, predType, valType, ImplicitPropertySide.NONE)) {
			return true;
		}
//		// TODO check for restrictions on subj class
//		OntConceptType predtype = declarationExtensions.getOntConceptType(pred);
//		String preduri = declarationExtensions.getConceptUri(pred);
//		Property prop = theJenaModel.getProperty(preduri);
//		if (prop == null) {
//			throw new JenaProcessorException("Unable to find property '" + preduri + "' in model.");
//		}
//		RDFNode val;
//		if (value instanceof SadlResource) {
//			OntConceptType srType = declarationExtensions.getOntConceptType((SadlResource)value);
//			SadlResource srValue = (SadlResource) value;
//			if (srType == null) {
//				srValue = ((SadlResource)value).getName();
//				srType = declarationExtensions.getOntConceptType(srValue);
//			}
//			if (srType == null) {
//				throw new JenaProcessorException("Unable to resolve SadlResource value");
//			}
//			if (srType.equals(OntConceptType.INSTANCE)) {
//				String valUri = declarationExtensions.getConceptUri(srValue);
//				if (valUri == null) {
//					throw new JenaProcessorException("Failed to find SadlResource in Xtext model");
//				}
//				val = theJenaModel.getIndividual(valUri);
//				if (val == null) {
//					throw new JenaProcessorException("Failed to retrieve instance '" + valUri + "' from Jena model");
//				}
//				return checkObjectPropertyValueInRange(prop, (Individual)val);
//			}
//			else {
//				return false;
//			}
//		}
//		else if (value instanceof SadlExplicitValue) {
//			if (prop.canAs(OntProperty.class)) {
//				val = modelProcessor.sadlExplicitValueToLiteral((SadlExplicitValue) value, prop.as(OntProperty.class).getRange());
//			}
//			else {
//				val = modelProcessor.sadlExplicitValueToLiteral((SadlExplicitValue) value, null);
//			}
//			return checkDataPropertyValueInRange(theJenaModel, null, prop, val);
//		}
		
		if (createErrorMessage(errorMessageBuilder, predType, valType, operations.get(0), false, val.eContainer())) {
			return false;
		}
		else {
			return true;
		}
	}

	private boolean checkNumericRangeLimits(String op, TypeCheckInfo predType, TypeCheckInfo valType) throws TranslationException {
		if (valType == null || predType == null) {
			return false;	// return as error
		}
		if (valType.getExplicitValue() instanceof Literal) {
			 Object value = ((Literal)valType.getExplicitValue()).getValue();
			  Node rngType = predType.getTypeCheckType();
			  if (!(rngType instanceof NamedNode)) {
				  throw new TranslationException("Unexpected non-NamedNode TypeCheckInfo typeCheckType");
			  }
			  boolean outOfRange = false;
			  if (rngType.toFullyQualifiedString().equals(XSD.xint.getURI())) {
				  if (op.equals(">") || op.equals("<")) {
					  if (value instanceof Long && ((Long)value >= MAX_INT || (Long)value <= MIN_INT)) {
						  outOfRange = true;
					  }
					  else if (value instanceof Integer && ((Integer)value >= MAX_INT || (Integer)value <= MIN_INT)) {
						  outOfRange = true;
					  }
				  }
				  else {
					  if (value instanceof Long && ((Long)value > MAX_INT || (Long)value < MIN_INT)) {
						  outOfRange = true;
					  }
					  else if (value instanceof Integer && ((Integer)value > MAX_INT || (Integer)value < MIN_INT)) {
						  outOfRange = true;
					  }
				  }
				  if (outOfRange) {
					  modelProcessor.addTypeCheckingError("Value is not in range of property", valType.context);
					  return false;
				  }
			  }
			  else if (rngType.toFullyQualifiedString().equals(XSD.xlong.getURI())) {
				  if (op.equals(">") || op.equals("<")) {
					  if (value instanceof Long && ((Long)value >= MAX_LONG || (Long)value <= MIN_LONG)) {
						  outOfRange = true;
					  }
					  else if (value instanceof Integer && ((Integer)value >= MAX_LONG || (Integer)value <= MIN_LONG)) {
						  outOfRange = true;
					  }
				  }
				  else {
					  if (value instanceof Long && ((Long)value > MAX_LONG || (Long)value < MIN_LONG)) {
						  outOfRange = true;
					  }
					  else if (value instanceof Integer && ((Integer)value > MAX_LONG || (Integer)value < MIN_LONG)) {
						  outOfRange = true;
					  }
				  }
				  if (outOfRange) {
					  modelProcessor.addTypeCheckingError("Value is not in range of property", valType.context);
					  return false;
				  }
			  }
			 
		}
		return true;	// if we don't detect a problem assume that there isn't one
	}

	public boolean checkDataPropertyValueInRange(OntModel theJenaModel2, Resource subj, OntProperty prop, Literal val) {
		OntResource rng = prop.getRange();
		if (rng == null) {
			return true;
		}
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

	public boolean checkObjectPropertyRange(OntModel theJenaModel2, OntProperty pred, OntResource obj, boolean isList, EObject expr) throws CircularDependencyException {
		if (pred.isObjectProperty()) {
			if (checkRangeForMatch(theJenaModel2, pred, obj, isList)) {
				return true;
			}
			ExtendedIterator<? extends OntProperty> propitr = pred.listSuperProperties(false);
			while (propitr.hasNext()) {
				OntProperty sprop = propitr.next();
				if (checkRangeForMatch(theJenaModel2, sprop, obj, isList)) {
					propitr.close();
					return true;
				}
			}
			return false;
		}
		return true;
	}
	
	public boolean checkPropertyRange(OntModel theJenaModel2, OntProperty pred, OntResource obj, boolean isList, EObject expr) throws CircularDependencyException {
		if (checkRangeForMatch(theJenaModel2, pred, obj, isList)) {
			return true;
		}
		ExtendedIterator<? extends OntProperty> propitr = pred.listSuperProperties(false);
		while (propitr.hasNext()) {
			OntProperty sprop = propitr.next();
			if (checkRangeForMatch(theJenaModel2, sprop, obj, isList)) {
				propitr.close();
				return true;
			}
		}
		return false;
	}
	
	private boolean checkRangeForMatch(OntModel theJenaModel2, OntProperty pred, OntResource obj, boolean isList) throws CircularDependencyException {
		StmtIterator rngitr = theJenaModel2.listStatements(pred, RDFS.range, (RDFNode)null);
		while (rngitr.hasNext()) {
			RDFNode rng = rngitr.nextStatement().getObject();
			if (isTypedListSubclass(rng)) {
				if (!isList) {
					rngitr.close();
					return false;
				}
				if (rng.canAs(OntClass.class )) {
					rng = getSadlTypedListType(rng.as(OntClass.class));
				}
				else {
// TODO this is an unhandled case					
				}
			}
			if (obj instanceof Individual) {
				ExtendedIterator<Resource> institr = obj.asIndividual().listRDFTypes(true);
				while (institr.hasNext()) {
					Resource typ = institr.next();
					if (rng.canAs(OntClass.class)) {
						if (SadlUtils.classIsSubclassOf(typ.as(OntClass.class), rng.asResource().as(OntClass.class), true, null)) {
							institr.close();
							rngitr.close();
							return true;
						}
					}
				}
			}
			else if (obj != null && obj.canAs(OntClass.class) && rng.isResource() && rng.asResource().canAs(OntClass.class)) {
				if (SadlUtils.classIsSubclassOf(obj.as(OntClass.class), rng.asResource().as(OntClass.class), true, null)) {
					rngitr.close();
					return true;
				}
			}
		}
		return false;
	}

	protected List<Node> getApplicableLocalRestrictions(NamedNode subj, NamedNode prop)
			throws IOException, PrefixNotFoundException, InvalidNameException, InvalidTypeException,
			TranslationException, ConfigurationException {
		// TODO Auto-generated method stub
		return null;
	}

			}
