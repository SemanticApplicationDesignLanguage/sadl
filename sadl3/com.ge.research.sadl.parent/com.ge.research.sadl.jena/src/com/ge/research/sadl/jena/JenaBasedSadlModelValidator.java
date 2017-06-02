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

import org.eclipse.emf.ecore.EObject;
import com.ge.research.sadl.errorgenerator.generator.*;
import com.ge.research.sadl.model.CircularDefinitionException;
import com.ge.research.sadl.model.ConceptIdentifier;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.ConceptName.RangeValueType;
import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.PrefixNotFoundException;
import com.ge.research.sadl.processing.ISadlModelValidator;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.processing.SadlOntologyHelper;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.processing.ISadlOntologyHelper.Context;
import com.ge.research.sadl.processing.ISadlOntologyHelper.ContextBuilder;
import com.ge.research.sadl.reasoner.CircularDependencyException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.BinaryOperation;
import com.ge.research.sadl.sADL.BooleanLiteral;
import com.ge.research.sadl.sADL.CommaSeparatedAbreviatedExpression;
import com.ge.research.sadl.sADL.Constant;
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
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration;
import com.ge.research.sadl.sADL.SadlConstantLiteral;
import com.ge.research.sadl.sADL.SadlDataType;
import com.ge.research.sadl.sADL.SadlExplicitValue;
import com.ge.research.sadl.sADL.SadlInstance;
import com.ge.research.sadl.sADL.SadlIntersectionType;
import com.ge.research.sadl.sADL.SadlModelElement;
import com.ge.research.sadl.sADL.SadlMustBeOneOf;
import com.ge.research.sadl.sADL.SadlNumberLiteral;
import com.ge.research.sadl.sADL.SadlParameterDeclaration;
import com.ge.research.sadl.sADL.SadlPrimitiveDataType;
import com.ge.research.sadl.sADL.SadlPropertyCondition;
import com.ge.research.sadl.sADL.SadlResource;
import com.ge.research.sadl.sADL.SadlSimpleTypeReference;
import com.ge.research.sadl.sADL.SadlStringLiteral;
import com.ge.research.sadl.sADL.SadlTypeReference;
import com.ge.research.sadl.sADL.SadlUnionType;
import com.ge.research.sadl.sADL.SelectExpression;
import com.ge.research.sadl.sADL.StringLiteral;
import com.ge.research.sadl.sADL.SubjHasProp;
import com.ge.research.sadl.sADL.Sublist;
import com.ge.research.sadl.sADL.TestStatement;
import com.ge.research.sadl.sADL.UnaryExpression;
import com.ge.research.sadl.sADL.Unit;
import com.ge.research.sadl.sADL.ValueTable;
import com.ge.research.sadl.sADL.impl.TestStatementImpl;
import com.hp.hpl.jena.datatypes.DatatypeFormatException;
import com.hp.hpl.jena.ontology.AllValuesFromRestriction;
import com.hp.hpl.jena.ontology.AnnotationProperty;
import com.hp.hpl.jena.ontology.DatatypeProperty;
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
	protected ValidationAcceptor issueAcceptor = null;
	protected OntModel theJenaModel = null;
	protected DeclarationExtensions declarationExtensions = null;
	private EObject defaultContext;
	
	protected Map<EObject, TypeCheckInfo> expressionsValidated = new HashMap<EObject,TypeCheckInfo>();
	private Map<EObject, Property> impliedPropertiesUsed = null;
	
	private IMetricsProcessor metricsProcessor = null;
	protected JenaBasedSadlModelProcessor modelProcessor = null;
	private List<ConceptName> binaryOpLeftImpliedProperties;
	private List<ConceptName> binaryOpRightImpliedProperties;
	protected Object lastSuperCallExpression = null; 

   	public enum ExplicitValueType {RESTRICTION, VALUE}

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
    	private ConceptIdentifier typeCheckType = null;			// the type of the TypeCheckInfo which must match the other side of the expression,
    															//	e.g., the range of the property of a "<property> of <subject>" expression
    	private RDFNode explicitValue = null;					// the explicit value that is allowed, as in a hasValue restriction
    	private ExplicitValueType explicitValueType;			// The type of the explicit value
     	private RangeValueType rangeValueType = RangeValueType.CLASS_OR_DT;	
    															// the range type, one of RangeValueType.CLASS_OR_DT (Class or RDFDataType)
    															//	or LIST (a subclass of http://sadl.org/sadllistmodel#List)
    	private List<ConceptName> implicitProperties = null;	// Implied properties, if any, that apply to this expressionType
 
    	private List<TypeCheckInfo> compoundTypes = null;		// If this is a disjunction of multiple types, this contains the next
    															//	lower level of TypeCheckInfos in the hierarchy, e.g., a property whose
    															//	range is a union of classes or which is given range in multiple imports
    	
    	private String typeToExprRelationship = "range";		// the relationship between the typeCheckType and the expressionType, e.g., range (the default)
    	
    	public TypeCheckInfo(ConceptIdentifier eType) {
    		setExpressionType(eType);
    	}
    	
    	/* Constructor for compound types (union, e.g. range) */
    	public TypeCheckInfo(ConceptIdentifier eType, JenaBasedSadlModelValidator validator, EObject ctx) {
    		setExpressionType(eType);
    		context = ctx;
    		if (ctx != null && this.getTypeCheckType() != null) {
    			validator.expressionsValidated.put(ctx,  this);
    		}
    	}
    	
    	public TypeCheckInfo(ConceptIdentifier eType, ConceptIdentifier tcType, JenaBasedSadlModelValidator validator, EObject ctx) {
    		setExpressionType(eType);
    		setTypeCheckType(tcType);
    		context = ctx;
    		if (ctx != null) {
    			validator.expressionsValidated.put(ctx,  this);
    		}
    	}
    	 	
		public TypeCheckInfo(ConceptIdentifier eType, ConceptIdentifier tcType, JenaBasedSadlModelValidator validator, List<ConceptName> impliedProps, EObject ctx) {
			this(eType, tcType, validator, ctx);
    		if (impliedProps != null) {
   				implicitProperties = impliedProps;
    		}
		}

    	public TypeCheckInfo(ConceptName eType, RDFNode valueRestriction, ExplicitValueType valueType, JenaBasedSadlModelValidator validator, EObject ctx) {
    		setExpressionType(eType);
    		setExplicitValueType(valueType);
    		context = ctx;
    		if (ctx != null && this.getTypeCheckType() != null) {
    			validator.expressionsValidated.put(ctx,  this);
    		}
    		setExplicitValue(valueRestriction);
    		if (valueType.equals(ExplicitValueType.RESTRICTION)) {
    			setTypeToExprRelationship("restriction to");
    		}
    		else {
    			setTypeToExprRelationship("explicit value");
    		}
		}

    	public TypeCheckInfo(ConceptIdentifier eType, ConceptIdentifier tcType, List<ConceptName> impliedProps, JenaBasedSadlModelValidator validator, EObject ctx) {
    		setExpressionType(eType);
    		setTypeCheckType(tcType);
			implicitProperties = impliedProps;
    		context = ctx;
    		if (ctx != null) {
    			validator.expressionsValidated.put(ctx,  this);
    		}
    	}
    	
		public boolean equals(Object o) {
			if (o instanceof TypeCheckInfo) {
				TypeCheckInfo other = (TypeCheckInfo) o;
				return context == other.context // Identity check should be fine for EObjects
					&& Objects.equals(getExpressionType(), other.getExpressionType())
					&& Objects.equals(getRangeValueType(), other.getRangeValueType())
					&& Objects.equals(getTypeCheckType(), other.getTypeCheckType());
				
			}
    		return false;
    	}
		
		@Override
		public int hashCode() {
			return Objects.hash(context, getExpressionType(), getRangeValueType(), getTypeCheckType());
		}

		public ConceptIdentifier getExpressionType() {
			return expressionType;
		}

		public void setExpressionType(ConceptIdentifier expressionType) {
			this.expressionType = expressionType;
		}

		public ConceptIdentifier getTypeCheckType() {
			return typeCheckType;
		}

		public void setTypeCheckType(ConceptIdentifier typeCheckType) {
			this.typeCheckType = typeCheckType;
			if (typeCheckType instanceof ConceptName) {
				setRangeValueType(((ConceptName)typeCheckType).getRangeValueType());
			}
		}
		
		public RangeValueType getRangeValueType() {
			if(this.getCompoundTypes() != null){
				return getCompoundRangeValueType(this);
			}else{
				return rangeValueType;
			}
		}
		
		private RangeValueType getCompoundRangeValueType(TypeCheckInfo tci){
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
						issueAcceptor.addError("Incompatable Range Types", tci.context); //TODO add new error message
					}
				}else{
					rvt = rvt2;
				}
			}
			
			return rvt;
		}
		
		protected void setContext(JenaBasedSadlModelValidator validator, EObject ctx) {
			this.context = ctx;
			if (ctx != null) {
				validator.expressionsValidated.put(ctx, this);
			}
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
				if (getRangeValueType() != null && !getRangeValueType().equals(RangeValueType.CLASS_OR_DT)) {
					sb.append(getRangeValueType().toString());
					sb.append(" of values of type, ");
				}
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
    }
	
	public JenaBasedSadlModelValidator(ValidationAcceptor issueAcceptor, OntModel theJenaModel, JenaBasedSadlModelProcessor processor) {
		this(issueAcceptor, theJenaModel, new DeclarationExtensions(), processor, null);
	}
	
	public JenaBasedSadlModelValidator(ValidationAcceptor issueAcceptor, OntModel theJenaModel, DeclarationExtensions declarationExtensions, JenaBasedSadlModelProcessor processor, IMetricsProcessor metricsProcessor){
		this.issueAcceptor = issueAcceptor;
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
			ConceptName numberLiteralConceptName = new ConceptName(XSD.xint.getURI());
			numberLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			TypeCheckInfo xsdTypeCheckInfo =  new TypeCheckInfo(numberLiteralConceptName, numberLiteralConceptName, this, null);				
			if(!compareTypes(operations, expr, null, exprTypeCheckInfo, xsdTypeCheckInfo)){
				createErrorMessage(errorMessageBuilder, exprTypeCheckInfo, xsdTypeCheckInfo, op);
				return false;
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
				errorMessageBuilder);
	}

	public boolean validateBinaryOperationByParts(EObject expression, Expression leftExpression,
			Expression rightExpression, String op, StringBuilder errorMessageBuilder) {
		List<String> operations = Arrays.asList(op.split("\\s+"));
		
		if(skipOperations(operations)){
			return true;
		}
		try {	
			boolean dontTypeCheck = false;
			TypeCheckInfo leftTypeCheckInfo = null;
			try {
				leftTypeCheckInfo = getType(leftExpression);
			} catch (DontTypeCheckException e) {
				dontTypeCheck = true;
			}
			if (useImpliedProperties(op)) {
				setBinaryOpLeftImpliedProperties(leftTypeCheckInfo != null ? leftTypeCheckInfo.getImplicitProperties() : null);
			}
			
			TypeCheckInfo rightTypeCheckInfo = null;
			try {
				rightTypeCheckInfo = getType(rightExpression);
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
			if(!dontTypeCheck && !compareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)){
				if (expression.eContainer() instanceof TestStatement && isQuery(leftExpression)) {
					// you can't tell what type a query will return
					return true;
				}
				if (!rulePremiseVariableAssignment(operations, leftTypeCheckInfo,rightTypeCheckInfo)) {
					String effectiveOp = op;
					if (leftExpression instanceof Constant && ((Constant)leftExpression).getConstant().equals("value")) {
						effectiveOp = "matching value";
//						leftTypeCheckInfo.setRangeValueType(RangeValueType.CLASS_OR_DT);
					}
					createErrorMessage(errorMessageBuilder, leftTypeCheckInfo, rightTypeCheckInfo, effectiveOp);
				}
				return false;
			}
			//It's possible there may be a local type restriction
			handleLocalRestriction(leftExpression,rightExpression,leftTypeCheckInfo,rightTypeCheckInfo);
			return true;
		} catch (Throwable t) {
			return handleValidationException(expression, t);
		}
	}
	
	protected void handleLocalRestriction(Expression leftExpression, Expression rightExpression, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		if (leftExpression instanceof PropOfSubject && rightExpression instanceof Declaration) {
			TypeCheckInfo subjtype = getType(((PropOfSubject)leftExpression).getRight());
			ConceptIdentifier subject = subjtype.getTypeCheckType();
			if (subject != null) {
				addLocalRestriction(subjtype.getTypeCheckType().toString(), leftTypeCheckInfo, rightTypeCheckInfo);
			}
		}
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

	private boolean isQuery(Expression expr) {
		if (expr instanceof StringLiteral) {
			String val = ((StringLiteral)expr).getValue();
			if (val.contains("select") && val.contains("where")) {
				return true;
			}
		}
		return false;
	}

	public void addLocalRestriction(String subjuri, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidTypeException {
		try {
			throw new Exception("addLocalRestriction not implemented");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private boolean handleValidationException(EObject expr, Throwable t) {
		try {
			if (t instanceof InvalidNameException) {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("Invalid Name"), expr);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				t.printStackTrace();
			} else if (t instanceof TranslationException) {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("Translation"), expr);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				t.printStackTrace();
			} else if (t instanceof URISyntaxException) {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("URI Syntax"), expr);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				t.printStackTrace();
			} else if (t instanceof IOException) {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("IO"), expr);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				t.printStackTrace();
			} else if (t instanceof ConfigurationException) {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("Configuration"), expr);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				t.printStackTrace();
			} else if (t instanceof NullPointerException){
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("Null Pointer"), expr);
			} else if (t instanceof DontTypeCheckException) {
				return true;
			} else if (t instanceof PropertyWithoutRangeException) {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.PROPERTY_WITHOUT_RANGE.get(((PropertyWithoutRangeException)t).getPropID()), expr);
				return true;
			} else if (t instanceof CircularDefinitionException) {
				t.printStackTrace();
			}
			else {
				t.printStackTrace();
			}
		}
		catch (Throwable t2) {
			issueAcceptor.addError("Unexpected exception (" + t.getClass().getCanonicalName() + ": " + 
					t2.getMessage() + ") displaying validation error : " + t.getMessage(), expr);
		}
		return false;
	}

	public boolean validate(EObject leftExpression, EObject rightExpression, String op, StringBuilder errorMessageBuilder) {
		List<String> operations = Arrays.asList(op.split("\\s+"));
		TypeCheckInfo leftTypeCheckInfo = null;
		TypeCheckInfo rightTypeCheckInfo = null;
		try {	
			leftTypeCheckInfo = getType(leftExpression);
		} catch (Throwable t) {
			if (handleValidationException(leftExpression, t)) {
				return true;
			}
		} 
		try {	
			rightTypeCheckInfo = getType(rightExpression);
		} catch (Throwable t) {
			if (handleValidationException(rightExpression, t)) {
				return true;
			}
		} 
		try {
			if (leftTypeCheckInfo == null && rightTypeCheckInfo == null) {
				// this condition happens when a file is loaded in the editor and clean/build is invoked
				return true;
			}
			if(!compareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)){
				createErrorMessage(errorMessageBuilder, leftTypeCheckInfo, rightTypeCheckInfo, op);
				return false;
			}
		} catch (InvalidNameException e) {
			try {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("Invalid Name"), rightExpression);
			} catch (InvalidTypeException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.INVALID_EXPRESSION_URI);
			}
			e.printStackTrace();
		} catch (DontTypeCheckException e) {
			return true;
		} catch (InvalidTypeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}

	protected void createErrorMessage(StringBuilder errorMessageBuilder, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo, String operation) throws InvalidTypeException {
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
				if (typeCheckInfo.getRangeValueType().equals(RangeValueType.LIST)) {
					if (sb2 == null) sb2 = new StringBuilder();
					sb2.append("a List of values of type ");
				}
				if (sb2 != null && typeCheckInfo.getTypeCheckType() != null) {
					sb2.append(getModelProcessor().conceptIdentifierToString(typeCheckInfo.getTypeCheckType()));
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
							getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_CHECK_EXCEPTION.get("Datatype Format"), typeCheckInfo.context);
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
//		if (expressionsValidated.containsKey(expression)) {
//			return expressionsValidated.get(expression);
//		}
		if(expression instanceof Name){
			return getType((Name)expression);
		}else if(expression instanceof SadlResource){
			return getType((SadlResource)expression);
		}
		else if(expression instanceof Declaration){
			SadlTypeReference decltype = ((Declaration)expression).getType();
			if (decltype != null) {
				return getType(decltype);
			}
			else {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.NULL_TYPE.toString(), expression);
			}
			//Need to return passing case for time being
//			ConceptName declarationConceptName = new ConceptName("todo");
//			return new TypeCheckInfo(declarationConceptName, declarationConceptName);
		}
		else if(expression instanceof StringLiteral || expression instanceof SadlStringLiteral) {
			ConceptName stringLiteralConceptName = new ConceptName(XSD.xstring.getURI());
			stringLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			return new TypeCheckInfo(stringLiteralConceptName, stringLiteralConceptName, this, expression);
		}
		else if(expression instanceof NumberLiteral || expression instanceof SadlNumberLiteral || expression instanceof Unit){
			BigDecimal value;
			Literal litval;
			if (expression instanceof Unit) { 
				value = ((Unit)expression).getValue().getValue();
				if (!getModelProcessor().ignoreUnittedQuantities) {
					//String unit = ((Unit)expression).getUnit();
					ConceptName uqcn = new ConceptName(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
					List<ConceptName> impliedProperties = getImpliedProperties(theJenaModel.getOntResource(uqcn.getUri()));
					if (impliedProperties != null) {
						return new TypeCheckInfo(uqcn, uqcn, impliedProperties, this, expression);
					}
					else {
						return new TypeCheckInfo(uqcn, uqcn, this, expression);
					}
				}
			}
			else if (expression instanceof NumberLiteral) {
				value = ((NumberLiteral)expression).getValue();
			}
			else {
				String strval = ((SadlNumberLiteral)expression).getLiteralNumber();
				if (strval.indexOf('.') >= 0) {
					value = BigDecimal.valueOf(Double.parseDouble(strval));
				}
				else {
					value = BigDecimal.valueOf(Long.parseLong(strval));
				}
			}
			ConceptName numberLiteralConceptName = null;
			if (value.stripTrailingZeros().scale() <= 0 || value.remainder(BigDecimal.ONE).compareTo(BigDecimal.ZERO) == 0) {
				numberLiteralConceptName = new ConceptName(XSD.xint.getURI());
				litval = theJenaModel.createTypedLiteral(value.intValue());
				
			}
			else {
				numberLiteralConceptName = new ConceptName(XSD.decimal.getURI());
				litval = theJenaModel.createTypedLiteral(value.doubleValue());
			}
			numberLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			TypeCheckInfo litTci = new TypeCheckInfo(numberLiteralConceptName, litval, ExplicitValueType.VALUE, this, expression); 
			litTci.setTypeCheckType(numberLiteralConceptName);
			return litTci;
		}
		else if(expression instanceof BooleanLiteral || expression instanceof SadlBooleanLiteral){
			ConceptName booleanLiteralConceptName = new ConceptName(XSD.xboolean.getURI());
			booleanLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			return new TypeCheckInfo(booleanLiteralConceptName, booleanLiteralConceptName, this, expression);
		}
		else if(expression instanceof Constant){
			return getType((Constant)expression);
		}
		else if (expression instanceof SadlConstantLiteral) {
			String term = ((SadlConstantLiteral)expression).getTerm();
			Literal litval = null;
			if (term.equals("PI")) {
				litval = theJenaModel.createTypedLiteral(Math.PI);
			}
			else if (term.equals("e")) {
				litval = theJenaModel.createTypedLiteral(Math.E);
			}
			else {
				throw new TranslationException("Unhandled SadlConstantLiteral type: " + expression.getClass().getCanonicalName());
			}
			ConceptName numberLiteralConceptName = new ConceptName(XSD.decimal.getURI());
			numberLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			TypeCheckInfo litTci = new TypeCheckInfo(numberLiteralConceptName, litval, ExplicitValueType.VALUE, this, expression); 
			litTci.setTypeCheckType(numberLiteralConceptName);
			return litTci;
		}
		else if(expression instanceof ValueTable){
			ConceptName declarationConceptName = new ConceptName("TODO");
			return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);
		}
		else if(expression instanceof PropOfSubject){
			return getType((PropOfSubject)expression);
		}
		else if(expression instanceof SubjHasProp){
			if (expression.eContainer() instanceof BinaryOperation) {
				// we are comparing or assigning this to something else so we want the type of the root (if there is a chain) property
				if (((SubjHasProp)expression).getProp() instanceof SadlResource) {
					SadlResource prop = ((SubjHasProp)expression).getProp();
					OntConceptType ptype = declarationExtensions.getOntConceptType(prop);
					return getType(prop);
				}
				else {
					issueAcceptor.addError("This subject-has-property construct isn't properly validated, please report.", expression);
				}
			}
			else {
				Declaration subjHasPropInDeclaration = subjHasPropIsDeclaration((SubjHasProp) expression);  // are we in a Declaration (a real declaration--the type is a class)
				if (subjHasPropInDeclaration != null) {
					return getType(subjHasPropInDeclaration);
				}
				else {
					issueAcceptor.addError("This appears to be a declaration isn't fully supported; should it be nested (in parentheses)", expression);
				}
			}
		}
		else if (expression instanceof CommaSeparatedAbreviatedExpression) {
			// validate the property initializations within
			validateCommaSeparatedAbreviatedExpression((CommaSeparatedAbreviatedExpression) expression);
			return getType(((CommaSeparatedAbreviatedExpression)expression).getLeft());
		}
		else if(expression instanceof UnaryExpression){
			return getType(((UnaryExpression) expression).getExpr());
		}
		else if(expression instanceof ElementInList){
			Expression el = ((ElementInList)expression).getElement();
			if (el instanceof PropOfSubject) {
				TypeCheckInfo listtype = getType(((PropOfSubject)el).getRight());
				if (listtype == null) {
					getModelProcessor().addIssueToAcceptor("Unable to get the List type", el);
				}
				else if (listtype.getRangeValueType() != RangeValueType.LIST) {
					getModelProcessor().addIssueToAcceptor("Expected a List", el);
				}
				else {
					// the element's type is the type of the list but not necessarily a list
					listtype = convertListTypeToElementOfListType(listtype);
				}
				return listtype;
			}
			else {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.UNHANDLED.get("element type in element in list construct. ", el.getClass().getCanonicalName() + "; please report"), expression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_ERROR_URI);
				}
			}
		}
		else if(expression instanceof BinaryOperation){
			List<String> operations = Arrays.asList(((BinaryOperation) expression).getOp().split("\\s+"));
			TypeCheckInfo leftTypeCheckInfo = unifyCompoundTypeCheckInfos(getType(((BinaryOperation) expression).getLeft()));
			TypeCheckInfo rightTypeCheckInfo = unifyCompoundTypeCheckInfos(getType(((BinaryOperation) expression).getRight()));
			if (leftTypeCheckInfo != null && isVariable(leftTypeCheckInfo) && ((BinaryOperation)expression).getRight() instanceof Declaration) {
				return rightTypeCheckInfo;
			}
			if (((BinaryOperation) expression).getLeft() instanceof PropOfSubject && ((BinaryOperation)expression).getRight() instanceof Declaration) {
				TypeCheckInfo subjtype = getType(((PropOfSubject)((BinaryOperation) expression).getLeft()).getRight());
				ConceptIdentifier subject = subjtype.getTypeCheckType();
				if (subject != null) {
					addLocalRestriction(subjtype.getTypeCheckType().toString(), leftTypeCheckInfo, rightTypeCheckInfo);
				}
			}

			TypeCheckInfo binopreturn = combineTypes(operations, ((BinaryOperation) expression).getLeft(), ((BinaryOperation) expression).getRight(), 
					leftTypeCheckInfo, rightTypeCheckInfo);
			if (getModelProcessor().isNumericOperator(((BinaryOperation) expression).getOp())) {
				if (leftTypeCheckInfo != null && !isNumeric(leftTypeCheckInfo) && !isNumericWithImpliedProperty(leftTypeCheckInfo, ((BinaryOperation)expression).getLeft())) {
					getModelProcessor().addIssueToAcceptor("Numeric operator requires numeric arguments", ((BinaryOperation)expression).getLeft());
				}
				if (rightTypeCheckInfo != null && !isNumeric(rightTypeCheckInfo) && !isNumericWithImpliedProperty(rightTypeCheckInfo, ((BinaryOperation)expression).getRight())) {
					getModelProcessor().addIssueToAcceptor("Numeric operator requires numeric arguments", ((BinaryOperation)expression).getRight());
				}
				ConceptName decimalLiteralConceptName = new ConceptName(XSD.decimal.getURI());
				decimalLiteralConceptName.setType(ConceptType.RDFDATATYPE);
				return new TypeCheckInfo(decimalLiteralConceptName, decimalLiteralConceptName, this, expression);
			}
			if (binopreturn != null) {
				return binopreturn;
			}
			else {
				// by default assume boolean binary operation
				ConceptName booleanLiteralConceptName = new ConceptName(XSD.xboolean.getURI());
				booleanLiteralConceptName.setType(ConceptType.RDFDATATYPE);
				return new TypeCheckInfo(booleanLiteralConceptName, booleanLiteralConceptName, this, expression);
			}
		}
		else if (expression instanceof Sublist) {
			// the type is the type of the list
			TypeCheckInfo listtype = getType((((Sublist)expression).getList()));
			if (listtype != null && !listtype.getRangeValueType().equals(RangeValueType.LIST)) {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.IS_NOT_A.get("this","list"), ((Sublist)expression).getList());
			}
			return listtype;
		}
		else if (expression instanceof SadlPrimitiveDataType)  {
			return getType((SadlPrimitiveDataType)expression);
		}
		else if (expression instanceof SadlSimpleTypeReference) {
			return getType((SadlSimpleTypeReference)expression);
		}
		else if (expression instanceof SadlIntersectionType) {
			return getType((SadlIntersectionType)expression);
		}
		else if (expression instanceof SadlPropertyCondition) {
			return getType((SadlPropertyCondition)expression);
		}
		else if (expression instanceof SadlResource) {
			return getType((SadlResource)expression);
		}
		else if (expression instanceof SadlTypeReference) {
			return getType((SadlTypeReference)expression);
		}
		else if (expression instanceof SadlUnionType) {
			return getType((SadlUnionType)expression);
		}
		else if (expression instanceof SadlInstance) {
			SadlResource inst = ((SadlInstance)expression).getInstance();
			if (inst == null) {
				SadlTypeReference typ = ((SadlInstance)expression).getType();
				if (typ != null && typ instanceof SadlSimpleTypeReference) {
					inst = ((SadlSimpleTypeReference)typ).getType();
				}
			}
			if (inst != null) {
				TypeCheckInfo insttci = getType(inst);
				if (insttci.getTypeCheckType() == null) {
					SadlTypeReference typ = ((SadlInstance)expression).getType();
					if (typ != null && typ instanceof SadlSimpleTypeReference) {
						SadlResource typsr = ((SadlSimpleTypeReference)typ).getType();
						insttci.setTypeCheckType(new ConceptName(declarationExtensions.getConceptUri(typsr)));
					}
				}
				return insttci;
			}
			else {
				throw new TranslationException("Unhandled condition of SadlInstance");
			}
		}
		else if (expression != null) {
			throw new TranslationException("Unhandled expression type: " + expression.getClass().getCanonicalName());
		}
		if (expression != null) {
			getModelProcessor().addIssueToAcceptor(SadlErrorMessages.DECOMPOSITION_ERROR.get(expression.toString()), expression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
		}
		return null;
	}
	
	protected void validateCommaSeparatedAbreviatedExpression(CommaSeparatedAbreviatedExpression expression) throws DontTypeCheckException, CircularDefinitionException, 
		InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		TypeCheckInfo proptct = getType(expression.getProp());
		TypeCheckInfo rghttct = getType(expression.getRight());
		if (!compareTypesUsingImpliedProperties(Arrays.asList("is"), expression.getProp(), expression.getRight(), proptct, rghttct)) {
			StringBuilder errorMessageBuilder = new StringBuilder();
			createErrorMessage(errorMessageBuilder, proptct, rghttct, "property initialization");
			issueAcceptor.addError(errorMessageBuilder.toString(), expression);
		}
	}

	private boolean subjHasPropIsNested(SubjHasProp expression) {
		if (expression.eContainer() instanceof CommaSeparatedAbreviatedExpression) {
			return true; 
		}
		if (expression.eContainer() instanceof BinaryOperation 
//				&& modelProcessor.isEqualityInequalityComparisonOperator(((BinaryOperation)expression.eContainer()).getOp())
				) {
			return true;
		}
		return false;
	}

	protected Declaration subjHasPropIsDeclaration(SubjHasProp expression) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		if (expression.getLeft() instanceof Declaration) {
			TypeCheckInfo declType = getType(((Declaration)expression.getLeft()).getType());
			Object tct = declType.getTypeCheckType();
			if (tct instanceof ConceptName && ((ConceptName)tct).getType().equals(ConceptType.ONTCLASS)) {
				return (Declaration)expression.getLeft();
			}
		}
		if (expression.getLeft() instanceof SubjHasProp) {
			return subjHasPropIsDeclaration((SubjHasProp)expression.getLeft());
		}
		return null;
	}

	private TypeCheckInfo unifyCompoundTypeCheckInfos(TypeCheckInfo tci) throws CircularDependencyException, InvalidNameException {
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
					ConceptIdentifier newTct = newTci.getTypeCheckType();
					if (newTct instanceof ConceptName) {
						OntClass newOr = theJenaModel.getOntClass(((ConceptName)newTct).getUri());
						if (newOr != null) {
							for (int j = 0; j < considered.size(); j++) {
								if (newRvt.equals(considered.get(j).getRangeValueType())) {
									ConceptIdentifier consideredTct = considered.get(j).getTypeCheckType();
									if (consideredTct instanceof ConceptName) {
										OntClass consideredOr = theJenaModel.getOntClass(((ConceptName)consideredTct).getUri());
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
								}
							}
						}
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

	private boolean isNumericWithImpliedProperty(TypeCheckInfo tci, Expression expr) throws DontTypeCheckException, InvalidNameException, InvalidTypeException {
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
		ConceptIdentifier ci;
		if (tci.getTypeCheckType() != null) {
			ci = tci.getTypeCheckType();
			if (ci instanceof ConceptName) {
				return getModelProcessor().isNumericType((ConceptName) ci);
			}
		}
		else if (tci.getExplicitValueType() != null) {
//TODO this is incomplete; more examples needed AWC 12/19/2016				
			ExplicitValueType evt = tci.getExplicitValueType();
			if (evt.equals(ExplicitValueType.RESTRICTION)) {
				issueAcceptor.addWarning("Explicit value type is RESTRICITON, which isn't yet handled. Please report with use case.", tci.context);
			}
			else if (tci.getExpressionType() instanceof ConceptName){
				return getModelProcessor().isNumericType((ConceptName) tci.getExpressionType());
			}
		}
		return false;
	}

	private TypeCheckInfo convertListTypeToElementOfListType(TypeCheckInfo listtype) {
		listtype.setRangeValueType((listtype.getTypeCheckType() != null && listtype.getTypeCheckType() instanceof ConceptName) ? ((ConceptName)listtype.getTypeCheckType()).getRangeValueType() : RangeValueType.CLASS_OR_DT);
		
		if(listtype.getCompoundTypes() == null){
			//not compound
			ConceptIdentifier tct = listtype.getTypeCheckType();
			if(tct instanceof ConceptName){
				((ConceptName) tct).setRangeValueType(RangeValueType.CLASS_OR_DT); 
			}
		}else{
			  Iterator<TypeCheckInfo> tci_iter = listtype.getCompoundTypes().iterator();
			  while(tci_iter.hasNext()){
				  convertListTypeToElementOfListType(tci_iter.next());
			  }
		}
		
		return listtype;
	}

	protected TypeCheckInfo getType(Constant expression) throws DontTypeCheckException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, CircularDefinitionException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		//What do we do about the rest of the constants?
		/*'--' | 'a'? 'type' ;*/
		String constant = expression.getConstant();	
		if(constant.equals("PI") || constant.equals("e")){
			ConceptName constantConceptName = new ConceptName(XSD.decimal.getURI());
			constantConceptName.setType(ConceptType.DATATYPEPROPERTY);
			return new TypeCheckInfo(constantConceptName, constantConceptName, this, expression);
		}
		else if(constant.equals("length") || constant.equals("count") ||
				   constant.equals("index")){
					ConceptName constantConceptName = new ConceptName(XSD.xint.getURI());
					constantConceptName.setType(ConceptType.DATATYPEPROPERTY);
					return new TypeCheckInfo(constantConceptName, constantConceptName, this, expression);
				}
		else if(constant.contains("element") && (constant.contains("first") || constant.contains("last"))){
			//Handle list types???
			ConceptName declarationConceptName = new ConceptName("TODO");
			return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);
		}
//		else if (constant.endsWith("value")) {
//			throw new DontTypeCheckException();
//		}
		else if (expression instanceof Constant && (((Constant)expression).getConstant().equals("value")
				|| ((Constant)expression).getConstant().equals("type"))) {
			Sublist slexpr = getSublistContainer(expression);
			if (slexpr != null) {
				return getType(slexpr.getList());
			}
			return getType(expression);
		}
		else if (constant.equals("a type")) {
			ConceptName rdfType = new ConceptName(RDFS.subClassOf.getURI());
			rdfType.setType(ConceptType.ONTCLASS);
			return new TypeCheckInfo(rdfType, rdfType, this, expression);
		}
		else if(constant.equals("None")){
			ConceptName constantConceptName = new ConceptName(constant);
			constantConceptName.setType(ConceptType.INDIVIDUAL);
			return new TypeCheckInfo(constantConceptName, constantConceptName, this, expression);
		}
		else if (constant.equals("known")) {
			ConceptName constantConceptName = new ConceptName(constant);
			constantConceptName.setType(ConceptType.INDIVIDUAL);
			return new TypeCheckInfo(constantConceptName, constantConceptName, this, expression);
		}
		else {
			// let any subclass validators do their thing
			lastSuperCallExpression = expression;
			return getType((Constant)expression);
		}

	}

	private boolean isVariable(TypeCheckInfo tci) {
		if (tci == null) return false;
		ConceptIdentifier ci = tci.getTypeCheckType();
		if (ci instanceof ConceptName && ((ConceptName)ci).getType() != null && ((ConceptName)ci).getType().equals(ConceptType.VARIABLE)) {
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
		getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_REFERENCE_ERROR.get(expression.getClass().getCanonicalName()), expression);
		if (metricsProcessor != null) {
			metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
		}
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);
	}
	
	private TypeCheckInfo getType(SadlIntersectionType expression) {
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);		
	}

	private TypeCheckInfo getType(SadlPrimitiveDataType expression) {
		TypeCheckInfo tci = getType(expression.getPrimitiveType());
		tci.setContext(this, expression);
		return tci;
	}

	private TypeCheckInfo getType(SadlDataType primitiveType) {
		String nm = primitiveType.getName();
		ConceptName cn = new ConceptName(XSD.getURI() + nm);
		cn.setType(ConceptType.RDFDATATYPE);
		return new TypeCheckInfo(cn, cn, this, null);
	}

	private TypeCheckInfo getType(SadlPropertyCondition expression) {
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);		
	}
	
	private TypeCheckInfo getType(SadlSimpleTypeReference expression) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		TypeCheckInfo tci = getType(expression.getType());
		if (expression.isList()) {
			tci.setRangeValueType(RangeValueType.LIST);
		}
		return tci;
	}

	private TypeCheckInfo getType(SadlUnionType expression) {
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);		
	}

	private TypeCheckInfo getType(PropOfSubject expression) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		String ofOp = expression.getOf();
		Expression predicate = expression.getLeft();
		Expression subject = expression.getRight();
		
		if (predicate instanceof Constant) {
			String cnstval = ((Constant)predicate).getConstant();
			TypeCheckInfo subjtype = null;
			if (constantRequiresListNext(cnstval)) {
				subjtype = getType(subject);
				if (subjtype != null && !subjtype.getRangeValueType().equals(RangeValueType.LIST)) {
					getModelProcessor().addIssueToAcceptor(SadlErrorMessages.MUST_BE_APPLIED_TO_LIST.get(cnstval, getTypeCheckTypeString(subjtype)), subject);
//					getModelProcessor().addIssueToAcceptor("'" + cnstval + "' must be applied to a List ('" + getTypeCheckTypeString(subjtype) + "' is not a List)", subject);
				}
			}
			else if (constantFollowedByIntThenList(cnstval)) {
				subjtype = getType(subject);
				
			}
			else if (constantFollowedByElementThenList(cnstval)) {
				subjtype = getType(subject);
				
			}
			if (cnstval.equals("length") || cnstval.equals("count") || cnstval.equals("index")) {
				ConceptName nlcn = new ConceptName(XSD.xint.getURI());
				nlcn.setType(ConceptType.RDFDATATYPE);
				return new TypeCheckInfo(nlcn, nlcn, this, expression);
			}
			else if (subjtype != null && (cnstval.equals("first element") || cnstval.equals("last element")) ) {
				subjtype.setRangeValueType(RangeValueType.CLASS_OR_DT);   	// keep type but change from List to reflect this is an element of the list
				return subjtype;
			}
			else if (cnstval.equals("a type")) {
				ConceptName rdfType = new ConceptName(RDFS.subClassOf.getURI());
				rdfType.setType(ConceptType.ONTCLASS);
				return new TypeCheckInfo(rdfType, rdfType, this, expression);
			}
			else {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.UNHANDLED.get("Constant Property", cnstval), expression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
			}
		}
		if (predicate instanceof Name) {
			try {
				OntConceptType predtype = declarationExtensions.getOntConceptType(((Name)predicate).getName());
				if (ofOp != null && ofOp.equals("in")) {
					// this is a list construct: element in list
				}
				else if (!predtype.equals(OntConceptType.CLASS_PROPERTY) && !predtype.equals(OntConceptType.DATATYPE_PROPERTY) && 
						!predtype.equals(OntConceptType.RDF_PROPERTY) && !predtype.equals(OntConceptType.ANNOTATION_PROPERTY)) {
					getModelProcessor().addIssueToAcceptor(SadlErrorMessages.EXPECTED_A.get("property in property chain"), predicate);
					//String preduri = declarationExtensions.getConceptUri(((Name)predicate).getName());
				}
			} catch (CircularDefinitionException e) {
				e.printStackTrace();
			}
		}
		boolean validSubject = true;
		if (subject instanceof Name) {
			// check for applicable local restriction first
			TypeCheckInfo predicateType;
			try {
				predicateType = getApplicableLocalRestriction(subject, predicate);
				if (predicateType != null) {
					if (subject instanceof PropOfSubject) {
						checkEmbeddedPropOfSubject(subject, predicate);
					}
					//
					if(predicateType.getTypeCheckType() != null){
						addEffectiveRange(predicateType, subject);
					}
					return predicateType;
				}
			} catch (PrefixNotFoundException e) {
				e.printStackTrace();
			} catch (InvalidTypeException e) {
				e.printStackTrace();
			}
			// check for AllValuesFrom restriction before defaulting to checking property range
			predicateType = getTypeFromRestriction(subject, predicate);
			if (predicateType != null) {
				if (subject instanceof PropOfSubject) {
					checkEmbeddedPropOfSubject(subject, predicate);
				}
				//
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
			if (subject instanceof PropOfSubject) {
				checkEmbeddedPropOfSubject(subject, predicate);	
				//TODO figure out how to add effective range for propOfSubj		
			}else if(validSubject && predicateType != null && predicateType.getTypeCheckType() != null){
				//add interface range
				addEffectiveRange(predicateType, subject);
			}
			return predicateType;
		}
		return null;
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
						if (subjTCI != null && !subjTCI.getTypeCheckType().toString().equals("TODO")) {
							addEffectiveRangeByTypeCheckInfo(predicateType, subjTCI);
						}
					}
					else {
	//					cls = ((Name)subject).getName();
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
			throws InvalidNameException, CircularDefinitionException {
		if (subjTCI.getCompoundTypes() != null) {
			Iterator<TypeCheckInfo> itr = subjTCI.getCompoundTypes().iterator();
			while (itr.hasNext()) {
				TypeCheckInfo nexttci = itr.next();
				addEffectiveRangeByTypeCheckInfo(predicateType, nexttci);
			}
		}
		else {
			ConceptIdentifier ci = subjTCI.getTypeCheckType();
			if (ci instanceof ConceptName) {
				// this should be the class name
				String className = ((ConceptName) ci).getUri();
				addEffectiveRangeUnit(className, predicateType);
			}
			else {
				throw new InvalidNameException("addEffectiveRangeByTypeCheckInfo called with TypeCheckInfo '" + subjTCI.toString() + ", which isn't handled.");
			}
		}
	}

	private void addEffectiveRangeUnit(String className, TypeCheckInfo predicateType) {
		String propertyName = predicateType.getExpressionType().toString();
		String rangeStr = predicateType.getTypeCheckType().toString();
		boolean isList = predicateType.getRangeValueType().equals(RangeValueType.LIST);
		metricsProcessor.addEffectiveRangeAndDomain(null, className, propertyName, rangeStr, isList);
	}
	
	private String getTypeCheckTypeString(TypeCheckInfo tci) {
		if (tci != null) {
			if (tci.typeCheckType != null) {
				return tci.typeCheckType.toString();
			}
			if (tci.getCompoundTypes() != null) {
				StringBuilder sb = new StringBuilder();
				Iterator<TypeCheckInfo> itr = tci.getCompoundTypes().iterator();
				while (itr.hasNext()) {
					sb.append(getTypeCheckTypeString(itr.next()));
				}
				return sb.toString();
			}
		}
		return "(null)";
	}

	protected TypeCheckInfo getApplicableLocalRestriction(Expression subject, Expression predicate) throws IOException, PrefixNotFoundException, InvalidNameException, InvalidTypeException, TranslationException, ConfigurationException {
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
				cnstval.equals("first element") ||
				cnstval.equals("last element")) {
			return true;
		}
		return false;
	}

	private void checkEmbeddedPropOfSubject(Expression subject, Expression predicate) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		if (predicate instanceof Name) {
			String propuri = declarationExtensions.getConceptUri(((Name)predicate).getName());
			OntConceptType oct = declarationExtensions.getOntConceptType(((Name)predicate).getName());
			if (oct.equals(OntConceptType.ANNOTATION_PROPERTY)) {
				issueAcceptor.addWarning(SadlErrorMessages.DOMAIN_MATCHING.get("annotation property"), predicate);
				return;
			}
			Property prop = theJenaModel.getProperty(propuri);
			TypeCheckInfo subjType = getType(subject);
			List<OntClass> subjClasses = subjType != null ? getTypeCheckTypeClasses(subjType) : null;
			StmtIterator domainItr = prop.listProperties(RDFS.domain);
			boolean domainMatched = false;
			List<Resource> domainList = null;
			while (domainItr.hasNext() && !domainMatched) {
				RDFNode dmn = domainItr.next().getObject();
				if (dmn instanceof Resource) {
					if (dmn.isURIResource()) {
						for (int i = 0; subjClasses != null && i < subjClasses.size(); i++) {
							if (subjClasses.get(i).getURI().equals(((Resource) dmn).getURI())) {
								domainItr.close();
								domainMatched = true;		// this is a direct match
								break;
							}
						}
					}
					if (!domainMatched) {
						if (domainList == null) domainList = new ArrayList<Resource>();
						domainList.add((Resource) dmn);
					}
				}
			}
			if (!domainMatched) {
				// there was no direct match
				for (int i = 0; domainList != null && i < domainList.size(); i++) {
					Resource dmn = domainList.get(i);
					if ((dmn instanceof OntResource || dmn.canAs(OntResource.class)) && subjType.getTypeCheckType() != null) {
						try {
							for (int j = 0; subjClasses != null && j < subjClasses.size(); j++) {
								OntClass subj = subjClasses.get(j);
								if ( SadlUtils.classIsSubclassOf(subj, dmn.as(OntResource.class),true, null)) {
									domainMatched = true;
									break;
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
						ConceptName cn2 = null;
						if (domainList.get(0).isURIResource()) {
							cn2 = createTypedConceptName(domainList.get(0).getURI(), OntConceptType.CLASS);
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
								TypeCheckInfo tci = new TypeCheckInfo(createTypedConceptName(propuri, oct), createTypedConceptName(domainList.get(i).getURI(), OntConceptType.CLASS), this, predicate);
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
					createErrorMessage(errorMessageBuilder, leftTypeCheckInfo, subjType, "chained property");
					getModelProcessor().addIssueToAcceptor(errorMessageBuilder.toString(), predicate);
				} catch (CircularDefinitionException e) {
					e.printStackTrace();
				}
			}
		}
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
			if (tci.getTypeCheckType() != null && tci.getTypeCheckType().toString() != null) {
				OntClass result = theJenaModel.getOntClass(tci.getTypeCheckType().toString());
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
					ConceptIdentifier varci = varTci.getTypeCheckType();
					if (varci instanceof ConceptName) {
						if (((ConceptName) varci).toString().equals("TODO")) {
							return null;
						}
						if (((ConceptName) varci).getName() == null || ((ConceptName) varci).getNamespace() == null) {
							return null;
						}
						subjuri = ((ConceptName)varci).getUri();
					}
				}
			}
			String propuri = declarationExtensions.getConceptUri(((Name)predicate).getName());
			if (propuri == null) {
				getModelProcessor().addIssueToAcceptor("Predicate name could not be resolved", predicate);
			}
			else {
				OntConceptType proptype = declarationExtensions.getOntConceptType(((Name)predicate).getName());
				return getTypeFromRestriction(subjuri, propuri, proptype, predicate);
			}
		}
		return null;
	}
	
	protected TypeCheckInfo getTypeFromRestriction(String subjuri, String propuri, OntConceptType proptype, Expression predicate) throws InvalidTypeException {
		Resource subj = theJenaModel.getResource(subjuri);
		if (subj != null) {
			if (!(subj instanceof OntClass || subj.canAs(OntClass.class)) && subj.canAs(Individual.class)) {
				subj = subj.as(Individual.class).getRDFType(true);
			}
			if (subj != null && subj.canAs(OntClass.class)){ 
				Property prop = theJenaModel.getProperty(propuri);
				if (isListAnnotatedProperty(prop)) {
					return null;
				}
				// now look for restrictions on "range"
				StmtIterator sitr = theJenaModel.listStatements(null, OWL.onProperty, prop);
				while (sitr.hasNext()) {
					Statement stmt = sitr.nextStatement();
					Resource sr = stmt.getSubject();
					if (sr.canAs(OntClass.class) && subj.as(OntClass.class).hasSuperClass(sr.as(OntClass.class))) {
						if (sr.as(OntClass.class).asRestriction().isAllValuesFromRestriction()) {
							Resource avf = sr.as(OntClass.class).asRestriction().asAllValuesFromRestriction().getAllValuesFrom();
							if (avf.isLiteral()) {
								TypeCheckInfo avftci =  new TypeCheckInfo(createTypedConceptName(propuri, proptype), 
										createTypedConceptName(avf.getURI(), OntConceptType.CLASS), this, predicate);
								avftci.setTypeToExprRelationship("restriction to");
								return avftci;
							}
							else if (avf.isURIResource()){
								List<ConceptName> impliedProperties = getImpliedProperties(avf);
								TypeCheckInfo avftci = new TypeCheckInfo(createTypedConceptName(propuri, proptype), 
										createTypedConceptName(avf.getURI(), OntConceptType.CLASS), impliedProperties, this, predicate);
								avftci.setTypeToExprRelationship("restriction to");
								if (isListAnnotatedProperty(prop)) {
									avftci.setRangeValueType(RangeValueType.LIST);
								}
								else if (isTypedListSubclass(avf)) {
									avftci.setTypeCheckType(getTypedListType(avf));
									avftci.setRangeValueType(RangeValueType.LIST);
								}
								return avftci;
							}
						}
						else if (sr.as(OntClass.class).asRestriction().isHasValueRestriction()) {
							RDFNode hvr = sr.as(OntClass.class).asRestriction().asHasValueRestriction().getHasValue();
							TypeCheckInfo hvtci = new TypeCheckInfo(createTypedConceptName(propuri, proptype), 
								hvr, ExplicitValueType.RESTRICTION, this, predicate);
							if (isListAnnotatedProperty(prop)) {
								hvtci.setRangeValueType(RangeValueType.LIST);
							}
							else if (isTypedListSubclass(hvr)) {
								hvtci.setTypeCheckType(getTypedListType(hvr));
								hvtci.setRangeValueType(RangeValueType.LIST);
							}
							return hvtci;
						}
					}
				}
			}
		}
		return null;
	}
	
	private ConceptName getTypedListType(RDFNode node) {
		return modelProcessor.getTypedListType(node);
	}

	private boolean isTypedListSubclass(RDFNode rest) {
		if (rest.isResource()) {
			if (rest.asResource().hasProperty(RDFS.subClassOf, theJenaModel.getResource(SadlConstants.SADL_LIST_MODEL_LIST_URI))) {
				return true;
			}
		}
		return false;
	}

	private boolean isListAnnotatedProperty(Property prop) {
		AnnotationProperty annprop = theJenaModel.getAnnotationProperty(SadlConstants.SADL_LIST_MODEL_RANGE_ANNOTATION_PROPERTY);
		if (annprop != null) {
			if (prop.hasProperty(annprop)) {
				return true;
			}
		}
		return false;
	}

	private TypeCheckInfo getType(Name expression) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		SadlResource qnm =expression.getName();
		if (qnm.eIsProxy()) {
			handleUndefinedFunctions(expression);
		}
		
		//If the expression is a function, find equation definition from name and get the return type
		if(expression.isFunction()){
			TypeCheckInfo ftci = getFunctionType(qnm);
			if (ftci != null) {
				return ftci;
			}
			handleUndefinedFunctions(expression);
		}
		return getType(qnm);
	}
	
	private TypeCheckInfo getFunctionType(SadlResource fsr) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		if(fsr.eContainer() instanceof EquationStatement){
			EquationStatement es = (EquationStatement) fsr.eContainer();
			if(es != null){
				return getType(es.getReturnType());
			}
		}else if(fsr.eContainer() instanceof ExternalEquationStatement){
			ExternalEquationStatement ees = (ExternalEquationStatement)fsr.eContainer();
			if(ees != null){
				return getType(ees.getReturnType());
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
			issueAcceptor.addWarning(SadlErrorMessages.TYPE_CHECK_TRANSLATOR_CLASS_NOT_FOUND.get(getModelProcessor().getConfigMgr().getTranslatorClassName()), expression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.WARNING_MARKER_URI, MetricsProcessor.UNDEFINED_FUNCTION_URI);
			}
		}
		else if(translator.isBuiltinFunctionTypeCheckingAvailable() == SadlConstants.SADL_BUILTIN_FUNCTIONS_TYPE_CHECKING_AVAILABILITY.NAME_ONLY){	
			if(translator.isBuiltinFunction(expressionName)){
				issueAcceptor.addWarning(SadlErrorMessages.TYPE_CHECK_BUILTIN_EXCEPTION.get(expressionName), expression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.WARNING_MARKER_URI, MetricsProcessor.UNDEFINED_FUNCTION_URI);
				}
			}else{
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.RETURN_TYPE_WARNING.get("Function " + expressionName), expression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNDEFINED_FUNCTION_URI);
				}
			}				
		}
		//Either the Reasoner/Translator provides full built-in information or provides nothing. 
		//Regardless, if this point is reached, error.
		else {
			getModelProcessor().addIssueToAcceptor(SadlErrorMessages.RETURN_TYPE_WARNING.get("Function " + expressionName), expression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNDEFINED_FUNCTION_URI);
			}
		}
		throw new DontTypeCheckException();
	}
	
	protected TypeCheckInfo getType(SadlResource qnm) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException{

		
//		ContextBuilder ctxBldr = new ContextBuilder(qnm);
//		Context ctx = ctxBldr.build();
//		SadlOntologyHelper soh = new SadlOntologyHelper();
//		soh.validate(ctx, qnm);
		
		String conceptUri = declarationExtensions.getConceptUri(qnm);
		EObject expression = qnm.eContainer();
		if (conceptUri == null) {
			getModelProcessor().addIssueToAcceptor(SadlErrorMessages.UNIDENTIFIED.toString(), (expression != null ? expression : qnm));
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
			}
		}
		
		OntConceptType conceptType;
		try {
			conceptType = declarationExtensions.getOntConceptType(qnm);
		} catch (CircularDefinitionException e) {
			conceptType = e.getDefinitionType();
			getModelProcessor().addIssueToAcceptor(e.getMessage(), expression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
			}
		}
		if(conceptType.equals(OntConceptType.CLASS) || conceptType.equals(OntConceptType.DATATYPE)){
			ConceptName conceptName = createTypedConceptName(conceptUri, conceptType);
			List<ConceptName> impliedProps = getImpliedProperties(theJenaModel.getResource(conceptUri));
			TypeCheckInfo tci = new TypeCheckInfo(conceptName, conceptName, this, impliedProps, expression);
			return tci;
		}
		else if(conceptType.equals(OntConceptType.DATATYPE_PROPERTY)){
			TypeCheckInfo propcheckinfo = getNameProperty(qnm, ConceptType.DATATYPEPROPERTY, conceptUri, expression);
			if (propcheckinfo != null) {
				return propcheckinfo;
			}
			throw new PropertyWithoutRangeException(declarationExtensions.getConcreteName(qnm));
		}
		else if(conceptType.equals(OntConceptType.CLASS_PROPERTY)){
			TypeCheckInfo propcheckinfo =  getNameProperty(qnm, ConceptType.OBJECTPROPERTY, conceptUri, expression);
			if (propcheckinfo != null) {
				return propcheckinfo;
			}
			throw new PropertyWithoutRangeException(declarationExtensions.getConcreteName(qnm));
		}
		else if (conceptType.equals(OntConceptType.RDF_PROPERTY)) {
			TypeCheckInfo rdfpropcheckinfo = getNameProperty(qnm, ConceptType.RDFPROPERTY, conceptUri, expression);
			if (rdfpropcheckinfo != null) {
				return rdfpropcheckinfo;
			}
			throw new PropertyWithoutRangeException(declarationExtensions.getConcreteName(qnm));
		}
		else if(conceptType.equals(OntConceptType.INSTANCE)){
			// this is an instance--if it is already in the ontology we can get its type. If not maybe we can get it from its declaration
			Individual individual = theJenaModel.getIndividual(conceptUri);
			if(individual == null){
				SadlResource qnmDecl = declarationExtensions.getDeclaration(qnm);
				if (qnmDecl != null) {
					if (qnmDecl.eContainer() instanceof SadlInstance) {
						SadlTypeReference typeref = ((SadlInstance)qnmDecl.eContainer()).getType();
						if (typeref != null) {
							return getType(typeref);
						}
					}
					else if (qnmDecl.eContainer() instanceof SadlMustBeOneOf) {
						if (qnmDecl.eContainer().eContainer() instanceof SadlClassOrPropertyDeclaration) {
							return getType(((SadlClassOrPropertyDeclaration)qnmDecl.eContainer().eContainer()).getClassOrProperty().get(0));
						}
					}
				}
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.UNIDENTIFIED.toString(), expression);
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
						tci = getSadlTypedListTypeCheckInfo(ontResource.as(OntClass.class), null, expression, null);
					}
					else {
						ConceptName declarationConceptName = new ConceptName("TODO");
						declarationConceptName.setType(ConceptType.ONTCLASS);
						tci =  new TypeCheckInfo(instConceptName, instConceptName, this, expression);
					}
				}
				else {
					String uriOfTypeToBeReturned = ontResource.getURI();
					ConceptName conceptName = new ConceptName(uriOfTypeToBeReturned);
					conceptName.setType(ConceptType.ONTCLASS);
					List<ConceptName> impliedProperties = getImpliedProperties(ontResource);
					tci = new TypeCheckInfo(instConceptName, conceptName, this, impliedProperties, expression);
				}
				if (typeitr.hasNext() && compoundTci == null) {
					compoundTci = new TypeCheckInfo(instConceptName, this, expression);
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
			String nm = declarationExtensions.getConcreteName(qnm);
			String uri = declarationExtensions.getConceptUri(qnm);
			TypeCheckInfo tci = getVariableType(ConceptType.VARIABLE, qnm, nm, uri, expression);
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
			return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);
		}
		else if (conceptType.equals(OntConceptType.FUNCTION_DEFN)) {
			return getFunctionType(qnm);
		}
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);
	}
	
	private ConceptName createTypedConceptName(String conceptUri, OntConceptType conceptType) {
		return modelProcessor.createTypedConceptName(conceptUri, conceptType);
	}

	protected TypeCheckInfo getNameProperty(SadlResource qnm, ConceptType propertyType, String conceptUri, EObject expression) throws DontTypeCheckException, InvalidTypeException {
		OntProperty property = theJenaModel.getOntProperty(conceptUri);
		if(property == null){
			getModelProcessor().addIssueToAcceptor(SadlErrorMessages.UNIDENTIFIED.toString(), qnm != null ? qnm : expression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			return null;
		}
		ConceptName propConceptName = new ConceptName(conceptUri);
		propConceptName.setType(propertyType);
		return getTypeInfoFromRange(propConceptName, property, expression);
	}

	private TypeCheckInfo getTypeInfoFromRange(ConceptName propConceptName, Property property,
			EObject expression) throws DontTypeCheckException, InvalidTypeException {
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
						compoundTci.addCompoundType(tci);
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
			EObject expression, ConceptType propertyType) throws InvalidTypeException {
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
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.UNEXPECTED_TYPE_CHECK_ERROR.get("union range", e.getMessage() ), getDefaultContext());
			}
		}
		else if (rng.canAs(IntersectionClass.class)){
			issueAcceptor.addWarning(SadlErrorMessages.TYPE_CHECK_HANDLE_WARNING.get("intersections"), expression);
		}
		else if (rng.canAs(Restriction.class)){
			issueAcceptor.addWarning(SadlErrorMessages.TYPE_CHECK_HANDLE_WARNING.get("restrictions"), expression);
		}
		return tci;
	}

	private TypeCheckInfo getSadlTypedListTypeCheckInfo(OntClass lst, ConceptName propConceptName, EObject expression, ConceptType propertyType) throws InvalidTypeException {
		ExtendedIterator<OntClass> eitr = ((OntClass)lst.as(OntClass.class)).listSuperClasses(true);
		while (eitr.hasNext()) {
			OntClass cls = eitr.next();
			if (cls.isRestriction()) {
				if (cls.canAs(AllValuesFromRestriction.class)) {
					if (((AllValuesFromRestriction)cls.as(AllValuesFromRestriction.class)).onProperty(theJenaModel.getProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
						Resource avf = ((AllValuesFromRestriction)cls.as(AllValuesFromRestriction.class)).getAllValuesFrom();
						eitr.close();
						if (avf.isURIResource()) {
//							List<ConceptName> impliedProperties = getImpliedProperties(avf.asResource());
							List<ConceptName> impliedProperties = null;		// don't impute implied properties when the range is a List
							ConceptName rangeConceptName = new ConceptName(avf.getURI());
							if (propertyType != null && propertyType.equals(ConceptType.DATATYPEPROPERTY)) {
								rangeConceptName.setType(ConceptType.RDFDATATYPE);
								rangeConceptName.setRangeValueType(propConceptName.getRangeValueType());
							}
							else {
								rangeConceptName.setType(ConceptType.ONTCLASS);
							}
							TypeCheckInfo tci = new TypeCheckInfo(propConceptName, rangeConceptName, impliedProperties, this, expression);
							tci.setRangeValueType(RangeValueType.LIST);
							return tci;
						}
					}
				}
			}
		}
		return null;
	}

	private boolean isSadlTypedList(RDFNode node) {
		if (node instanceof Resource && ((Resource)node).canAs(OntClass.class)) {
			OntClass cls = ((Resource)node).as(OntClass.class);
			if (cls.hasSuperClass(theJenaModel.getOntResource(SadlConstants.SADL_LIST_MODEL_LIST_URI))) {
				return true;
			}
		}
		return false;
	}

	private TypeCheckInfo createTypeCheckInfoForPropertyRange(RDFNode first, ConceptName propConceptName,
			EObject expression, ConceptType propertyType) throws InvalidTypeException {
		TypeCheckInfo tci;
		ConceptName rangeConceptName = new ConceptName(first.asResource().getURI());
		if (propertyType.equals(ConceptType.DATATYPEPROPERTY)) {
			rangeConceptName.setType(ConceptType.RDFDATATYPE);
			OntResource range;
			try {
				range = theJenaModel.getOntResource(rangeConceptName.getUri());
				if (theJenaModel.listStatements(range, RDF.type, RDFS.Datatype).hasNext()) {
					// this is a user-defined datatype
					RDFNode rngEC = range.listPropertyValues(OWL.equivalentClass).next();
					if (rngEC != null && rngEC.canAs(OntResource.class)) {
						RDFNode baseType = rngEC.as(OntResource.class).listPropertyValues(OWL2.onDatatype).next();
						if (baseType != null && baseType.isURIResource()) {
							ConceptName baseTypeConceptName = new ConceptName(baseType.asResource().getURI());
							baseTypeConceptName.setType(ConceptType.RDFDATATYPE);
							tci = new TypeCheckInfo(propConceptName, baseTypeConceptName, this, expression);
						}
					}
				}
				else {
					rangeConceptName.setRangeValueType(propConceptName.getRangeValueType());
				}
			} catch (InvalidNameException e) {
				e.printStackTrace();
			}
		}
		else {
			rangeConceptName.setType(ConceptType.ONTCLASS);
		}
		List<ConceptName> impliedProperties = getImpliedProperties(first.asResource());
		tci = new TypeCheckInfo(propConceptName, rangeConceptName, impliedProperties, this, expression);
		if (isTypedListSubclass(first)) {
			tci.setRangeValueType(RangeValueType.LIST);
			if (first.isURIResource()) {
				// looks like a named list in which case we probably have the wrong type
				if (!first.asResource().canAs(OntClass.class)){
					issueAcceptor.addError("Unexpected non-OntClass named list, please report."	, expression); 
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

	protected TypeCheckInfo getVariableType(ConceptType variable, SadlResource sr, String conceptNm, String conceptUri, EObject expression) throws DontTypeCheckException, CircularDefinitionException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		//Needs filled in for Requirements extension
		if (conceptUri == null) {
			return null;
		}
		if (expression instanceof SadlParameterDeclaration) {
			SadlTypeReference exprType = ((SadlParameterDeclaration)expression).getType();
			return getType(exprType);
		}
		else if (expression instanceof SubjHasProp) {
			SadlResource psr = ((SubjHasProp)expression).getProp();
			if (psr.equals(sr)) {
				ConceptName tci = new ConceptName("TODO");
				return new TypeCheckInfo(tci, tci, this, expression);
			}
			TypeCheckInfo ptci = getType(psr);
			return ptci;
		}
		else if (expression instanceof BinaryOperation) {
			if (((BinaryOperation)expression).getLeft() instanceof Name && !!((BinaryOperation)expression).getLeft().equals(sr)) {
				TypeCheckInfo ptci = getType(((BinaryOperation)expression).getRight());
				return ptci;
			}
		}
		else if (expression instanceof SelectExpression) {
			// find name in expression and get type from there
			Expression sexpr = findDefiningExpression(conceptUri, ((SelectExpression)expression).getWhereExpression());
			if (sexpr != null && !sexpr.equals(sr) && !(sexpr instanceof Name && ((Name)sexpr).getName().equals(sr))) {
				return getType(sexpr);
			}
		}
		ConceptName declarationConceptName = new ConceptName(conceptUri);
		declarationConceptName.setType(ConceptType.VARIABLE);
		return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);
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

	private TypeCheckInfo combineTypes(List<String> operations, Expression leftExpression, Expression rightExpression,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException, DontTypeCheckException, InvalidTypeException {
		if(!compareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)){
			return null;
		}
		if (getModelProcessor().isBooleanComparison(operations)) {
			ConceptName booleanLiteralConceptName = new ConceptName(XSD.xboolean.getURI());
			booleanLiteralConceptName.setType(ConceptType.DATATYPEPROPERTY);
			return new TypeCheckInfo(booleanLiteralConceptName, booleanLiteralConceptName, this, leftExpression.eContainer());
		}
		else if (getModelProcessor().isNumericOperator(operations)) {
			ConceptName lcn = getTypeCheckInfoType(leftTypeCheckInfo);
			ConceptName rcn = getTypeCheckInfoType(rightTypeCheckInfo);
			if (lcn == null || lcn.getNamespace() == null) {
				return leftTypeCheckInfo;
			}
			if (rcn == null || rcn.getNamespace() == null) {
				return rightTypeCheckInfo;
			}
			if (rcn.equals(lcn)) {
				return leftTypeCheckInfo;
			}
			ConceptName cn = numericalPrecedenceType(lcn, rcn);
			return new TypeCheckInfo(cn, cn, this, leftExpression.eContainer());
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

	private ConceptName getTypeCheckInfoType(TypeCheckInfo tci) throws InvalidNameException {
		if (tci.getExplicitValueType() != null && tci.getExplicitValueType().equals(ExplicitValueType.VALUE)) {
			if (tci.getExpressionType() instanceof ConceptName) {
				return (ConceptName) tci.getExpressionType();
			}
		}
		if (tci.getTypeCheckType() instanceof ConceptName) {
			return (ConceptName) tci.getTypeCheckType();
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
	 */
	protected boolean compareTypes(List<String> operations, EObject leftExpression, EObject rightExpression,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException, DontTypeCheckException, InvalidTypeException {
		boolean listTemporarilyDisabled = false;
		try {
			if (leftExpression instanceof Constant && 
					(((Constant)leftExpression).getConstant().equals("value") || ((Constant)leftExpression).getConstant().equals("type"))) {
				listTemporarilyDisabled = true;
				leftTypeCheckInfo.setRangeValueType(RangeValueType.CLASS_OR_DT);
			}
			List<TypeCheckInfo> ltciCompound = (leftTypeCheckInfo != null) ? leftTypeCheckInfo.getCompoundTypes() : null;
			if (ltciCompound != null) {
				for (int i = 0; i < ltciCompound.size(); i++) {
					boolean thisResult = compareTypes(operations, leftExpression, rightExpression, ltciCompound.get(i), rightTypeCheckInfo);
					if (thisResult) {
						return true;
					}
				}
				return false;
			}
			List<TypeCheckInfo> rtciCompound = (rightTypeCheckInfo != null) ? rightTypeCheckInfo.getCompoundTypes() : null;
			if (rtciCompound != null) {
				for (int i = 0; i < rtciCompound.size(); i++) {
					boolean thisResult = compareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rtciCompound.get(i));
					if (thisResult) {
						return true;
					}
				}
				return false;
			}
	//		if (leftTypeCheckInfo != null && leftTypeCheckInfo.getExplicitValue() != null && rightTypeCheckInfo != null) {
	//			ConceptIdentifier rExprType = rightTypeCheckInfo.getExpressionType();
	//			if (rExprType instanceof ConceptName) {
	//				ConceptIdentifier lci = getConceptIdentifierFromTypeCheckInfo(leftTypeCheckInfo);
	//				if (!(lci instanceof ConceptName) || !((ConceptName)rExprType).getUri().equals(((ConceptName)lci).getUri())) {
	//					if (rightTypeCheckInfo.getImplicitProperties() == null) {
	//						// no chance of implied properties fixing the problem
	//						return false;
	//					}
	//				}
	//			}
	//		}
			ConceptIdentifier leftConceptIdentifier = leftTypeCheckInfo != null ? getConceptIdentifierFromTypeCheckInfo(leftTypeCheckInfo): null;
			ConceptIdentifier rightConceptIdentifier = rightTypeCheckInfo != null ? getConceptIdentifierFromTypeCheckInfo(rightTypeCheckInfo) : null; 
			if ((leftConceptIdentifier != null && leftConceptIdentifier.toString().equals("None")) || 
					(rightConceptIdentifier != null && rightConceptIdentifier.toString().equals("None")) ||
					(leftConceptIdentifier != null && leftConceptIdentifier.toString().equals("TODO")) || 
					(rightConceptIdentifier != null && rightConceptIdentifier.toString().equals("TODO"))) {
				// Can't type-check on "None" as it represents that it doesn't exist.
				return true;
			}
			else if (leftConceptIdentifier == null) {
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_COMPARISON.toString(), leftExpression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				return false;
			}
			else if(rightConceptIdentifier == null){
				getModelProcessor().addIssueToAcceptor(SadlErrorMessages.TYPE_COMPARISON.toString(), rightExpression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.TYPE_CHECK_FAILURE_URI);
				}
				return false;
			}
			else if (!compatibleTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)) {
				if (isConjunctiveLocalRestriction(leftExpression, rightExpression)) {
					return true;
				}
				if (leftTypeCheckInfo.getImplicitProperties() != null || rightTypeCheckInfo.getImplicitProperties() != null) {
					return compareTypesUsingImpliedProperties(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo);
				}
				return false;
			}
		}
		finally {
			if (listTemporarilyDisabled) {
				leftTypeCheckInfo.setRangeValueType(RangeValueType.LIST);
			}
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
	
	private ConceptIdentifier getConceptIdentifierFromTypeCheckInfo(TypeCheckInfo tci) {
		
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
		return tci.getTypeCheckType();
	}

	private boolean compareTypesUsingImpliedProperties(List<String> operations, EObject leftExpression,
			EObject rightExpression, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException, DontTypeCheckException, InvalidTypeException {
		
		String opstr = (operations != null && operations.size() > 0) ? operations.get(0) : null;
		if (leftTypeCheckInfo.getImplicitProperties() != null) {
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
				if (compareTypes(operations, leftExpression, rightExpression, newltci, rightTypeCheckInfo)) {
					issueAcceptor.addInfo("Implied property '" + getModelProcessor().conceptIdentifierToString(cn) + "' used (left side" + (opstr != null ? (" of '" + opstr + "'") : "") + ") to pass type check", leftExpression);
					addImpliedPropertiesUsed(leftExpression, prop);
					return true;
				}
			}
		}
		else if (rightTypeCheckInfo.getImplicitProperties() != null) {
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
				if (compareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, newrtci)) {
					issueAcceptor.addInfo("Implied property '" + getModelProcessor().conceptIdentifierToString(cn) + "' used (right side" + (opstr != null ? (" of '" + opstr + "'") : "") + ") to pass type check", rightExpression);
					addImpliedPropertiesUsed(rightExpression, prop);
					return true;
				}
			}
		}
		return false;
	}

	private boolean compatibleTypes(List<String> operations, EObject leftExpression, EObject rightExpression,
									TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException, InvalidTypeException{
		
		if ((leftTypeCheckInfo.getRangeValueType() == null && rightTypeCheckInfo.getRangeValueType() != null && !rightTypeCheckInfo.getRangeValueType().equals(RangeValueType.CLASS_OR_DT)) || 
			(leftTypeCheckInfo.getRangeValueType() != null && !leftTypeCheckInfo.getRangeValueType().equals(RangeValueType.CLASS_OR_DT) && rightTypeCheckInfo.getRangeValueType() == null) ||
			(leftTypeCheckInfo.getRangeValueType() != null && rightTypeCheckInfo.getRangeValueType() != null && !(leftTypeCheckInfo.getRangeValueType().equals(rightTypeCheckInfo.getRangeValueType())))) {
			if (!isQualifyingListOperation(operations, leftTypeCheckInfo, rightTypeCheckInfo)) {
				if (isCompatibleListTypes(operations, leftTypeCheckInfo, rightTypeCheckInfo)) {
					return true;
				}
				return false;
			}
		}
		
		ConceptIdentifier leftConceptIdentifier = getConceptIdentifierFromTypeCheckInfo(leftTypeCheckInfo);
		ConceptIdentifier rightConceptIdentifier = getConceptIdentifierFromTypeCheckInfo(rightTypeCheckInfo);
		if (leftConceptIdentifier == null || rightConceptIdentifier == null) {
			return false;
		}
		if (leftConceptIdentifier instanceof ConceptName && rightConceptIdentifier instanceof ConceptName) {
			ConceptName leftConceptName = (ConceptName) leftConceptIdentifier;
			ConceptName rightConceptName = (ConceptName) rightConceptIdentifier;
			
			if (getModelProcessor().isNumericOperator(operations) && (!isNumeric(leftTypeCheckInfo) || !isNumeric(rightTypeCheckInfo))) {
				return false;
			}
			if (leftConceptName.equals(rightConceptName)) {
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
				else {
					return false;
				}
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
					if (rightTypeCheckInfo.getExpressionType() instanceof ConceptName && ((ConceptName)rightTypeCheckInfo.getExpressionType()).getType().equals(ConceptType.INDIVIDUAL)) {
						// here we can do a tighter check
						if (!getModelProcessor().instanceBelongsToClass(theJenaModel, 
								theJenaModel.getOntResource(((ConceptName)rightTypeCheckInfo.getExpressionType()).getUri()),
								theJenaModel.getOntResource(leftConceptName.getUri()))) {
							return false;
						}
					}
					OntClass subcls = theJenaModel.getOntClass(leftConceptName.getUri());
					OntResource supercls = theJenaModel.getOntResource(rightConceptName.getUri());
					if (SadlUtils.classIsSubclassOf(subcls, supercls, true, null)) {
						return true;
					}
					if (SadlUtils.classIsSubclassOf(theJenaModel.getOntClass(rightConceptName.getUri()), theJenaModel.getOntResource(leftConceptName.getUri()), true, null)) {
						return true;
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
				} catch (JenaProcessorException e) {
					e.printStackTrace();
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
				return instanceBelongsToClass(theJenaModel.getIndividual(leftConceptName.getUri()), theJenaModel.getOntClass(rightConceptName.getUri()));
			}
			else if ((leftConceptName.getType().equals(ConceptType.ONTCLASS) && rightConceptName.getType().equals(ConceptType.INDIVIDUAL))){
				return instanceBelongsToClass(theJenaModel.getIndividual(rightConceptName.getUri()), theJenaModel.getOntClass(leftConceptName.getUri()));
			}
			else if ((leftConceptName.getType().equals(ConceptType.INDIVIDUAL) && rightConceptName.getType().equals(ConceptType.INDIVIDUAL))){
				// TODO Is this the right way to compare for two individuals? 
				return instancesHaveCommonType(theJenaModel.getIndividual(leftConceptName.getUri()), theJenaModel.getIndividual(rightConceptName.getUri()));
			}
			else if (leftConceptName.getType().equals(ConceptType.VARIABLE) && isDeclaration(rightExpression)) {
				return true;
			}
		}
		return false;
	}
	
	private boolean isCompatibleListTypes(List<String> operations, TypeCheckInfo leftTypeCheckInfo,
			TypeCheckInfo rightTypeCheckInfo) {
		// TODO Auto-generated method stub
		return false;
	}

	private ConceptName getListType(TypeCheckInfo tci) {
		ConceptIdentifier tct = tci.getTypeCheckType();
		if (tct != null) {
			if (tct instanceof ConceptName) {
				try {
					OntResource cls = theJenaModel.getOntResource(((ConceptName)tct).getUri());
					if (tci.getTypeToExprRelationship().equals("range") || tci.getTypeToExprRelationship().equals("restriction to")) {
						if (cls.isURIResource()) {
//							return new ConceptName(cls.getURI());
							return (ConceptName) tct;
						}
					}
					if (cls != null && cls.canAs(OntClass.class)){
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
					}
				} catch (InvalidNameException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		return null;
	}

	private boolean isDeclaration(EObject expr) {
		if (expr instanceof Declaration) {
			return true;
		}
		if (expr instanceof UnaryExpression && ((UnaryExpression)expr).getExpr() instanceof Declaration) {
			return true;
		}
		return false;
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

	protected boolean isQualifyingListOperation(List<String> operations, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) {
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
		if (subject instanceof SadlResource) {
			org.eclipse.emf.ecore.resource.Resource rsrc = subject.eResource();
			if (rsrc != null) {
				if (ontModel != null) {
					OntConceptType stype;
					try {
						stype = declarationExtensions.getOntConceptType((SadlResource)subject);
						OntResource subj = null;
						String varName = null;
						if (stype.equals(OntConceptType.VARIABLE)) {
							TypeCheckInfo stci;
							// for now don't do any checking--may be able to do so later with variable definitions
							try {
								stci = getType(subject);
								//It's possible that there are local restrictions
								if (stci != null) {
									stci = getApplicableLocalRestriction(stci);
								}
								if (stci != null && stci.getTypeCheckType() != null) {
									subj = ontModel.getOntResource(stci.getTypeCheckType().toString());
									varName = declarationExtensions.getConcreteName((SadlResource)subject);
									if (subj != null) {
										Property prop = ontModel.getProperty(declarationExtensions.getConceptUri(predicate));
										if (prop != null) {
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
								Property prop = ontModel.getProperty(declarationExtensions.getConceptUri(predicate));
								if (prop != null) {
									checkPropertyDomain(ontModel, propsubj, prop, target, propOfSubjectCheck, varName);
								}
							}
						}
						else {
							subj = ontModel.getOntResource(declarationExtensions.getConceptUri((SadlResource)subject));
							if (subj != null) {
								String preduri = declarationExtensions.getConceptUri(predicate);
								if (preduri == null) {
									getModelProcessor().addIssueToAcceptor("Unable to resolve name", predicate);
								}
								else {
									Property prop = ontModel.getProperty(preduri);
									if (prop != null) {
										checkPropertyDomain(ontModel, subj, prop, target, propOfSubjectCheck, varName);
									}
								}
							}
						}
					} catch (CircularDefinitionException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
		}
	}

	protected TypeCheckInfo getApplicableLocalRestriction(TypeCheckInfo stci) {
		return stci;	
	}

	private void checkPropertyDomain(OntModel ontModel, OntResource subj, Property prop, Expression target, boolean propOfSubjectCheck, String varName) throws InvalidTypeException {
		if(prop.canAs(AnnotationProperty.class)){
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
				break;
			}
		}
		//Check for super properties
		stmtitr = prop.listProperties();
		while (stmtitr.hasNext()) {
			if(matchFound){
				break;
			}
			RDFNode obj = stmtitr.nextStatement().getObject();
			if(obj.canAs(Property.class)){
				StmtIterator stmtitr2 = ontModel.listStatements(obj.as(Property.class), RDFS.domain, (RDFNode)null);
				while(stmtitr2.hasNext()){
					RDFNode obj2 = stmtitr2.nextStatement().getObject();
					if (obj2.isResource()) {
						matchFound = checkForPropertyDomainMatch(subj, obj.as(Property.class), obj2.asResource());
					}
					if (matchFound) {
						break;
					}
				}
			}
		}
		stmtitr.close();
		if (subj != null && !matchFound) {
			if (varName != null) {
				if(propOfSubjectCheck){
					getModelProcessor().addIssueToAcceptor(SadlErrorMessages.VARIABLE_NOT_IN_DOMAIN_OF_PROPERTY.get(varName, subj.getURI(),prop.getURI()), target);
				}else{
					issueAcceptor.addWarning(SadlErrorMessages.VARIABLE_NOT_IN_DOMAIN_OF_PROPERTY.get(varName, subj.getURI(),prop.getURI()), target);
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
					getModelProcessor().addIssueToAcceptor(msg, target);
				}else{
					issueAcceptor.addWarning(msg, target);
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

	public boolean checkPropertyValueInRange(OntModel theJenaModel, Expression subj, SadlResource pred, EObject val) throws CircularDefinitionException, DontTypeCheckException, InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, InvalidTypeException, CircularDependencyException, PropertyWithoutRangeException {
		TypeCheckInfo predType = getType(pred);
		TypeCheckInfo valType = getType(val);
		List<String> operations = Arrays.asList("is");
		if (compareTypes(operations , pred, val, predType, valType)) {
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
		return false;
	}

	public boolean checkDataPropertyValueInRange(OntModel theJenaModel2, Resource subj, OntProperty prop, Literal val) {
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
}
