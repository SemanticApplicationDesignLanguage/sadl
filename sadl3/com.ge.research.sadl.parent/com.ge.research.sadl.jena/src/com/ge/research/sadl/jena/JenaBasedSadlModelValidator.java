package com.ge.research.sadl.jena;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EObject;

import com.ge.research.sadl.model.CircularDefinitionException;
import com.ge.research.sadl.model.ConceptIdentifier;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.ConceptName.RangeValueType;
import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.PrefixNotFoundException;
import com.ge.research.sadl.processing.ISadlModelValidator;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.reasoner.CircularDependencyException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.BinaryOperation;
import com.ge.research.sadl.sADL.BooleanLiteral;
import com.ge.research.sadl.sADL.Constant;
import com.ge.research.sadl.sADL.Declaration;
import com.ge.research.sadl.sADL.ElementInList;
import com.ge.research.sadl.sADL.EquationStatement;
import com.ge.research.sadl.sADL.Expression;
import com.ge.research.sadl.sADL.Name;
import com.ge.research.sadl.sADL.NumberLiteral;
import com.ge.research.sadl.sADL.PropOfSubject;
import com.ge.research.sadl.sADL.SadlDataType;
import com.ge.research.sadl.sADL.SadlIntersectionType;
import com.ge.research.sadl.sADL.SadlParameterDeclaration;
import com.ge.research.sadl.sADL.SadlPrimitiveDataType;
import com.ge.research.sadl.sADL.SadlPropertyCondition;
import com.ge.research.sadl.sADL.SadlResource;
import com.ge.research.sadl.sADL.SadlSimpleTypeReference;
import com.ge.research.sadl.sADL.SadlTypeReference;
import com.ge.research.sadl.sADL.SadlUnionType;
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
import com.hp.hpl.jena.rdf.model.NodeIterator;
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
	private List<String> comparisonOperators = Arrays.asList(">=",">","<=","<","==","!=","is","=","not","unique","in","contains","does",/*"not",*/"contain");
	private List<String> numericOperators = Arrays.asList("*","+","/","-","%","^");
	private List<String> canBeNumericOperators = Arrays.asList(">=",">","<=","<","==","!=","is","=");
	private EObject defaultContext;
	
	protected Map<EObject, TypeCheckInfo> expressionsValidated = new HashMap<EObject,TypeCheckInfo>();
	private Map<EObject, Property> impliedPropertiesUsed = null;
	
	private IMetricsProcessor metricsProcessor = null;
	protected JenaBasedSadlModelProcessor sadlModelProcessor = null; 

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
    			try {
	    			if (context.equals(((TypeCheckInfo)o).context)) {
	    				if (getExpressionType().equals(((TypeCheckInfo)o).getExpressionType()) &&
	    						getRangeValueType().equals(((TypeCheckInfo)o).getRangeValueType()) &&
	    						getTypeCheckType() != null && getTypeCheckType().equals(((TypeCheckInfo)o).getTypeCheckType())) {
	    					return true;
	    				}
	    			}
    			}
    			catch (NullPointerException e) {
    				
    			}
    		}
    		return false;
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
		}
		
		public RangeValueType getRangeValueType() {
			return rangeValueType;
		}
		
		protected void setContext(JenaBasedSadlModelValidator validator, EObject ctx) {
			this.context = ctx;
			if (ctx != null) {
				validator.expressionsValidated.put(ctx, this);
			}
		}

		protected void setRangeValueType(RangeValueType rangeValueType) {
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
				sb.append(getImplicitProperties().get(0).toFQString());
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
	
	public JenaBasedSadlModelValidator(ValidationAcceptor issueAcceptor, OntModel theJenaModel, DeclarationExtensions declarationExtensions, JenaBasedSadlModelProcessor processor, IMetricsProcessor metricsProcessor){
		this.issueAcceptor = issueAcceptor;
		this.theJenaModel = theJenaModel;
		this.declarationExtensions = declarationExtensions;
		this.sadlModelProcessor = processor ;
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
		List<String> operations = Arrays.asList(expression.getOp().split("\\s+"));
		
		if(skipOperations(operations)){
			return true;
		}
		
		try {	
			TypeCheckInfo leftTypeCheckInfo = getType(leftExpression);
			TypeCheckInfo rightTypeCheckInfo = getType(rightExpression);
			if (leftTypeCheckInfo == null && rightTypeCheckInfo == null) {
				// this condition happens when a file is loaded in the editor and clean/build is invoked
				return true;
			}
			if(!compareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)){
				if (expression.eContainer() instanceof TestStatement && isQuery(leftExpression)) {
					// you can't tell what type a query will return
					return true;
				}
				createErrorMessage(errorMessageBuilder, leftTypeCheckInfo, rightTypeCheckInfo, expression.getOp());
				return false;
			}
			if (leftExpression instanceof PropOfSubject && rightExpression instanceof Declaration) {
				TypeCheckInfo subjtype = getType(((PropOfSubject)leftExpression).getRight());
				ConceptIdentifier subject = subjtype.getTypeCheckType();
				if (subject != null) {
					addLocalRestriction(subjtype.getTypeCheckType().toString(), leftTypeCheckInfo, rightTypeCheckInfo);
				}
			}
			return true;
		} catch (Throwable t) {
			return handleValidationException(expression, t);
		}
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

	public void addLocalRestriction(String subjuri, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) {
		try {
			throw new Exception("addLocalRestriction not implemented");
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private boolean handleValidationException(EObject expr, Throwable t) {
		if (t instanceof InvalidNameException) {
			issueAcceptor.addError("An invalid name exception occurred while type-checking this expression.", expr);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			t.printStackTrace();
		} else if (t instanceof TranslationException) {
			issueAcceptor.addError("A translation exception exception occurred while type-checking this expression.", expr);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			t.printStackTrace();
		} else if (t instanceof URISyntaxException) {
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			issueAcceptor.addError("An URI syntax exception occurred while type-checking this expression.", expr);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			t.printStackTrace();
		} else if (t instanceof IOException) {
			issueAcceptor.addError("An IO exception occurred while type-checking this expression.", expr);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			t.printStackTrace();
		} else if (t instanceof ConfigurationException) {
			issueAcceptor.addError("A configuration exception occurred while type-checking this expression.", expr);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			t.printStackTrace();
		} else if (t instanceof NullPointerException){
			issueAcceptor.addError("A null pointer exception occurred while type-checking this expression.", expr);
		} else if (t instanceof DontTypeCheckException) {
			return true;
		} else if (t instanceof CircularDefinitionException) {
			// TODO Auto-generated catch block
			t.printStackTrace();
		}
		else {
			t.printStackTrace();
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
			issueAcceptor.addError("An invalid name exception occurred while type-checking this expression.", rightExpression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.INVALID_EXPRESSION_URI);
			}
			e.printStackTrace();
		} catch (DontTypeCheckException e) {
			return true;
		}
		return true;
	}

	private void createErrorMessage(StringBuilder errorMessageBuilder, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo, String operation) {
		String[] leftDesc = getTypeCheckInfoDescription(leftTypeCheckInfo);
		String[] rightDesc = getTypeCheckInfoDescription(rightTypeCheckInfo);
		
		if (leftDesc == null) {
			errorMessageBuilder.append("Undetermined left type");
		}
		else {
			if (leftDesc.length > 0) {
				errorMessageBuilder.append(leftDesc[0]);
			}
			if (leftDesc.length > 1) {
				errorMessageBuilder.append(" ");
				errorMessageBuilder.append(leftDesc[1]);
			}
		}
		if (comparisonOperators.contains(operation)) {
			errorMessageBuilder.append(" cannot be compared (" + operation + ") with ");
		}
		else {
			errorMessageBuilder.append(" cannot operate (" + operation + ") with ");
		}
		if (rightDesc == null) {
			errorMessageBuilder.append("Undetermined right type");
		}
		else {
			if (rightDesc.length > 0) {
				errorMessageBuilder.append(rightDesc[0]);
			}
			if (rightDesc.length > 1) {
				errorMessageBuilder.append(" ");
				errorMessageBuilder.append(rightDesc[1]);
			}
		}
		errorMessageBuilder.append(".");
	}

	private String[] getTypeCheckInfoDescription(TypeCheckInfo typeCheckInfo) {
		if (typeCheckInfo == null) {
			String[] result = new String[1];
			result[0] = "No type check info generated";
			return result;
		}
		else {
			StringBuilder sb1 = new StringBuilder();;
			StringBuilder sb2 = null;
			ConceptIdentifier typeExpr = typeCheckInfo.getExpressionType();
			sb1.append(typeExpr.toString());
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
					sb2.append(typeCheckInfo.getTypeCheckType().toString());
				}
				else if (typeCheckInfo.getExplicitValue() != null) {
					RDFNode ev = typeCheckInfo.getExplicitValue();
					if (typeCheckInfo.getExplicitValueType().equals(ExplicitValueType.VALUE)) {
						sb1.replace(0, sb1.length(), "explicit value ");
					}
					if (ev.isLiteral()) {
						try {
							sb2.append(ev.asLiteral().getValue().toString());
						}
						catch (DatatypeFormatException e) {
							issueAcceptor.addError(e.getMessage(), typeCheckInfo.context);
						}
					}
					else {
						sb2.append(ev.toString());
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

	protected TypeCheckInfo getType(EObject expression) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException{
		if (expressionsValidated.containsKey(expression)) {
			return expressionsValidated.get(expression);
		}
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
				issueAcceptor.addError("Unexpected null type error", expression);
			}
			//Need to return passing case for time being
//			ConceptName declarationConceptName = new ConceptName("TODO");
//			return new TypeCheckInfo(declarationConceptName, declarationConceptName);
		}
		else if(expression instanceof StringLiteral){
			ConceptName stringLiteralConceptName = new ConceptName(XSD.xstring.getURI());
			stringLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			return new TypeCheckInfo(stringLiteralConceptName, stringLiteralConceptName, this, expression);
		}
		else if(expression instanceof NumberLiteral || expression instanceof Unit){
			BigDecimal value;
			Literal litval;
			if (expression instanceof Unit) { 
				value = ((Unit)expression).getValue().getValue();
				String unit = ((Unit)expression).getUnit();
				ConceptName uqcn = new ConceptName(JenaBasedSadlModelProcessor.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
				List<ConceptName> impliedProperties = getImpliedProperties(theJenaModel.getOntResource(uqcn.getUri()));
				if (impliedProperties != null) {
					return new TypeCheckInfo(uqcn, uqcn, impliedProperties, this, expression);
				}
				else {
					return new TypeCheckInfo(uqcn, uqcn, this, expression);
				}
			}
			else {
				value = ((NumberLiteral)expression).getValue();
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
			return new TypeCheckInfo(numberLiteralConceptName, litval, ExplicitValueType.VALUE, this, expression);
		}
		else if(expression instanceof BooleanLiteral){
			ConceptName booleanLiteralConceptName = new ConceptName(XSD.xboolean.getURI());
			booleanLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			return new TypeCheckInfo(booleanLiteralConceptName, booleanLiteralConceptName, this, expression);
		}
		else if(expression instanceof Constant){
			return getType((Constant)expression);
		}
		else if(expression instanceof ValueTable){
			ConceptName declarationConceptName = new ConceptName("TODO");
			return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);
		}
		else if(expression instanceof PropOfSubject){
			return getType((PropOfSubject)expression);
		}
		else if(expression instanceof SubjHasProp){
			return getType(((SubjHasProp)expression).getLeft());
		}
		else if(expression instanceof UnaryExpression){
			return getType(((UnaryExpression) expression).getExpr());
		}
		else if(expression instanceof ElementInList){
			Expression el = ((ElementInList)expression).getElement();
			if (el instanceof PropOfSubject) {
				TypeCheckInfo listtype = getType(((PropOfSubject)el).getRight());
				if (listtype == null) {
					issueAcceptor.addError("Unable to get the List type", el);
				}
				else if (listtype.getRangeValueType() != RangeValueType.LIST) {
					issueAcceptor.addError("Expected a List", el);
				}
				else {
					// the element's type is the type of the list but not necessarily a list
					listtype.setRangeValueType((listtype.getTypeCheckType() != null && listtype.getTypeCheckType() instanceof ConceptName) ? ((ConceptName)listtype.getTypeCheckType()).getRangeValueType() : RangeValueType.CLASS_OR_DT);
				}
				return listtype;
			}
			else {
				issueAcceptor.addError("Unhandled element type in element in list construct: " + el.getClass().getCanonicalName() + "; please report", expression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
				}
			}
		}
		else if(expression instanceof BinaryOperation){
			List<String> operations = Arrays.asList(((BinaryOperation) expression).getOp().split("\\s+"));
			TypeCheckInfo leftTypeCheckInfo = getType(((BinaryOperation) expression).getLeft());
			TypeCheckInfo rightTypeCheckInfo = getType(((BinaryOperation) expression).getRight());
			if (leftTypeCheckInfo != null && isVariable(leftTypeCheckInfo) && ((BinaryOperation)expression).getRight() instanceof Declaration) {
				return rightTypeCheckInfo;
			}
			TypeCheckInfo binopreturn = combineTypes(operations, ((BinaryOperation) expression).getLeft(), ((BinaryOperation) expression).getRight(), 
					leftTypeCheckInfo, rightTypeCheckInfo);
			if (binopreturn != null) {
				return binopreturn;
			}
			if (isNumericOperator(((BinaryOperation) expression).getOp())) {
				ConceptName decimalLiteralConceptName = new ConceptName(XSD.decimal.getURI());
				decimalLiteralConceptName.setType(ConceptType.RDFDATATYPE);
				return new TypeCheckInfo(decimalLiteralConceptName, decimalLiteralConceptName, this, expression);
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
				issueAcceptor.addError("expected a List", ((Sublist)expression).getList());
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
		else {
			throw new TranslationException("Unhandled expression type: " + expression.getClass().getCanonicalName());
		}
		
		issueAcceptor.addError("This expression cannot be decomposed into a known type", expression);
		if (metricsProcessor != null) {
			metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
		}
		return null;
	}
	
	protected TypeCheckInfo getType(Constant expression) throws DontTypeCheckException {
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
		else if (constant.endsWith("value")) {
			throw new DontTypeCheckException();
		}
		else if(constant.equals("None")){
			ConceptName constantConceptName = new ConceptName(constant);
			constantConceptName.setType(ConceptType.INDIVIDUAL);
			return new TypeCheckInfo(constantConceptName, constantConceptName, this, expression);
		}
		else {
			// let any subclass validators do their thing
			return getType((Constant)expression);
		}

	}

	private boolean isVariable(TypeCheckInfo tci) {
		ConceptIdentifier ci = tci.getTypeCheckType();
		if (ci instanceof ConceptName && ((ConceptName)ci).getType() != null && ((ConceptName)ci).getType().equals(ConceptType.VARIABLE)) {
			return true;
		}
		return false;
	}

	private TypeCheckInfo getType(SadlTypeReference expression) throws DontTypeCheckException, CircularDefinitionException {
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
		issueAcceptor.addError("Unexpected type reference type: " + expression.getClass().getCanonicalName(), expression);
		if (metricsProcessor != null) {
			metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
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

	private TypeCheckInfo getType(SadlSimpleTypeReference expression) throws DontTypeCheckException, CircularDefinitionException {
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

	private TypeCheckInfo getType(PropOfSubject expression) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException {
		String ofOp = expression.getOf();
		Expression predicate = expression.getLeft();
		Expression subject = expression.getRight();
		
		if (predicate instanceof Constant) {
			String cnstval = ((Constant)predicate).getConstant();
			TypeCheckInfo subjtype = null;
			if (constantRequiresListNext(cnstval)) {
				subjtype = getType(subject);
				if (subjtype != null && !subjtype.getRangeValueType().equals(RangeValueType.LIST)) {
					issueAcceptor.addError("'" + cnstval + "' must be applied to a List ('" + getTypeCheckTypeString(subjtype) + "' is not a List)", subject);
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
			else if (cnstval.equals("first element") || cnstval.equals("last element")) {
				subjtype.setRangeValueType(RangeValueType.CLASS_OR_DT);   	// keep type but change from List to reflect this is an element of the list
				return subjtype;
			}
			else {
				issueAcceptor.addError("Unhandled constant property", expression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
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
					String preduri = declarationExtensions.getConceptUri(((Name)predicate).getName());
					issueAcceptor.addError("expected a property in property chain", predicate);
				}
			} catch (CircularDefinitionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
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
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvalidTypeException e) {
				// TODO Auto-generated catch block
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
		TypeCheckInfo predicateType = getType(predicate);
		if (subject instanceof PropOfSubject) {
			checkEmbeddedPropOfSubject(subject, predicate);	
			//TODO figure out how to add effective range for propOfSubj		
		}else if(predicateType != null && predicateType.getTypeCheckType() != null){
			//add interface range
			addEffectiveRange(predicateType, subject);
		}
		return predicateType;
	}
	
	private void addEffectiveRange(TypeCheckInfo predicateType, Expression subject){
		if(metricsProcessor != null){
			if (subject instanceof Name) {
				String className = declarationExtensions.getConceptUri(((Name) subject).getName());
				String propertyName = predicateType.getExpressionType().toString();
				String rangeStr = predicateType.getTypeCheckType().toString();
				boolean isList = predicateType.getRangeValueType().equals(RangeValueType.LIST);
				metricsProcessor.addEffectiveRangeAndDomain(null, className, propertyName, rangeStr, isList);
			}
			else {
				int i = 0;	// TODO 
			}
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

	private void checkEmbeddedPropOfSubject(Expression subject, Expression predicate) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException {
		if (predicate instanceof Name) {
			String propuri = declarationExtensions.getConceptUri(((Name)predicate).getName());
			OntConceptType oct = declarationExtensions.getOntConceptType(((Name)predicate).getName());
			if (oct.equals(OntConceptType.ANNOTATION_PROPERTY)) {
				issueAcceptor.addWarning("Unable to do domain-range matching on an annotation property", predicate);
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
							// TODO Auto-generated catch block
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
					issueAcceptor.addError(errorMessageBuilder.toString(), predicate);
				} catch (CircularDefinitionException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
	}
	
	private List<OntClass> getTypeCheckTypeClasses(TypeCheckInfo tci) {
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

	private TypeCheckInfo getTypeFromRestriction(Expression subject, Expression predicate) throws CircularDefinitionException {
		if (subject instanceof Name && predicate instanceof Name) {
			String subjuri = declarationExtensions.getConceptUri(((Name)subject).getName());
			Resource subj = theJenaModel.getResource(subjuri);
			if (subj != null) {
				if (!(subj instanceof OntClass || subj.canAs(OntClass.class)) && subj.canAs(Individual.class)) {
					subj = subj.as(Individual.class).getRDFType(true);
				}
				if (subj != null && subj.canAs(OntClass.class)){ 
					String propuri = declarationExtensions.getConceptUri(((Name)predicate).getName());
					Property prop = theJenaModel.getProperty(propuri);
					// now look for restrictions on "range"
					StmtIterator sitr = theJenaModel.listStatements(null, OWL.onProperty, prop);
					while (sitr.hasNext()) {
						Statement stmt = sitr.nextStatement();
						Resource sr = stmt.getSubject();
						if (sr.canAs(OntClass.class) && subj.as(OntClass.class).hasSuperClass(sr.as(OntClass.class))) {
							if (sr.as(OntClass.class).asRestriction().isAllValuesFromRestriction()) {
								Resource avf = sr.as(OntClass.class).asRestriction().asAllValuesFromRestriction().getAllValuesFrom();
								if (avf.isLiteral()) {
									TypeCheckInfo avftci =  new TypeCheckInfo(createTypedConceptName(propuri, declarationExtensions.getOntConceptType(((Name)predicate).getName())), 
											createTypedConceptName(avf.getURI(), OntConceptType.CLASS), this, predicate);
									avftci.setTypeToExprRelationship("restriction to");
									return avftci;
								}
								else if (avf.isURIResource()){
									List<ConceptName> impliedProperties = getImpliedProperties(avf);
									TypeCheckInfo avftci = new TypeCheckInfo(createTypedConceptName(propuri, declarationExtensions.getOntConceptType(((Name)predicate).getName())), 
											createTypedConceptName(avf.getURI(), OntConceptType.CLASS), impliedProperties, this, predicate);
									avftci.setTypeToExprRelationship("restriction to");
									if (isListAnnotatedProperty(prop)) {
										avftci.setRangeValueType(RangeValueType.LIST);
									}
									return avftci;
								}
							}
							else if (sr.as(OntClass.class).asRestriction().isHasValueRestriction()) {
								RDFNode hvr = sr.as(OntClass.class).asRestriction().asHasValueRestriction().getHasValue();
								TypeCheckInfo hvtci = new TypeCheckInfo(createTypedConceptName(propuri, declarationExtensions.getOntConceptType(((Name)predicate).getName())), 
									hvr, ExplicitValueType.RESTRICTION, this, predicate);
								if (isListAnnotatedProperty(prop)) {
									hvtci.setRangeValueType(RangeValueType.LIST);
								}
								return hvtci;
							}
						}
					}
				}
			}
		}
		return null;
	}

	private boolean isListAnnotatedProperty(Property prop) {
		AnnotationProperty annprop = theJenaModel.getAnnotationProperty(JenaBasedSadlModelProcessor.LIST_RANGE_ANNOTATION_PROPERTY);
		if (annprop != null) {
			if (prop.hasProperty(annprop)) {
				return true;
			}
		}
		return false;
	}

	private TypeCheckInfo getType(Name expression) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException, CircularDefinitionException {
		SadlResource qnm =expression.getName();
		if (qnm.eIsProxy()) {
			// this is a proxy so we don't know its type
			issueAcceptor.addWarning("Function is not defined so return type is unknown, can't do type checking", expression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.WARNING_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			throw new DontTypeCheckException();
		}
		
		//If the expression is a function, find equation definition from name and get the return type
		if(expression.isFunction()){
			if(qnm.eContainer() instanceof EquationStatement){
				EquationStatement es = (EquationStatement) qnm.eContainer();
				if(es != null){
					return getType(es.getReturnType());
				}
			}
		}
		
		return getType(qnm);
	}
	
	protected TypeCheckInfo getType(SadlResource qnm) throws DontTypeCheckException, CircularDefinitionException{
		String conceptUri = declarationExtensions.getConceptUri(qnm);
		EObject expression = qnm.eContainer();
		if (conceptUri == null) {
			issueAcceptor.addError("Unidentified expression", (expression != null ? expression : qnm));
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
		}
		
		OntConceptType conceptType;
		try {
			conceptType = declarationExtensions.getOntConceptType(qnm);
		} catch (CircularDefinitionException e) {
			conceptType = e.getDefinitionType();
			issueAcceptor.addError(e.getMessage(), expression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
		}
		if(conceptType.equals(OntConceptType.CLASS)){
			ConceptName conceptName = createTypedConceptName(conceptUri, conceptType);
			List<ConceptName> impliedProps = getImpliedProperties(theJenaModel.getResource(conceptUri));
			TypeCheckInfo tci = new TypeCheckInfo(conceptName, conceptName, this, impliedProps, expression);
			return tci;
		}
		else if(conceptType.equals(OntConceptType.DATATYPE_PROPERTY)){
			return getNameProperty(ConceptType.DATATYPEPROPERTY, conceptUri, expression);
		}
		else if(conceptType.equals(OntConceptType.CLASS_PROPERTY)){
			return getNameProperty(ConceptType.OBJECTPROPERTY, conceptUri, expression);
		}
		else if (conceptType.equals(OntConceptType.RDF_PROPERTY)) {
			TypeCheckInfo rdfpropcheckinfo = getNameProperty(ConceptType.RDFPROPERTY, conceptUri, expression);
			if (rdfpropcheckinfo != null) {
				return rdfpropcheckinfo;
			}
			throw new DontTypeCheckException();
		}
		else if(conceptType.equals(OntConceptType.INSTANCE)){
			//Direct type to which the instance belongs
			Individual individual = theJenaModel.getIndividual(conceptUri);
			if(individual == null){
				issueAcceptor.addError("Unidentified expression", expression);
				if (metricsProcessor != null) {
					metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
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
			return tci;
		}
		else if(conceptType.equals(OntConceptType.VARIABLE)){
			String nm = declarationExtensions.getConcreteName(qnm);
			String uri = declarationExtensions.getConceptUri(qnm);
			return getVariableType(ConceptType.VARIABLE, nm, uri, expression);

		}
		else if(conceptType.equals(OntConceptType.ANNOTATION_PROPERTY)){
			//This matches any type.
			ConceptName declarationConceptName = new ConceptName("TODO");
			return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);
		}
		
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);
	}
	
	private ConceptName createTypedConceptName(String conceptUri, OntConceptType conceptType) {
		ConceptName cn = new ConceptName(conceptUri);
		if (conceptType.equals(OntConceptType.CLASS)) {
			cn.setType(ConceptType.ONTCLASS);
		}
		else if (conceptType.equals(OntConceptType.ANNOTATION_PROPERTY)) {
			cn.setType(ConceptType.ANNOTATIONPROPERTY);
		}
		else if (conceptType.equals(OntConceptType.DATATYPE_PROPERTY)) {
			cn.setType(ConceptType.DATATYPEPROPERTY);
		}
		else if (conceptType.equals(OntConceptType.INSTANCE)) {
			cn.setType(ConceptType.INDIVIDUAL);
		}
		else if (conceptType.equals(OntConceptType.CLASS_PROPERTY)) {
			cn.setType(ConceptType.OBJECTPROPERTY);
		}
		else if (conceptType.equals(OntConceptType.DATATYPE)) {
			cn.setType(ConceptType.RDFDATATYPE);
		}
		else if (conceptType.equals(OntConceptType.RDF_PROPERTY)) {
			cn.setType(ConceptType.RDFPROPERTY);
		}
		else if (conceptType.equals(OntConceptType.VARIABLE)) {
			cn.setType(ConceptType.VARIABLE);
		}
		else if (conceptType.equals(OntConceptType.FUNCTION_DEFN)) {
			cn.setType(ConceptType.FUNCTION_DEFN);
		}
		return cn;
	}

	protected TypeCheckInfo getNameProperty(ConceptType propertyType, String conceptUri, EObject expression) throws DontTypeCheckException {
		OntProperty property = theJenaModel.getOntProperty(conceptUri);
		if(property == null){
			issueAcceptor.addError("Unidentified expression", expression);
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
			EObject expression) throws DontTypeCheckException {
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
				if (psuper.isLiteral()) {
					TypeCheckInfo superTCInfo = getNameProperty(propertyType, psuper.asResource().getURI(), expression);
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
			EObject expression, ConceptType propertyType) {
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
				issueAcceptor.addError("Unexpected error processing type check for union range: " + e.getMessage(), getDefaultContext());
			}
		}
		else if (rng.canAs(IntersectionClass.class)){
			issueAcceptor.addWarning("type checking doesn't handle intersection", expression);
		}
		else if (rng.canAs(Restriction.class)){
			issueAcceptor.addWarning("type checking doesn't handle restrictions", expression);
		}
		return tci;
	}

	private TypeCheckInfo getSadlTypedListTypeCheckInfo(OntClass lst, ConceptName propConceptName, EObject expression, ConceptType propertyType) {
		ExtendedIterator<OntClass> eitr = ((OntClass)lst.as(OntClass.class)).listSuperClasses(true);
		while (eitr.hasNext()) {
			OntClass cls = eitr.next();
			if (cls.isRestriction()) {
				if (cls.canAs(AllValuesFromRestriction.class)) {
					if (((AllValuesFromRestriction)cls.as(AllValuesFromRestriction.class)).onProperty(theJenaModel.getProperty(JenaBasedSadlModelProcessor.SADL_LIST_MODEL_FIRST_URI))) {
						Resource avf = ((AllValuesFromRestriction)cls.as(AllValuesFromRestriction.class)).getAllValuesFrom();
						eitr.close();
						if (avf.isURIResource()) {
							List<ConceptName> impliedProperties = getImpliedProperties(avf.asResource());
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
			if (cls.hasSuperClass(theJenaModel.getOntResource(JenaBasedSadlModelProcessor.SADL_LIST_MODEL_LIST_URI))) {
				return true;
			}
		}
		return false;
	}

	private TypeCheckInfo createTypeCheckInfoForPropertyRange(RDFNode first, ConceptName propConceptName,
			EObject expression, ConceptType propertyType) {
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
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		else {
			rangeConceptName.setType(ConceptType.ONTCLASS);
		}
		List<ConceptName> impliedProperties = getImpliedProperties(first.asResource());
		tci = new TypeCheckInfo(propConceptName, rangeConceptName, impliedProperties, this, expression);
		return tci;
	}

	private List<ConceptName> getImpliedProperties(Resource first) {
		List<ConceptName> retlst = null;
		// check superclasses
		if (first.canAs(OntClass.class)) {
			OntClass ontcls = first.as(OntClass.class);
			ExtendedIterator<OntClass> eitr = ontcls.listSuperClasses();
			while (eitr.hasNext()) {
				OntClass supercls = eitr.next();
				List<ConceptName> scips = getImpliedProperties(supercls);
				if (scips != null) {
					if (retlst == null) {
						retlst = scips;
					}
					else {
						retlst.addAll(scips);
					}
				}
			}
		}
		StmtIterator sitr = theJenaModel.listStatements(first, theJenaModel.getProperty(JenaBasedSadlModelProcessor.SADL_IMPLICIT_MODEL_IMPLIED_PROPERTY_URI), (RDFNode)null);
		if (sitr.hasNext()) {
			if (retlst == null) {
				retlst = new ArrayList<ConceptName>();
			}
			while (sitr.hasNext()) {
				RDFNode obj = sitr.nextStatement().getObject();
				if (obj.isURIResource()) {
					retlst.add(new ConceptName(obj.asResource().getURI()));
				}
			}
			return retlst;
		}
		return retlst;
	}

	private boolean isRangeKlugyDATASubclass(OntResource rsrc) {
		if (rsrc.getURI().endsWith("#DATA")) {
			return true;
		}
		if (rsrc.canAs(OntClass.class)){
			ExtendedIterator<OntClass> itr = rsrc.as(OntClass.class).listSuperClasses();
			while (itr.hasNext()) {
				OntClass spr = itr.next();
				if (spr.isURIResource() && spr.getURI().endsWith("#DATA")) {
					return true;
				}
			}
		}
		return false;
	}

	protected TypeCheckInfo getVariableType(ConceptType variable, String conceptNm, String conceptUri, EObject expression) throws DontTypeCheckException, CircularDefinitionException {
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
			TypeCheckInfo ptci = getType(psr);
			return ptci;
		}
		ConceptName declarationConceptName = new ConceptName(conceptUri);
		declarationConceptName.setType(ConceptType.VARIABLE);
		return new TypeCheckInfo(declarationConceptName, declarationConceptName, this, expression);
	}
	
	private TypeCheckInfo combineTypes(List<String> operations, Expression leftExpression, Expression rightExpression,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException, DontTypeCheckException {
		if(!compareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)){
			return null;
		}
		if (isBooleanComparison(operations)) {
			ConceptName booleanLiteralConceptName = new ConceptName(XSD.xboolean.getURI());
			booleanLiteralConceptName.setType(ConceptType.DATATYPEPROPERTY);
			return new TypeCheckInfo(booleanLiteralConceptName, booleanLiteralConceptName, this, leftExpression.eContainer());
		}
		else if (isNumericOperator(operations)) {
			ConceptName lcn = getTypeCheckInfoType(leftTypeCheckInfo);
			ConceptName rcn = getTypeCheckInfoType(rightTypeCheckInfo);
			if (lcn == null) {
				return leftTypeCheckInfo;
			}
			if (rcn == null) {
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

	protected boolean isBooleanComparison(List<String> operations) {
		if(comparisonOperators.containsAll(operations)){
			return true;
		}
		return false;
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
	 */
	private boolean compareTypes(List<String> operations, EObject leftExpression, EObject rightExpression,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException, DontTypeCheckException {
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
			//TODO
			return true;
		}
		else if (leftConceptIdentifier == null) {
			issueAcceptor.addError("Type comparison not possible", leftExpression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			return false;
		}
		else if(rightConceptIdentifier == null){
			issueAcceptor.addError("Type comparison not possible", rightExpression);
			if (metricsProcessor != null) {
				metricsProcessor.addMarker(null, MetricsProcessor.ERROR_MARKER_URI, MetricsProcessor.UNCLASSIFIED_FAILURE_URI);
			}
			return false;
		}
		else if (!compatibleTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)) {
			if (leftTypeCheckInfo.getImplicitProperties() != null || rightTypeCheckInfo.getImplicitProperties() != null) {
				return compareTypesUsingImpliedProperties(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo);
			}
			return false;
		}
		return true;
	}
	
	private ConceptIdentifier getConceptIdentifierFromTypeCheckInfo(TypeCheckInfo tci) {
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
			EObject rightExpression, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException, DontTypeCheckException {
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
					issueAcceptor.addInfo("Implied property '" + cn.getUri() + "' used (left side) to pass type check", leftExpression);
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
					issueAcceptor.addInfo("Implied property '" + cn.getUri() + "' used (right side) to pass type check", rightExpression);
					addImpliedPropertiesUsed(rightExpression, prop);
					return true;
				}
			}
		}
		return false;
	}

	private boolean compatibleTypes(List<String> operations, EObject leftExpression, EObject rightExpression,
									TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException{
		
		if ((leftTypeCheckInfo.getRangeValueType() == null && rightTypeCheckInfo.getRangeValueType() != null && !rightTypeCheckInfo.getRangeValueType().equals(RangeValueType.CLASS_OR_DT)) || 
			(leftTypeCheckInfo.getRangeValueType() != null && !leftTypeCheckInfo.getRangeValueType().equals(RangeValueType.CLASS_OR_DT) && rightTypeCheckInfo.getRangeValueType() == null) ||
			(leftTypeCheckInfo.getRangeValueType() != null && rightTypeCheckInfo.getRangeValueType() != null && !(leftTypeCheckInfo.getRangeValueType().equals(rightTypeCheckInfo.getRangeValueType())))) {
			if (!isQualifyingListOperation(operations, leftTypeCheckInfo, rightTypeCheckInfo)) {
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
			
			if (leftConceptName.equals(rightConceptName)) {
				return true;
			}
			else if ((isNumericOperator(operations) || canBeNumericOperator(operations)) && 
				(isNumericType(leftConceptName) && isNumericType(rightConceptName))) {
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
						if (!sadlModelProcessor.instanceBelongsToClass(theJenaModel, 
								theJenaModel.getOntResource(((ConceptName)rightTypeCheckInfo.getExpressionType()).getUri()),
								theJenaModel.getOntResource(leftConceptName.getUri()))) {
							return false;
						}
					}
					if (SadlUtils.classIsSubclassOf(theJenaModel.getOntClass(leftConceptName.getUri()), theJenaModel.getOntResource(rightConceptName.getUri()), true, null)) {
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
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (JenaProcessorException e) {
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

	private boolean canBeNumericOperator(List<String> operations) {
		Iterator<String> itr = operations.iterator();
		while (itr.hasNext()) {
			if (canBeNumericOperator(itr.next())) return true;
		}
		return false;
	}
	
	private boolean canBeNumericOperator(String op) {
		if (canBeNumericOperators.contains(op)) return true;
		return false;
	}

	private boolean isNumericOperator(String op) {
		if (numericOperators.contains(op)) return true;
		return false;
	}

	private boolean isNumericOperator(List<String> operations) {
		Iterator<String> itr = operations.iterator();
		while (itr.hasNext()) {
			if (isNumericOperator(itr.next())) return true;
		}
		return false;
	}

	private boolean isNumericType(ConceptName conceptName) {
		try {
			if (conceptName.getUri().equals(XSD.decimal.getURI()) ||
					conceptName.getUri().equals(XSD.integer.getURI()) ||
					conceptName.getUri().equals(XSD.xdouble.getURI()) ||
					conceptName.getUri().equals(XSD.xfloat.getURI()) ||
					conceptName.getUri().equals(XSD.xint.getURI()) ||
					conceptName.getUri().equals(XSD.xlong.getURI())) {
				return true;
			}
		} catch (InvalidNameException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return false;
	}

	protected boolean isQualifyingListOperation(List<String> operations, TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) {
		if (operations.contains("contain") || operations.contains("contains") && leftTypeCheckInfo != null && 
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

}
