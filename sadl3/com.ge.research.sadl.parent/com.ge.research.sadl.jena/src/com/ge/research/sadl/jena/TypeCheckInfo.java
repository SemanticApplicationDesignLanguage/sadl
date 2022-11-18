/************************************************************************
 * Copyright © 2007-2022 - General Electric Company, All Rights Reserved
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import org.apache.jena.rdf.model.RDFNode;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.diagnostics.Severity;

import com.ge.research.sadl.jena.JenaBasedSadlModelValidator.ExplicitValueType;
import com.ge.research.sadl.model.ConceptIdentifier;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.RangeValueType;
import com.ge.research.sadl.model.gp.ConstantNode;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.TranslationException;

/**
 * This class captures the information about the left or right hand side of an expression that is subject
 * to type checking, e.g., an assignment, a comparison, etc.
 * 
 */
public class TypeCheckInfo {

	// possible values of typeToExprRelationship
	public static final String DOMAIN = "domain";
	public static final String RANGE = "range";
	public static final String SELF = "self";
	public static final String EXPLICIT_VALUE = "explicit value";
	public static final String UNITTED_QUANTITY = "unitted quantity";
	public static final String RESTRICTED_TO = "restriction to";
	public static final String INSTANCE_OF_CLASS = "instance of class";
	public static final String INSTANCE_OF_LIST = "instance of typed list of type";
	public static final String NAMED_LIST_OF_TYPE = "type of a named typed list class";
	public static final String UNAMED_LIST_OF_TYPE = "type of an unnamed typed list class";
	public static final String FUNCTION_RETURN = "function return";
	public static final String FUNCTION_AS_ARGUMENT = "function as argument";

	private EObject context = null;							// the parse tree element from which this is derived, 
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
 
    	private List<TypeCheckInfo> compoundTypes = null;		// This can contain multiple TypeCheckInfo instances for multiple reasons.
    															// 1. If this the type of something that is a disjunction of multiple types, 
    															//    this contains the next lower level of TypeCheckInfos in the hierarchy, 
    															//    e.g., a property whose range is a union of classes or which is given 
    															//    range in multiple imports.
    															// 2. If this is the type of an instance of a List whose members are not all
    															//    of the same type, this is the type of each of the members of the list.
    															// Note that compound type TypeCheckInfo instances can be nested, as in when
    															// the members of a List belong to a union or intersection class.
    	
    	private String typeToExprRelationship = RANGE;			// the relationship between the typeCheckType and the expressionType, e.g., range (the default)
    															//	for explicit UnittedQuantity this will be the units 
    	
    	private String additionalInformation = null;			// any additional information to explain the contents of the instance of the class.
 
    	private NamedNode listElementType = null;				// if the type is a List, this is the type of the elements of the list
    	
    	private Severity severity = null;						// Guidance offered on the severity of any type mismatch involving this instance of the class.

    	private JenaBasedSadlModelValidator validator;
    	
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
    			setTypeToExprRelationship(EXPLICIT_VALUE);
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
			if (rangeValueType.equals(RangeValueType.LIST)) {
				return rangeValueType;
			}
			else if(this.getCompoundTypes() != null){
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
						getValidator().getModelProcessor().addTypeCheckingError("Property '" + tci.getExpressionType() + "' has incompatable Range Types, '" + rvt.toString() + "' and '" + rvt2.toString() + "'", tci.context); //TODO add new error message
					}
				}else{
					rvt = rvt2;
				}
			}
			
			return rvt;
		}
		
		protected void setContext(JenaBasedSadlModelValidator validator, EObject ctx) {
			this.setValidator(validator);
			this.context = ctx;
		}
		
		public EObject getContext() {
			return context;
		}

		public void setRangeValueType(RangeValueType rangeValueType) {
			this.rangeValueType = rangeValueType;
		}
		
		public String toString() {
			StringBuffer sb = new StringBuffer();
			if (compoundTypes != null) {
				sb.append("TypeCheckInfo(");
				if (rangeValueType.equals(RangeValueType.LIST)) {
					// this was generated from an actual list (expressionType is probably also ValueList?)
					sb.append("the List [");
					for (int i = 0; i < compoundTypes.size(); i++) {
						if (i > 0) {
							sb.append(",");
						}
						sb.append(compoundTypes.get(i).toString());
					}
					sb.append("]");
					return sb.toString();
				}
				else if (rangeValueType.equals(RangeValueType.UNION_OF_CLASSES)) {
					for (int i = 0; i < compoundTypes.size(); i++) {
						TypeCheckInfo tci = compoundTypes.get(i);
						if (i == 0) {
							ConceptIdentifier ci = tci.expressionType;
							if (ci instanceof ConceptName) {
								sb.append(((ConceptName)ci).getName());
							}
							else {
								sb.append(tci.expressionType);
							}
							if (typeToExprRelationship != null) {
								sb.append(", instance of class, {");
							}
							else {
								sb.append(", type, {");
							}
							sb.append(tci.typeCheckType.getName());
						}
						else {
							sb.append(" or ");
							sb.append(tci.typeCheckType.getName());
						}
					}
					sb.append("})");
					return sb.toString();
				}
				else if (rangeValueType.equals(RangeValueType.INTERSECTION_OF_CLASSES)) {
					for (int i = 0; i < compoundTypes.size(); i++) {
						TypeCheckInfo tci = compoundTypes.get(i);
						if (i == 0) {
							ConceptIdentifier ci = tci.expressionType;
							if (ci instanceof ConceptName) {
								sb.append(((ConceptName)ci).getName());
							}
							else {
								sb.append(tci.expressionType);
							}
							if (typeToExprRelationship != null) {
								sb.append(", instance of class, {");
							}
							else {
								sb.append(", type, {");
							}
							sb.append(tci.typeCheckType.getName());
						}
						else {
							sb.append(" and ");
							sb.append(tci.typeCheckType.getName());
						}
					}
					sb.append("})");
					return sb.toString();
				}
				else {
					sb.append("Compound TypeCheckInfo");
					if (rangeValueType != null) {
						sb.append(", ");
						sb.append(rangeValueType);
					}
					sb.append("([");
					for (int i = 0; i < compoundTypes.size(); i++) {
						if (i > 0) {
							sb.append(",");
						}
						sb.append(compoundTypes.get(i).toString());
					}
					sb.append("]");
					return sb.toString();
				}
			}
			else {
				sb.append("TypeCheckInfo(");
				if (rangeValueType.equals(RangeValueType.LIST)) {
					if (typeToExprRelationship.equals(INSTANCE_OF_LIST)) {
						if (expressionType instanceof ConceptName) {
							sb.append(((ConceptName)expressionType).getName());
						}
						else if (expressionType != null) {
							sb.append(expressionType.toString());
						}
						else {
							sb.append("unknown instance");
						}
						sb.append(" ");
						sb.append(typeToExprRelationship);
						sb.append(" ");
						sb.append(typeCheckType != null ? typeCheckType.toString() : "unknown type");
						sb.append(")");
						return sb.toString();
					}
					else {
						sb.append(typeCheckType != null ? typeCheckType.toString() : "unknown type");
						sb.append(", ");
						if (typeToExprRelationship != null) {
							sb.append(typeToExprRelationship);
							try {
								if (expressionType instanceof ConceptName &&
										getValidator().getModelProcessor().isProperty(expressionType)) {
									sb.append(", range of property ");
								}
								else {
									sb.append(" ");
								}
							} catch (InvalidTypeException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							} catch (TranslationException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
							if (expressionType instanceof ConceptName) {
								sb.append(((ConceptName)expressionType).getName());
							}
							else {
								sb.append(expressionType.toString());
							}
							sb.append(")");
							return sb.toString();
						}
					}
					sb.delete(15, sb.length() - 1);
				}
//				if (getRangeValueType() != null && !getRangeValueType().equals(RangeValueType.CLASS_OR_DT)) {
//					sb.append(getRangeValueType().toString());
//					sb.append(" of values of type, ");
//				}
				if (expressionType != null) {
					if (expressionType instanceof ConceptName) {
						sb.append(((ConceptName)expressionType).getName());
					}
					else {
						sb.append(expressionType.toString());
					}
					sb.append(", ");
				}
				if (typeToExprRelationship != null) {
					sb.append(typeToExprRelationship);
					sb.append(", ");
				}
				if (typeCheckType != null) {
					if (typeCheckType instanceof ConstantNode && ((ConstantNode)typeCheckType).getName().contentEquals(SadlConstants.CONSTANT_NONE)) {
						sb.append("\"--\" (don't check)");
					}
					else {
						sb.append(typeCheckType.toString());
					}
				}
				else {
					sb.append("unknown type");
				}
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

		public void addCompoundType(TypeCheckInfo additionalType) {
			if (compoundTypes == null) {
				compoundTypes= new ArrayList<TypeCheckInfo>();
			}
			if (!compoundTypes.contains(additionalType)) {
				compoundTypes.add(additionalType);
			}
		}

		String getTypeToExprRelationship() {
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

		public String getAdditionalInformation() {
			return additionalInformation;
		}

		public void setAdditionalInformation(String additionalInformation) {
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

		public NamedNode getListElementType() {
			return listElementType;
		}

		public void setListElementType(NamedNode listElementType) {
			this.listElementType = listElementType;
		}

		private JenaBasedSadlModelValidator getValidator() {
			return validator;
		}

		private void setValidator(JenaBasedSadlModelValidator validator) {
			this.validator = validator;
		}
 
}
