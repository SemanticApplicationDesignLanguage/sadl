package com.ge.research.sadl.jena;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.ecore.EObject;

import com.ge.research.sadl.model.ConceptIdentifier;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.ConceptName.RangeValueType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.processing.ISadlModelValidator;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.sADL.BinaryOperation;
import com.ge.research.sadl.sADL.BooleanLiteral;
import com.ge.research.sadl.sADL.Constant;
import com.ge.research.sadl.sADL.Declaration;
import com.ge.research.sadl.sADL.ElementInList;
import com.ge.research.sadl.sADL.Expression;
import com.ge.research.sadl.sADL.Function;
import com.ge.research.sadl.sADL.Name;
import com.ge.research.sadl.sADL.NumberLiteral;
import com.ge.research.sadl.sADL.PropOfSubject;
import com.ge.research.sadl.sADL.SadlDataType;
import com.ge.research.sadl.sADL.SadlIntersectionType;
import com.ge.research.sadl.sADL.SadlPrimitiveDataType;
import com.ge.research.sadl.sADL.SadlPropertyCondition;
import com.ge.research.sadl.sADL.SadlResource;
import com.ge.research.sadl.sADL.SadlSimpleTypeReference;
import com.ge.research.sadl.sADL.SadlTypeReference;
import com.ge.research.sadl.sADL.SadlUnionType;
import com.ge.research.sadl.sADL.StringLiteral;
import com.ge.research.sadl.sADL.SubjHasProp;
import com.ge.research.sadl.sADL.UnaryExpression;
import com.ge.research.sadl.sADL.ValueTable;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.IntersectionClass;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.UnionClass;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.XSD;

public class JenaBasedSadlModelValidator implements ISadlModelValidator {
	protected ValidationAcceptor issueAcceptor = null;
	protected OntModel theJenaModel = null;
	private DeclarationExtensions declarationExtensions = null;
	private List<String> comparisonOperators = Arrays.asList(">=",">","<=","<","==","!=","is","not","unique","in","contains","does",/*"not",*/"contain");
	private EObject defaultContext;
	
	public class TypeCheckInfo {
    	private ConceptIdentifier expressionType = null;
    	private ConceptIdentifier typeCheckType = null;
    	private RangeValueType rangeValueType = null;
    	
    	public TypeCheckInfo(ConceptIdentifier eType) {
    		setExpressionType(eType);
    	}
    	
    	public TypeCheckInfo(ConceptIdentifier eType, ConceptIdentifier tcType) {
    		setExpressionType(eType);
    		setTypeCheckType(tcType);
    	}

		public ConceptIdentifier getExpressionType() {
			return expressionType;
		}

		public void setExpressionType(ConceptIdentifier expressionType) {
			this.expressionType = expressionType;
		}

		private ConceptIdentifier getTypeCheckType() {
			return typeCheckType;
		}

		private void setTypeCheckType(ConceptIdentifier typeCheckType) {
			this.typeCheckType = typeCheckType;
		}
		
		protected RangeValueType getRangeValueType() {
			return rangeValueType;
		}

		protected void setRangeValueType(RangeValueType rangeValueType) {
			this.rangeValueType = rangeValueType;
		}
		
		public String toString() {
			StringBuffer sb = new StringBuffer("TypeCheckInfo(");
			if (getRangeValueType() != null && !getRangeValueType().equals(RangeValueType.CLASS_OR_DT)) {
				sb.append(getRangeValueType().toString());
				sb.append(" of values of type, ");
			}
			sb.append(expressionType.toString());
			sb.append(", ");
			sb.append(typeCheckType.toString());
			sb.append(")");
			return sb.toString();
		}
    }
	
	public JenaBasedSadlModelValidator(ValidationAcceptor issueAcceptor, OntModel theJenaModel, DeclarationExtensions declarationExtensions){
		this.issueAcceptor = issueAcceptor;
		this.theJenaModel = theJenaModel;
		this.declarationExtensions = declarationExtensions;
	}
	
	public boolean validate(BinaryOperation expression) {
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
			return compareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo);
		} catch (InvalidNameException e) {
			issueAcceptor.addError("An invalid name exception occurred while type-checking this expression.", expression);
			e.printStackTrace();
		} catch (TranslationException e) {
			issueAcceptor.addError("A translation exception exception occurred while type-checking this expression.", expression);
			e.printStackTrace();
		} catch (URISyntaxException e) {
			issueAcceptor.addError("An URI syntax exception occurred while type-checking this expression.", expression);
			e.printStackTrace();
		} catch (IOException e) {
			issueAcceptor.addError("An IO exception occurred while type-checking this expression.", expression);
			e.printStackTrace();
		} catch (ConfigurationException e) {
			issueAcceptor.addError("A configuration exception occurred while type-checking this expression.", expression);
			e.printStackTrace();
		} catch (NullPointerException e){
			//issueAcceptor.addError("A null pointer exception occurred while type-checking this expression.", expression);
		} catch (DontTypeCheckException e) {
			return true;
		}
		return false;
	}

	protected boolean skipOperations(List<String> operations) {
		if(operations.contains("and") || operations.contains("or")){
			return true;
		}
		return false;
	}

	protected TypeCheckInfo getType(Expression expression) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException{
		if(expression instanceof Name){
			return getType((Name)expression);
		}
		else if(expression instanceof Declaration){
			SadlTypeReference decltype = ((Declaration)expression).getType();
			return getType(decltype);
			//Need to return passing case for time being
//			ConceptName declarationConceptName = new ConceptName("TODO");
//			return new TypeCheckInfo(declarationConceptName, declarationConceptName);
		}
		else if(expression instanceof StringLiteral){
			ConceptName stringLiteralConceptName = new ConceptName(XSD.xstring.getURI());
			stringLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			return new TypeCheckInfo(stringLiteralConceptName, stringLiteralConceptName);
		}
		else if(expression instanceof NumberLiteral){
			ConceptName numberLiteralConceptName = new ConceptName(XSD.decimal.getURI());
			numberLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			return new TypeCheckInfo(numberLiteralConceptName, numberLiteralConceptName);
		}
		else if(expression instanceof BooleanLiteral){
			ConceptName booleanLiteralConceptName = new ConceptName(XSD.xboolean.getURI());
			booleanLiteralConceptName.setType(ConceptType.RDFDATATYPE);
			return new TypeCheckInfo(booleanLiteralConceptName, booleanLiteralConceptName);
		}
		else if(expression instanceof Constant){
			//What do we do about the rest of the constants?
			/*'--' | 'a'? 'type' ;*/
			String constant = ((Constant) expression).getConstant();	
			if(constant.equals("PI") || constant.equals("length") || constant.equals("count") ||
			   constant.equals("index")){
				ConceptName constantConceptName = new ConceptName(XSD.decimal.getURI());
				constantConceptName.setType(ConceptType.DATATYPEPROPERTY);
				return new TypeCheckInfo(constantConceptName, constantConceptName);
			}
			else if(constant.contains("element") && (constant.contains("first") || constant.contains("last"))){
				//Handle list types???
				ConceptName declarationConceptName = new ConceptName("TODO");
				return new TypeCheckInfo(declarationConceptName, declarationConceptName);
			}
			else if(constant.equals("None")){
				ConceptName constantConceptName = new ConceptName(constant);
				constantConceptName.setType(ConceptType.INDIVIDUAL);
				return new TypeCheckInfo(constantConceptName, constantConceptName);
			}
		}
		else if(expression instanceof Function){
			ConceptName declarationConceptName = new ConceptName("TODO");
			return new TypeCheckInfo(declarationConceptName, declarationConceptName);
		}
		else if(expression instanceof ValueTable){
			ConceptName declarationConceptName = new ConceptName("TODO");
			return new TypeCheckInfo(declarationConceptName, declarationConceptName);
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
			return getType(((ElementInList)expression).getElement());
		}
		else if(expression instanceof BinaryOperation){
			List<String> operations = Arrays.asList(((BinaryOperation) expression).getOp().split("\\s+"));
			TypeCheckInfo leftTypeCheckInfo = getType(((BinaryOperation) expression).getLeft());
			TypeCheckInfo rightTypeCheckInfo = getType(((BinaryOperation) expression).getRight());
			return combineTypes(operations, ((BinaryOperation) expression).getLeft(), ((BinaryOperation) expression).getRight(), 
					leftTypeCheckInfo, rightTypeCheckInfo);
		}
		
		issueAcceptor.addError("This expression cannot be decomposed into a known type", expression);
		return null;
	}

	private TypeCheckInfo getType(SadlTypeReference expression) throws DontTypeCheckException {
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
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName);
	}
	
	private TypeCheckInfo getType(SadlIntersectionType expression) {
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName);		
	}

	private TypeCheckInfo getType(SadlPrimitiveDataType expression) {
		return getType(expression.getPrimitiveType());
	}

	private TypeCheckInfo getType(SadlDataType primitiveType) {
		String nm = primitiveType.getName();
		ConceptName cn = new ConceptName(XSD.getURI() + nm);
		return new TypeCheckInfo(cn, cn);
	}

	private TypeCheckInfo getType(SadlPropertyCondition expression) {
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName);		
	}

	private TypeCheckInfo getType(SadlSimpleTypeReference expression) throws DontTypeCheckException {
		return getType(expression.getType());
	}

	private TypeCheckInfo getType(SadlUnionType expression) {
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName);		
	}

	private TypeCheckInfo getType(PropOfSubject expression) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException{
		List<String> operations = Collections.<String>emptyList();
		TypeCheckInfo predicateTypeCheckInfo = null;
		TypeCheckInfo subjectTypeCheckInfo = null;
		Expression predicate = expression.getLeft();
		Expression subject = expression.getRight();
		
		if (predicate instanceof Constant) {
			String cnstval = ((Constant)predicate).getConstant();
			if (cnstval.equals("length")) {
			}
			else if (cnstval.equals("count")) {
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject)subject).getLeft();
					subject = ((PropOfSubject)subject).getRight();
				}
			}
			else if (cnstval.equals("index")) {
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject)subject).getLeft();
					subject = ((PropOfSubject)subject).getRight();
				}
			}
			else if (cnstval.equals("first element")) {
			}
			else if (cnstval.equals("last element")) {
			}
			else {
				issueAcceptor.addError("Unhandled constant property", expression);
			}
		}
		
		return predicateTypeCheckInfo = getType(predicate);
//		subjectTypeCheckInfo = getType(subject);
//		return combineTypes(operations, predicate, subject, 
//				predicateTypeCheckInfo, subjectTypeCheckInfo);
	}
	
	private TypeCheckInfo getType(Name expression) throws InvalidNameException, TranslationException, URISyntaxException, IOException, ConfigurationException, DontTypeCheckException {
		SadlResource qnm =expression.getName();
		return getType(qnm);
	}
	
	protected TypeCheckInfo getType(SadlResource qnm) throws DontTypeCheckException{
		String conceptUri = declarationExtensions.getConceptUri(qnm);
		EObject expression = qnm.eContainer();
		if (conceptUri == null) {
			issueAcceptor.addError("Unidentified expression", expression);
		}
		
		OntConceptType conceptType = declarationExtensions.getOntConceptType(qnm);
		if(conceptType.equals(OntConceptType.CLASS)){
			ConceptName conceptName = new ConceptName(conceptUri);
			conceptName.setType(ConceptType.ONTCLASS);
			return new TypeCheckInfo(conceptName, conceptName);
		}
		else if(conceptType.equals(OntConceptType.DATATYPE_PROPERTY)){
			return getNameProperty(ConceptType.DATATYPEPROPERTY, conceptUri, expression);
		}
		else if(conceptType.equals(OntConceptType.CLASS_PROPERTY)){
			return getNameProperty(ConceptType.OBJECTPROPERTY, conceptUri, expression);
		}
		else if(conceptType.equals(OntConceptType.INSTANCE)){
			//Direct type to which the instance belongs
			Individual individual = theJenaModel.getIndividual(conceptUri);
			if(individual == null){
				issueAcceptor.addError("Unidentified expression", expression);
				return null;
			}
			ConceptName instConceptName = new ConceptName(conceptUri);
			instConceptName.setType(ConceptType.INDIVIDUAL);
			Resource ontResource = individual.getRDFType(true);
			if(!ontResource.isURIResource()){
				//Unhandled condition
				//TODO
				ConceptName declarationConceptName = new ConceptName("TODO");
				return new TypeCheckInfo(declarationConceptName, declarationConceptName);
			}
			String uriOfTypeToBeReturned = ontResource.getURI();
			ConceptName conceptName = new ConceptName(uriOfTypeToBeReturned);
			conceptName.setType(ConceptType.ONTCLASS);
			return new TypeCheckInfo(instConceptName, conceptName);
		}
		else if(conceptType.equals(OntConceptType.VARIABLE)){
			return getVariableType(ConceptType.VARIABLE, conceptUri, expression);

		}
		else if(conceptType.equals(OntConceptType.ANNOTATION_PROPERTY)){
			//This matches any type.
			ConceptName declarationConceptName = new ConceptName("TODO");
			return new TypeCheckInfo(declarationConceptName, declarationConceptName);
		}
		
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName);
	}
	
	protected TypeCheckInfo getNameProperty(ConceptType propertyType, String conceptUri, EObject expression) {
		OntProperty property = theJenaModel.getOntProperty(conceptUri);
		if(property == null){
			issueAcceptor.addError("Unidentified expression", expression);
			return null;
		}
		ConceptName propConceptName = new ConceptName(conceptUri);
		propConceptName.setType(propertyType);
		ExtendedIterator<? extends OntResource> pIterator = property.listRange();
		if(pIterator.hasNext()){
			OntResource first = pIterator.next();
			if(first.getURI() != null){
				ConceptName rangeConceptName = new ConceptName(first.getURI());
				if (propertyType.equals(ConceptType.DATATYPEPROPERTY)) {
					rangeConceptName.setType(ConceptType.RDFDATATYPE);
					rangeConceptName.setRangeValueType(propConceptName.getRangeValueType());
				}
				else {
					rangeConceptName.setType(ConceptType.ONTCLASS);
				}
				return new TypeCheckInfo(propConceptName, rangeConceptName);
			}
		}
		
		return null;
	}

	protected TypeCheckInfo getVariableType(ConceptType variable, String conceptUri, EObject expression) throws DontTypeCheckException {
		//Needs filled in for Requirements extension
		ConceptName declarationConceptName = new ConceptName("TODO");
		return new TypeCheckInfo(declarationConceptName, declarationConceptName);
	}
	
	private TypeCheckInfo combineTypes(List<String> operations, Expression leftExpression, Expression rightExpression,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException {
		if(!compareTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)){
			return null;
		}
		
		if(comparisonOperators.containsAll(operations)){
			ConceptName booleanLiteralConceptName = new ConceptName(XSD.xboolean.getURI());
			booleanLiteralConceptName.setType(ConceptType.DATATYPEPROPERTY);
			return new TypeCheckInfo(booleanLiteralConceptName, booleanLiteralConceptName);
		}
		else{
			return leftTypeCheckInfo;
		}
	}

	private boolean compareTypes(List<String> operations, Expression leftExpression, Expression rightExpression,
			TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException {
		ConceptIdentifier leftConceptIdentifier = leftTypeCheckInfo != null ? leftTypeCheckInfo.getTypeCheckType(): null;
		ConceptIdentifier rightConceptIdentifier = rightTypeCheckInfo != null ? rightTypeCheckInfo.getTypeCheckType() : null; 
		if (leftConceptIdentifier == null) {
			issueAcceptor.addError("Type comparison not possible", leftExpression);
			return false;
		}
		else if(rightConceptIdentifier == null){
			issueAcceptor.addError("Type comparison not possible", rightExpression);
			return false;
		}
		else if (leftConceptIdentifier.toString().equals("None") || rightConceptIdentifier.toString().equals("None") ||
				 leftConceptIdentifier.toString().equals("TODO") || rightConceptIdentifier.toString().equals("TODO")) {
			// Can't type-check on "None" as it represents that it doesn't exist.
			//TODO
			return true;
		}
		else if (!compatibleTypes(operations, leftExpression, rightExpression, leftTypeCheckInfo, rightTypeCheckInfo)) {
			return false;
		}
		return true;
	}

	private boolean compatibleTypes(List<String> operations, Expression leftExpression, Expression rightExpression,
									TypeCheckInfo leftTypeCheckInfo, TypeCheckInfo rightTypeCheckInfo) throws InvalidNameException{
		
		if ((leftTypeCheckInfo.getRangeValueType() == null && rightTypeCheckInfo.getRangeValueType() != null && !rightTypeCheckInfo.getRangeValueType().equals(RangeValueType.CLASS_OR_DT)) || 
			(leftTypeCheckInfo.getRangeValueType() != null && !leftTypeCheckInfo.getRangeValueType().equals(RangeValueType.CLASS_OR_DT) && rightTypeCheckInfo.getRangeValueType() == null) ||
			(leftTypeCheckInfo.getRangeValueType() != null && rightTypeCheckInfo.getRangeValueType() != null && !(leftTypeCheckInfo.getRangeValueType().equals(rightTypeCheckInfo.getRangeValueType())))) {
			return false;
		}
		
		ConceptIdentifier leftConceptIdentifier = leftTypeCheckInfo.getTypeCheckType();
		ConceptIdentifier rightConceptIdentifier = rightTypeCheckInfo.getTypeCheckType();
		if (leftConceptIdentifier == null || rightConceptIdentifier == null) {
			return false;
		}
		if (leftConceptIdentifier instanceof ConceptName && rightConceptIdentifier instanceof ConceptName) {
			ConceptName leftConceptName = (ConceptName) leftConceptIdentifier;
			ConceptName rightConceptName = (ConceptName) rightConceptIdentifier;
			
			if (leftConceptName.equals(rightConceptName)) {
				return true;
			}
			else if (leftConceptName.getType().equals(ConceptType.RDFDATATYPE) &&
					  rightConceptName.getType().equals(ConceptType.RDFDATATYPE)) {
				if(leftConceptName.getUri().equals(rightConceptName.getUri())){
					return true;
				}
				else if(isDecimal(leftConceptName) && isInteger(rightConceptName)){
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
				//How do we determine if either is a sub/super class of the other?
				if(leftConceptName.getUri().equals(rightConceptName.getUri())){
					return true;
				}
				if (classIsSubclassOf(theJenaModel.getOntClass(leftConceptName.getUri()), theJenaModel.getOntResource(rightConceptName.getUri()), true)) {
					return true;
				}
			}
			else if ((leftConceptName.getType().equals(ConceptType.INDIVIDUAL) &&
					rightConceptName.getType().equals(ConceptType.ONTCLASS)) ||
					(leftConceptName.getType().equals(ConceptType.ONTCLASS) &&
					rightConceptName.getType().equals(ConceptType.INDIVIDUAL))){
				if(leftConceptName.getUri().equals(rightConceptName.getUri())){
					return true;
				}
			}
		}
		
		return false;
	}
	
	/**
	 * return true if the first argument class is a subclass of the second
	 * argument class
	 * 
	 * @param subcls
	 * @param cls
	 * @return
	 */
	public boolean classIsSubclassOf(OntClass subcls, OntResource cls, boolean rootCall) {
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
						issueAcceptor.addError("Unexpected error during deep validation: apparent Union Class does not return operands.", getDefaultContext());
					}
				}
			}
		}
		try {
			if (cls.canAs(OntClass.class)) {
				ExtendedIterator<OntClass> eitr = cls.as(OntClass.class).listSubClasses();
				while (eitr.hasNext()) {
					OntClass subClsOfCls = eitr.next();
					if (subClsOfCls.equals(subcls)) {
						eitr.close();
						return true;
					}
					else {
						if (classIsSubclassOf(subcls, subClsOfCls, false)) {
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
						issueAcceptor.addError("Unexpected error during deep validation: apparent Intersection Class does not return operands.", getDefaultContext());
					}
				}
			}
// TODO We need to look for equivalent classes that provide a definition for a subclass, 
//			e.g. Component is equivalent to System is class, (System and connectedTo someValueFrom Network) => Component subclass of System.
			if (cls.canAs(OntClass.class)) {
//				SELECT ?eqClass 
//						WHERE {?class owl:equivalentClass ?eqClass}
				ExtendedIterator<OntClass> eqitr = cls.as(OntClass.class).listEquivalentClasses();
				while (eqitr.hasNext()) {
					OntClass eqcls = eqitr.next();
					if (classIsSubclassOf(subcls, eqcls, false)) {
						return true;
					}
				}
			}
//			if (subcls.hasSuperClass(cls, false)) {  // this doesn't work, don't know why awc 6/8/2012
//				return true;
//			}
//			else {
//				if (subcls.canAs(OntClass.class)) {
//					ExtendedIterator<OntClass> eitr = subcls.as(OntClass.class).listSuperClasses(false);
//					while (eitr.hasNext()) {
//						OntClass sprcls = eitr.next();
//						if (sprcls.equals(cls)) {
//							return true;
//						}
//					}
//				}
//			}
		} catch (Throwable t) {
			t.printStackTrace();
		}
		return false;
	}


	
	private boolean isInteger(ConceptIdentifier type) throws InvalidNameException {
		if (type instanceof ConceptName) {
			String uri = ((ConceptName)type).getUri();
			if (uri.equals(XSD.integer.getURI())) {
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
}
