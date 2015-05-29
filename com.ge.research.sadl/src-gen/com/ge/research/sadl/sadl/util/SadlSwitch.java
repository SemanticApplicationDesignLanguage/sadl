/**
 */
package com.ge.research.sadl.sadl.util;

import com.ge.research.sadl.sadl.AdditionalPropertyInfo;
import com.ge.research.sadl.sadl.AddlClassInfo;
import com.ge.research.sadl.sadl.AllValuesCondition;
import com.ge.research.sadl.sadl.AllValuesFrom;
import com.ge.research.sadl.sadl.AskQueryExpression;
import com.ge.research.sadl.sadl.BinaryOpExpression;
import com.ge.research.sadl.sadl.CardCondition;
import com.ge.research.sadl.sadl.Cardinality;
import com.ge.research.sadl.sadl.ClassDeclaration;
import com.ge.research.sadl.sadl.ComplementOfClass;
import com.ge.research.sadl.sadl.Condition;
import com.ge.research.sadl.sadl.ConstructExpression;
import com.ge.research.sadl.sadl.ContentList;
import com.ge.research.sadl.sadl.DataTypeRestriction;
import com.ge.research.sadl.sadl.DefaultValue;
import com.ge.research.sadl.sadl.DisjointClasses;
import com.ge.research.sadl.sadl.Display;
import com.ge.research.sadl.sadl.ElementSet;
import com.ge.research.sadl.sadl.EmbeddedInstanceDeclaration;
import com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom;
import com.ge.research.sadl.sadl.EnumeratedAllValuesFrom;
import com.ge.research.sadl.sadl.EnumeratedInstances;
import com.ge.research.sadl.sadl.EquivalentConcepts;
import com.ge.research.sadl.sadl.ExistentialNegation;
import com.ge.research.sadl.sadl.ExistingInstanceAttribution;
import com.ge.research.sadl.sadl.ExistingResourceList;
import com.ge.research.sadl.sadl.Explanation;
import com.ge.research.sadl.sadl.ExplicitValue;
import com.ge.research.sadl.sadl.Expr;
import com.ge.research.sadl.sadl.Expression;
import com.ge.research.sadl.sadl.Facets;
import com.ge.research.sadl.sadl.FunctionalProperty;
import com.ge.research.sadl.sadl.GraphPattern;
import com.ge.research.sadl.sadl.HasValue;
import com.ge.research.sadl.sadl.HasValueCondition;
import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.InstAttrPSV;
import com.ge.research.sadl.sadl.InstAttrSPV;
import com.ge.research.sadl.sadl.InstanceDeclaration;
import com.ge.research.sadl.sadl.InstanceDeclarationStatement;
import com.ge.research.sadl.sadl.InstanceDifferentFrom;
import com.ge.research.sadl.sadl.InstancesAllDifferent;
import com.ge.research.sadl.sadl.IntersectionResource;
import com.ge.research.sadl.sadl.IntervalValue;
import com.ge.research.sadl.sadl.InverseFunctionalProperty;
import com.ge.research.sadl.sadl.InverseProperty;
import com.ge.research.sadl.sadl.IsInverseOf;
import com.ge.research.sadl.sadl.JunctionExpression;
import com.ge.research.sadl.sadl.LiteralList;
import com.ge.research.sadl.sadl.LiteralValue;
import com.ge.research.sadl.sadl.MaxCardCondition;
import com.ge.research.sadl.sadl.MaxCardinality;
import com.ge.research.sadl.sadl.MergedTriples;
import com.ge.research.sadl.sadl.MinCardCondition;
import com.ge.research.sadl.sadl.MinCardinality;
import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.sadl.ModelElement;
import com.ge.research.sadl.sadl.ModelName;
import com.ge.research.sadl.sadl.NecessaryAndSufficient;
import com.ge.research.sadl.sadl.OfPatternReturningValues;
import com.ge.research.sadl.sadl.OfPhrase;
import com.ge.research.sadl.sadl.OrderElement;
import com.ge.research.sadl.sadl.OrderList;
import com.ge.research.sadl.sadl.PropOfSubj;
import com.ge.research.sadl.sadl.PropValPartialTriple;
import com.ge.research.sadl.sadl.PropertyDeclaration;
import com.ge.research.sadl.sadl.PropertyOfClass;
import com.ge.research.sadl.sadl.Query;
import com.ge.research.sadl.sadl.Range;
import com.ge.research.sadl.sadl.RangeType;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceByRestriction;
import com.ge.research.sadl.sadl.ResourceBySetOp;
import com.ge.research.sadl.sadl.ResourceIdentifier;
import com.ge.research.sadl.sadl.ResourceList;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.Rule;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.SelectExpression;
import com.ge.research.sadl.sadl.SomeValuesCondition;
import com.ge.research.sadl.sadl.SomeValuesFrom;
import com.ge.research.sadl.sadl.Statement;
import com.ge.research.sadl.sadl.SubTypeOf;
import com.ge.research.sadl.sadl.SubjProp;
import com.ge.research.sadl.sadl.SymmetricalProperty;
import com.ge.research.sadl.sadl.Test;
import com.ge.research.sadl.sadl.TransitiveProperty;
import com.ge.research.sadl.sadl.TypeDeclaration;
import com.ge.research.sadl.sadl.TypedBNode;
import com.ge.research.sadl.sadl.UnaryOpExpression;
import com.ge.research.sadl.sadl.UnionResource;
import com.ge.research.sadl.sadl.UserDefinedDataType;
import com.ge.research.sadl.sadl.ValueRow;
import com.ge.research.sadl.sadl.ValueTable;
import com.ge.research.sadl.sadl.VariableList;
import com.ge.research.sadl.sadl.WithChain;
import com.ge.research.sadl.sadl.WithPhrase;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.Switch;

/**
 * <!-- begin-user-doc -->
 * The <b>Switch</b> for the model's inheritance hierarchy.
 * It supports the call {@link #doSwitch(EObject) doSwitch(object)}
 * to invoke the <code>caseXXX</code> method for each class of the model,
 * starting with the actual class of the object
 * and proceeding up the inheritance hierarchy
 * until a non-null result is returned,
 * which is the result of the switch.
 * <!-- end-user-doc -->
 * @see com.ge.research.sadl.sadl.SadlPackage
 * @generated
 */
public class SadlSwitch<T> extends Switch<T>
{
  /**
   * The cached model package
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected static SadlPackage modelPackage;

  /**
   * Creates an instance of the switch.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SadlSwitch()
  {
    if (modelPackage == null)
    {
      modelPackage = SadlPackage.eINSTANCE;
    }
  }

  /**
   * Checks whether this is a switch for the given package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @parameter ePackage the package in question.
   * @return whether this is a switch for the given package.
   * @generated
   */
  @Override
  protected boolean isSwitchFor(EPackage ePackage)
  {
    return ePackage == modelPackage;
  }

  /**
   * Calls <code>caseXXX</code> for each class of the model until one returns a non null result; it yields that result.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the first non-null result returned by a <code>caseXXX</code> call.
   * @generated
   */
  @Override
  protected T doSwitch(int classifierID, EObject theEObject)
  {
    switch (classifierID)
    {
      case SadlPackage.MODEL:
      {
        Model model = (Model)theEObject;
        T result = caseModel(model);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.MODEL_NAME:
      {
        ModelName modelName = (ModelName)theEObject;
        T result = caseModelName(modelName);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.IMPORT:
      {
        Import import_ = (Import)theEObject;
        T result = caseImport(import_);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.MODEL_ELEMENT:
      {
        ModelElement modelElement = (ModelElement)theEObject;
        T result = caseModelElement(modelElement);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.STATEMENT:
      {
        Statement statement = (Statement)theEObject;
        T result = caseStatement(statement);
        if (result == null) result = caseModelElement(statement);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.RESOURCE_NAME:
      {
        ResourceName resourceName = (ResourceName)theEObject;
        T result = caseResourceName(resourceName);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.CONTENT_LIST:
      {
        ContentList contentList = (ContentList)theEObject;
        T result = caseContentList(contentList);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.RESOURCE_LIST:
      {
        ResourceList resourceList = (ResourceList)theEObject;
        T result = caseResourceList(resourceList);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.LITERAL_LIST:
      {
        LiteralList literalList = (LiteralList)theEObject;
        T result = caseLiteralList(literalList);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.RESOURCE_BY_NAME:
      {
        ResourceByName resourceByName = (ResourceByName)theEObject;
        T result = caseResourceByName(resourceByName);
        if (result == null) result = caseResourceIdentifier(resourceByName);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.EXISTING_RESOURCE_LIST:
      {
        ExistingResourceList existingResourceList = (ExistingResourceList)theEObject;
        T result = caseExistingResourceList(existingResourceList);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.RESOURCE_IDENTIFIER:
      {
        ResourceIdentifier resourceIdentifier = (ResourceIdentifier)theEObject;
        T result = caseResourceIdentifier(resourceIdentifier);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.RESOURCE_BY_SET_OP:
      {
        ResourceBySetOp resourceBySetOp = (ResourceBySetOp)theEObject;
        T result = caseResourceBySetOp(resourceBySetOp);
        if (result == null) result = caseResourceIdentifier(resourceBySetOp);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.RESOURCE_BY_RESTRICTION:
      {
        ResourceByRestriction resourceByRestriction = (ResourceByRestriction)theEObject;
        T result = caseResourceByRestriction(resourceByRestriction);
        if (result == null) result = caseResourceIdentifier(resourceByRestriction);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.UNION_RESOURCE:
      {
        UnionResource unionResource = (UnionResource)theEObject;
        T result = caseUnionResource(unionResource);
        if (result == null) result = caseResourceBySetOp(unionResource);
        if (result == null) result = caseResourceIdentifier(unionResource);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.INTERSECTION_RESOURCE:
      {
        IntersectionResource intersectionResource = (IntersectionResource)theEObject;
        T result = caseIntersectionResource(intersectionResource);
        if (result == null) result = caseResourceBySetOp(intersectionResource);
        if (result == null) result = caseResourceIdentifier(intersectionResource);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.CLASS_DECLARATION:
      {
        ClassDeclaration classDeclaration = (ClassDeclaration)theEObject;
        T result = caseClassDeclaration(classDeclaration);
        if (result == null) result = caseStatement(classDeclaration);
        if (result == null) result = caseModelElement(classDeclaration);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.ENUMERATED_INSTANCES:
      {
        EnumeratedInstances enumeratedInstances = (EnumeratedInstances)theEObject;
        T result = caseEnumeratedInstances(enumeratedInstances);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.ADDL_CLASS_INFO:
      {
        AddlClassInfo addlClassInfo = (AddlClassInfo)theEObject;
        T result = caseAddlClassInfo(addlClassInfo);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.RANGE:
      {
        Range range = (Range)theEObject;
        T result = caseRange(range);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.RANGE_TYPE:
      {
        RangeType rangeType = (RangeType)theEObject;
        T result = caseRangeType(rangeType);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.USER_DEFINED_DATA_TYPE:
      {
        UserDefinedDataType userDefinedDataType = (UserDefinedDataType)theEObject;
        T result = caseUserDefinedDataType(userDefinedDataType);
        if (result == null) result = caseStatement(userDefinedDataType);
        if (result == null) result = caseModelElement(userDefinedDataType);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.DATA_TYPE_RESTRICTION:
      {
        DataTypeRestriction dataTypeRestriction = (DataTypeRestriction)theEObject;
        T result = caseDataTypeRestriction(dataTypeRestriction);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.FACETS:
      {
        Facets facets = (Facets)theEObject;
        T result = caseFacets(facets);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.EQUIVALENT_CONCEPTS:
      {
        EquivalentConcepts equivalentConcepts = (EquivalentConcepts)theEObject;
        T result = caseEquivalentConcepts(equivalentConcepts);
        if (result == null) result = caseStatement(equivalentConcepts);
        if (result == null) result = caseModelElement(equivalentConcepts);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.DISJOINT_CLASSES:
      {
        DisjointClasses disjointClasses = (DisjointClasses)theEObject;
        T result = caseDisjointClasses(disjointClasses);
        if (result == null) result = caseStatement(disjointClasses);
        if (result == null) result = caseModelElement(disjointClasses);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.COMPLEMENT_OF_CLASS:
      {
        ComplementOfClass complementOfClass = (ComplementOfClass)theEObject;
        T result = caseComplementOfClass(complementOfClass);
        if (result == null) result = caseStatement(complementOfClass);
        if (result == null) result = caseModelElement(complementOfClass);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.ALL_VALUES_FROM:
      {
        AllValuesFrom allValuesFrom = (AllValuesFrom)theEObject;
        T result = caseAllValuesFrom(allValuesFrom);
        if (result == null) result = caseStatement(allValuesFrom);
        if (result == null) result = caseModelElement(allValuesFrom);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.SOME_VALUES_FROM:
      {
        SomeValuesFrom someValuesFrom = (SomeValuesFrom)theEObject;
        T result = caseSomeValuesFrom(someValuesFrom);
        if (result == null) result = caseStatement(someValuesFrom);
        if (result == null) result = caseModelElement(someValuesFrom);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.HAS_VALUE:
      {
        HasValue hasValue = (HasValue)theEObject;
        T result = caseHasValue(hasValue);
        if (result == null) result = caseStatement(hasValue);
        if (result == null) result = caseModelElement(hasValue);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.CARDINALITY:
      {
        Cardinality cardinality = (Cardinality)theEObject;
        T result = caseCardinality(cardinality);
        if (result == null) result = caseStatement(cardinality);
        if (result == null) result = caseModelElement(cardinality);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.MIN_CARDINALITY:
      {
        MinCardinality minCardinality = (MinCardinality)theEObject;
        T result = caseMinCardinality(minCardinality);
        if (result == null) result = caseStatement(minCardinality);
        if (result == null) result = caseModelElement(minCardinality);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.MAX_CARDINALITY:
      {
        MaxCardinality maxCardinality = (MaxCardinality)theEObject;
        T result = caseMaxCardinality(maxCardinality);
        if (result == null) result = caseStatement(maxCardinality);
        if (result == null) result = caseModelElement(maxCardinality);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.PROPERTY_OF_CLASS:
      {
        PropertyOfClass propertyOfClass = (PropertyOfClass)theEObject;
        T result = casePropertyOfClass(propertyOfClass);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.ALL_VALUES_CONDITION:
      {
        AllValuesCondition allValuesCondition = (AllValuesCondition)theEObject;
        T result = caseAllValuesCondition(allValuesCondition);
        if (result == null) result = caseCondition(allValuesCondition);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM:
      {
        EnumeratedAllValuesFrom enumeratedAllValuesFrom = (EnumeratedAllValuesFrom)theEObject;
        T result = caseEnumeratedAllValuesFrom(enumeratedAllValuesFrom);
        if (result == null) result = caseStatement(enumeratedAllValuesFrom);
        if (result == null) result = caseModelElement(enumeratedAllValuesFrom);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.ENUMERATED_ALL_AND_SOME_VALUES_FROM:
      {
        EnumeratedAllAndSomeValuesFrom enumeratedAllAndSomeValuesFrom = (EnumeratedAllAndSomeValuesFrom)theEObject;
        T result = caseEnumeratedAllAndSomeValuesFrom(enumeratedAllAndSomeValuesFrom);
        if (result == null) result = caseStatement(enumeratedAllAndSomeValuesFrom);
        if (result == null) result = caseModelElement(enumeratedAllAndSomeValuesFrom);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.DEFAULT_VALUE:
      {
        DefaultValue defaultValue = (DefaultValue)theEObject;
        T result = caseDefaultValue(defaultValue);
        if (result == null) result = caseStatement(defaultValue);
        if (result == null) result = caseModelElement(defaultValue);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.SOME_VALUES_CONDITION:
      {
        SomeValuesCondition someValuesCondition = (SomeValuesCondition)theEObject;
        T result = caseSomeValuesCondition(someValuesCondition);
        if (result == null) result = caseCondition(someValuesCondition);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.HAS_VALUE_CONDITION:
      {
        HasValueCondition hasValueCondition = (HasValueCondition)theEObject;
        T result = caseHasValueCondition(hasValueCondition);
        if (result == null) result = caseCondition(hasValueCondition);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.MIN_CARD_CONDITION:
      {
        MinCardCondition minCardCondition = (MinCardCondition)theEObject;
        T result = caseMinCardCondition(minCardCondition);
        if (result == null) result = caseCondition(minCardCondition);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.MAX_CARD_CONDITION:
      {
        MaxCardCondition maxCardCondition = (MaxCardCondition)theEObject;
        T result = caseMaxCardCondition(maxCardCondition);
        if (result == null) result = caseCondition(maxCardCondition);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.CARD_CONDITION:
      {
        CardCondition cardCondition = (CardCondition)theEObject;
        T result = caseCardCondition(cardCondition);
        if (result == null) result = caseCondition(cardCondition);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.NECESSARY_AND_SUFFICIENT:
      {
        NecessaryAndSufficient necessaryAndSufficient = (NecessaryAndSufficient)theEObject;
        T result = caseNecessaryAndSufficient(necessaryAndSufficient);
        if (result == null) result = caseStatement(necessaryAndSufficient);
        if (result == null) result = caseModelElement(necessaryAndSufficient);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.CONDITION:
      {
        Condition condition = (Condition)theEObject;
        T result = caseCondition(condition);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.PROPERTY_DECLARATION:
      {
        PropertyDeclaration propertyDeclaration = (PropertyDeclaration)theEObject;
        T result = casePropertyDeclaration(propertyDeclaration);
        if (result == null) result = caseStatement(propertyDeclaration);
        if (result == null) result = caseModelElement(propertyDeclaration);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.ADDITIONAL_PROPERTY_INFO:
      {
        AdditionalPropertyInfo additionalPropertyInfo = (AdditionalPropertyInfo)theEObject;
        T result = caseAdditionalPropertyInfo(additionalPropertyInfo);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.FUNCTIONAL_PROPERTY:
      {
        FunctionalProperty functionalProperty = (FunctionalProperty)theEObject;
        T result = caseFunctionalProperty(functionalProperty);
        if (result == null) result = caseStatement(functionalProperty);
        if (result == null) result = caseModelElement(functionalProperty);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.INVERSE_FUNCTIONAL_PROPERTY:
      {
        InverseFunctionalProperty inverseFunctionalProperty = (InverseFunctionalProperty)theEObject;
        T result = caseInverseFunctionalProperty(inverseFunctionalProperty);
        if (result == null) result = caseStatement(inverseFunctionalProperty);
        if (result == null) result = caseModelElement(inverseFunctionalProperty);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.SYMMETRICAL_PROPERTY:
      {
        SymmetricalProperty symmetricalProperty = (SymmetricalProperty)theEObject;
        T result = caseSymmetricalProperty(symmetricalProperty);
        if (result == null) result = caseStatement(symmetricalProperty);
        if (result == null) result = caseModelElement(symmetricalProperty);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.TRANSITIVE_PROPERTY:
      {
        TransitiveProperty transitiveProperty = (TransitiveProperty)theEObject;
        T result = caseTransitiveProperty(transitiveProperty);
        if (result == null) result = caseStatement(transitiveProperty);
        if (result == null) result = caseModelElement(transitiveProperty);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.INVERSE_PROPERTY:
      {
        InverseProperty inverseProperty = (InverseProperty)theEObject;
        T result = caseInverseProperty(inverseProperty);
        if (result == null) result = caseStatement(inverseProperty);
        if (result == null) result = caseModelElement(inverseProperty);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.IS_INVERSE_OF:
      {
        IsInverseOf isInverseOf = (IsInverseOf)theEObject;
        T result = caseIsInverseOf(isInverseOf);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.TYPED_BNODE:
      {
        TypedBNode typedBNode = (TypedBNode)theEObject;
        T result = caseTypedBNode(typedBNode);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.INSTANCE_DECLARATION_STATEMENT:
      {
        InstanceDeclarationStatement instanceDeclarationStatement = (InstanceDeclarationStatement)theEObject;
        T result = caseInstanceDeclarationStatement(instanceDeclarationStatement);
        if (result == null) result = caseStatement(instanceDeclarationStatement);
        if (result == null) result = caseModelElement(instanceDeclarationStatement);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.INSTANCE_DECLARATION:
      {
        InstanceDeclaration instanceDeclaration = (InstanceDeclaration)theEObject;
        T result = caseInstanceDeclaration(instanceDeclaration);
        if (result == null) result = caseInstanceDeclarationStatement(instanceDeclaration);
        if (result == null) result = caseEmbeddedInstanceDeclaration(instanceDeclaration);
        if (result == null) result = caseStatement(instanceDeclaration);
        if (result == null) result = caseModelElement(instanceDeclaration);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.TYPE_DECLARATION:
      {
        TypeDeclaration typeDeclaration = (TypeDeclaration)theEObject;
        T result = caseTypeDeclaration(typeDeclaration);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.INSTANCE_DIFFERENT_FROM:
      {
        InstanceDifferentFrom instanceDifferentFrom = (InstanceDifferentFrom)theEObject;
        T result = caseInstanceDifferentFrom(instanceDifferentFrom);
        if (result == null) result = caseStatement(instanceDifferentFrom);
        if (result == null) result = caseModelElement(instanceDifferentFrom);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.INSTANCES_ALL_DIFFERENT:
      {
        InstancesAllDifferent instancesAllDifferent = (InstancesAllDifferent)theEObject;
        T result = caseInstancesAllDifferent(instancesAllDifferent);
        if (result == null) result = caseStatement(instancesAllDifferent);
        if (result == null) result = caseModelElement(instancesAllDifferent);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION:
      {
        ExistingInstanceAttribution existingInstanceAttribution = (ExistingInstanceAttribution)theEObject;
        T result = caseExistingInstanceAttribution(existingInstanceAttribution);
        if (result == null) result = caseStatement(existingInstanceAttribution);
        if (result == null) result = caseModelElement(existingInstanceAttribution);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.OBJECT:
      {
        com.ge.research.sadl.sadl.Object object = (com.ge.research.sadl.sadl.Object)theEObject;
        T result = caseObject(object);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE:
      {
        PropValPartialTriple propValPartialTriple = (PropValPartialTriple)theEObject;
        T result = casePropValPartialTriple(propValPartialTriple);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.OF_PATTERN_RETURNING_VALUES:
      {
        OfPatternReturningValues ofPatternReturningValues = (OfPatternReturningValues)theEObject;
        T result = caseOfPatternReturningValues(ofPatternReturningValues);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.WITH_CHAIN:
      {
        WithChain withChain = (WithChain)theEObject;
        T result = caseWithChain(withChain);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.WITH_PHRASE:
      {
        WithPhrase withPhrase = (WithPhrase)theEObject;
        T result = caseWithPhrase(withPhrase);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.EMBEDDED_INSTANCE_DECLARATION:
      {
        EmbeddedInstanceDeclaration embeddedInstanceDeclaration = (EmbeddedInstanceDeclaration)theEObject;
        T result = caseEmbeddedInstanceDeclaration(embeddedInstanceDeclaration);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.MERGED_TRIPLES:
      {
        MergedTriples mergedTriples = (MergedTriples)theEObject;
        T result = caseMergedTriples(mergedTriples);
        if (result == null) result = caseGraphPattern(mergedTriples);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.OF_PHRASE:
      {
        OfPhrase ofPhrase = (OfPhrase)theEObject;
        T result = caseOfPhrase(ofPhrase);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.VARIABLE_LIST:
      {
        VariableList variableList = (VariableList)theEObject;
        T result = caseVariableList(variableList);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.RULE:
      {
        Rule rule = (Rule)theEObject;
        T result = caseRule(rule);
        if (result == null) result = caseModelElement(rule);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.QUERY:
      {
        Query query = (Query)theEObject;
        T result = caseQuery(query);
        if (result == null) result = caseModelElement(query);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.TEST:
      {
        Test test = (Test)theEObject;
        T result = caseTest(test);
        if (result == null) result = caseModelElement(test);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.EXPR:
      {
        Expr expr = (Expr)theEObject;
        T result = caseExpr(expr);
        if (result == null) result = caseModelElement(expr);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.DISPLAY:
      {
        Display display = (Display)theEObject;
        T result = caseDisplay(display);
        if (result == null) result = caseModelElement(display);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.EXPLANATION:
      {
        Explanation explanation = (Explanation)theEObject;
        T result = caseExplanation(explanation);
        if (result == null) result = caseModelElement(explanation);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.ELEMENT_SET:
      {
        ElementSet elementSet = (ElementSet)theEObject;
        T result = caseElementSet(elementSet);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.SELECT_EXPRESSION:
      {
        SelectExpression selectExpression = (SelectExpression)theEObject;
        T result = caseSelectExpression(selectExpression);
        if (result == null) result = caseExpression(selectExpression);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.CONSTRUCT_EXPRESSION:
      {
        ConstructExpression constructExpression = (ConstructExpression)theEObject;
        T result = caseConstructExpression(constructExpression);
        if (result == null) result = caseExpression(constructExpression);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.ASK_QUERY_EXPRESSION:
      {
        AskQueryExpression askQueryExpression = (AskQueryExpression)theEObject;
        T result = caseAskQueryExpression(askQueryExpression);
        if (result == null) result = caseExpression(askQueryExpression);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.ORDER_LIST:
      {
        OrderList orderList = (OrderList)theEObject;
        T result = caseOrderList(orderList);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.ORDER_ELEMENT:
      {
        OrderElement orderElement = (OrderElement)theEObject;
        T result = caseOrderElement(orderElement);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.EXPRESSION:
      {
        Expression expression = (Expression)theEObject;
        T result = caseExpression(expression);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.GRAPH_PATTERN:
      {
        GraphPattern graphPattern = (GraphPattern)theEObject;
        T result = caseGraphPattern(graphPattern);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.PROP_OF_SUBJ:
      {
        PropOfSubj propOfSubj = (PropOfSubj)theEObject;
        T result = casePropOfSubj(propOfSubj);
        if (result == null) result = caseGraphPattern(propOfSubj);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.SUBJ_PROP:
      {
        SubjProp subjProp = (SubjProp)theEObject;
        T result = caseSubjProp(subjProp);
        if (result == null) result = caseGraphPattern(subjProp);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.INST_ATTR_SPV:
      {
        InstAttrSPV instAttrSPV = (InstAttrSPV)theEObject;
        T result = caseInstAttrSPV(instAttrSPV);
        if (result == null) result = caseGraphPattern(instAttrSPV);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.INST_ATTR_PSV:
      {
        InstAttrPSV instAttrPSV = (InstAttrPSV)theEObject;
        T result = caseInstAttrPSV(instAttrPSV);
        if (result == null) result = caseGraphPattern(instAttrPSV);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.SUB_TYPE_OF:
      {
        SubTypeOf subTypeOf = (SubTypeOf)theEObject;
        T result = caseSubTypeOf(subTypeOf);
        if (result == null) result = caseGraphPattern(subTypeOf);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.EXISTENTIAL_NEGATION:
      {
        ExistentialNegation existentialNegation = (ExistentialNegation)theEObject;
        T result = caseExistentialNegation(existentialNegation);
        if (result == null) result = caseGraphPattern(existentialNegation);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.INTERVAL_VALUE:
      {
        IntervalValue intervalValue = (IntervalValue)theEObject;
        T result = caseIntervalValue(intervalValue);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.EXPLICIT_VALUE:
      {
        ExplicitValue explicitValue = (ExplicitValue)theEObject;
        T result = caseExplicitValue(explicitValue);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.VALUE_TABLE:
      {
        ValueTable valueTable = (ValueTable)theEObject;
        T result = caseValueTable(valueTable);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.LITERAL_VALUE:
      {
        LiteralValue literalValue = (LiteralValue)theEObject;
        T result = caseLiteralValue(literalValue);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.VALUE_ROW:
      {
        ValueRow valueRow = (ValueRow)theEObject;
        T result = caseValueRow(valueRow);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.JUNCTION_EXPRESSION:
      {
        JunctionExpression junctionExpression = (JunctionExpression)theEObject;
        T result = caseJunctionExpression(junctionExpression);
        if (result == null) result = caseExpression(junctionExpression);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.BINARY_OP_EXPRESSION:
      {
        BinaryOpExpression binaryOpExpression = (BinaryOpExpression)theEObject;
        T result = caseBinaryOpExpression(binaryOpExpression);
        if (result == null) result = caseExpression(binaryOpExpression);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SadlPackage.UNARY_OP_EXPRESSION:
      {
        UnaryOpExpression unaryOpExpression = (UnaryOpExpression)theEObject;
        T result = caseUnaryOpExpression(unaryOpExpression);
        if (result == null) result = caseExpression(unaryOpExpression);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      default: return defaultCase(theEObject);
    }
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Model</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Model</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseModel(Model object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Model Name</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Model Name</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseModelName(ModelName object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Import</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Import</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseImport(Import object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Model Element</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Model Element</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseModelElement(ModelElement object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Statement</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Statement</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseStatement(Statement object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Resource Name</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Resource Name</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseResourceName(ResourceName object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Content List</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Content List</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseContentList(ContentList object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Resource List</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Resource List</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseResourceList(ResourceList object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Literal List</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Literal List</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseLiteralList(LiteralList object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Resource By Name</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Resource By Name</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseResourceByName(ResourceByName object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Existing Resource List</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Existing Resource List</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseExistingResourceList(ExistingResourceList object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Resource Identifier</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Resource Identifier</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseResourceIdentifier(ResourceIdentifier object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Resource By Set Op</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Resource By Set Op</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseResourceBySetOp(ResourceBySetOp object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Resource By Restriction</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Resource By Restriction</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseResourceByRestriction(ResourceByRestriction object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Union Resource</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Union Resource</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseUnionResource(UnionResource object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Intersection Resource</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Intersection Resource</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseIntersectionResource(IntersectionResource object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Class Declaration</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Class Declaration</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseClassDeclaration(ClassDeclaration object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Enumerated Instances</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Enumerated Instances</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseEnumeratedInstances(EnumeratedInstances object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Addl Class Info</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Addl Class Info</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseAddlClassInfo(AddlClassInfo object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Range</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Range</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseRange(Range object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Range Type</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Range Type</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseRangeType(RangeType object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>User Defined Data Type</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>User Defined Data Type</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseUserDefinedDataType(UserDefinedDataType object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Data Type Restriction</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Data Type Restriction</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseDataTypeRestriction(DataTypeRestriction object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Facets</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Facets</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseFacets(Facets object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Equivalent Concepts</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Equivalent Concepts</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseEquivalentConcepts(EquivalentConcepts object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Disjoint Classes</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Disjoint Classes</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseDisjointClasses(DisjointClasses object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Complement Of Class</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Complement Of Class</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseComplementOfClass(ComplementOfClass object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>All Values From</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>All Values From</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseAllValuesFrom(AllValuesFrom object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Some Values From</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Some Values From</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseSomeValuesFrom(SomeValuesFrom object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Has Value</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Has Value</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseHasValue(HasValue object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Cardinality</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Cardinality</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseCardinality(Cardinality object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Min Cardinality</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Min Cardinality</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseMinCardinality(MinCardinality object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Max Cardinality</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Max Cardinality</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseMaxCardinality(MaxCardinality object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Property Of Class</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Property Of Class</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T casePropertyOfClass(PropertyOfClass object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>All Values Condition</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>All Values Condition</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseAllValuesCondition(AllValuesCondition object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Enumerated All Values From</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Enumerated All Values From</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseEnumeratedAllValuesFrom(EnumeratedAllValuesFrom object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Enumerated All And Some Values From</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Enumerated All And Some Values From</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseEnumeratedAllAndSomeValuesFrom(EnumeratedAllAndSomeValuesFrom object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Default Value</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Default Value</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseDefaultValue(DefaultValue object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Some Values Condition</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Some Values Condition</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseSomeValuesCondition(SomeValuesCondition object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Has Value Condition</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Has Value Condition</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseHasValueCondition(HasValueCondition object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Min Card Condition</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Min Card Condition</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseMinCardCondition(MinCardCondition object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Max Card Condition</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Max Card Condition</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseMaxCardCondition(MaxCardCondition object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Card Condition</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Card Condition</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseCardCondition(CardCondition object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Necessary And Sufficient</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Necessary And Sufficient</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseNecessaryAndSufficient(NecessaryAndSufficient object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Condition</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Condition</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseCondition(Condition object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Property Declaration</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Property Declaration</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T casePropertyDeclaration(PropertyDeclaration object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Additional Property Info</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Additional Property Info</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseAdditionalPropertyInfo(AdditionalPropertyInfo object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Functional Property</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Functional Property</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseFunctionalProperty(FunctionalProperty object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Inverse Functional Property</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Inverse Functional Property</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseInverseFunctionalProperty(InverseFunctionalProperty object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Symmetrical Property</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Symmetrical Property</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseSymmetricalProperty(SymmetricalProperty object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Transitive Property</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Transitive Property</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseTransitiveProperty(TransitiveProperty object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Inverse Property</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Inverse Property</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseInverseProperty(InverseProperty object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Is Inverse Of</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Is Inverse Of</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseIsInverseOf(IsInverseOf object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Typed BNode</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Typed BNode</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseTypedBNode(TypedBNode object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Instance Declaration Statement</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Instance Declaration Statement</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseInstanceDeclarationStatement(InstanceDeclarationStatement object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Instance Declaration</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Instance Declaration</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseInstanceDeclaration(InstanceDeclaration object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Type Declaration</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Type Declaration</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseTypeDeclaration(TypeDeclaration object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Instance Different From</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Instance Different From</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseInstanceDifferentFrom(InstanceDifferentFrom object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Instances All Different</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Instances All Different</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseInstancesAllDifferent(InstancesAllDifferent object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Existing Instance Attribution</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Existing Instance Attribution</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseExistingInstanceAttribution(ExistingInstanceAttribution object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Object</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Object</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseObject(com.ge.research.sadl.sadl.Object object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Prop Val Partial Triple</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Prop Val Partial Triple</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T casePropValPartialTriple(PropValPartialTriple object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Of Pattern Returning Values</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Of Pattern Returning Values</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseOfPatternReturningValues(OfPatternReturningValues object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>With Chain</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>With Chain</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseWithChain(WithChain object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>With Phrase</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>With Phrase</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseWithPhrase(WithPhrase object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Embedded Instance Declaration</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Embedded Instance Declaration</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseEmbeddedInstanceDeclaration(EmbeddedInstanceDeclaration object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Merged Triples</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Merged Triples</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseMergedTriples(MergedTriples object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Of Phrase</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Of Phrase</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseOfPhrase(OfPhrase object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Variable List</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Variable List</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseVariableList(VariableList object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Rule</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Rule</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseRule(Rule object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Query</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Query</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseQuery(Query object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Test</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Test</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseTest(Test object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Expr</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Expr</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseExpr(Expr object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Display</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Display</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseDisplay(Display object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Explanation</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Explanation</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseExplanation(Explanation object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Element Set</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Element Set</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseElementSet(ElementSet object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Select Expression</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Select Expression</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseSelectExpression(SelectExpression object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Construct Expression</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Construct Expression</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseConstructExpression(ConstructExpression object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Ask Query Expression</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Ask Query Expression</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseAskQueryExpression(AskQueryExpression object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Order List</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Order List</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseOrderList(OrderList object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Order Element</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Order Element</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseOrderElement(OrderElement object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Expression</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Expression</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseExpression(Expression object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Graph Pattern</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Graph Pattern</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseGraphPattern(GraphPattern object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Prop Of Subj</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Prop Of Subj</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T casePropOfSubj(PropOfSubj object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Subj Prop</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Subj Prop</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseSubjProp(SubjProp object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Inst Attr SPV</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Inst Attr SPV</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseInstAttrSPV(InstAttrSPV object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Inst Attr PSV</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Inst Attr PSV</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseInstAttrPSV(InstAttrPSV object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Sub Type Of</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Sub Type Of</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseSubTypeOf(SubTypeOf object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Existential Negation</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Existential Negation</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseExistentialNegation(ExistentialNegation object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Interval Value</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Interval Value</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseIntervalValue(IntervalValue object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Explicit Value</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Explicit Value</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseExplicitValue(ExplicitValue object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Value Table</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Value Table</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseValueTable(ValueTable object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Literal Value</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Literal Value</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseLiteralValue(LiteralValue object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Value Row</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Value Row</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseValueRow(ValueRow object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Junction Expression</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Junction Expression</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseJunctionExpression(JunctionExpression object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Binary Op Expression</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Binary Op Expression</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseBinaryOpExpression(BinaryOpExpression object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Unary Op Expression</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Unary Op Expression</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseUnaryOpExpression(UnaryOpExpression object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>EObject</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch, but this is the last case anyway.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>EObject</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject)
   * @generated
   */
  @Override
  public T defaultCase(EObject object)
  {
    return null;
  }

} //SadlSwitch
