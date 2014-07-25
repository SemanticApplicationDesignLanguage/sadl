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

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see com.ge.research.sadl.sadl.SadlPackage
 * @generated
 */
public class SadlAdapterFactory extends AdapterFactoryImpl
{
  /**
   * The cached model package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected static SadlPackage modelPackage;

  /**
   * Creates an instance of the adapter factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SadlAdapterFactory()
  {
    if (modelPackage == null)
    {
      modelPackage = SadlPackage.eINSTANCE;
    }
  }

  /**
   * Returns whether this factory is applicable for the type of the object.
   * <!-- begin-user-doc -->
   * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
   * <!-- end-user-doc -->
   * @return whether this factory is applicable for the type of the object.
   * @generated
   */
  @Override
  public boolean isFactoryForType(Object object)
  {
    if (object == modelPackage)
    {
      return true;
    }
    if (object instanceof EObject)
    {
      return ((EObject)object).eClass().getEPackage() == modelPackage;
    }
    return false;
  }

  /**
   * The switch that delegates to the <code>createXXX</code> methods.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected SadlSwitch<Adapter> modelSwitch =
    new SadlSwitch<Adapter>()
    {
      @Override
      public Adapter caseModel(Model object)
      {
        return createModelAdapter();
      }
      @Override
      public Adapter caseModelName(ModelName object)
      {
        return createModelNameAdapter();
      }
      @Override
      public Adapter caseImport(Import object)
      {
        return createImportAdapter();
      }
      @Override
      public Adapter caseModelElement(ModelElement object)
      {
        return createModelElementAdapter();
      }
      @Override
      public Adapter caseStatement(Statement object)
      {
        return createStatementAdapter();
      }
      @Override
      public Adapter caseResourceName(ResourceName object)
      {
        return createResourceNameAdapter();
      }
      @Override
      public Adapter caseContentList(ContentList object)
      {
        return createContentListAdapter();
      }
      @Override
      public Adapter caseResourceList(ResourceList object)
      {
        return createResourceListAdapter();
      }
      @Override
      public Adapter caseLiteralList(LiteralList object)
      {
        return createLiteralListAdapter();
      }
      @Override
      public Adapter caseResourceByName(ResourceByName object)
      {
        return createResourceByNameAdapter();
      }
      @Override
      public Adapter caseExistingResourceList(ExistingResourceList object)
      {
        return createExistingResourceListAdapter();
      }
      @Override
      public Adapter caseResourceIdentifier(ResourceIdentifier object)
      {
        return createResourceIdentifierAdapter();
      }
      @Override
      public Adapter caseResourceBySetOp(ResourceBySetOp object)
      {
        return createResourceBySetOpAdapter();
      }
      @Override
      public Adapter caseResourceByRestriction(ResourceByRestriction object)
      {
        return createResourceByRestrictionAdapter();
      }
      @Override
      public Adapter caseUnionResource(UnionResource object)
      {
        return createUnionResourceAdapter();
      }
      @Override
      public Adapter caseIntersectionResource(IntersectionResource object)
      {
        return createIntersectionResourceAdapter();
      }
      @Override
      public Adapter caseClassDeclaration(ClassDeclaration object)
      {
        return createClassDeclarationAdapter();
      }
      @Override
      public Adapter caseEnumeratedInstances(EnumeratedInstances object)
      {
        return createEnumeratedInstancesAdapter();
      }
      @Override
      public Adapter caseAddlClassInfo(AddlClassInfo object)
      {
        return createAddlClassInfoAdapter();
      }
      @Override
      public Adapter caseRange(Range object)
      {
        return createRangeAdapter();
      }
      @Override
      public Adapter caseRangeType(RangeType object)
      {
        return createRangeTypeAdapter();
      }
      @Override
      public Adapter caseUserDefinedDataType(UserDefinedDataType object)
      {
        return createUserDefinedDataTypeAdapter();
      }
      @Override
      public Adapter caseDataTypeRestriction(DataTypeRestriction object)
      {
        return createDataTypeRestrictionAdapter();
      }
      @Override
      public Adapter caseFacets(Facets object)
      {
        return createFacetsAdapter();
      }
      @Override
      public Adapter caseEquivalentConcepts(EquivalentConcepts object)
      {
        return createEquivalentConceptsAdapter();
      }
      @Override
      public Adapter caseDisjointClasses(DisjointClasses object)
      {
        return createDisjointClassesAdapter();
      }
      @Override
      public Adapter caseComplementOfClass(ComplementOfClass object)
      {
        return createComplementOfClassAdapter();
      }
      @Override
      public Adapter caseAllValuesFrom(AllValuesFrom object)
      {
        return createAllValuesFromAdapter();
      }
      @Override
      public Adapter caseSomeValuesFrom(SomeValuesFrom object)
      {
        return createSomeValuesFromAdapter();
      }
      @Override
      public Adapter caseHasValue(HasValue object)
      {
        return createHasValueAdapter();
      }
      @Override
      public Adapter caseCardinality(Cardinality object)
      {
        return createCardinalityAdapter();
      }
      @Override
      public Adapter caseMinCardinality(MinCardinality object)
      {
        return createMinCardinalityAdapter();
      }
      @Override
      public Adapter caseMaxCardinality(MaxCardinality object)
      {
        return createMaxCardinalityAdapter();
      }
      @Override
      public Adapter casePropertyOfClass(PropertyOfClass object)
      {
        return createPropertyOfClassAdapter();
      }
      @Override
      public Adapter caseAllValuesCondition(AllValuesCondition object)
      {
        return createAllValuesConditionAdapter();
      }
      @Override
      public Adapter caseEnumeratedAllValuesFrom(EnumeratedAllValuesFrom object)
      {
        return createEnumeratedAllValuesFromAdapter();
      }
      @Override
      public Adapter caseEnumeratedAllAndSomeValuesFrom(EnumeratedAllAndSomeValuesFrom object)
      {
        return createEnumeratedAllAndSomeValuesFromAdapter();
      }
      @Override
      public Adapter caseDefaultValue(DefaultValue object)
      {
        return createDefaultValueAdapter();
      }
      @Override
      public Adapter caseSomeValuesCondition(SomeValuesCondition object)
      {
        return createSomeValuesConditionAdapter();
      }
      @Override
      public Adapter caseHasValueCondition(HasValueCondition object)
      {
        return createHasValueConditionAdapter();
      }
      @Override
      public Adapter caseMinCardCondition(MinCardCondition object)
      {
        return createMinCardConditionAdapter();
      }
      @Override
      public Adapter caseMaxCardCondition(MaxCardCondition object)
      {
        return createMaxCardConditionAdapter();
      }
      @Override
      public Adapter caseCardCondition(CardCondition object)
      {
        return createCardConditionAdapter();
      }
      @Override
      public Adapter caseNecessaryAndSufficient(NecessaryAndSufficient object)
      {
        return createNecessaryAndSufficientAdapter();
      }
      @Override
      public Adapter caseCondition(Condition object)
      {
        return createConditionAdapter();
      }
      @Override
      public Adapter casePropertyDeclaration(PropertyDeclaration object)
      {
        return createPropertyDeclarationAdapter();
      }
      @Override
      public Adapter caseAdditionalPropertyInfo(AdditionalPropertyInfo object)
      {
        return createAdditionalPropertyInfoAdapter();
      }
      @Override
      public Adapter caseFunctionalProperty(FunctionalProperty object)
      {
        return createFunctionalPropertyAdapter();
      }
      @Override
      public Adapter caseInverseFunctionalProperty(InverseFunctionalProperty object)
      {
        return createInverseFunctionalPropertyAdapter();
      }
      @Override
      public Adapter caseSymmetricalProperty(SymmetricalProperty object)
      {
        return createSymmetricalPropertyAdapter();
      }
      @Override
      public Adapter caseTransitiveProperty(TransitiveProperty object)
      {
        return createTransitivePropertyAdapter();
      }
      @Override
      public Adapter caseInverseProperty(InverseProperty object)
      {
        return createInversePropertyAdapter();
      }
      @Override
      public Adapter caseIsInverseOf(IsInverseOf object)
      {
        return createIsInverseOfAdapter();
      }
      @Override
      public Adapter caseTypedBNode(TypedBNode object)
      {
        return createTypedBNodeAdapter();
      }
      @Override
      public Adapter caseInstanceDeclarationStatement(InstanceDeclarationStatement object)
      {
        return createInstanceDeclarationStatementAdapter();
      }
      @Override
      public Adapter caseInstanceDeclaration(InstanceDeclaration object)
      {
        return createInstanceDeclarationAdapter();
      }
      @Override
      public Adapter caseTypeDeclaration(TypeDeclaration object)
      {
        return createTypeDeclarationAdapter();
      }
      @Override
      public Adapter caseInstanceDifferentFrom(InstanceDifferentFrom object)
      {
        return createInstanceDifferentFromAdapter();
      }
      @Override
      public Adapter caseInstancesAllDifferent(InstancesAllDifferent object)
      {
        return createInstancesAllDifferentAdapter();
      }
      @Override
      public Adapter caseExistingInstanceAttribution(ExistingInstanceAttribution object)
      {
        return createExistingInstanceAttributionAdapter();
      }
      @Override
      public Adapter caseObject(com.ge.research.sadl.sadl.Object object)
      {
        return createObjectAdapter();
      }
      @Override
      public Adapter casePropValPartialTriple(PropValPartialTriple object)
      {
        return createPropValPartialTripleAdapter();
      }
      @Override
      public Adapter caseOfPatternReturningValues(OfPatternReturningValues object)
      {
        return createOfPatternReturningValuesAdapter();
      }
      @Override
      public Adapter caseWithChain(WithChain object)
      {
        return createWithChainAdapter();
      }
      @Override
      public Adapter caseWithPhrase(WithPhrase object)
      {
        return createWithPhraseAdapter();
      }
      @Override
      public Adapter caseEmbeddedInstanceDeclaration(EmbeddedInstanceDeclaration object)
      {
        return createEmbeddedInstanceDeclarationAdapter();
      }
      @Override
      public Adapter caseMergedTriples(MergedTriples object)
      {
        return createMergedTriplesAdapter();
      }
      @Override
      public Adapter caseOfPhrase(OfPhrase object)
      {
        return createOfPhraseAdapter();
      }
      @Override
      public Adapter caseVariableList(VariableList object)
      {
        return createVariableListAdapter();
      }
      @Override
      public Adapter caseRule(Rule object)
      {
        return createRuleAdapter();
      }
      @Override
      public Adapter caseQuery(Query object)
      {
        return createQueryAdapter();
      }
      @Override
      public Adapter caseTest(Test object)
      {
        return createTestAdapter();
      }
      @Override
      public Adapter caseExpr(Expr object)
      {
        return createExprAdapter();
      }
      @Override
      public Adapter caseDisplay(Display object)
      {
        return createDisplayAdapter();
      }
      @Override
      public Adapter caseExplanation(Explanation object)
      {
        return createExplanationAdapter();
      }
      @Override
      public Adapter caseElementSet(ElementSet object)
      {
        return createElementSetAdapter();
      }
      @Override
      public Adapter caseSelectExpression(SelectExpression object)
      {
        return createSelectExpressionAdapter();
      }
      @Override
      public Adapter caseConstructExpression(ConstructExpression object)
      {
        return createConstructExpressionAdapter();
      }
      @Override
      public Adapter caseAskQueryExpression(AskQueryExpression object)
      {
        return createAskQueryExpressionAdapter();
      }
      @Override
      public Adapter caseOrderList(OrderList object)
      {
        return createOrderListAdapter();
      }
      @Override
      public Adapter caseOrderElement(OrderElement object)
      {
        return createOrderElementAdapter();
      }
      @Override
      public Adapter caseExpression(Expression object)
      {
        return createExpressionAdapter();
      }
      @Override
      public Adapter caseGraphPattern(GraphPattern object)
      {
        return createGraphPatternAdapter();
      }
      @Override
      public Adapter casePropOfSubj(PropOfSubj object)
      {
        return createPropOfSubjAdapter();
      }
      @Override
      public Adapter caseSubjProp(SubjProp object)
      {
        return createSubjPropAdapter();
      }
      @Override
      public Adapter caseInstAttrSPV(InstAttrSPV object)
      {
        return createInstAttrSPVAdapter();
      }
      @Override
      public Adapter caseInstAttrPSV(InstAttrPSV object)
      {
        return createInstAttrPSVAdapter();
      }
      @Override
      public Adapter caseSubTypeOf(SubTypeOf object)
      {
        return createSubTypeOfAdapter();
      }
      @Override
      public Adapter caseIntervalValue(IntervalValue object)
      {
        return createIntervalValueAdapter();
      }
      @Override
      public Adapter caseExplicitValue(ExplicitValue object)
      {
        return createExplicitValueAdapter();
      }
      @Override
      public Adapter caseValueTable(ValueTable object)
      {
        return createValueTableAdapter();
      }
      @Override
      public Adapter caseLiteralValue(LiteralValue object)
      {
        return createLiteralValueAdapter();
      }
      @Override
      public Adapter caseValueRow(ValueRow object)
      {
        return createValueRowAdapter();
      }
      @Override
      public Adapter caseJunctionExpression(JunctionExpression object)
      {
        return createJunctionExpressionAdapter();
      }
      @Override
      public Adapter caseBinaryOpExpression(BinaryOpExpression object)
      {
        return createBinaryOpExpressionAdapter();
      }
      @Override
      public Adapter caseUnaryOpExpression(UnaryOpExpression object)
      {
        return createUnaryOpExpressionAdapter();
      }
      @Override
      public Adapter defaultCase(EObject object)
      {
        return createEObjectAdapter();
      }
    };

  /**
   * Creates an adapter for the <code>target</code>.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param target the object to adapt.
   * @return the adapter for the <code>target</code>.
   * @generated
   */
  @Override
  public Adapter createAdapter(Notifier target)
  {
    return modelSwitch.doSwitch((EObject)target);
  }


  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Model <em>Model</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Model
   * @generated
   */
  public Adapter createModelAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ModelName <em>Model Name</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ModelName
   * @generated
   */
  public Adapter createModelNameAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Import <em>Import</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Import
   * @generated
   */
  public Adapter createImportAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ModelElement <em>Model Element</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ModelElement
   * @generated
   */
  public Adapter createModelElementAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Statement <em>Statement</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Statement
   * @generated
   */
  public Adapter createStatementAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ResourceName <em>Resource Name</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ResourceName
   * @generated
   */
  public Adapter createResourceNameAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ContentList <em>Content List</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ContentList
   * @generated
   */
  public Adapter createContentListAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ResourceList <em>Resource List</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ResourceList
   * @generated
   */
  public Adapter createResourceListAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.LiteralList <em>Literal List</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.LiteralList
   * @generated
   */
  public Adapter createLiteralListAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ResourceByName <em>Resource By Name</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ResourceByName
   * @generated
   */
  public Adapter createResourceByNameAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ExistingResourceList <em>Existing Resource List</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ExistingResourceList
   * @generated
   */
  public Adapter createExistingResourceListAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ResourceIdentifier <em>Resource Identifier</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ResourceIdentifier
   * @generated
   */
  public Adapter createResourceIdentifierAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ResourceBySetOp <em>Resource By Set Op</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ResourceBySetOp
   * @generated
   */
  public Adapter createResourceBySetOpAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ResourceByRestriction <em>Resource By Restriction</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ResourceByRestriction
   * @generated
   */
  public Adapter createResourceByRestrictionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.UnionResource <em>Union Resource</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.UnionResource
   * @generated
   */
  public Adapter createUnionResourceAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.IntersectionResource <em>Intersection Resource</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.IntersectionResource
   * @generated
   */
  public Adapter createIntersectionResourceAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ClassDeclaration <em>Class Declaration</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ClassDeclaration
   * @generated
   */
  public Adapter createClassDeclarationAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.EnumeratedInstances <em>Enumerated Instances</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.EnumeratedInstances
   * @generated
   */
  public Adapter createEnumeratedInstancesAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.AddlClassInfo <em>Addl Class Info</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.AddlClassInfo
   * @generated
   */
  public Adapter createAddlClassInfoAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Range <em>Range</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Range
   * @generated
   */
  public Adapter createRangeAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.RangeType <em>Range Type</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.RangeType
   * @generated
   */
  public Adapter createRangeTypeAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.UserDefinedDataType <em>User Defined Data Type</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.UserDefinedDataType
   * @generated
   */
  public Adapter createUserDefinedDataTypeAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.DataTypeRestriction <em>Data Type Restriction</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.DataTypeRestriction
   * @generated
   */
  public Adapter createDataTypeRestrictionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Facets <em>Facets</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Facets
   * @generated
   */
  public Adapter createFacetsAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.EquivalentConcepts <em>Equivalent Concepts</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.EquivalentConcepts
   * @generated
   */
  public Adapter createEquivalentConceptsAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.DisjointClasses <em>Disjoint Classes</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.DisjointClasses
   * @generated
   */
  public Adapter createDisjointClassesAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ComplementOfClass <em>Complement Of Class</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ComplementOfClass
   * @generated
   */
  public Adapter createComplementOfClassAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.AllValuesFrom <em>All Values From</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.AllValuesFrom
   * @generated
   */
  public Adapter createAllValuesFromAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.SomeValuesFrom <em>Some Values From</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.SomeValuesFrom
   * @generated
   */
  public Adapter createSomeValuesFromAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.HasValue <em>Has Value</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.HasValue
   * @generated
   */
  public Adapter createHasValueAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Cardinality <em>Cardinality</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Cardinality
   * @generated
   */
  public Adapter createCardinalityAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.MinCardinality <em>Min Cardinality</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.MinCardinality
   * @generated
   */
  public Adapter createMinCardinalityAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.MaxCardinality <em>Max Cardinality</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.MaxCardinality
   * @generated
   */
  public Adapter createMaxCardinalityAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.PropertyOfClass <em>Property Of Class</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.PropertyOfClass
   * @generated
   */
  public Adapter createPropertyOfClassAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.AllValuesCondition <em>All Values Condition</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.AllValuesCondition
   * @generated
   */
  public Adapter createAllValuesConditionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.EnumeratedAllValuesFrom <em>Enumerated All Values From</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.EnumeratedAllValuesFrom
   * @generated
   */
  public Adapter createEnumeratedAllValuesFromAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom <em>Enumerated All And Some Values From</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom
   * @generated
   */
  public Adapter createEnumeratedAllAndSomeValuesFromAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.DefaultValue <em>Default Value</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.DefaultValue
   * @generated
   */
  public Adapter createDefaultValueAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.SomeValuesCondition <em>Some Values Condition</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.SomeValuesCondition
   * @generated
   */
  public Adapter createSomeValuesConditionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.HasValueCondition <em>Has Value Condition</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.HasValueCondition
   * @generated
   */
  public Adapter createHasValueConditionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.MinCardCondition <em>Min Card Condition</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.MinCardCondition
   * @generated
   */
  public Adapter createMinCardConditionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.MaxCardCondition <em>Max Card Condition</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.MaxCardCondition
   * @generated
   */
  public Adapter createMaxCardConditionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.CardCondition <em>Card Condition</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.CardCondition
   * @generated
   */
  public Adapter createCardConditionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.NecessaryAndSufficient <em>Necessary And Sufficient</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.NecessaryAndSufficient
   * @generated
   */
  public Adapter createNecessaryAndSufficientAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Condition <em>Condition</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Condition
   * @generated
   */
  public Adapter createConditionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.PropertyDeclaration <em>Property Declaration</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.PropertyDeclaration
   * @generated
   */
  public Adapter createPropertyDeclarationAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo <em>Additional Property Info</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.AdditionalPropertyInfo
   * @generated
   */
  public Adapter createAdditionalPropertyInfoAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.FunctionalProperty <em>Functional Property</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.FunctionalProperty
   * @generated
   */
  public Adapter createFunctionalPropertyAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.InverseFunctionalProperty <em>Inverse Functional Property</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.InverseFunctionalProperty
   * @generated
   */
  public Adapter createInverseFunctionalPropertyAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.SymmetricalProperty <em>Symmetrical Property</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.SymmetricalProperty
   * @generated
   */
  public Adapter createSymmetricalPropertyAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.TransitiveProperty <em>Transitive Property</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.TransitiveProperty
   * @generated
   */
  public Adapter createTransitivePropertyAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.InverseProperty <em>Inverse Property</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.InverseProperty
   * @generated
   */
  public Adapter createInversePropertyAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.IsInverseOf <em>Is Inverse Of</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.IsInverseOf
   * @generated
   */
  public Adapter createIsInverseOfAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.TypedBNode <em>Typed BNode</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.TypedBNode
   * @generated
   */
  public Adapter createTypedBNodeAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.InstanceDeclarationStatement <em>Instance Declaration Statement</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.InstanceDeclarationStatement
   * @generated
   */
  public Adapter createInstanceDeclarationStatementAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.InstanceDeclaration <em>Instance Declaration</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.InstanceDeclaration
   * @generated
   */
  public Adapter createInstanceDeclarationAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.TypeDeclaration <em>Type Declaration</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.TypeDeclaration
   * @generated
   */
  public Adapter createTypeDeclarationAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.InstanceDifferentFrom <em>Instance Different From</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.InstanceDifferentFrom
   * @generated
   */
  public Adapter createInstanceDifferentFromAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.InstancesAllDifferent <em>Instances All Different</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.InstancesAllDifferent
   * @generated
   */
  public Adapter createInstancesAllDifferentAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution <em>Existing Instance Attribution</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ExistingInstanceAttribution
   * @generated
   */
  public Adapter createExistingInstanceAttributionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Object <em>Object</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Object
   * @generated
   */
  public Adapter createObjectAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.PropValPartialTriple <em>Prop Val Partial Triple</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.PropValPartialTriple
   * @generated
   */
  public Adapter createPropValPartialTripleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.OfPatternReturningValues <em>Of Pattern Returning Values</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.OfPatternReturningValues
   * @generated
   */
  public Adapter createOfPatternReturningValuesAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.WithChain <em>With Chain</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.WithChain
   * @generated
   */
  public Adapter createWithChainAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.WithPhrase <em>With Phrase</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.WithPhrase
   * @generated
   */
  public Adapter createWithPhraseAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.EmbeddedInstanceDeclaration <em>Embedded Instance Declaration</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.EmbeddedInstanceDeclaration
   * @generated
   */
  public Adapter createEmbeddedInstanceDeclarationAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.MergedTriples <em>Merged Triples</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.MergedTriples
   * @generated
   */
  public Adapter createMergedTriplesAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.OfPhrase <em>Of Phrase</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.OfPhrase
   * @generated
   */
  public Adapter createOfPhraseAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.VariableList <em>Variable List</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.VariableList
   * @generated
   */
  public Adapter createVariableListAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Rule <em>Rule</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Rule
   * @generated
   */
  public Adapter createRuleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Query <em>Query</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Query
   * @generated
   */
  public Adapter createQueryAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Test <em>Test</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Test
   * @generated
   */
  public Adapter createTestAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Expr <em>Expr</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Expr
   * @generated
   */
  public Adapter createExprAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Display <em>Display</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Display
   * @generated
   */
  public Adapter createDisplayAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Explanation <em>Explanation</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Explanation
   * @generated
   */
  public Adapter createExplanationAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ElementSet <em>Element Set</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ElementSet
   * @generated
   */
  public Adapter createElementSetAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.SelectExpression <em>Select Expression</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.SelectExpression
   * @generated
   */
  public Adapter createSelectExpressionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ConstructExpression <em>Construct Expression</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ConstructExpression
   * @generated
   */
  public Adapter createConstructExpressionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.AskQueryExpression <em>Ask Query Expression</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.AskQueryExpression
   * @generated
   */
  public Adapter createAskQueryExpressionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.OrderList <em>Order List</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.OrderList
   * @generated
   */
  public Adapter createOrderListAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.OrderElement <em>Order Element</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.OrderElement
   * @generated
   */
  public Adapter createOrderElementAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.Expression <em>Expression</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.Expression
   * @generated
   */
  public Adapter createExpressionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.GraphPattern <em>Graph Pattern</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.GraphPattern
   * @generated
   */
  public Adapter createGraphPatternAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.PropOfSubj <em>Prop Of Subj</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.PropOfSubj
   * @generated
   */
  public Adapter createPropOfSubjAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.SubjProp <em>Subj Prop</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.SubjProp
   * @generated
   */
  public Adapter createSubjPropAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.InstAttrSPV <em>Inst Attr SPV</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.InstAttrSPV
   * @generated
   */
  public Adapter createInstAttrSPVAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.InstAttrPSV <em>Inst Attr PSV</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.InstAttrPSV
   * @generated
   */
  public Adapter createInstAttrPSVAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.SubTypeOf <em>Sub Type Of</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.SubTypeOf
   * @generated
   */
  public Adapter createSubTypeOfAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.IntervalValue <em>Interval Value</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.IntervalValue
   * @generated
   */
  public Adapter createIntervalValueAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ExplicitValue <em>Explicit Value</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ExplicitValue
   * @generated
   */
  public Adapter createExplicitValueAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ValueTable <em>Value Table</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ValueTable
   * @generated
   */
  public Adapter createValueTableAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.LiteralValue <em>Literal Value</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.LiteralValue
   * @generated
   */
  public Adapter createLiteralValueAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.ValueRow <em>Value Row</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.ValueRow
   * @generated
   */
  public Adapter createValueRowAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.JunctionExpression <em>Junction Expression</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.JunctionExpression
   * @generated
   */
  public Adapter createJunctionExpressionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.BinaryOpExpression <em>Binary Op Expression</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.BinaryOpExpression
   * @generated
   */
  public Adapter createBinaryOpExpressionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link com.ge.research.sadl.sadl.UnaryOpExpression <em>Unary Op Expression</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see com.ge.research.sadl.sadl.UnaryOpExpression
   * @generated
   */
  public Adapter createUnaryOpExpressionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for the default case.
   * <!-- begin-user-doc -->
   * This default implementation returns null.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @generated
   */
  public Adapter createEObjectAdapter()
  {
    return null;
  }

} //SadlAdapterFactory
