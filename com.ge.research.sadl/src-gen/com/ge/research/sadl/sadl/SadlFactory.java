/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see com.ge.research.sadl.sadl.SadlPackage
 * @generated
 */
public interface SadlFactory extends EFactory
{
  /**
   * The singleton instance of the factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  SadlFactory eINSTANCE = com.ge.research.sadl.sadl.impl.SadlFactoryImpl.init();

  /**
   * Returns a new object of class '<em>Model</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Model</em>'.
   * @generated
   */
  Model createModel();

  /**
   * Returns a new object of class '<em>Model Name</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Model Name</em>'.
   * @generated
   */
  ModelName createModelName();

  /**
   * Returns a new object of class '<em>Import</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Import</em>'.
   * @generated
   */
  Import createImport();

  /**
   * Returns a new object of class '<em>Model Element</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Model Element</em>'.
   * @generated
   */
  ModelElement createModelElement();

  /**
   * Returns a new object of class '<em>Statement</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Statement</em>'.
   * @generated
   */
  Statement createStatement();

  /**
   * Returns a new object of class '<em>Resource Name</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Resource Name</em>'.
   * @generated
   */
  ResourceName createResourceName();

  /**
   * Returns a new object of class '<em>Content List</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Content List</em>'.
   * @generated
   */
  ContentList createContentList();

  /**
   * Returns a new object of class '<em>Resource List</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Resource List</em>'.
   * @generated
   */
  ResourceList createResourceList();

  /**
   * Returns a new object of class '<em>Literal List</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Literal List</em>'.
   * @generated
   */
  LiteralList createLiteralList();

  /**
   * Returns a new object of class '<em>Resource By Name</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Resource By Name</em>'.
   * @generated
   */
  ResourceByName createResourceByName();

  /**
   * Returns a new object of class '<em>Existing Resource List</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Existing Resource List</em>'.
   * @generated
   */
  ExistingResourceList createExistingResourceList();

  /**
   * Returns a new object of class '<em>Resource Identifier</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Resource Identifier</em>'.
   * @generated
   */
  ResourceIdentifier createResourceIdentifier();

  /**
   * Returns a new object of class '<em>Resource By Set Op</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Resource By Set Op</em>'.
   * @generated
   */
  ResourceBySetOp createResourceBySetOp();

  /**
   * Returns a new object of class '<em>Resource By Restriction</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Resource By Restriction</em>'.
   * @generated
   */
  ResourceByRestriction createResourceByRestriction();

  /**
   * Returns a new object of class '<em>Union Resource</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Union Resource</em>'.
   * @generated
   */
  UnionResource createUnionResource();

  /**
   * Returns a new object of class '<em>Intersection Resource</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Intersection Resource</em>'.
   * @generated
   */
  IntersectionResource createIntersectionResource();

  /**
   * Returns a new object of class '<em>Class Declaration</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Class Declaration</em>'.
   * @generated
   */
  ClassDeclaration createClassDeclaration();

  /**
   * Returns a new object of class '<em>Enumerated Instances</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Enumerated Instances</em>'.
   * @generated
   */
  EnumeratedInstances createEnumeratedInstances();

  /**
   * Returns a new object of class '<em>Addl Class Info</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Addl Class Info</em>'.
   * @generated
   */
  AddlClassInfo createAddlClassInfo();

  /**
   * Returns a new object of class '<em>Range</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Range</em>'.
   * @generated
   */
  Range createRange();

  /**
   * Returns a new object of class '<em>Range Type</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Range Type</em>'.
   * @generated
   */
  RangeType createRangeType();

  /**
   * Returns a new object of class '<em>User Defined Data Type</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>User Defined Data Type</em>'.
   * @generated
   */
  UserDefinedDataType createUserDefinedDataType();

  /**
   * Returns a new object of class '<em>Data Type Restriction</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Data Type Restriction</em>'.
   * @generated
   */
  DataTypeRestriction createDataTypeRestriction();

  /**
   * Returns a new object of class '<em>Facets</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Facets</em>'.
   * @generated
   */
  Facets createFacets();

  /**
   * Returns a new object of class '<em>Equivalent Concepts</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Equivalent Concepts</em>'.
   * @generated
   */
  EquivalentConcepts createEquivalentConcepts();

  /**
   * Returns a new object of class '<em>Disjoint Classes</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Disjoint Classes</em>'.
   * @generated
   */
  DisjointClasses createDisjointClasses();

  /**
   * Returns a new object of class '<em>Complement Of Class</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Complement Of Class</em>'.
   * @generated
   */
  ComplementOfClass createComplementOfClass();

  /**
   * Returns a new object of class '<em>All Values From</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>All Values From</em>'.
   * @generated
   */
  AllValuesFrom createAllValuesFrom();

  /**
   * Returns a new object of class '<em>Some Values From</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Some Values From</em>'.
   * @generated
   */
  SomeValuesFrom createSomeValuesFrom();

  /**
   * Returns a new object of class '<em>Has Value</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Has Value</em>'.
   * @generated
   */
  HasValue createHasValue();

  /**
   * Returns a new object of class '<em>Cardinality</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Cardinality</em>'.
   * @generated
   */
  Cardinality createCardinality();

  /**
   * Returns a new object of class '<em>Min Cardinality</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Min Cardinality</em>'.
   * @generated
   */
  MinCardinality createMinCardinality();

  /**
   * Returns a new object of class '<em>Max Cardinality</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Max Cardinality</em>'.
   * @generated
   */
  MaxCardinality createMaxCardinality();

  /**
   * Returns a new object of class '<em>Property Of Class</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Property Of Class</em>'.
   * @generated
   */
  PropertyOfClass createPropertyOfClass();

  /**
   * Returns a new object of class '<em>All Values Condition</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>All Values Condition</em>'.
   * @generated
   */
  AllValuesCondition createAllValuesCondition();

  /**
   * Returns a new object of class '<em>Enumerated All Values From</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Enumerated All Values From</em>'.
   * @generated
   */
  EnumeratedAllValuesFrom createEnumeratedAllValuesFrom();

  /**
   * Returns a new object of class '<em>Enumerated All And Some Values From</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Enumerated All And Some Values From</em>'.
   * @generated
   */
  EnumeratedAllAndSomeValuesFrom createEnumeratedAllAndSomeValuesFrom();

  /**
   * Returns a new object of class '<em>Default Value</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Default Value</em>'.
   * @generated
   */
  DefaultValue createDefaultValue();

  /**
   * Returns a new object of class '<em>Some Values Condition</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Some Values Condition</em>'.
   * @generated
   */
  SomeValuesCondition createSomeValuesCondition();

  /**
   * Returns a new object of class '<em>Has Value Condition</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Has Value Condition</em>'.
   * @generated
   */
  HasValueCondition createHasValueCondition();

  /**
   * Returns a new object of class '<em>Min Card Condition</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Min Card Condition</em>'.
   * @generated
   */
  MinCardCondition createMinCardCondition();

  /**
   * Returns a new object of class '<em>Max Card Condition</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Max Card Condition</em>'.
   * @generated
   */
  MaxCardCondition createMaxCardCondition();

  /**
   * Returns a new object of class '<em>Card Condition</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Card Condition</em>'.
   * @generated
   */
  CardCondition createCardCondition();

  /**
   * Returns a new object of class '<em>Necessary And Sufficient</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Necessary And Sufficient</em>'.
   * @generated
   */
  NecessaryAndSufficient createNecessaryAndSufficient();

  /**
   * Returns a new object of class '<em>Condition</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Condition</em>'.
   * @generated
   */
  Condition createCondition();

  /**
   * Returns a new object of class '<em>Property Declaration</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Property Declaration</em>'.
   * @generated
   */
  PropertyDeclaration createPropertyDeclaration();

  /**
   * Returns a new object of class '<em>Additional Property Info</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Additional Property Info</em>'.
   * @generated
   */
  AdditionalPropertyInfo createAdditionalPropertyInfo();

  /**
   * Returns a new object of class '<em>Functional Property</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Functional Property</em>'.
   * @generated
   */
  FunctionalProperty createFunctionalProperty();

  /**
   * Returns a new object of class '<em>Inverse Functional Property</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Inverse Functional Property</em>'.
   * @generated
   */
  InverseFunctionalProperty createInverseFunctionalProperty();

  /**
   * Returns a new object of class '<em>Symmetrical Property</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Symmetrical Property</em>'.
   * @generated
   */
  SymmetricalProperty createSymmetricalProperty();

  /**
   * Returns a new object of class '<em>Transitive Property</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Transitive Property</em>'.
   * @generated
   */
  TransitiveProperty createTransitiveProperty();

  /**
   * Returns a new object of class '<em>Inverse Property</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Inverse Property</em>'.
   * @generated
   */
  InverseProperty createInverseProperty();

  /**
   * Returns a new object of class '<em>Is Inverse Of</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Is Inverse Of</em>'.
   * @generated
   */
  IsInverseOf createIsInverseOf();

  /**
   * Returns a new object of class '<em>Typed BNode</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Typed BNode</em>'.
   * @generated
   */
  TypedBNode createTypedBNode();

  /**
   * Returns a new object of class '<em>Instance Declaration Statement</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Instance Declaration Statement</em>'.
   * @generated
   */
  InstanceDeclarationStatement createInstanceDeclarationStatement();

  /**
   * Returns a new object of class '<em>Instance Declaration</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Instance Declaration</em>'.
   * @generated
   */
  InstanceDeclaration createInstanceDeclaration();

  /**
   * Returns a new object of class '<em>Type Declaration</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Type Declaration</em>'.
   * @generated
   */
  TypeDeclaration createTypeDeclaration();

  /**
   * Returns a new object of class '<em>Instance Different From</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Instance Different From</em>'.
   * @generated
   */
  InstanceDifferentFrom createInstanceDifferentFrom();

  /**
   * Returns a new object of class '<em>Instances All Different</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Instances All Different</em>'.
   * @generated
   */
  InstancesAllDifferent createInstancesAllDifferent();

  /**
   * Returns a new object of class '<em>Existing Instance Attribution</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Existing Instance Attribution</em>'.
   * @generated
   */
  ExistingInstanceAttribution createExistingInstanceAttribution();

  /**
   * Returns a new object of class '<em>Object</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Object</em>'.
   * @generated
   */
  Object createObject();

  /**
   * Returns a new object of class '<em>Prop Val Partial Triple</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Prop Val Partial Triple</em>'.
   * @generated
   */
  PropValPartialTriple createPropValPartialTriple();

  /**
   * Returns a new object of class '<em>Of Pattern Returning Values</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Of Pattern Returning Values</em>'.
   * @generated
   */
  OfPatternReturningValues createOfPatternReturningValues();

  /**
   * Returns a new object of class '<em>With Chain</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>With Chain</em>'.
   * @generated
   */
  WithChain createWithChain();

  /**
   * Returns a new object of class '<em>With Phrase</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>With Phrase</em>'.
   * @generated
   */
  WithPhrase createWithPhrase();

  /**
   * Returns a new object of class '<em>Embedded Instance Declaration</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Embedded Instance Declaration</em>'.
   * @generated
   */
  EmbeddedInstanceDeclaration createEmbeddedInstanceDeclaration();

  /**
   * Returns a new object of class '<em>Merged Triples</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Merged Triples</em>'.
   * @generated
   */
  MergedTriples createMergedTriples();

  /**
   * Returns a new object of class '<em>Of Phrase</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Of Phrase</em>'.
   * @generated
   */
  OfPhrase createOfPhrase();

  /**
   * Returns a new object of class '<em>Variable List</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Variable List</em>'.
   * @generated
   */
  VariableList createVariableList();

  /**
   * Returns a new object of class '<em>Rule</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Rule</em>'.
   * @generated
   */
  Rule createRule();

  /**
   * Returns a new object of class '<em>Query</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Query</em>'.
   * @generated
   */
  Query createQuery();

  /**
   * Returns a new object of class '<em>Test</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Test</em>'.
   * @generated
   */
  Test createTest();

  /**
   * Returns a new object of class '<em>Expr</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Expr</em>'.
   * @generated
   */
  Expr createExpr();

  /**
   * Returns a new object of class '<em>Display</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Display</em>'.
   * @generated
   */
  Display createDisplay();

  /**
   * Returns a new object of class '<em>Explanation</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Explanation</em>'.
   * @generated
   */
  Explanation createExplanation();

  /**
   * Returns a new object of class '<em>Element Set</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Element Set</em>'.
   * @generated
   */
  ElementSet createElementSet();

  /**
   * Returns a new object of class '<em>Select Expression</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Select Expression</em>'.
   * @generated
   */
  SelectExpression createSelectExpression();

  /**
   * Returns a new object of class '<em>Construct Expression</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Construct Expression</em>'.
   * @generated
   */
  ConstructExpression createConstructExpression();

  /**
   * Returns a new object of class '<em>Ask Query Expression</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Ask Query Expression</em>'.
   * @generated
   */
  AskQueryExpression createAskQueryExpression();

  /**
   * Returns a new object of class '<em>Order List</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Order List</em>'.
   * @generated
   */
  OrderList createOrderList();

  /**
   * Returns a new object of class '<em>Order Element</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Order Element</em>'.
   * @generated
   */
  OrderElement createOrderElement();

  /**
   * Returns a new object of class '<em>Expression</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Expression</em>'.
   * @generated
   */
  Expression createExpression();

  /**
   * Returns a new object of class '<em>Graph Pattern</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Graph Pattern</em>'.
   * @generated
   */
  GraphPattern createGraphPattern();

  /**
   * Returns a new object of class '<em>Prop Of Subj</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Prop Of Subj</em>'.
   * @generated
   */
  PropOfSubj createPropOfSubj();

  /**
   * Returns a new object of class '<em>Subj Prop</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Subj Prop</em>'.
   * @generated
   */
  SubjProp createSubjProp();

  /**
   * Returns a new object of class '<em>Inst Attr SPV</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Inst Attr SPV</em>'.
   * @generated
   */
  InstAttrSPV createInstAttrSPV();

  /**
   * Returns a new object of class '<em>Inst Attr PSV</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Inst Attr PSV</em>'.
   * @generated
   */
  InstAttrPSV createInstAttrPSV();

  /**
   * Returns a new object of class '<em>Sub Type Of</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Sub Type Of</em>'.
   * @generated
   */
  SubTypeOf createSubTypeOf();

  /**
   * Returns a new object of class '<em>Interval Value</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Interval Value</em>'.
   * @generated
   */
  IntervalValue createIntervalValue();

  /**
   * Returns a new object of class '<em>Explicit Value</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Explicit Value</em>'.
   * @generated
   */
  ExplicitValue createExplicitValue();

  /**
   * Returns a new object of class '<em>Value Table</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Value Table</em>'.
   * @generated
   */
  ValueTable createValueTable();

  /**
   * Returns a new object of class '<em>Literal Value</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Literal Value</em>'.
   * @generated
   */
  LiteralValue createLiteralValue();

  /**
   * Returns a new object of class '<em>Value Row</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Value Row</em>'.
   * @generated
   */
  ValueRow createValueRow();

  /**
   * Returns a new object of class '<em>Junction Expression</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Junction Expression</em>'.
   * @generated
   */
  JunctionExpression createJunctionExpression();

  /**
   * Returns a new object of class '<em>Binary Op Expression</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Binary Op Expression</em>'.
   * @generated
   */
  BinaryOpExpression createBinaryOpExpression();

  /**
   * Returns a new object of class '<em>Unary Op Expression</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Unary Op Expression</em>'.
   * @generated
   */
  UnaryOpExpression createUnaryOpExpression();

  /**
   * Returns the package supported by this factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the package supported by this factory.
   * @generated
   */
  SadlPackage getSadlPackage();

} //SadlFactory
