/**
 */
package com.ge.research.sadl.sadl.impl;

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
import com.ge.research.sadl.sadl.DataType;
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
import com.ge.research.sadl.sadl.SadlFactory;
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

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class SadlFactoryImpl extends EFactoryImpl implements SadlFactory
{
  /**
   * Creates the default factory implementation.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public static SadlFactory init()
  {
    try
    {
      SadlFactory theSadlFactory = (SadlFactory)EPackage.Registry.INSTANCE.getEFactory(SadlPackage.eNS_URI);
      if (theSadlFactory != null)
      {
        return theSadlFactory;
      }
    }
    catch (Exception exception)
    {
      EcorePlugin.INSTANCE.log(exception);
    }
    return new SadlFactoryImpl();
  }

  /**
   * Creates an instance of the factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SadlFactoryImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public EObject create(EClass eClass)
  {
    switch (eClass.getClassifierID())
    {
      case SadlPackage.MODEL: return createModel();
      case SadlPackage.MODEL_NAME: return createModelName();
      case SadlPackage.IMPORT: return createImport();
      case SadlPackage.MODEL_ELEMENT: return createModelElement();
      case SadlPackage.STATEMENT: return createStatement();
      case SadlPackage.RESOURCE_NAME: return createResourceName();
      case SadlPackage.CONTENT_LIST: return createContentList();
      case SadlPackage.RESOURCE_LIST: return createResourceList();
      case SadlPackage.LITERAL_LIST: return createLiteralList();
      case SadlPackage.RESOURCE_BY_NAME: return createResourceByName();
      case SadlPackage.EXISTING_RESOURCE_LIST: return createExistingResourceList();
      case SadlPackage.RESOURCE_IDENTIFIER: return createResourceIdentifier();
      case SadlPackage.RESOURCE_BY_SET_OP: return createResourceBySetOp();
      case SadlPackage.RESOURCE_BY_RESTRICTION: return createResourceByRestriction();
      case SadlPackage.UNION_RESOURCE: return createUnionResource();
      case SadlPackage.INTERSECTION_RESOURCE: return createIntersectionResource();
      case SadlPackage.CLASS_DECLARATION: return createClassDeclaration();
      case SadlPackage.ENUMERATED_INSTANCES: return createEnumeratedInstances();
      case SadlPackage.ADDL_CLASS_INFO: return createAddlClassInfo();
      case SadlPackage.RANGE: return createRange();
      case SadlPackage.RANGE_TYPE: return createRangeType();
      case SadlPackage.USER_DEFINED_DATA_TYPE: return createUserDefinedDataType();
      case SadlPackage.DATA_TYPE_RESTRICTION: return createDataTypeRestriction();
      case SadlPackage.FACETS: return createFacets();
      case SadlPackage.EQUIVALENT_CONCEPTS: return createEquivalentConcepts();
      case SadlPackage.DISJOINT_CLASSES: return createDisjointClasses();
      case SadlPackage.COMPLEMENT_OF_CLASS: return createComplementOfClass();
      case SadlPackage.ALL_VALUES_FROM: return createAllValuesFrom();
      case SadlPackage.SOME_VALUES_FROM: return createSomeValuesFrom();
      case SadlPackage.HAS_VALUE: return createHasValue();
      case SadlPackage.CARDINALITY: return createCardinality();
      case SadlPackage.MIN_CARDINALITY: return createMinCardinality();
      case SadlPackage.MAX_CARDINALITY: return createMaxCardinality();
      case SadlPackage.PROPERTY_OF_CLASS: return createPropertyOfClass();
      case SadlPackage.ALL_VALUES_CONDITION: return createAllValuesCondition();
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM: return createEnumeratedAllValuesFrom();
      case SadlPackage.ENUMERATED_ALL_AND_SOME_VALUES_FROM: return createEnumeratedAllAndSomeValuesFrom();
      case SadlPackage.DEFAULT_VALUE: return createDefaultValue();
      case SadlPackage.SOME_VALUES_CONDITION: return createSomeValuesCondition();
      case SadlPackage.HAS_VALUE_CONDITION: return createHasValueCondition();
      case SadlPackage.MIN_CARD_CONDITION: return createMinCardCondition();
      case SadlPackage.MAX_CARD_CONDITION: return createMaxCardCondition();
      case SadlPackage.CARD_CONDITION: return createCardCondition();
      case SadlPackage.NECESSARY_AND_SUFFICIENT: return createNecessaryAndSufficient();
      case SadlPackage.CONDITION: return createCondition();
      case SadlPackage.PROPERTY_DECLARATION: return createPropertyDeclaration();
      case SadlPackage.ADDITIONAL_PROPERTY_INFO: return createAdditionalPropertyInfo();
      case SadlPackage.FUNCTIONAL_PROPERTY: return createFunctionalProperty();
      case SadlPackage.INVERSE_FUNCTIONAL_PROPERTY: return createInverseFunctionalProperty();
      case SadlPackage.SYMMETRICAL_PROPERTY: return createSymmetricalProperty();
      case SadlPackage.TRANSITIVE_PROPERTY: return createTransitiveProperty();
      case SadlPackage.INVERSE_PROPERTY: return createInverseProperty();
      case SadlPackage.IS_INVERSE_OF: return createIsInverseOf();
      case SadlPackage.TYPED_BNODE: return createTypedBNode();
      case SadlPackage.INSTANCE_DECLARATION_STATEMENT: return createInstanceDeclarationStatement();
      case SadlPackage.INSTANCE_DECLARATION: return createInstanceDeclaration();
      case SadlPackage.TYPE_DECLARATION: return createTypeDeclaration();
      case SadlPackage.INSTANCE_DIFFERENT_FROM: return createInstanceDifferentFrom();
      case SadlPackage.INSTANCES_ALL_DIFFERENT: return createInstancesAllDifferent();
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION: return createExistingInstanceAttribution();
      case SadlPackage.OBJECT: return createObject();
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE: return createPropValPartialTriple();
      case SadlPackage.OF_PATTERN_RETURNING_VALUES: return createOfPatternReturningValues();
      case SadlPackage.WITH_CHAIN: return createWithChain();
      case SadlPackage.WITH_PHRASE: return createWithPhrase();
      case SadlPackage.EMBEDDED_INSTANCE_DECLARATION: return createEmbeddedInstanceDeclaration();
      case SadlPackage.MERGED_TRIPLES: return createMergedTriples();
      case SadlPackage.OF_PHRASE: return createOfPhrase();
      case SadlPackage.VARIABLE_LIST: return createVariableList();
      case SadlPackage.RULE: return createRule();
      case SadlPackage.QUERY: return createQuery();
      case SadlPackage.TEST: return createTest();
      case SadlPackage.EXPR: return createExpr();
      case SadlPackage.DISPLAY: return createDisplay();
      case SadlPackage.EXPLANATION: return createExplanation();
      case SadlPackage.ELEMENT_SET: return createElementSet();
      case SadlPackage.SELECT_EXPRESSION: return createSelectExpression();
      case SadlPackage.CONSTRUCT_EXPRESSION: return createConstructExpression();
      case SadlPackage.ASK_QUERY_EXPRESSION: return createAskQueryExpression();
      case SadlPackage.ORDER_LIST: return createOrderList();
      case SadlPackage.ORDER_ELEMENT: return createOrderElement();
      case SadlPackage.EXPRESSION: return createExpression();
      case SadlPackage.GRAPH_PATTERN: return createGraphPattern();
      case SadlPackage.PROP_OF_SUBJ: return createPropOfSubj();
      case SadlPackage.SUBJ_PROP: return createSubjProp();
      case SadlPackage.INST_ATTR_SPV: return createInstAttrSPV();
      case SadlPackage.INST_ATTR_PSV: return createInstAttrPSV();
      case SadlPackage.SUB_TYPE_OF: return createSubTypeOf();
      case SadlPackage.EXISTENTIAL_NEGATION: return createExistentialNegation();
      case SadlPackage.INTERVAL_VALUE: return createIntervalValue();
      case SadlPackage.EXPLICIT_VALUE: return createExplicitValue();
      case SadlPackage.VALUE_TABLE: return createValueTable();
      case SadlPackage.LITERAL_VALUE: return createLiteralValue();
      case SadlPackage.VALUE_ROW: return createValueRow();
      case SadlPackage.JUNCTION_EXPRESSION: return createJunctionExpression();
      case SadlPackage.BINARY_OP_EXPRESSION: return createBinaryOpExpression();
      case SadlPackage.UNARY_OP_EXPRESSION: return createUnaryOpExpression();
      default:
        throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
    }
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object createFromString(EDataType eDataType, String initialValue)
  {
    switch (eDataType.getClassifierID())
    {
      case SadlPackage.DATA_TYPE:
        return createDataTypeFromString(eDataType, initialValue);
      default:
        throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
    }
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String convertToString(EDataType eDataType, Object instanceValue)
  {
    switch (eDataType.getClassifierID())
    {
      case SadlPackage.DATA_TYPE:
        return convertDataTypeToString(eDataType, instanceValue);
      default:
        throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
    }
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Model createModel()
  {
    ModelImpl model = new ModelImpl();
    return model;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ModelName createModelName()
  {
    ModelNameImpl modelName = new ModelNameImpl();
    return modelName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Import createImport()
  {
    ImportImpl import_ = new ImportImpl();
    return import_;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ModelElement createModelElement()
  {
    ModelElementImpl modelElement = new ModelElementImpl();
    return modelElement;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Statement createStatement()
  {
    StatementImpl statement = new StatementImpl();
    return statement;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName createResourceName()
  {
    ResourceNameImpl resourceName = new ResourceNameImpl();
    return resourceName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ContentList createContentList()
  {
    ContentListImpl contentList = new ContentListImpl();
    return contentList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceList createResourceList()
  {
    ResourceListImpl resourceList = new ResourceListImpl();
    return resourceList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public LiteralList createLiteralList()
  {
    LiteralListImpl literalList = new LiteralListImpl();
    return literalList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName createResourceByName()
  {
    ResourceByNameImpl resourceByName = new ResourceByNameImpl();
    return resourceByName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExistingResourceList createExistingResourceList()
  {
    ExistingResourceListImpl existingResourceList = new ExistingResourceListImpl();
    return existingResourceList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceIdentifier createResourceIdentifier()
  {
    ResourceIdentifierImpl resourceIdentifier = new ResourceIdentifierImpl();
    return resourceIdentifier;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceBySetOp createResourceBySetOp()
  {
    ResourceBySetOpImpl resourceBySetOp = new ResourceBySetOpImpl();
    return resourceBySetOp;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByRestriction createResourceByRestriction()
  {
    ResourceByRestrictionImpl resourceByRestriction = new ResourceByRestrictionImpl();
    return resourceByRestriction;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public UnionResource createUnionResource()
  {
    UnionResourceImpl unionResource = new UnionResourceImpl();
    return unionResource;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public IntersectionResource createIntersectionResource()
  {
    IntersectionResourceImpl intersectionResource = new IntersectionResourceImpl();
    return intersectionResource;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ClassDeclaration createClassDeclaration()
  {
    ClassDeclarationImpl classDeclaration = new ClassDeclarationImpl();
    return classDeclaration;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EnumeratedInstances createEnumeratedInstances()
  {
    EnumeratedInstancesImpl enumeratedInstances = new EnumeratedInstancesImpl();
    return enumeratedInstances;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public AddlClassInfo createAddlClassInfo()
  {
    AddlClassInfoImpl addlClassInfo = new AddlClassInfoImpl();
    return addlClassInfo;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Range createRange()
  {
    RangeImpl range = new RangeImpl();
    return range;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public RangeType createRangeType()
  {
    RangeTypeImpl rangeType = new RangeTypeImpl();
    return rangeType;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public UserDefinedDataType createUserDefinedDataType()
  {
    UserDefinedDataTypeImpl userDefinedDataType = new UserDefinedDataTypeImpl();
    return userDefinedDataType;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public DataTypeRestriction createDataTypeRestriction()
  {
    DataTypeRestrictionImpl dataTypeRestriction = new DataTypeRestrictionImpl();
    return dataTypeRestriction;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Facets createFacets()
  {
    FacetsImpl facets = new FacetsImpl();
    return facets;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EquivalentConcepts createEquivalentConcepts()
  {
    EquivalentConceptsImpl equivalentConcepts = new EquivalentConceptsImpl();
    return equivalentConcepts;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public DisjointClasses createDisjointClasses()
  {
    DisjointClassesImpl disjointClasses = new DisjointClassesImpl();
    return disjointClasses;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ComplementOfClass createComplementOfClass()
  {
    ComplementOfClassImpl complementOfClass = new ComplementOfClassImpl();
    return complementOfClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public AllValuesFrom createAllValuesFrom()
  {
    AllValuesFromImpl allValuesFrom = new AllValuesFromImpl();
    return allValuesFrom;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SomeValuesFrom createSomeValuesFrom()
  {
    SomeValuesFromImpl someValuesFrom = new SomeValuesFromImpl();
    return someValuesFrom;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public HasValue createHasValue()
  {
    HasValueImpl hasValue = new HasValueImpl();
    return hasValue;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Cardinality createCardinality()
  {
    CardinalityImpl cardinality = new CardinalityImpl();
    return cardinality;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public MinCardinality createMinCardinality()
  {
    MinCardinalityImpl minCardinality = new MinCardinalityImpl();
    return minCardinality;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public MaxCardinality createMaxCardinality()
  {
    MaxCardinalityImpl maxCardinality = new MaxCardinalityImpl();
    return maxCardinality;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public PropertyOfClass createPropertyOfClass()
  {
    PropertyOfClassImpl propertyOfClass = new PropertyOfClassImpl();
    return propertyOfClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public AllValuesCondition createAllValuesCondition()
  {
    AllValuesConditionImpl allValuesCondition = new AllValuesConditionImpl();
    return allValuesCondition;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EnumeratedAllValuesFrom createEnumeratedAllValuesFrom()
  {
    EnumeratedAllValuesFromImpl enumeratedAllValuesFrom = new EnumeratedAllValuesFromImpl();
    return enumeratedAllValuesFrom;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EnumeratedAllAndSomeValuesFrom createEnumeratedAllAndSomeValuesFrom()
  {
    EnumeratedAllAndSomeValuesFromImpl enumeratedAllAndSomeValuesFrom = new EnumeratedAllAndSomeValuesFromImpl();
    return enumeratedAllAndSomeValuesFrom;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public DefaultValue createDefaultValue()
  {
    DefaultValueImpl defaultValue = new DefaultValueImpl();
    return defaultValue;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SomeValuesCondition createSomeValuesCondition()
  {
    SomeValuesConditionImpl someValuesCondition = new SomeValuesConditionImpl();
    return someValuesCondition;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public HasValueCondition createHasValueCondition()
  {
    HasValueConditionImpl hasValueCondition = new HasValueConditionImpl();
    return hasValueCondition;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public MinCardCondition createMinCardCondition()
  {
    MinCardConditionImpl minCardCondition = new MinCardConditionImpl();
    return minCardCondition;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public MaxCardCondition createMaxCardCondition()
  {
    MaxCardConditionImpl maxCardCondition = new MaxCardConditionImpl();
    return maxCardCondition;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public CardCondition createCardCondition()
  {
    CardConditionImpl cardCondition = new CardConditionImpl();
    return cardCondition;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NecessaryAndSufficient createNecessaryAndSufficient()
  {
    NecessaryAndSufficientImpl necessaryAndSufficient = new NecessaryAndSufficientImpl();
    return necessaryAndSufficient;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Condition createCondition()
  {
    ConditionImpl condition = new ConditionImpl();
    return condition;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public PropertyDeclaration createPropertyDeclaration()
  {
    PropertyDeclarationImpl propertyDeclaration = new PropertyDeclarationImpl();
    return propertyDeclaration;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public AdditionalPropertyInfo createAdditionalPropertyInfo()
  {
    AdditionalPropertyInfoImpl additionalPropertyInfo = new AdditionalPropertyInfoImpl();
    return additionalPropertyInfo;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public FunctionalProperty createFunctionalProperty()
  {
    FunctionalPropertyImpl functionalProperty = new FunctionalPropertyImpl();
    return functionalProperty;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public InverseFunctionalProperty createInverseFunctionalProperty()
  {
    InverseFunctionalPropertyImpl inverseFunctionalProperty = new InverseFunctionalPropertyImpl();
    return inverseFunctionalProperty;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SymmetricalProperty createSymmetricalProperty()
  {
    SymmetricalPropertyImpl symmetricalProperty = new SymmetricalPropertyImpl();
    return symmetricalProperty;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public TransitiveProperty createTransitiveProperty()
  {
    TransitivePropertyImpl transitiveProperty = new TransitivePropertyImpl();
    return transitiveProperty;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public InverseProperty createInverseProperty()
  {
    InversePropertyImpl inverseProperty = new InversePropertyImpl();
    return inverseProperty;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public IsInverseOf createIsInverseOf()
  {
    IsInverseOfImpl isInverseOf = new IsInverseOfImpl();
    return isInverseOf;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public TypedBNode createTypedBNode()
  {
    TypedBNodeImpl typedBNode = new TypedBNodeImpl();
    return typedBNode;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public InstanceDeclarationStatement createInstanceDeclarationStatement()
  {
    InstanceDeclarationStatementImpl instanceDeclarationStatement = new InstanceDeclarationStatementImpl();
    return instanceDeclarationStatement;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public InstanceDeclaration createInstanceDeclaration()
  {
    InstanceDeclarationImpl instanceDeclaration = new InstanceDeclarationImpl();
    return instanceDeclaration;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public TypeDeclaration createTypeDeclaration()
  {
    TypeDeclarationImpl typeDeclaration = new TypeDeclarationImpl();
    return typeDeclaration;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public InstanceDifferentFrom createInstanceDifferentFrom()
  {
    InstanceDifferentFromImpl instanceDifferentFrom = new InstanceDifferentFromImpl();
    return instanceDifferentFrom;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public InstancesAllDifferent createInstancesAllDifferent()
  {
    InstancesAllDifferentImpl instancesAllDifferent = new InstancesAllDifferentImpl();
    return instancesAllDifferent;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExistingInstanceAttribution createExistingInstanceAttribution()
  {
    ExistingInstanceAttributionImpl existingInstanceAttribution = new ExistingInstanceAttributionImpl();
    return existingInstanceAttribution;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public com.ge.research.sadl.sadl.Object createObject()
  {
    ObjectImpl object = new ObjectImpl();
    return object;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public PropValPartialTriple createPropValPartialTriple()
  {
    PropValPartialTripleImpl propValPartialTriple = new PropValPartialTripleImpl();
    return propValPartialTriple;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public OfPatternReturningValues createOfPatternReturningValues()
  {
    OfPatternReturningValuesImpl ofPatternReturningValues = new OfPatternReturningValuesImpl();
    return ofPatternReturningValues;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public WithChain createWithChain()
  {
    WithChainImpl withChain = new WithChainImpl();
    return withChain;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public WithPhrase createWithPhrase()
  {
    WithPhraseImpl withPhrase = new WithPhraseImpl();
    return withPhrase;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EmbeddedInstanceDeclaration createEmbeddedInstanceDeclaration()
  {
    EmbeddedInstanceDeclarationImpl embeddedInstanceDeclaration = new EmbeddedInstanceDeclarationImpl();
    return embeddedInstanceDeclaration;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public MergedTriples createMergedTriples()
  {
    MergedTriplesImpl mergedTriples = new MergedTriplesImpl();
    return mergedTriples;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public OfPhrase createOfPhrase()
  {
    OfPhraseImpl ofPhrase = new OfPhraseImpl();
    return ofPhrase;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public VariableList createVariableList()
  {
    VariableListImpl variableList = new VariableListImpl();
    return variableList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Rule createRule()
  {
    RuleImpl rule = new RuleImpl();
    return rule;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Query createQuery()
  {
    QueryImpl query = new QueryImpl();
    return query;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Test createTest()
  {
    TestImpl test = new TestImpl();
    return test;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Expr createExpr()
  {
    ExprImpl expr = new ExprImpl();
    return expr;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Display createDisplay()
  {
    DisplayImpl display = new DisplayImpl();
    return display;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Explanation createExplanation()
  {
    ExplanationImpl explanation = new ExplanationImpl();
    return explanation;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ElementSet createElementSet()
  {
    ElementSetImpl elementSet = new ElementSetImpl();
    return elementSet;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SelectExpression createSelectExpression()
  {
    SelectExpressionImpl selectExpression = new SelectExpressionImpl();
    return selectExpression;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ConstructExpression createConstructExpression()
  {
    ConstructExpressionImpl constructExpression = new ConstructExpressionImpl();
    return constructExpression;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public AskQueryExpression createAskQueryExpression()
  {
    AskQueryExpressionImpl askQueryExpression = new AskQueryExpressionImpl();
    return askQueryExpression;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public OrderList createOrderList()
  {
    OrderListImpl orderList = new OrderListImpl();
    return orderList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public OrderElement createOrderElement()
  {
    OrderElementImpl orderElement = new OrderElementImpl();
    return orderElement;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Expression createExpression()
  {
    ExpressionImpl expression = new ExpressionImpl();
    return expression;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public GraphPattern createGraphPattern()
  {
    GraphPatternImpl graphPattern = new GraphPatternImpl();
    return graphPattern;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public PropOfSubj createPropOfSubj()
  {
    PropOfSubjImpl propOfSubj = new PropOfSubjImpl();
    return propOfSubj;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SubjProp createSubjProp()
  {
    SubjPropImpl subjProp = new SubjPropImpl();
    return subjProp;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public InstAttrSPV createInstAttrSPV()
  {
    InstAttrSPVImpl instAttrSPV = new InstAttrSPVImpl();
    return instAttrSPV;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public InstAttrPSV createInstAttrPSV()
  {
    InstAttrPSVImpl instAttrPSV = new InstAttrPSVImpl();
    return instAttrPSV;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SubTypeOf createSubTypeOf()
  {
    SubTypeOfImpl subTypeOf = new SubTypeOfImpl();
    return subTypeOf;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExistentialNegation createExistentialNegation()
  {
    ExistentialNegationImpl existentialNegation = new ExistentialNegationImpl();
    return existentialNegation;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public IntervalValue createIntervalValue()
  {
    IntervalValueImpl intervalValue = new IntervalValueImpl();
    return intervalValue;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExplicitValue createExplicitValue()
  {
    ExplicitValueImpl explicitValue = new ExplicitValueImpl();
    return explicitValue;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ValueTable createValueTable()
  {
    ValueTableImpl valueTable = new ValueTableImpl();
    return valueTable;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public LiteralValue createLiteralValue()
  {
    LiteralValueImpl literalValue = new LiteralValueImpl();
    return literalValue;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ValueRow createValueRow()
  {
    ValueRowImpl valueRow = new ValueRowImpl();
    return valueRow;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public JunctionExpression createJunctionExpression()
  {
    JunctionExpressionImpl junctionExpression = new JunctionExpressionImpl();
    return junctionExpression;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public BinaryOpExpression createBinaryOpExpression()
  {
    BinaryOpExpressionImpl binaryOpExpression = new BinaryOpExpressionImpl();
    return binaryOpExpression;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public UnaryOpExpression createUnaryOpExpression()
  {
    UnaryOpExpressionImpl unaryOpExpression = new UnaryOpExpressionImpl();
    return unaryOpExpression;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public DataType createDataTypeFromString(EDataType eDataType, String initialValue)
  {
    DataType result = DataType.get(initialValue);
    if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
    return result;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String convertDataTypeToString(EDataType eDataType, Object instanceValue)
  {
    return instanceValue == null ? null : instanceValue.toString();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SadlPackage getSadlPackage()
  {
    return (SadlPackage)getEPackage();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @deprecated
   * @generated
   */
  @Deprecated
  public static SadlPackage getPackage()
  {
    return SadlPackage.eINSTANCE;
  }

} //SadlFactoryImpl
