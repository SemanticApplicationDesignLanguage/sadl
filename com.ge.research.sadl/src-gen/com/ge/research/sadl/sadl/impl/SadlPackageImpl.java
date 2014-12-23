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
import com.ge.research.sadl.sadl.ValueRow;
import com.ge.research.sadl.sadl.ValueTable;
import com.ge.research.sadl.sadl.VariableList;
import com.ge.research.sadl.sadl.WithChain;
import com.ge.research.sadl.sadl.WithPhrase;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class SadlPackageImpl extends EPackageImpl implements SadlPackage
{
  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass modelEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass modelNameEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass importEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass modelElementEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass statementEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass resourceNameEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass contentListEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass resourceListEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass literalListEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass resourceByNameEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass existingResourceListEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass resourceIdentifierEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass resourceBySetOpEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass resourceByRestrictionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass unionResourceEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass intersectionResourceEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass classDeclarationEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass enumeratedInstancesEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass addlClassInfoEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass rangeEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass rangeTypeEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass equivalentConceptsEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass disjointClassesEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass complementOfClassEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass allValuesFromEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass someValuesFromEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass hasValueEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass cardinalityEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass minCardinalityEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass maxCardinalityEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass propertyOfClassEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass allValuesConditionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass enumeratedAllValuesFromEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass enumeratedAllAndSomeValuesFromEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass defaultValueEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass someValuesConditionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass hasValueConditionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass minCardConditionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass maxCardConditionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass cardConditionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass necessaryAndSufficientEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass conditionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass propertyDeclarationEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass additionalPropertyInfoEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass functionalPropertyEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass inverseFunctionalPropertyEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass symmetricalPropertyEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass transitivePropertyEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass inversePropertyEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass isInverseOfEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass typedBNodeEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass instanceDeclarationStatementEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass instanceDeclarationEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass typeDeclarationEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass instanceDifferentFromEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass instancesAllDifferentEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass existingInstanceAttributionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass objectEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass propValPartialTripleEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass ofPatternReturningValuesEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass withChainEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass withPhraseEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass embeddedInstanceDeclarationEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass mergedTriplesEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass ofPhraseEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass variableListEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass ruleEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass queryEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass testEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass exprEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass displayEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass explanationEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass elementSetEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass selectExpressionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass constructExpressionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass askQueryExpressionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass orderListEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass orderElementEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass expressionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass graphPatternEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass propOfSubjEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass subjPropEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass instAttrSPVEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass instAttrPSVEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass subTypeOfEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass intervalValueEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass explicitValueEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass valueTableEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass literalValueEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass valueRowEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass junctionExpressionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass binaryOpExpressionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass unaryOpExpressionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EEnum dataTypeEEnum = null;

  /**
   * Creates an instance of the model <b>Package</b>, registered with
   * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
   * package URI value.
   * <p>Note: the correct way to create the package is via the static
   * factory method {@link #init init()}, which also performs
   * initialization of the package, or returns the registered package,
   * if one already exists.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.emf.ecore.EPackage.Registry
   * @see com.ge.research.sadl.sadl.SadlPackage#eNS_URI
   * @see #init()
   * @generated
   */
  private SadlPackageImpl()
  {
    super(eNS_URI, SadlFactory.eINSTANCE);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private static boolean isInited = false;

  /**
   * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
   * 
   * <p>This method is used to initialize {@link SadlPackage#eINSTANCE} when that field is accessed.
   * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #eNS_URI
   * @see #createPackageContents()
   * @see #initializePackageContents()
   * @generated
   */
  public static SadlPackage init()
  {
    if (isInited) return (SadlPackage)EPackage.Registry.INSTANCE.getEPackage(SadlPackage.eNS_URI);

    // Obtain or create and register package
    SadlPackageImpl theSadlPackage = (SadlPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof SadlPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new SadlPackageImpl());

    isInited = true;

    // Create package meta-data objects
    theSadlPackage.createPackageContents();

    // Initialize created meta-data
    theSadlPackage.initializePackageContents();

    // Mark meta-data to indicate it can't be changed
    theSadlPackage.freeze();

  
    // Update the registry and return the package
    EPackage.Registry.INSTANCE.put(SadlPackage.eNS_URI, theSadlPackage);
    return theSadlPackage;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getModel()
  {
    return modelEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getModel_ModelName()
  {
    return (EReference)modelEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getModel_Imports()
  {
    return (EReference)modelEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getModel_Elements()
  {
    return (EReference)modelEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getModelName()
  {
    return modelNameEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getModelName_BaseUri()
  {
    return (EAttribute)modelNameEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getModelName_Alias()
  {
    return (EAttribute)modelNameEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getModelName_Version()
  {
    return (EAttribute)modelNameEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getModelName_AnnContent()
  {
    return (EReference)modelNameEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getImport()
  {
    return importEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getImport_ImportURI()
  {
    return (EAttribute)importEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getImport_Alias()
  {
    return (EAttribute)importEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getModelElement()
  {
    return modelElementEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getStatement()
  {
    return statementEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getResourceName()
  {
    return resourceNameEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getResourceName_Name()
  {
    return (EAttribute)resourceNameEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getResourceName_AnnType()
  {
    return (EAttribute)resourceNameEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getResourceName_AnnContent()
  {
    return (EReference)resourceNameEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getContentList()
  {
    return contentListEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getContentList_AnnContent()
  {
    return (EAttribute)contentListEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getResourceList()
  {
    return resourceListEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getResourceList_Names()
  {
    return (EReference)resourceListEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getLiteralList()
  {
    return literalListEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getLiteralList_Literals()
  {
    return (EReference)literalListEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getResourceByName()
  {
    return resourceByNameEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getResourceByName_Name()
  {
    return (EReference)resourceByNameEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getExistingResourceList()
  {
    return existingResourceListEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExistingResourceList_Names()
  {
    return (EReference)existingResourceListEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getResourceIdentifier()
  {
    return resourceIdentifierEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getResourceBySetOp()
  {
    return resourceBySetOpEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getResourceBySetOp_AnnType()
  {
    return (EAttribute)resourceBySetOpEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getResourceBySetOp_AnnContent()
  {
    return (EAttribute)resourceBySetOpEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getResourceBySetOp_Names()
  {
    return (EReference)resourceBySetOpEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getResourceBySetOp_Op()
  {
    return (EAttribute)resourceBySetOpEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getResourceByRestriction()
  {
    return resourceByRestrictionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getResourceByRestriction_AnnType()
  {
    return (EAttribute)resourceByRestrictionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getResourceByRestriction_AnnContent()
  {
    return (EAttribute)resourceByRestrictionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getResourceByRestriction_PropName()
  {
    return (EReference)resourceByRestrictionEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getResourceByRestriction_Cond()
  {
    return (EReference)resourceByRestrictionEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getUnionResource()
  {
    return unionResourceEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getIntersectionResource()
  {
    return intersectionResourceEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getClassDeclaration()
  {
    return classDeclarationEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getClassDeclaration_ClassName()
  {
    return (EReference)classDeclarationEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getClassDeclaration_MustBeOneOf()
  {
    return (EReference)classDeclarationEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getClassDeclaration_DescribedBy()
  {
    return (EReference)classDeclarationEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getClassDeclaration_ClassList()
  {
    return (EReference)classDeclarationEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getClassDeclaration_ClassIdentifier()
  {
    return (EReference)classDeclarationEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getEnumeratedInstances()
  {
    return enumeratedInstancesEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getEnumeratedInstances_InstanceList()
  {
    return (EReference)enumeratedInstancesEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAddlClassInfo()
  {
    return addlClassInfoEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAddlClassInfo_PropertyByName()
  {
    return (EReference)addlClassInfoEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAddlClassInfo_PropertyName()
  {
    return (EReference)addlClassInfoEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAddlClassInfo_Range()
  {
    return (EReference)addlClassInfoEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAddlClassInfo_Restriction()
  {
    return (EReference)addlClassInfoEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getRange()
  {
    return rangeEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRange_Single()
  {
    return (EAttribute)rangeEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getRange_Type()
  {
    return (EReference)rangeEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getRangeType()
  {
    return rangeTypeEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getRangeType_ClassIdentifier()
  {
    return (EReference)rangeTypeEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRangeType_DataType()
  {
    return (EAttribute)rangeTypeEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getEquivalentConcepts()
  {
    return equivalentConceptsEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getEquivalentConcepts_Class1()
  {
    return (EReference)equivalentConceptsEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getEquivalentConcepts_Class2()
  {
    return (EReference)equivalentConceptsEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getDisjointClasses()
  {
    return disjointClassesEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getDisjointClasses_Class1()
  {
    return (EReference)disjointClassesEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getDisjointClasses_Class2()
  {
    return (EReference)disjointClassesEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getDisjointClasses_Classes()
  {
    return (EReference)disjointClassesEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getComplementOfClass()
  {
    return complementOfClassEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getComplementOfClass_Class1()
  {
    return (EReference)complementOfClassEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getComplementOfClass_Class2()
  {
    return (EReference)complementOfClassEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAllValuesFrom()
  {
    return allValuesFromEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAllValuesFrom_Restricted()
  {
    return (EReference)allValuesFromEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAllValuesFrom_Cond()
  {
    return (EReference)allValuesFromEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAllValuesFrom_ClassName()
  {
    return (EReference)allValuesFromEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAllValuesFrom_PropertyName()
  {
    return (EReference)allValuesFromEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSomeValuesFrom()
  {
    return someValuesFromEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSomeValuesFrom_Restricted()
  {
    return (EReference)someValuesFromEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSomeValuesFrom_Cond()
  {
    return (EReference)someValuesFromEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSomeValuesFrom_ClassName()
  {
    return (EReference)someValuesFromEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSomeValuesFrom_PropertyName()
  {
    return (EReference)someValuesFromEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getHasValue()
  {
    return hasValueEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getHasValue_Restricted()
  {
    return (EReference)hasValueEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getHasValue_Cond()
  {
    return (EReference)hasValueEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getHasValue_ClassName()
  {
    return (EReference)hasValueEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getHasValue_PropertyName()
  {
    return (EReference)hasValueEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getCardinality()
  {
    return cardinalityEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getCardinality_Restricted()
  {
    return (EReference)cardinalityEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getCardinality_Cond()
  {
    return (EReference)cardinalityEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getCardinality_ClassName()
  {
    return (EReference)cardinalityEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getCardinality_PropertyName()
  {
    return (EReference)cardinalityEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getMinCardinality()
  {
    return minCardinalityEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMinCardinality_Restricted()
  {
    return (EReference)minCardinalityEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMinCardinality_Cond()
  {
    return (EReference)minCardinalityEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMinCardinality_ClassName()
  {
    return (EReference)minCardinalityEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMinCardinality_PropertyName()
  {
    return (EReference)minCardinalityEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getMaxCardinality()
  {
    return maxCardinalityEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMaxCardinality_Restricted()
  {
    return (EReference)maxCardinalityEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMaxCardinality_Cond()
  {
    return (EReference)maxCardinalityEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMaxCardinality_ClassName()
  {
    return (EReference)maxCardinalityEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMaxCardinality_PropertyName()
  {
    return (EReference)maxCardinalityEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getPropertyOfClass()
  {
    return propertyOfClassEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropertyOfClass_PropertyName()
  {
    return (EReference)propertyOfClassEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropertyOfClass_ClassName()
  {
    return (EReference)propertyOfClassEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAllValuesCondition()
  {
    return allValuesConditionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAllValuesCondition_Restriction()
  {
    return (EReference)allValuesConditionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getEnumeratedAllValuesFrom()
  {
    return enumeratedAllValuesFromEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getEnumeratedAllValuesFrom_Restricted()
  {
    return (EReference)enumeratedAllValuesFromEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getEnumeratedAllValuesFrom_Enumeration()
  {
    return (EReference)enumeratedAllValuesFromEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getEnumeratedAllAndSomeValuesFrom()
  {
    return enumeratedAllAndSomeValuesFromEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getEnumeratedAllAndSomeValuesFrom_Restricted()
  {
    return (EReference)enumeratedAllAndSomeValuesFromEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getEnumeratedAllAndSomeValuesFrom_Enumeration()
  {
    return (EReference)enumeratedAllAndSomeValuesFromEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getDefaultValue()
  {
    return defaultValueEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getDefaultValue_DefValueClass()
  {
    return (EReference)defaultValueEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getDefaultValue_Level()
  {
    return (EAttribute)defaultValueEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getDefaultValue_DefValue()
  {
    return (EReference)defaultValueEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSomeValuesCondition()
  {
    return someValuesConditionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSomeValuesCondition_Restriction()
  {
    return (EReference)someValuesConditionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getHasValueCondition()
  {
    return hasValueConditionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getHasValueCondition_Restriction()
  {
    return (EReference)hasValueConditionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getMinCardCondition()
  {
    return minCardConditionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getMinCardCondition_Card()
  {
    return (EAttribute)minCardConditionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMinCardCondition_ClassQualifier()
  {
    return (EReference)minCardConditionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getMaxCardCondition()
  {
    return maxCardConditionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getMaxCardCondition_Card()
  {
    return (EAttribute)maxCardConditionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMaxCardCondition_ClassQualifier()
  {
    return (EReference)maxCardConditionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getCardCondition()
  {
    return cardConditionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getCardCondition_Card()
  {
    return (EAttribute)cardConditionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getCardCondition_ClassQualifier()
  {
    return (EReference)cardConditionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getNecessaryAndSufficient()
  {
    return necessaryAndSufficientEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getNecessaryAndSufficient_SuperClass()
  {
    return (EReference)necessaryAndSufficientEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getNecessaryAndSufficient_Article()
  {
    return (EAttribute)necessaryAndSufficientEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getNecessaryAndSufficient_SubClass()
  {
    return (EReference)necessaryAndSufficientEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getNecessaryAndSufficient_PropertyName()
  {
    return (EReference)necessaryAndSufficientEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getNecessaryAndSufficient_Cond()
  {
    return (EReference)necessaryAndSufficientEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getCondition()
  {
    return conditionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getPropertyDeclaration()
  {
    return propertyDeclarationEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropertyDeclaration_PropertyName()
  {
    return (EReference)propertyDeclarationEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropertyDeclaration_SuperPropName()
  {
    return (EReference)propertyDeclarationEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropertyDeclaration_AddlPropInfo()
  {
    return (EReference)propertyDeclarationEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getPropertyDeclaration_Article()
  {
    return (EAttribute)propertyDeclarationEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropertyDeclaration_Domain()
  {
    return (EReference)propertyDeclarationEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropertyDeclaration_RangeResource()
  {
    return (EReference)propertyDeclarationEClass.getEStructuralFeatures().get(5);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropertyDeclaration_AnnotationProperty()
  {
    return (EReference)propertyDeclarationEClass.getEStructuralFeatures().get(6);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAdditionalPropertyInfo()
  {
    return additionalPropertyInfoEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAdditionalPropertyInfo_Domain()
  {
    return (EReference)additionalPropertyInfoEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAdditionalPropertyInfo_Cond()
  {
    return (EReference)additionalPropertyInfoEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAdditionalPropertyInfo_Range()
  {
    return (EReference)additionalPropertyInfoEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getAdditionalPropertyInfo_Isfunc()
  {
    return (EAttribute)additionalPropertyInfoEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getAdditionalPropertyInfo_Isinvfunc()
  {
    return (EAttribute)additionalPropertyInfoEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getAdditionalPropertyInfo_IsSym()
  {
    return (EAttribute)additionalPropertyInfoEClass.getEStructuralFeatures().get(5);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getAdditionalPropertyInfo_IsTrans()
  {
    return (EAttribute)additionalPropertyInfoEClass.getEStructuralFeatures().get(6);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAdditionalPropertyInfo_IsInvOf()
  {
    return (EReference)additionalPropertyInfoEClass.getEStructuralFeatures().get(7);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getFunctionalProperty()
  {
    return functionalPropertyEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getFunctionalProperty_PropertyName()
  {
    return (EReference)functionalPropertyEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getInverseFunctionalProperty()
  {
    return inverseFunctionalPropertyEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInverseFunctionalProperty_PropertyName()
  {
    return (EReference)inverseFunctionalPropertyEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSymmetricalProperty()
  {
    return symmetricalPropertyEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSymmetricalProperty_PropertyName()
  {
    return (EReference)symmetricalPropertyEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getTransitiveProperty()
  {
    return transitivePropertyEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getTransitiveProperty_PropertyName()
  {
    return (EReference)transitivePropertyEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getInverseProperty()
  {
    return inversePropertyEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInverseProperty_PropertyName1()
  {
    return (EReference)inversePropertyEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInverseProperty_InvOf()
  {
    return (EReference)inversePropertyEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getIsInverseOf()
  {
    return isInverseOfEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getIsInverseOf_PropertyName2()
  {
    return (EReference)isInverseOfEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getTypedBNode()
  {
    return typedBNodeEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getTypedBNode_Article()
  {
    return (EAttribute)typedBNodeEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getTypedBNode_ClassIdentifier()
  {
    return (EReference)typedBNodeEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getInstanceDeclarationStatement()
  {
    return instanceDeclarationStatementEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getInstanceDeclaration()
  {
    return instanceDeclarationEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstanceDeclaration_TypeDecl()
  {
    return (EReference)instanceDeclarationEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstanceDeclaration_AddlInfoItems()
  {
    return (EReference)instanceDeclarationEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getInstanceDeclaration_Article()
  {
    return (EAttribute)instanceDeclarationEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstanceDeclaration_ClassName()
  {
    return (EReference)instanceDeclarationEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstanceDeclaration_InstanceName()
  {
    return (EReference)instanceDeclarationEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getTypeDeclaration()
  {
    return typeDeclarationEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getTypeDeclaration_InstName()
  {
    return (EReference)typeDeclarationEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getTypeDeclaration_Type()
  {
    return (EReference)typeDeclarationEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getInstanceDifferentFrom()
  {
    return instanceDifferentFromEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstanceDifferentFrom_InstName1()
  {
    return (EReference)instanceDifferentFromEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstanceDifferentFrom_InstName2()
  {
    return (EReference)instanceDifferentFromEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getInstancesAllDifferent()
  {
    return instancesAllDifferentEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstancesAllDifferent_Instances()
  {
    return (EReference)instancesAllDifferentEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getExistingInstanceAttribution()
  {
    return existingInstanceAttributionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExistingInstanceAttribution_Subj()
  {
    return (EReference)existingInstanceAttributionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExistingInstanceAttribution_AddlInfoItems()
  {
    return (EReference)existingInstanceAttributionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExistingInstanceAttribution_POfS()
  {
    return (EReference)existingInstanceAttributionEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExistingInstanceAttribution_Obj()
  {
    return (EReference)existingInstanceAttributionEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getObject()
  {
    return objectEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getObject_Val()
  {
    return (EReference)objectEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getPropValPartialTriple()
  {
    return propValPartialTripleEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropValPartialTriple_PropertyName()
  {
    return (EReference)propValPartialTripleEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropValPartialTriple_ObjectValue()
  {
    return (EReference)propValPartialTripleEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropValPartialTriple_ObjectValueBNode()
  {
    return (EReference)propValPartialTripleEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getOfPatternReturningValues()
  {
    return ofPatternReturningValuesEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getOfPatternReturningValues_Ofphrs()
  {
    return (EReference)ofPatternReturningValuesEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getOfPatternReturningValues_Subject()
  {
    return (EReference)ofPatternReturningValuesEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getOfPatternReturningValues_Type()
  {
    return (EReference)ofPatternReturningValuesEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getWithChain()
  {
    return withChainEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getWithChain_Wps()
  {
    return (EReference)withChainEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getWithPhrase()
  {
    return withPhraseEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getWithPhrase_PropertyName()
  {
    return (EReference)withPhraseEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getWithPhrase_Value()
  {
    return (EReference)withPhraseEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getEmbeddedInstanceDeclaration()
  {
    return embeddedInstanceDeclarationEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getMergedTriples()
  {
    return mergedTriplesEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMergedTriples_Ops()
  {
    return (EReference)mergedTriplesEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMergedTriples_Pivot()
  {
    return (EReference)mergedTriplesEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getMergedTriples_Wcs()
  {
    return (EReference)mergedTriplesEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getOfPhrase()
  {
    return ofPhraseEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getOfPhrase_Article()
  {
    return (EAttribute)ofPhraseEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getOfPhrase_PropertyName()
  {
    return (EReference)ofPhraseEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getVariableList()
  {
    return variableListEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getVariableList_Names()
  {
    return (EReference)variableListEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getRule()
  {
    return ruleEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRule_Name()
  {
    return (EAttribute)ruleEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getRule_Givens()
  {
    return (EReference)ruleEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getRule_Ifs()
  {
    return (EReference)ruleEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getRule_Thens()
  {
    return (EReference)ruleEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getQuery()
  {
    return queryEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getQuery_Expr()
  {
    return (EReference)queryEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getTest()
  {
    return testEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getTest_Expr()
  {
    return (EReference)testEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getExpr()
  {
    return exprEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExpr_Expr()
  {
    return (EReference)exprEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getDisplay()
  {
    return displayEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getDisplay_DisplayString()
  {
    return (EAttribute)displayEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getDisplay_Model()
  {
    return (EAttribute)displayEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getExplanation()
  {
    return explanationEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExplanation_Expr()
  {
    return (EReference)explanationEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getExplanation_Rulename()
  {
    return (EAttribute)explanationEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getElementSet()
  {
    return elementSetEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getElementSet_Elements()
  {
    return (EReference)elementSetEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSelectExpression()
  {
    return selectExpressionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getSelectExpression_Distinct()
  {
    return (EAttribute)selectExpressionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getSelectExpression_AllVars()
  {
    return (EAttribute)selectExpressionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSelectExpression_VarList()
  {
    return (EReference)selectExpressionEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getSelectExpression_Orderby()
  {
    return (EAttribute)selectExpressionEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSelectExpression_OrderList()
  {
    return (EReference)selectExpressionEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getConstructExpression()
  {
    return constructExpressionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getConstructExpression_Subj()
  {
    return (EReference)constructExpressionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getConstructExpression_Pred()
  {
    return (EReference)constructExpressionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getConstructExpression_Obj()
  {
    return (EReference)constructExpressionEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAskQueryExpression()
  {
    return askQueryExpressionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getOrderList()
  {
    return orderListEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getOrderList_OrderList()
  {
    return (EReference)orderListEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getOrderElement()
  {
    return orderElementEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getOrderElement_Order()
  {
    return (EAttribute)orderElementEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getOrderElement_Name()
  {
    return (EReference)orderElementEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getExpression()
  {
    return expressionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExpression_Expr()
  {
    return (EReference)expressionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getExpression_Func()
  {
    return (EAttribute)expressionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExpression_Args()
  {
    return (EReference)expressionEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExpression_Gp()
  {
    return (EReference)expressionEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExpression_Ivalue()
  {
    return (EReference)expressionEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExpression_Value()
  {
    return (EReference)expressionEClass.getEStructuralFeatures().get(5);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExpression_ValueTable()
  {
    return (EReference)expressionEClass.getEStructuralFeatures().get(6);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getGraphPattern()
  {
    return graphPatternEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getPropOfSubj()
  {
    return propOfSubjEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropOfSubj_OfPhr()
  {
    return (EReference)propOfSubjEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getPropOfSubj_Subj()
  {
    return (EReference)propOfSubjEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSubjProp()
  {
    return subjPropEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSubjProp_Subj()
  {
    return (EReference)subjPropEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSubjProp_HwPhr()
  {
    return (EReference)subjPropEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getInstAttrSPV()
  {
    return instAttrSPVEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstAttrSPV_Subj()
  {
    return (EReference)instAttrSPVEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstAttrSPV_Props()
  {
    return (EReference)instAttrSPVEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstAttrSPV_Vals()
  {
    return (EReference)instAttrSPVEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getInstAttrPSV()
  {
    return instAttrPSVEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstAttrPSV_Prop()
  {
    return (EReference)instAttrPSVEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInstAttrPSV_Val()
  {
    return (EReference)instAttrPSVEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSubTypeOf()
  {
    return subTypeOfEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSubTypeOf_Subclass()
  {
    return (EReference)subTypeOfEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSubTypeOf_Superclass()
  {
    return (EReference)subTypeOfEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getIntervalValue()
  {
    return intervalValueEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getIntervalValue_Op()
  {
    return (EAttribute)intervalValueEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getIntervalValue_Expr()
  {
    return (EReference)intervalValueEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getExplicitValue()
  {
    return explicitValueEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExplicitValue_InstName()
  {
    return (EReference)explicitValueEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getExplicitValue_LitValue()
  {
    return (EReference)explicitValueEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getExplicitValue_Term()
  {
    return (EAttribute)explicitValueEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getValueTable()
  {
    return valueTableEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getValueTable_Row()
  {
    return (EReference)valueTableEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getValueTable_Rows()
  {
    return (EReference)valueTableEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getLiteralValue()
  {
    return literalValueEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getLiteralValue_LiteralNumber()
  {
    return (EAttribute)literalValueEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getLiteralValue_LiteralString()
  {
    return (EAttribute)literalValueEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getLiteralValue_LiteralBoolean()
  {
    return (EAttribute)literalValueEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getValueRow()
  {
    return valueRowEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getValueRow_ExplicitValues()
  {
    return (EReference)valueRowEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getJunctionExpression()
  {
    return junctionExpressionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getJunctionExpression_Left()
  {
    return (EReference)junctionExpressionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getJunctionExpression_Op()
  {
    return (EAttribute)junctionExpressionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getJunctionExpression_Right()
  {
    return (EReference)junctionExpressionEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getBinaryOpExpression()
  {
    return binaryOpExpressionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getBinaryOpExpression_Left()
  {
    return (EReference)binaryOpExpressionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getBinaryOpExpression_Op()
  {
    return (EAttribute)binaryOpExpressionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getBinaryOpExpression_Right()
  {
    return (EReference)binaryOpExpressionEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getUnaryOpExpression()
  {
    return unaryOpExpressionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getUnaryOpExpression_Op()
  {
    return (EAttribute)unaryOpExpressionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EEnum getDataType()
  {
    return dataTypeEEnum;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SadlFactory getSadlFactory()
  {
    return (SadlFactory)getEFactoryInstance();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private boolean isCreated = false;

  /**
   * Creates the meta-model objects for the package.  This method is
   * guarded to have no affect on any invocation but its first.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void createPackageContents()
  {
    if (isCreated) return;
    isCreated = true;

    // Create classes and their features
    modelEClass = createEClass(MODEL);
    createEReference(modelEClass, MODEL__MODEL_NAME);
    createEReference(modelEClass, MODEL__IMPORTS);
    createEReference(modelEClass, MODEL__ELEMENTS);

    modelNameEClass = createEClass(MODEL_NAME);
    createEAttribute(modelNameEClass, MODEL_NAME__BASE_URI);
    createEAttribute(modelNameEClass, MODEL_NAME__ALIAS);
    createEAttribute(modelNameEClass, MODEL_NAME__VERSION);
    createEReference(modelNameEClass, MODEL_NAME__ANN_CONTENT);

    importEClass = createEClass(IMPORT);
    createEAttribute(importEClass, IMPORT__IMPORT_URI);
    createEAttribute(importEClass, IMPORT__ALIAS);

    modelElementEClass = createEClass(MODEL_ELEMENT);

    statementEClass = createEClass(STATEMENT);

    resourceNameEClass = createEClass(RESOURCE_NAME);
    createEAttribute(resourceNameEClass, RESOURCE_NAME__NAME);
    createEAttribute(resourceNameEClass, RESOURCE_NAME__ANN_TYPE);
    createEReference(resourceNameEClass, RESOURCE_NAME__ANN_CONTENT);

    contentListEClass = createEClass(CONTENT_LIST);
    createEAttribute(contentListEClass, CONTENT_LIST__ANN_CONTENT);

    resourceListEClass = createEClass(RESOURCE_LIST);
    createEReference(resourceListEClass, RESOURCE_LIST__NAMES);

    literalListEClass = createEClass(LITERAL_LIST);
    createEReference(literalListEClass, LITERAL_LIST__LITERALS);

    resourceByNameEClass = createEClass(RESOURCE_BY_NAME);
    createEReference(resourceByNameEClass, RESOURCE_BY_NAME__NAME);

    existingResourceListEClass = createEClass(EXISTING_RESOURCE_LIST);
    createEReference(existingResourceListEClass, EXISTING_RESOURCE_LIST__NAMES);

    resourceIdentifierEClass = createEClass(RESOURCE_IDENTIFIER);

    resourceBySetOpEClass = createEClass(RESOURCE_BY_SET_OP);
    createEAttribute(resourceBySetOpEClass, RESOURCE_BY_SET_OP__ANN_TYPE);
    createEAttribute(resourceBySetOpEClass, RESOURCE_BY_SET_OP__ANN_CONTENT);
    createEReference(resourceBySetOpEClass, RESOURCE_BY_SET_OP__NAMES);
    createEAttribute(resourceBySetOpEClass, RESOURCE_BY_SET_OP__OP);

    resourceByRestrictionEClass = createEClass(RESOURCE_BY_RESTRICTION);
    createEAttribute(resourceByRestrictionEClass, RESOURCE_BY_RESTRICTION__ANN_TYPE);
    createEAttribute(resourceByRestrictionEClass, RESOURCE_BY_RESTRICTION__ANN_CONTENT);
    createEReference(resourceByRestrictionEClass, RESOURCE_BY_RESTRICTION__PROP_NAME);
    createEReference(resourceByRestrictionEClass, RESOURCE_BY_RESTRICTION__COND);

    unionResourceEClass = createEClass(UNION_RESOURCE);

    intersectionResourceEClass = createEClass(INTERSECTION_RESOURCE);

    classDeclarationEClass = createEClass(CLASS_DECLARATION);
    createEReference(classDeclarationEClass, CLASS_DECLARATION__CLASS_NAME);
    createEReference(classDeclarationEClass, CLASS_DECLARATION__MUST_BE_ONE_OF);
    createEReference(classDeclarationEClass, CLASS_DECLARATION__DESCRIBED_BY);
    createEReference(classDeclarationEClass, CLASS_DECLARATION__CLASS_LIST);
    createEReference(classDeclarationEClass, CLASS_DECLARATION__CLASS_IDENTIFIER);

    enumeratedInstancesEClass = createEClass(ENUMERATED_INSTANCES);
    createEReference(enumeratedInstancesEClass, ENUMERATED_INSTANCES__INSTANCE_LIST);

    addlClassInfoEClass = createEClass(ADDL_CLASS_INFO);
    createEReference(addlClassInfoEClass, ADDL_CLASS_INFO__PROPERTY_BY_NAME);
    createEReference(addlClassInfoEClass, ADDL_CLASS_INFO__PROPERTY_NAME);
    createEReference(addlClassInfoEClass, ADDL_CLASS_INFO__RANGE);
    createEReference(addlClassInfoEClass, ADDL_CLASS_INFO__RESTRICTION);

    rangeEClass = createEClass(RANGE);
    createEAttribute(rangeEClass, RANGE__SINGLE);
    createEReference(rangeEClass, RANGE__TYPE);

    rangeTypeEClass = createEClass(RANGE_TYPE);
    createEReference(rangeTypeEClass, RANGE_TYPE__CLASS_IDENTIFIER);
    createEAttribute(rangeTypeEClass, RANGE_TYPE__DATA_TYPE);

    equivalentConceptsEClass = createEClass(EQUIVALENT_CONCEPTS);
    createEReference(equivalentConceptsEClass, EQUIVALENT_CONCEPTS__CLASS1);
    createEReference(equivalentConceptsEClass, EQUIVALENT_CONCEPTS__CLASS2);

    disjointClassesEClass = createEClass(DISJOINT_CLASSES);
    createEReference(disjointClassesEClass, DISJOINT_CLASSES__CLASS1);
    createEReference(disjointClassesEClass, DISJOINT_CLASSES__CLASS2);
    createEReference(disjointClassesEClass, DISJOINT_CLASSES__CLASSES);

    complementOfClassEClass = createEClass(COMPLEMENT_OF_CLASS);
    createEReference(complementOfClassEClass, COMPLEMENT_OF_CLASS__CLASS1);
    createEReference(complementOfClassEClass, COMPLEMENT_OF_CLASS__CLASS2);

    allValuesFromEClass = createEClass(ALL_VALUES_FROM);
    createEReference(allValuesFromEClass, ALL_VALUES_FROM__RESTRICTED);
    createEReference(allValuesFromEClass, ALL_VALUES_FROM__COND);
    createEReference(allValuesFromEClass, ALL_VALUES_FROM__CLASS_NAME);
    createEReference(allValuesFromEClass, ALL_VALUES_FROM__PROPERTY_NAME);

    someValuesFromEClass = createEClass(SOME_VALUES_FROM);
    createEReference(someValuesFromEClass, SOME_VALUES_FROM__RESTRICTED);
    createEReference(someValuesFromEClass, SOME_VALUES_FROM__COND);
    createEReference(someValuesFromEClass, SOME_VALUES_FROM__CLASS_NAME);
    createEReference(someValuesFromEClass, SOME_VALUES_FROM__PROPERTY_NAME);

    hasValueEClass = createEClass(HAS_VALUE);
    createEReference(hasValueEClass, HAS_VALUE__RESTRICTED);
    createEReference(hasValueEClass, HAS_VALUE__COND);
    createEReference(hasValueEClass, HAS_VALUE__CLASS_NAME);
    createEReference(hasValueEClass, HAS_VALUE__PROPERTY_NAME);

    cardinalityEClass = createEClass(CARDINALITY);
    createEReference(cardinalityEClass, CARDINALITY__RESTRICTED);
    createEReference(cardinalityEClass, CARDINALITY__COND);
    createEReference(cardinalityEClass, CARDINALITY__CLASS_NAME);
    createEReference(cardinalityEClass, CARDINALITY__PROPERTY_NAME);

    minCardinalityEClass = createEClass(MIN_CARDINALITY);
    createEReference(minCardinalityEClass, MIN_CARDINALITY__RESTRICTED);
    createEReference(minCardinalityEClass, MIN_CARDINALITY__COND);
    createEReference(minCardinalityEClass, MIN_CARDINALITY__CLASS_NAME);
    createEReference(minCardinalityEClass, MIN_CARDINALITY__PROPERTY_NAME);

    maxCardinalityEClass = createEClass(MAX_CARDINALITY);
    createEReference(maxCardinalityEClass, MAX_CARDINALITY__RESTRICTED);
    createEReference(maxCardinalityEClass, MAX_CARDINALITY__COND);
    createEReference(maxCardinalityEClass, MAX_CARDINALITY__CLASS_NAME);
    createEReference(maxCardinalityEClass, MAX_CARDINALITY__PROPERTY_NAME);

    propertyOfClassEClass = createEClass(PROPERTY_OF_CLASS);
    createEReference(propertyOfClassEClass, PROPERTY_OF_CLASS__PROPERTY_NAME);
    createEReference(propertyOfClassEClass, PROPERTY_OF_CLASS__CLASS_NAME);

    allValuesConditionEClass = createEClass(ALL_VALUES_CONDITION);
    createEReference(allValuesConditionEClass, ALL_VALUES_CONDITION__RESTRICTION);

    enumeratedAllValuesFromEClass = createEClass(ENUMERATED_ALL_VALUES_FROM);
    createEReference(enumeratedAllValuesFromEClass, ENUMERATED_ALL_VALUES_FROM__RESTRICTED);
    createEReference(enumeratedAllValuesFromEClass, ENUMERATED_ALL_VALUES_FROM__ENUMERATION);

    enumeratedAllAndSomeValuesFromEClass = createEClass(ENUMERATED_ALL_AND_SOME_VALUES_FROM);
    createEReference(enumeratedAllAndSomeValuesFromEClass, ENUMERATED_ALL_AND_SOME_VALUES_FROM__RESTRICTED);
    createEReference(enumeratedAllAndSomeValuesFromEClass, ENUMERATED_ALL_AND_SOME_VALUES_FROM__ENUMERATION);

    defaultValueEClass = createEClass(DEFAULT_VALUE);
    createEReference(defaultValueEClass, DEFAULT_VALUE__DEF_VALUE_CLASS);
    createEAttribute(defaultValueEClass, DEFAULT_VALUE__LEVEL);
    createEReference(defaultValueEClass, DEFAULT_VALUE__DEF_VALUE);

    someValuesConditionEClass = createEClass(SOME_VALUES_CONDITION);
    createEReference(someValuesConditionEClass, SOME_VALUES_CONDITION__RESTRICTION);

    hasValueConditionEClass = createEClass(HAS_VALUE_CONDITION);
    createEReference(hasValueConditionEClass, HAS_VALUE_CONDITION__RESTRICTION);

    minCardConditionEClass = createEClass(MIN_CARD_CONDITION);
    createEAttribute(minCardConditionEClass, MIN_CARD_CONDITION__CARD);
    createEReference(minCardConditionEClass, MIN_CARD_CONDITION__CLASS_QUALIFIER);

    maxCardConditionEClass = createEClass(MAX_CARD_CONDITION);
    createEAttribute(maxCardConditionEClass, MAX_CARD_CONDITION__CARD);
    createEReference(maxCardConditionEClass, MAX_CARD_CONDITION__CLASS_QUALIFIER);

    cardConditionEClass = createEClass(CARD_CONDITION);
    createEAttribute(cardConditionEClass, CARD_CONDITION__CARD);
    createEReference(cardConditionEClass, CARD_CONDITION__CLASS_QUALIFIER);

    necessaryAndSufficientEClass = createEClass(NECESSARY_AND_SUFFICIENT);
    createEReference(necessaryAndSufficientEClass, NECESSARY_AND_SUFFICIENT__SUPER_CLASS);
    createEAttribute(necessaryAndSufficientEClass, NECESSARY_AND_SUFFICIENT__ARTICLE);
    createEReference(necessaryAndSufficientEClass, NECESSARY_AND_SUFFICIENT__SUB_CLASS);
    createEReference(necessaryAndSufficientEClass, NECESSARY_AND_SUFFICIENT__PROPERTY_NAME);
    createEReference(necessaryAndSufficientEClass, NECESSARY_AND_SUFFICIENT__COND);

    conditionEClass = createEClass(CONDITION);

    propertyDeclarationEClass = createEClass(PROPERTY_DECLARATION);
    createEReference(propertyDeclarationEClass, PROPERTY_DECLARATION__PROPERTY_NAME);
    createEReference(propertyDeclarationEClass, PROPERTY_DECLARATION__SUPER_PROP_NAME);
    createEReference(propertyDeclarationEClass, PROPERTY_DECLARATION__ADDL_PROP_INFO);
    createEAttribute(propertyDeclarationEClass, PROPERTY_DECLARATION__ARTICLE);
    createEReference(propertyDeclarationEClass, PROPERTY_DECLARATION__DOMAIN);
    createEReference(propertyDeclarationEClass, PROPERTY_DECLARATION__RANGE_RESOURCE);
    createEReference(propertyDeclarationEClass, PROPERTY_DECLARATION__ANNOTATION_PROPERTY);

    additionalPropertyInfoEClass = createEClass(ADDITIONAL_PROPERTY_INFO);
    createEReference(additionalPropertyInfoEClass, ADDITIONAL_PROPERTY_INFO__DOMAIN);
    createEReference(additionalPropertyInfoEClass, ADDITIONAL_PROPERTY_INFO__COND);
    createEReference(additionalPropertyInfoEClass, ADDITIONAL_PROPERTY_INFO__RANGE);
    createEAttribute(additionalPropertyInfoEClass, ADDITIONAL_PROPERTY_INFO__ISFUNC);
    createEAttribute(additionalPropertyInfoEClass, ADDITIONAL_PROPERTY_INFO__ISINVFUNC);
    createEAttribute(additionalPropertyInfoEClass, ADDITIONAL_PROPERTY_INFO__IS_SYM);
    createEAttribute(additionalPropertyInfoEClass, ADDITIONAL_PROPERTY_INFO__IS_TRANS);
    createEReference(additionalPropertyInfoEClass, ADDITIONAL_PROPERTY_INFO__IS_INV_OF);

    functionalPropertyEClass = createEClass(FUNCTIONAL_PROPERTY);
    createEReference(functionalPropertyEClass, FUNCTIONAL_PROPERTY__PROPERTY_NAME);

    inverseFunctionalPropertyEClass = createEClass(INVERSE_FUNCTIONAL_PROPERTY);
    createEReference(inverseFunctionalPropertyEClass, INVERSE_FUNCTIONAL_PROPERTY__PROPERTY_NAME);

    symmetricalPropertyEClass = createEClass(SYMMETRICAL_PROPERTY);
    createEReference(symmetricalPropertyEClass, SYMMETRICAL_PROPERTY__PROPERTY_NAME);

    transitivePropertyEClass = createEClass(TRANSITIVE_PROPERTY);
    createEReference(transitivePropertyEClass, TRANSITIVE_PROPERTY__PROPERTY_NAME);

    inversePropertyEClass = createEClass(INVERSE_PROPERTY);
    createEReference(inversePropertyEClass, INVERSE_PROPERTY__PROPERTY_NAME1);
    createEReference(inversePropertyEClass, INVERSE_PROPERTY__INV_OF);

    isInverseOfEClass = createEClass(IS_INVERSE_OF);
    createEReference(isInverseOfEClass, IS_INVERSE_OF__PROPERTY_NAME2);

    typedBNodeEClass = createEClass(TYPED_BNODE);
    createEAttribute(typedBNodeEClass, TYPED_BNODE__ARTICLE);
    createEReference(typedBNodeEClass, TYPED_BNODE__CLASS_IDENTIFIER);

    instanceDeclarationStatementEClass = createEClass(INSTANCE_DECLARATION_STATEMENT);

    instanceDeclarationEClass = createEClass(INSTANCE_DECLARATION);
    createEReference(instanceDeclarationEClass, INSTANCE_DECLARATION__TYPE_DECL);
    createEReference(instanceDeclarationEClass, INSTANCE_DECLARATION__ADDL_INFO_ITEMS);
    createEAttribute(instanceDeclarationEClass, INSTANCE_DECLARATION__ARTICLE);
    createEReference(instanceDeclarationEClass, INSTANCE_DECLARATION__CLASS_NAME);
    createEReference(instanceDeclarationEClass, INSTANCE_DECLARATION__INSTANCE_NAME);

    typeDeclarationEClass = createEClass(TYPE_DECLARATION);
    createEReference(typeDeclarationEClass, TYPE_DECLARATION__INST_NAME);
    createEReference(typeDeclarationEClass, TYPE_DECLARATION__TYPE);

    instanceDifferentFromEClass = createEClass(INSTANCE_DIFFERENT_FROM);
    createEReference(instanceDifferentFromEClass, INSTANCE_DIFFERENT_FROM__INST_NAME1);
    createEReference(instanceDifferentFromEClass, INSTANCE_DIFFERENT_FROM__INST_NAME2);

    instancesAllDifferentEClass = createEClass(INSTANCES_ALL_DIFFERENT);
    createEReference(instancesAllDifferentEClass, INSTANCES_ALL_DIFFERENT__INSTANCES);

    existingInstanceAttributionEClass = createEClass(EXISTING_INSTANCE_ATTRIBUTION);
    createEReference(existingInstanceAttributionEClass, EXISTING_INSTANCE_ATTRIBUTION__SUBJ);
    createEReference(existingInstanceAttributionEClass, EXISTING_INSTANCE_ATTRIBUTION__ADDL_INFO_ITEMS);
    createEReference(existingInstanceAttributionEClass, EXISTING_INSTANCE_ATTRIBUTION__POF_S);
    createEReference(existingInstanceAttributionEClass, EXISTING_INSTANCE_ATTRIBUTION__OBJ);

    objectEClass = createEClass(OBJECT);
    createEReference(objectEClass, OBJECT__VAL);

    propValPartialTripleEClass = createEClass(PROP_VAL_PARTIAL_TRIPLE);
    createEReference(propValPartialTripleEClass, PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME);
    createEReference(propValPartialTripleEClass, PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE);
    createEReference(propValPartialTripleEClass, PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE);

    ofPatternReturningValuesEClass = createEClass(OF_PATTERN_RETURNING_VALUES);
    createEReference(ofPatternReturningValuesEClass, OF_PATTERN_RETURNING_VALUES__OFPHRS);
    createEReference(ofPatternReturningValuesEClass, OF_PATTERN_RETURNING_VALUES__SUBJECT);
    createEReference(ofPatternReturningValuesEClass, OF_PATTERN_RETURNING_VALUES__TYPE);

    withChainEClass = createEClass(WITH_CHAIN);
    createEReference(withChainEClass, WITH_CHAIN__WPS);

    withPhraseEClass = createEClass(WITH_PHRASE);
    createEReference(withPhraseEClass, WITH_PHRASE__PROPERTY_NAME);
    createEReference(withPhraseEClass, WITH_PHRASE__VALUE);

    embeddedInstanceDeclarationEClass = createEClass(EMBEDDED_INSTANCE_DECLARATION);

    mergedTriplesEClass = createEClass(MERGED_TRIPLES);
    createEReference(mergedTriplesEClass, MERGED_TRIPLES__OPS);
    createEReference(mergedTriplesEClass, MERGED_TRIPLES__PIVOT);
    createEReference(mergedTriplesEClass, MERGED_TRIPLES__WCS);

    ofPhraseEClass = createEClass(OF_PHRASE);
    createEAttribute(ofPhraseEClass, OF_PHRASE__ARTICLE);
    createEReference(ofPhraseEClass, OF_PHRASE__PROPERTY_NAME);

    variableListEClass = createEClass(VARIABLE_LIST);
    createEReference(variableListEClass, VARIABLE_LIST__NAMES);

    ruleEClass = createEClass(RULE);
    createEAttribute(ruleEClass, RULE__NAME);
    createEReference(ruleEClass, RULE__GIVENS);
    createEReference(ruleEClass, RULE__IFS);
    createEReference(ruleEClass, RULE__THENS);

    queryEClass = createEClass(QUERY);
    createEReference(queryEClass, QUERY__EXPR);

    testEClass = createEClass(TEST);
    createEReference(testEClass, TEST__EXPR);

    exprEClass = createEClass(EXPR);
    createEReference(exprEClass, EXPR__EXPR);

    displayEClass = createEClass(DISPLAY);
    createEAttribute(displayEClass, DISPLAY__DISPLAY_STRING);
    createEAttribute(displayEClass, DISPLAY__MODEL);

    explanationEClass = createEClass(EXPLANATION);
    createEReference(explanationEClass, EXPLANATION__EXPR);
    createEAttribute(explanationEClass, EXPLANATION__RULENAME);

    elementSetEClass = createEClass(ELEMENT_SET);
    createEReference(elementSetEClass, ELEMENT_SET__ELEMENTS);

    selectExpressionEClass = createEClass(SELECT_EXPRESSION);
    createEAttribute(selectExpressionEClass, SELECT_EXPRESSION__DISTINCT);
    createEAttribute(selectExpressionEClass, SELECT_EXPRESSION__ALL_VARS);
    createEReference(selectExpressionEClass, SELECT_EXPRESSION__VAR_LIST);
    createEAttribute(selectExpressionEClass, SELECT_EXPRESSION__ORDERBY);
    createEReference(selectExpressionEClass, SELECT_EXPRESSION__ORDER_LIST);

    constructExpressionEClass = createEClass(CONSTRUCT_EXPRESSION);
    createEReference(constructExpressionEClass, CONSTRUCT_EXPRESSION__SUBJ);
    createEReference(constructExpressionEClass, CONSTRUCT_EXPRESSION__PRED);
    createEReference(constructExpressionEClass, CONSTRUCT_EXPRESSION__OBJ);

    askQueryExpressionEClass = createEClass(ASK_QUERY_EXPRESSION);

    orderListEClass = createEClass(ORDER_LIST);
    createEReference(orderListEClass, ORDER_LIST__ORDER_LIST);

    orderElementEClass = createEClass(ORDER_ELEMENT);
    createEAttribute(orderElementEClass, ORDER_ELEMENT__ORDER);
    createEReference(orderElementEClass, ORDER_ELEMENT__NAME);

    expressionEClass = createEClass(EXPRESSION);
    createEReference(expressionEClass, EXPRESSION__EXPR);
    createEAttribute(expressionEClass, EXPRESSION__FUNC);
    createEReference(expressionEClass, EXPRESSION__ARGS);
    createEReference(expressionEClass, EXPRESSION__GP);
    createEReference(expressionEClass, EXPRESSION__IVALUE);
    createEReference(expressionEClass, EXPRESSION__VALUE);
    createEReference(expressionEClass, EXPRESSION__VALUE_TABLE);

    graphPatternEClass = createEClass(GRAPH_PATTERN);

    propOfSubjEClass = createEClass(PROP_OF_SUBJ);
    createEReference(propOfSubjEClass, PROP_OF_SUBJ__OF_PHR);
    createEReference(propOfSubjEClass, PROP_OF_SUBJ__SUBJ);

    subjPropEClass = createEClass(SUBJ_PROP);
    createEReference(subjPropEClass, SUBJ_PROP__SUBJ);
    createEReference(subjPropEClass, SUBJ_PROP__HW_PHR);

    instAttrSPVEClass = createEClass(INST_ATTR_SPV);
    createEReference(instAttrSPVEClass, INST_ATTR_SPV__SUBJ);
    createEReference(instAttrSPVEClass, INST_ATTR_SPV__PROPS);
    createEReference(instAttrSPVEClass, INST_ATTR_SPV__VALS);

    instAttrPSVEClass = createEClass(INST_ATTR_PSV);
    createEReference(instAttrPSVEClass, INST_ATTR_PSV__PROP);
    createEReference(instAttrPSVEClass, INST_ATTR_PSV__VAL);

    subTypeOfEClass = createEClass(SUB_TYPE_OF);
    createEReference(subTypeOfEClass, SUB_TYPE_OF__SUBCLASS);
    createEReference(subTypeOfEClass, SUB_TYPE_OF__SUPERCLASS);

    intervalValueEClass = createEClass(INTERVAL_VALUE);
    createEAttribute(intervalValueEClass, INTERVAL_VALUE__OP);
    createEReference(intervalValueEClass, INTERVAL_VALUE__EXPR);

    explicitValueEClass = createEClass(EXPLICIT_VALUE);
    createEReference(explicitValueEClass, EXPLICIT_VALUE__INST_NAME);
    createEReference(explicitValueEClass, EXPLICIT_VALUE__LIT_VALUE);
    createEAttribute(explicitValueEClass, EXPLICIT_VALUE__TERM);

    valueTableEClass = createEClass(VALUE_TABLE);
    createEReference(valueTableEClass, VALUE_TABLE__ROW);
    createEReference(valueTableEClass, VALUE_TABLE__ROWS);

    literalValueEClass = createEClass(LITERAL_VALUE);
    createEAttribute(literalValueEClass, LITERAL_VALUE__LITERAL_NUMBER);
    createEAttribute(literalValueEClass, LITERAL_VALUE__LITERAL_STRING);
    createEAttribute(literalValueEClass, LITERAL_VALUE__LITERAL_BOOLEAN);

    valueRowEClass = createEClass(VALUE_ROW);
    createEReference(valueRowEClass, VALUE_ROW__EXPLICIT_VALUES);

    junctionExpressionEClass = createEClass(JUNCTION_EXPRESSION);
    createEReference(junctionExpressionEClass, JUNCTION_EXPRESSION__LEFT);
    createEAttribute(junctionExpressionEClass, JUNCTION_EXPRESSION__OP);
    createEReference(junctionExpressionEClass, JUNCTION_EXPRESSION__RIGHT);

    binaryOpExpressionEClass = createEClass(BINARY_OP_EXPRESSION);
    createEReference(binaryOpExpressionEClass, BINARY_OP_EXPRESSION__LEFT);
    createEAttribute(binaryOpExpressionEClass, BINARY_OP_EXPRESSION__OP);
    createEReference(binaryOpExpressionEClass, BINARY_OP_EXPRESSION__RIGHT);

    unaryOpExpressionEClass = createEClass(UNARY_OP_EXPRESSION);
    createEAttribute(unaryOpExpressionEClass, UNARY_OP_EXPRESSION__OP);

    // Create enums
    dataTypeEEnum = createEEnum(DATA_TYPE);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private boolean isInitialized = false;

  /**
   * Complete the initialization of the package and its meta-model.  This
   * method is guarded to have no affect on any invocation but its first.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void initializePackageContents()
  {
    if (isInitialized) return;
    isInitialized = true;

    // Initialize package
    setName(eNAME);
    setNsPrefix(eNS_PREFIX);
    setNsURI(eNS_URI);

    // Create type parameters

    // Set bounds for type parameters

    // Add supertypes to classes
    statementEClass.getESuperTypes().add(this.getModelElement());
    resourceByNameEClass.getESuperTypes().add(this.getResourceIdentifier());
    resourceBySetOpEClass.getESuperTypes().add(this.getResourceIdentifier());
    resourceByRestrictionEClass.getESuperTypes().add(this.getResourceIdentifier());
    unionResourceEClass.getESuperTypes().add(this.getResourceBySetOp());
    intersectionResourceEClass.getESuperTypes().add(this.getResourceBySetOp());
    classDeclarationEClass.getESuperTypes().add(this.getStatement());
    equivalentConceptsEClass.getESuperTypes().add(this.getStatement());
    disjointClassesEClass.getESuperTypes().add(this.getStatement());
    complementOfClassEClass.getESuperTypes().add(this.getStatement());
    allValuesFromEClass.getESuperTypes().add(this.getStatement());
    someValuesFromEClass.getESuperTypes().add(this.getStatement());
    hasValueEClass.getESuperTypes().add(this.getStatement());
    cardinalityEClass.getESuperTypes().add(this.getStatement());
    minCardinalityEClass.getESuperTypes().add(this.getStatement());
    maxCardinalityEClass.getESuperTypes().add(this.getStatement());
    allValuesConditionEClass.getESuperTypes().add(this.getCondition());
    enumeratedAllValuesFromEClass.getESuperTypes().add(this.getStatement());
    enumeratedAllAndSomeValuesFromEClass.getESuperTypes().add(this.getStatement());
    defaultValueEClass.getESuperTypes().add(this.getStatement());
    someValuesConditionEClass.getESuperTypes().add(this.getCondition());
    hasValueConditionEClass.getESuperTypes().add(this.getCondition());
    minCardConditionEClass.getESuperTypes().add(this.getCondition());
    maxCardConditionEClass.getESuperTypes().add(this.getCondition());
    cardConditionEClass.getESuperTypes().add(this.getCondition());
    necessaryAndSufficientEClass.getESuperTypes().add(this.getStatement());
    propertyDeclarationEClass.getESuperTypes().add(this.getStatement());
    functionalPropertyEClass.getESuperTypes().add(this.getStatement());
    inverseFunctionalPropertyEClass.getESuperTypes().add(this.getStatement());
    symmetricalPropertyEClass.getESuperTypes().add(this.getStatement());
    transitivePropertyEClass.getESuperTypes().add(this.getStatement());
    inversePropertyEClass.getESuperTypes().add(this.getStatement());
    instanceDeclarationStatementEClass.getESuperTypes().add(this.getStatement());
    instanceDeclarationEClass.getESuperTypes().add(this.getInstanceDeclarationStatement());
    instanceDeclarationEClass.getESuperTypes().add(this.getEmbeddedInstanceDeclaration());
    instanceDifferentFromEClass.getESuperTypes().add(this.getStatement());
    instancesAllDifferentEClass.getESuperTypes().add(this.getStatement());
    existingInstanceAttributionEClass.getESuperTypes().add(this.getStatement());
    mergedTriplesEClass.getESuperTypes().add(this.getGraphPattern());
    ruleEClass.getESuperTypes().add(this.getModelElement());
    queryEClass.getESuperTypes().add(this.getModelElement());
    testEClass.getESuperTypes().add(this.getModelElement());
    exprEClass.getESuperTypes().add(this.getModelElement());
    displayEClass.getESuperTypes().add(this.getModelElement());
    explanationEClass.getESuperTypes().add(this.getModelElement());
    selectExpressionEClass.getESuperTypes().add(this.getExpression());
    constructExpressionEClass.getESuperTypes().add(this.getExpression());
    askQueryExpressionEClass.getESuperTypes().add(this.getExpression());
    propOfSubjEClass.getESuperTypes().add(this.getGraphPattern());
    subjPropEClass.getESuperTypes().add(this.getGraphPattern());
    instAttrSPVEClass.getESuperTypes().add(this.getGraphPattern());
    instAttrPSVEClass.getESuperTypes().add(this.getGraphPattern());
    subTypeOfEClass.getESuperTypes().add(this.getGraphPattern());
    junctionExpressionEClass.getESuperTypes().add(this.getExpression());
    binaryOpExpressionEClass.getESuperTypes().add(this.getExpression());
    unaryOpExpressionEClass.getESuperTypes().add(this.getExpression());

    // Initialize classes and features; add operations and parameters
    initEClass(modelEClass, Model.class, "Model", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getModel_ModelName(), this.getModelName(), null, "modelName", null, 0, 1, Model.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getModel_Imports(), this.getImport(), null, "imports", null, 0, -1, Model.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getModel_Elements(), this.getModelElement(), null, "elements", null, 0, -1, Model.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(modelNameEClass, ModelName.class, "ModelName", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getModelName_BaseUri(), ecorePackage.getEString(), "baseUri", null, 0, 1, ModelName.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getModelName_Alias(), ecorePackage.getEString(), "alias", null, 0, 1, ModelName.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getModelName_Version(), ecorePackage.getEString(), "version", null, 0, 1, ModelName.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getModelName_AnnContent(), this.getContentList(), null, "annContent", null, 0, -1, ModelName.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(importEClass, Import.class, "Import", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getImport_ImportURI(), ecorePackage.getEString(), "importURI", null, 0, 1, Import.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getImport_Alias(), ecorePackage.getEString(), "alias", null, 0, 1, Import.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(modelElementEClass, ModelElement.class, "ModelElement", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(statementEClass, Statement.class, "Statement", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(resourceNameEClass, ResourceName.class, "ResourceName", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getResourceName_Name(), ecorePackage.getEString(), "name", null, 0, 1, ResourceName.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getResourceName_AnnType(), ecorePackage.getEString(), "annType", null, 0, -1, ResourceName.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getResourceName_AnnContent(), this.getContentList(), null, "annContent", null, 0, -1, ResourceName.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(contentListEClass, ContentList.class, "ContentList", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getContentList_AnnContent(), ecorePackage.getEString(), "annContent", null, 0, -1, ContentList.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(resourceListEClass, ResourceList.class, "ResourceList", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getResourceList_Names(), this.getResourceName(), null, "names", null, 0, -1, ResourceList.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(literalListEClass, LiteralList.class, "LiteralList", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getLiteralList_Literals(), this.getLiteralValue(), null, "literals", null, 0, -1, LiteralList.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(resourceByNameEClass, ResourceByName.class, "ResourceByName", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getResourceByName_Name(), this.getResourceName(), null, "name", null, 0, 1, ResourceByName.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(existingResourceListEClass, ExistingResourceList.class, "ExistingResourceList", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getExistingResourceList_Names(), this.getResourceIdentifier(), null, "names", null, 0, -1, ExistingResourceList.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(resourceIdentifierEClass, ResourceIdentifier.class, "ResourceIdentifier", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(resourceBySetOpEClass, ResourceBySetOp.class, "ResourceBySetOp", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getResourceBySetOp_AnnType(), ecorePackage.getEString(), "annType", null, 0, 1, ResourceBySetOp.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getResourceBySetOp_AnnContent(), ecorePackage.getEString(), "annContent", null, 0, 1, ResourceBySetOp.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getResourceBySetOp_Names(), this.getResourceIdentifier(), null, "names", null, 0, -1, ResourceBySetOp.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getResourceBySetOp_Op(), ecorePackage.getEString(), "op", null, 0, -1, ResourceBySetOp.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(resourceByRestrictionEClass, ResourceByRestriction.class, "ResourceByRestriction", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getResourceByRestriction_AnnType(), ecorePackage.getEString(), "annType", null, 0, 1, ResourceByRestriction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getResourceByRestriction_AnnContent(), ecorePackage.getEString(), "annContent", null, 0, 1, ResourceByRestriction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getResourceByRestriction_PropName(), this.getResourceByName(), null, "propName", null, 0, 1, ResourceByRestriction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getResourceByRestriction_Cond(), this.getCondition(), null, "cond", null, 0, 1, ResourceByRestriction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(unionResourceEClass, UnionResource.class, "UnionResource", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(intersectionResourceEClass, IntersectionResource.class, "IntersectionResource", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(classDeclarationEClass, ClassDeclaration.class, "ClassDeclaration", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getClassDeclaration_ClassName(), this.getResourceName(), null, "className", null, 0, 1, ClassDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getClassDeclaration_MustBeOneOf(), this.getEnumeratedInstances(), null, "mustBeOneOf", null, 0, 1, ClassDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getClassDeclaration_DescribedBy(), this.getAddlClassInfo(), null, "describedBy", null, 0, -1, ClassDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getClassDeclaration_ClassList(), this.getResourceList(), null, "classList", null, 0, 1, ClassDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getClassDeclaration_ClassIdentifier(), this.getResourceIdentifier(), null, "classIdentifier", null, 0, 1, ClassDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(enumeratedInstancesEClass, EnumeratedInstances.class, "EnumeratedInstances", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getEnumeratedInstances_InstanceList(), this.getResourceList(), null, "instanceList", null, 0, 1, EnumeratedInstances.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(addlClassInfoEClass, AddlClassInfo.class, "AddlClassInfo", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getAddlClassInfo_PropertyByName(), this.getResourceByName(), null, "propertyByName", null, 0, 1, AddlClassInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAddlClassInfo_PropertyName(), this.getResourceName(), null, "propertyName", null, 0, 1, AddlClassInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAddlClassInfo_Range(), this.getRange(), null, "range", null, 0, 1, AddlClassInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAddlClassInfo_Restriction(), this.getCondition(), null, "restriction", null, 0, 1, AddlClassInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(rangeEClass, Range.class, "Range", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getRange_Single(), ecorePackage.getEString(), "single", null, 0, 1, Range.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getRange_Type(), this.getRangeType(), null, "type", null, 0, 1, Range.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(rangeTypeEClass, RangeType.class, "RangeType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getRangeType_ClassIdentifier(), this.getResourceIdentifier(), null, "classIdentifier", null, 0, 1, RangeType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRangeType_DataType(), this.getDataType(), "dataType", null, 0, 1, RangeType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(equivalentConceptsEClass, EquivalentConcepts.class, "EquivalentConcepts", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getEquivalentConcepts_Class1(), this.getResourceByName(), null, "class1", null, 0, 1, EquivalentConcepts.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getEquivalentConcepts_Class2(), this.getResourceIdentifier(), null, "class2", null, 0, 1, EquivalentConcepts.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(disjointClassesEClass, DisjointClasses.class, "DisjointClasses", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getDisjointClasses_Class1(), this.getResourceByName(), null, "class1", null, 0, 1, DisjointClasses.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getDisjointClasses_Class2(), this.getResourceIdentifier(), null, "class2", null, 0, 1, DisjointClasses.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getDisjointClasses_Classes(), this.getExistingResourceList(), null, "classes", null, 0, 1, DisjointClasses.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(complementOfClassEClass, ComplementOfClass.class, "ComplementOfClass", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getComplementOfClass_Class1(), this.getResourceByName(), null, "class1", null, 0, 1, ComplementOfClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getComplementOfClass_Class2(), this.getResourceIdentifier(), null, "class2", null, 0, 1, ComplementOfClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(allValuesFromEClass, AllValuesFrom.class, "AllValuesFrom", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getAllValuesFrom_Restricted(), this.getPropertyOfClass(), null, "restricted", null, 0, 1, AllValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAllValuesFrom_Cond(), this.getAllValuesCondition(), null, "cond", null, 0, 1, AllValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAllValuesFrom_ClassName(), this.getResourceIdentifier(), null, "className", null, 0, 1, AllValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAllValuesFrom_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, AllValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(someValuesFromEClass, SomeValuesFrom.class, "SomeValuesFrom", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSomeValuesFrom_Restricted(), this.getPropertyOfClass(), null, "restricted", null, 0, 1, SomeValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSomeValuesFrom_Cond(), this.getSomeValuesCondition(), null, "cond", null, 0, 1, SomeValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSomeValuesFrom_ClassName(), this.getResourceIdentifier(), null, "className", null, 0, 1, SomeValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSomeValuesFrom_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, SomeValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(hasValueEClass, HasValue.class, "HasValue", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getHasValue_Restricted(), this.getPropertyOfClass(), null, "restricted", null, 0, 1, HasValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getHasValue_Cond(), this.getHasValueCondition(), null, "cond", null, 0, 1, HasValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getHasValue_ClassName(), this.getResourceIdentifier(), null, "className", null, 0, 1, HasValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getHasValue_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, HasValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(cardinalityEClass, Cardinality.class, "Cardinality", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getCardinality_Restricted(), this.getPropertyOfClass(), null, "restricted", null, 0, 1, Cardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getCardinality_Cond(), this.getCardCondition(), null, "cond", null, 0, 1, Cardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getCardinality_ClassName(), this.getResourceIdentifier(), null, "className", null, 0, 1, Cardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getCardinality_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, Cardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(minCardinalityEClass, MinCardinality.class, "MinCardinality", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getMinCardinality_Restricted(), this.getPropertyOfClass(), null, "restricted", null, 0, 1, MinCardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getMinCardinality_Cond(), this.getMinCardCondition(), null, "cond", null, 0, 1, MinCardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getMinCardinality_ClassName(), this.getResourceIdentifier(), null, "className", null, 0, 1, MinCardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getMinCardinality_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, MinCardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(maxCardinalityEClass, MaxCardinality.class, "MaxCardinality", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getMaxCardinality_Restricted(), this.getPropertyOfClass(), null, "restricted", null, 0, 1, MaxCardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getMaxCardinality_Cond(), this.getMaxCardCondition(), null, "cond", null, 0, 1, MaxCardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getMaxCardinality_ClassName(), this.getResourceIdentifier(), null, "className", null, 0, 1, MaxCardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getMaxCardinality_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, MaxCardinality.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(propertyOfClassEClass, PropertyOfClass.class, "PropertyOfClass", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getPropertyOfClass_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, PropertyOfClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getPropertyOfClass_ClassName(), this.getResourceIdentifier(), null, "className", null, 0, 1, PropertyOfClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(allValuesConditionEClass, AllValuesCondition.class, "AllValuesCondition", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getAllValuesCondition_Restriction(), this.getResourceIdentifier(), null, "restriction", null, 0, 1, AllValuesCondition.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(enumeratedAllValuesFromEClass, EnumeratedAllValuesFrom.class, "EnumeratedAllValuesFrom", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getEnumeratedAllValuesFrom_Restricted(), this.getPropertyOfClass(), null, "restricted", null, 0, 1, EnumeratedAllValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getEnumeratedAllValuesFrom_Enumeration(), ecorePackage.getEObject(), null, "enumeration", null, 0, 1, EnumeratedAllValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(enumeratedAllAndSomeValuesFromEClass, EnumeratedAllAndSomeValuesFrom.class, "EnumeratedAllAndSomeValuesFrom", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getEnumeratedAllAndSomeValuesFrom_Restricted(), this.getPropertyOfClass(), null, "restricted", null, 0, 1, EnumeratedAllAndSomeValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getEnumeratedAllAndSomeValuesFrom_Enumeration(), ecorePackage.getEObject(), null, "enumeration", null, 0, 1, EnumeratedAllAndSomeValuesFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(defaultValueEClass, DefaultValue.class, "DefaultValue", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getDefaultValue_DefValueClass(), this.getPropertyOfClass(), null, "defValueClass", null, 0, 1, DefaultValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getDefaultValue_Level(), ecorePackage.getEString(), "level", null, 0, 1, DefaultValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getDefaultValue_DefValue(), this.getExplicitValue(), null, "defValue", null, 0, 1, DefaultValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(someValuesConditionEClass, SomeValuesCondition.class, "SomeValuesCondition", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSomeValuesCondition_Restriction(), ecorePackage.getEObject(), null, "restriction", null, 0, 1, SomeValuesCondition.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(hasValueConditionEClass, HasValueCondition.class, "HasValueCondition", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getHasValueCondition_Restriction(), this.getExplicitValue(), null, "restriction", null, 0, 1, HasValueCondition.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(minCardConditionEClass, MinCardCondition.class, "MinCardCondition", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getMinCardCondition_Card(), ecorePackage.getEString(), "card", null, 0, 1, MinCardCondition.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getMinCardCondition_ClassQualifier(), this.getResourceIdentifier(), null, "classQualifier", null, 0, 1, MinCardCondition.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(maxCardConditionEClass, MaxCardCondition.class, "MaxCardCondition", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getMaxCardCondition_Card(), ecorePackage.getEString(), "card", null, 0, 1, MaxCardCondition.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getMaxCardCondition_ClassQualifier(), this.getResourceIdentifier(), null, "classQualifier", null, 0, 1, MaxCardCondition.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(cardConditionEClass, CardCondition.class, "CardCondition", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getCardCondition_Card(), ecorePackage.getEString(), "card", null, 0, 1, CardCondition.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getCardCondition_ClassQualifier(), this.getResourceIdentifier(), null, "classQualifier", null, 0, 1, CardCondition.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(necessaryAndSufficientEClass, NecessaryAndSufficient.class, "NecessaryAndSufficient", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getNecessaryAndSufficient_SuperClass(), this.getTypedBNode(), null, "superClass", null, 0, 1, NecessaryAndSufficient.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getNecessaryAndSufficient_Article(), ecorePackage.getEString(), "article", null, 0, 1, NecessaryAndSufficient.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getNecessaryAndSufficient_SubClass(), this.getResourceName(), null, "subClass", null, 0, 1, NecessaryAndSufficient.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getNecessaryAndSufficient_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, -1, NecessaryAndSufficient.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getNecessaryAndSufficient_Cond(), this.getCondition(), null, "cond", null, 0, -1, NecessaryAndSufficient.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(conditionEClass, Condition.class, "Condition", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(propertyDeclarationEClass, PropertyDeclaration.class, "PropertyDeclaration", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getPropertyDeclaration_PropertyName(), this.getResourceName(), null, "propertyName", null, 0, 1, PropertyDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getPropertyDeclaration_SuperPropName(), this.getResourceByName(), null, "superPropName", null, 0, 1, PropertyDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getPropertyDeclaration_AddlPropInfo(), this.getAdditionalPropertyInfo(), null, "addlPropInfo", null, 0, -1, PropertyDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getPropertyDeclaration_Article(), ecorePackage.getEString(), "article", null, 0, 1, PropertyDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getPropertyDeclaration_Domain(), this.getResourceIdentifier(), null, "domain", null, 0, 1, PropertyDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getPropertyDeclaration_RangeResource(), this.getResourceIdentifier(), null, "rangeResource", null, 0, 1, PropertyDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getPropertyDeclaration_AnnotationProperty(), this.getResourceName(), null, "annotationProperty", null, 0, 1, PropertyDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(additionalPropertyInfoEClass, AdditionalPropertyInfo.class, "AdditionalPropertyInfo", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getAdditionalPropertyInfo_Domain(), this.getResourceIdentifier(), null, "domain", null, 0, 1, AdditionalPropertyInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAdditionalPropertyInfo_Cond(), this.getCondition(), null, "cond", null, 0, 1, AdditionalPropertyInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAdditionalPropertyInfo_Range(), this.getRange(), null, "range", null, 0, 1, AdditionalPropertyInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getAdditionalPropertyInfo_Isfunc(), ecorePackage.getEString(), "isfunc", null, 0, 1, AdditionalPropertyInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getAdditionalPropertyInfo_Isinvfunc(), ecorePackage.getEString(), "isinvfunc", null, 0, 1, AdditionalPropertyInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getAdditionalPropertyInfo_IsSym(), ecorePackage.getEString(), "isSym", null, 0, 1, AdditionalPropertyInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getAdditionalPropertyInfo_IsTrans(), ecorePackage.getEString(), "isTrans", null, 0, 1, AdditionalPropertyInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAdditionalPropertyInfo_IsInvOf(), this.getIsInverseOf(), null, "isInvOf", null, 0, 1, AdditionalPropertyInfo.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(functionalPropertyEClass, FunctionalProperty.class, "FunctionalProperty", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getFunctionalProperty_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, FunctionalProperty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(inverseFunctionalPropertyEClass, InverseFunctionalProperty.class, "InverseFunctionalProperty", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getInverseFunctionalProperty_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, InverseFunctionalProperty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(symmetricalPropertyEClass, SymmetricalProperty.class, "SymmetricalProperty", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSymmetricalProperty_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, SymmetricalProperty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(transitivePropertyEClass, TransitiveProperty.class, "TransitiveProperty", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getTransitiveProperty_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, TransitiveProperty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(inversePropertyEClass, InverseProperty.class, "InverseProperty", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getInverseProperty_PropertyName1(), this.getResourceByName(), null, "propertyName1", null, 0, 1, InverseProperty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getInverseProperty_InvOf(), this.getIsInverseOf(), null, "invOf", null, 0, 1, InverseProperty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(isInverseOfEClass, IsInverseOf.class, "IsInverseOf", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getIsInverseOf_PropertyName2(), this.getResourceByName(), null, "propertyName2", null, 0, 1, IsInverseOf.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(typedBNodeEClass, TypedBNode.class, "TypedBNode", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getTypedBNode_Article(), ecorePackage.getEString(), "article", null, 0, 1, TypedBNode.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getTypedBNode_ClassIdentifier(), this.getResourceIdentifier(), null, "classIdentifier", null, 0, 1, TypedBNode.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(instanceDeclarationStatementEClass, InstanceDeclarationStatement.class, "InstanceDeclarationStatement", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(instanceDeclarationEClass, InstanceDeclaration.class, "InstanceDeclaration", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getInstanceDeclaration_TypeDecl(), this.getTypeDeclaration(), null, "typeDecl", null, 0, 1, InstanceDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getInstanceDeclaration_AddlInfoItems(), this.getPropValPartialTriple(), null, "addlInfoItems", null, 0, -1, InstanceDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getInstanceDeclaration_Article(), ecorePackage.getEString(), "article", null, 0, 1, InstanceDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getInstanceDeclaration_ClassName(), this.getResourceByName(), null, "className", null, 0, 1, InstanceDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getInstanceDeclaration_InstanceName(), this.getResourceName(), null, "instanceName", null, 0, 1, InstanceDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(typeDeclarationEClass, TypeDeclaration.class, "TypeDeclaration", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getTypeDeclaration_InstName(), this.getResourceName(), null, "instName", null, 0, 1, TypeDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getTypeDeclaration_Type(), this.getTypedBNode(), null, "type", null, 0, 1, TypeDeclaration.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(instanceDifferentFromEClass, InstanceDifferentFrom.class, "InstanceDifferentFrom", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getInstanceDifferentFrom_InstName1(), this.getResourceByName(), null, "instName1", null, 0, 1, InstanceDifferentFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getInstanceDifferentFrom_InstName2(), this.getResourceByName(), null, "instName2", null, 0, 1, InstanceDifferentFrom.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(instancesAllDifferentEClass, InstancesAllDifferent.class, "InstancesAllDifferent", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getInstancesAllDifferent_Instances(), this.getExistingResourceList(), null, "instances", null, 0, 1, InstancesAllDifferent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(existingInstanceAttributionEClass, ExistingInstanceAttribution.class, "ExistingInstanceAttribution", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getExistingInstanceAttribution_Subj(), this.getResourceByName(), null, "subj", null, 0, 1, ExistingInstanceAttribution.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getExistingInstanceAttribution_AddlInfoItems(), this.getPropValPartialTriple(), null, "addlInfoItems", null, 0, -1, ExistingInstanceAttribution.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getExistingInstanceAttribution_POfS(), this.getOfPatternReturningValues(), null, "pOfS", null, 0, 1, ExistingInstanceAttribution.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getExistingInstanceAttribution_Obj(), ecorePackage.getEObject(), null, "obj", null, 0, 1, ExistingInstanceAttribution.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(objectEClass, com.ge.research.sadl.sadl.Object.class, "Object", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getObject_Val(), ecorePackage.getEObject(), null, "val", null, 0, 1, com.ge.research.sadl.sadl.Object.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(propValPartialTripleEClass, PropValPartialTriple.class, "PropValPartialTriple", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getPropValPartialTriple_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, PropValPartialTriple.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getPropValPartialTriple_ObjectValue(), this.getExplicitValue(), null, "objectValue", null, 0, 1, PropValPartialTriple.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getPropValPartialTriple_ObjectValueBNode(), this.getInstanceDeclaration(), null, "objectValueBNode", null, 0, 1, PropValPartialTriple.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(ofPatternReturningValuesEClass, OfPatternReturningValues.class, "OfPatternReturningValues", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getOfPatternReturningValues_Ofphrs(), this.getOfPhrase(), null, "ofphrs", null, 0, -1, OfPatternReturningValues.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getOfPatternReturningValues_Subject(), this.getResourceByName(), null, "subject", null, 0, 1, OfPatternReturningValues.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getOfPatternReturningValues_Type(), this.getTypedBNode(), null, "type", null, 0, 1, OfPatternReturningValues.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(withChainEClass, WithChain.class, "WithChain", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getWithChain_Wps(), this.getWithPhrase(), null, "wps", null, 0, -1, WithChain.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(withPhraseEClass, WithPhrase.class, "WithPhrase", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getWithPhrase_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, WithPhrase.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getWithPhrase_Value(), ecorePackage.getEObject(), null, "value", null, 0, 1, WithPhrase.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(embeddedInstanceDeclarationEClass, EmbeddedInstanceDeclaration.class, "EmbeddedInstanceDeclaration", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(mergedTriplesEClass, MergedTriples.class, "MergedTriples", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getMergedTriples_Ops(), this.getOfPhrase(), null, "ops", null, 0, -1, MergedTriples.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getMergedTriples_Pivot(), this.getTypedBNode(), null, "pivot", null, 0, 1, MergedTriples.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getMergedTriples_Wcs(), this.getWithChain(), null, "wcs", null, 0, -1, MergedTriples.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(ofPhraseEClass, OfPhrase.class, "OfPhrase", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getOfPhrase_Article(), ecorePackage.getEString(), "article", null, 0, 1, OfPhrase.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getOfPhrase_PropertyName(), this.getResourceByName(), null, "propertyName", null, 0, 1, OfPhrase.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(variableListEClass, VariableList.class, "VariableList", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getVariableList_Names(), this.getResourceName(), null, "names", null, 0, -1, VariableList.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(ruleEClass, Rule.class, "Rule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getRule_Name(), ecorePackage.getEString(), "name", null, 0, 1, Rule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getRule_Givens(), this.getElementSet(), null, "givens", null, 0, 1, Rule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getRule_Ifs(), this.getElementSet(), null, "ifs", null, 0, 1, Rule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getRule_Thens(), this.getElementSet(), null, "thens", null, 0, 1, Rule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(queryEClass, Query.class, "Query", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getQuery_Expr(), this.getExpression(), null, "expr", null, 0, 1, Query.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(testEClass, Test.class, "Test", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getTest_Expr(), this.getExpression(), null, "expr", null, 0, 1, Test.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(exprEClass, Expr.class, "Expr", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getExpr_Expr(), this.getExpression(), null, "expr", null, 0, 1, Expr.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(displayEClass, Display.class, "Display", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getDisplay_DisplayString(), ecorePackage.getEString(), "displayString", null, 0, 1, Display.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getDisplay_Model(), ecorePackage.getEString(), "model", null, 0, 1, Display.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(explanationEClass, Explanation.class, "Explanation", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getExplanation_Expr(), ecorePackage.getEObject(), null, "expr", null, 0, 1, Explanation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getExplanation_Rulename(), ecorePackage.getEString(), "rulename", null, 0, 1, Explanation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(elementSetEClass, ElementSet.class, "ElementSet", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getElementSet_Elements(), this.getExpression(), null, "elements", null, 0, -1, ElementSet.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(selectExpressionEClass, SelectExpression.class, "SelectExpression", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getSelectExpression_Distinct(), ecorePackage.getEString(), "distinct", null, 0, 1, SelectExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getSelectExpression_AllVars(), ecorePackage.getEString(), "allVars", null, 0, 1, SelectExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSelectExpression_VarList(), this.getVariableList(), null, "varList", null, 0, 1, SelectExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getSelectExpression_Orderby(), ecorePackage.getEString(), "orderby", null, 0, 1, SelectExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSelectExpression_OrderList(), this.getOrderList(), null, "orderList", null, 0, 1, SelectExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(constructExpressionEClass, ConstructExpression.class, "ConstructExpression", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getConstructExpression_Subj(), this.getResourceName(), null, "subj", null, 0, 1, ConstructExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getConstructExpression_Pred(), this.getResourceName(), null, "pred", null, 0, 1, ConstructExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getConstructExpression_Obj(), this.getResourceName(), null, "obj", null, 0, 1, ConstructExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(askQueryExpressionEClass, AskQueryExpression.class, "AskQueryExpression", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(orderListEClass, OrderList.class, "OrderList", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getOrderList_OrderList(), this.getOrderElement(), null, "orderList", null, 0, -1, OrderList.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(orderElementEClass, OrderElement.class, "OrderElement", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getOrderElement_Order(), ecorePackage.getEString(), "order", null, 0, 1, OrderElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getOrderElement_Name(), this.getResourceName(), null, "name", null, 0, 1, OrderElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(expressionEClass, Expression.class, "Expression", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getExpression_Expr(), this.getExpression(), null, "expr", null, 0, 1, Expression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getExpression_Func(), ecorePackage.getEString(), "func", null, 0, 1, Expression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getExpression_Args(), this.getExpression(), null, "args", null, 0, -1, Expression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getExpression_Gp(), this.getGraphPattern(), null, "gp", null, 0, 1, Expression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getExpression_Ivalue(), this.getIntervalValue(), null, "ivalue", null, 0, 1, Expression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getExpression_Value(), this.getExplicitValue(), null, "value", null, 0, 1, Expression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getExpression_ValueTable(), this.getValueTable(), null, "valueTable", null, 0, 1, Expression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(graphPatternEClass, GraphPattern.class, "GraphPattern", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(propOfSubjEClass, PropOfSubj.class, "PropOfSubj", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getPropOfSubj_OfPhr(), this.getOfPhrase(), null, "ofPhr", null, 0, -1, PropOfSubj.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getPropOfSubj_Subj(), this.getResourceByName(), null, "subj", null, 0, 1, PropOfSubj.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(subjPropEClass, SubjProp.class, "SubjProp", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSubjProp_Subj(), this.getResourceByName(), null, "subj", null, 0, 1, SubjProp.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSubjProp_HwPhr(), this.getWithPhrase(), null, "hwPhr", null, 0, -1, SubjProp.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(instAttrSPVEClass, InstAttrSPV.class, "InstAttrSPV", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getInstAttrSPV_Subj(), this.getResourceByName(), null, "subj", null, 0, 1, InstAttrSPV.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getInstAttrSPV_Props(), this.getResourceByName(), null, "props", null, 0, -1, InstAttrSPV.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getInstAttrSPV_Vals(), this.getExpression(), null, "vals", null, 0, -1, InstAttrSPV.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(instAttrPSVEClass, InstAttrPSV.class, "InstAttrPSV", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getInstAttrPSV_Prop(), this.getPropOfSubj(), null, "prop", null, 0, 1, InstAttrPSV.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getInstAttrPSV_Val(), this.getExplicitValue(), null, "val", null, 0, 1, InstAttrPSV.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(subTypeOfEClass, SubTypeOf.class, "SubTypeOf", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSubTypeOf_Subclass(), this.getResourceByName(), null, "subclass", null, 0, 1, SubTypeOf.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSubTypeOf_Superclass(), this.getResourceByName(), null, "superclass", null, 0, 1, SubTypeOf.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(intervalValueEClass, IntervalValue.class, "IntervalValue", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getIntervalValue_Op(), ecorePackage.getEString(), "op", null, 0, 1, IntervalValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getIntervalValue_Expr(), this.getExpression(), null, "expr", null, 0, 1, IntervalValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(explicitValueEClass, ExplicitValue.class, "ExplicitValue", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getExplicitValue_InstName(), this.getResourceByName(), null, "instName", null, 0, 1, ExplicitValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getExplicitValue_LitValue(), this.getLiteralValue(), null, "litValue", null, 0, 1, ExplicitValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getExplicitValue_Term(), ecorePackage.getEString(), "term", null, 0, 1, ExplicitValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(valueTableEClass, ValueTable.class, "ValueTable", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getValueTable_Row(), this.getValueRow(), null, "row", null, 0, 1, ValueTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getValueTable_Rows(), this.getValueRow(), null, "rows", null, 0, -1, ValueTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(literalValueEClass, LiteralValue.class, "LiteralValue", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getLiteralValue_LiteralNumber(), ecorePackage.getEString(), "literalNumber", null, 0, 1, LiteralValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getLiteralValue_LiteralString(), ecorePackage.getEString(), "literalString", null, 0, 1, LiteralValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getLiteralValue_LiteralBoolean(), ecorePackage.getEString(), "literalBoolean", null, 0, 1, LiteralValue.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(valueRowEClass, ValueRow.class, "ValueRow", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getValueRow_ExplicitValues(), this.getExplicitValue(), null, "explicitValues", null, 0, -1, ValueRow.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(junctionExpressionEClass, JunctionExpression.class, "JunctionExpression", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getJunctionExpression_Left(), this.getExpression(), null, "left", null, 0, 1, JunctionExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getJunctionExpression_Op(), ecorePackage.getEString(), "op", null, 0, 1, JunctionExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getJunctionExpression_Right(), this.getExpression(), null, "right", null, 0, 1, JunctionExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(binaryOpExpressionEClass, BinaryOpExpression.class, "BinaryOpExpression", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getBinaryOpExpression_Left(), this.getExpression(), null, "left", null, 0, 1, BinaryOpExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getBinaryOpExpression_Op(), ecorePackage.getEString(), "op", null, 0, 1, BinaryOpExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getBinaryOpExpression_Right(), this.getExpression(), null, "right", null, 0, 1, BinaryOpExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(unaryOpExpressionEClass, UnaryOpExpression.class, "UnaryOpExpression", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getUnaryOpExpression_Op(), ecorePackage.getEString(), "op", null, 0, 1, UnaryOpExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    // Initialize enums and add enum literals
    initEEnum(dataTypeEEnum, DataType.class, "DataType");
    addEEnumLiteral(dataTypeEEnum, DataType.STRING);
    addEEnumLiteral(dataTypeEEnum, DataType.BOOLEAN);
    addEEnumLiteral(dataTypeEEnum, DataType.DECIMAL);
    addEEnumLiteral(dataTypeEEnum, DataType.INT);
    addEEnumLiteral(dataTypeEEnum, DataType.LONG);
    addEEnumLiteral(dataTypeEEnum, DataType.FLOAT);
    addEEnumLiteral(dataTypeEEnum, DataType.DOUBLE);
    addEEnumLiteral(dataTypeEEnum, DataType.DURATION);
    addEEnumLiteral(dataTypeEEnum, DataType.DATE_TIME);
    addEEnumLiteral(dataTypeEEnum, DataType.TIME);
    addEEnumLiteral(dataTypeEEnum, DataType.DATE);
    addEEnumLiteral(dataTypeEEnum, DataType.GYEAR_MONTH);
    addEEnumLiteral(dataTypeEEnum, DataType.GYEAR);
    addEEnumLiteral(dataTypeEEnum, DataType.GMONTH_DAY);
    addEEnumLiteral(dataTypeEEnum, DataType.GDAY);
    addEEnumLiteral(dataTypeEEnum, DataType.GMONTH);
    addEEnumLiteral(dataTypeEEnum, DataType.HEX_BINARY);
    addEEnumLiteral(dataTypeEEnum, DataType.BASE64_BINARY);
    addEEnumLiteral(dataTypeEEnum, DataType.ANY_URI);
    addEEnumLiteral(dataTypeEEnum, DataType.DATA);

    // Create resource
    createResource(eNS_URI);
  }

} //SadlPackageImpl
