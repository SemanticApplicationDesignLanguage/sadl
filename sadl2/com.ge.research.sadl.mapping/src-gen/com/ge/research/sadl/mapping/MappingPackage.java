/**
 */
package com.ge.research.sadl.mapping;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see com.ge.research.sadl.mapping.MappingFactory
 * @model kind="package"
 * @generated
 */
public interface MappingPackage extends EPackage
{
  /**
   * The package name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNAME = "mapping";

  /**
   * The package namespace URI.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_URI = "http://www.ge.com/research/sadl/Mapping";

  /**
   * The package namespace name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_PREFIX = "mapping";

  /**
   * The singleton instance of the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  MappingPackage eINSTANCE = com.ge.research.sadl.mapping.impl.MappingPackageImpl.init();

  /**
   * The meta object id for the '{@link com.ge.research.sadl.mapping.impl.ModelImpl <em>Model</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.mapping.impl.ModelImpl
   * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getModel()
   * @generated
   */
  int MODEL = 0;

  /**
   * The feature id for the '<em><b>Uri</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL__URI = 0;

  /**
   * The feature id for the '<em><b>Imports</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL__IMPORTS = 1;

  /**
   * The feature id for the '<em><b>Triples</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL__TRIPLES = 2;

  /**
   * The number of structural features of the '<em>Model</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.mapping.impl.NewModelNSImpl <em>New Model NS</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.mapping.impl.NewModelNSImpl
   * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getNewModelNS()
   * @generated
   */
  int NEW_MODEL_NS = 1;

  /**
   * The feature id for the '<em><b>Base Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int NEW_MODEL_NS__BASE_URI = 0;

  /**
   * The feature id for the '<em><b>Prefix</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int NEW_MODEL_NS__PREFIX = 1;

  /**
   * The number of structural features of the '<em>New Model NS</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int NEW_MODEL_NS_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.mapping.impl.ImportImpl <em>Import</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.mapping.impl.ImportImpl
   * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getImport()
   * @generated
   */
  int IMPORT = 2;

  /**
   * The feature id for the '<em><b>Import URI</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int IMPORT__IMPORT_URI = 0;

  /**
   * The feature id for the '<em><b>Alias</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int IMPORT__ALIAS = 1;

  /**
   * The number of structural features of the '<em>Import</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int IMPORT_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.mapping.impl.GroupImpl <em>Group</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.mapping.impl.GroupImpl
   * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getGroup()
   * @generated
   */
  int GROUP = 3;

  /**
   * The feature id for the '<em><b>Group Lines</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GROUP__GROUP_LINES = 0;

  /**
   * The number of structural features of the '<em>Group</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GROUP_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.mapping.impl.TripleImpl <em>Triple</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.mapping.impl.TripleImpl
   * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getTriple()
   * @generated
   */
  int TRIPLE = 4;

  /**
   * The feature id for the '<em><b>Subj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TRIPLE__SUBJ = 0;

  /**
   * The feature id for the '<em><b>Pred</b></em>' reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TRIPLE__PRED = 1;

  /**
   * The feature id for the '<em><b>Objval</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TRIPLE__OBJVAL = 2;

  /**
   * The number of structural features of the '<em>Triple</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TRIPLE_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.mapping.impl.LiteralValueImpl <em>Literal Value</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.mapping.impl.LiteralValueImpl
   * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getLiteralValue()
   * @generated
   */
  int LITERAL_VALUE = 5;

  /**
   * The feature id for the '<em><b>Literal Number</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int LITERAL_VALUE__LITERAL_NUMBER = 0;

  /**
   * The feature id for the '<em><b>Literal String</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int LITERAL_VALUE__LITERAL_STRING = 1;

  /**
   * The feature id for the '<em><b>Literal Boolean</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int LITERAL_VALUE__LITERAL_BOOLEAN = 2;

  /**
   * The number of structural features of the '<em>Literal Value</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int LITERAL_VALUE_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.mapping.impl.RefImpl <em>Ref</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.mapping.impl.RefImpl
   * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getRef()
   * @generated
   */
  int REF = 6;

  /**
   * The feature id for the '<em><b>Ref</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int REF__REF = 0;

  /**
   * The feature id for the '<em><b>Addlcols</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int REF__ADDLCOLS = 1;

  /**
   * The feature id for the '<em><b>Row</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int REF__ROW = 2;

  /**
   * The number of structural features of the '<em>Ref</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int REF_FEATURE_COUNT = 3;


  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.mapping.Model <em>Model</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Model</em>'.
   * @see com.ge.research.sadl.mapping.Model
   * @generated
   */
  EClass getModel();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.mapping.Model#getUri <em>Uri</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Uri</em>'.
   * @see com.ge.research.sadl.mapping.Model#getUri()
   * @see #getModel()
   * @generated
   */
  EReference getModel_Uri();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.mapping.Model#getImports <em>Imports</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Imports</em>'.
   * @see com.ge.research.sadl.mapping.Model#getImports()
   * @see #getModel()
   * @generated
   */
  EReference getModel_Imports();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.mapping.Model#getTriples <em>Triples</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Triples</em>'.
   * @see com.ge.research.sadl.mapping.Model#getTriples()
   * @see #getModel()
   * @generated
   */
  EReference getModel_Triples();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.mapping.NewModelNS <em>New Model NS</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>New Model NS</em>'.
   * @see com.ge.research.sadl.mapping.NewModelNS
   * @generated
   */
  EClass getNewModelNS();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.mapping.NewModelNS#getBaseUri <em>Base Uri</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Base Uri</em>'.
   * @see com.ge.research.sadl.mapping.NewModelNS#getBaseUri()
   * @see #getNewModelNS()
   * @generated
   */
  EAttribute getNewModelNS_BaseUri();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.mapping.NewModelNS#getPrefix <em>Prefix</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Prefix</em>'.
   * @see com.ge.research.sadl.mapping.NewModelNS#getPrefix()
   * @see #getNewModelNS()
   * @generated
   */
  EAttribute getNewModelNS_Prefix();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.mapping.Import <em>Import</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Import</em>'.
   * @see com.ge.research.sadl.mapping.Import
   * @generated
   */
  EClass getImport();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.mapping.Import#getImportURI <em>Import URI</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Import URI</em>'.
   * @see com.ge.research.sadl.mapping.Import#getImportURI()
   * @see #getImport()
   * @generated
   */
  EAttribute getImport_ImportURI();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.mapping.Import#getAlias <em>Alias</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Alias</em>'.
   * @see com.ge.research.sadl.mapping.Import#getAlias()
   * @see #getImport()
   * @generated
   */
  EAttribute getImport_Alias();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.mapping.Group <em>Group</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Group</em>'.
   * @see com.ge.research.sadl.mapping.Group
   * @generated
   */
  EClass getGroup();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.mapping.Group#getGroupLines <em>Group Lines</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Group Lines</em>'.
   * @see com.ge.research.sadl.mapping.Group#getGroupLines()
   * @see #getGroup()
   * @generated
   */
  EReference getGroup_GroupLines();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.mapping.Triple <em>Triple</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Triple</em>'.
   * @see com.ge.research.sadl.mapping.Triple
   * @generated
   */
  EClass getTriple();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.mapping.Triple#getSubj <em>Subj</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Subj</em>'.
   * @see com.ge.research.sadl.mapping.Triple#getSubj()
   * @see #getTriple()
   * @generated
   */
  EReference getTriple_Subj();

  /**
   * Returns the meta object for the reference '{@link com.ge.research.sadl.mapping.Triple#getPred <em>Pred</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference '<em>Pred</em>'.
   * @see com.ge.research.sadl.mapping.Triple#getPred()
   * @see #getTriple()
   * @generated
   */
  EReference getTriple_Pred();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.mapping.Triple#getObjval <em>Objval</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Objval</em>'.
   * @see com.ge.research.sadl.mapping.Triple#getObjval()
   * @see #getTriple()
   * @generated
   */
  EReference getTriple_Objval();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.mapping.LiteralValue <em>Literal Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Literal Value</em>'.
   * @see com.ge.research.sadl.mapping.LiteralValue
   * @generated
   */
  EClass getLiteralValue();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.mapping.LiteralValue#getLiteralNumber <em>Literal Number</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Literal Number</em>'.
   * @see com.ge.research.sadl.mapping.LiteralValue#getLiteralNumber()
   * @see #getLiteralValue()
   * @generated
   */
  EAttribute getLiteralValue_LiteralNumber();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.mapping.LiteralValue#getLiteralString <em>Literal String</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Literal String</em>'.
   * @see com.ge.research.sadl.mapping.LiteralValue#getLiteralString()
   * @see #getLiteralValue()
   * @generated
   */
  EAttribute getLiteralValue_LiteralString();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.mapping.LiteralValue#getLiteralBoolean <em>Literal Boolean</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Literal Boolean</em>'.
   * @see com.ge.research.sadl.mapping.LiteralValue#getLiteralBoolean()
   * @see #getLiteralValue()
   * @generated
   */
  EAttribute getLiteralValue_LiteralBoolean();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.mapping.Ref <em>Ref</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Ref</em>'.
   * @see com.ge.research.sadl.mapping.Ref
   * @generated
   */
  EClass getRef();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.mapping.Ref#getRef <em>Ref</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Ref</em>'.
   * @see com.ge.research.sadl.mapping.Ref#getRef()
   * @see #getRef()
   * @generated
   */
  EAttribute getRef_Ref();

  /**
   * Returns the meta object for the attribute list '{@link com.ge.research.sadl.mapping.Ref#getAddlcols <em>Addlcols</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute list '<em>Addlcols</em>'.
   * @see com.ge.research.sadl.mapping.Ref#getAddlcols()
   * @see #getRef()
   * @generated
   */
  EAttribute getRef_Addlcols();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.mapping.Ref#getRow <em>Row</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Row</em>'.
   * @see com.ge.research.sadl.mapping.Ref#getRow()
   * @see #getRef()
   * @generated
   */
  EAttribute getRef_Row();

  /**
   * Returns the factory that creates the instances of the model.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the factory that creates the instances of the model.
   * @generated
   */
  MappingFactory getMappingFactory();

  /**
   * <!-- begin-user-doc -->
   * Defines literals for the meta objects that represent
   * <ul>
   *   <li>each class,</li>
   *   <li>each feature of each class,</li>
   *   <li>each enum,</li>
   *   <li>and each data type</li>
   * </ul>
   * <!-- end-user-doc -->
   * @generated
   */
  interface Literals
  {
    /**
     * The meta object literal for the '{@link com.ge.research.sadl.mapping.impl.ModelImpl <em>Model</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.mapping.impl.ModelImpl
     * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getModel()
     * @generated
     */
    EClass MODEL = eINSTANCE.getModel();

    /**
     * The meta object literal for the '<em><b>Uri</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MODEL__URI = eINSTANCE.getModel_Uri();

    /**
     * The meta object literal for the '<em><b>Imports</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MODEL__IMPORTS = eINSTANCE.getModel_Imports();

    /**
     * The meta object literal for the '<em><b>Triples</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MODEL__TRIPLES = eINSTANCE.getModel_Triples();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.mapping.impl.NewModelNSImpl <em>New Model NS</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.mapping.impl.NewModelNSImpl
     * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getNewModelNS()
     * @generated
     */
    EClass NEW_MODEL_NS = eINSTANCE.getNewModelNS();

    /**
     * The meta object literal for the '<em><b>Base Uri</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute NEW_MODEL_NS__BASE_URI = eINSTANCE.getNewModelNS_BaseUri();

    /**
     * The meta object literal for the '<em><b>Prefix</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute NEW_MODEL_NS__PREFIX = eINSTANCE.getNewModelNS_Prefix();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.mapping.impl.ImportImpl <em>Import</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.mapping.impl.ImportImpl
     * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getImport()
     * @generated
     */
    EClass IMPORT = eINSTANCE.getImport();

    /**
     * The meta object literal for the '<em><b>Import URI</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute IMPORT__IMPORT_URI = eINSTANCE.getImport_ImportURI();

    /**
     * The meta object literal for the '<em><b>Alias</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute IMPORT__ALIAS = eINSTANCE.getImport_Alias();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.mapping.impl.GroupImpl <em>Group</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.mapping.impl.GroupImpl
     * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getGroup()
     * @generated
     */
    EClass GROUP = eINSTANCE.getGroup();

    /**
     * The meta object literal for the '<em><b>Group Lines</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference GROUP__GROUP_LINES = eINSTANCE.getGroup_GroupLines();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.mapping.impl.TripleImpl <em>Triple</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.mapping.impl.TripleImpl
     * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getTriple()
     * @generated
     */
    EClass TRIPLE = eINSTANCE.getTriple();

    /**
     * The meta object literal for the '<em><b>Subj</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference TRIPLE__SUBJ = eINSTANCE.getTriple_Subj();

    /**
     * The meta object literal for the '<em><b>Pred</b></em>' reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference TRIPLE__PRED = eINSTANCE.getTriple_Pred();

    /**
     * The meta object literal for the '<em><b>Objval</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference TRIPLE__OBJVAL = eINSTANCE.getTriple_Objval();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.mapping.impl.LiteralValueImpl <em>Literal Value</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.mapping.impl.LiteralValueImpl
     * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getLiteralValue()
     * @generated
     */
    EClass LITERAL_VALUE = eINSTANCE.getLiteralValue();

    /**
     * The meta object literal for the '<em><b>Literal Number</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute LITERAL_VALUE__LITERAL_NUMBER = eINSTANCE.getLiteralValue_LiteralNumber();

    /**
     * The meta object literal for the '<em><b>Literal String</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute LITERAL_VALUE__LITERAL_STRING = eINSTANCE.getLiteralValue_LiteralString();

    /**
     * The meta object literal for the '<em><b>Literal Boolean</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute LITERAL_VALUE__LITERAL_BOOLEAN = eINSTANCE.getLiteralValue_LiteralBoolean();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.mapping.impl.RefImpl <em>Ref</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.mapping.impl.RefImpl
     * @see com.ge.research.sadl.mapping.impl.MappingPackageImpl#getRef()
     * @generated
     */
    EClass REF = eINSTANCE.getRef();

    /**
     * The meta object literal for the '<em><b>Ref</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute REF__REF = eINSTANCE.getRef_Ref();

    /**
     * The meta object literal for the '<em><b>Addlcols</b></em>' attribute list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute REF__ADDLCOLS = eINSTANCE.getRef_Addlcols();

    /**
     * The meta object literal for the '<em><b>Row</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute REF__ROW = eINSTANCE.getRef_Row();

  }

} //MappingPackage
