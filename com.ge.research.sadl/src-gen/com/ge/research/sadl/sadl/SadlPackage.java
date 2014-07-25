/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
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
 * @see com.ge.research.sadl.sadl.SadlFactory
 * @model kind="package"
 * @generated
 */
public interface SadlPackage extends EPackage
{
  /**
   * The package name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNAME = "sadl";

  /**
   * The package namespace URI.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_URI = "http://www.ge.com/research/sadl/Sadl";

  /**
   * The package namespace name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_PREFIX = "sadl";

  /**
   * The singleton instance of the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  SadlPackage eINSTANCE = com.ge.research.sadl.sadl.impl.SadlPackageImpl.init();

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ModelImpl <em>Model</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ModelImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getModel()
   * @generated
   */
  int MODEL = 0;

  /**
   * The feature id for the '<em><b>Model Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL__MODEL_NAME = 0;

  /**
   * The feature id for the '<em><b>Imports</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL__IMPORTS = 1;

  /**
   * The feature id for the '<em><b>Elements</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL__ELEMENTS = 2;

  /**
   * The number of structural features of the '<em>Model</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ModelNameImpl <em>Model Name</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ModelNameImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getModelName()
   * @generated
   */
  int MODEL_NAME = 1;

  /**
   * The feature id for the '<em><b>Base Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL_NAME__BASE_URI = 0;

  /**
   * The feature id for the '<em><b>Alias</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL_NAME__ALIAS = 1;

  /**
   * The feature id for the '<em><b>Version</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL_NAME__VERSION = 2;

  /**
   * The number of structural features of the '<em>Model Name</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL_NAME_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ImportImpl <em>Import</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ImportImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getImport()
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
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ModelElementImpl <em>Model Element</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ModelElementImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getModelElement()
   * @generated
   */
  int MODEL_ELEMENT = 3;

  /**
   * The number of structural features of the '<em>Model Element</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL_ELEMENT_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.StatementImpl <em>Statement</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.StatementImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getStatement()
   * @generated
   */
  int STATEMENT = 4;

  /**
   * The number of structural features of the '<em>Statement</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int STATEMENT_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ResourceNameImpl <em>Resource Name</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ResourceNameImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceName()
   * @generated
   */
  int RESOURCE_NAME = 5;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_NAME__NAME = 0;

  /**
   * The feature id for the '<em><b>Ann Type</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_NAME__ANN_TYPE = 1;

  /**
   * The feature id for the '<em><b>Ann Content</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_NAME__ANN_CONTENT = 2;

  /**
   * The number of structural features of the '<em>Resource Name</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_NAME_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ContentListImpl <em>Content List</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ContentListImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getContentList()
   * @generated
   */
  int CONTENT_LIST = 6;

  /**
   * The feature id for the '<em><b>Ann Content</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTENT_LIST__ANN_CONTENT = 0;

  /**
   * The number of structural features of the '<em>Content List</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTENT_LIST_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ResourceListImpl <em>Resource List</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ResourceListImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceList()
   * @generated
   */
  int RESOURCE_LIST = 7;

  /**
   * The feature id for the '<em><b>Names</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_LIST__NAMES = 0;

  /**
   * The number of structural features of the '<em>Resource List</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_LIST_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.LiteralListImpl <em>Literal List</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.LiteralListImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getLiteralList()
   * @generated
   */
  int LITERAL_LIST = 8;

  /**
   * The feature id for the '<em><b>Literals</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int LITERAL_LIST__LITERALS = 0;

  /**
   * The number of structural features of the '<em>Literal List</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int LITERAL_LIST_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ResourceIdentifierImpl <em>Resource Identifier</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ResourceIdentifierImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceIdentifier()
   * @generated
   */
  int RESOURCE_IDENTIFIER = 11;

  /**
   * The number of structural features of the '<em>Resource Identifier</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_IDENTIFIER_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ResourceByNameImpl <em>Resource By Name</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ResourceByNameImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceByName()
   * @generated
   */
  int RESOURCE_BY_NAME = 9;

  /**
   * The feature id for the '<em><b>Name</b></em>' reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_NAME__NAME = RESOURCE_IDENTIFIER_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Resource By Name</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_NAME_FEATURE_COUNT = RESOURCE_IDENTIFIER_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ExistingResourceListImpl <em>Existing Resource List</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ExistingResourceListImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExistingResourceList()
   * @generated
   */
  int EXISTING_RESOURCE_LIST = 10;

  /**
   * The feature id for the '<em><b>Names</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXISTING_RESOURCE_LIST__NAMES = 0;

  /**
   * The number of structural features of the '<em>Existing Resource List</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXISTING_RESOURCE_LIST_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ResourceBySetOpImpl <em>Resource By Set Op</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ResourceBySetOpImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceBySetOp()
   * @generated
   */
  int RESOURCE_BY_SET_OP = 12;

  /**
   * The feature id for the '<em><b>Ann Type</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_SET_OP__ANN_TYPE = RESOURCE_IDENTIFIER_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Ann Content</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_SET_OP__ANN_CONTENT = RESOURCE_IDENTIFIER_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Names</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_SET_OP__NAMES = RESOURCE_IDENTIFIER_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Op</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_SET_OP__OP = RESOURCE_IDENTIFIER_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Resource By Set Op</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_SET_OP_FEATURE_COUNT = RESOURCE_IDENTIFIER_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ResourceByRestrictionImpl <em>Resource By Restriction</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ResourceByRestrictionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceByRestriction()
   * @generated
   */
  int RESOURCE_BY_RESTRICTION = 13;

  /**
   * The feature id for the '<em><b>Ann Type</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_RESTRICTION__ANN_TYPE = RESOURCE_IDENTIFIER_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Ann Content</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_RESTRICTION__ANN_CONTENT = RESOURCE_IDENTIFIER_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Prop Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_RESTRICTION__PROP_NAME = RESOURCE_IDENTIFIER_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Cond</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_RESTRICTION__COND = RESOURCE_IDENTIFIER_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Resource By Restriction</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RESOURCE_BY_RESTRICTION_FEATURE_COUNT = RESOURCE_IDENTIFIER_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.UnionResourceImpl <em>Union Resource</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.UnionResourceImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getUnionResource()
   * @generated
   */
  int UNION_RESOURCE = 14;

  /**
   * The feature id for the '<em><b>Ann Type</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNION_RESOURCE__ANN_TYPE = RESOURCE_BY_SET_OP__ANN_TYPE;

  /**
   * The feature id for the '<em><b>Ann Content</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNION_RESOURCE__ANN_CONTENT = RESOURCE_BY_SET_OP__ANN_CONTENT;

  /**
   * The feature id for the '<em><b>Names</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNION_RESOURCE__NAMES = RESOURCE_BY_SET_OP__NAMES;

  /**
   * The feature id for the '<em><b>Op</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNION_RESOURCE__OP = RESOURCE_BY_SET_OP__OP;

  /**
   * The number of structural features of the '<em>Union Resource</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNION_RESOURCE_FEATURE_COUNT = RESOURCE_BY_SET_OP_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.IntersectionResourceImpl <em>Intersection Resource</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.IntersectionResourceImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getIntersectionResource()
   * @generated
   */
  int INTERSECTION_RESOURCE = 15;

  /**
   * The feature id for the '<em><b>Ann Type</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INTERSECTION_RESOURCE__ANN_TYPE = RESOURCE_BY_SET_OP__ANN_TYPE;

  /**
   * The feature id for the '<em><b>Ann Content</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INTERSECTION_RESOURCE__ANN_CONTENT = RESOURCE_BY_SET_OP__ANN_CONTENT;

  /**
   * The feature id for the '<em><b>Names</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INTERSECTION_RESOURCE__NAMES = RESOURCE_BY_SET_OP__NAMES;

  /**
   * The feature id for the '<em><b>Op</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INTERSECTION_RESOURCE__OP = RESOURCE_BY_SET_OP__OP;

  /**
   * The number of structural features of the '<em>Intersection Resource</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INTERSECTION_RESOURCE_FEATURE_COUNT = RESOURCE_BY_SET_OP_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ClassDeclarationImpl <em>Class Declaration</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ClassDeclarationImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getClassDeclaration()
   * @generated
   */
  int CLASS_DECLARATION = 16;

  /**
   * The feature id for the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CLASS_DECLARATION__CLASS_NAME = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Must Be One Of</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CLASS_DECLARATION__MUST_BE_ONE_OF = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Described By</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CLASS_DECLARATION__DESCRIBED_BY = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Class List</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CLASS_DECLARATION__CLASS_LIST = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Class Identifier</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CLASS_DECLARATION__CLASS_IDENTIFIER = STATEMENT_FEATURE_COUNT + 4;

  /**
   * The number of structural features of the '<em>Class Declaration</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CLASS_DECLARATION_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 5;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.EnumeratedInstancesImpl <em>Enumerated Instances</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.EnumeratedInstancesImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getEnumeratedInstances()
   * @generated
   */
  int ENUMERATED_INSTANCES = 17;

  /**
   * The feature id for the '<em><b>Instance List</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ENUMERATED_INSTANCES__INSTANCE_LIST = 0;

  /**
   * The number of structural features of the '<em>Enumerated Instances</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ENUMERATED_INSTANCES_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.AddlClassInfoImpl <em>Addl Class Info</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.AddlClassInfoImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getAddlClassInfo()
   * @generated
   */
  int ADDL_CLASS_INFO = 18;

  /**
   * The feature id for the '<em><b>Property By Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDL_CLASS_INFO__PROPERTY_BY_NAME = 0;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDL_CLASS_INFO__PROPERTY_NAME = 1;

  /**
   * The feature id for the '<em><b>Range</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDL_CLASS_INFO__RANGE = 2;

  /**
   * The feature id for the '<em><b>Restriction</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDL_CLASS_INFO__RESTRICTION = 3;

  /**
   * The number of structural features of the '<em>Addl Class Info</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDL_CLASS_INFO_FEATURE_COUNT = 4;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.RangeImpl <em>Range</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.RangeImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getRange()
   * @generated
   */
  int RANGE = 19;

  /**
   * The feature id for the '<em><b>Single</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RANGE__SINGLE = 0;

  /**
   * The feature id for the '<em><b>Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RANGE__TYPE = 1;

  /**
   * The number of structural features of the '<em>Range</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RANGE_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.RangeTypeImpl <em>Range Type</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.RangeTypeImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getRangeType()
   * @generated
   */
  int RANGE_TYPE = 20;

  /**
   * The feature id for the '<em><b>Class Identifier</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RANGE_TYPE__CLASS_IDENTIFIER = 0;

  /**
   * The number of structural features of the '<em>Range Type</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RANGE_TYPE_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.UserDefinedDataTypeImpl <em>User Defined Data Type</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.UserDefinedDataTypeImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getUserDefinedDataType()
   * @generated
   */
  int USER_DEFINED_DATA_TYPE = 21;

  /**
   * The feature id for the '<em><b>Udt</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int USER_DEFINED_DATA_TYPE__UDT = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Restriction</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int USER_DEFINED_DATA_TYPE__RESTRICTION = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>User Defined Data Type</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int USER_DEFINED_DATA_TYPE_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.DataTypeRestrictionImpl <em>Data Type Restriction</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.DataTypeRestrictionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getDataTypeRestriction()
   * @generated
   */
  int DATA_TYPE_RESTRICTION = 22;

  /**
   * The feature id for the '<em><b>Basetype</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DATA_TYPE_RESTRICTION__BASETYPE = 0;

  /**
   * The feature id for the '<em><b>Facets</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DATA_TYPE_RESTRICTION__FACETS = 1;

  /**
   * The feature id for the '<em><b>Basetypes</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DATA_TYPE_RESTRICTION__BASETYPES = 2;

  /**
   * The number of structural features of the '<em>Data Type Restriction</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DATA_TYPE_RESTRICTION_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.FacetsImpl <em>Facets</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.FacetsImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getFacets()
   * @generated
   */
  int FACETS = 23;

  /**
   * The feature id for the '<em><b>Minexin</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FACETS__MINEXIN = 0;

  /**
   * The feature id for the '<em><b>Min</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FACETS__MIN = 1;

  /**
   * The feature id for the '<em><b>Max</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FACETS__MAX = 2;

  /**
   * The feature id for the '<em><b>Maxexin</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FACETS__MAXEXIN = 3;

  /**
   * The feature id for the '<em><b>Regex</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FACETS__REGEX = 4;

  /**
   * The feature id for the '<em><b>Len</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FACETS__LEN = 5;

  /**
   * The feature id for the '<em><b>Minlen</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FACETS__MINLEN = 6;

  /**
   * The feature id for the '<em><b>Maxlen</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FACETS__MAXLEN = 7;

  /**
   * The feature id for the '<em><b>Values</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FACETS__VALUES = 8;

  /**
   * The number of structural features of the '<em>Facets</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FACETS_FEATURE_COUNT = 9;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.EquivalentConceptsImpl <em>Equivalent Concepts</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.EquivalentConceptsImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getEquivalentConcepts()
   * @generated
   */
  int EQUIVALENT_CONCEPTS = 24;

  /**
   * The feature id for the '<em><b>Class1</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EQUIVALENT_CONCEPTS__CLASS1 = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Class2</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EQUIVALENT_CONCEPTS__CLASS2 = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Equivalent Concepts</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EQUIVALENT_CONCEPTS_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.DisjointClassesImpl <em>Disjoint Classes</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.DisjointClassesImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getDisjointClasses()
   * @generated
   */
  int DISJOINT_CLASSES = 25;

  /**
   * The feature id for the '<em><b>Class1</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DISJOINT_CLASSES__CLASS1 = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Class2</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DISJOINT_CLASSES__CLASS2 = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Classes</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DISJOINT_CLASSES__CLASSES = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Disjoint Classes</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DISJOINT_CLASSES_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ComplementOfClassImpl <em>Complement Of Class</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ComplementOfClassImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getComplementOfClass()
   * @generated
   */
  int COMPLEMENT_OF_CLASS = 26;

  /**
   * The feature id for the '<em><b>Class1</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int COMPLEMENT_OF_CLASS__CLASS1 = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Class2</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int COMPLEMENT_OF_CLASS__CLASS2 = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Complement Of Class</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int COMPLEMENT_OF_CLASS_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.AllValuesFromImpl <em>All Values From</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.AllValuesFromImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getAllValuesFrom()
   * @generated
   */
  int ALL_VALUES_FROM = 27;

  /**
   * The feature id for the '<em><b>Restricted</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ALL_VALUES_FROM__RESTRICTED = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Cond</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ALL_VALUES_FROM__COND = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ALL_VALUES_FROM__CLASS_NAME = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ALL_VALUES_FROM__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>All Values From</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ALL_VALUES_FROM_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.SomeValuesFromImpl <em>Some Values From</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.SomeValuesFromImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSomeValuesFrom()
   * @generated
   */
  int SOME_VALUES_FROM = 28;

  /**
   * The feature id for the '<em><b>Restricted</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SOME_VALUES_FROM__RESTRICTED = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Cond</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SOME_VALUES_FROM__COND = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SOME_VALUES_FROM__CLASS_NAME = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SOME_VALUES_FROM__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Some Values From</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SOME_VALUES_FROM_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.HasValueImpl <em>Has Value</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.HasValueImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getHasValue()
   * @generated
   */
  int HAS_VALUE = 29;

  /**
   * The feature id for the '<em><b>Restricted</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int HAS_VALUE__RESTRICTED = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Cond</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int HAS_VALUE__COND = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int HAS_VALUE__CLASS_NAME = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int HAS_VALUE__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Has Value</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int HAS_VALUE_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.CardinalityImpl <em>Cardinality</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.CardinalityImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getCardinality()
   * @generated
   */
  int CARDINALITY = 30;

  /**
   * The feature id for the '<em><b>Restricted</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CARDINALITY__RESTRICTED = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Cond</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CARDINALITY__COND = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CARDINALITY__CLASS_NAME = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CARDINALITY__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Cardinality</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CARDINALITY_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.MinCardinalityImpl <em>Min Cardinality</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.MinCardinalityImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getMinCardinality()
   * @generated
   */
  int MIN_CARDINALITY = 31;

  /**
   * The feature id for the '<em><b>Restricted</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MIN_CARDINALITY__RESTRICTED = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Cond</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MIN_CARDINALITY__COND = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MIN_CARDINALITY__CLASS_NAME = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MIN_CARDINALITY__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Min Cardinality</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MIN_CARDINALITY_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.MaxCardinalityImpl <em>Max Cardinality</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.MaxCardinalityImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getMaxCardinality()
   * @generated
   */
  int MAX_CARDINALITY = 32;

  /**
   * The feature id for the '<em><b>Restricted</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MAX_CARDINALITY__RESTRICTED = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Cond</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MAX_CARDINALITY__COND = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MAX_CARDINALITY__CLASS_NAME = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MAX_CARDINALITY__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Max Cardinality</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MAX_CARDINALITY_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.PropertyOfClassImpl <em>Property Of Class</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.PropertyOfClassImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getPropertyOfClass()
   * @generated
   */
  int PROPERTY_OF_CLASS = 33;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_OF_CLASS__PROPERTY_NAME = 0;

  /**
   * The feature id for the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_OF_CLASS__CLASS_NAME = 1;

  /**
   * The number of structural features of the '<em>Property Of Class</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_OF_CLASS_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ConditionImpl <em>Condition</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ConditionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getCondition()
   * @generated
   */
  int CONDITION = 44;

  /**
   * The number of structural features of the '<em>Condition</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONDITION_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.AllValuesConditionImpl <em>All Values Condition</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.AllValuesConditionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getAllValuesCondition()
   * @generated
   */
  int ALL_VALUES_CONDITION = 34;

  /**
   * The feature id for the '<em><b>Restriction</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ALL_VALUES_CONDITION__RESTRICTION = CONDITION_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>All Values Condition</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ALL_VALUES_CONDITION_FEATURE_COUNT = CONDITION_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.EnumeratedAllValuesFromImpl <em>Enumerated All Values From</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.EnumeratedAllValuesFromImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getEnumeratedAllValuesFrom()
   * @generated
   */
  int ENUMERATED_ALL_VALUES_FROM = 35;

  /**
   * The feature id for the '<em><b>Restricted</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ENUMERATED_ALL_VALUES_FROM__RESTRICTED = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Enumeration</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ENUMERATED_ALL_VALUES_FROM__ENUMERATION = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Enumerated All Values From</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ENUMERATED_ALL_VALUES_FROM_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.EnumeratedAllAndSomeValuesFromImpl <em>Enumerated All And Some Values From</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.EnumeratedAllAndSomeValuesFromImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getEnumeratedAllAndSomeValuesFrom()
   * @generated
   */
  int ENUMERATED_ALL_AND_SOME_VALUES_FROM = 36;

  /**
   * The feature id for the '<em><b>Restricted</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ENUMERATED_ALL_AND_SOME_VALUES_FROM__RESTRICTED = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Enumeration</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ENUMERATED_ALL_AND_SOME_VALUES_FROM__ENUMERATION = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Enumerated All And Some Values From</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ENUMERATED_ALL_AND_SOME_VALUES_FROM_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.DefaultValueImpl <em>Default Value</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.DefaultValueImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getDefaultValue()
   * @generated
   */
  int DEFAULT_VALUE = 37;

  /**
   * The feature id for the '<em><b>Def Value Class</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DEFAULT_VALUE__DEF_VALUE_CLASS = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Level</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DEFAULT_VALUE__LEVEL = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Def Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DEFAULT_VALUE__DEF_VALUE = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Default Value</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DEFAULT_VALUE_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.SomeValuesConditionImpl <em>Some Values Condition</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.SomeValuesConditionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSomeValuesCondition()
   * @generated
   */
  int SOME_VALUES_CONDITION = 38;

  /**
   * The feature id for the '<em><b>Restriction</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SOME_VALUES_CONDITION__RESTRICTION = CONDITION_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Some Values Condition</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SOME_VALUES_CONDITION_FEATURE_COUNT = CONDITION_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.HasValueConditionImpl <em>Has Value Condition</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.HasValueConditionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getHasValueCondition()
   * @generated
   */
  int HAS_VALUE_CONDITION = 39;

  /**
   * The feature id for the '<em><b>Restriction</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int HAS_VALUE_CONDITION__RESTRICTION = CONDITION_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Has Value Condition</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int HAS_VALUE_CONDITION_FEATURE_COUNT = CONDITION_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.MinCardConditionImpl <em>Min Card Condition</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.MinCardConditionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getMinCardCondition()
   * @generated
   */
  int MIN_CARD_CONDITION = 40;

  /**
   * The feature id for the '<em><b>Card</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MIN_CARD_CONDITION__CARD = CONDITION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Class Qualifier</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MIN_CARD_CONDITION__CLASS_QUALIFIER = CONDITION_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Min Card Condition</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MIN_CARD_CONDITION_FEATURE_COUNT = CONDITION_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.MaxCardConditionImpl <em>Max Card Condition</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.MaxCardConditionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getMaxCardCondition()
   * @generated
   */
  int MAX_CARD_CONDITION = 41;

  /**
   * The feature id for the '<em><b>Card</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MAX_CARD_CONDITION__CARD = CONDITION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Class Qualifier</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MAX_CARD_CONDITION__CLASS_QUALIFIER = CONDITION_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Max Card Condition</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MAX_CARD_CONDITION_FEATURE_COUNT = CONDITION_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.CardConditionImpl <em>Card Condition</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.CardConditionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getCardCondition()
   * @generated
   */
  int CARD_CONDITION = 42;

  /**
   * The feature id for the '<em><b>Card</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CARD_CONDITION__CARD = CONDITION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Class Qualifier</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CARD_CONDITION__CLASS_QUALIFIER = CONDITION_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Card Condition</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CARD_CONDITION_FEATURE_COUNT = CONDITION_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.NecessaryAndSufficientImpl <em>Necessary And Sufficient</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.NecessaryAndSufficientImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getNecessaryAndSufficient()
   * @generated
   */
  int NECESSARY_AND_SUFFICIENT = 43;

  /**
   * The feature id for the '<em><b>Super Class</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int NECESSARY_AND_SUFFICIENT__SUPER_CLASS = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Article</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int NECESSARY_AND_SUFFICIENT__ARTICLE = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Sub Class</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int NECESSARY_AND_SUFFICIENT__SUB_CLASS = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int NECESSARY_AND_SUFFICIENT__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Cond</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int NECESSARY_AND_SUFFICIENT__COND = STATEMENT_FEATURE_COUNT + 4;

  /**
   * The number of structural features of the '<em>Necessary And Sufficient</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int NECESSARY_AND_SUFFICIENT_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 5;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.PropertyDeclarationImpl <em>Property Declaration</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.PropertyDeclarationImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getPropertyDeclaration()
   * @generated
   */
  int PROPERTY_DECLARATION = 45;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_DECLARATION__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Super Prop Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_DECLARATION__SUPER_PROP_NAME = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Addl Prop Info</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_DECLARATION__ADDL_PROP_INFO = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Article</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_DECLARATION__ARTICLE = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Domain</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_DECLARATION__DOMAIN = STATEMENT_FEATURE_COUNT + 4;

  /**
   * The feature id for the '<em><b>Range Resource</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_DECLARATION__RANGE_RESOURCE = STATEMENT_FEATURE_COUNT + 5;

  /**
   * The feature id for the '<em><b>Annotation Property</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_DECLARATION__ANNOTATION_PROPERTY = STATEMENT_FEATURE_COUNT + 6;

  /**
   * The number of structural features of the '<em>Property Declaration</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_DECLARATION_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 7;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl <em>Additional Property Info</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getAdditionalPropertyInfo()
   * @generated
   */
  int ADDITIONAL_PROPERTY_INFO = 46;

  /**
   * The feature id for the '<em><b>Domain</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDITIONAL_PROPERTY_INFO__DOMAIN = 0;

  /**
   * The feature id for the '<em><b>Cond</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDITIONAL_PROPERTY_INFO__COND = 1;

  /**
   * The feature id for the '<em><b>Range</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDITIONAL_PROPERTY_INFO__RANGE = 2;

  /**
   * The feature id for the '<em><b>Isfunc</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDITIONAL_PROPERTY_INFO__ISFUNC = 3;

  /**
   * The feature id for the '<em><b>Isinvfunc</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDITIONAL_PROPERTY_INFO__ISINVFUNC = 4;

  /**
   * The feature id for the '<em><b>Is Sym</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDITIONAL_PROPERTY_INFO__IS_SYM = 5;

  /**
   * The feature id for the '<em><b>Is Trans</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDITIONAL_PROPERTY_INFO__IS_TRANS = 6;

  /**
   * The feature id for the '<em><b>Is Inv Of</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDITIONAL_PROPERTY_INFO__IS_INV_OF = 7;

  /**
   * The number of structural features of the '<em>Additional Property Info</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ADDITIONAL_PROPERTY_INFO_FEATURE_COUNT = 8;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.FunctionalPropertyImpl <em>Functional Property</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.FunctionalPropertyImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getFunctionalProperty()
   * @generated
   */
  int FUNCTIONAL_PROPERTY = 47;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FUNCTIONAL_PROPERTY__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Functional Property</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FUNCTIONAL_PROPERTY_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.InverseFunctionalPropertyImpl <em>Inverse Functional Property</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.InverseFunctionalPropertyImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInverseFunctionalProperty()
   * @generated
   */
  int INVERSE_FUNCTIONAL_PROPERTY = 48;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INVERSE_FUNCTIONAL_PROPERTY__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Inverse Functional Property</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INVERSE_FUNCTIONAL_PROPERTY_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.SymmetricalPropertyImpl <em>Symmetrical Property</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.SymmetricalPropertyImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSymmetricalProperty()
   * @generated
   */
  int SYMMETRICAL_PROPERTY = 49;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SYMMETRICAL_PROPERTY__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Symmetrical Property</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SYMMETRICAL_PROPERTY_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.TransitivePropertyImpl <em>Transitive Property</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.TransitivePropertyImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getTransitiveProperty()
   * @generated
   */
  int TRANSITIVE_PROPERTY = 50;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TRANSITIVE_PROPERTY__PROPERTY_NAME = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Transitive Property</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TRANSITIVE_PROPERTY_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.InversePropertyImpl <em>Inverse Property</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.InversePropertyImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInverseProperty()
   * @generated
   */
  int INVERSE_PROPERTY = 51;

  /**
   * The feature id for the '<em><b>Property Name1</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INVERSE_PROPERTY__PROPERTY_NAME1 = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Inv Of</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INVERSE_PROPERTY__INV_OF = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Inverse Property</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INVERSE_PROPERTY_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.IsInverseOfImpl <em>Is Inverse Of</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.IsInverseOfImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getIsInverseOf()
   * @generated
   */
  int IS_INVERSE_OF = 52;

  /**
   * The feature id for the '<em><b>Property Name2</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int IS_INVERSE_OF__PROPERTY_NAME2 = 0;

  /**
   * The number of structural features of the '<em>Is Inverse Of</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int IS_INVERSE_OF_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.TypedBNodeImpl <em>Typed BNode</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.TypedBNodeImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getTypedBNode()
   * @generated
   */
  int TYPED_BNODE = 53;

  /**
   * The feature id for the '<em><b>Article</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TYPED_BNODE__ARTICLE = 0;

  /**
   * The feature id for the '<em><b>Class Identifier</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TYPED_BNODE__CLASS_IDENTIFIER = 1;

  /**
   * The number of structural features of the '<em>Typed BNode</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TYPED_BNODE_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.InstanceDeclarationStatementImpl <em>Instance Declaration Statement</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.InstanceDeclarationStatementImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstanceDeclarationStatement()
   * @generated
   */
  int INSTANCE_DECLARATION_STATEMENT = 54;

  /**
   * The number of structural features of the '<em>Instance Declaration Statement</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCE_DECLARATION_STATEMENT_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.InstanceDeclarationImpl <em>Instance Declaration</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.InstanceDeclarationImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstanceDeclaration()
   * @generated
   */
  int INSTANCE_DECLARATION = 55;

  /**
   * The feature id for the '<em><b>Type Decl</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCE_DECLARATION__TYPE_DECL = INSTANCE_DECLARATION_STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Addl Info Items</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCE_DECLARATION__ADDL_INFO_ITEMS = INSTANCE_DECLARATION_STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Article</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCE_DECLARATION__ARTICLE = INSTANCE_DECLARATION_STATEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCE_DECLARATION__CLASS_NAME = INSTANCE_DECLARATION_STATEMENT_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Instance Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCE_DECLARATION__INSTANCE_NAME = INSTANCE_DECLARATION_STATEMENT_FEATURE_COUNT + 4;

  /**
   * The number of structural features of the '<em>Instance Declaration</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCE_DECLARATION_FEATURE_COUNT = INSTANCE_DECLARATION_STATEMENT_FEATURE_COUNT + 5;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.TypeDeclarationImpl <em>Type Declaration</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.TypeDeclarationImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getTypeDeclaration()
   * @generated
   */
  int TYPE_DECLARATION = 56;

  /**
   * The feature id for the '<em><b>Inst Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TYPE_DECLARATION__INST_NAME = 0;

  /**
   * The feature id for the '<em><b>Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TYPE_DECLARATION__TYPE = 1;

  /**
   * The number of structural features of the '<em>Type Declaration</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TYPE_DECLARATION_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.InstanceDifferentFromImpl <em>Instance Different From</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.InstanceDifferentFromImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstanceDifferentFrom()
   * @generated
   */
  int INSTANCE_DIFFERENT_FROM = 57;

  /**
   * The feature id for the '<em><b>Inst Name1</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCE_DIFFERENT_FROM__INST_NAME1 = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Inst Name2</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCE_DIFFERENT_FROM__INST_NAME2 = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Instance Different From</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCE_DIFFERENT_FROM_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.InstancesAllDifferentImpl <em>Instances All Different</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.InstancesAllDifferentImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstancesAllDifferent()
   * @generated
   */
  int INSTANCES_ALL_DIFFERENT = 58;

  /**
   * The feature id for the '<em><b>Instances</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCES_ALL_DIFFERENT__INSTANCES = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Instances All Different</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INSTANCES_ALL_DIFFERENT_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ExistingInstanceAttributionImpl <em>Existing Instance Attribution</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ExistingInstanceAttributionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExistingInstanceAttribution()
   * @generated
   */
  int EXISTING_INSTANCE_ATTRIBUTION = 59;

  /**
   * The feature id for the '<em><b>Subj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXISTING_INSTANCE_ATTRIBUTION__SUBJ = STATEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Addl Info Items</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXISTING_INSTANCE_ATTRIBUTION__ADDL_INFO_ITEMS = STATEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>POf S</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXISTING_INSTANCE_ATTRIBUTION__POF_S = STATEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Obj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXISTING_INSTANCE_ATTRIBUTION__OBJ = STATEMENT_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Existing Instance Attribution</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXISTING_INSTANCE_ATTRIBUTION_FEATURE_COUNT = STATEMENT_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ObjectImpl <em>Object</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ObjectImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getObject()
   * @generated
   */
  int OBJECT = 60;

  /**
   * The feature id for the '<em><b>Val</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OBJECT__VAL = 0;

  /**
   * The number of structural features of the '<em>Object</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OBJECT_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.PropValPartialTripleImpl <em>Prop Val Partial Triple</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.PropValPartialTripleImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getPropValPartialTriple()
   * @generated
   */
  int PROP_VAL_PARTIAL_TRIPLE = 61;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME = 0;

  /**
   * The feature id for the '<em><b>Object Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE = 1;

  /**
   * The feature id for the '<em><b>Object Value BNode</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE = 2;

  /**
   * The number of structural features of the '<em>Prop Val Partial Triple</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROP_VAL_PARTIAL_TRIPLE_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.OfPatternReturningValuesImpl <em>Of Pattern Returning Values</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.OfPatternReturningValuesImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getOfPatternReturningValues()
   * @generated
   */
  int OF_PATTERN_RETURNING_VALUES = 62;

  /**
   * The feature id for the '<em><b>Ofphrs</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OF_PATTERN_RETURNING_VALUES__OFPHRS = 0;

  /**
   * The feature id for the '<em><b>Subject</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OF_PATTERN_RETURNING_VALUES__SUBJECT = 1;

  /**
   * The feature id for the '<em><b>Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OF_PATTERN_RETURNING_VALUES__TYPE = 2;

  /**
   * The number of structural features of the '<em>Of Pattern Returning Values</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OF_PATTERN_RETURNING_VALUES_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.WithChainImpl <em>With Chain</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.WithChainImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getWithChain()
   * @generated
   */
  int WITH_CHAIN = 63;

  /**
   * The feature id for the '<em><b>Wps</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int WITH_CHAIN__WPS = 0;

  /**
   * The number of structural features of the '<em>With Chain</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int WITH_CHAIN_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.WithPhraseImpl <em>With Phrase</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.WithPhraseImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getWithPhrase()
   * @generated
   */
  int WITH_PHRASE = 64;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int WITH_PHRASE__PROPERTY_NAME = 0;

  /**
   * The feature id for the '<em><b>Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int WITH_PHRASE__VALUE = 1;

  /**
   * The number of structural features of the '<em>With Phrase</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int WITH_PHRASE_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.EmbeddedInstanceDeclarationImpl <em>Embedded Instance Declaration</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.EmbeddedInstanceDeclarationImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getEmbeddedInstanceDeclaration()
   * @generated
   */
  int EMBEDDED_INSTANCE_DECLARATION = 65;

  /**
   * The number of structural features of the '<em>Embedded Instance Declaration</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EMBEDDED_INSTANCE_DECLARATION_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.GraphPatternImpl <em>Graph Pattern</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.GraphPatternImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getGraphPattern()
   * @generated
   */
  int GRAPH_PATTERN = 82;

  /**
   * The number of structural features of the '<em>Graph Pattern</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRAPH_PATTERN_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.MergedTriplesImpl <em>Merged Triples</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.MergedTriplesImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getMergedTriples()
   * @generated
   */
  int MERGED_TRIPLES = 66;

  /**
   * The feature id for the '<em><b>Ops</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MERGED_TRIPLES__OPS = GRAPH_PATTERN_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Pivot</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MERGED_TRIPLES__PIVOT = GRAPH_PATTERN_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Wcs</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MERGED_TRIPLES__WCS = GRAPH_PATTERN_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Merged Triples</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MERGED_TRIPLES_FEATURE_COUNT = GRAPH_PATTERN_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.OfPhraseImpl <em>Of Phrase</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.OfPhraseImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getOfPhrase()
   * @generated
   */
  int OF_PHRASE = 67;

  /**
   * The feature id for the '<em><b>Article</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OF_PHRASE__ARTICLE = 0;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OF_PHRASE__PROPERTY_NAME = 1;

  /**
   * The number of structural features of the '<em>Of Phrase</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OF_PHRASE_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.VariableListImpl <em>Variable List</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.VariableListImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getVariableList()
   * @generated
   */
  int VARIABLE_LIST = 68;

  /**
   * The feature id for the '<em><b>Names</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int VARIABLE_LIST__NAMES = 0;

  /**
   * The number of structural features of the '<em>Variable List</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int VARIABLE_LIST_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.RuleImpl <em>Rule</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.RuleImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getRule()
   * @generated
   */
  int RULE = 69;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RULE__NAME = MODEL_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Givens</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RULE__GIVENS = MODEL_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Ifs</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RULE__IFS = MODEL_ELEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Thens</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RULE__THENS = MODEL_ELEMENT_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Rule</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int RULE_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.QueryImpl <em>Query</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.QueryImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getQuery()
   * @generated
   */
  int QUERY = 70;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int QUERY__EXPR = MODEL_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Query</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int QUERY_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.TestImpl <em>Test</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.TestImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getTest()
   * @generated
   */
  int TEST = 71;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TEST__EXPR = MODEL_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Test</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TEST_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ExprImpl <em>Expr</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ExprImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExpr()
   * @generated
   */
  int EXPR = 72;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPR__EXPR = MODEL_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Expr</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPR_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.DisplayImpl <em>Display</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.DisplayImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getDisplay()
   * @generated
   */
  int DISPLAY = 73;

  /**
   * The feature id for the '<em><b>Display String</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DISPLAY__DISPLAY_STRING = MODEL_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Model</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DISPLAY__MODEL = MODEL_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Display</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DISPLAY_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ExplanationImpl <em>Explanation</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ExplanationImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExplanation()
   * @generated
   */
  int EXPLANATION = 74;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPLANATION__EXPR = MODEL_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Rulename</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPLANATION__RULENAME = MODEL_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Explanation</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPLANATION_FEATURE_COUNT = MODEL_ELEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ElementSetImpl <em>Element Set</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ElementSetImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getElementSet()
   * @generated
   */
  int ELEMENT_SET = 75;

  /**
   * The feature id for the '<em><b>Elements</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ELEMENT_SET__ELEMENTS = 0;

  /**
   * The number of structural features of the '<em>Element Set</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ELEMENT_SET_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ExpressionImpl <em>Expression</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ExpressionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExpression()
   * @generated
   */
  int EXPRESSION = 81;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPRESSION__EXPR = 0;

  /**
   * The feature id for the '<em><b>Func</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPRESSION__FUNC = 1;

  /**
   * The feature id for the '<em><b>Args</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPRESSION__ARGS = 2;

  /**
   * The feature id for the '<em><b>Gp</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPRESSION__GP = 3;

  /**
   * The feature id for the '<em><b>Ivalue</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPRESSION__IVALUE = 4;

  /**
   * The feature id for the '<em><b>Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPRESSION__VALUE = 5;

  /**
   * The feature id for the '<em><b>Value Table</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPRESSION__VALUE_TABLE = 6;

  /**
   * The number of structural features of the '<em>Expression</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPRESSION_FEATURE_COUNT = 7;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.SelectExpressionImpl <em>Select Expression</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.SelectExpressionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSelectExpression()
   * @generated
   */
  int SELECT_EXPRESSION = 76;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__EXPR = EXPRESSION__EXPR;

  /**
   * The feature id for the '<em><b>Func</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__FUNC = EXPRESSION__FUNC;

  /**
   * The feature id for the '<em><b>Args</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__ARGS = EXPRESSION__ARGS;

  /**
   * The feature id for the '<em><b>Gp</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__GP = EXPRESSION__GP;

  /**
   * The feature id for the '<em><b>Ivalue</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__IVALUE = EXPRESSION__IVALUE;

  /**
   * The feature id for the '<em><b>Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__VALUE = EXPRESSION__VALUE;

  /**
   * The feature id for the '<em><b>Value Table</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__VALUE_TABLE = EXPRESSION__VALUE_TABLE;

  /**
   * The feature id for the '<em><b>Distinct</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__DISTINCT = EXPRESSION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>All Vars</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__ALL_VARS = EXPRESSION_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Var List</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__VAR_LIST = EXPRESSION_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Orderby</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__ORDERBY = EXPRESSION_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Order List</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION__ORDER_LIST = EXPRESSION_FEATURE_COUNT + 4;

  /**
   * The number of structural features of the '<em>Select Expression</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SELECT_EXPRESSION_FEATURE_COUNT = EXPRESSION_FEATURE_COUNT + 5;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ConstructExpressionImpl <em>Construct Expression</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ConstructExpressionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getConstructExpression()
   * @generated
   */
  int CONSTRUCT_EXPRESSION = 77;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCT_EXPRESSION__EXPR = EXPRESSION__EXPR;

  /**
   * The feature id for the '<em><b>Func</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCT_EXPRESSION__FUNC = EXPRESSION__FUNC;

  /**
   * The feature id for the '<em><b>Args</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCT_EXPRESSION__ARGS = EXPRESSION__ARGS;

  /**
   * The feature id for the '<em><b>Gp</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCT_EXPRESSION__GP = EXPRESSION__GP;

  /**
   * The feature id for the '<em><b>Ivalue</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCT_EXPRESSION__IVALUE = EXPRESSION__IVALUE;

  /**
   * The feature id for the '<em><b>Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCT_EXPRESSION__VALUE = EXPRESSION__VALUE;

  /**
   * The feature id for the '<em><b>Value Table</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCT_EXPRESSION__VALUE_TABLE = EXPRESSION__VALUE_TABLE;

  /**
   * The feature id for the '<em><b>Subj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCT_EXPRESSION__SUBJ = EXPRESSION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Pred</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCT_EXPRESSION__PRED = EXPRESSION_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Obj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCT_EXPRESSION__OBJ = EXPRESSION_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Construct Expression</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCT_EXPRESSION_FEATURE_COUNT = EXPRESSION_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.AskQueryExpressionImpl <em>Ask Query Expression</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.AskQueryExpressionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getAskQueryExpression()
   * @generated
   */
  int ASK_QUERY_EXPRESSION = 78;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ASK_QUERY_EXPRESSION__EXPR = EXPRESSION__EXPR;

  /**
   * The feature id for the '<em><b>Func</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ASK_QUERY_EXPRESSION__FUNC = EXPRESSION__FUNC;

  /**
   * The feature id for the '<em><b>Args</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ASK_QUERY_EXPRESSION__ARGS = EXPRESSION__ARGS;

  /**
   * The feature id for the '<em><b>Gp</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ASK_QUERY_EXPRESSION__GP = EXPRESSION__GP;

  /**
   * The feature id for the '<em><b>Ivalue</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ASK_QUERY_EXPRESSION__IVALUE = EXPRESSION__IVALUE;

  /**
   * The feature id for the '<em><b>Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ASK_QUERY_EXPRESSION__VALUE = EXPRESSION__VALUE;

  /**
   * The feature id for the '<em><b>Value Table</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ASK_QUERY_EXPRESSION__VALUE_TABLE = EXPRESSION__VALUE_TABLE;

  /**
   * The number of structural features of the '<em>Ask Query Expression</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ASK_QUERY_EXPRESSION_FEATURE_COUNT = EXPRESSION_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.OrderListImpl <em>Order List</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.OrderListImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getOrderList()
   * @generated
   */
  int ORDER_LIST = 79;

  /**
   * The feature id for the '<em><b>Order List</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ORDER_LIST__ORDER_LIST = 0;

  /**
   * The number of structural features of the '<em>Order List</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ORDER_LIST_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.OrderElementImpl <em>Order Element</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.OrderElementImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getOrderElement()
   * @generated
   */
  int ORDER_ELEMENT = 80;

  /**
   * The feature id for the '<em><b>Order</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ORDER_ELEMENT__ORDER = 0;

  /**
   * The feature id for the '<em><b>Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ORDER_ELEMENT__NAME = 1;

  /**
   * The number of structural features of the '<em>Order Element</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ORDER_ELEMENT_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.PropOfSubjImpl <em>Prop Of Subj</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.PropOfSubjImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getPropOfSubj()
   * @generated
   */
  int PROP_OF_SUBJ = 83;

  /**
   * The feature id for the '<em><b>Of Phr</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROP_OF_SUBJ__OF_PHR = GRAPH_PATTERN_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Subj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROP_OF_SUBJ__SUBJ = GRAPH_PATTERN_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Prop Of Subj</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROP_OF_SUBJ_FEATURE_COUNT = GRAPH_PATTERN_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.SubjPropImpl <em>Subj Prop</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.SubjPropImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSubjProp()
   * @generated
   */
  int SUBJ_PROP = 84;

  /**
   * The feature id for the '<em><b>Subj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SUBJ_PROP__SUBJ = GRAPH_PATTERN_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Hw Phr</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SUBJ_PROP__HW_PHR = GRAPH_PATTERN_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Subj Prop</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SUBJ_PROP_FEATURE_COUNT = GRAPH_PATTERN_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.InstAttrSPVImpl <em>Inst Attr SPV</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.InstAttrSPVImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstAttrSPV()
   * @generated
   */
  int INST_ATTR_SPV = 85;

  /**
   * The feature id for the '<em><b>Subj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INST_ATTR_SPV__SUBJ = GRAPH_PATTERN_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Props</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INST_ATTR_SPV__PROPS = GRAPH_PATTERN_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Vals</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INST_ATTR_SPV__VALS = GRAPH_PATTERN_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Inst Attr SPV</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INST_ATTR_SPV_FEATURE_COUNT = GRAPH_PATTERN_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.InstAttrPSVImpl <em>Inst Attr PSV</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.InstAttrPSVImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstAttrPSV()
   * @generated
   */
  int INST_ATTR_PSV = 86;

  /**
   * The feature id for the '<em><b>Prop</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INST_ATTR_PSV__PROP = GRAPH_PATTERN_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Val</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INST_ATTR_PSV__VAL = GRAPH_PATTERN_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Inst Attr PSV</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INST_ATTR_PSV_FEATURE_COUNT = GRAPH_PATTERN_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.SubTypeOfImpl <em>Sub Type Of</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.SubTypeOfImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSubTypeOf()
   * @generated
   */
  int SUB_TYPE_OF = 87;

  /**
   * The feature id for the '<em><b>Subclass</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SUB_TYPE_OF__SUBCLASS = GRAPH_PATTERN_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Superclass</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SUB_TYPE_OF__SUPERCLASS = GRAPH_PATTERN_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Sub Type Of</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SUB_TYPE_OF_FEATURE_COUNT = GRAPH_PATTERN_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.IntervalValueImpl <em>Interval Value</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.IntervalValueImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getIntervalValue()
   * @generated
   */
  int INTERVAL_VALUE = 88;

  /**
   * The feature id for the '<em><b>Op</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INTERVAL_VALUE__OP = 0;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INTERVAL_VALUE__EXPR = 1;

  /**
   * The number of structural features of the '<em>Interval Value</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INTERVAL_VALUE_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ExplicitValueImpl <em>Explicit Value</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ExplicitValueImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExplicitValue()
   * @generated
   */
  int EXPLICIT_VALUE = 89;

  /**
   * The feature id for the '<em><b>Inst Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPLICIT_VALUE__INST_NAME = 0;

  /**
   * The feature id for the '<em><b>Lit Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPLICIT_VALUE__LIT_VALUE = 1;

  /**
   * The feature id for the '<em><b>Term</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPLICIT_VALUE__TERM = 2;

  /**
   * The number of structural features of the '<em>Explicit Value</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXPLICIT_VALUE_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ValueTableImpl <em>Value Table</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ValueTableImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getValueTable()
   * @generated
   */
  int VALUE_TABLE = 90;

  /**
   * The feature id for the '<em><b>Row</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int VALUE_TABLE__ROW = 0;

  /**
   * The feature id for the '<em><b>Rows</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int VALUE_TABLE__ROWS = 1;

  /**
   * The number of structural features of the '<em>Value Table</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int VALUE_TABLE_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.LiteralValueImpl <em>Literal Value</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.LiteralValueImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getLiteralValue()
   * @generated
   */
  int LITERAL_VALUE = 91;

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
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.ValueRowImpl <em>Value Row</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.ValueRowImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getValueRow()
   * @generated
   */
  int VALUE_ROW = 92;

  /**
   * The feature id for the '<em><b>Explicit Values</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int VALUE_ROW__EXPLICIT_VALUES = 0;

  /**
   * The number of structural features of the '<em>Value Row</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int VALUE_ROW_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.JunctionExpressionImpl <em>Junction Expression</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.JunctionExpressionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getJunctionExpression()
   * @generated
   */
  int JUNCTION_EXPRESSION = 93;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int JUNCTION_EXPRESSION__EXPR = EXPRESSION__EXPR;

  /**
   * The feature id for the '<em><b>Func</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int JUNCTION_EXPRESSION__FUNC = EXPRESSION__FUNC;

  /**
   * The feature id for the '<em><b>Args</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int JUNCTION_EXPRESSION__ARGS = EXPRESSION__ARGS;

  /**
   * The feature id for the '<em><b>Gp</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int JUNCTION_EXPRESSION__GP = EXPRESSION__GP;

  /**
   * The feature id for the '<em><b>Ivalue</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int JUNCTION_EXPRESSION__IVALUE = EXPRESSION__IVALUE;

  /**
   * The feature id for the '<em><b>Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int JUNCTION_EXPRESSION__VALUE = EXPRESSION__VALUE;

  /**
   * The feature id for the '<em><b>Value Table</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int JUNCTION_EXPRESSION__VALUE_TABLE = EXPRESSION__VALUE_TABLE;

  /**
   * The feature id for the '<em><b>Left</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int JUNCTION_EXPRESSION__LEFT = EXPRESSION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Op</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int JUNCTION_EXPRESSION__OP = EXPRESSION_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Right</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int JUNCTION_EXPRESSION__RIGHT = EXPRESSION_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Junction Expression</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int JUNCTION_EXPRESSION_FEATURE_COUNT = EXPRESSION_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.BinaryOpExpressionImpl <em>Binary Op Expression</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.BinaryOpExpressionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getBinaryOpExpression()
   * @generated
   */
  int BINARY_OP_EXPRESSION = 94;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINARY_OP_EXPRESSION__EXPR = EXPRESSION__EXPR;

  /**
   * The feature id for the '<em><b>Func</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINARY_OP_EXPRESSION__FUNC = EXPRESSION__FUNC;

  /**
   * The feature id for the '<em><b>Args</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINARY_OP_EXPRESSION__ARGS = EXPRESSION__ARGS;

  /**
   * The feature id for the '<em><b>Gp</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINARY_OP_EXPRESSION__GP = EXPRESSION__GP;

  /**
   * The feature id for the '<em><b>Ivalue</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINARY_OP_EXPRESSION__IVALUE = EXPRESSION__IVALUE;

  /**
   * The feature id for the '<em><b>Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINARY_OP_EXPRESSION__VALUE = EXPRESSION__VALUE;

  /**
   * The feature id for the '<em><b>Value Table</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINARY_OP_EXPRESSION__VALUE_TABLE = EXPRESSION__VALUE_TABLE;

  /**
   * The feature id for the '<em><b>Left</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINARY_OP_EXPRESSION__LEFT = EXPRESSION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Op</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINARY_OP_EXPRESSION__OP = EXPRESSION_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Right</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINARY_OP_EXPRESSION__RIGHT = EXPRESSION_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Binary Op Expression</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINARY_OP_EXPRESSION_FEATURE_COUNT = EXPRESSION_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.impl.UnaryOpExpressionImpl <em>Unary Op Expression</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.impl.UnaryOpExpressionImpl
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getUnaryOpExpression()
   * @generated
   */
  int UNARY_OP_EXPRESSION = 95;

  /**
   * The feature id for the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNARY_OP_EXPRESSION__EXPR = EXPRESSION__EXPR;

  /**
   * The feature id for the '<em><b>Func</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNARY_OP_EXPRESSION__FUNC = EXPRESSION__FUNC;

  /**
   * The feature id for the '<em><b>Args</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNARY_OP_EXPRESSION__ARGS = EXPRESSION__ARGS;

  /**
   * The feature id for the '<em><b>Gp</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNARY_OP_EXPRESSION__GP = EXPRESSION__GP;

  /**
   * The feature id for the '<em><b>Ivalue</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNARY_OP_EXPRESSION__IVALUE = EXPRESSION__IVALUE;

  /**
   * The feature id for the '<em><b>Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNARY_OP_EXPRESSION__VALUE = EXPRESSION__VALUE;

  /**
   * The feature id for the '<em><b>Value Table</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNARY_OP_EXPRESSION__VALUE_TABLE = EXPRESSION__VALUE_TABLE;

  /**
   * The feature id for the '<em><b>Op</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNARY_OP_EXPRESSION__OP = EXPRESSION_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Unary Op Expression</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int UNARY_OP_EXPRESSION_FEATURE_COUNT = EXPRESSION_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.sadl.DataType <em>Data Type</em>}' enum.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.sadl.DataType
   * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getDataType()
   * @generated
   */
  int DATA_TYPE = 96;


  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Model <em>Model</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Model</em>'.
   * @see com.ge.research.sadl.sadl.Model
   * @generated
   */
  EClass getModel();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Model#getModelName <em>Model Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Model Name</em>'.
   * @see com.ge.research.sadl.sadl.Model#getModelName()
   * @see #getModel()
   * @generated
   */
  EReference getModel_ModelName();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.Model#getImports <em>Imports</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Imports</em>'.
   * @see com.ge.research.sadl.sadl.Model#getImports()
   * @see #getModel()
   * @generated
   */
  EReference getModel_Imports();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.Model#getElements <em>Elements</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Elements</em>'.
   * @see com.ge.research.sadl.sadl.Model#getElements()
   * @see #getModel()
   * @generated
   */
  EReference getModel_Elements();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ModelName <em>Model Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Model Name</em>'.
   * @see com.ge.research.sadl.sadl.ModelName
   * @generated
   */
  EClass getModelName();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.ModelName#getBaseUri <em>Base Uri</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Base Uri</em>'.
   * @see com.ge.research.sadl.sadl.ModelName#getBaseUri()
   * @see #getModelName()
   * @generated
   */
  EAttribute getModelName_BaseUri();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.ModelName#getAlias <em>Alias</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Alias</em>'.
   * @see com.ge.research.sadl.sadl.ModelName#getAlias()
   * @see #getModelName()
   * @generated
   */
  EAttribute getModelName_Alias();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.ModelName#getVersion <em>Version</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Version</em>'.
   * @see com.ge.research.sadl.sadl.ModelName#getVersion()
   * @see #getModelName()
   * @generated
   */
  EAttribute getModelName_Version();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Import <em>Import</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Import</em>'.
   * @see com.ge.research.sadl.sadl.Import
   * @generated
   */
  EClass getImport();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Import#getImportURI <em>Import URI</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Import URI</em>'.
   * @see com.ge.research.sadl.sadl.Import#getImportURI()
   * @see #getImport()
   * @generated
   */
  EAttribute getImport_ImportURI();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Import#getAlias <em>Alias</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Alias</em>'.
   * @see com.ge.research.sadl.sadl.Import#getAlias()
   * @see #getImport()
   * @generated
   */
  EAttribute getImport_Alias();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ModelElement <em>Model Element</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Model Element</em>'.
   * @see com.ge.research.sadl.sadl.ModelElement
   * @generated
   */
  EClass getModelElement();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Statement <em>Statement</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Statement</em>'.
   * @see com.ge.research.sadl.sadl.Statement
   * @generated
   */
  EClass getStatement();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ResourceName <em>Resource Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Resource Name</em>'.
   * @see com.ge.research.sadl.sadl.ResourceName
   * @generated
   */
  EClass getResourceName();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.ResourceName#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see com.ge.research.sadl.sadl.ResourceName#getName()
   * @see #getResourceName()
   * @generated
   */
  EAttribute getResourceName_Name();

  /**
   * Returns the meta object for the attribute list '{@link com.ge.research.sadl.sadl.ResourceName#getAnnType <em>Ann Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute list '<em>Ann Type</em>'.
   * @see com.ge.research.sadl.sadl.ResourceName#getAnnType()
   * @see #getResourceName()
   * @generated
   */
  EAttribute getResourceName_AnnType();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.ResourceName#getAnnContent <em>Ann Content</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Ann Content</em>'.
   * @see com.ge.research.sadl.sadl.ResourceName#getAnnContent()
   * @see #getResourceName()
   * @generated
   */
  EReference getResourceName_AnnContent();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ContentList <em>Content List</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Content List</em>'.
   * @see com.ge.research.sadl.sadl.ContentList
   * @generated
   */
  EClass getContentList();

  /**
   * Returns the meta object for the attribute list '{@link com.ge.research.sadl.sadl.ContentList#getAnnContent <em>Ann Content</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute list '<em>Ann Content</em>'.
   * @see com.ge.research.sadl.sadl.ContentList#getAnnContent()
   * @see #getContentList()
   * @generated
   */
  EAttribute getContentList_AnnContent();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ResourceList <em>Resource List</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Resource List</em>'.
   * @see com.ge.research.sadl.sadl.ResourceList
   * @generated
   */
  EClass getResourceList();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.ResourceList#getNames <em>Names</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Names</em>'.
   * @see com.ge.research.sadl.sadl.ResourceList#getNames()
   * @see #getResourceList()
   * @generated
   */
  EReference getResourceList_Names();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.LiteralList <em>Literal List</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Literal List</em>'.
   * @see com.ge.research.sadl.sadl.LiteralList
   * @generated
   */
  EClass getLiteralList();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.LiteralList#getLiterals <em>Literals</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Literals</em>'.
   * @see com.ge.research.sadl.sadl.LiteralList#getLiterals()
   * @see #getLiteralList()
   * @generated
   */
  EReference getLiteralList_Literals();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ResourceByName <em>Resource By Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Resource By Name</em>'.
   * @see com.ge.research.sadl.sadl.ResourceByName
   * @generated
   */
  EClass getResourceByName();

  /**
   * Returns the meta object for the reference '{@link com.ge.research.sadl.sadl.ResourceByName#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference '<em>Name</em>'.
   * @see com.ge.research.sadl.sadl.ResourceByName#getName()
   * @see #getResourceByName()
   * @generated
   */
  EReference getResourceByName_Name();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ExistingResourceList <em>Existing Resource List</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Existing Resource List</em>'.
   * @see com.ge.research.sadl.sadl.ExistingResourceList
   * @generated
   */
  EClass getExistingResourceList();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.ExistingResourceList#getNames <em>Names</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Names</em>'.
   * @see com.ge.research.sadl.sadl.ExistingResourceList#getNames()
   * @see #getExistingResourceList()
   * @generated
   */
  EReference getExistingResourceList_Names();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ResourceIdentifier <em>Resource Identifier</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Resource Identifier</em>'.
   * @see com.ge.research.sadl.sadl.ResourceIdentifier
   * @generated
   */
  EClass getResourceIdentifier();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ResourceBySetOp <em>Resource By Set Op</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Resource By Set Op</em>'.
   * @see com.ge.research.sadl.sadl.ResourceBySetOp
   * @generated
   */
  EClass getResourceBySetOp();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.ResourceBySetOp#getAnnType <em>Ann Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Ann Type</em>'.
   * @see com.ge.research.sadl.sadl.ResourceBySetOp#getAnnType()
   * @see #getResourceBySetOp()
   * @generated
   */
  EAttribute getResourceBySetOp_AnnType();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.ResourceBySetOp#getAnnContent <em>Ann Content</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Ann Content</em>'.
   * @see com.ge.research.sadl.sadl.ResourceBySetOp#getAnnContent()
   * @see #getResourceBySetOp()
   * @generated
   */
  EAttribute getResourceBySetOp_AnnContent();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.ResourceBySetOp#getNames <em>Names</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Names</em>'.
   * @see com.ge.research.sadl.sadl.ResourceBySetOp#getNames()
   * @see #getResourceBySetOp()
   * @generated
   */
  EReference getResourceBySetOp_Names();

  /**
   * Returns the meta object for the attribute list '{@link com.ge.research.sadl.sadl.ResourceBySetOp#getOp <em>Op</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute list '<em>Op</em>'.
   * @see com.ge.research.sadl.sadl.ResourceBySetOp#getOp()
   * @see #getResourceBySetOp()
   * @generated
   */
  EAttribute getResourceBySetOp_Op();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ResourceByRestriction <em>Resource By Restriction</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Resource By Restriction</em>'.
   * @see com.ge.research.sadl.sadl.ResourceByRestriction
   * @generated
   */
  EClass getResourceByRestriction();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.ResourceByRestriction#getAnnType <em>Ann Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Ann Type</em>'.
   * @see com.ge.research.sadl.sadl.ResourceByRestriction#getAnnType()
   * @see #getResourceByRestriction()
   * @generated
   */
  EAttribute getResourceByRestriction_AnnType();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.ResourceByRestriction#getAnnContent <em>Ann Content</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Ann Content</em>'.
   * @see com.ge.research.sadl.sadl.ResourceByRestriction#getAnnContent()
   * @see #getResourceByRestriction()
   * @generated
   */
  EAttribute getResourceByRestriction_AnnContent();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ResourceByRestriction#getPropName <em>Prop Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Prop Name</em>'.
   * @see com.ge.research.sadl.sadl.ResourceByRestriction#getPropName()
   * @see #getResourceByRestriction()
   * @generated
   */
  EReference getResourceByRestriction_PropName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ResourceByRestriction#getCond <em>Cond</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Cond</em>'.
   * @see com.ge.research.sadl.sadl.ResourceByRestriction#getCond()
   * @see #getResourceByRestriction()
   * @generated
   */
  EReference getResourceByRestriction_Cond();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.UnionResource <em>Union Resource</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Union Resource</em>'.
   * @see com.ge.research.sadl.sadl.UnionResource
   * @generated
   */
  EClass getUnionResource();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.IntersectionResource <em>Intersection Resource</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Intersection Resource</em>'.
   * @see com.ge.research.sadl.sadl.IntersectionResource
   * @generated
   */
  EClass getIntersectionResource();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ClassDeclaration <em>Class Declaration</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Class Declaration</em>'.
   * @see com.ge.research.sadl.sadl.ClassDeclaration
   * @generated
   */
  EClass getClassDeclaration();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ClassDeclaration#getClassName <em>Class Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Name</em>'.
   * @see com.ge.research.sadl.sadl.ClassDeclaration#getClassName()
   * @see #getClassDeclaration()
   * @generated
   */
  EReference getClassDeclaration_ClassName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ClassDeclaration#getMustBeOneOf <em>Must Be One Of</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Must Be One Of</em>'.
   * @see com.ge.research.sadl.sadl.ClassDeclaration#getMustBeOneOf()
   * @see #getClassDeclaration()
   * @generated
   */
  EReference getClassDeclaration_MustBeOneOf();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.ClassDeclaration#getDescribedBy <em>Described By</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Described By</em>'.
   * @see com.ge.research.sadl.sadl.ClassDeclaration#getDescribedBy()
   * @see #getClassDeclaration()
   * @generated
   */
  EReference getClassDeclaration_DescribedBy();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ClassDeclaration#getClassList <em>Class List</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class List</em>'.
   * @see com.ge.research.sadl.sadl.ClassDeclaration#getClassList()
   * @see #getClassDeclaration()
   * @generated
   */
  EReference getClassDeclaration_ClassList();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ClassDeclaration#getClassIdentifier <em>Class Identifier</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Identifier</em>'.
   * @see com.ge.research.sadl.sadl.ClassDeclaration#getClassIdentifier()
   * @see #getClassDeclaration()
   * @generated
   */
  EReference getClassDeclaration_ClassIdentifier();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.EnumeratedInstances <em>Enumerated Instances</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Enumerated Instances</em>'.
   * @see com.ge.research.sadl.sadl.EnumeratedInstances
   * @generated
   */
  EClass getEnumeratedInstances();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.EnumeratedInstances#getInstanceList <em>Instance List</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Instance List</em>'.
   * @see com.ge.research.sadl.sadl.EnumeratedInstances#getInstanceList()
   * @see #getEnumeratedInstances()
   * @generated
   */
  EReference getEnumeratedInstances_InstanceList();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.AddlClassInfo <em>Addl Class Info</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Addl Class Info</em>'.
   * @see com.ge.research.sadl.sadl.AddlClassInfo
   * @generated
   */
  EClass getAddlClassInfo();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AddlClassInfo#getPropertyByName <em>Property By Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property By Name</em>'.
   * @see com.ge.research.sadl.sadl.AddlClassInfo#getPropertyByName()
   * @see #getAddlClassInfo()
   * @generated
   */
  EReference getAddlClassInfo_PropertyByName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AddlClassInfo#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.AddlClassInfo#getPropertyName()
   * @see #getAddlClassInfo()
   * @generated
   */
  EReference getAddlClassInfo_PropertyName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AddlClassInfo#getRange <em>Range</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Range</em>'.
   * @see com.ge.research.sadl.sadl.AddlClassInfo#getRange()
   * @see #getAddlClassInfo()
   * @generated
   */
  EReference getAddlClassInfo_Range();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AddlClassInfo#getRestriction <em>Restriction</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restriction</em>'.
   * @see com.ge.research.sadl.sadl.AddlClassInfo#getRestriction()
   * @see #getAddlClassInfo()
   * @generated
   */
  EReference getAddlClassInfo_Restriction();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Range <em>Range</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Range</em>'.
   * @see com.ge.research.sadl.sadl.Range
   * @generated
   */
  EClass getRange();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Range#getSingle <em>Single</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Single</em>'.
   * @see com.ge.research.sadl.sadl.Range#getSingle()
   * @see #getRange()
   * @generated
   */
  EAttribute getRange_Single();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Range#getType <em>Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Type</em>'.
   * @see com.ge.research.sadl.sadl.Range#getType()
   * @see #getRange()
   * @generated
   */
  EReference getRange_Type();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.RangeType <em>Range Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Range Type</em>'.
   * @see com.ge.research.sadl.sadl.RangeType
   * @generated
   */
  EClass getRangeType();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.RangeType#getClassIdentifier <em>Class Identifier</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Identifier</em>'.
   * @see com.ge.research.sadl.sadl.RangeType#getClassIdentifier()
   * @see #getRangeType()
   * @generated
   */
  EReference getRangeType_ClassIdentifier();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.UserDefinedDataType <em>User Defined Data Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>User Defined Data Type</em>'.
   * @see com.ge.research.sadl.sadl.UserDefinedDataType
   * @generated
   */
  EClass getUserDefinedDataType();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.UserDefinedDataType#getUdt <em>Udt</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Udt</em>'.
   * @see com.ge.research.sadl.sadl.UserDefinedDataType#getUdt()
   * @see #getUserDefinedDataType()
   * @generated
   */
  EAttribute getUserDefinedDataType_Udt();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.UserDefinedDataType#getRestriction <em>Restriction</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restriction</em>'.
   * @see com.ge.research.sadl.sadl.UserDefinedDataType#getRestriction()
   * @see #getUserDefinedDataType()
   * @generated
   */
  EReference getUserDefinedDataType_Restriction();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.DataTypeRestriction <em>Data Type Restriction</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Data Type Restriction</em>'.
   * @see com.ge.research.sadl.sadl.DataTypeRestriction
   * @generated
   */
  EClass getDataTypeRestriction();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.DataTypeRestriction#getBasetype <em>Basetype</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Basetype</em>'.
   * @see com.ge.research.sadl.sadl.DataTypeRestriction#getBasetype()
   * @see #getDataTypeRestriction()
   * @generated
   */
  EAttribute getDataTypeRestriction_Basetype();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.DataTypeRestriction#getFacets <em>Facets</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Facets</em>'.
   * @see com.ge.research.sadl.sadl.DataTypeRestriction#getFacets()
   * @see #getDataTypeRestriction()
   * @generated
   */
  EReference getDataTypeRestriction_Facets();

  /**
   * Returns the meta object for the attribute list '{@link com.ge.research.sadl.sadl.DataTypeRestriction#getBasetypes <em>Basetypes</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute list '<em>Basetypes</em>'.
   * @see com.ge.research.sadl.sadl.DataTypeRestriction#getBasetypes()
   * @see #getDataTypeRestriction()
   * @generated
   */
  EAttribute getDataTypeRestriction_Basetypes();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Facets <em>Facets</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Facets</em>'.
   * @see com.ge.research.sadl.sadl.Facets
   * @generated
   */
  EClass getFacets();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Facets#getMinexin <em>Minexin</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Minexin</em>'.
   * @see com.ge.research.sadl.sadl.Facets#getMinexin()
   * @see #getFacets()
   * @generated
   */
  EAttribute getFacets_Minexin();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Facets#getMin <em>Min</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Min</em>'.
   * @see com.ge.research.sadl.sadl.Facets#getMin()
   * @see #getFacets()
   * @generated
   */
  EAttribute getFacets_Min();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Facets#getMax <em>Max</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Max</em>'.
   * @see com.ge.research.sadl.sadl.Facets#getMax()
   * @see #getFacets()
   * @generated
   */
  EAttribute getFacets_Max();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Facets#getMaxexin <em>Maxexin</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Maxexin</em>'.
   * @see com.ge.research.sadl.sadl.Facets#getMaxexin()
   * @see #getFacets()
   * @generated
   */
  EAttribute getFacets_Maxexin();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Facets#getRegex <em>Regex</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Regex</em>'.
   * @see com.ge.research.sadl.sadl.Facets#getRegex()
   * @see #getFacets()
   * @generated
   */
  EAttribute getFacets_Regex();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Facets#getLen <em>Len</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Len</em>'.
   * @see com.ge.research.sadl.sadl.Facets#getLen()
   * @see #getFacets()
   * @generated
   */
  EAttribute getFacets_Len();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Facets#getMinlen <em>Minlen</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Minlen</em>'.
   * @see com.ge.research.sadl.sadl.Facets#getMinlen()
   * @see #getFacets()
   * @generated
   */
  EAttribute getFacets_Minlen();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Facets#getMaxlen <em>Maxlen</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Maxlen</em>'.
   * @see com.ge.research.sadl.sadl.Facets#getMaxlen()
   * @see #getFacets()
   * @generated
   */
  EAttribute getFacets_Maxlen();

  /**
   * Returns the meta object for the attribute list '{@link com.ge.research.sadl.sadl.Facets#getValues <em>Values</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute list '<em>Values</em>'.
   * @see com.ge.research.sadl.sadl.Facets#getValues()
   * @see #getFacets()
   * @generated
   */
  EAttribute getFacets_Values();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.EquivalentConcepts <em>Equivalent Concepts</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Equivalent Concepts</em>'.
   * @see com.ge.research.sadl.sadl.EquivalentConcepts
   * @generated
   */
  EClass getEquivalentConcepts();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.EquivalentConcepts#getClass1 <em>Class1</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class1</em>'.
   * @see com.ge.research.sadl.sadl.EquivalentConcepts#getClass1()
   * @see #getEquivalentConcepts()
   * @generated
   */
  EReference getEquivalentConcepts_Class1();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.EquivalentConcepts#getClass2 <em>Class2</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class2</em>'.
   * @see com.ge.research.sadl.sadl.EquivalentConcepts#getClass2()
   * @see #getEquivalentConcepts()
   * @generated
   */
  EReference getEquivalentConcepts_Class2();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.DisjointClasses <em>Disjoint Classes</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Disjoint Classes</em>'.
   * @see com.ge.research.sadl.sadl.DisjointClasses
   * @generated
   */
  EClass getDisjointClasses();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.DisjointClasses#getClass1 <em>Class1</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class1</em>'.
   * @see com.ge.research.sadl.sadl.DisjointClasses#getClass1()
   * @see #getDisjointClasses()
   * @generated
   */
  EReference getDisjointClasses_Class1();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.DisjointClasses#getClass2 <em>Class2</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class2</em>'.
   * @see com.ge.research.sadl.sadl.DisjointClasses#getClass2()
   * @see #getDisjointClasses()
   * @generated
   */
  EReference getDisjointClasses_Class2();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.DisjointClasses#getClasses <em>Classes</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Classes</em>'.
   * @see com.ge.research.sadl.sadl.DisjointClasses#getClasses()
   * @see #getDisjointClasses()
   * @generated
   */
  EReference getDisjointClasses_Classes();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ComplementOfClass <em>Complement Of Class</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Complement Of Class</em>'.
   * @see com.ge.research.sadl.sadl.ComplementOfClass
   * @generated
   */
  EClass getComplementOfClass();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ComplementOfClass#getClass1 <em>Class1</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class1</em>'.
   * @see com.ge.research.sadl.sadl.ComplementOfClass#getClass1()
   * @see #getComplementOfClass()
   * @generated
   */
  EReference getComplementOfClass_Class1();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ComplementOfClass#getClass2 <em>Class2</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class2</em>'.
   * @see com.ge.research.sadl.sadl.ComplementOfClass#getClass2()
   * @see #getComplementOfClass()
   * @generated
   */
  EReference getComplementOfClass_Class2();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.AllValuesFrom <em>All Values From</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>All Values From</em>'.
   * @see com.ge.research.sadl.sadl.AllValuesFrom
   * @generated
   */
  EClass getAllValuesFrom();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AllValuesFrom#getRestricted <em>Restricted</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restricted</em>'.
   * @see com.ge.research.sadl.sadl.AllValuesFrom#getRestricted()
   * @see #getAllValuesFrom()
   * @generated
   */
  EReference getAllValuesFrom_Restricted();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AllValuesFrom#getCond <em>Cond</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Cond</em>'.
   * @see com.ge.research.sadl.sadl.AllValuesFrom#getCond()
   * @see #getAllValuesFrom()
   * @generated
   */
  EReference getAllValuesFrom_Cond();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AllValuesFrom#getClassName <em>Class Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Name</em>'.
   * @see com.ge.research.sadl.sadl.AllValuesFrom#getClassName()
   * @see #getAllValuesFrom()
   * @generated
   */
  EReference getAllValuesFrom_ClassName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AllValuesFrom#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.AllValuesFrom#getPropertyName()
   * @see #getAllValuesFrom()
   * @generated
   */
  EReference getAllValuesFrom_PropertyName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.SomeValuesFrom <em>Some Values From</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Some Values From</em>'.
   * @see com.ge.research.sadl.sadl.SomeValuesFrom
   * @generated
   */
  EClass getSomeValuesFrom();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.SomeValuesFrom#getRestricted <em>Restricted</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restricted</em>'.
   * @see com.ge.research.sadl.sadl.SomeValuesFrom#getRestricted()
   * @see #getSomeValuesFrom()
   * @generated
   */
  EReference getSomeValuesFrom_Restricted();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.SomeValuesFrom#getCond <em>Cond</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Cond</em>'.
   * @see com.ge.research.sadl.sadl.SomeValuesFrom#getCond()
   * @see #getSomeValuesFrom()
   * @generated
   */
  EReference getSomeValuesFrom_Cond();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.SomeValuesFrom#getClassName <em>Class Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Name</em>'.
   * @see com.ge.research.sadl.sadl.SomeValuesFrom#getClassName()
   * @see #getSomeValuesFrom()
   * @generated
   */
  EReference getSomeValuesFrom_ClassName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.SomeValuesFrom#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.SomeValuesFrom#getPropertyName()
   * @see #getSomeValuesFrom()
   * @generated
   */
  EReference getSomeValuesFrom_PropertyName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.HasValue <em>Has Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Has Value</em>'.
   * @see com.ge.research.sadl.sadl.HasValue
   * @generated
   */
  EClass getHasValue();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.HasValue#getRestricted <em>Restricted</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restricted</em>'.
   * @see com.ge.research.sadl.sadl.HasValue#getRestricted()
   * @see #getHasValue()
   * @generated
   */
  EReference getHasValue_Restricted();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.HasValue#getCond <em>Cond</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Cond</em>'.
   * @see com.ge.research.sadl.sadl.HasValue#getCond()
   * @see #getHasValue()
   * @generated
   */
  EReference getHasValue_Cond();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.HasValue#getClassName <em>Class Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Name</em>'.
   * @see com.ge.research.sadl.sadl.HasValue#getClassName()
   * @see #getHasValue()
   * @generated
   */
  EReference getHasValue_ClassName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.HasValue#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.HasValue#getPropertyName()
   * @see #getHasValue()
   * @generated
   */
  EReference getHasValue_PropertyName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Cardinality <em>Cardinality</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Cardinality</em>'.
   * @see com.ge.research.sadl.sadl.Cardinality
   * @generated
   */
  EClass getCardinality();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Cardinality#getRestricted <em>Restricted</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restricted</em>'.
   * @see com.ge.research.sadl.sadl.Cardinality#getRestricted()
   * @see #getCardinality()
   * @generated
   */
  EReference getCardinality_Restricted();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Cardinality#getCond <em>Cond</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Cond</em>'.
   * @see com.ge.research.sadl.sadl.Cardinality#getCond()
   * @see #getCardinality()
   * @generated
   */
  EReference getCardinality_Cond();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Cardinality#getClassName <em>Class Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Name</em>'.
   * @see com.ge.research.sadl.sadl.Cardinality#getClassName()
   * @see #getCardinality()
   * @generated
   */
  EReference getCardinality_ClassName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Cardinality#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.Cardinality#getPropertyName()
   * @see #getCardinality()
   * @generated
   */
  EReference getCardinality_PropertyName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.MinCardinality <em>Min Cardinality</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Min Cardinality</em>'.
   * @see com.ge.research.sadl.sadl.MinCardinality
   * @generated
   */
  EClass getMinCardinality();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.MinCardinality#getRestricted <em>Restricted</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restricted</em>'.
   * @see com.ge.research.sadl.sadl.MinCardinality#getRestricted()
   * @see #getMinCardinality()
   * @generated
   */
  EReference getMinCardinality_Restricted();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.MinCardinality#getCond <em>Cond</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Cond</em>'.
   * @see com.ge.research.sadl.sadl.MinCardinality#getCond()
   * @see #getMinCardinality()
   * @generated
   */
  EReference getMinCardinality_Cond();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.MinCardinality#getClassName <em>Class Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Name</em>'.
   * @see com.ge.research.sadl.sadl.MinCardinality#getClassName()
   * @see #getMinCardinality()
   * @generated
   */
  EReference getMinCardinality_ClassName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.MinCardinality#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.MinCardinality#getPropertyName()
   * @see #getMinCardinality()
   * @generated
   */
  EReference getMinCardinality_PropertyName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.MaxCardinality <em>Max Cardinality</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Max Cardinality</em>'.
   * @see com.ge.research.sadl.sadl.MaxCardinality
   * @generated
   */
  EClass getMaxCardinality();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.MaxCardinality#getRestricted <em>Restricted</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restricted</em>'.
   * @see com.ge.research.sadl.sadl.MaxCardinality#getRestricted()
   * @see #getMaxCardinality()
   * @generated
   */
  EReference getMaxCardinality_Restricted();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.MaxCardinality#getCond <em>Cond</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Cond</em>'.
   * @see com.ge.research.sadl.sadl.MaxCardinality#getCond()
   * @see #getMaxCardinality()
   * @generated
   */
  EReference getMaxCardinality_Cond();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.MaxCardinality#getClassName <em>Class Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Name</em>'.
   * @see com.ge.research.sadl.sadl.MaxCardinality#getClassName()
   * @see #getMaxCardinality()
   * @generated
   */
  EReference getMaxCardinality_ClassName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.MaxCardinality#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.MaxCardinality#getPropertyName()
   * @see #getMaxCardinality()
   * @generated
   */
  EReference getMaxCardinality_PropertyName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.PropertyOfClass <em>Property Of Class</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Property Of Class</em>'.
   * @see com.ge.research.sadl.sadl.PropertyOfClass
   * @generated
   */
  EClass getPropertyOfClass();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.PropertyOfClass#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.PropertyOfClass#getPropertyName()
   * @see #getPropertyOfClass()
   * @generated
   */
  EReference getPropertyOfClass_PropertyName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.PropertyOfClass#getClassName <em>Class Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Name</em>'.
   * @see com.ge.research.sadl.sadl.PropertyOfClass#getClassName()
   * @see #getPropertyOfClass()
   * @generated
   */
  EReference getPropertyOfClass_ClassName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.AllValuesCondition <em>All Values Condition</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>All Values Condition</em>'.
   * @see com.ge.research.sadl.sadl.AllValuesCondition
   * @generated
   */
  EClass getAllValuesCondition();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AllValuesCondition#getRestriction <em>Restriction</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restriction</em>'.
   * @see com.ge.research.sadl.sadl.AllValuesCondition#getRestriction()
   * @see #getAllValuesCondition()
   * @generated
   */
  EReference getAllValuesCondition_Restriction();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.EnumeratedAllValuesFrom <em>Enumerated All Values From</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Enumerated All Values From</em>'.
   * @see com.ge.research.sadl.sadl.EnumeratedAllValuesFrom
   * @generated
   */
  EClass getEnumeratedAllValuesFrom();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.EnumeratedAllValuesFrom#getRestricted <em>Restricted</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restricted</em>'.
   * @see com.ge.research.sadl.sadl.EnumeratedAllValuesFrom#getRestricted()
   * @see #getEnumeratedAllValuesFrom()
   * @generated
   */
  EReference getEnumeratedAllValuesFrom_Restricted();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.EnumeratedAllValuesFrom#getEnumeration <em>Enumeration</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Enumeration</em>'.
   * @see com.ge.research.sadl.sadl.EnumeratedAllValuesFrom#getEnumeration()
   * @see #getEnumeratedAllValuesFrom()
   * @generated
   */
  EReference getEnumeratedAllValuesFrom_Enumeration();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom <em>Enumerated All And Some Values From</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Enumerated All And Some Values From</em>'.
   * @see com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom
   * @generated
   */
  EClass getEnumeratedAllAndSomeValuesFrom();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom#getRestricted <em>Restricted</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restricted</em>'.
   * @see com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom#getRestricted()
   * @see #getEnumeratedAllAndSomeValuesFrom()
   * @generated
   */
  EReference getEnumeratedAllAndSomeValuesFrom_Restricted();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom#getEnumeration <em>Enumeration</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Enumeration</em>'.
   * @see com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom#getEnumeration()
   * @see #getEnumeratedAllAndSomeValuesFrom()
   * @generated
   */
  EReference getEnumeratedAllAndSomeValuesFrom_Enumeration();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.DefaultValue <em>Default Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Default Value</em>'.
   * @see com.ge.research.sadl.sadl.DefaultValue
   * @generated
   */
  EClass getDefaultValue();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.DefaultValue#getDefValueClass <em>Def Value Class</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Def Value Class</em>'.
   * @see com.ge.research.sadl.sadl.DefaultValue#getDefValueClass()
   * @see #getDefaultValue()
   * @generated
   */
  EReference getDefaultValue_DefValueClass();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.DefaultValue#getLevel <em>Level</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Level</em>'.
   * @see com.ge.research.sadl.sadl.DefaultValue#getLevel()
   * @see #getDefaultValue()
   * @generated
   */
  EAttribute getDefaultValue_Level();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.DefaultValue#getDefValue <em>Def Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Def Value</em>'.
   * @see com.ge.research.sadl.sadl.DefaultValue#getDefValue()
   * @see #getDefaultValue()
   * @generated
   */
  EReference getDefaultValue_DefValue();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.SomeValuesCondition <em>Some Values Condition</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Some Values Condition</em>'.
   * @see com.ge.research.sadl.sadl.SomeValuesCondition
   * @generated
   */
  EClass getSomeValuesCondition();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.SomeValuesCondition#getRestriction <em>Restriction</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restriction</em>'.
   * @see com.ge.research.sadl.sadl.SomeValuesCondition#getRestriction()
   * @see #getSomeValuesCondition()
   * @generated
   */
  EReference getSomeValuesCondition_Restriction();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.HasValueCondition <em>Has Value Condition</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Has Value Condition</em>'.
   * @see com.ge.research.sadl.sadl.HasValueCondition
   * @generated
   */
  EClass getHasValueCondition();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.HasValueCondition#getRestriction <em>Restriction</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Restriction</em>'.
   * @see com.ge.research.sadl.sadl.HasValueCondition#getRestriction()
   * @see #getHasValueCondition()
   * @generated
   */
  EReference getHasValueCondition_Restriction();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.MinCardCondition <em>Min Card Condition</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Min Card Condition</em>'.
   * @see com.ge.research.sadl.sadl.MinCardCondition
   * @generated
   */
  EClass getMinCardCondition();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.MinCardCondition#getCard <em>Card</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Card</em>'.
   * @see com.ge.research.sadl.sadl.MinCardCondition#getCard()
   * @see #getMinCardCondition()
   * @generated
   */
  EAttribute getMinCardCondition_Card();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.MinCardCondition#getClassQualifier <em>Class Qualifier</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Qualifier</em>'.
   * @see com.ge.research.sadl.sadl.MinCardCondition#getClassQualifier()
   * @see #getMinCardCondition()
   * @generated
   */
  EReference getMinCardCondition_ClassQualifier();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.MaxCardCondition <em>Max Card Condition</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Max Card Condition</em>'.
   * @see com.ge.research.sadl.sadl.MaxCardCondition
   * @generated
   */
  EClass getMaxCardCondition();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.MaxCardCondition#getCard <em>Card</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Card</em>'.
   * @see com.ge.research.sadl.sadl.MaxCardCondition#getCard()
   * @see #getMaxCardCondition()
   * @generated
   */
  EAttribute getMaxCardCondition_Card();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.MaxCardCondition#getClassQualifier <em>Class Qualifier</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Qualifier</em>'.
   * @see com.ge.research.sadl.sadl.MaxCardCondition#getClassQualifier()
   * @see #getMaxCardCondition()
   * @generated
   */
  EReference getMaxCardCondition_ClassQualifier();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.CardCondition <em>Card Condition</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Card Condition</em>'.
   * @see com.ge.research.sadl.sadl.CardCondition
   * @generated
   */
  EClass getCardCondition();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.CardCondition#getCard <em>Card</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Card</em>'.
   * @see com.ge.research.sadl.sadl.CardCondition#getCard()
   * @see #getCardCondition()
   * @generated
   */
  EAttribute getCardCondition_Card();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.CardCondition#getClassQualifier <em>Class Qualifier</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Qualifier</em>'.
   * @see com.ge.research.sadl.sadl.CardCondition#getClassQualifier()
   * @see #getCardCondition()
   * @generated
   */
  EReference getCardCondition_ClassQualifier();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.NecessaryAndSufficient <em>Necessary And Sufficient</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Necessary And Sufficient</em>'.
   * @see com.ge.research.sadl.sadl.NecessaryAndSufficient
   * @generated
   */
  EClass getNecessaryAndSufficient();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getSuperClass <em>Super Class</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Super Class</em>'.
   * @see com.ge.research.sadl.sadl.NecessaryAndSufficient#getSuperClass()
   * @see #getNecessaryAndSufficient()
   * @generated
   */
  EReference getNecessaryAndSufficient_SuperClass();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getArticle <em>Article</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Article</em>'.
   * @see com.ge.research.sadl.sadl.NecessaryAndSufficient#getArticle()
   * @see #getNecessaryAndSufficient()
   * @generated
   */
  EAttribute getNecessaryAndSufficient_Article();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getSubClass <em>Sub Class</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Sub Class</em>'.
   * @see com.ge.research.sadl.sadl.NecessaryAndSufficient#getSubClass()
   * @see #getNecessaryAndSufficient()
   * @generated
   */
  EReference getNecessaryAndSufficient_SubClass();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.NecessaryAndSufficient#getPropertyName()
   * @see #getNecessaryAndSufficient()
   * @generated
   */
  EReference getNecessaryAndSufficient_PropertyName();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getCond <em>Cond</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Cond</em>'.
   * @see com.ge.research.sadl.sadl.NecessaryAndSufficient#getCond()
   * @see #getNecessaryAndSufficient()
   * @generated
   */
  EReference getNecessaryAndSufficient_Cond();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Condition <em>Condition</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Condition</em>'.
   * @see com.ge.research.sadl.sadl.Condition
   * @generated
   */
  EClass getCondition();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.PropertyDeclaration <em>Property Declaration</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Property Declaration</em>'.
   * @see com.ge.research.sadl.sadl.PropertyDeclaration
   * @generated
   */
  EClass getPropertyDeclaration();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.PropertyDeclaration#getPropertyName()
   * @see #getPropertyDeclaration()
   * @generated
   */
  EReference getPropertyDeclaration_PropertyName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getSuperPropName <em>Super Prop Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Super Prop Name</em>'.
   * @see com.ge.research.sadl.sadl.PropertyDeclaration#getSuperPropName()
   * @see #getPropertyDeclaration()
   * @generated
   */
  EReference getPropertyDeclaration_SuperPropName();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getAddlPropInfo <em>Addl Prop Info</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Addl Prop Info</em>'.
   * @see com.ge.research.sadl.sadl.PropertyDeclaration#getAddlPropInfo()
   * @see #getPropertyDeclaration()
   * @generated
   */
  EReference getPropertyDeclaration_AddlPropInfo();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getArticle <em>Article</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Article</em>'.
   * @see com.ge.research.sadl.sadl.PropertyDeclaration#getArticle()
   * @see #getPropertyDeclaration()
   * @generated
   */
  EAttribute getPropertyDeclaration_Article();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getDomain <em>Domain</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Domain</em>'.
   * @see com.ge.research.sadl.sadl.PropertyDeclaration#getDomain()
   * @see #getPropertyDeclaration()
   * @generated
   */
  EReference getPropertyDeclaration_Domain();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getRangeResource <em>Range Resource</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Range Resource</em>'.
   * @see com.ge.research.sadl.sadl.PropertyDeclaration#getRangeResource()
   * @see #getPropertyDeclaration()
   * @generated
   */
  EReference getPropertyDeclaration_RangeResource();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getAnnotationProperty <em>Annotation Property</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Annotation Property</em>'.
   * @see com.ge.research.sadl.sadl.PropertyDeclaration#getAnnotationProperty()
   * @see #getPropertyDeclaration()
   * @generated
   */
  EReference getPropertyDeclaration_AnnotationProperty();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo <em>Additional Property Info</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Additional Property Info</em>'.
   * @see com.ge.research.sadl.sadl.AdditionalPropertyInfo
   * @generated
   */
  EClass getAdditionalPropertyInfo();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getDomain <em>Domain</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Domain</em>'.
   * @see com.ge.research.sadl.sadl.AdditionalPropertyInfo#getDomain()
   * @see #getAdditionalPropertyInfo()
   * @generated
   */
  EReference getAdditionalPropertyInfo_Domain();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getCond <em>Cond</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Cond</em>'.
   * @see com.ge.research.sadl.sadl.AdditionalPropertyInfo#getCond()
   * @see #getAdditionalPropertyInfo()
   * @generated
   */
  EReference getAdditionalPropertyInfo_Cond();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getRange <em>Range</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Range</em>'.
   * @see com.ge.research.sadl.sadl.AdditionalPropertyInfo#getRange()
   * @see #getAdditionalPropertyInfo()
   * @generated
   */
  EReference getAdditionalPropertyInfo_Range();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsfunc <em>Isfunc</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Isfunc</em>'.
   * @see com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsfunc()
   * @see #getAdditionalPropertyInfo()
   * @generated
   */
  EAttribute getAdditionalPropertyInfo_Isfunc();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsinvfunc <em>Isinvfunc</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Isinvfunc</em>'.
   * @see com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsinvfunc()
   * @see #getAdditionalPropertyInfo()
   * @generated
   */
  EAttribute getAdditionalPropertyInfo_Isinvfunc();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsSym <em>Is Sym</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Is Sym</em>'.
   * @see com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsSym()
   * @see #getAdditionalPropertyInfo()
   * @generated
   */
  EAttribute getAdditionalPropertyInfo_IsSym();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsTrans <em>Is Trans</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Is Trans</em>'.
   * @see com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsTrans()
   * @see #getAdditionalPropertyInfo()
   * @generated
   */
  EAttribute getAdditionalPropertyInfo_IsTrans();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsInvOf <em>Is Inv Of</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Is Inv Of</em>'.
   * @see com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsInvOf()
   * @see #getAdditionalPropertyInfo()
   * @generated
   */
  EReference getAdditionalPropertyInfo_IsInvOf();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.FunctionalProperty <em>Functional Property</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Functional Property</em>'.
   * @see com.ge.research.sadl.sadl.FunctionalProperty
   * @generated
   */
  EClass getFunctionalProperty();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.FunctionalProperty#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.FunctionalProperty#getPropertyName()
   * @see #getFunctionalProperty()
   * @generated
   */
  EReference getFunctionalProperty_PropertyName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.InverseFunctionalProperty <em>Inverse Functional Property</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Inverse Functional Property</em>'.
   * @see com.ge.research.sadl.sadl.InverseFunctionalProperty
   * @generated
   */
  EClass getInverseFunctionalProperty();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InverseFunctionalProperty#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.InverseFunctionalProperty#getPropertyName()
   * @see #getInverseFunctionalProperty()
   * @generated
   */
  EReference getInverseFunctionalProperty_PropertyName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.SymmetricalProperty <em>Symmetrical Property</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Symmetrical Property</em>'.
   * @see com.ge.research.sadl.sadl.SymmetricalProperty
   * @generated
   */
  EClass getSymmetricalProperty();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.SymmetricalProperty#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.SymmetricalProperty#getPropertyName()
   * @see #getSymmetricalProperty()
   * @generated
   */
  EReference getSymmetricalProperty_PropertyName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.TransitiveProperty <em>Transitive Property</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Transitive Property</em>'.
   * @see com.ge.research.sadl.sadl.TransitiveProperty
   * @generated
   */
  EClass getTransitiveProperty();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.TransitiveProperty#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.TransitiveProperty#getPropertyName()
   * @see #getTransitiveProperty()
   * @generated
   */
  EReference getTransitiveProperty_PropertyName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.InverseProperty <em>Inverse Property</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Inverse Property</em>'.
   * @see com.ge.research.sadl.sadl.InverseProperty
   * @generated
   */
  EClass getInverseProperty();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InverseProperty#getPropertyName1 <em>Property Name1</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name1</em>'.
   * @see com.ge.research.sadl.sadl.InverseProperty#getPropertyName1()
   * @see #getInverseProperty()
   * @generated
   */
  EReference getInverseProperty_PropertyName1();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InverseProperty#getInvOf <em>Inv Of</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Inv Of</em>'.
   * @see com.ge.research.sadl.sadl.InverseProperty#getInvOf()
   * @see #getInverseProperty()
   * @generated
   */
  EReference getInverseProperty_InvOf();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.IsInverseOf <em>Is Inverse Of</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Is Inverse Of</em>'.
   * @see com.ge.research.sadl.sadl.IsInverseOf
   * @generated
   */
  EClass getIsInverseOf();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.IsInverseOf#getPropertyName2 <em>Property Name2</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name2</em>'.
   * @see com.ge.research.sadl.sadl.IsInverseOf#getPropertyName2()
   * @see #getIsInverseOf()
   * @generated
   */
  EReference getIsInverseOf_PropertyName2();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.TypedBNode <em>Typed BNode</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Typed BNode</em>'.
   * @see com.ge.research.sadl.sadl.TypedBNode
   * @generated
   */
  EClass getTypedBNode();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.TypedBNode#getArticle <em>Article</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Article</em>'.
   * @see com.ge.research.sadl.sadl.TypedBNode#getArticle()
   * @see #getTypedBNode()
   * @generated
   */
  EAttribute getTypedBNode_Article();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.TypedBNode#getClassIdentifier <em>Class Identifier</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Identifier</em>'.
   * @see com.ge.research.sadl.sadl.TypedBNode#getClassIdentifier()
   * @see #getTypedBNode()
   * @generated
   */
  EReference getTypedBNode_ClassIdentifier();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.InstanceDeclarationStatement <em>Instance Declaration Statement</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Instance Declaration Statement</em>'.
   * @see com.ge.research.sadl.sadl.InstanceDeclarationStatement
   * @generated
   */
  EClass getInstanceDeclarationStatement();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.InstanceDeclaration <em>Instance Declaration</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Instance Declaration</em>'.
   * @see com.ge.research.sadl.sadl.InstanceDeclaration
   * @generated
   */
  EClass getInstanceDeclaration();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InstanceDeclaration#getTypeDecl <em>Type Decl</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Type Decl</em>'.
   * @see com.ge.research.sadl.sadl.InstanceDeclaration#getTypeDecl()
   * @see #getInstanceDeclaration()
   * @generated
   */
  EReference getInstanceDeclaration_TypeDecl();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.InstanceDeclaration#getAddlInfoItems <em>Addl Info Items</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Addl Info Items</em>'.
   * @see com.ge.research.sadl.sadl.InstanceDeclaration#getAddlInfoItems()
   * @see #getInstanceDeclaration()
   * @generated
   */
  EReference getInstanceDeclaration_AddlInfoItems();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.InstanceDeclaration#getArticle <em>Article</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Article</em>'.
   * @see com.ge.research.sadl.sadl.InstanceDeclaration#getArticle()
   * @see #getInstanceDeclaration()
   * @generated
   */
  EAttribute getInstanceDeclaration_Article();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InstanceDeclaration#getClassName <em>Class Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Class Name</em>'.
   * @see com.ge.research.sadl.sadl.InstanceDeclaration#getClassName()
   * @see #getInstanceDeclaration()
   * @generated
   */
  EReference getInstanceDeclaration_ClassName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InstanceDeclaration#getInstanceName <em>Instance Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Instance Name</em>'.
   * @see com.ge.research.sadl.sadl.InstanceDeclaration#getInstanceName()
   * @see #getInstanceDeclaration()
   * @generated
   */
  EReference getInstanceDeclaration_InstanceName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.TypeDeclaration <em>Type Declaration</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Type Declaration</em>'.
   * @see com.ge.research.sadl.sadl.TypeDeclaration
   * @generated
   */
  EClass getTypeDeclaration();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.TypeDeclaration#getInstName <em>Inst Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Inst Name</em>'.
   * @see com.ge.research.sadl.sadl.TypeDeclaration#getInstName()
   * @see #getTypeDeclaration()
   * @generated
   */
  EReference getTypeDeclaration_InstName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.TypeDeclaration#getType <em>Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Type</em>'.
   * @see com.ge.research.sadl.sadl.TypeDeclaration#getType()
   * @see #getTypeDeclaration()
   * @generated
   */
  EReference getTypeDeclaration_Type();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.InstanceDifferentFrom <em>Instance Different From</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Instance Different From</em>'.
   * @see com.ge.research.sadl.sadl.InstanceDifferentFrom
   * @generated
   */
  EClass getInstanceDifferentFrom();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InstanceDifferentFrom#getInstName1 <em>Inst Name1</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Inst Name1</em>'.
   * @see com.ge.research.sadl.sadl.InstanceDifferentFrom#getInstName1()
   * @see #getInstanceDifferentFrom()
   * @generated
   */
  EReference getInstanceDifferentFrom_InstName1();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InstanceDifferentFrom#getInstName2 <em>Inst Name2</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Inst Name2</em>'.
   * @see com.ge.research.sadl.sadl.InstanceDifferentFrom#getInstName2()
   * @see #getInstanceDifferentFrom()
   * @generated
   */
  EReference getInstanceDifferentFrom_InstName2();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.InstancesAllDifferent <em>Instances All Different</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Instances All Different</em>'.
   * @see com.ge.research.sadl.sadl.InstancesAllDifferent
   * @generated
   */
  EClass getInstancesAllDifferent();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InstancesAllDifferent#getInstances <em>Instances</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Instances</em>'.
   * @see com.ge.research.sadl.sadl.InstancesAllDifferent#getInstances()
   * @see #getInstancesAllDifferent()
   * @generated
   */
  EReference getInstancesAllDifferent_Instances();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution <em>Existing Instance Attribution</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Existing Instance Attribution</em>'.
   * @see com.ge.research.sadl.sadl.ExistingInstanceAttribution
   * @generated
   */
  EClass getExistingInstanceAttribution();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution#getSubj <em>Subj</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Subj</em>'.
   * @see com.ge.research.sadl.sadl.ExistingInstanceAttribution#getSubj()
   * @see #getExistingInstanceAttribution()
   * @generated
   */
  EReference getExistingInstanceAttribution_Subj();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution#getAddlInfoItems <em>Addl Info Items</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Addl Info Items</em>'.
   * @see com.ge.research.sadl.sadl.ExistingInstanceAttribution#getAddlInfoItems()
   * @see #getExistingInstanceAttribution()
   * @generated
   */
  EReference getExistingInstanceAttribution_AddlInfoItems();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution#getPOfS <em>POf S</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>POf S</em>'.
   * @see com.ge.research.sadl.sadl.ExistingInstanceAttribution#getPOfS()
   * @see #getExistingInstanceAttribution()
   * @generated
   */
  EReference getExistingInstanceAttribution_POfS();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution#getObj <em>Obj</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Obj</em>'.
   * @see com.ge.research.sadl.sadl.ExistingInstanceAttribution#getObj()
   * @see #getExistingInstanceAttribution()
   * @generated
   */
  EReference getExistingInstanceAttribution_Obj();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Object <em>Object</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Object</em>'.
   * @see com.ge.research.sadl.sadl.Object
   * @generated
   */
  EClass getObject();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Object#getVal <em>Val</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Val</em>'.
   * @see com.ge.research.sadl.sadl.Object#getVal()
   * @see #getObject()
   * @generated
   */
  EReference getObject_Val();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.PropValPartialTriple <em>Prop Val Partial Triple</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Prop Val Partial Triple</em>'.
   * @see com.ge.research.sadl.sadl.PropValPartialTriple
   * @generated
   */
  EClass getPropValPartialTriple();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.PropValPartialTriple#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.PropValPartialTriple#getPropertyName()
   * @see #getPropValPartialTriple()
   * @generated
   */
  EReference getPropValPartialTriple_PropertyName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.PropValPartialTriple#getObjectValue <em>Object Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Object Value</em>'.
   * @see com.ge.research.sadl.sadl.PropValPartialTriple#getObjectValue()
   * @see #getPropValPartialTriple()
   * @generated
   */
  EReference getPropValPartialTriple_ObjectValue();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.PropValPartialTriple#getObjectValueBNode <em>Object Value BNode</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Object Value BNode</em>'.
   * @see com.ge.research.sadl.sadl.PropValPartialTriple#getObjectValueBNode()
   * @see #getPropValPartialTriple()
   * @generated
   */
  EReference getPropValPartialTriple_ObjectValueBNode();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.OfPatternReturningValues <em>Of Pattern Returning Values</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Of Pattern Returning Values</em>'.
   * @see com.ge.research.sadl.sadl.OfPatternReturningValues
   * @generated
   */
  EClass getOfPatternReturningValues();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.OfPatternReturningValues#getOfphrs <em>Ofphrs</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Ofphrs</em>'.
   * @see com.ge.research.sadl.sadl.OfPatternReturningValues#getOfphrs()
   * @see #getOfPatternReturningValues()
   * @generated
   */
  EReference getOfPatternReturningValues_Ofphrs();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.OfPatternReturningValues#getSubject <em>Subject</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Subject</em>'.
   * @see com.ge.research.sadl.sadl.OfPatternReturningValues#getSubject()
   * @see #getOfPatternReturningValues()
   * @generated
   */
  EReference getOfPatternReturningValues_Subject();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.OfPatternReturningValues#getType <em>Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Type</em>'.
   * @see com.ge.research.sadl.sadl.OfPatternReturningValues#getType()
   * @see #getOfPatternReturningValues()
   * @generated
   */
  EReference getOfPatternReturningValues_Type();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.WithChain <em>With Chain</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>With Chain</em>'.
   * @see com.ge.research.sadl.sadl.WithChain
   * @generated
   */
  EClass getWithChain();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.WithChain#getWps <em>Wps</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Wps</em>'.
   * @see com.ge.research.sadl.sadl.WithChain#getWps()
   * @see #getWithChain()
   * @generated
   */
  EReference getWithChain_Wps();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.WithPhrase <em>With Phrase</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>With Phrase</em>'.
   * @see com.ge.research.sadl.sadl.WithPhrase
   * @generated
   */
  EClass getWithPhrase();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.WithPhrase#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.WithPhrase#getPropertyName()
   * @see #getWithPhrase()
   * @generated
   */
  EReference getWithPhrase_PropertyName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.WithPhrase#getValue <em>Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Value</em>'.
   * @see com.ge.research.sadl.sadl.WithPhrase#getValue()
   * @see #getWithPhrase()
   * @generated
   */
  EReference getWithPhrase_Value();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.EmbeddedInstanceDeclaration <em>Embedded Instance Declaration</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Embedded Instance Declaration</em>'.
   * @see com.ge.research.sadl.sadl.EmbeddedInstanceDeclaration
   * @generated
   */
  EClass getEmbeddedInstanceDeclaration();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.MergedTriples <em>Merged Triples</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Merged Triples</em>'.
   * @see com.ge.research.sadl.sadl.MergedTriples
   * @generated
   */
  EClass getMergedTriples();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.MergedTriples#getOps <em>Ops</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Ops</em>'.
   * @see com.ge.research.sadl.sadl.MergedTriples#getOps()
   * @see #getMergedTriples()
   * @generated
   */
  EReference getMergedTriples_Ops();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.MergedTriples#getPivot <em>Pivot</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Pivot</em>'.
   * @see com.ge.research.sadl.sadl.MergedTriples#getPivot()
   * @see #getMergedTriples()
   * @generated
   */
  EReference getMergedTriples_Pivot();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.MergedTriples#getWcs <em>Wcs</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Wcs</em>'.
   * @see com.ge.research.sadl.sadl.MergedTriples#getWcs()
   * @see #getMergedTriples()
   * @generated
   */
  EReference getMergedTriples_Wcs();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.OfPhrase <em>Of Phrase</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Of Phrase</em>'.
   * @see com.ge.research.sadl.sadl.OfPhrase
   * @generated
   */
  EClass getOfPhrase();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.OfPhrase#getArticle <em>Article</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Article</em>'.
   * @see com.ge.research.sadl.sadl.OfPhrase#getArticle()
   * @see #getOfPhrase()
   * @generated
   */
  EAttribute getOfPhrase_Article();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.OfPhrase#getPropertyName <em>Property Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Property Name</em>'.
   * @see com.ge.research.sadl.sadl.OfPhrase#getPropertyName()
   * @see #getOfPhrase()
   * @generated
   */
  EReference getOfPhrase_PropertyName();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.VariableList <em>Variable List</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Variable List</em>'.
   * @see com.ge.research.sadl.sadl.VariableList
   * @generated
   */
  EClass getVariableList();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.VariableList#getNames <em>Names</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Names</em>'.
   * @see com.ge.research.sadl.sadl.VariableList#getNames()
   * @see #getVariableList()
   * @generated
   */
  EReference getVariableList_Names();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Rule <em>Rule</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Rule</em>'.
   * @see com.ge.research.sadl.sadl.Rule
   * @generated
   */
  EClass getRule();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Rule#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see com.ge.research.sadl.sadl.Rule#getName()
   * @see #getRule()
   * @generated
   */
  EAttribute getRule_Name();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Rule#getGivens <em>Givens</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Givens</em>'.
   * @see com.ge.research.sadl.sadl.Rule#getGivens()
   * @see #getRule()
   * @generated
   */
  EReference getRule_Givens();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Rule#getIfs <em>Ifs</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Ifs</em>'.
   * @see com.ge.research.sadl.sadl.Rule#getIfs()
   * @see #getRule()
   * @generated
   */
  EReference getRule_Ifs();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Rule#getThens <em>Thens</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Thens</em>'.
   * @see com.ge.research.sadl.sadl.Rule#getThens()
   * @see #getRule()
   * @generated
   */
  EReference getRule_Thens();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Query <em>Query</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Query</em>'.
   * @see com.ge.research.sadl.sadl.Query
   * @generated
   */
  EClass getQuery();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Query#getExpr <em>Expr</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Expr</em>'.
   * @see com.ge.research.sadl.sadl.Query#getExpr()
   * @see #getQuery()
   * @generated
   */
  EReference getQuery_Expr();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Test <em>Test</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Test</em>'.
   * @see com.ge.research.sadl.sadl.Test
   * @generated
   */
  EClass getTest();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Test#getExpr <em>Expr</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Expr</em>'.
   * @see com.ge.research.sadl.sadl.Test#getExpr()
   * @see #getTest()
   * @generated
   */
  EReference getTest_Expr();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Expr <em>Expr</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Expr</em>'.
   * @see com.ge.research.sadl.sadl.Expr
   * @generated
   */
  EClass getExpr();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Expr#getExpr <em>Expr</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Expr</em>'.
   * @see com.ge.research.sadl.sadl.Expr#getExpr()
   * @see #getExpr()
   * @generated
   */
  EReference getExpr_Expr();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Display <em>Display</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Display</em>'.
   * @see com.ge.research.sadl.sadl.Display
   * @generated
   */
  EClass getDisplay();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Display#getDisplayString <em>Display String</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Display String</em>'.
   * @see com.ge.research.sadl.sadl.Display#getDisplayString()
   * @see #getDisplay()
   * @generated
   */
  EAttribute getDisplay_DisplayString();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Display#getModel <em>Model</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Model</em>'.
   * @see com.ge.research.sadl.sadl.Display#getModel()
   * @see #getDisplay()
   * @generated
   */
  EAttribute getDisplay_Model();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Explanation <em>Explanation</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Explanation</em>'.
   * @see com.ge.research.sadl.sadl.Explanation
   * @generated
   */
  EClass getExplanation();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Explanation#getExpr <em>Expr</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Expr</em>'.
   * @see com.ge.research.sadl.sadl.Explanation#getExpr()
   * @see #getExplanation()
   * @generated
   */
  EReference getExplanation_Expr();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Explanation#getRulename <em>Rulename</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Rulename</em>'.
   * @see com.ge.research.sadl.sadl.Explanation#getRulename()
   * @see #getExplanation()
   * @generated
   */
  EAttribute getExplanation_Rulename();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ElementSet <em>Element Set</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Element Set</em>'.
   * @see com.ge.research.sadl.sadl.ElementSet
   * @generated
   */
  EClass getElementSet();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.ElementSet#getElements <em>Elements</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Elements</em>'.
   * @see com.ge.research.sadl.sadl.ElementSet#getElements()
   * @see #getElementSet()
   * @generated
   */
  EReference getElementSet_Elements();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.SelectExpression <em>Select Expression</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Select Expression</em>'.
   * @see com.ge.research.sadl.sadl.SelectExpression
   * @generated
   */
  EClass getSelectExpression();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.SelectExpression#getDistinct <em>Distinct</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Distinct</em>'.
   * @see com.ge.research.sadl.sadl.SelectExpression#getDistinct()
   * @see #getSelectExpression()
   * @generated
   */
  EAttribute getSelectExpression_Distinct();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.SelectExpression#getAllVars <em>All Vars</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>All Vars</em>'.
   * @see com.ge.research.sadl.sadl.SelectExpression#getAllVars()
   * @see #getSelectExpression()
   * @generated
   */
  EAttribute getSelectExpression_AllVars();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.SelectExpression#getVarList <em>Var List</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Var List</em>'.
   * @see com.ge.research.sadl.sadl.SelectExpression#getVarList()
   * @see #getSelectExpression()
   * @generated
   */
  EReference getSelectExpression_VarList();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.SelectExpression#getOrderby <em>Orderby</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Orderby</em>'.
   * @see com.ge.research.sadl.sadl.SelectExpression#getOrderby()
   * @see #getSelectExpression()
   * @generated
   */
  EAttribute getSelectExpression_Orderby();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.SelectExpression#getOrderList <em>Order List</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Order List</em>'.
   * @see com.ge.research.sadl.sadl.SelectExpression#getOrderList()
   * @see #getSelectExpression()
   * @generated
   */
  EReference getSelectExpression_OrderList();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ConstructExpression <em>Construct Expression</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Construct Expression</em>'.
   * @see com.ge.research.sadl.sadl.ConstructExpression
   * @generated
   */
  EClass getConstructExpression();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ConstructExpression#getSubj <em>Subj</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Subj</em>'.
   * @see com.ge.research.sadl.sadl.ConstructExpression#getSubj()
   * @see #getConstructExpression()
   * @generated
   */
  EReference getConstructExpression_Subj();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ConstructExpression#getPred <em>Pred</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Pred</em>'.
   * @see com.ge.research.sadl.sadl.ConstructExpression#getPred()
   * @see #getConstructExpression()
   * @generated
   */
  EReference getConstructExpression_Pred();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ConstructExpression#getObj <em>Obj</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Obj</em>'.
   * @see com.ge.research.sadl.sadl.ConstructExpression#getObj()
   * @see #getConstructExpression()
   * @generated
   */
  EReference getConstructExpression_Obj();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.AskQueryExpression <em>Ask Query Expression</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Ask Query Expression</em>'.
   * @see com.ge.research.sadl.sadl.AskQueryExpression
   * @generated
   */
  EClass getAskQueryExpression();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.OrderList <em>Order List</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Order List</em>'.
   * @see com.ge.research.sadl.sadl.OrderList
   * @generated
   */
  EClass getOrderList();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.OrderList#getOrderList <em>Order List</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Order List</em>'.
   * @see com.ge.research.sadl.sadl.OrderList#getOrderList()
   * @see #getOrderList()
   * @generated
   */
  EReference getOrderList_OrderList();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.OrderElement <em>Order Element</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Order Element</em>'.
   * @see com.ge.research.sadl.sadl.OrderElement
   * @generated
   */
  EClass getOrderElement();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.OrderElement#getOrder <em>Order</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Order</em>'.
   * @see com.ge.research.sadl.sadl.OrderElement#getOrder()
   * @see #getOrderElement()
   * @generated
   */
  EAttribute getOrderElement_Order();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.OrderElement#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Name</em>'.
   * @see com.ge.research.sadl.sadl.OrderElement#getName()
   * @see #getOrderElement()
   * @generated
   */
  EReference getOrderElement_Name();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.Expression <em>Expression</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Expression</em>'.
   * @see com.ge.research.sadl.sadl.Expression
   * @generated
   */
  EClass getExpression();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Expression#getExpr <em>Expr</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Expr</em>'.
   * @see com.ge.research.sadl.sadl.Expression#getExpr()
   * @see #getExpression()
   * @generated
   */
  EReference getExpression_Expr();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.Expression#getFunc <em>Func</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Func</em>'.
   * @see com.ge.research.sadl.sadl.Expression#getFunc()
   * @see #getExpression()
   * @generated
   */
  EAttribute getExpression_Func();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.Expression#getArgs <em>Args</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Args</em>'.
   * @see com.ge.research.sadl.sadl.Expression#getArgs()
   * @see #getExpression()
   * @generated
   */
  EReference getExpression_Args();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Expression#getGp <em>Gp</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Gp</em>'.
   * @see com.ge.research.sadl.sadl.Expression#getGp()
   * @see #getExpression()
   * @generated
   */
  EReference getExpression_Gp();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Expression#getIvalue <em>Ivalue</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Ivalue</em>'.
   * @see com.ge.research.sadl.sadl.Expression#getIvalue()
   * @see #getExpression()
   * @generated
   */
  EReference getExpression_Ivalue();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Expression#getValue <em>Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Value</em>'.
   * @see com.ge.research.sadl.sadl.Expression#getValue()
   * @see #getExpression()
   * @generated
   */
  EReference getExpression_Value();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.Expression#getValueTable <em>Value Table</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Value Table</em>'.
   * @see com.ge.research.sadl.sadl.Expression#getValueTable()
   * @see #getExpression()
   * @generated
   */
  EReference getExpression_ValueTable();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.GraphPattern <em>Graph Pattern</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Graph Pattern</em>'.
   * @see com.ge.research.sadl.sadl.GraphPattern
   * @generated
   */
  EClass getGraphPattern();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.PropOfSubj <em>Prop Of Subj</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Prop Of Subj</em>'.
   * @see com.ge.research.sadl.sadl.PropOfSubj
   * @generated
   */
  EClass getPropOfSubj();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.PropOfSubj#getOfPhr <em>Of Phr</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Of Phr</em>'.
   * @see com.ge.research.sadl.sadl.PropOfSubj#getOfPhr()
   * @see #getPropOfSubj()
   * @generated
   */
  EReference getPropOfSubj_OfPhr();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.PropOfSubj#getSubj <em>Subj</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Subj</em>'.
   * @see com.ge.research.sadl.sadl.PropOfSubj#getSubj()
   * @see #getPropOfSubj()
   * @generated
   */
  EReference getPropOfSubj_Subj();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.SubjProp <em>Subj Prop</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Subj Prop</em>'.
   * @see com.ge.research.sadl.sadl.SubjProp
   * @generated
   */
  EClass getSubjProp();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.SubjProp#getSubj <em>Subj</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Subj</em>'.
   * @see com.ge.research.sadl.sadl.SubjProp#getSubj()
   * @see #getSubjProp()
   * @generated
   */
  EReference getSubjProp_Subj();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.SubjProp#getHwPhr <em>Hw Phr</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Hw Phr</em>'.
   * @see com.ge.research.sadl.sadl.SubjProp#getHwPhr()
   * @see #getSubjProp()
   * @generated
   */
  EReference getSubjProp_HwPhr();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.InstAttrSPV <em>Inst Attr SPV</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Inst Attr SPV</em>'.
   * @see com.ge.research.sadl.sadl.InstAttrSPV
   * @generated
   */
  EClass getInstAttrSPV();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InstAttrSPV#getSubj <em>Subj</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Subj</em>'.
   * @see com.ge.research.sadl.sadl.InstAttrSPV#getSubj()
   * @see #getInstAttrSPV()
   * @generated
   */
  EReference getInstAttrSPV_Subj();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.InstAttrSPV#getProps <em>Props</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Props</em>'.
   * @see com.ge.research.sadl.sadl.InstAttrSPV#getProps()
   * @see #getInstAttrSPV()
   * @generated
   */
  EReference getInstAttrSPV_Props();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.InstAttrSPV#getVals <em>Vals</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Vals</em>'.
   * @see com.ge.research.sadl.sadl.InstAttrSPV#getVals()
   * @see #getInstAttrSPV()
   * @generated
   */
  EReference getInstAttrSPV_Vals();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.InstAttrPSV <em>Inst Attr PSV</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Inst Attr PSV</em>'.
   * @see com.ge.research.sadl.sadl.InstAttrPSV
   * @generated
   */
  EClass getInstAttrPSV();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InstAttrPSV#getProp <em>Prop</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Prop</em>'.
   * @see com.ge.research.sadl.sadl.InstAttrPSV#getProp()
   * @see #getInstAttrPSV()
   * @generated
   */
  EReference getInstAttrPSV_Prop();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.InstAttrPSV#getVal <em>Val</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Val</em>'.
   * @see com.ge.research.sadl.sadl.InstAttrPSV#getVal()
   * @see #getInstAttrPSV()
   * @generated
   */
  EReference getInstAttrPSV_Val();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.SubTypeOf <em>Sub Type Of</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Sub Type Of</em>'.
   * @see com.ge.research.sadl.sadl.SubTypeOf
   * @generated
   */
  EClass getSubTypeOf();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.SubTypeOf#getSubclass <em>Subclass</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Subclass</em>'.
   * @see com.ge.research.sadl.sadl.SubTypeOf#getSubclass()
   * @see #getSubTypeOf()
   * @generated
   */
  EReference getSubTypeOf_Subclass();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.SubTypeOf#getSuperclass <em>Superclass</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Superclass</em>'.
   * @see com.ge.research.sadl.sadl.SubTypeOf#getSuperclass()
   * @see #getSubTypeOf()
   * @generated
   */
  EReference getSubTypeOf_Superclass();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.IntervalValue <em>Interval Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Interval Value</em>'.
   * @see com.ge.research.sadl.sadl.IntervalValue
   * @generated
   */
  EClass getIntervalValue();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.IntervalValue#getOp <em>Op</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Op</em>'.
   * @see com.ge.research.sadl.sadl.IntervalValue#getOp()
   * @see #getIntervalValue()
   * @generated
   */
  EAttribute getIntervalValue_Op();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.IntervalValue#getExpr <em>Expr</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Expr</em>'.
   * @see com.ge.research.sadl.sadl.IntervalValue#getExpr()
   * @see #getIntervalValue()
   * @generated
   */
  EReference getIntervalValue_Expr();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ExplicitValue <em>Explicit Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Explicit Value</em>'.
   * @see com.ge.research.sadl.sadl.ExplicitValue
   * @generated
   */
  EClass getExplicitValue();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ExplicitValue#getInstName <em>Inst Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Inst Name</em>'.
   * @see com.ge.research.sadl.sadl.ExplicitValue#getInstName()
   * @see #getExplicitValue()
   * @generated
   */
  EReference getExplicitValue_InstName();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ExplicitValue#getLitValue <em>Lit Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Lit Value</em>'.
   * @see com.ge.research.sadl.sadl.ExplicitValue#getLitValue()
   * @see #getExplicitValue()
   * @generated
   */
  EReference getExplicitValue_LitValue();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.ExplicitValue#getTerm <em>Term</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Term</em>'.
   * @see com.ge.research.sadl.sadl.ExplicitValue#getTerm()
   * @see #getExplicitValue()
   * @generated
   */
  EAttribute getExplicitValue_Term();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ValueTable <em>Value Table</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Value Table</em>'.
   * @see com.ge.research.sadl.sadl.ValueTable
   * @generated
   */
  EClass getValueTable();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.ValueTable#getRow <em>Row</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Row</em>'.
   * @see com.ge.research.sadl.sadl.ValueTable#getRow()
   * @see #getValueTable()
   * @generated
   */
  EReference getValueTable_Row();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.ValueTable#getRows <em>Rows</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Rows</em>'.
   * @see com.ge.research.sadl.sadl.ValueTable#getRows()
   * @see #getValueTable()
   * @generated
   */
  EReference getValueTable_Rows();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.LiteralValue <em>Literal Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Literal Value</em>'.
   * @see com.ge.research.sadl.sadl.LiteralValue
   * @generated
   */
  EClass getLiteralValue();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.LiteralValue#getLiteralNumber <em>Literal Number</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Literal Number</em>'.
   * @see com.ge.research.sadl.sadl.LiteralValue#getLiteralNumber()
   * @see #getLiteralValue()
   * @generated
   */
  EAttribute getLiteralValue_LiteralNumber();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.LiteralValue#getLiteralString <em>Literal String</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Literal String</em>'.
   * @see com.ge.research.sadl.sadl.LiteralValue#getLiteralString()
   * @see #getLiteralValue()
   * @generated
   */
  EAttribute getLiteralValue_LiteralString();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.LiteralValue#getLiteralBoolean <em>Literal Boolean</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Literal Boolean</em>'.
   * @see com.ge.research.sadl.sadl.LiteralValue#getLiteralBoolean()
   * @see #getLiteralValue()
   * @generated
   */
  EAttribute getLiteralValue_LiteralBoolean();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.ValueRow <em>Value Row</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Value Row</em>'.
   * @see com.ge.research.sadl.sadl.ValueRow
   * @generated
   */
  EClass getValueRow();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.sadl.ValueRow#getExplicitValues <em>Explicit Values</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Explicit Values</em>'.
   * @see com.ge.research.sadl.sadl.ValueRow#getExplicitValues()
   * @see #getValueRow()
   * @generated
   */
  EReference getValueRow_ExplicitValues();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.JunctionExpression <em>Junction Expression</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Junction Expression</em>'.
   * @see com.ge.research.sadl.sadl.JunctionExpression
   * @generated
   */
  EClass getJunctionExpression();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.JunctionExpression#getLeft <em>Left</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Left</em>'.
   * @see com.ge.research.sadl.sadl.JunctionExpression#getLeft()
   * @see #getJunctionExpression()
   * @generated
   */
  EReference getJunctionExpression_Left();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.JunctionExpression#getOp <em>Op</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Op</em>'.
   * @see com.ge.research.sadl.sadl.JunctionExpression#getOp()
   * @see #getJunctionExpression()
   * @generated
   */
  EAttribute getJunctionExpression_Op();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.JunctionExpression#getRight <em>Right</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Right</em>'.
   * @see com.ge.research.sadl.sadl.JunctionExpression#getRight()
   * @see #getJunctionExpression()
   * @generated
   */
  EReference getJunctionExpression_Right();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.BinaryOpExpression <em>Binary Op Expression</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Binary Op Expression</em>'.
   * @see com.ge.research.sadl.sadl.BinaryOpExpression
   * @generated
   */
  EClass getBinaryOpExpression();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.BinaryOpExpression#getLeft <em>Left</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Left</em>'.
   * @see com.ge.research.sadl.sadl.BinaryOpExpression#getLeft()
   * @see #getBinaryOpExpression()
   * @generated
   */
  EReference getBinaryOpExpression_Left();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.BinaryOpExpression#getOp <em>Op</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Op</em>'.
   * @see com.ge.research.sadl.sadl.BinaryOpExpression#getOp()
   * @see #getBinaryOpExpression()
   * @generated
   */
  EAttribute getBinaryOpExpression_Op();

  /**
   * Returns the meta object for the containment reference '{@link com.ge.research.sadl.sadl.BinaryOpExpression#getRight <em>Right</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Right</em>'.
   * @see com.ge.research.sadl.sadl.BinaryOpExpression#getRight()
   * @see #getBinaryOpExpression()
   * @generated
   */
  EReference getBinaryOpExpression_Right();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.sadl.UnaryOpExpression <em>Unary Op Expression</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Unary Op Expression</em>'.
   * @see com.ge.research.sadl.sadl.UnaryOpExpression
   * @generated
   */
  EClass getUnaryOpExpression();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.sadl.UnaryOpExpression#getOp <em>Op</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Op</em>'.
   * @see com.ge.research.sadl.sadl.UnaryOpExpression#getOp()
   * @see #getUnaryOpExpression()
   * @generated
   */
  EAttribute getUnaryOpExpression_Op();

  /**
   * Returns the meta object for enum '{@link com.ge.research.sadl.sadl.DataType <em>Data Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for enum '<em>Data Type</em>'.
   * @see com.ge.research.sadl.sadl.DataType
   * @generated
   */
  EEnum getDataType();

  /**
   * Returns the factory that creates the instances of the model.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the factory that creates the instances of the model.
   * @generated
   */
  SadlFactory getSadlFactory();

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
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ModelImpl <em>Model</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ModelImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getModel()
     * @generated
     */
    EClass MODEL = eINSTANCE.getModel();

    /**
     * The meta object literal for the '<em><b>Model Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MODEL__MODEL_NAME = eINSTANCE.getModel_ModelName();

    /**
     * The meta object literal for the '<em><b>Imports</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MODEL__IMPORTS = eINSTANCE.getModel_Imports();

    /**
     * The meta object literal for the '<em><b>Elements</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MODEL__ELEMENTS = eINSTANCE.getModel_Elements();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ModelNameImpl <em>Model Name</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ModelNameImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getModelName()
     * @generated
     */
    EClass MODEL_NAME = eINSTANCE.getModelName();

    /**
     * The meta object literal for the '<em><b>Base Uri</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute MODEL_NAME__BASE_URI = eINSTANCE.getModelName_BaseUri();

    /**
     * The meta object literal for the '<em><b>Alias</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute MODEL_NAME__ALIAS = eINSTANCE.getModelName_Alias();

    /**
     * The meta object literal for the '<em><b>Version</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute MODEL_NAME__VERSION = eINSTANCE.getModelName_Version();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ImportImpl <em>Import</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ImportImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getImport()
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
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ModelElementImpl <em>Model Element</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ModelElementImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getModelElement()
     * @generated
     */
    EClass MODEL_ELEMENT = eINSTANCE.getModelElement();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.StatementImpl <em>Statement</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.StatementImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getStatement()
     * @generated
     */
    EClass STATEMENT = eINSTANCE.getStatement();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ResourceNameImpl <em>Resource Name</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ResourceNameImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceName()
     * @generated
     */
    EClass RESOURCE_NAME = eINSTANCE.getResourceName();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute RESOURCE_NAME__NAME = eINSTANCE.getResourceName_Name();

    /**
     * The meta object literal for the '<em><b>Ann Type</b></em>' attribute list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute RESOURCE_NAME__ANN_TYPE = eINSTANCE.getResourceName_AnnType();

    /**
     * The meta object literal for the '<em><b>Ann Content</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference RESOURCE_NAME__ANN_CONTENT = eINSTANCE.getResourceName_AnnContent();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ContentListImpl <em>Content List</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ContentListImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getContentList()
     * @generated
     */
    EClass CONTENT_LIST = eINSTANCE.getContentList();

    /**
     * The meta object literal for the '<em><b>Ann Content</b></em>' attribute list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute CONTENT_LIST__ANN_CONTENT = eINSTANCE.getContentList_AnnContent();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ResourceListImpl <em>Resource List</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ResourceListImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceList()
     * @generated
     */
    EClass RESOURCE_LIST = eINSTANCE.getResourceList();

    /**
     * The meta object literal for the '<em><b>Names</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference RESOURCE_LIST__NAMES = eINSTANCE.getResourceList_Names();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.LiteralListImpl <em>Literal List</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.LiteralListImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getLiteralList()
     * @generated
     */
    EClass LITERAL_LIST = eINSTANCE.getLiteralList();

    /**
     * The meta object literal for the '<em><b>Literals</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference LITERAL_LIST__LITERALS = eINSTANCE.getLiteralList_Literals();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ResourceByNameImpl <em>Resource By Name</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ResourceByNameImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceByName()
     * @generated
     */
    EClass RESOURCE_BY_NAME = eINSTANCE.getResourceByName();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference RESOURCE_BY_NAME__NAME = eINSTANCE.getResourceByName_Name();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ExistingResourceListImpl <em>Existing Resource List</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ExistingResourceListImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExistingResourceList()
     * @generated
     */
    EClass EXISTING_RESOURCE_LIST = eINSTANCE.getExistingResourceList();

    /**
     * The meta object literal for the '<em><b>Names</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXISTING_RESOURCE_LIST__NAMES = eINSTANCE.getExistingResourceList_Names();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ResourceIdentifierImpl <em>Resource Identifier</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ResourceIdentifierImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceIdentifier()
     * @generated
     */
    EClass RESOURCE_IDENTIFIER = eINSTANCE.getResourceIdentifier();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ResourceBySetOpImpl <em>Resource By Set Op</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ResourceBySetOpImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceBySetOp()
     * @generated
     */
    EClass RESOURCE_BY_SET_OP = eINSTANCE.getResourceBySetOp();

    /**
     * The meta object literal for the '<em><b>Ann Type</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute RESOURCE_BY_SET_OP__ANN_TYPE = eINSTANCE.getResourceBySetOp_AnnType();

    /**
     * The meta object literal for the '<em><b>Ann Content</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute RESOURCE_BY_SET_OP__ANN_CONTENT = eINSTANCE.getResourceBySetOp_AnnContent();

    /**
     * The meta object literal for the '<em><b>Names</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference RESOURCE_BY_SET_OP__NAMES = eINSTANCE.getResourceBySetOp_Names();

    /**
     * The meta object literal for the '<em><b>Op</b></em>' attribute list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute RESOURCE_BY_SET_OP__OP = eINSTANCE.getResourceBySetOp_Op();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ResourceByRestrictionImpl <em>Resource By Restriction</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ResourceByRestrictionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getResourceByRestriction()
     * @generated
     */
    EClass RESOURCE_BY_RESTRICTION = eINSTANCE.getResourceByRestriction();

    /**
     * The meta object literal for the '<em><b>Ann Type</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute RESOURCE_BY_RESTRICTION__ANN_TYPE = eINSTANCE.getResourceByRestriction_AnnType();

    /**
     * The meta object literal for the '<em><b>Ann Content</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute RESOURCE_BY_RESTRICTION__ANN_CONTENT = eINSTANCE.getResourceByRestriction_AnnContent();

    /**
     * The meta object literal for the '<em><b>Prop Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference RESOURCE_BY_RESTRICTION__PROP_NAME = eINSTANCE.getResourceByRestriction_PropName();

    /**
     * The meta object literal for the '<em><b>Cond</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference RESOURCE_BY_RESTRICTION__COND = eINSTANCE.getResourceByRestriction_Cond();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.UnionResourceImpl <em>Union Resource</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.UnionResourceImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getUnionResource()
     * @generated
     */
    EClass UNION_RESOURCE = eINSTANCE.getUnionResource();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.IntersectionResourceImpl <em>Intersection Resource</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.IntersectionResourceImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getIntersectionResource()
     * @generated
     */
    EClass INTERSECTION_RESOURCE = eINSTANCE.getIntersectionResource();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ClassDeclarationImpl <em>Class Declaration</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ClassDeclarationImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getClassDeclaration()
     * @generated
     */
    EClass CLASS_DECLARATION = eINSTANCE.getClassDeclaration();

    /**
     * The meta object literal for the '<em><b>Class Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CLASS_DECLARATION__CLASS_NAME = eINSTANCE.getClassDeclaration_ClassName();

    /**
     * The meta object literal for the '<em><b>Must Be One Of</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CLASS_DECLARATION__MUST_BE_ONE_OF = eINSTANCE.getClassDeclaration_MustBeOneOf();

    /**
     * The meta object literal for the '<em><b>Described By</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CLASS_DECLARATION__DESCRIBED_BY = eINSTANCE.getClassDeclaration_DescribedBy();

    /**
     * The meta object literal for the '<em><b>Class List</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CLASS_DECLARATION__CLASS_LIST = eINSTANCE.getClassDeclaration_ClassList();

    /**
     * The meta object literal for the '<em><b>Class Identifier</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CLASS_DECLARATION__CLASS_IDENTIFIER = eINSTANCE.getClassDeclaration_ClassIdentifier();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.EnumeratedInstancesImpl <em>Enumerated Instances</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.EnumeratedInstancesImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getEnumeratedInstances()
     * @generated
     */
    EClass ENUMERATED_INSTANCES = eINSTANCE.getEnumeratedInstances();

    /**
     * The meta object literal for the '<em><b>Instance List</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ENUMERATED_INSTANCES__INSTANCE_LIST = eINSTANCE.getEnumeratedInstances_InstanceList();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.AddlClassInfoImpl <em>Addl Class Info</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.AddlClassInfoImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getAddlClassInfo()
     * @generated
     */
    EClass ADDL_CLASS_INFO = eINSTANCE.getAddlClassInfo();

    /**
     * The meta object literal for the '<em><b>Property By Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ADDL_CLASS_INFO__PROPERTY_BY_NAME = eINSTANCE.getAddlClassInfo_PropertyByName();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ADDL_CLASS_INFO__PROPERTY_NAME = eINSTANCE.getAddlClassInfo_PropertyName();

    /**
     * The meta object literal for the '<em><b>Range</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ADDL_CLASS_INFO__RANGE = eINSTANCE.getAddlClassInfo_Range();

    /**
     * The meta object literal for the '<em><b>Restriction</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ADDL_CLASS_INFO__RESTRICTION = eINSTANCE.getAddlClassInfo_Restriction();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.RangeImpl <em>Range</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.RangeImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getRange()
     * @generated
     */
    EClass RANGE = eINSTANCE.getRange();

    /**
     * The meta object literal for the '<em><b>Single</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute RANGE__SINGLE = eINSTANCE.getRange_Single();

    /**
     * The meta object literal for the '<em><b>Type</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference RANGE__TYPE = eINSTANCE.getRange_Type();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.RangeTypeImpl <em>Range Type</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.RangeTypeImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getRangeType()
     * @generated
     */
    EClass RANGE_TYPE = eINSTANCE.getRangeType();

    /**
     * The meta object literal for the '<em><b>Class Identifier</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference RANGE_TYPE__CLASS_IDENTIFIER = eINSTANCE.getRangeType_ClassIdentifier();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.UserDefinedDataTypeImpl <em>User Defined Data Type</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.UserDefinedDataTypeImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getUserDefinedDataType()
     * @generated
     */
    EClass USER_DEFINED_DATA_TYPE = eINSTANCE.getUserDefinedDataType();

    /**
     * The meta object literal for the '<em><b>Udt</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute USER_DEFINED_DATA_TYPE__UDT = eINSTANCE.getUserDefinedDataType_Udt();

    /**
     * The meta object literal for the '<em><b>Restriction</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference USER_DEFINED_DATA_TYPE__RESTRICTION = eINSTANCE.getUserDefinedDataType_Restriction();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.DataTypeRestrictionImpl <em>Data Type Restriction</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.DataTypeRestrictionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getDataTypeRestriction()
     * @generated
     */
    EClass DATA_TYPE_RESTRICTION = eINSTANCE.getDataTypeRestriction();

    /**
     * The meta object literal for the '<em><b>Basetype</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute DATA_TYPE_RESTRICTION__BASETYPE = eINSTANCE.getDataTypeRestriction_Basetype();

    /**
     * The meta object literal for the '<em><b>Facets</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference DATA_TYPE_RESTRICTION__FACETS = eINSTANCE.getDataTypeRestriction_Facets();

    /**
     * The meta object literal for the '<em><b>Basetypes</b></em>' attribute list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute DATA_TYPE_RESTRICTION__BASETYPES = eINSTANCE.getDataTypeRestriction_Basetypes();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.FacetsImpl <em>Facets</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.FacetsImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getFacets()
     * @generated
     */
    EClass FACETS = eINSTANCE.getFacets();

    /**
     * The meta object literal for the '<em><b>Minexin</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FACETS__MINEXIN = eINSTANCE.getFacets_Minexin();

    /**
     * The meta object literal for the '<em><b>Min</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FACETS__MIN = eINSTANCE.getFacets_Min();

    /**
     * The meta object literal for the '<em><b>Max</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FACETS__MAX = eINSTANCE.getFacets_Max();

    /**
     * The meta object literal for the '<em><b>Maxexin</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FACETS__MAXEXIN = eINSTANCE.getFacets_Maxexin();

    /**
     * The meta object literal for the '<em><b>Regex</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FACETS__REGEX = eINSTANCE.getFacets_Regex();

    /**
     * The meta object literal for the '<em><b>Len</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FACETS__LEN = eINSTANCE.getFacets_Len();

    /**
     * The meta object literal for the '<em><b>Minlen</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FACETS__MINLEN = eINSTANCE.getFacets_Minlen();

    /**
     * The meta object literal for the '<em><b>Maxlen</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FACETS__MAXLEN = eINSTANCE.getFacets_Maxlen();

    /**
     * The meta object literal for the '<em><b>Values</b></em>' attribute list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FACETS__VALUES = eINSTANCE.getFacets_Values();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.EquivalentConceptsImpl <em>Equivalent Concepts</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.EquivalentConceptsImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getEquivalentConcepts()
     * @generated
     */
    EClass EQUIVALENT_CONCEPTS = eINSTANCE.getEquivalentConcepts();

    /**
     * The meta object literal for the '<em><b>Class1</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EQUIVALENT_CONCEPTS__CLASS1 = eINSTANCE.getEquivalentConcepts_Class1();

    /**
     * The meta object literal for the '<em><b>Class2</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EQUIVALENT_CONCEPTS__CLASS2 = eINSTANCE.getEquivalentConcepts_Class2();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.DisjointClassesImpl <em>Disjoint Classes</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.DisjointClassesImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getDisjointClasses()
     * @generated
     */
    EClass DISJOINT_CLASSES = eINSTANCE.getDisjointClasses();

    /**
     * The meta object literal for the '<em><b>Class1</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference DISJOINT_CLASSES__CLASS1 = eINSTANCE.getDisjointClasses_Class1();

    /**
     * The meta object literal for the '<em><b>Class2</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference DISJOINT_CLASSES__CLASS2 = eINSTANCE.getDisjointClasses_Class2();

    /**
     * The meta object literal for the '<em><b>Classes</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference DISJOINT_CLASSES__CLASSES = eINSTANCE.getDisjointClasses_Classes();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ComplementOfClassImpl <em>Complement Of Class</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ComplementOfClassImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getComplementOfClass()
     * @generated
     */
    EClass COMPLEMENT_OF_CLASS = eINSTANCE.getComplementOfClass();

    /**
     * The meta object literal for the '<em><b>Class1</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference COMPLEMENT_OF_CLASS__CLASS1 = eINSTANCE.getComplementOfClass_Class1();

    /**
     * The meta object literal for the '<em><b>Class2</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference COMPLEMENT_OF_CLASS__CLASS2 = eINSTANCE.getComplementOfClass_Class2();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.AllValuesFromImpl <em>All Values From</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.AllValuesFromImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getAllValuesFrom()
     * @generated
     */
    EClass ALL_VALUES_FROM = eINSTANCE.getAllValuesFrom();

    /**
     * The meta object literal for the '<em><b>Restricted</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ALL_VALUES_FROM__RESTRICTED = eINSTANCE.getAllValuesFrom_Restricted();

    /**
     * The meta object literal for the '<em><b>Cond</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ALL_VALUES_FROM__COND = eINSTANCE.getAllValuesFrom_Cond();

    /**
     * The meta object literal for the '<em><b>Class Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ALL_VALUES_FROM__CLASS_NAME = eINSTANCE.getAllValuesFrom_ClassName();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ALL_VALUES_FROM__PROPERTY_NAME = eINSTANCE.getAllValuesFrom_PropertyName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.SomeValuesFromImpl <em>Some Values From</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.SomeValuesFromImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSomeValuesFrom()
     * @generated
     */
    EClass SOME_VALUES_FROM = eINSTANCE.getSomeValuesFrom();

    /**
     * The meta object literal for the '<em><b>Restricted</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SOME_VALUES_FROM__RESTRICTED = eINSTANCE.getSomeValuesFrom_Restricted();

    /**
     * The meta object literal for the '<em><b>Cond</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SOME_VALUES_FROM__COND = eINSTANCE.getSomeValuesFrom_Cond();

    /**
     * The meta object literal for the '<em><b>Class Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SOME_VALUES_FROM__CLASS_NAME = eINSTANCE.getSomeValuesFrom_ClassName();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SOME_VALUES_FROM__PROPERTY_NAME = eINSTANCE.getSomeValuesFrom_PropertyName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.HasValueImpl <em>Has Value</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.HasValueImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getHasValue()
     * @generated
     */
    EClass HAS_VALUE = eINSTANCE.getHasValue();

    /**
     * The meta object literal for the '<em><b>Restricted</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference HAS_VALUE__RESTRICTED = eINSTANCE.getHasValue_Restricted();

    /**
     * The meta object literal for the '<em><b>Cond</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference HAS_VALUE__COND = eINSTANCE.getHasValue_Cond();

    /**
     * The meta object literal for the '<em><b>Class Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference HAS_VALUE__CLASS_NAME = eINSTANCE.getHasValue_ClassName();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference HAS_VALUE__PROPERTY_NAME = eINSTANCE.getHasValue_PropertyName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.CardinalityImpl <em>Cardinality</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.CardinalityImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getCardinality()
     * @generated
     */
    EClass CARDINALITY = eINSTANCE.getCardinality();

    /**
     * The meta object literal for the '<em><b>Restricted</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CARDINALITY__RESTRICTED = eINSTANCE.getCardinality_Restricted();

    /**
     * The meta object literal for the '<em><b>Cond</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CARDINALITY__COND = eINSTANCE.getCardinality_Cond();

    /**
     * The meta object literal for the '<em><b>Class Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CARDINALITY__CLASS_NAME = eINSTANCE.getCardinality_ClassName();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CARDINALITY__PROPERTY_NAME = eINSTANCE.getCardinality_PropertyName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.MinCardinalityImpl <em>Min Cardinality</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.MinCardinalityImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getMinCardinality()
     * @generated
     */
    EClass MIN_CARDINALITY = eINSTANCE.getMinCardinality();

    /**
     * The meta object literal for the '<em><b>Restricted</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MIN_CARDINALITY__RESTRICTED = eINSTANCE.getMinCardinality_Restricted();

    /**
     * The meta object literal for the '<em><b>Cond</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MIN_CARDINALITY__COND = eINSTANCE.getMinCardinality_Cond();

    /**
     * The meta object literal for the '<em><b>Class Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MIN_CARDINALITY__CLASS_NAME = eINSTANCE.getMinCardinality_ClassName();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MIN_CARDINALITY__PROPERTY_NAME = eINSTANCE.getMinCardinality_PropertyName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.MaxCardinalityImpl <em>Max Cardinality</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.MaxCardinalityImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getMaxCardinality()
     * @generated
     */
    EClass MAX_CARDINALITY = eINSTANCE.getMaxCardinality();

    /**
     * The meta object literal for the '<em><b>Restricted</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MAX_CARDINALITY__RESTRICTED = eINSTANCE.getMaxCardinality_Restricted();

    /**
     * The meta object literal for the '<em><b>Cond</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MAX_CARDINALITY__COND = eINSTANCE.getMaxCardinality_Cond();

    /**
     * The meta object literal for the '<em><b>Class Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MAX_CARDINALITY__CLASS_NAME = eINSTANCE.getMaxCardinality_ClassName();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MAX_CARDINALITY__PROPERTY_NAME = eINSTANCE.getMaxCardinality_PropertyName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.PropertyOfClassImpl <em>Property Of Class</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.PropertyOfClassImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getPropertyOfClass()
     * @generated
     */
    EClass PROPERTY_OF_CLASS = eINSTANCE.getPropertyOfClass();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROPERTY_OF_CLASS__PROPERTY_NAME = eINSTANCE.getPropertyOfClass_PropertyName();

    /**
     * The meta object literal for the '<em><b>Class Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROPERTY_OF_CLASS__CLASS_NAME = eINSTANCE.getPropertyOfClass_ClassName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.AllValuesConditionImpl <em>All Values Condition</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.AllValuesConditionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getAllValuesCondition()
     * @generated
     */
    EClass ALL_VALUES_CONDITION = eINSTANCE.getAllValuesCondition();

    /**
     * The meta object literal for the '<em><b>Restriction</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ALL_VALUES_CONDITION__RESTRICTION = eINSTANCE.getAllValuesCondition_Restriction();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.EnumeratedAllValuesFromImpl <em>Enumerated All Values From</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.EnumeratedAllValuesFromImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getEnumeratedAllValuesFrom()
     * @generated
     */
    EClass ENUMERATED_ALL_VALUES_FROM = eINSTANCE.getEnumeratedAllValuesFrom();

    /**
     * The meta object literal for the '<em><b>Restricted</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ENUMERATED_ALL_VALUES_FROM__RESTRICTED = eINSTANCE.getEnumeratedAllValuesFrom_Restricted();

    /**
     * The meta object literal for the '<em><b>Enumeration</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ENUMERATED_ALL_VALUES_FROM__ENUMERATION = eINSTANCE.getEnumeratedAllValuesFrom_Enumeration();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.EnumeratedAllAndSomeValuesFromImpl <em>Enumerated All And Some Values From</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.EnumeratedAllAndSomeValuesFromImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getEnumeratedAllAndSomeValuesFrom()
     * @generated
     */
    EClass ENUMERATED_ALL_AND_SOME_VALUES_FROM = eINSTANCE.getEnumeratedAllAndSomeValuesFrom();

    /**
     * The meta object literal for the '<em><b>Restricted</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ENUMERATED_ALL_AND_SOME_VALUES_FROM__RESTRICTED = eINSTANCE.getEnumeratedAllAndSomeValuesFrom_Restricted();

    /**
     * The meta object literal for the '<em><b>Enumeration</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ENUMERATED_ALL_AND_SOME_VALUES_FROM__ENUMERATION = eINSTANCE.getEnumeratedAllAndSomeValuesFrom_Enumeration();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.DefaultValueImpl <em>Default Value</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.DefaultValueImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getDefaultValue()
     * @generated
     */
    EClass DEFAULT_VALUE = eINSTANCE.getDefaultValue();

    /**
     * The meta object literal for the '<em><b>Def Value Class</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference DEFAULT_VALUE__DEF_VALUE_CLASS = eINSTANCE.getDefaultValue_DefValueClass();

    /**
     * The meta object literal for the '<em><b>Level</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute DEFAULT_VALUE__LEVEL = eINSTANCE.getDefaultValue_Level();

    /**
     * The meta object literal for the '<em><b>Def Value</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference DEFAULT_VALUE__DEF_VALUE = eINSTANCE.getDefaultValue_DefValue();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.SomeValuesConditionImpl <em>Some Values Condition</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.SomeValuesConditionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSomeValuesCondition()
     * @generated
     */
    EClass SOME_VALUES_CONDITION = eINSTANCE.getSomeValuesCondition();

    /**
     * The meta object literal for the '<em><b>Restriction</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SOME_VALUES_CONDITION__RESTRICTION = eINSTANCE.getSomeValuesCondition_Restriction();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.HasValueConditionImpl <em>Has Value Condition</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.HasValueConditionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getHasValueCondition()
     * @generated
     */
    EClass HAS_VALUE_CONDITION = eINSTANCE.getHasValueCondition();

    /**
     * The meta object literal for the '<em><b>Restriction</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference HAS_VALUE_CONDITION__RESTRICTION = eINSTANCE.getHasValueCondition_Restriction();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.MinCardConditionImpl <em>Min Card Condition</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.MinCardConditionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getMinCardCondition()
     * @generated
     */
    EClass MIN_CARD_CONDITION = eINSTANCE.getMinCardCondition();

    /**
     * The meta object literal for the '<em><b>Card</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute MIN_CARD_CONDITION__CARD = eINSTANCE.getMinCardCondition_Card();

    /**
     * The meta object literal for the '<em><b>Class Qualifier</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MIN_CARD_CONDITION__CLASS_QUALIFIER = eINSTANCE.getMinCardCondition_ClassQualifier();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.MaxCardConditionImpl <em>Max Card Condition</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.MaxCardConditionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getMaxCardCondition()
     * @generated
     */
    EClass MAX_CARD_CONDITION = eINSTANCE.getMaxCardCondition();

    /**
     * The meta object literal for the '<em><b>Card</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute MAX_CARD_CONDITION__CARD = eINSTANCE.getMaxCardCondition_Card();

    /**
     * The meta object literal for the '<em><b>Class Qualifier</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MAX_CARD_CONDITION__CLASS_QUALIFIER = eINSTANCE.getMaxCardCondition_ClassQualifier();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.CardConditionImpl <em>Card Condition</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.CardConditionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getCardCondition()
     * @generated
     */
    EClass CARD_CONDITION = eINSTANCE.getCardCondition();

    /**
     * The meta object literal for the '<em><b>Card</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute CARD_CONDITION__CARD = eINSTANCE.getCardCondition_Card();

    /**
     * The meta object literal for the '<em><b>Class Qualifier</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CARD_CONDITION__CLASS_QUALIFIER = eINSTANCE.getCardCondition_ClassQualifier();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.NecessaryAndSufficientImpl <em>Necessary And Sufficient</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.NecessaryAndSufficientImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getNecessaryAndSufficient()
     * @generated
     */
    EClass NECESSARY_AND_SUFFICIENT = eINSTANCE.getNecessaryAndSufficient();

    /**
     * The meta object literal for the '<em><b>Super Class</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference NECESSARY_AND_SUFFICIENT__SUPER_CLASS = eINSTANCE.getNecessaryAndSufficient_SuperClass();

    /**
     * The meta object literal for the '<em><b>Article</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute NECESSARY_AND_SUFFICIENT__ARTICLE = eINSTANCE.getNecessaryAndSufficient_Article();

    /**
     * The meta object literal for the '<em><b>Sub Class</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference NECESSARY_AND_SUFFICIENT__SUB_CLASS = eINSTANCE.getNecessaryAndSufficient_SubClass();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference NECESSARY_AND_SUFFICIENT__PROPERTY_NAME = eINSTANCE.getNecessaryAndSufficient_PropertyName();

    /**
     * The meta object literal for the '<em><b>Cond</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference NECESSARY_AND_SUFFICIENT__COND = eINSTANCE.getNecessaryAndSufficient_Cond();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ConditionImpl <em>Condition</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ConditionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getCondition()
     * @generated
     */
    EClass CONDITION = eINSTANCE.getCondition();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.PropertyDeclarationImpl <em>Property Declaration</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.PropertyDeclarationImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getPropertyDeclaration()
     * @generated
     */
    EClass PROPERTY_DECLARATION = eINSTANCE.getPropertyDeclaration();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROPERTY_DECLARATION__PROPERTY_NAME = eINSTANCE.getPropertyDeclaration_PropertyName();

    /**
     * The meta object literal for the '<em><b>Super Prop Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROPERTY_DECLARATION__SUPER_PROP_NAME = eINSTANCE.getPropertyDeclaration_SuperPropName();

    /**
     * The meta object literal for the '<em><b>Addl Prop Info</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROPERTY_DECLARATION__ADDL_PROP_INFO = eINSTANCE.getPropertyDeclaration_AddlPropInfo();

    /**
     * The meta object literal for the '<em><b>Article</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute PROPERTY_DECLARATION__ARTICLE = eINSTANCE.getPropertyDeclaration_Article();

    /**
     * The meta object literal for the '<em><b>Domain</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROPERTY_DECLARATION__DOMAIN = eINSTANCE.getPropertyDeclaration_Domain();

    /**
     * The meta object literal for the '<em><b>Range Resource</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROPERTY_DECLARATION__RANGE_RESOURCE = eINSTANCE.getPropertyDeclaration_RangeResource();

    /**
     * The meta object literal for the '<em><b>Annotation Property</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROPERTY_DECLARATION__ANNOTATION_PROPERTY = eINSTANCE.getPropertyDeclaration_AnnotationProperty();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl <em>Additional Property Info</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getAdditionalPropertyInfo()
     * @generated
     */
    EClass ADDITIONAL_PROPERTY_INFO = eINSTANCE.getAdditionalPropertyInfo();

    /**
     * The meta object literal for the '<em><b>Domain</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ADDITIONAL_PROPERTY_INFO__DOMAIN = eINSTANCE.getAdditionalPropertyInfo_Domain();

    /**
     * The meta object literal for the '<em><b>Cond</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ADDITIONAL_PROPERTY_INFO__COND = eINSTANCE.getAdditionalPropertyInfo_Cond();

    /**
     * The meta object literal for the '<em><b>Range</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ADDITIONAL_PROPERTY_INFO__RANGE = eINSTANCE.getAdditionalPropertyInfo_Range();

    /**
     * The meta object literal for the '<em><b>Isfunc</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ADDITIONAL_PROPERTY_INFO__ISFUNC = eINSTANCE.getAdditionalPropertyInfo_Isfunc();

    /**
     * The meta object literal for the '<em><b>Isinvfunc</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ADDITIONAL_PROPERTY_INFO__ISINVFUNC = eINSTANCE.getAdditionalPropertyInfo_Isinvfunc();

    /**
     * The meta object literal for the '<em><b>Is Sym</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ADDITIONAL_PROPERTY_INFO__IS_SYM = eINSTANCE.getAdditionalPropertyInfo_IsSym();

    /**
     * The meta object literal for the '<em><b>Is Trans</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ADDITIONAL_PROPERTY_INFO__IS_TRANS = eINSTANCE.getAdditionalPropertyInfo_IsTrans();

    /**
     * The meta object literal for the '<em><b>Is Inv Of</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ADDITIONAL_PROPERTY_INFO__IS_INV_OF = eINSTANCE.getAdditionalPropertyInfo_IsInvOf();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.FunctionalPropertyImpl <em>Functional Property</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.FunctionalPropertyImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getFunctionalProperty()
     * @generated
     */
    EClass FUNCTIONAL_PROPERTY = eINSTANCE.getFunctionalProperty();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference FUNCTIONAL_PROPERTY__PROPERTY_NAME = eINSTANCE.getFunctionalProperty_PropertyName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.InverseFunctionalPropertyImpl <em>Inverse Functional Property</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.InverseFunctionalPropertyImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInverseFunctionalProperty()
     * @generated
     */
    EClass INVERSE_FUNCTIONAL_PROPERTY = eINSTANCE.getInverseFunctionalProperty();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INVERSE_FUNCTIONAL_PROPERTY__PROPERTY_NAME = eINSTANCE.getInverseFunctionalProperty_PropertyName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.SymmetricalPropertyImpl <em>Symmetrical Property</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.SymmetricalPropertyImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSymmetricalProperty()
     * @generated
     */
    EClass SYMMETRICAL_PROPERTY = eINSTANCE.getSymmetricalProperty();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SYMMETRICAL_PROPERTY__PROPERTY_NAME = eINSTANCE.getSymmetricalProperty_PropertyName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.TransitivePropertyImpl <em>Transitive Property</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.TransitivePropertyImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getTransitiveProperty()
     * @generated
     */
    EClass TRANSITIVE_PROPERTY = eINSTANCE.getTransitiveProperty();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference TRANSITIVE_PROPERTY__PROPERTY_NAME = eINSTANCE.getTransitiveProperty_PropertyName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.InversePropertyImpl <em>Inverse Property</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.InversePropertyImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInverseProperty()
     * @generated
     */
    EClass INVERSE_PROPERTY = eINSTANCE.getInverseProperty();

    /**
     * The meta object literal for the '<em><b>Property Name1</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INVERSE_PROPERTY__PROPERTY_NAME1 = eINSTANCE.getInverseProperty_PropertyName1();

    /**
     * The meta object literal for the '<em><b>Inv Of</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INVERSE_PROPERTY__INV_OF = eINSTANCE.getInverseProperty_InvOf();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.IsInverseOfImpl <em>Is Inverse Of</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.IsInverseOfImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getIsInverseOf()
     * @generated
     */
    EClass IS_INVERSE_OF = eINSTANCE.getIsInverseOf();

    /**
     * The meta object literal for the '<em><b>Property Name2</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference IS_INVERSE_OF__PROPERTY_NAME2 = eINSTANCE.getIsInverseOf_PropertyName2();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.TypedBNodeImpl <em>Typed BNode</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.TypedBNodeImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getTypedBNode()
     * @generated
     */
    EClass TYPED_BNODE = eINSTANCE.getTypedBNode();

    /**
     * The meta object literal for the '<em><b>Article</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute TYPED_BNODE__ARTICLE = eINSTANCE.getTypedBNode_Article();

    /**
     * The meta object literal for the '<em><b>Class Identifier</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference TYPED_BNODE__CLASS_IDENTIFIER = eINSTANCE.getTypedBNode_ClassIdentifier();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.InstanceDeclarationStatementImpl <em>Instance Declaration Statement</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.InstanceDeclarationStatementImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstanceDeclarationStatement()
     * @generated
     */
    EClass INSTANCE_DECLARATION_STATEMENT = eINSTANCE.getInstanceDeclarationStatement();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.InstanceDeclarationImpl <em>Instance Declaration</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.InstanceDeclarationImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstanceDeclaration()
     * @generated
     */
    EClass INSTANCE_DECLARATION = eINSTANCE.getInstanceDeclaration();

    /**
     * The meta object literal for the '<em><b>Type Decl</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INSTANCE_DECLARATION__TYPE_DECL = eINSTANCE.getInstanceDeclaration_TypeDecl();

    /**
     * The meta object literal for the '<em><b>Addl Info Items</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INSTANCE_DECLARATION__ADDL_INFO_ITEMS = eINSTANCE.getInstanceDeclaration_AddlInfoItems();

    /**
     * The meta object literal for the '<em><b>Article</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute INSTANCE_DECLARATION__ARTICLE = eINSTANCE.getInstanceDeclaration_Article();

    /**
     * The meta object literal for the '<em><b>Class Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INSTANCE_DECLARATION__CLASS_NAME = eINSTANCE.getInstanceDeclaration_ClassName();

    /**
     * The meta object literal for the '<em><b>Instance Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INSTANCE_DECLARATION__INSTANCE_NAME = eINSTANCE.getInstanceDeclaration_InstanceName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.TypeDeclarationImpl <em>Type Declaration</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.TypeDeclarationImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getTypeDeclaration()
     * @generated
     */
    EClass TYPE_DECLARATION = eINSTANCE.getTypeDeclaration();

    /**
     * The meta object literal for the '<em><b>Inst Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference TYPE_DECLARATION__INST_NAME = eINSTANCE.getTypeDeclaration_InstName();

    /**
     * The meta object literal for the '<em><b>Type</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference TYPE_DECLARATION__TYPE = eINSTANCE.getTypeDeclaration_Type();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.InstanceDifferentFromImpl <em>Instance Different From</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.InstanceDifferentFromImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstanceDifferentFrom()
     * @generated
     */
    EClass INSTANCE_DIFFERENT_FROM = eINSTANCE.getInstanceDifferentFrom();

    /**
     * The meta object literal for the '<em><b>Inst Name1</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INSTANCE_DIFFERENT_FROM__INST_NAME1 = eINSTANCE.getInstanceDifferentFrom_InstName1();

    /**
     * The meta object literal for the '<em><b>Inst Name2</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INSTANCE_DIFFERENT_FROM__INST_NAME2 = eINSTANCE.getInstanceDifferentFrom_InstName2();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.InstancesAllDifferentImpl <em>Instances All Different</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.InstancesAllDifferentImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstancesAllDifferent()
     * @generated
     */
    EClass INSTANCES_ALL_DIFFERENT = eINSTANCE.getInstancesAllDifferent();

    /**
     * The meta object literal for the '<em><b>Instances</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INSTANCES_ALL_DIFFERENT__INSTANCES = eINSTANCE.getInstancesAllDifferent_Instances();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ExistingInstanceAttributionImpl <em>Existing Instance Attribution</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ExistingInstanceAttributionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExistingInstanceAttribution()
     * @generated
     */
    EClass EXISTING_INSTANCE_ATTRIBUTION = eINSTANCE.getExistingInstanceAttribution();

    /**
     * The meta object literal for the '<em><b>Subj</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXISTING_INSTANCE_ATTRIBUTION__SUBJ = eINSTANCE.getExistingInstanceAttribution_Subj();

    /**
     * The meta object literal for the '<em><b>Addl Info Items</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXISTING_INSTANCE_ATTRIBUTION__ADDL_INFO_ITEMS = eINSTANCE.getExistingInstanceAttribution_AddlInfoItems();

    /**
     * The meta object literal for the '<em><b>POf S</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXISTING_INSTANCE_ATTRIBUTION__POF_S = eINSTANCE.getExistingInstanceAttribution_POfS();

    /**
     * The meta object literal for the '<em><b>Obj</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXISTING_INSTANCE_ATTRIBUTION__OBJ = eINSTANCE.getExistingInstanceAttribution_Obj();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ObjectImpl <em>Object</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ObjectImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getObject()
     * @generated
     */
    EClass OBJECT = eINSTANCE.getObject();

    /**
     * The meta object literal for the '<em><b>Val</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference OBJECT__VAL = eINSTANCE.getObject_Val();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.PropValPartialTripleImpl <em>Prop Val Partial Triple</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.PropValPartialTripleImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getPropValPartialTriple()
     * @generated
     */
    EClass PROP_VAL_PARTIAL_TRIPLE = eINSTANCE.getPropValPartialTriple();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME = eINSTANCE.getPropValPartialTriple_PropertyName();

    /**
     * The meta object literal for the '<em><b>Object Value</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE = eINSTANCE.getPropValPartialTriple_ObjectValue();

    /**
     * The meta object literal for the '<em><b>Object Value BNode</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE = eINSTANCE.getPropValPartialTriple_ObjectValueBNode();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.OfPatternReturningValuesImpl <em>Of Pattern Returning Values</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.OfPatternReturningValuesImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getOfPatternReturningValues()
     * @generated
     */
    EClass OF_PATTERN_RETURNING_VALUES = eINSTANCE.getOfPatternReturningValues();

    /**
     * The meta object literal for the '<em><b>Ofphrs</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference OF_PATTERN_RETURNING_VALUES__OFPHRS = eINSTANCE.getOfPatternReturningValues_Ofphrs();

    /**
     * The meta object literal for the '<em><b>Subject</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference OF_PATTERN_RETURNING_VALUES__SUBJECT = eINSTANCE.getOfPatternReturningValues_Subject();

    /**
     * The meta object literal for the '<em><b>Type</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference OF_PATTERN_RETURNING_VALUES__TYPE = eINSTANCE.getOfPatternReturningValues_Type();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.WithChainImpl <em>With Chain</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.WithChainImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getWithChain()
     * @generated
     */
    EClass WITH_CHAIN = eINSTANCE.getWithChain();

    /**
     * The meta object literal for the '<em><b>Wps</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference WITH_CHAIN__WPS = eINSTANCE.getWithChain_Wps();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.WithPhraseImpl <em>With Phrase</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.WithPhraseImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getWithPhrase()
     * @generated
     */
    EClass WITH_PHRASE = eINSTANCE.getWithPhrase();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference WITH_PHRASE__PROPERTY_NAME = eINSTANCE.getWithPhrase_PropertyName();

    /**
     * The meta object literal for the '<em><b>Value</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference WITH_PHRASE__VALUE = eINSTANCE.getWithPhrase_Value();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.EmbeddedInstanceDeclarationImpl <em>Embedded Instance Declaration</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.EmbeddedInstanceDeclarationImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getEmbeddedInstanceDeclaration()
     * @generated
     */
    EClass EMBEDDED_INSTANCE_DECLARATION = eINSTANCE.getEmbeddedInstanceDeclaration();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.MergedTriplesImpl <em>Merged Triples</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.MergedTriplesImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getMergedTriples()
     * @generated
     */
    EClass MERGED_TRIPLES = eINSTANCE.getMergedTriples();

    /**
     * The meta object literal for the '<em><b>Ops</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MERGED_TRIPLES__OPS = eINSTANCE.getMergedTriples_Ops();

    /**
     * The meta object literal for the '<em><b>Pivot</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MERGED_TRIPLES__PIVOT = eINSTANCE.getMergedTriples_Pivot();

    /**
     * The meta object literal for the '<em><b>Wcs</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MERGED_TRIPLES__WCS = eINSTANCE.getMergedTriples_Wcs();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.OfPhraseImpl <em>Of Phrase</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.OfPhraseImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getOfPhrase()
     * @generated
     */
    EClass OF_PHRASE = eINSTANCE.getOfPhrase();

    /**
     * The meta object literal for the '<em><b>Article</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute OF_PHRASE__ARTICLE = eINSTANCE.getOfPhrase_Article();

    /**
     * The meta object literal for the '<em><b>Property Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference OF_PHRASE__PROPERTY_NAME = eINSTANCE.getOfPhrase_PropertyName();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.VariableListImpl <em>Variable List</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.VariableListImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getVariableList()
     * @generated
     */
    EClass VARIABLE_LIST = eINSTANCE.getVariableList();

    /**
     * The meta object literal for the '<em><b>Names</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference VARIABLE_LIST__NAMES = eINSTANCE.getVariableList_Names();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.RuleImpl <em>Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.RuleImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getRule()
     * @generated
     */
    EClass RULE = eINSTANCE.getRule();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute RULE__NAME = eINSTANCE.getRule_Name();

    /**
     * The meta object literal for the '<em><b>Givens</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference RULE__GIVENS = eINSTANCE.getRule_Givens();

    /**
     * The meta object literal for the '<em><b>Ifs</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference RULE__IFS = eINSTANCE.getRule_Ifs();

    /**
     * The meta object literal for the '<em><b>Thens</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference RULE__THENS = eINSTANCE.getRule_Thens();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.QueryImpl <em>Query</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.QueryImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getQuery()
     * @generated
     */
    EClass QUERY = eINSTANCE.getQuery();

    /**
     * The meta object literal for the '<em><b>Expr</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference QUERY__EXPR = eINSTANCE.getQuery_Expr();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.TestImpl <em>Test</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.TestImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getTest()
     * @generated
     */
    EClass TEST = eINSTANCE.getTest();

    /**
     * The meta object literal for the '<em><b>Expr</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference TEST__EXPR = eINSTANCE.getTest_Expr();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ExprImpl <em>Expr</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ExprImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExpr()
     * @generated
     */
    EClass EXPR = eINSTANCE.getExpr();

    /**
     * The meta object literal for the '<em><b>Expr</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXPR__EXPR = eINSTANCE.getExpr_Expr();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.DisplayImpl <em>Display</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.DisplayImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getDisplay()
     * @generated
     */
    EClass DISPLAY = eINSTANCE.getDisplay();

    /**
     * The meta object literal for the '<em><b>Display String</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute DISPLAY__DISPLAY_STRING = eINSTANCE.getDisplay_DisplayString();

    /**
     * The meta object literal for the '<em><b>Model</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute DISPLAY__MODEL = eINSTANCE.getDisplay_Model();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ExplanationImpl <em>Explanation</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ExplanationImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExplanation()
     * @generated
     */
    EClass EXPLANATION = eINSTANCE.getExplanation();

    /**
     * The meta object literal for the '<em><b>Expr</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXPLANATION__EXPR = eINSTANCE.getExplanation_Expr();

    /**
     * The meta object literal for the '<em><b>Rulename</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute EXPLANATION__RULENAME = eINSTANCE.getExplanation_Rulename();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ElementSetImpl <em>Element Set</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ElementSetImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getElementSet()
     * @generated
     */
    EClass ELEMENT_SET = eINSTANCE.getElementSet();

    /**
     * The meta object literal for the '<em><b>Elements</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ELEMENT_SET__ELEMENTS = eINSTANCE.getElementSet_Elements();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.SelectExpressionImpl <em>Select Expression</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.SelectExpressionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSelectExpression()
     * @generated
     */
    EClass SELECT_EXPRESSION = eINSTANCE.getSelectExpression();

    /**
     * The meta object literal for the '<em><b>Distinct</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute SELECT_EXPRESSION__DISTINCT = eINSTANCE.getSelectExpression_Distinct();

    /**
     * The meta object literal for the '<em><b>All Vars</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute SELECT_EXPRESSION__ALL_VARS = eINSTANCE.getSelectExpression_AllVars();

    /**
     * The meta object literal for the '<em><b>Var List</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SELECT_EXPRESSION__VAR_LIST = eINSTANCE.getSelectExpression_VarList();

    /**
     * The meta object literal for the '<em><b>Orderby</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute SELECT_EXPRESSION__ORDERBY = eINSTANCE.getSelectExpression_Orderby();

    /**
     * The meta object literal for the '<em><b>Order List</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SELECT_EXPRESSION__ORDER_LIST = eINSTANCE.getSelectExpression_OrderList();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ConstructExpressionImpl <em>Construct Expression</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ConstructExpressionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getConstructExpression()
     * @generated
     */
    EClass CONSTRUCT_EXPRESSION = eINSTANCE.getConstructExpression();

    /**
     * The meta object literal for the '<em><b>Subj</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CONSTRUCT_EXPRESSION__SUBJ = eINSTANCE.getConstructExpression_Subj();

    /**
     * The meta object literal for the '<em><b>Pred</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CONSTRUCT_EXPRESSION__PRED = eINSTANCE.getConstructExpression_Pred();

    /**
     * The meta object literal for the '<em><b>Obj</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CONSTRUCT_EXPRESSION__OBJ = eINSTANCE.getConstructExpression_Obj();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.AskQueryExpressionImpl <em>Ask Query Expression</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.AskQueryExpressionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getAskQueryExpression()
     * @generated
     */
    EClass ASK_QUERY_EXPRESSION = eINSTANCE.getAskQueryExpression();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.OrderListImpl <em>Order List</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.OrderListImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getOrderList()
     * @generated
     */
    EClass ORDER_LIST = eINSTANCE.getOrderList();

    /**
     * The meta object literal for the '<em><b>Order List</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ORDER_LIST__ORDER_LIST = eINSTANCE.getOrderList_OrderList();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.OrderElementImpl <em>Order Element</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.OrderElementImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getOrderElement()
     * @generated
     */
    EClass ORDER_ELEMENT = eINSTANCE.getOrderElement();

    /**
     * The meta object literal for the '<em><b>Order</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ORDER_ELEMENT__ORDER = eINSTANCE.getOrderElement_Order();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ORDER_ELEMENT__NAME = eINSTANCE.getOrderElement_Name();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ExpressionImpl <em>Expression</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ExpressionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExpression()
     * @generated
     */
    EClass EXPRESSION = eINSTANCE.getExpression();

    /**
     * The meta object literal for the '<em><b>Expr</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXPRESSION__EXPR = eINSTANCE.getExpression_Expr();

    /**
     * The meta object literal for the '<em><b>Func</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute EXPRESSION__FUNC = eINSTANCE.getExpression_Func();

    /**
     * The meta object literal for the '<em><b>Args</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXPRESSION__ARGS = eINSTANCE.getExpression_Args();

    /**
     * The meta object literal for the '<em><b>Gp</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXPRESSION__GP = eINSTANCE.getExpression_Gp();

    /**
     * The meta object literal for the '<em><b>Ivalue</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXPRESSION__IVALUE = eINSTANCE.getExpression_Ivalue();

    /**
     * The meta object literal for the '<em><b>Value</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXPRESSION__VALUE = eINSTANCE.getExpression_Value();

    /**
     * The meta object literal for the '<em><b>Value Table</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXPRESSION__VALUE_TABLE = eINSTANCE.getExpression_ValueTable();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.GraphPatternImpl <em>Graph Pattern</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.GraphPatternImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getGraphPattern()
     * @generated
     */
    EClass GRAPH_PATTERN = eINSTANCE.getGraphPattern();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.PropOfSubjImpl <em>Prop Of Subj</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.PropOfSubjImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getPropOfSubj()
     * @generated
     */
    EClass PROP_OF_SUBJ = eINSTANCE.getPropOfSubj();

    /**
     * The meta object literal for the '<em><b>Of Phr</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROP_OF_SUBJ__OF_PHR = eINSTANCE.getPropOfSubj_OfPhr();

    /**
     * The meta object literal for the '<em><b>Subj</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PROP_OF_SUBJ__SUBJ = eINSTANCE.getPropOfSubj_Subj();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.SubjPropImpl <em>Subj Prop</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.SubjPropImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSubjProp()
     * @generated
     */
    EClass SUBJ_PROP = eINSTANCE.getSubjProp();

    /**
     * The meta object literal for the '<em><b>Subj</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SUBJ_PROP__SUBJ = eINSTANCE.getSubjProp_Subj();

    /**
     * The meta object literal for the '<em><b>Hw Phr</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SUBJ_PROP__HW_PHR = eINSTANCE.getSubjProp_HwPhr();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.InstAttrSPVImpl <em>Inst Attr SPV</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.InstAttrSPVImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstAttrSPV()
     * @generated
     */
    EClass INST_ATTR_SPV = eINSTANCE.getInstAttrSPV();

    /**
     * The meta object literal for the '<em><b>Subj</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INST_ATTR_SPV__SUBJ = eINSTANCE.getInstAttrSPV_Subj();

    /**
     * The meta object literal for the '<em><b>Props</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INST_ATTR_SPV__PROPS = eINSTANCE.getInstAttrSPV_Props();

    /**
     * The meta object literal for the '<em><b>Vals</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INST_ATTR_SPV__VALS = eINSTANCE.getInstAttrSPV_Vals();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.InstAttrPSVImpl <em>Inst Attr PSV</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.InstAttrPSVImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getInstAttrPSV()
     * @generated
     */
    EClass INST_ATTR_PSV = eINSTANCE.getInstAttrPSV();

    /**
     * The meta object literal for the '<em><b>Prop</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INST_ATTR_PSV__PROP = eINSTANCE.getInstAttrPSV_Prop();

    /**
     * The meta object literal for the '<em><b>Val</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INST_ATTR_PSV__VAL = eINSTANCE.getInstAttrPSV_Val();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.SubTypeOfImpl <em>Sub Type Of</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.SubTypeOfImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getSubTypeOf()
     * @generated
     */
    EClass SUB_TYPE_OF = eINSTANCE.getSubTypeOf();

    /**
     * The meta object literal for the '<em><b>Subclass</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SUB_TYPE_OF__SUBCLASS = eINSTANCE.getSubTypeOf_Subclass();

    /**
     * The meta object literal for the '<em><b>Superclass</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SUB_TYPE_OF__SUPERCLASS = eINSTANCE.getSubTypeOf_Superclass();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.IntervalValueImpl <em>Interval Value</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.IntervalValueImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getIntervalValue()
     * @generated
     */
    EClass INTERVAL_VALUE = eINSTANCE.getIntervalValue();

    /**
     * The meta object literal for the '<em><b>Op</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute INTERVAL_VALUE__OP = eINSTANCE.getIntervalValue_Op();

    /**
     * The meta object literal for the '<em><b>Expr</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INTERVAL_VALUE__EXPR = eINSTANCE.getIntervalValue_Expr();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ExplicitValueImpl <em>Explicit Value</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ExplicitValueImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getExplicitValue()
     * @generated
     */
    EClass EXPLICIT_VALUE = eINSTANCE.getExplicitValue();

    /**
     * The meta object literal for the '<em><b>Inst Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXPLICIT_VALUE__INST_NAME = eINSTANCE.getExplicitValue_InstName();

    /**
     * The meta object literal for the '<em><b>Lit Value</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EXPLICIT_VALUE__LIT_VALUE = eINSTANCE.getExplicitValue_LitValue();

    /**
     * The meta object literal for the '<em><b>Term</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute EXPLICIT_VALUE__TERM = eINSTANCE.getExplicitValue_Term();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ValueTableImpl <em>Value Table</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ValueTableImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getValueTable()
     * @generated
     */
    EClass VALUE_TABLE = eINSTANCE.getValueTable();

    /**
     * The meta object literal for the '<em><b>Row</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference VALUE_TABLE__ROW = eINSTANCE.getValueTable_Row();

    /**
     * The meta object literal for the '<em><b>Rows</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference VALUE_TABLE__ROWS = eINSTANCE.getValueTable_Rows();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.LiteralValueImpl <em>Literal Value</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.LiteralValueImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getLiteralValue()
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
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.ValueRowImpl <em>Value Row</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.ValueRowImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getValueRow()
     * @generated
     */
    EClass VALUE_ROW = eINSTANCE.getValueRow();

    /**
     * The meta object literal for the '<em><b>Explicit Values</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference VALUE_ROW__EXPLICIT_VALUES = eINSTANCE.getValueRow_ExplicitValues();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.JunctionExpressionImpl <em>Junction Expression</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.JunctionExpressionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getJunctionExpression()
     * @generated
     */
    EClass JUNCTION_EXPRESSION = eINSTANCE.getJunctionExpression();

    /**
     * The meta object literal for the '<em><b>Left</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference JUNCTION_EXPRESSION__LEFT = eINSTANCE.getJunctionExpression_Left();

    /**
     * The meta object literal for the '<em><b>Op</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute JUNCTION_EXPRESSION__OP = eINSTANCE.getJunctionExpression_Op();

    /**
     * The meta object literal for the '<em><b>Right</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference JUNCTION_EXPRESSION__RIGHT = eINSTANCE.getJunctionExpression_Right();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.BinaryOpExpressionImpl <em>Binary Op Expression</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.BinaryOpExpressionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getBinaryOpExpression()
     * @generated
     */
    EClass BINARY_OP_EXPRESSION = eINSTANCE.getBinaryOpExpression();

    /**
     * The meta object literal for the '<em><b>Left</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference BINARY_OP_EXPRESSION__LEFT = eINSTANCE.getBinaryOpExpression_Left();

    /**
     * The meta object literal for the '<em><b>Op</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute BINARY_OP_EXPRESSION__OP = eINSTANCE.getBinaryOpExpression_Op();

    /**
     * The meta object literal for the '<em><b>Right</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference BINARY_OP_EXPRESSION__RIGHT = eINSTANCE.getBinaryOpExpression_Right();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.impl.UnaryOpExpressionImpl <em>Unary Op Expression</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.impl.UnaryOpExpressionImpl
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getUnaryOpExpression()
     * @generated
     */
    EClass UNARY_OP_EXPRESSION = eINSTANCE.getUnaryOpExpression();

    /**
     * The meta object literal for the '<em><b>Op</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute UNARY_OP_EXPRESSION__OP = eINSTANCE.getUnaryOpExpression_Op();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.sadl.DataType <em>Data Type</em>}' enum.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.sadl.DataType
     * @see com.ge.research.sadl.sadl.impl.SadlPackageImpl#getDataType()
     * @generated
     */
    EEnum DATA_TYPE = eINSTANCE.getDataType();

  }

} //SadlPackage
