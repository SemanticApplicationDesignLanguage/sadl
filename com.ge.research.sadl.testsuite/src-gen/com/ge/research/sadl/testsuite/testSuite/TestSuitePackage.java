/**
 */
package com.ge.research.sadl.testsuite.testSuite;

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
 * @see com.ge.research.sadl.testsuite.testSuite.TestSuiteFactory
 * @model kind="package"
 * @generated
 */
public interface TestSuitePackage extends EPackage
{
  /**
   * The package name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNAME = "testSuite";

  /**
   * The package namespace URI.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_URI = "http://www.ge.com/research/sadl/testsuite/TestSuite";

  /**
   * The package namespace name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_PREFIX = "testSuite";

  /**
   * The singleton instance of the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  TestSuitePackage eINSTANCE = com.ge.research.sadl.testsuite.testSuite.impl.TestSuitePackageImpl.init();

  /**
   * The meta object id for the '{@link com.ge.research.sadl.testsuite.testSuite.impl.ModelImpl <em>Model</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.testsuite.testSuite.impl.ModelImpl
   * @see com.ge.research.sadl.testsuite.testSuite.impl.TestSuitePackageImpl#getModel()
   * @generated
   */
  int MODEL = 0;

  /**
   * The feature id for the '<em><b>Tests</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL__TESTS = 0;

  /**
   * The number of structural features of the '<em>Model</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link com.ge.research.sadl.testsuite.testSuite.impl.TestImpl <em>Test</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see com.ge.research.sadl.testsuite.testSuite.impl.TestImpl
   * @see com.ge.research.sadl.testsuite.testSuite.impl.TestSuitePackageImpl#getTest()
   * @generated
   */
  int TEST = 1;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TEST__NAME = 0;

  /**
   * The number of structural features of the '<em>Test</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TEST_FEATURE_COUNT = 1;


  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.testsuite.testSuite.Model <em>Model</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Model</em>'.
   * @see com.ge.research.sadl.testsuite.testSuite.Model
   * @generated
   */
  EClass getModel();

  /**
   * Returns the meta object for the containment reference list '{@link com.ge.research.sadl.testsuite.testSuite.Model#getTests <em>Tests</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Tests</em>'.
   * @see com.ge.research.sadl.testsuite.testSuite.Model#getTests()
   * @see #getModel()
   * @generated
   */
  EReference getModel_Tests();

  /**
   * Returns the meta object for class '{@link com.ge.research.sadl.testsuite.testSuite.Test <em>Test</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Test</em>'.
   * @see com.ge.research.sadl.testsuite.testSuite.Test
   * @generated
   */
  EClass getTest();

  /**
   * Returns the meta object for the attribute '{@link com.ge.research.sadl.testsuite.testSuite.Test#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see com.ge.research.sadl.testsuite.testSuite.Test#getName()
   * @see #getTest()
   * @generated
   */
  EAttribute getTest_Name();

  /**
   * Returns the factory that creates the instances of the model.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the factory that creates the instances of the model.
   * @generated
   */
  TestSuiteFactory getTestSuiteFactory();

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
     * The meta object literal for the '{@link com.ge.research.sadl.testsuite.testSuite.impl.ModelImpl <em>Model</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.testsuite.testSuite.impl.ModelImpl
     * @see com.ge.research.sadl.testsuite.testSuite.impl.TestSuitePackageImpl#getModel()
     * @generated
     */
    EClass MODEL = eINSTANCE.getModel();

    /**
     * The meta object literal for the '<em><b>Tests</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MODEL__TESTS = eINSTANCE.getModel_Tests();

    /**
     * The meta object literal for the '{@link com.ge.research.sadl.testsuite.testSuite.impl.TestImpl <em>Test</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see com.ge.research.sadl.testsuite.testSuite.impl.TestImpl
     * @see com.ge.research.sadl.testsuite.testSuite.impl.TestSuitePackageImpl#getTest()
     * @generated
     */
    EClass TEST = eINSTANCE.getTest();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute TEST__NAME = eINSTANCE.getTest_Name();

  }

} //TestSuitePackage
