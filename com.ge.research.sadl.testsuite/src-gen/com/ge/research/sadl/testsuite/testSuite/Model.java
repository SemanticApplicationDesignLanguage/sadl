/**
 */
package com.ge.research.sadl.testsuite.testSuite;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Model</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.testsuite.testSuite.Model#getTests <em>Tests</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.testsuite.testSuite.TestSuitePackage#getModel()
 * @model
 * @generated
 */
public interface Model extends EObject
{
  /**
   * Returns the value of the '<em><b>Tests</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.testsuite.testSuite.Test}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Tests</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Tests</em>' containment reference list.
   * @see com.ge.research.sadl.testsuite.testSuite.TestSuitePackage#getModel_Tests()
   * @model containment="true"
   * @generated
   */
  EList<Test> getTests();

} // Model
