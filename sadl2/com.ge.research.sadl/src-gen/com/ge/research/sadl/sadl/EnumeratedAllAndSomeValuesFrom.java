/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Enumerated All And Some Values From</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom#getRestricted <em>Restricted</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom#getEnumeration <em>Enumeration</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getEnumeratedAllAndSomeValuesFrom()
 * @model
 * @generated
 */
public interface EnumeratedAllAndSomeValuesFrom extends Statement
{
  /**
   * Returns the value of the '<em><b>Restricted</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Restricted</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Restricted</em>' containment reference.
   * @see #setRestricted(PropertyOfClass)
   * @see com.ge.research.sadl.sadl.SadlPackage#getEnumeratedAllAndSomeValuesFrom_Restricted()
   * @model containment="true"
   * @generated
   */
  PropertyOfClass getRestricted();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom#getRestricted <em>Restricted</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Restricted</em>' containment reference.
   * @see #getRestricted()
   * @generated
   */
  void setRestricted(PropertyOfClass value);

  /**
   * Returns the value of the '<em><b>Enumeration</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Enumeration</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Enumeration</em>' containment reference.
   * @see #setEnumeration(EObject)
   * @see com.ge.research.sadl.sadl.SadlPackage#getEnumeratedAllAndSomeValuesFrom_Enumeration()
   * @model containment="true"
   * @generated
   */
  EObject getEnumeration();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.EnumeratedAllAndSomeValuesFrom#getEnumeration <em>Enumeration</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Enumeration</em>' containment reference.
   * @see #getEnumeration()
   * @generated
   */
  void setEnumeration(EObject value);

} // EnumeratedAllAndSomeValuesFrom
