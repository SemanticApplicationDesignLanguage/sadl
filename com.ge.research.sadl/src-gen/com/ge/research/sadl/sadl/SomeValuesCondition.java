/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Some Values Condition</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.SomeValuesCondition#getRestriction <em>Restriction</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getSomeValuesCondition()
 * @model
 * @generated
 */
public interface SomeValuesCondition extends Condition
{
  /**
   * Returns the value of the '<em><b>Restriction</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Restriction</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Restriction</em>' containment reference.
   * @see #setRestriction(EObject)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSomeValuesCondition_Restriction()
   * @model containment="true"
   * @generated
   */
  EObject getRestriction();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SomeValuesCondition#getRestriction <em>Restriction</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Restriction</em>' containment reference.
   * @see #getRestriction()
   * @generated
   */
  void setRestriction(EObject value);

} // SomeValuesCondition
