/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Has Value Condition</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.HasValueCondition#getRestriction <em>Restriction</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getHasValueCondition()
 * @model
 * @generated
 */
public interface HasValueCondition extends Condition
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
   * @see #setRestriction(ExplicitValue)
   * @see com.ge.research.sadl.sadl.SadlPackage#getHasValueCondition_Restriction()
   * @model containment="true"
   * @generated
   */
  ExplicitValue getRestriction();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.HasValueCondition#getRestriction <em>Restriction</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Restriction</em>' containment reference.
   * @see #getRestriction()
   * @generated
   */
  void setRestriction(ExplicitValue value);

} // HasValueCondition
