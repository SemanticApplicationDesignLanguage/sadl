/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>All Values Condition</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.AllValuesCondition#getRestriction <em>Restriction</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getAllValuesCondition()
 * @model
 * @generated
 */
public interface AllValuesCondition extends Condition
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
   * @see #setRestriction(ResourceIdentifier)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAllValuesCondition_Restriction()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getRestriction();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AllValuesCondition#getRestriction <em>Restriction</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Restriction</em>' containment reference.
   * @see #getRestriction()
   * @generated
   */
  void setRestriction(ResourceIdentifier value);

} // AllValuesCondition
