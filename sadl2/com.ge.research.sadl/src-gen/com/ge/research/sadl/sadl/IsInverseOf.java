/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Is Inverse Of</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.IsInverseOf#getPropertyName2 <em>Property Name2</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getIsInverseOf()
 * @model
 * @generated
 */
public interface IsInverseOf extends EObject
{
  /**
   * Returns the value of the '<em><b>Property Name2</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Property Name2</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Property Name2</em>' containment reference.
   * @see #setPropertyName2(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getIsInverseOf_PropertyName2()
   * @model containment="true"
   * @generated
   */
  ResourceByName getPropertyName2();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.IsInverseOf#getPropertyName2 <em>Property Name2</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Property Name2</em>' containment reference.
   * @see #getPropertyName2()
   * @generated
   */
  void setPropertyName2(ResourceByName value);

} // IsInverseOf
