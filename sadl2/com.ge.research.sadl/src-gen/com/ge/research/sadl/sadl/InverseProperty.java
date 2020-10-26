/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Inverse Property</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.InverseProperty#getPropertyName1 <em>Property Name1</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.InverseProperty#getInvOf <em>Inv Of</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getInverseProperty()
 * @model
 * @generated
 */
public interface InverseProperty extends Statement
{
  /**
   * Returns the value of the '<em><b>Property Name1</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Property Name1</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Property Name1</em>' containment reference.
   * @see #setPropertyName1(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getInverseProperty_PropertyName1()
   * @model containment="true"
   * @generated
   */
  ResourceByName getPropertyName1();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InverseProperty#getPropertyName1 <em>Property Name1</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Property Name1</em>' containment reference.
   * @see #getPropertyName1()
   * @generated
   */
  void setPropertyName1(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Inv Of</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Inv Of</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Inv Of</em>' containment reference.
   * @see #setInvOf(IsInverseOf)
   * @see com.ge.research.sadl.sadl.SadlPackage#getInverseProperty_InvOf()
   * @model containment="true"
   * @generated
   */
  IsInverseOf getInvOf();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InverseProperty#getInvOf <em>Inv Of</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Inv Of</em>' containment reference.
   * @see #getInvOf()
   * @generated
   */
  void setInvOf(IsInverseOf value);

} // InverseProperty
