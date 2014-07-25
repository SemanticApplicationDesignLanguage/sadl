/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>User Defined Data Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.UserDefinedDataType#getUdt <em>Udt</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.UserDefinedDataType#getRestriction <em>Restriction</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getUserDefinedDataType()
 * @model
 * @generated
 */
public interface UserDefinedDataType extends Statement
{
  /**
   * Returns the value of the '<em><b>Udt</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Udt</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Udt</em>' attribute.
   * @see #setUdt(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getUserDefinedDataType_Udt()
   * @model
   * @generated
   */
  String getUdt();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.UserDefinedDataType#getUdt <em>Udt</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Udt</em>' attribute.
   * @see #getUdt()
   * @generated
   */
  void setUdt(String value);

  /**
   * Returns the value of the '<em><b>Restriction</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Restriction</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Restriction</em>' containment reference.
   * @see #setRestriction(DataTypeRestriction)
   * @see com.ge.research.sadl.sadl.SadlPackage#getUserDefinedDataType_Restriction()
   * @model containment="true"
   * @generated
   */
  DataTypeRestriction getRestriction();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.UserDefinedDataType#getRestriction <em>Restriction</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Restriction</em>' containment reference.
   * @see #getRestriction()
   * @generated
   */
  void setRestriction(DataTypeRestriction value);

} // UserDefinedDataType
