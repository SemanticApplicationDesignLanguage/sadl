/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Range Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.RangeType#getClassIdentifier <em>Class Identifier</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.RangeType#getDataType <em>Data Type</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getRangeType()
 * @model
 * @generated
 */
public interface RangeType extends EObject
{
  /**
   * Returns the value of the '<em><b>Class Identifier</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Class Identifier</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Class Identifier</em>' containment reference.
   * @see #setClassIdentifier(ResourceIdentifier)
   * @see com.ge.research.sadl.sadl.SadlPackage#getRangeType_ClassIdentifier()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getClassIdentifier();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.RangeType#getClassIdentifier <em>Class Identifier</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class Identifier</em>' containment reference.
   * @see #getClassIdentifier()
   * @generated
   */
  void setClassIdentifier(ResourceIdentifier value);

  /**
   * Returns the value of the '<em><b>Data Type</b></em>' attribute.
   * The literals are from the enumeration {@link com.ge.research.sadl.sadl.DataType}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Data Type</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Data Type</em>' attribute.
   * @see com.ge.research.sadl.sadl.DataType
   * @see #setDataType(DataType)
   * @see com.ge.research.sadl.sadl.SadlPackage#getRangeType_DataType()
   * @model
   * @generated
   */
  DataType getDataType();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.RangeType#getDataType <em>Data Type</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Data Type</em>' attribute.
   * @see com.ge.research.sadl.sadl.DataType
   * @see #getDataType()
   * @generated
   */
  void setDataType(DataType value);

} // RangeType
