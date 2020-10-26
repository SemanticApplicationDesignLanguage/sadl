/**
 */
package com.ge.research.sadl.mapping;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Literal Value</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.mapping.LiteralValue#getLiteralNumber <em>Literal Number</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.LiteralValue#getLiteralString <em>Literal String</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.LiteralValue#getLiteralBoolean <em>Literal Boolean</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.mapping.MappingPackage#getLiteralValue()
 * @model
 * @generated
 */
public interface LiteralValue extends EObject
{
  /**
   * Returns the value of the '<em><b>Literal Number</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Literal Number</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Literal Number</em>' attribute.
   * @see #setLiteralNumber(String)
   * @see com.ge.research.sadl.mapping.MappingPackage#getLiteralValue_LiteralNumber()
   * @model
   * @generated
   */
  String getLiteralNumber();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.mapping.LiteralValue#getLiteralNumber <em>Literal Number</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Literal Number</em>' attribute.
   * @see #getLiteralNumber()
   * @generated
   */
  void setLiteralNumber(String value);

  /**
   * Returns the value of the '<em><b>Literal String</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Literal String</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Literal String</em>' attribute.
   * @see #setLiteralString(String)
   * @see com.ge.research.sadl.mapping.MappingPackage#getLiteralValue_LiteralString()
   * @model
   * @generated
   */
  String getLiteralString();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.mapping.LiteralValue#getLiteralString <em>Literal String</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Literal String</em>' attribute.
   * @see #getLiteralString()
   * @generated
   */
  void setLiteralString(String value);

  /**
   * Returns the value of the '<em><b>Literal Boolean</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Literal Boolean</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Literal Boolean</em>' attribute.
   * @see #setLiteralBoolean(String)
   * @see com.ge.research.sadl.mapping.MappingPackage#getLiteralValue_LiteralBoolean()
   * @model
   * @generated
   */
  String getLiteralBoolean();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.mapping.LiteralValue#getLiteralBoolean <em>Literal Boolean</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Literal Boolean</em>' attribute.
   * @see #getLiteralBoolean()
   * @generated
   */
  void setLiteralBoolean(String value);

} // LiteralValue
