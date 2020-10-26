/**
 */
package com.ge.research.sadl.mapping;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>New Model NS</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.mapping.NewModelNS#getBaseUri <em>Base Uri</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.NewModelNS#getPrefix <em>Prefix</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.mapping.MappingPackage#getNewModelNS()
 * @model
 * @generated
 */
public interface NewModelNS extends EObject
{
  /**
   * Returns the value of the '<em><b>Base Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Base Uri</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Base Uri</em>' attribute.
   * @see #setBaseUri(String)
   * @see com.ge.research.sadl.mapping.MappingPackage#getNewModelNS_BaseUri()
   * @model
   * @generated
   */
  String getBaseUri();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.mapping.NewModelNS#getBaseUri <em>Base Uri</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Base Uri</em>' attribute.
   * @see #getBaseUri()
   * @generated
   */
  void setBaseUri(String value);

  /**
   * Returns the value of the '<em><b>Prefix</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Prefix</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Prefix</em>' attribute.
   * @see #setPrefix(String)
   * @see com.ge.research.sadl.mapping.MappingPackage#getNewModelNS_Prefix()
   * @model
   * @generated
   */
  String getPrefix();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.mapping.NewModelNS#getPrefix <em>Prefix</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Prefix</em>' attribute.
   * @see #getPrefix()
   * @generated
   */
  void setPrefix(String value);

} // NewModelNS
