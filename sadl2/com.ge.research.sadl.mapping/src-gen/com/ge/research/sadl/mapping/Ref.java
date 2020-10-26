/**
 */
package com.ge.research.sadl.mapping;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Ref</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.mapping.Ref#getRef <em>Ref</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.Ref#getAddlcols <em>Addlcols</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.Ref#getRow <em>Row</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.mapping.MappingPackage#getRef()
 * @model
 * @generated
 */
public interface Ref extends EObject
{
  /**
   * Returns the value of the '<em><b>Ref</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Ref</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Ref</em>' attribute.
   * @see #setRef(String)
   * @see com.ge.research.sadl.mapping.MappingPackage#getRef_Ref()
   * @model
   * @generated
   */
  String getRef();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.mapping.Ref#getRef <em>Ref</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Ref</em>' attribute.
   * @see #getRef()
   * @generated
   */
  void setRef(String value);

  /**
   * Returns the value of the '<em><b>Addlcols</b></em>' attribute list.
   * The list contents are of type {@link java.lang.String}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Addlcols</em>' attribute list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Addlcols</em>' attribute list.
   * @see com.ge.research.sadl.mapping.MappingPackage#getRef_Addlcols()
   * @model unique="false"
   * @generated
   */
  EList<String> getAddlcols();

  /**
   * Returns the value of the '<em><b>Row</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Row</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Row</em>' attribute.
   * @see #setRow(String)
   * @see com.ge.research.sadl.mapping.MappingPackage#getRef_Row()
   * @model
   * @generated
   */
  String getRow();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.mapping.Ref#getRow <em>Row</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Row</em>' attribute.
   * @see #getRow()
   * @generated
   */
  void setRow(String value);

} // Ref
