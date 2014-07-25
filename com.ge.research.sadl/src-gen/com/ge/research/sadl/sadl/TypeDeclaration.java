/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Type Declaration</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.TypeDeclaration#getInstName <em>Inst Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.TypeDeclaration#getType <em>Type</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getTypeDeclaration()
 * @model
 * @generated
 */
public interface TypeDeclaration extends EObject
{
  /**
   * Returns the value of the '<em><b>Inst Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Inst Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Inst Name</em>' containment reference.
   * @see #setInstName(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getTypeDeclaration_InstName()
   * @model containment="true"
   * @generated
   */
  ResourceName getInstName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.TypeDeclaration#getInstName <em>Inst Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Inst Name</em>' containment reference.
   * @see #getInstName()
   * @generated
   */
  void setInstName(ResourceName value);

  /**
   * Returns the value of the '<em><b>Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Type</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Type</em>' containment reference.
   * @see #setType(TypedBNode)
   * @see com.ge.research.sadl.sadl.SadlPackage#getTypeDeclaration_Type()
   * @model containment="true"
   * @generated
   */
  TypedBNode getType();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.TypeDeclaration#getType <em>Type</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Type</em>' containment reference.
   * @see #getType()
   * @generated
   */
  void setType(TypedBNode value);

} // TypeDeclaration
