/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Resource Name</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.ResourceName#getName <em>Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ResourceName#getAnnType <em>Ann Type</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ResourceName#getAnnContent <em>Ann Content</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getResourceName()
 * @model
 * @generated
 */
public interface ResourceName extends EObject
{
  /**
   * Returns the value of the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Name</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Name</em>' attribute.
   * @see #setName(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getResourceName_Name()
   * @model
   * @generated
   */
  String getName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ResourceName#getName <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Name</em>' attribute.
   * @see #getName()
   * @generated
   */
  void setName(String value);

  /**
   * Returns the value of the '<em><b>Ann Type</b></em>' attribute list.
   * The list contents are of type {@link java.lang.String}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Ann Type</em>' attribute list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Ann Type</em>' attribute list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getResourceName_AnnType()
   * @model unique="false"
   * @generated
   */
  EList<String> getAnnType();

  /**
   * Returns the value of the '<em><b>Ann Content</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.ContentList}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Ann Content</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Ann Content</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getResourceName_AnnContent()
   * @model containment="true"
   * @generated
   */
  EList<ContentList> getAnnContent();

} // ResourceName
