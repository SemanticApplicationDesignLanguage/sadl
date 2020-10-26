/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Resource By Set Op</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.ResourceBySetOp#getAnnType <em>Ann Type</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ResourceBySetOp#getAnnContent <em>Ann Content</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ResourceBySetOp#getNames <em>Names</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ResourceBySetOp#getOp <em>Op</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getResourceBySetOp()
 * @model
 * @generated
 */
public interface ResourceBySetOp extends ResourceIdentifier
{
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getResourceBySetOp_AnnType()
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getResourceBySetOp_AnnContent()
   * @model containment="true"
   * @generated
   */
  EList<ContentList> getAnnContent();

  /**
   * Returns the value of the '<em><b>Names</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.ResourceIdentifier}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Names</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Names</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getResourceBySetOp_Names()
   * @model containment="true"
   * @generated
   */
  EList<ResourceIdentifier> getNames();

  /**
   * Returns the value of the '<em><b>Op</b></em>' attribute list.
   * The list contents are of type {@link java.lang.String}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Op</em>' attribute list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Op</em>' attribute list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getResourceBySetOp_Op()
   * @model unique="false"
   * @generated
   */
  EList<String> getOp();

} // ResourceBySetOp
