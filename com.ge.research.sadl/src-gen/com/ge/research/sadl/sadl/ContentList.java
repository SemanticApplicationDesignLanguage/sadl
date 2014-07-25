/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Content List</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.ContentList#getAnnContent <em>Ann Content</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getContentList()
 * @model
 * @generated
 */
public interface ContentList extends EObject
{
  /**
   * Returns the value of the '<em><b>Ann Content</b></em>' attribute list.
   * The list contents are of type {@link java.lang.String}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Ann Content</em>' attribute list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Ann Content</em>' attribute list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getContentList_AnnContent()
   * @model unique="false"
   * @generated
   */
  EList<String> getAnnContent();

} // ContentList
