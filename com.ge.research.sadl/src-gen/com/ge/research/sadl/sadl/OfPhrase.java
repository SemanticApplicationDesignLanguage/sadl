/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Of Phrase</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.OfPhrase#getArticle <em>Article</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.OfPhrase#getPropertyName <em>Property Name</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getOfPhrase()
 * @model
 * @generated
 */
public interface OfPhrase extends EObject
{
  /**
   * Returns the value of the '<em><b>Article</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Article</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Article</em>' attribute.
   * @see #setArticle(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getOfPhrase_Article()
   * @model
   * @generated
   */
  String getArticle();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.OfPhrase#getArticle <em>Article</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Article</em>' attribute.
   * @see #getArticle()
   * @generated
   */
  void setArticle(String value);

  /**
   * Returns the value of the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Property Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Property Name</em>' containment reference.
   * @see #setPropertyName(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getOfPhrase_PropertyName()
   * @model containment="true"
   * @generated
   */
  ResourceByName getPropertyName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.OfPhrase#getPropertyName <em>Property Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Property Name</em>' containment reference.
   * @see #getPropertyName()
   * @generated
   */
  void setPropertyName(ResourceByName value);

} // OfPhrase
