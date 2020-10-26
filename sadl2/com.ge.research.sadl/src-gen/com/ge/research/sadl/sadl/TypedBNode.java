/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Typed BNode</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.TypedBNode#getArticle <em>Article</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.TypedBNode#getClassIdentifier <em>Class Identifier</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getTypedBNode()
 * @model
 * @generated
 */
public interface TypedBNode extends EObject
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getTypedBNode_Article()
   * @model
   * @generated
   */
  String getArticle();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.TypedBNode#getArticle <em>Article</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Article</em>' attribute.
   * @see #getArticle()
   * @generated
   */
  void setArticle(String value);

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
   * @see com.ge.research.sadl.sadl.SadlPackage#getTypedBNode_ClassIdentifier()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getClassIdentifier();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.TypedBNode#getClassIdentifier <em>Class Identifier</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class Identifier</em>' containment reference.
   * @see #getClassIdentifier()
   * @generated
   */
  void setClassIdentifier(ResourceIdentifier value);

} // TypedBNode
