/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Necessary And Sufficient</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getSuperClass <em>Super Class</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getArticle <em>Article</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getSubClass <em>Sub Class</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getCond <em>Cond</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getNecessaryAndSufficient()
 * @model
 * @generated
 */
public interface NecessaryAndSufficient extends Statement
{
  /**
   * Returns the value of the '<em><b>Super Class</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Super Class</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Super Class</em>' containment reference.
   * @see #setSuperClass(TypedBNode)
   * @see com.ge.research.sadl.sadl.SadlPackage#getNecessaryAndSufficient_SuperClass()
   * @model containment="true"
   * @generated
   */
  TypedBNode getSuperClass();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getSuperClass <em>Super Class</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Super Class</em>' containment reference.
   * @see #getSuperClass()
   * @generated
   */
  void setSuperClass(TypedBNode value);

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
   * @see com.ge.research.sadl.sadl.SadlPackage#getNecessaryAndSufficient_Article()
   * @model
   * @generated
   */
  String getArticle();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getArticle <em>Article</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Article</em>' attribute.
   * @see #getArticle()
   * @generated
   */
  void setArticle(String value);

  /**
   * Returns the value of the '<em><b>Sub Class</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Sub Class</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Sub Class</em>' containment reference.
   * @see #setSubClass(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getNecessaryAndSufficient_SubClass()
   * @model containment="true"
   * @generated
   */
  ResourceName getSubClass();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.NecessaryAndSufficient#getSubClass <em>Sub Class</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Sub Class</em>' containment reference.
   * @see #getSubClass()
   * @generated
   */
  void setSubClass(ResourceName value);

  /**
   * Returns the value of the '<em><b>Property Name</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.ResourceByName}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Property Name</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Property Name</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getNecessaryAndSufficient_PropertyName()
   * @model containment="true"
   * @generated
   */
  EList<ResourceByName> getPropertyName();

  /**
   * Returns the value of the '<em><b>Cond</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.Condition}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Cond</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Cond</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getNecessaryAndSufficient_Cond()
   * @model containment="true"
   * @generated
   */
  EList<Condition> getCond();

} // NecessaryAndSufficient
