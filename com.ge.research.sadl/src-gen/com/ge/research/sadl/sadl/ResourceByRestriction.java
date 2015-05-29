/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Resource By Restriction</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.ResourceByRestriction#getAnnType <em>Ann Type</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ResourceByRestriction#getAnnContent <em>Ann Content</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ResourceByRestriction#getPropName <em>Prop Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ResourceByRestriction#getCond <em>Cond</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getResourceByRestriction()
 * @model
 * @generated
 */
public interface ResourceByRestriction extends ResourceIdentifier
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getResourceByRestriction_AnnType()
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getResourceByRestriction_AnnContent()
   * @model containment="true"
   * @generated
   */
  EList<ContentList> getAnnContent();

  /**
   * Returns the value of the '<em><b>Prop Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Prop Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Prop Name</em>' containment reference.
   * @see #setPropName(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getResourceByRestriction_PropName()
   * @model containment="true"
   * @generated
   */
  ResourceByName getPropName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ResourceByRestriction#getPropName <em>Prop Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Prop Name</em>' containment reference.
   * @see #getPropName()
   * @generated
   */
  void setPropName(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Cond</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Cond</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Cond</em>' containment reference.
   * @see #setCond(Condition)
   * @see com.ge.research.sadl.sadl.SadlPackage#getResourceByRestriction_Cond()
   * @model containment="true"
   * @generated
   */
  Condition getCond();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ResourceByRestriction#getCond <em>Cond</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Cond</em>' containment reference.
   * @see #getCond()
   * @generated
   */
  void setCond(Condition value);

} // ResourceByRestriction
