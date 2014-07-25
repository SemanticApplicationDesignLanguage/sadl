/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Of Pattern Returning Values</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.OfPatternReturningValues#getOfphrs <em>Ofphrs</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.OfPatternReturningValues#getSubject <em>Subject</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.OfPatternReturningValues#getType <em>Type</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getOfPatternReturningValues()
 * @model
 * @generated
 */
public interface OfPatternReturningValues extends EObject
{
  /**
   * Returns the value of the '<em><b>Ofphrs</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.OfPhrase}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Ofphrs</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Ofphrs</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getOfPatternReturningValues_Ofphrs()
   * @model containment="true"
   * @generated
   */
  EList<OfPhrase> getOfphrs();

  /**
   * Returns the value of the '<em><b>Subject</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Subject</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Subject</em>' containment reference.
   * @see #setSubject(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getOfPatternReturningValues_Subject()
   * @model containment="true"
   * @generated
   */
  ResourceByName getSubject();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.OfPatternReturningValues#getSubject <em>Subject</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Subject</em>' containment reference.
   * @see #getSubject()
   * @generated
   */
  void setSubject(ResourceByName value);

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
   * @see com.ge.research.sadl.sadl.SadlPackage#getOfPatternReturningValues_Type()
   * @model containment="true"
   * @generated
   */
  TypedBNode getType();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.OfPatternReturningValues#getType <em>Type</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Type</em>' containment reference.
   * @see #getType()
   * @generated
   */
  void setType(TypedBNode value);

} // OfPatternReturningValues
