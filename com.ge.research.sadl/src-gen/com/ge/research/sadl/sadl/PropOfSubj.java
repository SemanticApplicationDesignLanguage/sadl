/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Prop Of Subj</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.PropOfSubj#getOfPhr <em>Of Phr</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropOfSubj#getSubj <em>Subj</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getPropOfSubj()
 * @model
 * @generated
 */
public interface PropOfSubj extends GraphPattern
{
  /**
   * Returns the value of the '<em><b>Of Phr</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.OfPhrase}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Of Phr</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Of Phr</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropOfSubj_OfPhr()
   * @model containment="true"
   * @generated
   */
  EList<OfPhrase> getOfPhr();

  /**
   * Returns the value of the '<em><b>Subj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Subj</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Subj</em>' containment reference.
   * @see #setSubj(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropOfSubj_Subj()
   * @model containment="true"
   * @generated
   */
  ResourceByName getSubj();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropOfSubj#getSubj <em>Subj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Subj</em>' containment reference.
   * @see #getSubj()
   * @generated
   */
  void setSubj(ResourceByName value);

} // PropOfSubj
