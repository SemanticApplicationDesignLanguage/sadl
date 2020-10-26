/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Subj Prop</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.SubjProp#getSubj <em>Subj</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.SubjProp#getHwPhr <em>Hw Phr</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getSubjProp()
 * @model
 * @generated
 */
public interface SubjProp extends GraphPattern
{
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getSubjProp_Subj()
   * @model containment="true"
   * @generated
   */
  ResourceByName getSubj();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SubjProp#getSubj <em>Subj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Subj</em>' containment reference.
   * @see #getSubj()
   * @generated
   */
  void setSubj(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Hw Phr</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.WithPhrase}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Hw Phr</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Hw Phr</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getSubjProp_HwPhr()
   * @model containment="true"
   * @generated
   */
  EList<WithPhrase> getHwPhr();

} // SubjProp
