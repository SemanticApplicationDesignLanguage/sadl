/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Inst Attr SPV</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.InstAttrSPV#getSubj <em>Subj</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.InstAttrSPV#getProps <em>Props</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.InstAttrSPV#getVals <em>Vals</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getInstAttrSPV()
 * @model
 * @generated
 */
public interface InstAttrSPV extends GraphPattern
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstAttrSPV_Subj()
   * @model containment="true"
   * @generated
   */
  ResourceByName getSubj();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InstAttrSPV#getSubj <em>Subj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Subj</em>' containment reference.
   * @see #getSubj()
   * @generated
   */
  void setSubj(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Props</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.ResourceByName}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Props</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Props</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstAttrSPV_Props()
   * @model containment="true"
   * @generated
   */
  EList<ResourceByName> getProps();

  /**
   * Returns the value of the '<em><b>Vals</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.Expression}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Vals</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Vals</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstAttrSPV_Vals()
   * @model containment="true"
   * @generated
   */
  EList<Expression> getVals();

} // InstAttrSPV
