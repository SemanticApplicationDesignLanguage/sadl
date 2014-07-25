/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Merged Triples</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.MergedTriples#getOps <em>Ops</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.MergedTriples#getPivot <em>Pivot</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.MergedTriples#getWcs <em>Wcs</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getMergedTriples()
 * @model
 * @generated
 */
public interface MergedTriples extends GraphPattern
{
  /**
   * Returns the value of the '<em><b>Ops</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.OfPhrase}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Ops</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Ops</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getMergedTriples_Ops()
   * @model containment="true"
   * @generated
   */
  EList<OfPhrase> getOps();

  /**
   * Returns the value of the '<em><b>Pivot</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Pivot</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Pivot</em>' containment reference.
   * @see #setPivot(TypedBNode)
   * @see com.ge.research.sadl.sadl.SadlPackage#getMergedTriples_Pivot()
   * @model containment="true"
   * @generated
   */
  TypedBNode getPivot();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.MergedTriples#getPivot <em>Pivot</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Pivot</em>' containment reference.
   * @see #getPivot()
   * @generated
   */
  void setPivot(TypedBNode value);

  /**
   * Returns the value of the '<em><b>Wcs</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.WithChain}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Wcs</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Wcs</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getMergedTriples_Wcs()
   * @model containment="true"
   * @generated
   */
  EList<WithChain> getWcs();

} // MergedTriples
