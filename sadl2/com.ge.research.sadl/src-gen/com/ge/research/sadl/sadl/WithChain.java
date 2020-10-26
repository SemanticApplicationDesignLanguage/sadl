/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>With Chain</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.WithChain#getWps <em>Wps</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getWithChain()
 * @model
 * @generated
 */
public interface WithChain extends EObject
{
  /**
   * Returns the value of the '<em><b>Wps</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.WithPhrase}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Wps</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Wps</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getWithChain_Wps()
   * @model containment="true"
   * @generated
   */
  EList<WithPhrase> getWps();

} // WithChain
