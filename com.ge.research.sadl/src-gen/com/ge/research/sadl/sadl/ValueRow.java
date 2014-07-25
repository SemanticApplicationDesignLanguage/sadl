/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Value Row</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.ValueRow#getExplicitValues <em>Explicit Values</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getValueRow()
 * @model
 * @generated
 */
public interface ValueRow extends EObject
{
  /**
   * Returns the value of the '<em><b>Explicit Values</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.ExplicitValue}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Explicit Values</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Explicit Values</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getValueRow_ExplicitValues()
   * @model containment="true"
   * @generated
   */
  EList<ExplicitValue> getExplicitValues();

} // ValueRow
