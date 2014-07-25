/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Range Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.RangeType#getClassIdentifier <em>Class Identifier</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getRangeType()
 * @model
 * @generated
 */
public interface RangeType extends EObject
{
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getRangeType_ClassIdentifier()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getClassIdentifier();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.RangeType#getClassIdentifier <em>Class Identifier</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class Identifier</em>' containment reference.
   * @see #getClassIdentifier()
   * @generated
   */
  void setClassIdentifier(ResourceIdentifier value);

} // RangeType
