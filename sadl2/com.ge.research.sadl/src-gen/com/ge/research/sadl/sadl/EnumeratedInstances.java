/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Enumerated Instances</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.EnumeratedInstances#getInstanceList <em>Instance List</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getEnumeratedInstances()
 * @model
 * @generated
 */
public interface EnumeratedInstances extends EObject
{
  /**
   * Returns the value of the '<em><b>Instance List</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Instance List</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Instance List</em>' containment reference.
   * @see #setInstanceList(ResourceList)
   * @see com.ge.research.sadl.sadl.SadlPackage#getEnumeratedInstances_InstanceList()
   * @model containment="true"
   * @generated
   */
  ResourceList getInstanceList();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.EnumeratedInstances#getInstanceList <em>Instance List</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Instance List</em>' containment reference.
   * @see #getInstanceList()
   * @generated
   */
  void setInstanceList(ResourceList value);

} // EnumeratedInstances
