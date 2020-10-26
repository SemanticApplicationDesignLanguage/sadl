/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Instances All Different</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.InstancesAllDifferent#getInstances <em>Instances</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getInstancesAllDifferent()
 * @model
 * @generated
 */
public interface InstancesAllDifferent extends Statement
{
  /**
   * Returns the value of the '<em><b>Instances</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Instances</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Instances</em>' containment reference.
   * @see #setInstances(ExistingResourceList)
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstancesAllDifferent_Instances()
   * @model containment="true"
   * @generated
   */
  ExistingResourceList getInstances();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InstancesAllDifferent#getInstances <em>Instances</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Instances</em>' containment reference.
   * @see #getInstances()
   * @generated
   */
  void setInstances(ExistingResourceList value);

} // InstancesAllDifferent
