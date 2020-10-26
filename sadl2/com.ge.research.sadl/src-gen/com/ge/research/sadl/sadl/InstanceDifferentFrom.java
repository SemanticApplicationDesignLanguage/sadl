/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Instance Different From</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.InstanceDifferentFrom#getInstName1 <em>Inst Name1</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.InstanceDifferentFrom#getInstName2 <em>Inst Name2</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getInstanceDifferentFrom()
 * @model
 * @generated
 */
public interface InstanceDifferentFrom extends Statement
{
  /**
   * Returns the value of the '<em><b>Inst Name1</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Inst Name1</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Inst Name1</em>' containment reference.
   * @see #setInstName1(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstanceDifferentFrom_InstName1()
   * @model containment="true"
   * @generated
   */
  ResourceByName getInstName1();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InstanceDifferentFrom#getInstName1 <em>Inst Name1</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Inst Name1</em>' containment reference.
   * @see #getInstName1()
   * @generated
   */
  void setInstName1(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Inst Name2</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Inst Name2</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Inst Name2</em>' containment reference.
   * @see #setInstName2(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstanceDifferentFrom_InstName2()
   * @model containment="true"
   * @generated
   */
  ResourceByName getInstName2();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InstanceDifferentFrom#getInstName2 <em>Inst Name2</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Inst Name2</em>' containment reference.
   * @see #getInstName2()
   * @generated
   */
  void setInstName2(ResourceByName value);

} // InstanceDifferentFrom
