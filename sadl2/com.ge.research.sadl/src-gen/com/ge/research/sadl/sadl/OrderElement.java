/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Order Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.OrderElement#getOrder <em>Order</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.OrderElement#getName <em>Name</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getOrderElement()
 * @model
 * @generated
 */
public interface OrderElement extends EObject
{
  /**
   * Returns the value of the '<em><b>Order</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Order</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Order</em>' attribute.
   * @see #setOrder(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getOrderElement_Order()
   * @model
   * @generated
   */
  String getOrder();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.OrderElement#getOrder <em>Order</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Order</em>' attribute.
   * @see #getOrder()
   * @generated
   */
  void setOrder(String value);

  /**
   * Returns the value of the '<em><b>Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Name</em>' containment reference.
   * @see #setName(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getOrderElement_Name()
   * @model containment="true"
   * @generated
   */
  ResourceName getName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.OrderElement#getName <em>Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Name</em>' containment reference.
   * @see #getName()
   * @generated
   */
  void setName(ResourceName value);

} // OrderElement
