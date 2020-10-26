/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Order List</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.OrderList#getOrderList <em>Order List</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getOrderList()
 * @model
 * @generated
 */
public interface OrderList extends EObject
{
  /**
   * Returns the value of the '<em><b>Order List</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.OrderElement}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Order List</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Order List</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getOrderList_OrderList()
   * @model containment="true"
   * @generated
   */
  EList<OrderElement> getOrderList();

} // OrderList
