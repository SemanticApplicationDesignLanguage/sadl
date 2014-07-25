/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.OrderElement;
import com.ge.research.sadl.sadl.OrderList;
import com.ge.research.sadl.sadl.SadlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Order List</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.OrderListImpl#getOrderList <em>Order List</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class OrderListImpl extends MinimalEObjectImpl.Container implements OrderList
{
  /**
   * The cached value of the '{@link #getOrderList() <em>Order List</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getOrderList()
   * @generated
   * @ordered
   */
  protected EList<OrderElement> orderList;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected OrderListImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return SadlPackage.Literals.ORDER_LIST;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<OrderElement> getOrderList()
  {
    if (orderList == null)
    {
      orderList = new EObjectContainmentEList<OrderElement>(OrderElement.class, this, SadlPackage.ORDER_LIST__ORDER_LIST);
    }
    return orderList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
  {
    switch (featureID)
    {
      case SadlPackage.ORDER_LIST__ORDER_LIST:
        return ((InternalEList<?>)getOrderList()).basicRemove(otherEnd, msgs);
    }
    return super.eInverseRemove(otherEnd, featureID, msgs);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case SadlPackage.ORDER_LIST__ORDER_LIST:
        return getOrderList();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SadlPackage.ORDER_LIST__ORDER_LIST:
        getOrderList().clear();
        getOrderList().addAll((Collection<? extends OrderElement>)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case SadlPackage.ORDER_LIST__ORDER_LIST:
        getOrderList().clear();
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case SadlPackage.ORDER_LIST__ORDER_LIST:
        return orderList != null && !orderList.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //OrderListImpl
