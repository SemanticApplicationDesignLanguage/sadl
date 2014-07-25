/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.OrderList;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.SelectExpression;
import com.ge.research.sadl.sadl.VariableList;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Select Expression</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.SelectExpressionImpl#getDistinct <em>Distinct</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.SelectExpressionImpl#getAllVars <em>All Vars</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.SelectExpressionImpl#getVarList <em>Var List</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.SelectExpressionImpl#getOrderby <em>Orderby</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.SelectExpressionImpl#getOrderList <em>Order List</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SelectExpressionImpl extends ExpressionImpl implements SelectExpression
{
  /**
   * The default value of the '{@link #getDistinct() <em>Distinct</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDistinct()
   * @generated
   * @ordered
   */
  protected static final String DISTINCT_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getDistinct() <em>Distinct</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDistinct()
   * @generated
   * @ordered
   */
  protected String distinct = DISTINCT_EDEFAULT;

  /**
   * The default value of the '{@link #getAllVars() <em>All Vars</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAllVars()
   * @generated
   * @ordered
   */
  protected static final String ALL_VARS_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getAllVars() <em>All Vars</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAllVars()
   * @generated
   * @ordered
   */
  protected String allVars = ALL_VARS_EDEFAULT;

  /**
   * The cached value of the '{@link #getVarList() <em>Var List</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getVarList()
   * @generated
   * @ordered
   */
  protected VariableList varList;

  /**
   * The default value of the '{@link #getOrderby() <em>Orderby</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getOrderby()
   * @generated
   * @ordered
   */
  protected static final String ORDERBY_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getOrderby() <em>Orderby</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getOrderby()
   * @generated
   * @ordered
   */
  protected String orderby = ORDERBY_EDEFAULT;

  /**
   * The cached value of the '{@link #getOrderList() <em>Order List</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getOrderList()
   * @generated
   * @ordered
   */
  protected OrderList orderList;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected SelectExpressionImpl()
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
    return SadlPackage.Literals.SELECT_EXPRESSION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getDistinct()
  {
    return distinct;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setDistinct(String newDistinct)
  {
    String oldDistinct = distinct;
    distinct = newDistinct;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.SELECT_EXPRESSION__DISTINCT, oldDistinct, distinct));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getAllVars()
  {
    return allVars;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setAllVars(String newAllVars)
  {
    String oldAllVars = allVars;
    allVars = newAllVars;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.SELECT_EXPRESSION__ALL_VARS, oldAllVars, allVars));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public VariableList getVarList()
  {
    return varList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetVarList(VariableList newVarList, NotificationChain msgs)
  {
    VariableList oldVarList = varList;
    varList = newVarList;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.SELECT_EXPRESSION__VAR_LIST, oldVarList, newVarList);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setVarList(VariableList newVarList)
  {
    if (newVarList != varList)
    {
      NotificationChain msgs = null;
      if (varList != null)
        msgs = ((InternalEObject)varList).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.SELECT_EXPRESSION__VAR_LIST, null, msgs);
      if (newVarList != null)
        msgs = ((InternalEObject)newVarList).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.SELECT_EXPRESSION__VAR_LIST, null, msgs);
      msgs = basicSetVarList(newVarList, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.SELECT_EXPRESSION__VAR_LIST, newVarList, newVarList));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getOrderby()
  {
    return orderby;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setOrderby(String newOrderby)
  {
    String oldOrderby = orderby;
    orderby = newOrderby;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.SELECT_EXPRESSION__ORDERBY, oldOrderby, orderby));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public OrderList getOrderList()
  {
    return orderList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetOrderList(OrderList newOrderList, NotificationChain msgs)
  {
    OrderList oldOrderList = orderList;
    orderList = newOrderList;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.SELECT_EXPRESSION__ORDER_LIST, oldOrderList, newOrderList);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setOrderList(OrderList newOrderList)
  {
    if (newOrderList != orderList)
    {
      NotificationChain msgs = null;
      if (orderList != null)
        msgs = ((InternalEObject)orderList).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.SELECT_EXPRESSION__ORDER_LIST, null, msgs);
      if (newOrderList != null)
        msgs = ((InternalEObject)newOrderList).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.SELECT_EXPRESSION__ORDER_LIST, null, msgs);
      msgs = basicSetOrderList(newOrderList, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.SELECT_EXPRESSION__ORDER_LIST, newOrderList, newOrderList));
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
      case SadlPackage.SELECT_EXPRESSION__VAR_LIST:
        return basicSetVarList(null, msgs);
      case SadlPackage.SELECT_EXPRESSION__ORDER_LIST:
        return basicSetOrderList(null, msgs);
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
      case SadlPackage.SELECT_EXPRESSION__DISTINCT:
        return getDistinct();
      case SadlPackage.SELECT_EXPRESSION__ALL_VARS:
        return getAllVars();
      case SadlPackage.SELECT_EXPRESSION__VAR_LIST:
        return getVarList();
      case SadlPackage.SELECT_EXPRESSION__ORDERBY:
        return getOrderby();
      case SadlPackage.SELECT_EXPRESSION__ORDER_LIST:
        return getOrderList();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SadlPackage.SELECT_EXPRESSION__DISTINCT:
        setDistinct((String)newValue);
        return;
      case SadlPackage.SELECT_EXPRESSION__ALL_VARS:
        setAllVars((String)newValue);
        return;
      case SadlPackage.SELECT_EXPRESSION__VAR_LIST:
        setVarList((VariableList)newValue);
        return;
      case SadlPackage.SELECT_EXPRESSION__ORDERBY:
        setOrderby((String)newValue);
        return;
      case SadlPackage.SELECT_EXPRESSION__ORDER_LIST:
        setOrderList((OrderList)newValue);
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
      case SadlPackage.SELECT_EXPRESSION__DISTINCT:
        setDistinct(DISTINCT_EDEFAULT);
        return;
      case SadlPackage.SELECT_EXPRESSION__ALL_VARS:
        setAllVars(ALL_VARS_EDEFAULT);
        return;
      case SadlPackage.SELECT_EXPRESSION__VAR_LIST:
        setVarList((VariableList)null);
        return;
      case SadlPackage.SELECT_EXPRESSION__ORDERBY:
        setOrderby(ORDERBY_EDEFAULT);
        return;
      case SadlPackage.SELECT_EXPRESSION__ORDER_LIST:
        setOrderList((OrderList)null);
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
      case SadlPackage.SELECT_EXPRESSION__DISTINCT:
        return DISTINCT_EDEFAULT == null ? distinct != null : !DISTINCT_EDEFAULT.equals(distinct);
      case SadlPackage.SELECT_EXPRESSION__ALL_VARS:
        return ALL_VARS_EDEFAULT == null ? allVars != null : !ALL_VARS_EDEFAULT.equals(allVars);
      case SadlPackage.SELECT_EXPRESSION__VAR_LIST:
        return varList != null;
      case SadlPackage.SELECT_EXPRESSION__ORDERBY:
        return ORDERBY_EDEFAULT == null ? orderby != null : !ORDERBY_EDEFAULT.equals(orderby);
      case SadlPackage.SELECT_EXPRESSION__ORDER_LIST:
        return orderList != null;
    }
    return super.eIsSet(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String toString()
  {
    if (eIsProxy()) return super.toString();

    StringBuffer result = new StringBuffer(super.toString());
    result.append(" (distinct: ");
    result.append(distinct);
    result.append(", allVars: ");
    result.append(allVars);
    result.append(", orderby: ");
    result.append(orderby);
    result.append(')');
    return result.toString();
  }

} //SelectExpressionImpl
