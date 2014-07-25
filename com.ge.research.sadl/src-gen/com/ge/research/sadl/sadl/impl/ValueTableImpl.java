/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.ValueRow;
import com.ge.research.sadl.sadl.ValueTable;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Value Table</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ValueTableImpl#getRow <em>Row</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ValueTableImpl#getRows <em>Rows</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ValueTableImpl extends MinimalEObjectImpl.Container implements ValueTable
{
  /**
   * The cached value of the '{@link #getRow() <em>Row</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRow()
   * @generated
   * @ordered
   */
  protected ValueRow row;

  /**
   * The cached value of the '{@link #getRows() <em>Rows</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRows()
   * @generated
   * @ordered
   */
  protected EList<ValueRow> rows;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ValueTableImpl()
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
    return SadlPackage.Literals.VALUE_TABLE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ValueRow getRow()
  {
    return row;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetRow(ValueRow newRow, NotificationChain msgs)
  {
    ValueRow oldRow = row;
    row = newRow;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.VALUE_TABLE__ROW, oldRow, newRow);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setRow(ValueRow newRow)
  {
    if (newRow != row)
    {
      NotificationChain msgs = null;
      if (row != null)
        msgs = ((InternalEObject)row).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.VALUE_TABLE__ROW, null, msgs);
      if (newRow != null)
        msgs = ((InternalEObject)newRow).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.VALUE_TABLE__ROW, null, msgs);
      msgs = basicSetRow(newRow, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.VALUE_TABLE__ROW, newRow, newRow));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<ValueRow> getRows()
  {
    if (rows == null)
    {
      rows = new EObjectContainmentEList<ValueRow>(ValueRow.class, this, SadlPackage.VALUE_TABLE__ROWS);
    }
    return rows;
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
      case SadlPackage.VALUE_TABLE__ROW:
        return basicSetRow(null, msgs);
      case SadlPackage.VALUE_TABLE__ROWS:
        return ((InternalEList<?>)getRows()).basicRemove(otherEnd, msgs);
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
      case SadlPackage.VALUE_TABLE__ROW:
        return getRow();
      case SadlPackage.VALUE_TABLE__ROWS:
        return getRows();
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
      case SadlPackage.VALUE_TABLE__ROW:
        setRow((ValueRow)newValue);
        return;
      case SadlPackage.VALUE_TABLE__ROWS:
        getRows().clear();
        getRows().addAll((Collection<? extends ValueRow>)newValue);
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
      case SadlPackage.VALUE_TABLE__ROW:
        setRow((ValueRow)null);
        return;
      case SadlPackage.VALUE_TABLE__ROWS:
        getRows().clear();
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
      case SadlPackage.VALUE_TABLE__ROW:
        return row != null;
      case SadlPackage.VALUE_TABLE__ROWS:
        return rows != null && !rows.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //ValueTableImpl
