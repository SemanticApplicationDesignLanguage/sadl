/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.DataTypeRestriction;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.UserDefinedDataType;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>User Defined Data Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.UserDefinedDataTypeImpl#getUdt <em>Udt</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.UserDefinedDataTypeImpl#getRestriction <em>Restriction</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class UserDefinedDataTypeImpl extends StatementImpl implements UserDefinedDataType
{
  /**
   * The default value of the '{@link #getUdt() <em>Udt</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getUdt()
   * @generated
   * @ordered
   */
  protected static final String UDT_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getUdt() <em>Udt</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getUdt()
   * @generated
   * @ordered
   */
  protected String udt = UDT_EDEFAULT;

  /**
   * The cached value of the '{@link #getRestriction() <em>Restriction</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRestriction()
   * @generated
   * @ordered
   */
  protected DataTypeRestriction restriction;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected UserDefinedDataTypeImpl()
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
    return SadlPackage.Literals.USER_DEFINED_DATA_TYPE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getUdt()
  {
    return udt;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setUdt(String newUdt)
  {
    String oldUdt = udt;
    udt = newUdt;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.USER_DEFINED_DATA_TYPE__UDT, oldUdt, udt));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public DataTypeRestriction getRestriction()
  {
    return restriction;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetRestriction(DataTypeRestriction newRestriction, NotificationChain msgs)
  {
    DataTypeRestriction oldRestriction = restriction;
    restriction = newRestriction;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.USER_DEFINED_DATA_TYPE__RESTRICTION, oldRestriction, newRestriction);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setRestriction(DataTypeRestriction newRestriction)
  {
    if (newRestriction != restriction)
    {
      NotificationChain msgs = null;
      if (restriction != null)
        msgs = ((InternalEObject)restriction).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.USER_DEFINED_DATA_TYPE__RESTRICTION, null, msgs);
      if (newRestriction != null)
        msgs = ((InternalEObject)newRestriction).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.USER_DEFINED_DATA_TYPE__RESTRICTION, null, msgs);
      msgs = basicSetRestriction(newRestriction, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.USER_DEFINED_DATA_TYPE__RESTRICTION, newRestriction, newRestriction));
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
      case SadlPackage.USER_DEFINED_DATA_TYPE__RESTRICTION:
        return basicSetRestriction(null, msgs);
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
      case SadlPackage.USER_DEFINED_DATA_TYPE__UDT:
        return getUdt();
      case SadlPackage.USER_DEFINED_DATA_TYPE__RESTRICTION:
        return getRestriction();
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
      case SadlPackage.USER_DEFINED_DATA_TYPE__UDT:
        setUdt((String)newValue);
        return;
      case SadlPackage.USER_DEFINED_DATA_TYPE__RESTRICTION:
        setRestriction((DataTypeRestriction)newValue);
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
      case SadlPackage.USER_DEFINED_DATA_TYPE__UDT:
        setUdt(UDT_EDEFAULT);
        return;
      case SadlPackage.USER_DEFINED_DATA_TYPE__RESTRICTION:
        setRestriction((DataTypeRestriction)null);
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
      case SadlPackage.USER_DEFINED_DATA_TYPE__UDT:
        return UDT_EDEFAULT == null ? udt != null : !UDT_EDEFAULT.equals(udt);
      case SadlPackage.USER_DEFINED_DATA_TYPE__RESTRICTION:
        return restriction != null;
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
    result.append(" (udt: ");
    result.append(udt);
    result.append(')');
    return result.toString();
  }

} //UserDefinedDataTypeImpl
