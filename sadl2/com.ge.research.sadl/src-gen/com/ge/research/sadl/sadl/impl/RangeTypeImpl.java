/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.DataType;
import com.ge.research.sadl.sadl.RangeType;
import com.ge.research.sadl.sadl.ResourceIdentifier;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Range Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.RangeTypeImpl#getClassIdentifier <em>Class Identifier</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.RangeTypeImpl#getDataType <em>Data Type</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class RangeTypeImpl extends MinimalEObjectImpl.Container implements RangeType
{
  /**
   * The cached value of the '{@link #getClassIdentifier() <em>Class Identifier</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getClassIdentifier()
   * @generated
   * @ordered
   */
  protected ResourceIdentifier classIdentifier;

  /**
   * The default value of the '{@link #getDataType() <em>Data Type</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDataType()
   * @generated
   * @ordered
   */
  protected static final DataType DATA_TYPE_EDEFAULT = DataType.STRING;

  /**
   * The cached value of the '{@link #getDataType() <em>Data Type</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDataType()
   * @generated
   * @ordered
   */
  protected DataType dataType = DATA_TYPE_EDEFAULT;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected RangeTypeImpl()
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
    return SadlPackage.Literals.RANGE_TYPE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceIdentifier getClassIdentifier()
  {
    return classIdentifier;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetClassIdentifier(ResourceIdentifier newClassIdentifier, NotificationChain msgs)
  {
    ResourceIdentifier oldClassIdentifier = classIdentifier;
    classIdentifier = newClassIdentifier;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.RANGE_TYPE__CLASS_IDENTIFIER, oldClassIdentifier, newClassIdentifier);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setClassIdentifier(ResourceIdentifier newClassIdentifier)
  {
    if (newClassIdentifier != classIdentifier)
    {
      NotificationChain msgs = null;
      if (classIdentifier != null)
        msgs = ((InternalEObject)classIdentifier).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RANGE_TYPE__CLASS_IDENTIFIER, null, msgs);
      if (newClassIdentifier != null)
        msgs = ((InternalEObject)newClassIdentifier).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RANGE_TYPE__CLASS_IDENTIFIER, null, msgs);
      msgs = basicSetClassIdentifier(newClassIdentifier, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RANGE_TYPE__CLASS_IDENTIFIER, newClassIdentifier, newClassIdentifier));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public DataType getDataType()
  {
    return dataType;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setDataType(DataType newDataType)
  {
    DataType oldDataType = dataType;
    dataType = newDataType == null ? DATA_TYPE_EDEFAULT : newDataType;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RANGE_TYPE__DATA_TYPE, oldDataType, dataType));
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
      case SadlPackage.RANGE_TYPE__CLASS_IDENTIFIER:
        return basicSetClassIdentifier(null, msgs);
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
      case SadlPackage.RANGE_TYPE__CLASS_IDENTIFIER:
        return getClassIdentifier();
      case SadlPackage.RANGE_TYPE__DATA_TYPE:
        return getDataType();
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
      case SadlPackage.RANGE_TYPE__CLASS_IDENTIFIER:
        setClassIdentifier((ResourceIdentifier)newValue);
        return;
      case SadlPackage.RANGE_TYPE__DATA_TYPE:
        setDataType((DataType)newValue);
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
      case SadlPackage.RANGE_TYPE__CLASS_IDENTIFIER:
        setClassIdentifier((ResourceIdentifier)null);
        return;
      case SadlPackage.RANGE_TYPE__DATA_TYPE:
        setDataType(DATA_TYPE_EDEFAULT);
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
      case SadlPackage.RANGE_TYPE__CLASS_IDENTIFIER:
        return classIdentifier != null;
      case SadlPackage.RANGE_TYPE__DATA_TYPE:
        return dataType != DATA_TYPE_EDEFAULT;
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
    result.append(" (dataType: ");
    result.append(dataType);
    result.append(')');
    return result.toString();
  }

} //RangeTypeImpl
