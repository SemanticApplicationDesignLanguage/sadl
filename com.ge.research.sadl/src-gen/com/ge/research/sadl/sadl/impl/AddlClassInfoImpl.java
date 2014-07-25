/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.AddlClassInfo;
import com.ge.research.sadl.sadl.Condition;
import com.ge.research.sadl.sadl.Range;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Addl Class Info</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AddlClassInfoImpl#getPropertyByName <em>Property By Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AddlClassInfoImpl#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AddlClassInfoImpl#getRange <em>Range</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AddlClassInfoImpl#getRestriction <em>Restriction</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class AddlClassInfoImpl extends MinimalEObjectImpl.Container implements AddlClassInfo
{
  /**
   * The cached value of the '{@link #getPropertyByName() <em>Property By Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPropertyByName()
   * @generated
   * @ordered
   */
  protected ResourceByName propertyByName;

  /**
   * The cached value of the '{@link #getPropertyName() <em>Property Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPropertyName()
   * @generated
   * @ordered
   */
  protected ResourceName propertyName;

  /**
   * The cached value of the '{@link #getRange() <em>Range</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRange()
   * @generated
   * @ordered
   */
  protected Range range;

  /**
   * The cached value of the '{@link #getRestriction() <em>Restriction</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRestriction()
   * @generated
   * @ordered
   */
  protected Condition restriction;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected AddlClassInfoImpl()
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
    return SadlPackage.Literals.ADDL_CLASS_INFO;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getPropertyByName()
  {
    return propertyByName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPropertyByName(ResourceByName newPropertyByName, NotificationChain msgs)
  {
    ResourceByName oldPropertyByName = propertyByName;
    propertyByName = newPropertyByName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ADDL_CLASS_INFO__PROPERTY_BY_NAME, oldPropertyByName, newPropertyByName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPropertyByName(ResourceByName newPropertyByName)
  {
    if (newPropertyByName != propertyByName)
    {
      NotificationChain msgs = null;
      if (propertyByName != null)
        msgs = ((InternalEObject)propertyByName).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDL_CLASS_INFO__PROPERTY_BY_NAME, null, msgs);
      if (newPropertyByName != null)
        msgs = ((InternalEObject)newPropertyByName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDL_CLASS_INFO__PROPERTY_BY_NAME, null, msgs);
      msgs = basicSetPropertyByName(newPropertyByName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDL_CLASS_INFO__PROPERTY_BY_NAME, newPropertyByName, newPropertyByName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName getPropertyName()
  {
    return propertyName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPropertyName(ResourceName newPropertyName, NotificationChain msgs)
  {
    ResourceName oldPropertyName = propertyName;
    propertyName = newPropertyName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ADDL_CLASS_INFO__PROPERTY_NAME, oldPropertyName, newPropertyName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPropertyName(ResourceName newPropertyName)
  {
    if (newPropertyName != propertyName)
    {
      NotificationChain msgs = null;
      if (propertyName != null)
        msgs = ((InternalEObject)propertyName).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDL_CLASS_INFO__PROPERTY_NAME, null, msgs);
      if (newPropertyName != null)
        msgs = ((InternalEObject)newPropertyName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDL_CLASS_INFO__PROPERTY_NAME, null, msgs);
      msgs = basicSetPropertyName(newPropertyName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDL_CLASS_INFO__PROPERTY_NAME, newPropertyName, newPropertyName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Range getRange()
  {
    return range;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetRange(Range newRange, NotificationChain msgs)
  {
    Range oldRange = range;
    range = newRange;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ADDL_CLASS_INFO__RANGE, oldRange, newRange);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setRange(Range newRange)
  {
    if (newRange != range)
    {
      NotificationChain msgs = null;
      if (range != null)
        msgs = ((InternalEObject)range).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDL_CLASS_INFO__RANGE, null, msgs);
      if (newRange != null)
        msgs = ((InternalEObject)newRange).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDL_CLASS_INFO__RANGE, null, msgs);
      msgs = basicSetRange(newRange, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDL_CLASS_INFO__RANGE, newRange, newRange));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Condition getRestriction()
  {
    return restriction;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetRestriction(Condition newRestriction, NotificationChain msgs)
  {
    Condition oldRestriction = restriction;
    restriction = newRestriction;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ADDL_CLASS_INFO__RESTRICTION, oldRestriction, newRestriction);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setRestriction(Condition newRestriction)
  {
    if (newRestriction != restriction)
    {
      NotificationChain msgs = null;
      if (restriction != null)
        msgs = ((InternalEObject)restriction).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDL_CLASS_INFO__RESTRICTION, null, msgs);
      if (newRestriction != null)
        msgs = ((InternalEObject)newRestriction).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDL_CLASS_INFO__RESTRICTION, null, msgs);
      msgs = basicSetRestriction(newRestriction, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDL_CLASS_INFO__RESTRICTION, newRestriction, newRestriction));
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
      case SadlPackage.ADDL_CLASS_INFO__PROPERTY_BY_NAME:
        return basicSetPropertyByName(null, msgs);
      case SadlPackage.ADDL_CLASS_INFO__PROPERTY_NAME:
        return basicSetPropertyName(null, msgs);
      case SadlPackage.ADDL_CLASS_INFO__RANGE:
        return basicSetRange(null, msgs);
      case SadlPackage.ADDL_CLASS_INFO__RESTRICTION:
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
      case SadlPackage.ADDL_CLASS_INFO__PROPERTY_BY_NAME:
        return getPropertyByName();
      case SadlPackage.ADDL_CLASS_INFO__PROPERTY_NAME:
        return getPropertyName();
      case SadlPackage.ADDL_CLASS_INFO__RANGE:
        return getRange();
      case SadlPackage.ADDL_CLASS_INFO__RESTRICTION:
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
      case SadlPackage.ADDL_CLASS_INFO__PROPERTY_BY_NAME:
        setPropertyByName((ResourceByName)newValue);
        return;
      case SadlPackage.ADDL_CLASS_INFO__PROPERTY_NAME:
        setPropertyName((ResourceName)newValue);
        return;
      case SadlPackage.ADDL_CLASS_INFO__RANGE:
        setRange((Range)newValue);
        return;
      case SadlPackage.ADDL_CLASS_INFO__RESTRICTION:
        setRestriction((Condition)newValue);
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
      case SadlPackage.ADDL_CLASS_INFO__PROPERTY_BY_NAME:
        setPropertyByName((ResourceByName)null);
        return;
      case SadlPackage.ADDL_CLASS_INFO__PROPERTY_NAME:
        setPropertyName((ResourceName)null);
        return;
      case SadlPackage.ADDL_CLASS_INFO__RANGE:
        setRange((Range)null);
        return;
      case SadlPackage.ADDL_CLASS_INFO__RESTRICTION:
        setRestriction((Condition)null);
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
      case SadlPackage.ADDL_CLASS_INFO__PROPERTY_BY_NAME:
        return propertyByName != null;
      case SadlPackage.ADDL_CLASS_INFO__PROPERTY_NAME:
        return propertyName != null;
      case SadlPackage.ADDL_CLASS_INFO__RANGE:
        return range != null;
      case SadlPackage.ADDL_CLASS_INFO__RESTRICTION:
        return restriction != null;
    }
    return super.eIsSet(featureID);
  }

} //AddlClassInfoImpl
