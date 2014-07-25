/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.AllValuesCondition;
import com.ge.research.sadl.sadl.AllValuesFrom;
import com.ge.research.sadl.sadl.PropertyOfClass;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceIdentifier;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>All Values From</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AllValuesFromImpl#getRestricted <em>Restricted</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AllValuesFromImpl#getCond <em>Cond</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AllValuesFromImpl#getClassName <em>Class Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AllValuesFromImpl#getPropertyName <em>Property Name</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class AllValuesFromImpl extends StatementImpl implements AllValuesFrom
{
  /**
   * The cached value of the '{@link #getRestricted() <em>Restricted</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRestricted()
   * @generated
   * @ordered
   */
  protected PropertyOfClass restricted;

  /**
   * The cached value of the '{@link #getCond() <em>Cond</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getCond()
   * @generated
   * @ordered
   */
  protected AllValuesCondition cond;

  /**
   * The cached value of the '{@link #getClassName() <em>Class Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getClassName()
   * @generated
   * @ordered
   */
  protected ResourceIdentifier className;

  /**
   * The cached value of the '{@link #getPropertyName() <em>Property Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPropertyName()
   * @generated
   * @ordered
   */
  protected ResourceByName propertyName;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected AllValuesFromImpl()
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
    return SadlPackage.Literals.ALL_VALUES_FROM;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public PropertyOfClass getRestricted()
  {
    return restricted;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetRestricted(PropertyOfClass newRestricted, NotificationChain msgs)
  {
    PropertyOfClass oldRestricted = restricted;
    restricted = newRestricted;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ALL_VALUES_FROM__RESTRICTED, oldRestricted, newRestricted);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setRestricted(PropertyOfClass newRestricted)
  {
    if (newRestricted != restricted)
    {
      NotificationChain msgs = null;
      if (restricted != null)
        msgs = ((InternalEObject)restricted).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ALL_VALUES_FROM__RESTRICTED, null, msgs);
      if (newRestricted != null)
        msgs = ((InternalEObject)newRestricted).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ALL_VALUES_FROM__RESTRICTED, null, msgs);
      msgs = basicSetRestricted(newRestricted, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ALL_VALUES_FROM__RESTRICTED, newRestricted, newRestricted));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public AllValuesCondition getCond()
  {
    return cond;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetCond(AllValuesCondition newCond, NotificationChain msgs)
  {
    AllValuesCondition oldCond = cond;
    cond = newCond;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ALL_VALUES_FROM__COND, oldCond, newCond);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setCond(AllValuesCondition newCond)
  {
    if (newCond != cond)
    {
      NotificationChain msgs = null;
      if (cond != null)
        msgs = ((InternalEObject)cond).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ALL_VALUES_FROM__COND, null, msgs);
      if (newCond != null)
        msgs = ((InternalEObject)newCond).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ALL_VALUES_FROM__COND, null, msgs);
      msgs = basicSetCond(newCond, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ALL_VALUES_FROM__COND, newCond, newCond));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceIdentifier getClassName()
  {
    return className;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetClassName(ResourceIdentifier newClassName, NotificationChain msgs)
  {
    ResourceIdentifier oldClassName = className;
    className = newClassName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ALL_VALUES_FROM__CLASS_NAME, oldClassName, newClassName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setClassName(ResourceIdentifier newClassName)
  {
    if (newClassName != className)
    {
      NotificationChain msgs = null;
      if (className != null)
        msgs = ((InternalEObject)className).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ALL_VALUES_FROM__CLASS_NAME, null, msgs);
      if (newClassName != null)
        msgs = ((InternalEObject)newClassName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ALL_VALUES_FROM__CLASS_NAME, null, msgs);
      msgs = basicSetClassName(newClassName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ALL_VALUES_FROM__CLASS_NAME, newClassName, newClassName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getPropertyName()
  {
    return propertyName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPropertyName(ResourceByName newPropertyName, NotificationChain msgs)
  {
    ResourceByName oldPropertyName = propertyName;
    propertyName = newPropertyName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ALL_VALUES_FROM__PROPERTY_NAME, oldPropertyName, newPropertyName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPropertyName(ResourceByName newPropertyName)
  {
    if (newPropertyName != propertyName)
    {
      NotificationChain msgs = null;
      if (propertyName != null)
        msgs = ((InternalEObject)propertyName).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ALL_VALUES_FROM__PROPERTY_NAME, null, msgs);
      if (newPropertyName != null)
        msgs = ((InternalEObject)newPropertyName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ALL_VALUES_FROM__PROPERTY_NAME, null, msgs);
      msgs = basicSetPropertyName(newPropertyName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ALL_VALUES_FROM__PROPERTY_NAME, newPropertyName, newPropertyName));
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
      case SadlPackage.ALL_VALUES_FROM__RESTRICTED:
        return basicSetRestricted(null, msgs);
      case SadlPackage.ALL_VALUES_FROM__COND:
        return basicSetCond(null, msgs);
      case SadlPackage.ALL_VALUES_FROM__CLASS_NAME:
        return basicSetClassName(null, msgs);
      case SadlPackage.ALL_VALUES_FROM__PROPERTY_NAME:
        return basicSetPropertyName(null, msgs);
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
      case SadlPackage.ALL_VALUES_FROM__RESTRICTED:
        return getRestricted();
      case SadlPackage.ALL_VALUES_FROM__COND:
        return getCond();
      case SadlPackage.ALL_VALUES_FROM__CLASS_NAME:
        return getClassName();
      case SadlPackage.ALL_VALUES_FROM__PROPERTY_NAME:
        return getPropertyName();
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
      case SadlPackage.ALL_VALUES_FROM__RESTRICTED:
        setRestricted((PropertyOfClass)newValue);
        return;
      case SadlPackage.ALL_VALUES_FROM__COND:
        setCond((AllValuesCondition)newValue);
        return;
      case SadlPackage.ALL_VALUES_FROM__CLASS_NAME:
        setClassName((ResourceIdentifier)newValue);
        return;
      case SadlPackage.ALL_VALUES_FROM__PROPERTY_NAME:
        setPropertyName((ResourceByName)newValue);
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
      case SadlPackage.ALL_VALUES_FROM__RESTRICTED:
        setRestricted((PropertyOfClass)null);
        return;
      case SadlPackage.ALL_VALUES_FROM__COND:
        setCond((AllValuesCondition)null);
        return;
      case SadlPackage.ALL_VALUES_FROM__CLASS_NAME:
        setClassName((ResourceIdentifier)null);
        return;
      case SadlPackage.ALL_VALUES_FROM__PROPERTY_NAME:
        setPropertyName((ResourceByName)null);
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
      case SadlPackage.ALL_VALUES_FROM__RESTRICTED:
        return restricted != null;
      case SadlPackage.ALL_VALUES_FROM__COND:
        return cond != null;
      case SadlPackage.ALL_VALUES_FROM__CLASS_NAME:
        return className != null;
      case SadlPackage.ALL_VALUES_FROM__PROPERTY_NAME:
        return propertyName != null;
    }
    return super.eIsSet(featureID);
  }

} //AllValuesFromImpl
