/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.EnumeratedAllValuesFrom;
import com.ge.research.sadl.sadl.PropertyOfClass;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Enumerated All Values From</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.EnumeratedAllValuesFromImpl#getRestricted <em>Restricted</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.EnumeratedAllValuesFromImpl#getEnumeration <em>Enumeration</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class EnumeratedAllValuesFromImpl extends StatementImpl implements EnumeratedAllValuesFrom
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
   * The cached value of the '{@link #getEnumeration() <em>Enumeration</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getEnumeration()
   * @generated
   * @ordered
   */
  protected EObject enumeration;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected EnumeratedAllValuesFromImpl()
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
    return SadlPackage.Literals.ENUMERATED_ALL_VALUES_FROM;
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
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ENUMERATED_ALL_VALUES_FROM__RESTRICTED, oldRestricted, newRestricted);
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
        msgs = ((InternalEObject)restricted).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ENUMERATED_ALL_VALUES_FROM__RESTRICTED, null, msgs);
      if (newRestricted != null)
        msgs = ((InternalEObject)newRestricted).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ENUMERATED_ALL_VALUES_FROM__RESTRICTED, null, msgs);
      msgs = basicSetRestricted(newRestricted, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ENUMERATED_ALL_VALUES_FROM__RESTRICTED, newRestricted, newRestricted));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EObject getEnumeration()
  {
    return enumeration;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetEnumeration(EObject newEnumeration, NotificationChain msgs)
  {
    EObject oldEnumeration = enumeration;
    enumeration = newEnumeration;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ENUMERATED_ALL_VALUES_FROM__ENUMERATION, oldEnumeration, newEnumeration);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setEnumeration(EObject newEnumeration)
  {
    if (newEnumeration != enumeration)
    {
      NotificationChain msgs = null;
      if (enumeration != null)
        msgs = ((InternalEObject)enumeration).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ENUMERATED_ALL_VALUES_FROM__ENUMERATION, null, msgs);
      if (newEnumeration != null)
        msgs = ((InternalEObject)newEnumeration).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ENUMERATED_ALL_VALUES_FROM__ENUMERATION, null, msgs);
      msgs = basicSetEnumeration(newEnumeration, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ENUMERATED_ALL_VALUES_FROM__ENUMERATION, newEnumeration, newEnumeration));
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
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM__RESTRICTED:
        return basicSetRestricted(null, msgs);
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM__ENUMERATION:
        return basicSetEnumeration(null, msgs);
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
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM__RESTRICTED:
        return getRestricted();
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM__ENUMERATION:
        return getEnumeration();
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
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM__RESTRICTED:
        setRestricted((PropertyOfClass)newValue);
        return;
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM__ENUMERATION:
        setEnumeration((EObject)newValue);
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
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM__RESTRICTED:
        setRestricted((PropertyOfClass)null);
        return;
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM__ENUMERATION:
        setEnumeration((EObject)null);
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
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM__RESTRICTED:
        return restricted != null;
      case SadlPackage.ENUMERATED_ALL_VALUES_FROM__ENUMERATION:
        return enumeration != null;
    }
    return super.eIsSet(featureID);
  }

} //EnumeratedAllValuesFromImpl
