/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.InverseProperty;
import com.ge.research.sadl.sadl.IsInverseOf;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Inverse Property</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InversePropertyImpl#getPropertyName1 <em>Property Name1</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InversePropertyImpl#getInvOf <em>Inv Of</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class InversePropertyImpl extends StatementImpl implements InverseProperty
{
  /**
   * The cached value of the '{@link #getPropertyName1() <em>Property Name1</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPropertyName1()
   * @generated
   * @ordered
   */
  protected ResourceByName propertyName1;

  /**
   * The cached value of the '{@link #getInvOf() <em>Inv Of</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getInvOf()
   * @generated
   * @ordered
   */
  protected IsInverseOf invOf;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected InversePropertyImpl()
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
    return SadlPackage.Literals.INVERSE_PROPERTY;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getPropertyName1()
  {
    return propertyName1;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPropertyName1(ResourceByName newPropertyName1, NotificationChain msgs)
  {
    ResourceByName oldPropertyName1 = propertyName1;
    propertyName1 = newPropertyName1;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.INVERSE_PROPERTY__PROPERTY_NAME1, oldPropertyName1, newPropertyName1);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPropertyName1(ResourceByName newPropertyName1)
  {
    if (newPropertyName1 != propertyName1)
    {
      NotificationChain msgs = null;
      if (propertyName1 != null)
        msgs = ((InternalEObject)propertyName1).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INVERSE_PROPERTY__PROPERTY_NAME1, null, msgs);
      if (newPropertyName1 != null)
        msgs = ((InternalEObject)newPropertyName1).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INVERSE_PROPERTY__PROPERTY_NAME1, null, msgs);
      msgs = basicSetPropertyName1(newPropertyName1, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INVERSE_PROPERTY__PROPERTY_NAME1, newPropertyName1, newPropertyName1));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public IsInverseOf getInvOf()
  {
    return invOf;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetInvOf(IsInverseOf newInvOf, NotificationChain msgs)
  {
    IsInverseOf oldInvOf = invOf;
    invOf = newInvOf;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.INVERSE_PROPERTY__INV_OF, oldInvOf, newInvOf);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setInvOf(IsInverseOf newInvOf)
  {
    if (newInvOf != invOf)
    {
      NotificationChain msgs = null;
      if (invOf != null)
        msgs = ((InternalEObject)invOf).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INVERSE_PROPERTY__INV_OF, null, msgs);
      if (newInvOf != null)
        msgs = ((InternalEObject)newInvOf).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INVERSE_PROPERTY__INV_OF, null, msgs);
      msgs = basicSetInvOf(newInvOf, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INVERSE_PROPERTY__INV_OF, newInvOf, newInvOf));
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
      case SadlPackage.INVERSE_PROPERTY__PROPERTY_NAME1:
        return basicSetPropertyName1(null, msgs);
      case SadlPackage.INVERSE_PROPERTY__INV_OF:
        return basicSetInvOf(null, msgs);
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
      case SadlPackage.INVERSE_PROPERTY__PROPERTY_NAME1:
        return getPropertyName1();
      case SadlPackage.INVERSE_PROPERTY__INV_OF:
        return getInvOf();
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
      case SadlPackage.INVERSE_PROPERTY__PROPERTY_NAME1:
        setPropertyName1((ResourceByName)newValue);
        return;
      case SadlPackage.INVERSE_PROPERTY__INV_OF:
        setInvOf((IsInverseOf)newValue);
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
      case SadlPackage.INVERSE_PROPERTY__PROPERTY_NAME1:
        setPropertyName1((ResourceByName)null);
        return;
      case SadlPackage.INVERSE_PROPERTY__INV_OF:
        setInvOf((IsInverseOf)null);
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
      case SadlPackage.INVERSE_PROPERTY__PROPERTY_NAME1:
        return propertyName1 != null;
      case SadlPackage.INVERSE_PROPERTY__INV_OF:
        return invOf != null;
    }
    return super.eIsSet(featureID);
  }

} //InversePropertyImpl
