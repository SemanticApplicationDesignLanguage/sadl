/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.IsInverseOf;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Is Inverse Of</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.IsInverseOfImpl#getPropertyName2 <em>Property Name2</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class IsInverseOfImpl extends MinimalEObjectImpl.Container implements IsInverseOf
{
  /**
   * The cached value of the '{@link #getPropertyName2() <em>Property Name2</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPropertyName2()
   * @generated
   * @ordered
   */
  protected ResourceByName propertyName2;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected IsInverseOfImpl()
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
    return SadlPackage.Literals.IS_INVERSE_OF;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getPropertyName2()
  {
    return propertyName2;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPropertyName2(ResourceByName newPropertyName2, NotificationChain msgs)
  {
    ResourceByName oldPropertyName2 = propertyName2;
    propertyName2 = newPropertyName2;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.IS_INVERSE_OF__PROPERTY_NAME2, oldPropertyName2, newPropertyName2);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPropertyName2(ResourceByName newPropertyName2)
  {
    if (newPropertyName2 != propertyName2)
    {
      NotificationChain msgs = null;
      if (propertyName2 != null)
        msgs = ((InternalEObject)propertyName2).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.IS_INVERSE_OF__PROPERTY_NAME2, null, msgs);
      if (newPropertyName2 != null)
        msgs = ((InternalEObject)newPropertyName2).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.IS_INVERSE_OF__PROPERTY_NAME2, null, msgs);
      msgs = basicSetPropertyName2(newPropertyName2, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.IS_INVERSE_OF__PROPERTY_NAME2, newPropertyName2, newPropertyName2));
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
      case SadlPackage.IS_INVERSE_OF__PROPERTY_NAME2:
        return basicSetPropertyName2(null, msgs);
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
      case SadlPackage.IS_INVERSE_OF__PROPERTY_NAME2:
        return getPropertyName2();
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
      case SadlPackage.IS_INVERSE_OF__PROPERTY_NAME2:
        setPropertyName2((ResourceByName)newValue);
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
      case SadlPackage.IS_INVERSE_OF__PROPERTY_NAME2:
        setPropertyName2((ResourceByName)null);
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
      case SadlPackage.IS_INVERSE_OF__PROPERTY_NAME2:
        return propertyName2 != null;
    }
    return super.eIsSet(featureID);
  }

} //IsInverseOfImpl
