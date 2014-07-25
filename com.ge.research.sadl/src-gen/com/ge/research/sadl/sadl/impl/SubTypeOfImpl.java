/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.SubTypeOf;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Sub Type Of</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.SubTypeOfImpl#getSubclass <em>Subclass</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.SubTypeOfImpl#getSuperclass <em>Superclass</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SubTypeOfImpl extends GraphPatternImpl implements SubTypeOf
{
  /**
   * The cached value of the '{@link #getSubclass() <em>Subclass</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getSubclass()
   * @generated
   * @ordered
   */
  protected ResourceByName subclass;

  /**
   * The cached value of the '{@link #getSuperclass() <em>Superclass</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getSuperclass()
   * @generated
   * @ordered
   */
  protected ResourceByName superclass;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected SubTypeOfImpl()
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
    return SadlPackage.Literals.SUB_TYPE_OF;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getSubclass()
  {
    return subclass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetSubclass(ResourceByName newSubclass, NotificationChain msgs)
  {
    ResourceByName oldSubclass = subclass;
    subclass = newSubclass;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.SUB_TYPE_OF__SUBCLASS, oldSubclass, newSubclass);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setSubclass(ResourceByName newSubclass)
  {
    if (newSubclass != subclass)
    {
      NotificationChain msgs = null;
      if (subclass != null)
        msgs = ((InternalEObject)subclass).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.SUB_TYPE_OF__SUBCLASS, null, msgs);
      if (newSubclass != null)
        msgs = ((InternalEObject)newSubclass).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.SUB_TYPE_OF__SUBCLASS, null, msgs);
      msgs = basicSetSubclass(newSubclass, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.SUB_TYPE_OF__SUBCLASS, newSubclass, newSubclass));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getSuperclass()
  {
    return superclass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetSuperclass(ResourceByName newSuperclass, NotificationChain msgs)
  {
    ResourceByName oldSuperclass = superclass;
    superclass = newSuperclass;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.SUB_TYPE_OF__SUPERCLASS, oldSuperclass, newSuperclass);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setSuperclass(ResourceByName newSuperclass)
  {
    if (newSuperclass != superclass)
    {
      NotificationChain msgs = null;
      if (superclass != null)
        msgs = ((InternalEObject)superclass).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.SUB_TYPE_OF__SUPERCLASS, null, msgs);
      if (newSuperclass != null)
        msgs = ((InternalEObject)newSuperclass).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.SUB_TYPE_OF__SUPERCLASS, null, msgs);
      msgs = basicSetSuperclass(newSuperclass, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.SUB_TYPE_OF__SUPERCLASS, newSuperclass, newSuperclass));
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
      case SadlPackage.SUB_TYPE_OF__SUBCLASS:
        return basicSetSubclass(null, msgs);
      case SadlPackage.SUB_TYPE_OF__SUPERCLASS:
        return basicSetSuperclass(null, msgs);
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
      case SadlPackage.SUB_TYPE_OF__SUBCLASS:
        return getSubclass();
      case SadlPackage.SUB_TYPE_OF__SUPERCLASS:
        return getSuperclass();
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
      case SadlPackage.SUB_TYPE_OF__SUBCLASS:
        setSubclass((ResourceByName)newValue);
        return;
      case SadlPackage.SUB_TYPE_OF__SUPERCLASS:
        setSuperclass((ResourceByName)newValue);
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
      case SadlPackage.SUB_TYPE_OF__SUBCLASS:
        setSubclass((ResourceByName)null);
        return;
      case SadlPackage.SUB_TYPE_OF__SUPERCLASS:
        setSuperclass((ResourceByName)null);
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
      case SadlPackage.SUB_TYPE_OF__SUBCLASS:
        return subclass != null;
      case SadlPackage.SUB_TYPE_OF__SUPERCLASS:
        return superclass != null;
    }
    return super.eIsSet(featureID);
  }

} //SubTypeOfImpl
