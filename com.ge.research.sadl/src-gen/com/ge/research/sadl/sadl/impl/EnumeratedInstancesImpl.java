/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.EnumeratedInstances;
import com.ge.research.sadl.sadl.ResourceList;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Enumerated Instances</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.EnumeratedInstancesImpl#getInstanceList <em>Instance List</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class EnumeratedInstancesImpl extends MinimalEObjectImpl.Container implements EnumeratedInstances
{
  /**
   * The cached value of the '{@link #getInstanceList() <em>Instance List</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getInstanceList()
   * @generated
   * @ordered
   */
  protected ResourceList instanceList;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected EnumeratedInstancesImpl()
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
    return SadlPackage.Literals.ENUMERATED_INSTANCES;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceList getInstanceList()
  {
    return instanceList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetInstanceList(ResourceList newInstanceList, NotificationChain msgs)
  {
    ResourceList oldInstanceList = instanceList;
    instanceList = newInstanceList;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ENUMERATED_INSTANCES__INSTANCE_LIST, oldInstanceList, newInstanceList);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setInstanceList(ResourceList newInstanceList)
  {
    if (newInstanceList != instanceList)
    {
      NotificationChain msgs = null;
      if (instanceList != null)
        msgs = ((InternalEObject)instanceList).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ENUMERATED_INSTANCES__INSTANCE_LIST, null, msgs);
      if (newInstanceList != null)
        msgs = ((InternalEObject)newInstanceList).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ENUMERATED_INSTANCES__INSTANCE_LIST, null, msgs);
      msgs = basicSetInstanceList(newInstanceList, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ENUMERATED_INSTANCES__INSTANCE_LIST, newInstanceList, newInstanceList));
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
      case SadlPackage.ENUMERATED_INSTANCES__INSTANCE_LIST:
        return basicSetInstanceList(null, msgs);
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
      case SadlPackage.ENUMERATED_INSTANCES__INSTANCE_LIST:
        return getInstanceList();
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
      case SadlPackage.ENUMERATED_INSTANCES__INSTANCE_LIST:
        setInstanceList((ResourceList)newValue);
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
      case SadlPackage.ENUMERATED_INSTANCES__INSTANCE_LIST:
        setInstanceList((ResourceList)null);
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
      case SadlPackage.ENUMERATED_INSTANCES__INSTANCE_LIST:
        return instanceList != null;
    }
    return super.eIsSet(featureID);
  }

} //EnumeratedInstancesImpl
