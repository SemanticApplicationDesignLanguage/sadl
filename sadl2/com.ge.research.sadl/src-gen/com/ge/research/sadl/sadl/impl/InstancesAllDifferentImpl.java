/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ExistingResourceList;
import com.ge.research.sadl.sadl.InstancesAllDifferent;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Instances All Different</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstancesAllDifferentImpl#getInstances <em>Instances</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class InstancesAllDifferentImpl extends StatementImpl implements InstancesAllDifferent
{
  /**
   * The cached value of the '{@link #getInstances() <em>Instances</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getInstances()
   * @generated
   * @ordered
   */
  protected ExistingResourceList instances;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected InstancesAllDifferentImpl()
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
    return SadlPackage.Literals.INSTANCES_ALL_DIFFERENT;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExistingResourceList getInstances()
  {
    return instances;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetInstances(ExistingResourceList newInstances, NotificationChain msgs)
  {
    ExistingResourceList oldInstances = instances;
    instances = newInstances;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCES_ALL_DIFFERENT__INSTANCES, oldInstances, newInstances);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setInstances(ExistingResourceList newInstances)
  {
    if (newInstances != instances)
    {
      NotificationChain msgs = null;
      if (instances != null)
        msgs = ((InternalEObject)instances).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCES_ALL_DIFFERENT__INSTANCES, null, msgs);
      if (newInstances != null)
        msgs = ((InternalEObject)newInstances).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCES_ALL_DIFFERENT__INSTANCES, null, msgs);
      msgs = basicSetInstances(newInstances, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCES_ALL_DIFFERENT__INSTANCES, newInstances, newInstances));
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
      case SadlPackage.INSTANCES_ALL_DIFFERENT__INSTANCES:
        return basicSetInstances(null, msgs);
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
      case SadlPackage.INSTANCES_ALL_DIFFERENT__INSTANCES:
        return getInstances();
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
      case SadlPackage.INSTANCES_ALL_DIFFERENT__INSTANCES:
        setInstances((ExistingResourceList)newValue);
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
      case SadlPackage.INSTANCES_ALL_DIFFERENT__INSTANCES:
        setInstances((ExistingResourceList)null);
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
      case SadlPackage.INSTANCES_ALL_DIFFERENT__INSTANCES:
        return instances != null;
    }
    return super.eIsSet(featureID);
  }

} //InstancesAllDifferentImpl
