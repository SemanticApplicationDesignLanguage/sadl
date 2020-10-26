/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.InstanceDifferentFrom;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Instance Different From</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstanceDifferentFromImpl#getInstName1 <em>Inst Name1</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstanceDifferentFromImpl#getInstName2 <em>Inst Name2</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class InstanceDifferentFromImpl extends StatementImpl implements InstanceDifferentFrom
{
  /**
   * The cached value of the '{@link #getInstName1() <em>Inst Name1</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getInstName1()
   * @generated
   * @ordered
   */
  protected ResourceByName instName1;

  /**
   * The cached value of the '{@link #getInstName2() <em>Inst Name2</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getInstName2()
   * @generated
   * @ordered
   */
  protected ResourceByName instName2;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected InstanceDifferentFromImpl()
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
    return SadlPackage.Literals.INSTANCE_DIFFERENT_FROM;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getInstName1()
  {
    return instName1;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetInstName1(ResourceByName newInstName1, NotificationChain msgs)
  {
    ResourceByName oldInstName1 = instName1;
    instName1 = newInstName1;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME1, oldInstName1, newInstName1);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setInstName1(ResourceByName newInstName1)
  {
    if (newInstName1 != instName1)
    {
      NotificationChain msgs = null;
      if (instName1 != null)
        msgs = ((InternalEObject)instName1).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME1, null, msgs);
      if (newInstName1 != null)
        msgs = ((InternalEObject)newInstName1).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME1, null, msgs);
      msgs = basicSetInstName1(newInstName1, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME1, newInstName1, newInstName1));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getInstName2()
  {
    return instName2;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetInstName2(ResourceByName newInstName2, NotificationChain msgs)
  {
    ResourceByName oldInstName2 = instName2;
    instName2 = newInstName2;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME2, oldInstName2, newInstName2);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setInstName2(ResourceByName newInstName2)
  {
    if (newInstName2 != instName2)
    {
      NotificationChain msgs = null;
      if (instName2 != null)
        msgs = ((InternalEObject)instName2).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME2, null, msgs);
      if (newInstName2 != null)
        msgs = ((InternalEObject)newInstName2).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME2, null, msgs);
      msgs = basicSetInstName2(newInstName2, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME2, newInstName2, newInstName2));
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
      case SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME1:
        return basicSetInstName1(null, msgs);
      case SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME2:
        return basicSetInstName2(null, msgs);
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
      case SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME1:
        return getInstName1();
      case SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME2:
        return getInstName2();
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
      case SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME1:
        setInstName1((ResourceByName)newValue);
        return;
      case SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME2:
        setInstName2((ResourceByName)newValue);
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
      case SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME1:
        setInstName1((ResourceByName)null);
        return;
      case SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME2:
        setInstName2((ResourceByName)null);
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
      case SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME1:
        return instName1 != null;
      case SadlPackage.INSTANCE_DIFFERENT_FROM__INST_NAME2:
        return instName2 != null;
    }
    return super.eIsSet(featureID);
  }

} //InstanceDifferentFromImpl
