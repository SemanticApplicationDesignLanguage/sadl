/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.TypeDeclaration;
import com.ge.research.sadl.sadl.TypedBNode;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Type Declaration</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.TypeDeclarationImpl#getInstName <em>Inst Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.TypeDeclarationImpl#getType <em>Type</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TypeDeclarationImpl extends MinimalEObjectImpl.Container implements TypeDeclaration
{
  /**
   * The cached value of the '{@link #getInstName() <em>Inst Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getInstName()
   * @generated
   * @ordered
   */
  protected ResourceName instName;

  /**
   * The cached value of the '{@link #getType() <em>Type</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getType()
   * @generated
   * @ordered
   */
  protected TypedBNode type;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected TypeDeclarationImpl()
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
    return SadlPackage.Literals.TYPE_DECLARATION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName getInstName()
  {
    return instName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetInstName(ResourceName newInstName, NotificationChain msgs)
  {
    ResourceName oldInstName = instName;
    instName = newInstName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.TYPE_DECLARATION__INST_NAME, oldInstName, newInstName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setInstName(ResourceName newInstName)
  {
    if (newInstName != instName)
    {
      NotificationChain msgs = null;
      if (instName != null)
        msgs = ((InternalEObject)instName).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.TYPE_DECLARATION__INST_NAME, null, msgs);
      if (newInstName != null)
        msgs = ((InternalEObject)newInstName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.TYPE_DECLARATION__INST_NAME, null, msgs);
      msgs = basicSetInstName(newInstName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.TYPE_DECLARATION__INST_NAME, newInstName, newInstName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public TypedBNode getType()
  {
    return type;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetType(TypedBNode newType, NotificationChain msgs)
  {
    TypedBNode oldType = type;
    type = newType;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.TYPE_DECLARATION__TYPE, oldType, newType);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setType(TypedBNode newType)
  {
    if (newType != type)
    {
      NotificationChain msgs = null;
      if (type != null)
        msgs = ((InternalEObject)type).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.TYPE_DECLARATION__TYPE, null, msgs);
      if (newType != null)
        msgs = ((InternalEObject)newType).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.TYPE_DECLARATION__TYPE, null, msgs);
      msgs = basicSetType(newType, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.TYPE_DECLARATION__TYPE, newType, newType));
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
      case SadlPackage.TYPE_DECLARATION__INST_NAME:
        return basicSetInstName(null, msgs);
      case SadlPackage.TYPE_DECLARATION__TYPE:
        return basicSetType(null, msgs);
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
      case SadlPackage.TYPE_DECLARATION__INST_NAME:
        return getInstName();
      case SadlPackage.TYPE_DECLARATION__TYPE:
        return getType();
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
      case SadlPackage.TYPE_DECLARATION__INST_NAME:
        setInstName((ResourceName)newValue);
        return;
      case SadlPackage.TYPE_DECLARATION__TYPE:
        setType((TypedBNode)newValue);
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
      case SadlPackage.TYPE_DECLARATION__INST_NAME:
        setInstName((ResourceName)null);
        return;
      case SadlPackage.TYPE_DECLARATION__TYPE:
        setType((TypedBNode)null);
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
      case SadlPackage.TYPE_DECLARATION__INST_NAME:
        return instName != null;
      case SadlPackage.TYPE_DECLARATION__TYPE:
        return type != null;
    }
    return super.eIsSet(featureID);
  }

} //TypeDeclarationImpl
