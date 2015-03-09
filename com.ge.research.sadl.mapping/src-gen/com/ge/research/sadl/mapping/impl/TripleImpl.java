/**
 */
package com.ge.research.sadl.mapping.impl;

import com.ge.research.sadl.mapping.MappingPackage;
import com.ge.research.sadl.mapping.Triple;

import com.ge.research.sadl.sadl.ResourceName;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Triple</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.mapping.impl.TripleImpl#getSubj <em>Subj</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.impl.TripleImpl#getPred <em>Pred</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.impl.TripleImpl#getObjval <em>Objval</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TripleImpl extends MinimalEObjectImpl.Container implements Triple
{
  /**
   * The cached value of the '{@link #getSubj() <em>Subj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getSubj()
   * @generated
   * @ordered
   */
  protected EObject subj;

  /**
   * The cached value of the '{@link #getPred() <em>Pred</em>}' reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPred()
   * @generated
   * @ordered
   */
  protected ResourceName pred;

  /**
   * The cached value of the '{@link #getObjval() <em>Objval</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getObjval()
   * @generated
   * @ordered
   */
  protected EObject objval;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected TripleImpl()
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
    return MappingPackage.Literals.TRIPLE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EObject getSubj()
  {
    return subj;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetSubj(EObject newSubj, NotificationChain msgs)
  {
    EObject oldSubj = subj;
    subj = newSubj;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, MappingPackage.TRIPLE__SUBJ, oldSubj, newSubj);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setSubj(EObject newSubj)
  {
    if (newSubj != subj)
    {
      NotificationChain msgs = null;
      if (subj != null)
        msgs = ((InternalEObject)subj).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - MappingPackage.TRIPLE__SUBJ, null, msgs);
      if (newSubj != null)
        msgs = ((InternalEObject)newSubj).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - MappingPackage.TRIPLE__SUBJ, null, msgs);
      msgs = basicSetSubj(newSubj, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.TRIPLE__SUBJ, newSubj, newSubj));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName getPred()
  {
    if (pred != null && pred.eIsProxy())
    {
      InternalEObject oldPred = (InternalEObject)pred;
      pred = (ResourceName)eResolveProxy(oldPred);
      if (pred != oldPred)
      {
        if (eNotificationRequired())
          eNotify(new ENotificationImpl(this, Notification.RESOLVE, MappingPackage.TRIPLE__PRED, oldPred, pred));
      }
    }
    return pred;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName basicGetPred()
  {
    return pred;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPred(ResourceName newPred)
  {
    ResourceName oldPred = pred;
    pred = newPred;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.TRIPLE__PRED, oldPred, pred));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EObject getObjval()
  {
    return objval;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetObjval(EObject newObjval, NotificationChain msgs)
  {
    EObject oldObjval = objval;
    objval = newObjval;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, MappingPackage.TRIPLE__OBJVAL, oldObjval, newObjval);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setObjval(EObject newObjval)
  {
    if (newObjval != objval)
    {
      NotificationChain msgs = null;
      if (objval != null)
        msgs = ((InternalEObject)objval).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - MappingPackage.TRIPLE__OBJVAL, null, msgs);
      if (newObjval != null)
        msgs = ((InternalEObject)newObjval).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - MappingPackage.TRIPLE__OBJVAL, null, msgs);
      msgs = basicSetObjval(newObjval, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.TRIPLE__OBJVAL, newObjval, newObjval));
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
      case MappingPackage.TRIPLE__SUBJ:
        return basicSetSubj(null, msgs);
      case MappingPackage.TRIPLE__OBJVAL:
        return basicSetObjval(null, msgs);
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
      case MappingPackage.TRIPLE__SUBJ:
        return getSubj();
      case MappingPackage.TRIPLE__PRED:
        if (resolve) return getPred();
        return basicGetPred();
      case MappingPackage.TRIPLE__OBJVAL:
        return getObjval();
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
      case MappingPackage.TRIPLE__SUBJ:
        setSubj((EObject)newValue);
        return;
      case MappingPackage.TRIPLE__PRED:
        setPred((ResourceName)newValue);
        return;
      case MappingPackage.TRIPLE__OBJVAL:
        setObjval((EObject)newValue);
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
      case MappingPackage.TRIPLE__SUBJ:
        setSubj((EObject)null);
        return;
      case MappingPackage.TRIPLE__PRED:
        setPred((ResourceName)null);
        return;
      case MappingPackage.TRIPLE__OBJVAL:
        setObjval((EObject)null);
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
      case MappingPackage.TRIPLE__SUBJ:
        return subj != null;
      case MappingPackage.TRIPLE__PRED:
        return pred != null;
      case MappingPackage.TRIPLE__OBJVAL:
        return objval != null;
    }
    return super.eIsSet(featureID);
  }

} //TripleImpl
