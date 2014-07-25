/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.SubjProp;
import com.ge.research.sadl.sadl.WithPhrase;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Subj Prop</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.SubjPropImpl#getSubj <em>Subj</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.SubjPropImpl#getHwPhr <em>Hw Phr</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SubjPropImpl extends GraphPatternImpl implements SubjProp
{
  /**
   * The cached value of the '{@link #getSubj() <em>Subj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getSubj()
   * @generated
   * @ordered
   */
  protected ResourceByName subj;

  /**
   * The cached value of the '{@link #getHwPhr() <em>Hw Phr</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getHwPhr()
   * @generated
   * @ordered
   */
  protected EList<WithPhrase> hwPhr;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected SubjPropImpl()
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
    return SadlPackage.Literals.SUBJ_PROP;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getSubj()
  {
    return subj;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetSubj(ResourceByName newSubj, NotificationChain msgs)
  {
    ResourceByName oldSubj = subj;
    subj = newSubj;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.SUBJ_PROP__SUBJ, oldSubj, newSubj);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setSubj(ResourceByName newSubj)
  {
    if (newSubj != subj)
    {
      NotificationChain msgs = null;
      if (subj != null)
        msgs = ((InternalEObject)subj).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.SUBJ_PROP__SUBJ, null, msgs);
      if (newSubj != null)
        msgs = ((InternalEObject)newSubj).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.SUBJ_PROP__SUBJ, null, msgs);
      msgs = basicSetSubj(newSubj, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.SUBJ_PROP__SUBJ, newSubj, newSubj));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<WithPhrase> getHwPhr()
  {
    if (hwPhr == null)
    {
      hwPhr = new EObjectContainmentEList<WithPhrase>(WithPhrase.class, this, SadlPackage.SUBJ_PROP__HW_PHR);
    }
    return hwPhr;
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
      case SadlPackage.SUBJ_PROP__SUBJ:
        return basicSetSubj(null, msgs);
      case SadlPackage.SUBJ_PROP__HW_PHR:
        return ((InternalEList<?>)getHwPhr()).basicRemove(otherEnd, msgs);
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
      case SadlPackage.SUBJ_PROP__SUBJ:
        return getSubj();
      case SadlPackage.SUBJ_PROP__HW_PHR:
        return getHwPhr();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SadlPackage.SUBJ_PROP__SUBJ:
        setSubj((ResourceByName)newValue);
        return;
      case SadlPackage.SUBJ_PROP__HW_PHR:
        getHwPhr().clear();
        getHwPhr().addAll((Collection<? extends WithPhrase>)newValue);
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
      case SadlPackage.SUBJ_PROP__SUBJ:
        setSubj((ResourceByName)null);
        return;
      case SadlPackage.SUBJ_PROP__HW_PHR:
        getHwPhr().clear();
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
      case SadlPackage.SUBJ_PROP__SUBJ:
        return subj != null;
      case SadlPackage.SUBJ_PROP__HW_PHR:
        return hwPhr != null && !hwPhr.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //SubjPropImpl
