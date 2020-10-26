/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.OfPhrase;
import com.ge.research.sadl.sadl.PropOfSubj;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.SadlPackage;

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
 * An implementation of the model object '<em><b>Prop Of Subj</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropOfSubjImpl#getOfPhr <em>Of Phr</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropOfSubjImpl#getSubj <em>Subj</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class PropOfSubjImpl extends GraphPatternImpl implements PropOfSubj
{
  /**
   * The cached value of the '{@link #getOfPhr() <em>Of Phr</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getOfPhr()
   * @generated
   * @ordered
   */
  protected EList<OfPhrase> ofPhr;

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
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected PropOfSubjImpl()
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
    return SadlPackage.Literals.PROP_OF_SUBJ;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<OfPhrase> getOfPhr()
  {
    if (ofPhr == null)
    {
      ofPhr = new EObjectContainmentEList<OfPhrase>(OfPhrase.class, this, SadlPackage.PROP_OF_SUBJ__OF_PHR);
    }
    return ofPhr;
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
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.PROP_OF_SUBJ__SUBJ, oldSubj, newSubj);
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
        msgs = ((InternalEObject)subj).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROP_OF_SUBJ__SUBJ, null, msgs);
      if (newSubj != null)
        msgs = ((InternalEObject)newSubj).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROP_OF_SUBJ__SUBJ, null, msgs);
      msgs = basicSetSubj(newSubj, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.PROP_OF_SUBJ__SUBJ, newSubj, newSubj));
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
      case SadlPackage.PROP_OF_SUBJ__OF_PHR:
        return ((InternalEList<?>)getOfPhr()).basicRemove(otherEnd, msgs);
      case SadlPackage.PROP_OF_SUBJ__SUBJ:
        return basicSetSubj(null, msgs);
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
      case SadlPackage.PROP_OF_SUBJ__OF_PHR:
        return getOfPhr();
      case SadlPackage.PROP_OF_SUBJ__SUBJ:
        return getSubj();
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
      case SadlPackage.PROP_OF_SUBJ__OF_PHR:
        getOfPhr().clear();
        getOfPhr().addAll((Collection<? extends OfPhrase>)newValue);
        return;
      case SadlPackage.PROP_OF_SUBJ__SUBJ:
        setSubj((ResourceByName)newValue);
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
      case SadlPackage.PROP_OF_SUBJ__OF_PHR:
        getOfPhr().clear();
        return;
      case SadlPackage.PROP_OF_SUBJ__SUBJ:
        setSubj((ResourceByName)null);
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
      case SadlPackage.PROP_OF_SUBJ__OF_PHR:
        return ofPhr != null && !ofPhr.isEmpty();
      case SadlPackage.PROP_OF_SUBJ__SUBJ:
        return subj != null;
    }
    return super.eIsSet(featureID);
  }

} //PropOfSubjImpl
