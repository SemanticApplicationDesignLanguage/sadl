/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.MergedTriples;
import com.ge.research.sadl.sadl.OfPhrase;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.TypedBNode;
import com.ge.research.sadl.sadl.WithChain;

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
 * An implementation of the model object '<em><b>Merged Triples</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.MergedTriplesImpl#getOps <em>Ops</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.MergedTriplesImpl#getPivot <em>Pivot</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.MergedTriplesImpl#getWcs <em>Wcs</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class MergedTriplesImpl extends GraphPatternImpl implements MergedTriples
{
  /**
   * The cached value of the '{@link #getOps() <em>Ops</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getOps()
   * @generated
   * @ordered
   */
  protected EList<OfPhrase> ops;

  /**
   * The cached value of the '{@link #getPivot() <em>Pivot</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPivot()
   * @generated
   * @ordered
   */
  protected TypedBNode pivot;

  /**
   * The cached value of the '{@link #getWcs() <em>Wcs</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getWcs()
   * @generated
   * @ordered
   */
  protected EList<WithChain> wcs;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected MergedTriplesImpl()
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
    return SadlPackage.Literals.MERGED_TRIPLES;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<OfPhrase> getOps()
  {
    if (ops == null)
    {
      ops = new EObjectContainmentEList<OfPhrase>(OfPhrase.class, this, SadlPackage.MERGED_TRIPLES__OPS);
    }
    return ops;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public TypedBNode getPivot()
  {
    return pivot;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPivot(TypedBNode newPivot, NotificationChain msgs)
  {
    TypedBNode oldPivot = pivot;
    pivot = newPivot;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.MERGED_TRIPLES__PIVOT, oldPivot, newPivot);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPivot(TypedBNode newPivot)
  {
    if (newPivot != pivot)
    {
      NotificationChain msgs = null;
      if (pivot != null)
        msgs = ((InternalEObject)pivot).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.MERGED_TRIPLES__PIVOT, null, msgs);
      if (newPivot != null)
        msgs = ((InternalEObject)newPivot).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.MERGED_TRIPLES__PIVOT, null, msgs);
      msgs = basicSetPivot(newPivot, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.MERGED_TRIPLES__PIVOT, newPivot, newPivot));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<WithChain> getWcs()
  {
    if (wcs == null)
    {
      wcs = new EObjectContainmentEList<WithChain>(WithChain.class, this, SadlPackage.MERGED_TRIPLES__WCS);
    }
    return wcs;
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
      case SadlPackage.MERGED_TRIPLES__OPS:
        return ((InternalEList<?>)getOps()).basicRemove(otherEnd, msgs);
      case SadlPackage.MERGED_TRIPLES__PIVOT:
        return basicSetPivot(null, msgs);
      case SadlPackage.MERGED_TRIPLES__WCS:
        return ((InternalEList<?>)getWcs()).basicRemove(otherEnd, msgs);
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
      case SadlPackage.MERGED_TRIPLES__OPS:
        return getOps();
      case SadlPackage.MERGED_TRIPLES__PIVOT:
        return getPivot();
      case SadlPackage.MERGED_TRIPLES__WCS:
        return getWcs();
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
      case SadlPackage.MERGED_TRIPLES__OPS:
        getOps().clear();
        getOps().addAll((Collection<? extends OfPhrase>)newValue);
        return;
      case SadlPackage.MERGED_TRIPLES__PIVOT:
        setPivot((TypedBNode)newValue);
        return;
      case SadlPackage.MERGED_TRIPLES__WCS:
        getWcs().clear();
        getWcs().addAll((Collection<? extends WithChain>)newValue);
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
      case SadlPackage.MERGED_TRIPLES__OPS:
        getOps().clear();
        return;
      case SadlPackage.MERGED_TRIPLES__PIVOT:
        setPivot((TypedBNode)null);
        return;
      case SadlPackage.MERGED_TRIPLES__WCS:
        getWcs().clear();
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
      case SadlPackage.MERGED_TRIPLES__OPS:
        return ops != null && !ops.isEmpty();
      case SadlPackage.MERGED_TRIPLES__PIVOT:
        return pivot != null;
      case SadlPackage.MERGED_TRIPLES__WCS:
        return wcs != null && !wcs.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //MergedTriplesImpl
