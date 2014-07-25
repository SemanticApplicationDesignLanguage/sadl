/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ExistingInstanceAttribution;
import com.ge.research.sadl.sadl.OfPatternReturningValues;
import com.ge.research.sadl.sadl.PropValPartialTriple;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.SadlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Existing Instance Attribution</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExistingInstanceAttributionImpl#getSubj <em>Subj</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExistingInstanceAttributionImpl#getAddlInfoItems <em>Addl Info Items</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExistingInstanceAttributionImpl#getPOfS <em>POf S</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExistingInstanceAttributionImpl#getObj <em>Obj</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ExistingInstanceAttributionImpl extends StatementImpl implements ExistingInstanceAttribution
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
   * The cached value of the '{@link #getAddlInfoItems() <em>Addl Info Items</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAddlInfoItems()
   * @generated
   * @ordered
   */
  protected EList<PropValPartialTriple> addlInfoItems;

  /**
   * The cached value of the '{@link #getPOfS() <em>POf S</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPOfS()
   * @generated
   * @ordered
   */
  protected OfPatternReturningValues pOfS;

  /**
   * The cached value of the '{@link #getObj() <em>Obj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getObj()
   * @generated
   * @ordered
   */
  protected EObject obj;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ExistingInstanceAttributionImpl()
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
    return SadlPackage.Literals.EXISTING_INSTANCE_ATTRIBUTION;
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
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__SUBJ, oldSubj, newSubj);
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
        msgs = ((InternalEObject)subj).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__SUBJ, null, msgs);
      if (newSubj != null)
        msgs = ((InternalEObject)newSubj).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__SUBJ, null, msgs);
      msgs = basicSetSubj(newSubj, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__SUBJ, newSubj, newSubj));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<PropValPartialTriple> getAddlInfoItems()
  {
    if (addlInfoItems == null)
    {
      addlInfoItems = new EObjectContainmentEList<PropValPartialTriple>(PropValPartialTriple.class, this, SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__ADDL_INFO_ITEMS);
    }
    return addlInfoItems;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public OfPatternReturningValues getPOfS()
  {
    return pOfS;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPOfS(OfPatternReturningValues newPOfS, NotificationChain msgs)
  {
    OfPatternReturningValues oldPOfS = pOfS;
    pOfS = newPOfS;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__POF_S, oldPOfS, newPOfS);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPOfS(OfPatternReturningValues newPOfS)
  {
    if (newPOfS != pOfS)
    {
      NotificationChain msgs = null;
      if (pOfS != null)
        msgs = ((InternalEObject)pOfS).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__POF_S, null, msgs);
      if (newPOfS != null)
        msgs = ((InternalEObject)newPOfS).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__POF_S, null, msgs);
      msgs = basicSetPOfS(newPOfS, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__POF_S, newPOfS, newPOfS));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EObject getObj()
  {
    return obj;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetObj(EObject newObj, NotificationChain msgs)
  {
    EObject oldObj = obj;
    obj = newObj;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__OBJ, oldObj, newObj);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setObj(EObject newObj)
  {
    if (newObj != obj)
    {
      NotificationChain msgs = null;
      if (obj != null)
        msgs = ((InternalEObject)obj).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__OBJ, null, msgs);
      if (newObj != null)
        msgs = ((InternalEObject)newObj).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__OBJ, null, msgs);
      msgs = basicSetObj(newObj, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__OBJ, newObj, newObj));
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
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__SUBJ:
        return basicSetSubj(null, msgs);
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__ADDL_INFO_ITEMS:
        return ((InternalEList<?>)getAddlInfoItems()).basicRemove(otherEnd, msgs);
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__POF_S:
        return basicSetPOfS(null, msgs);
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__OBJ:
        return basicSetObj(null, msgs);
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
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__SUBJ:
        return getSubj();
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__ADDL_INFO_ITEMS:
        return getAddlInfoItems();
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__POF_S:
        return getPOfS();
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__OBJ:
        return getObj();
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
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__SUBJ:
        setSubj((ResourceByName)newValue);
        return;
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__ADDL_INFO_ITEMS:
        getAddlInfoItems().clear();
        getAddlInfoItems().addAll((Collection<? extends PropValPartialTriple>)newValue);
        return;
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__POF_S:
        setPOfS((OfPatternReturningValues)newValue);
        return;
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__OBJ:
        setObj((EObject)newValue);
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
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__SUBJ:
        setSubj((ResourceByName)null);
        return;
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__ADDL_INFO_ITEMS:
        getAddlInfoItems().clear();
        return;
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__POF_S:
        setPOfS((OfPatternReturningValues)null);
        return;
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__OBJ:
        setObj((EObject)null);
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
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__SUBJ:
        return subj != null;
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__ADDL_INFO_ITEMS:
        return addlInfoItems != null && !addlInfoItems.isEmpty();
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__POF_S:
        return pOfS != null;
      case SadlPackage.EXISTING_INSTANCE_ATTRIBUTION__OBJ:
        return obj != null;
    }
    return super.eIsSet(featureID);
  }

} //ExistingInstanceAttributionImpl
