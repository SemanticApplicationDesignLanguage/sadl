/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.Expression;
import com.ge.research.sadl.sadl.InstAttrSPV;
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
 * An implementation of the model object '<em><b>Inst Attr SPV</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstAttrSPVImpl#getSubj <em>Subj</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstAttrSPVImpl#getProps <em>Props</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstAttrSPVImpl#getVals <em>Vals</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class InstAttrSPVImpl extends GraphPatternImpl implements InstAttrSPV
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
   * The cached value of the '{@link #getProps() <em>Props</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getProps()
   * @generated
   * @ordered
   */
  protected EList<ResourceByName> props;

  /**
   * The cached value of the '{@link #getVals() <em>Vals</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getVals()
   * @generated
   * @ordered
   */
  protected EList<Expression> vals;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected InstAttrSPVImpl()
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
    return SadlPackage.Literals.INST_ATTR_SPV;
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
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.INST_ATTR_SPV__SUBJ, oldSubj, newSubj);
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
        msgs = ((InternalEObject)subj).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INST_ATTR_SPV__SUBJ, null, msgs);
      if (newSubj != null)
        msgs = ((InternalEObject)newSubj).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INST_ATTR_SPV__SUBJ, null, msgs);
      msgs = basicSetSubj(newSubj, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INST_ATTR_SPV__SUBJ, newSubj, newSubj));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<ResourceByName> getProps()
  {
    if (props == null)
    {
      props = new EObjectContainmentEList<ResourceByName>(ResourceByName.class, this, SadlPackage.INST_ATTR_SPV__PROPS);
    }
    return props;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<Expression> getVals()
  {
    if (vals == null)
    {
      vals = new EObjectContainmentEList<Expression>(Expression.class, this, SadlPackage.INST_ATTR_SPV__VALS);
    }
    return vals;
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
      case SadlPackage.INST_ATTR_SPV__SUBJ:
        return basicSetSubj(null, msgs);
      case SadlPackage.INST_ATTR_SPV__PROPS:
        return ((InternalEList<?>)getProps()).basicRemove(otherEnd, msgs);
      case SadlPackage.INST_ATTR_SPV__VALS:
        return ((InternalEList<?>)getVals()).basicRemove(otherEnd, msgs);
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
      case SadlPackage.INST_ATTR_SPV__SUBJ:
        return getSubj();
      case SadlPackage.INST_ATTR_SPV__PROPS:
        return getProps();
      case SadlPackage.INST_ATTR_SPV__VALS:
        return getVals();
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
      case SadlPackage.INST_ATTR_SPV__SUBJ:
        setSubj((ResourceByName)newValue);
        return;
      case SadlPackage.INST_ATTR_SPV__PROPS:
        getProps().clear();
        getProps().addAll((Collection<? extends ResourceByName>)newValue);
        return;
      case SadlPackage.INST_ATTR_SPV__VALS:
        getVals().clear();
        getVals().addAll((Collection<? extends Expression>)newValue);
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
      case SadlPackage.INST_ATTR_SPV__SUBJ:
        setSubj((ResourceByName)null);
        return;
      case SadlPackage.INST_ATTR_SPV__PROPS:
        getProps().clear();
        return;
      case SadlPackage.INST_ATTR_SPV__VALS:
        getVals().clear();
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
      case SadlPackage.INST_ATTR_SPV__SUBJ:
        return subj != null;
      case SadlPackage.INST_ATTR_SPV__PROPS:
        return props != null && !props.isEmpty();
      case SadlPackage.INST_ATTR_SPV__VALS:
        return vals != null && !vals.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //InstAttrSPVImpl
