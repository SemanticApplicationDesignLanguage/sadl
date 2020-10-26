/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ExplicitValue;
import com.ge.research.sadl.sadl.InstAttrPSV;
import com.ge.research.sadl.sadl.PropOfSubj;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Inst Attr PSV</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstAttrPSVImpl#getProp <em>Prop</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstAttrPSVImpl#getVal <em>Val</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class InstAttrPSVImpl extends GraphPatternImpl implements InstAttrPSV
{
  /**
   * The cached value of the '{@link #getProp() <em>Prop</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getProp()
   * @generated
   * @ordered
   */
  protected PropOfSubj prop;

  /**
   * The cached value of the '{@link #getVal() <em>Val</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getVal()
   * @generated
   * @ordered
   */
  protected ExplicitValue val;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected InstAttrPSVImpl()
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
    return SadlPackage.Literals.INST_ATTR_PSV;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public PropOfSubj getProp()
  {
    return prop;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetProp(PropOfSubj newProp, NotificationChain msgs)
  {
    PropOfSubj oldProp = prop;
    prop = newProp;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.INST_ATTR_PSV__PROP, oldProp, newProp);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setProp(PropOfSubj newProp)
  {
    if (newProp != prop)
    {
      NotificationChain msgs = null;
      if (prop != null)
        msgs = ((InternalEObject)prop).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INST_ATTR_PSV__PROP, null, msgs);
      if (newProp != null)
        msgs = ((InternalEObject)newProp).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INST_ATTR_PSV__PROP, null, msgs);
      msgs = basicSetProp(newProp, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INST_ATTR_PSV__PROP, newProp, newProp));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExplicitValue getVal()
  {
    return val;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetVal(ExplicitValue newVal, NotificationChain msgs)
  {
    ExplicitValue oldVal = val;
    val = newVal;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.INST_ATTR_PSV__VAL, oldVal, newVal);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setVal(ExplicitValue newVal)
  {
    if (newVal != val)
    {
      NotificationChain msgs = null;
      if (val != null)
        msgs = ((InternalEObject)val).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INST_ATTR_PSV__VAL, null, msgs);
      if (newVal != null)
        msgs = ((InternalEObject)newVal).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INST_ATTR_PSV__VAL, null, msgs);
      msgs = basicSetVal(newVal, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INST_ATTR_PSV__VAL, newVal, newVal));
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
      case SadlPackage.INST_ATTR_PSV__PROP:
        return basicSetProp(null, msgs);
      case SadlPackage.INST_ATTR_PSV__VAL:
        return basicSetVal(null, msgs);
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
      case SadlPackage.INST_ATTR_PSV__PROP:
        return getProp();
      case SadlPackage.INST_ATTR_PSV__VAL:
        return getVal();
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
      case SadlPackage.INST_ATTR_PSV__PROP:
        setProp((PropOfSubj)newValue);
        return;
      case SadlPackage.INST_ATTR_PSV__VAL:
        setVal((ExplicitValue)newValue);
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
      case SadlPackage.INST_ATTR_PSV__PROP:
        setProp((PropOfSubj)null);
        return;
      case SadlPackage.INST_ATTR_PSV__VAL:
        setVal((ExplicitValue)null);
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
      case SadlPackage.INST_ATTR_PSV__PROP:
        return prop != null;
      case SadlPackage.INST_ATTR_PSV__VAL:
        return val != null;
    }
    return super.eIsSet(featureID);
  }

} //InstAttrPSVImpl
