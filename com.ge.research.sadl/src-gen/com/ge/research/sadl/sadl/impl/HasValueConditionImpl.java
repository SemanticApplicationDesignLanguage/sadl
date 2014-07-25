/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ExplicitValue;
import com.ge.research.sadl.sadl.HasValueCondition;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Has Value Condition</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.HasValueConditionImpl#getRestriction <em>Restriction</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class HasValueConditionImpl extends ConditionImpl implements HasValueCondition
{
  /**
   * The cached value of the '{@link #getRestriction() <em>Restriction</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRestriction()
   * @generated
   * @ordered
   */
  protected ExplicitValue restriction;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected HasValueConditionImpl()
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
    return SadlPackage.Literals.HAS_VALUE_CONDITION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExplicitValue getRestriction()
  {
    return restriction;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetRestriction(ExplicitValue newRestriction, NotificationChain msgs)
  {
    ExplicitValue oldRestriction = restriction;
    restriction = newRestriction;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.HAS_VALUE_CONDITION__RESTRICTION, oldRestriction, newRestriction);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setRestriction(ExplicitValue newRestriction)
  {
    if (newRestriction != restriction)
    {
      NotificationChain msgs = null;
      if (restriction != null)
        msgs = ((InternalEObject)restriction).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.HAS_VALUE_CONDITION__RESTRICTION, null, msgs);
      if (newRestriction != null)
        msgs = ((InternalEObject)newRestriction).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.HAS_VALUE_CONDITION__RESTRICTION, null, msgs);
      msgs = basicSetRestriction(newRestriction, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.HAS_VALUE_CONDITION__RESTRICTION, newRestriction, newRestriction));
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
      case SadlPackage.HAS_VALUE_CONDITION__RESTRICTION:
        return basicSetRestriction(null, msgs);
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
      case SadlPackage.HAS_VALUE_CONDITION__RESTRICTION:
        return getRestriction();
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
      case SadlPackage.HAS_VALUE_CONDITION__RESTRICTION:
        setRestriction((ExplicitValue)newValue);
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
      case SadlPackage.HAS_VALUE_CONDITION__RESTRICTION:
        setRestriction((ExplicitValue)null);
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
      case SadlPackage.HAS_VALUE_CONDITION__RESTRICTION:
        return restriction != null;
    }
    return super.eIsSet(featureID);
  }

} //HasValueConditionImpl
