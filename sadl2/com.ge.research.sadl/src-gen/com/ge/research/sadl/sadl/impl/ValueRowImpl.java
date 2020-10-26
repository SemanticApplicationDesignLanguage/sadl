/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ExplicitValue;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.ValueRow;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Value Row</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ValueRowImpl#getExplicitValues <em>Explicit Values</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ValueRowImpl extends MinimalEObjectImpl.Container implements ValueRow
{
  /**
   * The cached value of the '{@link #getExplicitValues() <em>Explicit Values</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getExplicitValues()
   * @generated
   * @ordered
   */
  protected EList<ExplicitValue> explicitValues;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ValueRowImpl()
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
    return SadlPackage.Literals.VALUE_ROW;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<ExplicitValue> getExplicitValues()
  {
    if (explicitValues == null)
    {
      explicitValues = new EObjectContainmentEList<ExplicitValue>(ExplicitValue.class, this, SadlPackage.VALUE_ROW__EXPLICIT_VALUES);
    }
    return explicitValues;
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
      case SadlPackage.VALUE_ROW__EXPLICIT_VALUES:
        return ((InternalEList<?>)getExplicitValues()).basicRemove(otherEnd, msgs);
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
      case SadlPackage.VALUE_ROW__EXPLICIT_VALUES:
        return getExplicitValues();
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
      case SadlPackage.VALUE_ROW__EXPLICIT_VALUES:
        getExplicitValues().clear();
        getExplicitValues().addAll((Collection<? extends ExplicitValue>)newValue);
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
      case SadlPackage.VALUE_ROW__EXPLICIT_VALUES:
        getExplicitValues().clear();
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
      case SadlPackage.VALUE_ROW__EXPLICIT_VALUES:
        return explicitValues != null && !explicitValues.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //ValueRowImpl
