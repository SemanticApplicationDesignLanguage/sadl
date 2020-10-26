/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.OfPatternReturningValues;
import com.ge.research.sadl.sadl.OfPhrase;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.TypedBNode;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Of Pattern Returning Values</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.OfPatternReturningValuesImpl#getOfphrs <em>Ofphrs</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.OfPatternReturningValuesImpl#getSubject <em>Subject</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.OfPatternReturningValuesImpl#getType <em>Type</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class OfPatternReturningValuesImpl extends MinimalEObjectImpl.Container implements OfPatternReturningValues
{
  /**
   * The cached value of the '{@link #getOfphrs() <em>Ofphrs</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getOfphrs()
   * @generated
   * @ordered
   */
  protected EList<OfPhrase> ofphrs;

  /**
   * The cached value of the '{@link #getSubject() <em>Subject</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getSubject()
   * @generated
   * @ordered
   */
  protected ResourceByName subject;

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
  protected OfPatternReturningValuesImpl()
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
    return SadlPackage.Literals.OF_PATTERN_RETURNING_VALUES;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<OfPhrase> getOfphrs()
  {
    if (ofphrs == null)
    {
      ofphrs = new EObjectContainmentEList<OfPhrase>(OfPhrase.class, this, SadlPackage.OF_PATTERN_RETURNING_VALUES__OFPHRS);
    }
    return ofphrs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getSubject()
  {
    return subject;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetSubject(ResourceByName newSubject, NotificationChain msgs)
  {
    ResourceByName oldSubject = subject;
    subject = newSubject;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.OF_PATTERN_RETURNING_VALUES__SUBJECT, oldSubject, newSubject);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setSubject(ResourceByName newSubject)
  {
    if (newSubject != subject)
    {
      NotificationChain msgs = null;
      if (subject != null)
        msgs = ((InternalEObject)subject).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.OF_PATTERN_RETURNING_VALUES__SUBJECT, null, msgs);
      if (newSubject != null)
        msgs = ((InternalEObject)newSubject).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.OF_PATTERN_RETURNING_VALUES__SUBJECT, null, msgs);
      msgs = basicSetSubject(newSubject, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.OF_PATTERN_RETURNING_VALUES__SUBJECT, newSubject, newSubject));
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
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.OF_PATTERN_RETURNING_VALUES__TYPE, oldType, newType);
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
        msgs = ((InternalEObject)type).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.OF_PATTERN_RETURNING_VALUES__TYPE, null, msgs);
      if (newType != null)
        msgs = ((InternalEObject)newType).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.OF_PATTERN_RETURNING_VALUES__TYPE, null, msgs);
      msgs = basicSetType(newType, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.OF_PATTERN_RETURNING_VALUES__TYPE, newType, newType));
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
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__OFPHRS:
        return ((InternalEList<?>)getOfphrs()).basicRemove(otherEnd, msgs);
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__SUBJECT:
        return basicSetSubject(null, msgs);
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__TYPE:
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
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__OFPHRS:
        return getOfphrs();
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__SUBJECT:
        return getSubject();
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__TYPE:
        return getType();
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
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__OFPHRS:
        getOfphrs().clear();
        getOfphrs().addAll((Collection<? extends OfPhrase>)newValue);
        return;
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__SUBJECT:
        setSubject((ResourceByName)newValue);
        return;
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__TYPE:
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
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__OFPHRS:
        getOfphrs().clear();
        return;
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__SUBJECT:
        setSubject((ResourceByName)null);
        return;
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__TYPE:
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
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__OFPHRS:
        return ofphrs != null && !ofphrs.isEmpty();
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__SUBJECT:
        return subject != null;
      case SadlPackage.OF_PATTERN_RETURNING_VALUES__TYPE:
        return type != null;
    }
    return super.eIsSet(featureID);
  }

} //OfPatternReturningValuesImpl
