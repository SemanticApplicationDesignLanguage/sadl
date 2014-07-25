/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ConstructExpression;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Construct Expression</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ConstructExpressionImpl#getSubj <em>Subj</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ConstructExpressionImpl#getPred <em>Pred</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ConstructExpressionImpl#getObj <em>Obj</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ConstructExpressionImpl extends ExpressionImpl implements ConstructExpression
{
  /**
   * The cached value of the '{@link #getSubj() <em>Subj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getSubj()
   * @generated
   * @ordered
   */
  protected ResourceName subj;

  /**
   * The cached value of the '{@link #getPred() <em>Pred</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPred()
   * @generated
   * @ordered
   */
  protected ResourceName pred;

  /**
   * The cached value of the '{@link #getObj() <em>Obj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getObj()
   * @generated
   * @ordered
   */
  protected ResourceName obj;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ConstructExpressionImpl()
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
    return SadlPackage.Literals.CONSTRUCT_EXPRESSION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName getSubj()
  {
    return subj;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetSubj(ResourceName newSubj, NotificationChain msgs)
  {
    ResourceName oldSubj = subj;
    subj = newSubj;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.CONSTRUCT_EXPRESSION__SUBJ, oldSubj, newSubj);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setSubj(ResourceName newSubj)
  {
    if (newSubj != subj)
    {
      NotificationChain msgs = null;
      if (subj != null)
        msgs = ((InternalEObject)subj).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CONSTRUCT_EXPRESSION__SUBJ, null, msgs);
      if (newSubj != null)
        msgs = ((InternalEObject)newSubj).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CONSTRUCT_EXPRESSION__SUBJ, null, msgs);
      msgs = basicSetSubj(newSubj, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.CONSTRUCT_EXPRESSION__SUBJ, newSubj, newSubj));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName getPred()
  {
    return pred;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPred(ResourceName newPred, NotificationChain msgs)
  {
    ResourceName oldPred = pred;
    pred = newPred;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.CONSTRUCT_EXPRESSION__PRED, oldPred, newPred);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPred(ResourceName newPred)
  {
    if (newPred != pred)
    {
      NotificationChain msgs = null;
      if (pred != null)
        msgs = ((InternalEObject)pred).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CONSTRUCT_EXPRESSION__PRED, null, msgs);
      if (newPred != null)
        msgs = ((InternalEObject)newPred).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CONSTRUCT_EXPRESSION__PRED, null, msgs);
      msgs = basicSetPred(newPred, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.CONSTRUCT_EXPRESSION__PRED, newPred, newPred));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName getObj()
  {
    return obj;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetObj(ResourceName newObj, NotificationChain msgs)
  {
    ResourceName oldObj = obj;
    obj = newObj;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.CONSTRUCT_EXPRESSION__OBJ, oldObj, newObj);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setObj(ResourceName newObj)
  {
    if (newObj != obj)
    {
      NotificationChain msgs = null;
      if (obj != null)
        msgs = ((InternalEObject)obj).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CONSTRUCT_EXPRESSION__OBJ, null, msgs);
      if (newObj != null)
        msgs = ((InternalEObject)newObj).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CONSTRUCT_EXPRESSION__OBJ, null, msgs);
      msgs = basicSetObj(newObj, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.CONSTRUCT_EXPRESSION__OBJ, newObj, newObj));
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
      case SadlPackage.CONSTRUCT_EXPRESSION__SUBJ:
        return basicSetSubj(null, msgs);
      case SadlPackage.CONSTRUCT_EXPRESSION__PRED:
        return basicSetPred(null, msgs);
      case SadlPackage.CONSTRUCT_EXPRESSION__OBJ:
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
      case SadlPackage.CONSTRUCT_EXPRESSION__SUBJ:
        return getSubj();
      case SadlPackage.CONSTRUCT_EXPRESSION__PRED:
        return getPred();
      case SadlPackage.CONSTRUCT_EXPRESSION__OBJ:
        return getObj();
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
      case SadlPackage.CONSTRUCT_EXPRESSION__SUBJ:
        setSubj((ResourceName)newValue);
        return;
      case SadlPackage.CONSTRUCT_EXPRESSION__PRED:
        setPred((ResourceName)newValue);
        return;
      case SadlPackage.CONSTRUCT_EXPRESSION__OBJ:
        setObj((ResourceName)newValue);
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
      case SadlPackage.CONSTRUCT_EXPRESSION__SUBJ:
        setSubj((ResourceName)null);
        return;
      case SadlPackage.CONSTRUCT_EXPRESSION__PRED:
        setPred((ResourceName)null);
        return;
      case SadlPackage.CONSTRUCT_EXPRESSION__OBJ:
        setObj((ResourceName)null);
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
      case SadlPackage.CONSTRUCT_EXPRESSION__SUBJ:
        return subj != null;
      case SadlPackage.CONSTRUCT_EXPRESSION__PRED:
        return pred != null;
      case SadlPackage.CONSTRUCT_EXPRESSION__OBJ:
        return obj != null;
    }
    return super.eIsSet(featureID);
  }

} //ConstructExpressionImpl
