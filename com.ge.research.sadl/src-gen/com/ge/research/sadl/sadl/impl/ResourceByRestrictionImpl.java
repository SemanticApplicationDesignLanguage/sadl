/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.Condition;
import com.ge.research.sadl.sadl.ContentList;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceByRestriction;
import com.ge.research.sadl.sadl.SadlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EDataTypeEList;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Resource By Restriction</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ResourceByRestrictionImpl#getAnnType <em>Ann Type</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ResourceByRestrictionImpl#getAnnContent <em>Ann Content</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ResourceByRestrictionImpl#getPropName <em>Prop Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ResourceByRestrictionImpl#getCond <em>Cond</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ResourceByRestrictionImpl extends ResourceIdentifierImpl implements ResourceByRestriction
{
  /**
   * The cached value of the '{@link #getAnnType() <em>Ann Type</em>}' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnType()
   * @generated
   * @ordered
   */
  protected EList<String> annType;

  /**
   * The cached value of the '{@link #getAnnContent() <em>Ann Content</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnContent()
   * @generated
   * @ordered
   */
  protected EList<ContentList> annContent;

  /**
   * The cached value of the '{@link #getPropName() <em>Prop Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPropName()
   * @generated
   * @ordered
   */
  protected ResourceByName propName;

  /**
   * The cached value of the '{@link #getCond() <em>Cond</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getCond()
   * @generated
   * @ordered
   */
  protected Condition cond;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ResourceByRestrictionImpl()
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
    return SadlPackage.Literals.RESOURCE_BY_RESTRICTION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<String> getAnnType()
  {
    if (annType == null)
    {
      annType = new EDataTypeEList<String>(String.class, this, SadlPackage.RESOURCE_BY_RESTRICTION__ANN_TYPE);
    }
    return annType;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<ContentList> getAnnContent()
  {
    if (annContent == null)
    {
      annContent = new EObjectContainmentEList<ContentList>(ContentList.class, this, SadlPackage.RESOURCE_BY_RESTRICTION__ANN_CONTENT);
    }
    return annContent;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getPropName()
  {
    return propName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPropName(ResourceByName newPropName, NotificationChain msgs)
  {
    ResourceByName oldPropName = propName;
    propName = newPropName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.RESOURCE_BY_RESTRICTION__PROP_NAME, oldPropName, newPropName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPropName(ResourceByName newPropName)
  {
    if (newPropName != propName)
    {
      NotificationChain msgs = null;
      if (propName != null)
        msgs = ((InternalEObject)propName).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RESOURCE_BY_RESTRICTION__PROP_NAME, null, msgs);
      if (newPropName != null)
        msgs = ((InternalEObject)newPropName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RESOURCE_BY_RESTRICTION__PROP_NAME, null, msgs);
      msgs = basicSetPropName(newPropName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RESOURCE_BY_RESTRICTION__PROP_NAME, newPropName, newPropName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Condition getCond()
  {
    return cond;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetCond(Condition newCond, NotificationChain msgs)
  {
    Condition oldCond = cond;
    cond = newCond;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.RESOURCE_BY_RESTRICTION__COND, oldCond, newCond);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setCond(Condition newCond)
  {
    if (newCond != cond)
    {
      NotificationChain msgs = null;
      if (cond != null)
        msgs = ((InternalEObject)cond).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RESOURCE_BY_RESTRICTION__COND, null, msgs);
      if (newCond != null)
        msgs = ((InternalEObject)newCond).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RESOURCE_BY_RESTRICTION__COND, null, msgs);
      msgs = basicSetCond(newCond, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RESOURCE_BY_RESTRICTION__COND, newCond, newCond));
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
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_CONTENT:
        return ((InternalEList<?>)getAnnContent()).basicRemove(otherEnd, msgs);
      case SadlPackage.RESOURCE_BY_RESTRICTION__PROP_NAME:
        return basicSetPropName(null, msgs);
      case SadlPackage.RESOURCE_BY_RESTRICTION__COND:
        return basicSetCond(null, msgs);
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
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_TYPE:
        return getAnnType();
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_CONTENT:
        return getAnnContent();
      case SadlPackage.RESOURCE_BY_RESTRICTION__PROP_NAME:
        return getPropName();
      case SadlPackage.RESOURCE_BY_RESTRICTION__COND:
        return getCond();
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
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_TYPE:
        getAnnType().clear();
        getAnnType().addAll((Collection<? extends String>)newValue);
        return;
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_CONTENT:
        getAnnContent().clear();
        getAnnContent().addAll((Collection<? extends ContentList>)newValue);
        return;
      case SadlPackage.RESOURCE_BY_RESTRICTION__PROP_NAME:
        setPropName((ResourceByName)newValue);
        return;
      case SadlPackage.RESOURCE_BY_RESTRICTION__COND:
        setCond((Condition)newValue);
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
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_TYPE:
        getAnnType().clear();
        return;
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_CONTENT:
        getAnnContent().clear();
        return;
      case SadlPackage.RESOURCE_BY_RESTRICTION__PROP_NAME:
        setPropName((ResourceByName)null);
        return;
      case SadlPackage.RESOURCE_BY_RESTRICTION__COND:
        setCond((Condition)null);
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
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_TYPE:
        return annType != null && !annType.isEmpty();
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_CONTENT:
        return annContent != null && !annContent.isEmpty();
      case SadlPackage.RESOURCE_BY_RESTRICTION__PROP_NAME:
        return propName != null;
      case SadlPackage.RESOURCE_BY_RESTRICTION__COND:
        return cond != null;
    }
    return super.eIsSet(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String toString()
  {
    if (eIsProxy()) return super.toString();

    StringBuffer result = new StringBuffer(super.toString());
    result.append(" (annType: ");
    result.append(annType);
    result.append(')');
    return result.toString();
  }

} //ResourceByRestrictionImpl
