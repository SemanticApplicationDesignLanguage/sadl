/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.Condition;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceByRestriction;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

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
   * The default value of the '{@link #getAnnType() <em>Ann Type</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnType()
   * @generated
   * @ordered
   */
  protected static final String ANN_TYPE_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getAnnType() <em>Ann Type</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnType()
   * @generated
   * @ordered
   */
  protected String annType = ANN_TYPE_EDEFAULT;

  /**
   * The default value of the '{@link #getAnnContent() <em>Ann Content</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnContent()
   * @generated
   * @ordered
   */
  protected static final String ANN_CONTENT_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getAnnContent() <em>Ann Content</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnContent()
   * @generated
   * @ordered
   */
  protected String annContent = ANN_CONTENT_EDEFAULT;

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
  public String getAnnType()
  {
    return annType;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setAnnType(String newAnnType)
  {
    String oldAnnType = annType;
    annType = newAnnType;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RESOURCE_BY_RESTRICTION__ANN_TYPE, oldAnnType, annType));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getAnnContent()
  {
    return annContent;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setAnnContent(String newAnnContent)
  {
    String oldAnnContent = annContent;
    annContent = newAnnContent;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RESOURCE_BY_RESTRICTION__ANN_CONTENT, oldAnnContent, annContent));
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
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_TYPE:
        setAnnType((String)newValue);
        return;
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_CONTENT:
        setAnnContent((String)newValue);
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
        setAnnType(ANN_TYPE_EDEFAULT);
        return;
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_CONTENT:
        setAnnContent(ANN_CONTENT_EDEFAULT);
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
        return ANN_TYPE_EDEFAULT == null ? annType != null : !ANN_TYPE_EDEFAULT.equals(annType);
      case SadlPackage.RESOURCE_BY_RESTRICTION__ANN_CONTENT:
        return ANN_CONTENT_EDEFAULT == null ? annContent != null : !ANN_CONTENT_EDEFAULT.equals(annContent);
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
    result.append(", annContent: ");
    result.append(annContent);
    result.append(')');
    return result.toString();
  }

} //ResourceByRestrictionImpl
