/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.AdditionalPropertyInfo;
import com.ge.research.sadl.sadl.Condition;
import com.ge.research.sadl.sadl.IsInverseOf;
import com.ge.research.sadl.sadl.Range;
import com.ge.research.sadl.sadl.ResourceIdentifier;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Additional Property Info</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl#getDomain <em>Domain</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl#getCond <em>Cond</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl#getRange <em>Range</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl#getIsfunc <em>Isfunc</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl#getIsinvfunc <em>Isinvfunc</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl#getIsSym <em>Is Sym</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl#getIsTrans <em>Is Trans</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.AdditionalPropertyInfoImpl#getIsInvOf <em>Is Inv Of</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class AdditionalPropertyInfoImpl extends MinimalEObjectImpl.Container implements AdditionalPropertyInfo
{
  /**
   * The cached value of the '{@link #getDomain() <em>Domain</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDomain()
   * @generated
   * @ordered
   */
  protected ResourceIdentifier domain;

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
   * The cached value of the '{@link #getRange() <em>Range</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRange()
   * @generated
   * @ordered
   */
  protected Range range;

  /**
   * The default value of the '{@link #getIsfunc() <em>Isfunc</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getIsfunc()
   * @generated
   * @ordered
   */
  protected static final String ISFUNC_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getIsfunc() <em>Isfunc</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getIsfunc()
   * @generated
   * @ordered
   */
  protected String isfunc = ISFUNC_EDEFAULT;

  /**
   * The default value of the '{@link #getIsinvfunc() <em>Isinvfunc</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getIsinvfunc()
   * @generated
   * @ordered
   */
  protected static final String ISINVFUNC_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getIsinvfunc() <em>Isinvfunc</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getIsinvfunc()
   * @generated
   * @ordered
   */
  protected String isinvfunc = ISINVFUNC_EDEFAULT;

  /**
   * The default value of the '{@link #getIsSym() <em>Is Sym</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getIsSym()
   * @generated
   * @ordered
   */
  protected static final String IS_SYM_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getIsSym() <em>Is Sym</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getIsSym()
   * @generated
   * @ordered
   */
  protected String isSym = IS_SYM_EDEFAULT;

  /**
   * The default value of the '{@link #getIsTrans() <em>Is Trans</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getIsTrans()
   * @generated
   * @ordered
   */
  protected static final String IS_TRANS_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getIsTrans() <em>Is Trans</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getIsTrans()
   * @generated
   * @ordered
   */
  protected String isTrans = IS_TRANS_EDEFAULT;

  /**
   * The cached value of the '{@link #getIsInvOf() <em>Is Inv Of</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getIsInvOf()
   * @generated
   * @ordered
   */
  protected IsInverseOf isInvOf;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected AdditionalPropertyInfoImpl()
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
    return SadlPackage.Literals.ADDITIONAL_PROPERTY_INFO;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceIdentifier getDomain()
  {
    return domain;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetDomain(ResourceIdentifier newDomain, NotificationChain msgs)
  {
    ResourceIdentifier oldDomain = domain;
    domain = newDomain;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__DOMAIN, oldDomain, newDomain);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setDomain(ResourceIdentifier newDomain)
  {
    if (newDomain != domain)
    {
      NotificationChain msgs = null;
      if (domain != null)
        msgs = ((InternalEObject)domain).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDITIONAL_PROPERTY_INFO__DOMAIN, null, msgs);
      if (newDomain != null)
        msgs = ((InternalEObject)newDomain).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDITIONAL_PROPERTY_INFO__DOMAIN, null, msgs);
      msgs = basicSetDomain(newDomain, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__DOMAIN, newDomain, newDomain));
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
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__COND, oldCond, newCond);
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
        msgs = ((InternalEObject)cond).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDITIONAL_PROPERTY_INFO__COND, null, msgs);
      if (newCond != null)
        msgs = ((InternalEObject)newCond).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDITIONAL_PROPERTY_INFO__COND, null, msgs);
      msgs = basicSetCond(newCond, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__COND, newCond, newCond));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Range getRange()
  {
    return range;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetRange(Range newRange, NotificationChain msgs)
  {
    Range oldRange = range;
    range = newRange;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__RANGE, oldRange, newRange);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setRange(Range newRange)
  {
    if (newRange != range)
    {
      NotificationChain msgs = null;
      if (range != null)
        msgs = ((InternalEObject)range).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDITIONAL_PROPERTY_INFO__RANGE, null, msgs);
      if (newRange != null)
        msgs = ((InternalEObject)newRange).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDITIONAL_PROPERTY_INFO__RANGE, null, msgs);
      msgs = basicSetRange(newRange, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__RANGE, newRange, newRange));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getIsfunc()
  {
    return isfunc;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setIsfunc(String newIsfunc)
  {
    String oldIsfunc = isfunc;
    isfunc = newIsfunc;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__ISFUNC, oldIsfunc, isfunc));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getIsinvfunc()
  {
    return isinvfunc;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setIsinvfunc(String newIsinvfunc)
  {
    String oldIsinvfunc = isinvfunc;
    isinvfunc = newIsinvfunc;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__ISINVFUNC, oldIsinvfunc, isinvfunc));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getIsSym()
  {
    return isSym;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setIsSym(String newIsSym)
  {
    String oldIsSym = isSym;
    isSym = newIsSym;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_SYM, oldIsSym, isSym));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getIsTrans()
  {
    return isTrans;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setIsTrans(String newIsTrans)
  {
    String oldIsTrans = isTrans;
    isTrans = newIsTrans;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_TRANS, oldIsTrans, isTrans));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public IsInverseOf getIsInvOf()
  {
    return isInvOf;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetIsInvOf(IsInverseOf newIsInvOf, NotificationChain msgs)
  {
    IsInverseOf oldIsInvOf = isInvOf;
    isInvOf = newIsInvOf;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_INV_OF, oldIsInvOf, newIsInvOf);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setIsInvOf(IsInverseOf newIsInvOf)
  {
    if (newIsInvOf != isInvOf)
    {
      NotificationChain msgs = null;
      if (isInvOf != null)
        msgs = ((InternalEObject)isInvOf).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_INV_OF, null, msgs);
      if (newIsInvOf != null)
        msgs = ((InternalEObject)newIsInvOf).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_INV_OF, null, msgs);
      msgs = basicSetIsInvOf(newIsInvOf, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_INV_OF, newIsInvOf, newIsInvOf));
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
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__DOMAIN:
        return basicSetDomain(null, msgs);
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__COND:
        return basicSetCond(null, msgs);
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__RANGE:
        return basicSetRange(null, msgs);
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_INV_OF:
        return basicSetIsInvOf(null, msgs);
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
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__DOMAIN:
        return getDomain();
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__COND:
        return getCond();
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__RANGE:
        return getRange();
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__ISFUNC:
        return getIsfunc();
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__ISINVFUNC:
        return getIsinvfunc();
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_SYM:
        return getIsSym();
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_TRANS:
        return getIsTrans();
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_INV_OF:
        return getIsInvOf();
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
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__DOMAIN:
        setDomain((ResourceIdentifier)newValue);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__COND:
        setCond((Condition)newValue);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__RANGE:
        setRange((Range)newValue);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__ISFUNC:
        setIsfunc((String)newValue);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__ISINVFUNC:
        setIsinvfunc((String)newValue);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_SYM:
        setIsSym((String)newValue);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_TRANS:
        setIsTrans((String)newValue);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_INV_OF:
        setIsInvOf((IsInverseOf)newValue);
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
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__DOMAIN:
        setDomain((ResourceIdentifier)null);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__COND:
        setCond((Condition)null);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__RANGE:
        setRange((Range)null);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__ISFUNC:
        setIsfunc(ISFUNC_EDEFAULT);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__ISINVFUNC:
        setIsinvfunc(ISINVFUNC_EDEFAULT);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_SYM:
        setIsSym(IS_SYM_EDEFAULT);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_TRANS:
        setIsTrans(IS_TRANS_EDEFAULT);
        return;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_INV_OF:
        setIsInvOf((IsInverseOf)null);
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
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__DOMAIN:
        return domain != null;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__COND:
        return cond != null;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__RANGE:
        return range != null;
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__ISFUNC:
        return ISFUNC_EDEFAULT == null ? isfunc != null : !ISFUNC_EDEFAULT.equals(isfunc);
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__ISINVFUNC:
        return ISINVFUNC_EDEFAULT == null ? isinvfunc != null : !ISINVFUNC_EDEFAULT.equals(isinvfunc);
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_SYM:
        return IS_SYM_EDEFAULT == null ? isSym != null : !IS_SYM_EDEFAULT.equals(isSym);
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_TRANS:
        return IS_TRANS_EDEFAULT == null ? isTrans != null : !IS_TRANS_EDEFAULT.equals(isTrans);
      case SadlPackage.ADDITIONAL_PROPERTY_INFO__IS_INV_OF:
        return isInvOf != null;
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
    result.append(" (isfunc: ");
    result.append(isfunc);
    result.append(", isinvfunc: ");
    result.append(isinvfunc);
    result.append(", isSym: ");
    result.append(isSym);
    result.append(", isTrans: ");
    result.append(isTrans);
    result.append(')');
    return result.toString();
  }

} //AdditionalPropertyInfoImpl
