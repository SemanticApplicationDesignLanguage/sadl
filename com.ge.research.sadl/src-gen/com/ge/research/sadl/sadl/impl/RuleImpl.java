/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ElementSet;
import com.ge.research.sadl.sadl.Rule;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.RuleImpl#getName <em>Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.RuleImpl#getGivens <em>Givens</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.RuleImpl#getIfs <em>Ifs</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.RuleImpl#getThens <em>Thens</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class RuleImpl extends ModelElementImpl implements Rule
{
  /**
   * The default value of the '{@link #getName() <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getName()
   * @generated
   * @ordered
   */
  protected static final String NAME_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getName()
   * @generated
   * @ordered
   */
  protected String name = NAME_EDEFAULT;

  /**
   * The cached value of the '{@link #getGivens() <em>Givens</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getGivens()
   * @generated
   * @ordered
   */
  protected ElementSet givens;

  /**
   * The cached value of the '{@link #getIfs() <em>Ifs</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getIfs()
   * @generated
   * @ordered
   */
  protected ElementSet ifs;

  /**
   * The cached value of the '{@link #getThens() <em>Thens</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getThens()
   * @generated
   * @ordered
   */
  protected ElementSet thens;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected RuleImpl()
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
    return SadlPackage.Literals.RULE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getName()
  {
    return name;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setName(String newName)
  {
    String oldName = name;
    name = newName;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RULE__NAME, oldName, name));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ElementSet getGivens()
  {
    return givens;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetGivens(ElementSet newGivens, NotificationChain msgs)
  {
    ElementSet oldGivens = givens;
    givens = newGivens;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.RULE__GIVENS, oldGivens, newGivens);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setGivens(ElementSet newGivens)
  {
    if (newGivens != givens)
    {
      NotificationChain msgs = null;
      if (givens != null)
        msgs = ((InternalEObject)givens).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RULE__GIVENS, null, msgs);
      if (newGivens != null)
        msgs = ((InternalEObject)newGivens).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RULE__GIVENS, null, msgs);
      msgs = basicSetGivens(newGivens, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RULE__GIVENS, newGivens, newGivens));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ElementSet getIfs()
  {
    return ifs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetIfs(ElementSet newIfs, NotificationChain msgs)
  {
    ElementSet oldIfs = ifs;
    ifs = newIfs;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.RULE__IFS, oldIfs, newIfs);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setIfs(ElementSet newIfs)
  {
    if (newIfs != ifs)
    {
      NotificationChain msgs = null;
      if (ifs != null)
        msgs = ((InternalEObject)ifs).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RULE__IFS, null, msgs);
      if (newIfs != null)
        msgs = ((InternalEObject)newIfs).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RULE__IFS, null, msgs);
      msgs = basicSetIfs(newIfs, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RULE__IFS, newIfs, newIfs));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ElementSet getThens()
  {
    return thens;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetThens(ElementSet newThens, NotificationChain msgs)
  {
    ElementSet oldThens = thens;
    thens = newThens;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.RULE__THENS, oldThens, newThens);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setThens(ElementSet newThens)
  {
    if (newThens != thens)
    {
      NotificationChain msgs = null;
      if (thens != null)
        msgs = ((InternalEObject)thens).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RULE__THENS, null, msgs);
      if (newThens != null)
        msgs = ((InternalEObject)newThens).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.RULE__THENS, null, msgs);
      msgs = basicSetThens(newThens, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RULE__THENS, newThens, newThens));
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
      case SadlPackage.RULE__GIVENS:
        return basicSetGivens(null, msgs);
      case SadlPackage.RULE__IFS:
        return basicSetIfs(null, msgs);
      case SadlPackage.RULE__THENS:
        return basicSetThens(null, msgs);
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
      case SadlPackage.RULE__NAME:
        return getName();
      case SadlPackage.RULE__GIVENS:
        return getGivens();
      case SadlPackage.RULE__IFS:
        return getIfs();
      case SadlPackage.RULE__THENS:
        return getThens();
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
      case SadlPackage.RULE__NAME:
        setName((String)newValue);
        return;
      case SadlPackage.RULE__GIVENS:
        setGivens((ElementSet)newValue);
        return;
      case SadlPackage.RULE__IFS:
        setIfs((ElementSet)newValue);
        return;
      case SadlPackage.RULE__THENS:
        setThens((ElementSet)newValue);
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
      case SadlPackage.RULE__NAME:
        setName(NAME_EDEFAULT);
        return;
      case SadlPackage.RULE__GIVENS:
        setGivens((ElementSet)null);
        return;
      case SadlPackage.RULE__IFS:
        setIfs((ElementSet)null);
        return;
      case SadlPackage.RULE__THENS:
        setThens((ElementSet)null);
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
      case SadlPackage.RULE__NAME:
        return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
      case SadlPackage.RULE__GIVENS:
        return givens != null;
      case SadlPackage.RULE__IFS:
        return ifs != null;
      case SadlPackage.RULE__THENS:
        return thens != null;
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
    result.append(" (name: ");
    result.append(name);
    result.append(')');
    return result.toString();
  }

} //RuleImpl
