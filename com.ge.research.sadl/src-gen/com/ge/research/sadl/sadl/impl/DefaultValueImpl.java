/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.DefaultValue;
import com.ge.research.sadl.sadl.ExplicitValue;
import com.ge.research.sadl.sadl.PropertyOfClass;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Default Value</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.DefaultValueImpl#getDefValueClass <em>Def Value Class</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.DefaultValueImpl#getLevel <em>Level</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.DefaultValueImpl#getDefValue <em>Def Value</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DefaultValueImpl extends StatementImpl implements DefaultValue
{
  /**
   * The cached value of the '{@link #getDefValueClass() <em>Def Value Class</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDefValueClass()
   * @generated
   * @ordered
   */
  protected PropertyOfClass defValueClass;

  /**
   * The default value of the '{@link #getLevel() <em>Level</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLevel()
   * @generated
   * @ordered
   */
  protected static final String LEVEL_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getLevel() <em>Level</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLevel()
   * @generated
   * @ordered
   */
  protected String level = LEVEL_EDEFAULT;

  /**
   * The cached value of the '{@link #getDefValue() <em>Def Value</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDefValue()
   * @generated
   * @ordered
   */
  protected ExplicitValue defValue;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected DefaultValueImpl()
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
    return SadlPackage.Literals.DEFAULT_VALUE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public PropertyOfClass getDefValueClass()
  {
    return defValueClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetDefValueClass(PropertyOfClass newDefValueClass, NotificationChain msgs)
  {
    PropertyOfClass oldDefValueClass = defValueClass;
    defValueClass = newDefValueClass;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.DEFAULT_VALUE__DEF_VALUE_CLASS, oldDefValueClass, newDefValueClass);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setDefValueClass(PropertyOfClass newDefValueClass)
  {
    if (newDefValueClass != defValueClass)
    {
      NotificationChain msgs = null;
      if (defValueClass != null)
        msgs = ((InternalEObject)defValueClass).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.DEFAULT_VALUE__DEF_VALUE_CLASS, null, msgs);
      if (newDefValueClass != null)
        msgs = ((InternalEObject)newDefValueClass).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.DEFAULT_VALUE__DEF_VALUE_CLASS, null, msgs);
      msgs = basicSetDefValueClass(newDefValueClass, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.DEFAULT_VALUE__DEF_VALUE_CLASS, newDefValueClass, newDefValueClass));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getLevel()
  {
    return level;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setLevel(String newLevel)
  {
    String oldLevel = level;
    level = newLevel;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.DEFAULT_VALUE__LEVEL, oldLevel, level));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExplicitValue getDefValue()
  {
    return defValue;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetDefValue(ExplicitValue newDefValue, NotificationChain msgs)
  {
    ExplicitValue oldDefValue = defValue;
    defValue = newDefValue;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.DEFAULT_VALUE__DEF_VALUE, oldDefValue, newDefValue);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setDefValue(ExplicitValue newDefValue)
  {
    if (newDefValue != defValue)
    {
      NotificationChain msgs = null;
      if (defValue != null)
        msgs = ((InternalEObject)defValue).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.DEFAULT_VALUE__DEF_VALUE, null, msgs);
      if (newDefValue != null)
        msgs = ((InternalEObject)newDefValue).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.DEFAULT_VALUE__DEF_VALUE, null, msgs);
      msgs = basicSetDefValue(newDefValue, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.DEFAULT_VALUE__DEF_VALUE, newDefValue, newDefValue));
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
      case SadlPackage.DEFAULT_VALUE__DEF_VALUE_CLASS:
        return basicSetDefValueClass(null, msgs);
      case SadlPackage.DEFAULT_VALUE__DEF_VALUE:
        return basicSetDefValue(null, msgs);
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
      case SadlPackage.DEFAULT_VALUE__DEF_VALUE_CLASS:
        return getDefValueClass();
      case SadlPackage.DEFAULT_VALUE__LEVEL:
        return getLevel();
      case SadlPackage.DEFAULT_VALUE__DEF_VALUE:
        return getDefValue();
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
      case SadlPackage.DEFAULT_VALUE__DEF_VALUE_CLASS:
        setDefValueClass((PropertyOfClass)newValue);
        return;
      case SadlPackage.DEFAULT_VALUE__LEVEL:
        setLevel((String)newValue);
        return;
      case SadlPackage.DEFAULT_VALUE__DEF_VALUE:
        setDefValue((ExplicitValue)newValue);
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
      case SadlPackage.DEFAULT_VALUE__DEF_VALUE_CLASS:
        setDefValueClass((PropertyOfClass)null);
        return;
      case SadlPackage.DEFAULT_VALUE__LEVEL:
        setLevel(LEVEL_EDEFAULT);
        return;
      case SadlPackage.DEFAULT_VALUE__DEF_VALUE:
        setDefValue((ExplicitValue)null);
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
      case SadlPackage.DEFAULT_VALUE__DEF_VALUE_CLASS:
        return defValueClass != null;
      case SadlPackage.DEFAULT_VALUE__LEVEL:
        return LEVEL_EDEFAULT == null ? level != null : !LEVEL_EDEFAULT.equals(level);
      case SadlPackage.DEFAULT_VALUE__DEF_VALUE:
        return defValue != null;
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
    result.append(" (level: ");
    result.append(level);
    result.append(')');
    return result.toString();
  }

} //DefaultValueImpl
