/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ExplicitValue;
import com.ge.research.sadl.sadl.LiteralValue;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.ValueRow;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Explicit Value</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExplicitValueImpl#getInstName <em>Inst Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExplicitValueImpl#getLitValue <em>Lit Value</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExplicitValueImpl#getValueList <em>Value List</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExplicitValueImpl#getRow <em>Row</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExplicitValueImpl#getTerm <em>Term</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ExplicitValueImpl extends MinimalEObjectImpl.Container implements ExplicitValue
{
  /**
   * The cached value of the '{@link #getInstName() <em>Inst Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getInstName()
   * @generated
   * @ordered
   */
  protected ResourceByName instName;

  /**
   * The cached value of the '{@link #getLitValue() <em>Lit Value</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLitValue()
   * @generated
   * @ordered
   */
  protected LiteralValue litValue;

  /**
   * The default value of the '{@link #getValueList() <em>Value List</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getValueList()
   * @generated
   * @ordered
   */
  protected static final String VALUE_LIST_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getValueList() <em>Value List</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getValueList()
   * @generated
   * @ordered
   */
  protected String valueList = VALUE_LIST_EDEFAULT;

  /**
   * The cached value of the '{@link #getRow() <em>Row</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRow()
   * @generated
   * @ordered
   */
  protected ValueRow row;

  /**
   * The default value of the '{@link #getTerm() <em>Term</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getTerm()
   * @generated
   * @ordered
   */
  protected static final String TERM_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getTerm() <em>Term</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getTerm()
   * @generated
   * @ordered
   */
  protected String term = TERM_EDEFAULT;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ExplicitValueImpl()
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
    return SadlPackage.Literals.EXPLICIT_VALUE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getInstName()
  {
    return instName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetInstName(ResourceByName newInstName, NotificationChain msgs)
  {
    ResourceByName oldInstName = instName;
    instName = newInstName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.EXPLICIT_VALUE__INST_NAME, oldInstName, newInstName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setInstName(ResourceByName newInstName)
  {
    if (newInstName != instName)
    {
      NotificationChain msgs = null;
      if (instName != null)
        msgs = ((InternalEObject)instName).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPLICIT_VALUE__INST_NAME, null, msgs);
      if (newInstName != null)
        msgs = ((InternalEObject)newInstName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPLICIT_VALUE__INST_NAME, null, msgs);
      msgs = basicSetInstName(newInstName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXPLICIT_VALUE__INST_NAME, newInstName, newInstName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public LiteralValue getLitValue()
  {
    return litValue;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetLitValue(LiteralValue newLitValue, NotificationChain msgs)
  {
    LiteralValue oldLitValue = litValue;
    litValue = newLitValue;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.EXPLICIT_VALUE__LIT_VALUE, oldLitValue, newLitValue);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setLitValue(LiteralValue newLitValue)
  {
    if (newLitValue != litValue)
    {
      NotificationChain msgs = null;
      if (litValue != null)
        msgs = ((InternalEObject)litValue).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPLICIT_VALUE__LIT_VALUE, null, msgs);
      if (newLitValue != null)
        msgs = ((InternalEObject)newLitValue).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPLICIT_VALUE__LIT_VALUE, null, msgs);
      msgs = basicSetLitValue(newLitValue, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXPLICIT_VALUE__LIT_VALUE, newLitValue, newLitValue));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getValueList()
  {
    return valueList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setValueList(String newValueList)
  {
    String oldValueList = valueList;
    valueList = newValueList;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXPLICIT_VALUE__VALUE_LIST, oldValueList, valueList));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ValueRow getRow()
  {
    return row;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetRow(ValueRow newRow, NotificationChain msgs)
  {
    ValueRow oldRow = row;
    row = newRow;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.EXPLICIT_VALUE__ROW, oldRow, newRow);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setRow(ValueRow newRow)
  {
    if (newRow != row)
    {
      NotificationChain msgs = null;
      if (row != null)
        msgs = ((InternalEObject)row).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPLICIT_VALUE__ROW, null, msgs);
      if (newRow != null)
        msgs = ((InternalEObject)newRow).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPLICIT_VALUE__ROW, null, msgs);
      msgs = basicSetRow(newRow, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXPLICIT_VALUE__ROW, newRow, newRow));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getTerm()
  {
    return term;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setTerm(String newTerm)
  {
    String oldTerm = term;
    term = newTerm;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXPLICIT_VALUE__TERM, oldTerm, term));
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
      case SadlPackage.EXPLICIT_VALUE__INST_NAME:
        return basicSetInstName(null, msgs);
      case SadlPackage.EXPLICIT_VALUE__LIT_VALUE:
        return basicSetLitValue(null, msgs);
      case SadlPackage.EXPLICIT_VALUE__ROW:
        return basicSetRow(null, msgs);
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
      case SadlPackage.EXPLICIT_VALUE__INST_NAME:
        return getInstName();
      case SadlPackage.EXPLICIT_VALUE__LIT_VALUE:
        return getLitValue();
      case SadlPackage.EXPLICIT_VALUE__VALUE_LIST:
        return getValueList();
      case SadlPackage.EXPLICIT_VALUE__ROW:
        return getRow();
      case SadlPackage.EXPLICIT_VALUE__TERM:
        return getTerm();
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
      case SadlPackage.EXPLICIT_VALUE__INST_NAME:
        setInstName((ResourceByName)newValue);
        return;
      case SadlPackage.EXPLICIT_VALUE__LIT_VALUE:
        setLitValue((LiteralValue)newValue);
        return;
      case SadlPackage.EXPLICIT_VALUE__VALUE_LIST:
        setValueList((String)newValue);
        return;
      case SadlPackage.EXPLICIT_VALUE__ROW:
        setRow((ValueRow)newValue);
        return;
      case SadlPackage.EXPLICIT_VALUE__TERM:
        setTerm((String)newValue);
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
      case SadlPackage.EXPLICIT_VALUE__INST_NAME:
        setInstName((ResourceByName)null);
        return;
      case SadlPackage.EXPLICIT_VALUE__LIT_VALUE:
        setLitValue((LiteralValue)null);
        return;
      case SadlPackage.EXPLICIT_VALUE__VALUE_LIST:
        setValueList(VALUE_LIST_EDEFAULT);
        return;
      case SadlPackage.EXPLICIT_VALUE__ROW:
        setRow((ValueRow)null);
        return;
      case SadlPackage.EXPLICIT_VALUE__TERM:
        setTerm(TERM_EDEFAULT);
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
      case SadlPackage.EXPLICIT_VALUE__INST_NAME:
        return instName != null;
      case SadlPackage.EXPLICIT_VALUE__LIT_VALUE:
        return litValue != null;
      case SadlPackage.EXPLICIT_VALUE__VALUE_LIST:
        return VALUE_LIST_EDEFAULT == null ? valueList != null : !VALUE_LIST_EDEFAULT.equals(valueList);
      case SadlPackage.EXPLICIT_VALUE__ROW:
        return row != null;
      case SadlPackage.EXPLICIT_VALUE__TERM:
        return TERM_EDEFAULT == null ? term != null : !TERM_EDEFAULT.equals(term);
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
    result.append(" (valueList: ");
    result.append(valueList);
    result.append(", term: ");
    result.append(term);
    result.append(')');
    return result.toString();
  }

} //ExplicitValueImpl
