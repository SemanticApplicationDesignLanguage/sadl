/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ExplicitValue;
import com.ge.research.sadl.sadl.Expression;
import com.ge.research.sadl.sadl.GraphPattern;
import com.ge.research.sadl.sadl.IntervalValue;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.ValueTable;

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
 * An implementation of the model object '<em><b>Expression</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExpressionImpl#getExpr <em>Expr</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExpressionImpl#getFunc <em>Func</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExpressionImpl#getArgs <em>Args</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExpressionImpl#getGp <em>Gp</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExpressionImpl#getIvalue <em>Ivalue</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExpressionImpl#getValue <em>Value</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ExpressionImpl#getValueTable <em>Value Table</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ExpressionImpl extends MinimalEObjectImpl.Container implements Expression
{
  /**
   * The cached value of the '{@link #getExpr() <em>Expr</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getExpr()
   * @generated
   * @ordered
   */
  protected Expression expr;

  /**
   * The default value of the '{@link #getFunc() <em>Func</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getFunc()
   * @generated
   * @ordered
   */
  protected static final String FUNC_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getFunc() <em>Func</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getFunc()
   * @generated
   * @ordered
   */
  protected String func = FUNC_EDEFAULT;

  /**
   * The cached value of the '{@link #getArgs() <em>Args</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getArgs()
   * @generated
   * @ordered
   */
  protected EList<Expression> args;

  /**
   * The cached value of the '{@link #getGp() <em>Gp</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getGp()
   * @generated
   * @ordered
   */
  protected GraphPattern gp;

  /**
   * The cached value of the '{@link #getIvalue() <em>Ivalue</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getIvalue()
   * @generated
   * @ordered
   */
  protected IntervalValue ivalue;

  /**
   * The cached value of the '{@link #getValue() <em>Value</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getValue()
   * @generated
   * @ordered
   */
  protected ExplicitValue value;

  /**
   * The cached value of the '{@link #getValueTable() <em>Value Table</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getValueTable()
   * @generated
   * @ordered
   */
  protected ValueTable valueTable;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ExpressionImpl()
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
    return SadlPackage.Literals.EXPRESSION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Expression getExpr()
  {
    return expr;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetExpr(Expression newExpr, NotificationChain msgs)
  {
    Expression oldExpr = expr;
    expr = newExpr;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.EXPRESSION__EXPR, oldExpr, newExpr);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setExpr(Expression newExpr)
  {
    if (newExpr != expr)
    {
      NotificationChain msgs = null;
      if (expr != null)
        msgs = ((InternalEObject)expr).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPRESSION__EXPR, null, msgs);
      if (newExpr != null)
        msgs = ((InternalEObject)newExpr).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPRESSION__EXPR, null, msgs);
      msgs = basicSetExpr(newExpr, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXPRESSION__EXPR, newExpr, newExpr));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getFunc()
  {
    return func;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setFunc(String newFunc)
  {
    String oldFunc = func;
    func = newFunc;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXPRESSION__FUNC, oldFunc, func));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<Expression> getArgs()
  {
    if (args == null)
    {
      args = new EObjectContainmentEList<Expression>(Expression.class, this, SadlPackage.EXPRESSION__ARGS);
    }
    return args;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public GraphPattern getGp()
  {
    return gp;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetGp(GraphPattern newGp, NotificationChain msgs)
  {
    GraphPattern oldGp = gp;
    gp = newGp;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.EXPRESSION__GP, oldGp, newGp);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setGp(GraphPattern newGp)
  {
    if (newGp != gp)
    {
      NotificationChain msgs = null;
      if (gp != null)
        msgs = ((InternalEObject)gp).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPRESSION__GP, null, msgs);
      if (newGp != null)
        msgs = ((InternalEObject)newGp).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPRESSION__GP, null, msgs);
      msgs = basicSetGp(newGp, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXPRESSION__GP, newGp, newGp));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public IntervalValue getIvalue()
  {
    return ivalue;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetIvalue(IntervalValue newIvalue, NotificationChain msgs)
  {
    IntervalValue oldIvalue = ivalue;
    ivalue = newIvalue;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.EXPRESSION__IVALUE, oldIvalue, newIvalue);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setIvalue(IntervalValue newIvalue)
  {
    if (newIvalue != ivalue)
    {
      NotificationChain msgs = null;
      if (ivalue != null)
        msgs = ((InternalEObject)ivalue).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPRESSION__IVALUE, null, msgs);
      if (newIvalue != null)
        msgs = ((InternalEObject)newIvalue).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPRESSION__IVALUE, null, msgs);
      msgs = basicSetIvalue(newIvalue, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXPRESSION__IVALUE, newIvalue, newIvalue));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExplicitValue getValue()
  {
    return value;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetValue(ExplicitValue newValue, NotificationChain msgs)
  {
    ExplicitValue oldValue = value;
    value = newValue;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.EXPRESSION__VALUE, oldValue, newValue);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setValue(ExplicitValue newValue)
  {
    if (newValue != value)
    {
      NotificationChain msgs = null;
      if (value != null)
        msgs = ((InternalEObject)value).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPRESSION__VALUE, null, msgs);
      if (newValue != null)
        msgs = ((InternalEObject)newValue).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPRESSION__VALUE, null, msgs);
      msgs = basicSetValue(newValue, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXPRESSION__VALUE, newValue, newValue));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ValueTable getValueTable()
  {
    return valueTable;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetValueTable(ValueTable newValueTable, NotificationChain msgs)
  {
    ValueTable oldValueTable = valueTable;
    valueTable = newValueTable;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.EXPRESSION__VALUE_TABLE, oldValueTable, newValueTable);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setValueTable(ValueTable newValueTable)
  {
    if (newValueTable != valueTable)
    {
      NotificationChain msgs = null;
      if (valueTable != null)
        msgs = ((InternalEObject)valueTable).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPRESSION__VALUE_TABLE, null, msgs);
      if (newValueTable != null)
        msgs = ((InternalEObject)newValueTable).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.EXPRESSION__VALUE_TABLE, null, msgs);
      msgs = basicSetValueTable(newValueTable, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.EXPRESSION__VALUE_TABLE, newValueTable, newValueTable));
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
      case SadlPackage.EXPRESSION__EXPR:
        return basicSetExpr(null, msgs);
      case SadlPackage.EXPRESSION__ARGS:
        return ((InternalEList<?>)getArgs()).basicRemove(otherEnd, msgs);
      case SadlPackage.EXPRESSION__GP:
        return basicSetGp(null, msgs);
      case SadlPackage.EXPRESSION__IVALUE:
        return basicSetIvalue(null, msgs);
      case SadlPackage.EXPRESSION__VALUE:
        return basicSetValue(null, msgs);
      case SadlPackage.EXPRESSION__VALUE_TABLE:
        return basicSetValueTable(null, msgs);
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
      case SadlPackage.EXPRESSION__EXPR:
        return getExpr();
      case SadlPackage.EXPRESSION__FUNC:
        return getFunc();
      case SadlPackage.EXPRESSION__ARGS:
        return getArgs();
      case SadlPackage.EXPRESSION__GP:
        return getGp();
      case SadlPackage.EXPRESSION__IVALUE:
        return getIvalue();
      case SadlPackage.EXPRESSION__VALUE:
        return getValue();
      case SadlPackage.EXPRESSION__VALUE_TABLE:
        return getValueTable();
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
      case SadlPackage.EXPRESSION__EXPR:
        setExpr((Expression)newValue);
        return;
      case SadlPackage.EXPRESSION__FUNC:
        setFunc((String)newValue);
        return;
      case SadlPackage.EXPRESSION__ARGS:
        getArgs().clear();
        getArgs().addAll((Collection<? extends Expression>)newValue);
        return;
      case SadlPackage.EXPRESSION__GP:
        setGp((GraphPattern)newValue);
        return;
      case SadlPackage.EXPRESSION__IVALUE:
        setIvalue((IntervalValue)newValue);
        return;
      case SadlPackage.EXPRESSION__VALUE:
        setValue((ExplicitValue)newValue);
        return;
      case SadlPackage.EXPRESSION__VALUE_TABLE:
        setValueTable((ValueTable)newValue);
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
      case SadlPackage.EXPRESSION__EXPR:
        setExpr((Expression)null);
        return;
      case SadlPackage.EXPRESSION__FUNC:
        setFunc(FUNC_EDEFAULT);
        return;
      case SadlPackage.EXPRESSION__ARGS:
        getArgs().clear();
        return;
      case SadlPackage.EXPRESSION__GP:
        setGp((GraphPattern)null);
        return;
      case SadlPackage.EXPRESSION__IVALUE:
        setIvalue((IntervalValue)null);
        return;
      case SadlPackage.EXPRESSION__VALUE:
        setValue((ExplicitValue)null);
        return;
      case SadlPackage.EXPRESSION__VALUE_TABLE:
        setValueTable((ValueTable)null);
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
      case SadlPackage.EXPRESSION__EXPR:
        return expr != null;
      case SadlPackage.EXPRESSION__FUNC:
        return FUNC_EDEFAULT == null ? func != null : !FUNC_EDEFAULT.equals(func);
      case SadlPackage.EXPRESSION__ARGS:
        return args != null && !args.isEmpty();
      case SadlPackage.EXPRESSION__GP:
        return gp != null;
      case SadlPackage.EXPRESSION__IVALUE:
        return ivalue != null;
      case SadlPackage.EXPRESSION__VALUE:
        return value != null;
      case SadlPackage.EXPRESSION__VALUE_TABLE:
        return valueTable != null;
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
    result.append(" (func: ");
    result.append(func);
    result.append(')');
    return result.toString();
  }

} //ExpressionImpl
