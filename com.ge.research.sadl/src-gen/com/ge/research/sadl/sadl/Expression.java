/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Expression</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.Expression#getExpr <em>Expr</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Expression#getFunc <em>Func</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Expression#getArgs <em>Args</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Expression#getGp <em>Gp</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Expression#getIvalue <em>Ivalue</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Expression#getValue <em>Value</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Expression#getValueTable <em>Value Table</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getExpression()
 * @model
 * @generated
 */
public interface Expression extends EObject
{
  /**
   * Returns the value of the '<em><b>Expr</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Expr</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Expr</em>' containment reference.
   * @see #setExpr(Expression)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExpression_Expr()
   * @model containment="true"
   * @generated
   */
  Expression getExpr();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Expression#getExpr <em>Expr</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Expr</em>' containment reference.
   * @see #getExpr()
   * @generated
   */
  void setExpr(Expression value);

  /**
   * Returns the value of the '<em><b>Func</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Func</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Func</em>' attribute.
   * @see #setFunc(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExpression_Func()
   * @model
   * @generated
   */
  String getFunc();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Expression#getFunc <em>Func</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Func</em>' attribute.
   * @see #getFunc()
   * @generated
   */
  void setFunc(String value);

  /**
   * Returns the value of the '<em><b>Args</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.Expression}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Args</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Args</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getExpression_Args()
   * @model containment="true"
   * @generated
   */
  EList<Expression> getArgs();

  /**
   * Returns the value of the '<em><b>Gp</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Gp</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Gp</em>' containment reference.
   * @see #setGp(GraphPattern)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExpression_Gp()
   * @model containment="true"
   * @generated
   */
  GraphPattern getGp();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Expression#getGp <em>Gp</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Gp</em>' containment reference.
   * @see #getGp()
   * @generated
   */
  void setGp(GraphPattern value);

  /**
   * Returns the value of the '<em><b>Ivalue</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Ivalue</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Ivalue</em>' containment reference.
   * @see #setIvalue(IntervalValue)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExpression_Ivalue()
   * @model containment="true"
   * @generated
   */
  IntervalValue getIvalue();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Expression#getIvalue <em>Ivalue</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Ivalue</em>' containment reference.
   * @see #getIvalue()
   * @generated
   */
  void setIvalue(IntervalValue value);

  /**
   * Returns the value of the '<em><b>Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Value</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Value</em>' containment reference.
   * @see #setValue(ExplicitValue)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExpression_Value()
   * @model containment="true"
   * @generated
   */
  ExplicitValue getValue();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Expression#getValue <em>Value</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Value</em>' containment reference.
   * @see #getValue()
   * @generated
   */
  void setValue(ExplicitValue value);

  /**
   * Returns the value of the '<em><b>Value Table</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Value Table</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Value Table</em>' containment reference.
   * @see #setValueTable(ValueTable)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExpression_ValueTable()
   * @model containment="true"
   * @generated
   */
  ValueTable getValueTable();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Expression#getValueTable <em>Value Table</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Value Table</em>' containment reference.
   * @see #getValueTable()
   * @generated
   */
  void setValueTable(ValueTable value);

} // Expression
