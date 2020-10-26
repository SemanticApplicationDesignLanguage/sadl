/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Select Expression</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.SelectExpression#getDistinct <em>Distinct</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.SelectExpression#getAllVars <em>All Vars</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.SelectExpression#getVarList <em>Var List</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.SelectExpression#getOrderby <em>Orderby</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.SelectExpression#getOrderList <em>Order List</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getSelectExpression()
 * @model
 * @generated
 */
public interface SelectExpression extends Expression
{
  /**
   * Returns the value of the '<em><b>Distinct</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Distinct</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Distinct</em>' attribute.
   * @see #setDistinct(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSelectExpression_Distinct()
   * @model
   * @generated
   */
  String getDistinct();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SelectExpression#getDistinct <em>Distinct</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Distinct</em>' attribute.
   * @see #getDistinct()
   * @generated
   */
  void setDistinct(String value);

  /**
   * Returns the value of the '<em><b>All Vars</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>All Vars</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>All Vars</em>' attribute.
   * @see #setAllVars(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSelectExpression_AllVars()
   * @model
   * @generated
   */
  String getAllVars();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SelectExpression#getAllVars <em>All Vars</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>All Vars</em>' attribute.
   * @see #getAllVars()
   * @generated
   */
  void setAllVars(String value);

  /**
   * Returns the value of the '<em><b>Var List</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Var List</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Var List</em>' containment reference.
   * @see #setVarList(VariableList)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSelectExpression_VarList()
   * @model containment="true"
   * @generated
   */
  VariableList getVarList();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SelectExpression#getVarList <em>Var List</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Var List</em>' containment reference.
   * @see #getVarList()
   * @generated
   */
  void setVarList(VariableList value);

  /**
   * Returns the value of the '<em><b>Orderby</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Orderby</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Orderby</em>' attribute.
   * @see #setOrderby(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSelectExpression_Orderby()
   * @model
   * @generated
   */
  String getOrderby();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SelectExpression#getOrderby <em>Orderby</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Orderby</em>' attribute.
   * @see #getOrderby()
   * @generated
   */
  void setOrderby(String value);

  /**
   * Returns the value of the '<em><b>Order List</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Order List</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Order List</em>' containment reference.
   * @see #setOrderList(OrderList)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSelectExpression_OrderList()
   * @model containment="true"
   * @generated
   */
  OrderList getOrderList();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SelectExpression#getOrderList <em>Order List</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Order List</em>' containment reference.
   * @see #getOrderList()
   * @generated
   */
  void setOrderList(OrderList value);

} // SelectExpression
