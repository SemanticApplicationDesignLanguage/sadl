/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.Rule#getName <em>Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Rule#getGivens <em>Givens</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Rule#getIfs <em>Ifs</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Rule#getThens <em>Thens</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getRule()
 * @model
 * @generated
 */
public interface Rule extends ModelElement
{
  /**
   * Returns the value of the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Name</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Name</em>' attribute.
   * @see #setName(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getRule_Name()
   * @model
   * @generated
   */
  String getName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Rule#getName <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Name</em>' attribute.
   * @see #getName()
   * @generated
   */
  void setName(String value);

  /**
   * Returns the value of the '<em><b>Givens</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Givens</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Givens</em>' containment reference.
   * @see #setGivens(ElementSet)
   * @see com.ge.research.sadl.sadl.SadlPackage#getRule_Givens()
   * @model containment="true"
   * @generated
   */
  ElementSet getGivens();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Rule#getGivens <em>Givens</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Givens</em>' containment reference.
   * @see #getGivens()
   * @generated
   */
  void setGivens(ElementSet value);

  /**
   * Returns the value of the '<em><b>Ifs</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Ifs</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Ifs</em>' containment reference.
   * @see #setIfs(ElementSet)
   * @see com.ge.research.sadl.sadl.SadlPackage#getRule_Ifs()
   * @model containment="true"
   * @generated
   */
  ElementSet getIfs();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Rule#getIfs <em>Ifs</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Ifs</em>' containment reference.
   * @see #getIfs()
   * @generated
   */
  void setIfs(ElementSet value);

  /**
   * Returns the value of the '<em><b>Thens</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Thens</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Thens</em>' containment reference.
   * @see #setThens(ElementSet)
   * @see com.ge.research.sadl.sadl.SadlPackage#getRule_Thens()
   * @model containment="true"
   * @generated
   */
  ElementSet getThens();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Rule#getThens <em>Thens</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Thens</em>' containment reference.
   * @see #getThens()
   * @generated
   */
  void setThens(ElementSet value);

} // Rule
