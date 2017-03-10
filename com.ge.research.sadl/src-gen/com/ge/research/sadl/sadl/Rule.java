/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.Rule#getName <em>Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Rule#getAnnProps <em>Ann Props</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Rule#getAnnValues <em>Ann Values</em>}</li>
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
   * Returns the value of the '<em><b>Ann Props</b></em>' attribute list.
   * The list contents are of type {@link java.lang.String}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Ann Props</em>' attribute list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Ann Props</em>' attribute list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getRule_AnnProps()
   * @model unique="false"
   * @generated
   */
  EList<String> getAnnProps();

  /**
   * Returns the value of the '<em><b>Ann Values</b></em>' attribute list.
   * The list contents are of type {@link java.lang.String}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Ann Values</em>' attribute list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Ann Values</em>' attribute list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getRule_AnnValues()
   * @model unique="false"
   * @generated
   */
  EList<String> getAnnValues();

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
