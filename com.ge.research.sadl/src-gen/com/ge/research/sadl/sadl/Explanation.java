/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Explanation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.Explanation#getExpr <em>Expr</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Explanation#getRulename <em>Rulename</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getExplanation()
 * @model
 * @generated
 */
public interface Explanation extends ModelElement
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
   * @see #setExpr(EObject)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExplanation_Expr()
   * @model containment="true"
   * @generated
   */
  EObject getExpr();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Explanation#getExpr <em>Expr</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Expr</em>' containment reference.
   * @see #getExpr()
   * @generated
   */
  void setExpr(EObject value);

  /**
   * Returns the value of the '<em><b>Rulename</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Rulename</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Rulename</em>' attribute.
   * @see #setRulename(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExplanation_Rulename()
   * @model
   * @generated
   */
  String getRulename();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Explanation#getRulename <em>Rulename</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Rulename</em>' attribute.
   * @see #getRulename()
   * @generated
   */
  void setRulename(String value);

} // Explanation
