/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Construct Expression</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.ConstructExpression#getSubj <em>Subj</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ConstructExpression#getPred <em>Pred</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ConstructExpression#getObj <em>Obj</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getConstructExpression()
 * @model
 * @generated
 */
public interface ConstructExpression extends Expression
{
  /**
   * Returns the value of the '<em><b>Subj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Subj</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Subj</em>' containment reference.
   * @see #setSubj(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getConstructExpression_Subj()
   * @model containment="true"
   * @generated
   */
  ResourceName getSubj();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ConstructExpression#getSubj <em>Subj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Subj</em>' containment reference.
   * @see #getSubj()
   * @generated
   */
  void setSubj(ResourceName value);

  /**
   * Returns the value of the '<em><b>Pred</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Pred</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Pred</em>' containment reference.
   * @see #setPred(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getConstructExpression_Pred()
   * @model containment="true"
   * @generated
   */
  ResourceName getPred();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ConstructExpression#getPred <em>Pred</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Pred</em>' containment reference.
   * @see #getPred()
   * @generated
   */
  void setPred(ResourceName value);

  /**
   * Returns the value of the '<em><b>Obj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Obj</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Obj</em>' containment reference.
   * @see #setObj(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getConstructExpression_Obj()
   * @model containment="true"
   * @generated
   */
  ResourceName getObj();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ConstructExpression#getObj <em>Obj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Obj</em>' containment reference.
   * @see #getObj()
   * @generated
   */
  void setObj(ResourceName value);

} // ConstructExpression
