/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Min Card Condition</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.MinCardCondition#getCard <em>Card</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.MinCardCondition#getClassQualifier <em>Class Qualifier</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getMinCardCondition()
 * @model
 * @generated
 */
public interface MinCardCondition extends Condition
{
  /**
   * Returns the value of the '<em><b>Card</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Card</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Card</em>' attribute.
   * @see #setCard(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getMinCardCondition_Card()
   * @model
   * @generated
   */
  String getCard();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.MinCardCondition#getCard <em>Card</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Card</em>' attribute.
   * @see #getCard()
   * @generated
   */
  void setCard(String value);

  /**
   * Returns the value of the '<em><b>Class Qualifier</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Class Qualifier</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Class Qualifier</em>' containment reference.
   * @see #setClassQualifier(ResourceIdentifier)
   * @see com.ge.research.sadl.sadl.SadlPackage#getMinCardCondition_ClassQualifier()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getClassQualifier();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.MinCardCondition#getClassQualifier <em>Class Qualifier</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class Qualifier</em>' containment reference.
   * @see #getClassQualifier()
   * @generated
   */
  void setClassQualifier(ResourceIdentifier value);

} // MinCardCondition
