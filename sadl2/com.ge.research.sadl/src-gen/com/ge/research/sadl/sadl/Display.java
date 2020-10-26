/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Display</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.Display#getDisplayString <em>Display String</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Display#getModel <em>Model</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getDisplay()
 * @model
 * @generated
 */
public interface Display extends ModelElement
{
  /**
   * Returns the value of the '<em><b>Display String</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Display String</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Display String</em>' attribute.
   * @see #setDisplayString(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getDisplay_DisplayString()
   * @model
   * @generated
   */
  String getDisplayString();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Display#getDisplayString <em>Display String</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Display String</em>' attribute.
   * @see #getDisplayString()
   * @generated
   */
  void setDisplayString(String value);

  /**
   * Returns the value of the '<em><b>Model</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Model</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Model</em>' attribute.
   * @see #setModel(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getDisplay_Model()
   * @model
   * @generated
   */
  String getModel();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Display#getModel <em>Model</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Model</em>' attribute.
   * @see #getModel()
   * @generated
   */
  void setModel(String value);

} // Display
