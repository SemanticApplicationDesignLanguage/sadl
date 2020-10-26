/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Inst Attr PSV</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.InstAttrPSV#getProp <em>Prop</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.InstAttrPSV#getVal <em>Val</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getInstAttrPSV()
 * @model
 * @generated
 */
public interface InstAttrPSV extends GraphPattern
{
  /**
   * Returns the value of the '<em><b>Prop</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Prop</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Prop</em>' containment reference.
   * @see #setProp(PropOfSubj)
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstAttrPSV_Prop()
   * @model containment="true"
   * @generated
   */
  PropOfSubj getProp();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InstAttrPSV#getProp <em>Prop</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Prop</em>' containment reference.
   * @see #getProp()
   * @generated
   */
  void setProp(PropOfSubj value);

  /**
   * Returns the value of the '<em><b>Val</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Val</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Val</em>' containment reference.
   * @see #setVal(ExplicitValue)
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstAttrPSV_Val()
   * @model containment="true"
   * @generated
   */
  ExplicitValue getVal();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InstAttrPSV#getVal <em>Val</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Val</em>' containment reference.
   * @see #getVal()
   * @generated
   */
  void setVal(ExplicitValue value);

} // InstAttrPSV
