/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Explicit Value</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.ExplicitValue#getInstName <em>Inst Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ExplicitValue#getLitValue <em>Lit Value</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ExplicitValue#getValueList <em>Value List</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ExplicitValue#getRow <em>Row</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ExplicitValue#getTerm <em>Term</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getExplicitValue()
 * @model
 * @generated
 */
public interface ExplicitValue extends EObject
{
  /**
   * Returns the value of the '<em><b>Inst Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Inst Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Inst Name</em>' containment reference.
   * @see #setInstName(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExplicitValue_InstName()
   * @model containment="true"
   * @generated
   */
  ResourceByName getInstName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ExplicitValue#getInstName <em>Inst Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Inst Name</em>' containment reference.
   * @see #getInstName()
   * @generated
   */
  void setInstName(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Lit Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Lit Value</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Lit Value</em>' containment reference.
   * @see #setLitValue(LiteralValue)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExplicitValue_LitValue()
   * @model containment="true"
   * @generated
   */
  LiteralValue getLitValue();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ExplicitValue#getLitValue <em>Lit Value</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Lit Value</em>' containment reference.
   * @see #getLitValue()
   * @generated
   */
  void setLitValue(LiteralValue value);

  /**
   * Returns the value of the '<em><b>Value List</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Value List</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Value List</em>' attribute.
   * @see #setValueList(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExplicitValue_ValueList()
   * @model
   * @generated
   */
  String getValueList();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ExplicitValue#getValueList <em>Value List</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Value List</em>' attribute.
   * @see #getValueList()
   * @generated
   */
  void setValueList(String value);

  /**
   * Returns the value of the '<em><b>Row</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Row</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Row</em>' containment reference.
   * @see #setRow(ValueRow)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExplicitValue_Row()
   * @model containment="true"
   * @generated
   */
  ValueRow getRow();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ExplicitValue#getRow <em>Row</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Row</em>' containment reference.
   * @see #getRow()
   * @generated
   */
  void setRow(ValueRow value);

  /**
   * Returns the value of the '<em><b>Term</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Term</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Term</em>' attribute.
   * @see #setTerm(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExplicitValue_Term()
   * @model
   * @generated
   */
  String getTerm();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ExplicitValue#getTerm <em>Term</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Term</em>' attribute.
   * @see #getTerm()
   * @generated
   */
  void setTerm(String value);

} // ExplicitValue
