/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Default Value</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.DefaultValue#getDefValueClass <em>Def Value Class</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.DefaultValue#getLevel <em>Level</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.DefaultValue#getDefValue <em>Def Value</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getDefaultValue()
 * @model
 * @generated
 */
public interface DefaultValue extends Statement
{
  /**
   * Returns the value of the '<em><b>Def Value Class</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Def Value Class</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Def Value Class</em>' containment reference.
   * @see #setDefValueClass(PropertyOfClass)
   * @see com.ge.research.sadl.sadl.SadlPackage#getDefaultValue_DefValueClass()
   * @model containment="true"
   * @generated
   */
  PropertyOfClass getDefValueClass();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.DefaultValue#getDefValueClass <em>Def Value Class</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Def Value Class</em>' containment reference.
   * @see #getDefValueClass()
   * @generated
   */
  void setDefValueClass(PropertyOfClass value);

  /**
   * Returns the value of the '<em><b>Level</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Level</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Level</em>' attribute.
   * @see #setLevel(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getDefaultValue_Level()
   * @model
   * @generated
   */
  String getLevel();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.DefaultValue#getLevel <em>Level</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Level</em>' attribute.
   * @see #getLevel()
   * @generated
   */
  void setLevel(String value);

  /**
   * Returns the value of the '<em><b>Def Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Def Value</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Def Value</em>' containment reference.
   * @see #setDefValue(ExplicitValue)
   * @see com.ge.research.sadl.sadl.SadlPackage#getDefaultValue_DefValue()
   * @model containment="true"
   * @generated
   */
  ExplicitValue getDefValue();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.DefaultValue#getDefValue <em>Def Value</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Def Value</em>' containment reference.
   * @see #getDefValue()
   * @generated
   */
  void setDefValue(ExplicitValue value);

} // DefaultValue
