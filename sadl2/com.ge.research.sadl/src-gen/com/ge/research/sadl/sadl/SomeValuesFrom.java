/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Some Values From</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.SomeValuesFrom#getRestricted <em>Restricted</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.SomeValuesFrom#getCond <em>Cond</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.SomeValuesFrom#getClassName <em>Class Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.SomeValuesFrom#getPropertyName <em>Property Name</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getSomeValuesFrom()
 * @model
 * @generated
 */
public interface SomeValuesFrom extends Statement
{
  /**
   * Returns the value of the '<em><b>Restricted</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Restricted</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Restricted</em>' containment reference.
   * @see #setRestricted(PropertyOfClass)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSomeValuesFrom_Restricted()
   * @model containment="true"
   * @generated
   */
  PropertyOfClass getRestricted();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SomeValuesFrom#getRestricted <em>Restricted</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Restricted</em>' containment reference.
   * @see #getRestricted()
   * @generated
   */
  void setRestricted(PropertyOfClass value);

  /**
   * Returns the value of the '<em><b>Cond</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Cond</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Cond</em>' containment reference.
   * @see #setCond(SomeValuesCondition)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSomeValuesFrom_Cond()
   * @model containment="true"
   * @generated
   */
  SomeValuesCondition getCond();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SomeValuesFrom#getCond <em>Cond</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Cond</em>' containment reference.
   * @see #getCond()
   * @generated
   */
  void setCond(SomeValuesCondition value);

  /**
   * Returns the value of the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Class Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Class Name</em>' containment reference.
   * @see #setClassName(ResourceIdentifier)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSomeValuesFrom_ClassName()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getClassName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SomeValuesFrom#getClassName <em>Class Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class Name</em>' containment reference.
   * @see #getClassName()
   * @generated
   */
  void setClassName(ResourceIdentifier value);

  /**
   * Returns the value of the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Property Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Property Name</em>' containment reference.
   * @see #setPropertyName(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSomeValuesFrom_PropertyName()
   * @model containment="true"
   * @generated
   */
  ResourceByName getPropertyName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SomeValuesFrom#getPropertyName <em>Property Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Property Name</em>' containment reference.
   * @see #getPropertyName()
   * @generated
   */
  void setPropertyName(ResourceByName value);

} // SomeValuesFrom
