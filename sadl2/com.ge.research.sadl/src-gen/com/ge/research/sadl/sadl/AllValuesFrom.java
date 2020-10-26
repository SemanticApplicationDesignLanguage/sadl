/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>All Values From</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.AllValuesFrom#getRestricted <em>Restricted</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AllValuesFrom#getCond <em>Cond</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AllValuesFrom#getClassName <em>Class Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AllValuesFrom#getPropertyName <em>Property Name</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getAllValuesFrom()
 * @model
 * @generated
 */
public interface AllValuesFrom extends Statement
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getAllValuesFrom_Restricted()
   * @model containment="true"
   * @generated
   */
  PropertyOfClass getRestricted();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AllValuesFrom#getRestricted <em>Restricted</em>}' containment reference.
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
   * @see #setCond(AllValuesCondition)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAllValuesFrom_Cond()
   * @model containment="true"
   * @generated
   */
  AllValuesCondition getCond();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AllValuesFrom#getCond <em>Cond</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Cond</em>' containment reference.
   * @see #getCond()
   * @generated
   */
  void setCond(AllValuesCondition value);

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
   * @see com.ge.research.sadl.sadl.SadlPackage#getAllValuesFrom_ClassName()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getClassName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AllValuesFrom#getClassName <em>Class Name</em>}' containment reference.
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getAllValuesFrom_PropertyName()
   * @model containment="true"
   * @generated
   */
  ResourceByName getPropertyName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AllValuesFrom#getPropertyName <em>Property Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Property Name</em>' containment reference.
   * @see #getPropertyName()
   * @generated
   */
  void setPropertyName(ResourceByName value);

} // AllValuesFrom
