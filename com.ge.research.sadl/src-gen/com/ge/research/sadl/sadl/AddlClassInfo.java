/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Addl Class Info</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.AddlClassInfo#getPropertyByName <em>Property By Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AddlClassInfo#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AddlClassInfo#getRange <em>Range</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AddlClassInfo#getRestriction <em>Restriction</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getAddlClassInfo()
 * @model
 * @generated
 */
public interface AddlClassInfo extends EObject
{
  /**
   * Returns the value of the '<em><b>Property By Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Property By Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Property By Name</em>' containment reference.
   * @see #setPropertyByName(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAddlClassInfo_PropertyByName()
   * @model containment="true"
   * @generated
   */
  ResourceByName getPropertyByName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AddlClassInfo#getPropertyByName <em>Property By Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Property By Name</em>' containment reference.
   * @see #getPropertyByName()
   * @generated
   */
  void setPropertyByName(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Property Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Property Name</em>' containment reference.
   * @see #setPropertyName(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAddlClassInfo_PropertyName()
   * @model containment="true"
   * @generated
   */
  ResourceName getPropertyName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AddlClassInfo#getPropertyName <em>Property Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Property Name</em>' containment reference.
   * @see #getPropertyName()
   * @generated
   */
  void setPropertyName(ResourceName value);

  /**
   * Returns the value of the '<em><b>Range</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Range</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Range</em>' containment reference.
   * @see #setRange(Range)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAddlClassInfo_Range()
   * @model containment="true"
   * @generated
   */
  Range getRange();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AddlClassInfo#getRange <em>Range</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Range</em>' containment reference.
   * @see #getRange()
   * @generated
   */
  void setRange(Range value);

  /**
   * Returns the value of the '<em><b>Restriction</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Restriction</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Restriction</em>' containment reference.
   * @see #setRestriction(Condition)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAddlClassInfo_Restriction()
   * @model containment="true"
   * @generated
   */
  Condition getRestriction();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AddlClassInfo#getRestriction <em>Restriction</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Restriction</em>' containment reference.
   * @see #getRestriction()
   * @generated
   */
  void setRestriction(Condition value);

} // AddlClassInfo
