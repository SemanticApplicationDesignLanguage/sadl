/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Property Of Class</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.PropertyOfClass#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropertyOfClass#getClassName <em>Class Name</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyOfClass()
 * @model
 * @generated
 */
public interface PropertyOfClass extends EObject
{
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyOfClass_PropertyName()
   * @model containment="true"
   * @generated
   */
  ResourceByName getPropertyName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropertyOfClass#getPropertyName <em>Property Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Property Name</em>' containment reference.
   * @see #getPropertyName()
   * @generated
   */
  void setPropertyName(ResourceByName value);

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
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyOfClass_ClassName()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getClassName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropertyOfClass#getClassName <em>Class Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class Name</em>' containment reference.
   * @see #getClassName()
   * @generated
   */
  void setClassName(ResourceIdentifier value);

} // PropertyOfClass
