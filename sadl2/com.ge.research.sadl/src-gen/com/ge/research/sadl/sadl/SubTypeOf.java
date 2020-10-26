/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Sub Type Of</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.SubTypeOf#getSubclass <em>Subclass</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.SubTypeOf#getSuperclass <em>Superclass</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getSubTypeOf()
 * @model
 * @generated
 */
public interface SubTypeOf extends GraphPattern
{
  /**
   * Returns the value of the '<em><b>Subclass</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Subclass</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Subclass</em>' containment reference.
   * @see #setSubclass(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSubTypeOf_Subclass()
   * @model containment="true"
   * @generated
   */
  ResourceByName getSubclass();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SubTypeOf#getSubclass <em>Subclass</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Subclass</em>' containment reference.
   * @see #getSubclass()
   * @generated
   */
  void setSubclass(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Superclass</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Superclass</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Superclass</em>' containment reference.
   * @see #setSuperclass(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getSubTypeOf_Superclass()
   * @model containment="true"
   * @generated
   */
  ResourceByName getSuperclass();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.SubTypeOf#getSuperclass <em>Superclass</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Superclass</em>' containment reference.
   * @see #getSuperclass()
   * @generated
   */
  void setSuperclass(ResourceByName value);

} // SubTypeOf
