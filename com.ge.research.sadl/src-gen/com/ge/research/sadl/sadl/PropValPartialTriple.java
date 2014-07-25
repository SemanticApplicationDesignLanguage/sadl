/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Prop Val Partial Triple</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.PropValPartialTriple#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropValPartialTriple#getObjectValue <em>Object Value</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropValPartialTriple#getObjectValueBNode <em>Object Value BNode</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getPropValPartialTriple()
 * @model
 * @generated
 */
public interface PropValPartialTriple extends EObject
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropValPartialTriple_PropertyName()
   * @model containment="true"
   * @generated
   */
  ResourceByName getPropertyName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropValPartialTriple#getPropertyName <em>Property Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Property Name</em>' containment reference.
   * @see #getPropertyName()
   * @generated
   */
  void setPropertyName(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Object Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Object Value</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Object Value</em>' containment reference.
   * @see #setObjectValue(ExplicitValue)
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropValPartialTriple_ObjectValue()
   * @model containment="true"
   * @generated
   */
  ExplicitValue getObjectValue();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropValPartialTriple#getObjectValue <em>Object Value</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Object Value</em>' containment reference.
   * @see #getObjectValue()
   * @generated
   */
  void setObjectValue(ExplicitValue value);

  /**
   * Returns the value of the '<em><b>Object Value BNode</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Object Value BNode</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Object Value BNode</em>' containment reference.
   * @see #setObjectValueBNode(InstanceDeclaration)
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropValPartialTriple_ObjectValueBNode()
   * @model containment="true"
   * @generated
   */
  InstanceDeclaration getObjectValueBNode();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropValPartialTriple#getObjectValueBNode <em>Object Value BNode</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Object Value BNode</em>' containment reference.
   * @see #getObjectValueBNode()
   * @generated
   */
  void setObjectValueBNode(InstanceDeclaration value);

} // PropValPartialTriple
