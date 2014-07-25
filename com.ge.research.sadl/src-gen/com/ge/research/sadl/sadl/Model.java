/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Model</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.Model#getModelName <em>Model Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Model#getImports <em>Imports</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Model#getElements <em>Elements</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getModel()
 * @model
 * @generated
 */
public interface Model extends EObject
{
  /**
   * Returns the value of the '<em><b>Model Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Model Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Model Name</em>' containment reference.
   * @see #setModelName(ModelName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getModel_ModelName()
   * @model containment="true"
   * @generated
   */
  ModelName getModelName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Model#getModelName <em>Model Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Model Name</em>' containment reference.
   * @see #getModelName()
   * @generated
   */
  void setModelName(ModelName value);

  /**
   * Returns the value of the '<em><b>Imports</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.Import}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Imports</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Imports</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getModel_Imports()
   * @model containment="true"
   * @generated
   */
  EList<Import> getImports();

  /**
   * Returns the value of the '<em><b>Elements</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.ModelElement}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Elements</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Elements</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getModel_Elements()
   * @model containment="true"
   * @generated
   */
  EList<ModelElement> getElements();

} // Model
