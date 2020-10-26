/**
 */
package com.ge.research.sadl.mapping;

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
 *   <li>{@link com.ge.research.sadl.mapping.Model#getUri <em>Uri</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.Model#getImports <em>Imports</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.Model#getTriples <em>Triples</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.mapping.MappingPackage#getModel()
 * @model
 * @generated
 */
public interface Model extends EObject
{
  /**
   * Returns the value of the '<em><b>Uri</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Uri</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Uri</em>' containment reference.
   * @see #setUri(NewModelNS)
   * @see com.ge.research.sadl.mapping.MappingPackage#getModel_Uri()
   * @model containment="true"
   * @generated
   */
  NewModelNS getUri();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.mapping.Model#getUri <em>Uri</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Uri</em>' containment reference.
   * @see #getUri()
   * @generated
   */
  void setUri(NewModelNS value);

  /**
   * Returns the value of the '<em><b>Imports</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.mapping.Import}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Imports</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Imports</em>' containment reference list.
   * @see com.ge.research.sadl.mapping.MappingPackage#getModel_Imports()
   * @model containment="true"
   * @generated
   */
  EList<Import> getImports();

  /**
   * Returns the value of the '<em><b>Triples</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.emf.ecore.EObject}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Triples</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Triples</em>' containment reference list.
   * @see com.ge.research.sadl.mapping.MappingPackage#getModel_Triples()
   * @model containment="true"
   * @generated
   */
  EList<EObject> getTriples();

} // Model
