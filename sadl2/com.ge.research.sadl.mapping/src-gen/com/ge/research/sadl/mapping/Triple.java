/**
 */
package com.ge.research.sadl.mapping;

import com.ge.research.sadl.sadl.ResourceName;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Triple</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.mapping.Triple#getSubj <em>Subj</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.Triple#getPred <em>Pred</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.Triple#getObjval <em>Objval</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.mapping.MappingPackage#getTriple()
 * @model
 * @generated
 */
public interface Triple extends EObject
{
  /**
   * Returns the value of the '<em><b>Subj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Subj</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Subj</em>' containment reference.
   * @see #setSubj(EObject)
   * @see com.ge.research.sadl.mapping.MappingPackage#getTriple_Subj()
   * @model containment="true"
   * @generated
   */
  EObject getSubj();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.mapping.Triple#getSubj <em>Subj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Subj</em>' containment reference.
   * @see #getSubj()
   * @generated
   */
  void setSubj(EObject value);

  /**
   * Returns the value of the '<em><b>Pred</b></em>' reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Pred</em>' reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Pred</em>' reference.
   * @see #setPred(ResourceName)
   * @see com.ge.research.sadl.mapping.MappingPackage#getTriple_Pred()
   * @model
   * @generated
   */
  ResourceName getPred();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.mapping.Triple#getPred <em>Pred</em>}' reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Pred</em>' reference.
   * @see #getPred()
   * @generated
   */
  void setPred(ResourceName value);

  /**
   * Returns the value of the '<em><b>Objval</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Objval</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Objval</em>' containment reference.
   * @see #setObjval(EObject)
   * @see com.ge.research.sadl.mapping.MappingPackage#getTriple_Objval()
   * @model containment="true"
   * @generated
   */
  EObject getObjval();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.mapping.Triple#getObjval <em>Objval</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Objval</em>' containment reference.
   * @see #getObjval()
   * @generated
   */
  void setObjval(EObject value);

} // Triple
