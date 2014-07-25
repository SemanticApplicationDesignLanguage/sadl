/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Existing Instance Attribution</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution#getSubj <em>Subj</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution#getAddlInfoItems <em>Addl Info Items</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution#getPOfS <em>POf S</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution#getObj <em>Obj</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getExistingInstanceAttribution()
 * @model
 * @generated
 */
public interface ExistingInstanceAttribution extends Statement
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
   * @see #setSubj(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExistingInstanceAttribution_Subj()
   * @model containment="true"
   * @generated
   */
  ResourceByName getSubj();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution#getSubj <em>Subj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Subj</em>' containment reference.
   * @see #getSubj()
   * @generated
   */
  void setSubj(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Addl Info Items</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.PropValPartialTriple}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Addl Info Items</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Addl Info Items</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getExistingInstanceAttribution_AddlInfoItems()
   * @model containment="true"
   * @generated
   */
  EList<PropValPartialTriple> getAddlInfoItems();

  /**
   * Returns the value of the '<em><b>POf S</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>POf S</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>POf S</em>' containment reference.
   * @see #setPOfS(OfPatternReturningValues)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExistingInstanceAttribution_POfS()
   * @model containment="true"
   * @generated
   */
  OfPatternReturningValues getPOfS();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution#getPOfS <em>POf S</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>POf S</em>' containment reference.
   * @see #getPOfS()
   * @generated
   */
  void setPOfS(OfPatternReturningValues value);

  /**
   * Returns the value of the '<em><b>Obj</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Obj</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Obj</em>' containment reference.
   * @see #setObj(EObject)
   * @see com.ge.research.sadl.sadl.SadlPackage#getExistingInstanceAttribution_Obj()
   * @model containment="true"
   * @generated
   */
  EObject getObj();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ExistingInstanceAttribution#getObj <em>Obj</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Obj</em>' containment reference.
   * @see #getObj()
   * @generated
   */
  void setObj(EObject value);

} // ExistingInstanceAttribution
