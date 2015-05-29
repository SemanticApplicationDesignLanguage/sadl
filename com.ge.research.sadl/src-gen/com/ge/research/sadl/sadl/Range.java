/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Range</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.Range#getSingle <em>Single</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Range#getList <em>List</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Range#getLists <em>Lists</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.Range#getType <em>Type</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getRange()
 * @model
 * @generated
 */
public interface Range extends EObject
{
  /**
   * Returns the value of the '<em><b>Single</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Single</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Single</em>' attribute.
   * @see #setSingle(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getRange_Single()
   * @model
   * @generated
   */
  String getSingle();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Range#getSingle <em>Single</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Single</em>' attribute.
   * @see #getSingle()
   * @generated
   */
  void setSingle(String value);

  /**
   * Returns the value of the '<em><b>List</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>List</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>List</em>' attribute.
   * @see #setList(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getRange_List()
   * @model
   * @generated
   */
  String getList();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Range#getList <em>List</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>List</em>' attribute.
   * @see #getList()
   * @generated
   */
  void setList(String value);

  /**
   * Returns the value of the '<em><b>Lists</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Lists</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Lists</em>' attribute.
   * @see #setLists(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getRange_Lists()
   * @model
   * @generated
   */
  String getLists();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Range#getLists <em>Lists</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Lists</em>' attribute.
   * @see #getLists()
   * @generated
   */
  void setLists(String value);

  /**
   * Returns the value of the '<em><b>Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Type</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Type</em>' containment reference.
   * @see #setType(RangeType)
   * @see com.ge.research.sadl.sadl.SadlPackage#getRange_Type()
   * @model containment="true"
   * @generated
   */
  RangeType getType();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.Range#getType <em>Type</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Type</em>' containment reference.
   * @see #getType()
   * @generated
   */
  void setType(RangeType value);

} // Range
