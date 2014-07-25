/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Value Table</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.ValueTable#getRow <em>Row</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ValueTable#getRows <em>Rows</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getValueTable()
 * @model
 * @generated
 */
public interface ValueTable extends EObject
{
  /**
   * Returns the value of the '<em><b>Row</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Row</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Row</em>' containment reference.
   * @see #setRow(ValueRow)
   * @see com.ge.research.sadl.sadl.SadlPackage#getValueTable_Row()
   * @model containment="true"
   * @generated
   */
  ValueRow getRow();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ValueTable#getRow <em>Row</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Row</em>' containment reference.
   * @see #getRow()
   * @generated
   */
  void setRow(ValueRow value);

  /**
   * Returns the value of the '<em><b>Rows</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.ValueRow}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Rows</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Rows</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getValueTable_Rows()
   * @model containment="true"
   * @generated
   */
  EList<ValueRow> getRows();

} // ValueTable
