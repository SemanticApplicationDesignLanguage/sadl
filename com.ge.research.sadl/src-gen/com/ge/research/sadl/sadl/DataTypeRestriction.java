/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Data Type Restriction</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.DataTypeRestriction#getBasetype <em>Basetype</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.DataTypeRestriction#getFacets <em>Facets</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.DataTypeRestriction#getBasetypes <em>Basetypes</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getDataTypeRestriction()
 * @model
 * @generated
 */
public interface DataTypeRestriction extends EObject
{
  /**
   * Returns the value of the '<em><b>Basetype</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Basetype</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Basetype</em>' attribute.
   * @see #setBasetype(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getDataTypeRestriction_Basetype()
   * @model
   * @generated
   */
  String getBasetype();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.DataTypeRestriction#getBasetype <em>Basetype</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Basetype</em>' attribute.
   * @see #getBasetype()
   * @generated
   */
  void setBasetype(String value);

  /**
   * Returns the value of the '<em><b>Facets</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Facets</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Facets</em>' containment reference.
   * @see #setFacets(Facets)
   * @see com.ge.research.sadl.sadl.SadlPackage#getDataTypeRestriction_Facets()
   * @model containment="true"
   * @generated
   */
  Facets getFacets();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.DataTypeRestriction#getFacets <em>Facets</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Facets</em>' containment reference.
   * @see #getFacets()
   * @generated
   */
  void setFacets(Facets value);

  /**
   * Returns the value of the '<em><b>Basetypes</b></em>' attribute list.
   * The list contents are of type {@link java.lang.String}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Basetypes</em>' attribute list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Basetypes</em>' attribute list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getDataTypeRestriction_Basetypes()
   * @model unique="false"
   * @generated
   */
  EList<String> getBasetypes();

} // DataTypeRestriction
