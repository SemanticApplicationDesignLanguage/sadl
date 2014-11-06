/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Model Name</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.ModelName#getBaseUri <em>Base Uri</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ModelName#getAlias <em>Alias</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ModelName#getVersion <em>Version</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ModelName#getAnnContent <em>Ann Content</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getModelName()
 * @model
 * @generated
 */
public interface ModelName extends EObject
{
  /**
   * Returns the value of the '<em><b>Base Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Base Uri</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Base Uri</em>' attribute.
   * @see #setBaseUri(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getModelName_BaseUri()
   * @model
   * @generated
   */
  String getBaseUri();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ModelName#getBaseUri <em>Base Uri</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Base Uri</em>' attribute.
   * @see #getBaseUri()
   * @generated
   */
  void setBaseUri(String value);

  /**
   * Returns the value of the '<em><b>Alias</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Alias</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Alias</em>' attribute.
   * @see #setAlias(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getModelName_Alias()
   * @model
   * @generated
   */
  String getAlias();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ModelName#getAlias <em>Alias</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Alias</em>' attribute.
   * @see #getAlias()
   * @generated
   */
  void setAlias(String value);

  /**
   * Returns the value of the '<em><b>Version</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Version</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Version</em>' attribute.
   * @see #setVersion(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getModelName_Version()
   * @model
   * @generated
   */
  String getVersion();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ModelName#getVersion <em>Version</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Version</em>' attribute.
   * @see #getVersion()
   * @generated
   */
  void setVersion(String value);

  /**
   * Returns the value of the '<em><b>Ann Content</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.ContentList}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Ann Content</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Ann Content</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getModelName_AnnContent()
   * @model containment="true"
   * @generated
   */
  EList<ContentList> getAnnContent();

} // ModelName
