/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Property Declaration</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.PropertyDeclaration#getNoDomainPropName <em>No Domain Prop Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropertyDeclaration#getRange <em>Range</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropertyDeclaration#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropertyDeclaration#getSuperPropName <em>Super Prop Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropertyDeclaration#getAddlPropInfo <em>Addl Prop Info</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropertyDeclaration#getArticle <em>Article</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropertyDeclaration#getDomain <em>Domain</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropertyDeclaration#getRangeResource <em>Range Resource</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.PropertyDeclaration#getAnnotationProperty <em>Annotation Property</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyDeclaration()
 * @model
 * @generated
 */
public interface PropertyDeclaration extends Statement
{
  /**
   * Returns the value of the '<em><b>No Domain Prop Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>No Domain Prop Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>No Domain Prop Name</em>' containment reference.
   * @see #setNoDomainPropName(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyDeclaration_NoDomainPropName()
   * @model containment="true"
   * @generated
   */
  ResourceName getNoDomainPropName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getNoDomainPropName <em>No Domain Prop Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>No Domain Prop Name</em>' containment reference.
   * @see #getNoDomainPropName()
   * @generated
   */
  void setNoDomainPropName(ResourceName value);

  /**
   * Returns the value of the '<em><b>Range</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Range</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Range</em>' containment reference.
   * @see #setRange(Range)
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyDeclaration_Range()
   * @model containment="true"
   * @generated
   */
  Range getRange();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getRange <em>Range</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Range</em>' containment reference.
   * @see #getRange()
   * @generated
   */
  void setRange(Range value);

  /**
   * Returns the value of the '<em><b>Property Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Property Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Property Name</em>' containment reference.
   * @see #setPropertyName(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyDeclaration_PropertyName()
   * @model containment="true"
   * @generated
   */
  ResourceName getPropertyName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getPropertyName <em>Property Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Property Name</em>' containment reference.
   * @see #getPropertyName()
   * @generated
   */
  void setPropertyName(ResourceName value);

  /**
   * Returns the value of the '<em><b>Super Prop Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Super Prop Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Super Prop Name</em>' containment reference.
   * @see #setSuperPropName(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyDeclaration_SuperPropName()
   * @model containment="true"
   * @generated
   */
  ResourceByName getSuperPropName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getSuperPropName <em>Super Prop Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Super Prop Name</em>' containment reference.
   * @see #getSuperPropName()
   * @generated
   */
  void setSuperPropName(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Addl Prop Info</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.AdditionalPropertyInfo}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Addl Prop Info</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Addl Prop Info</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyDeclaration_AddlPropInfo()
   * @model containment="true"
   * @generated
   */
  EList<AdditionalPropertyInfo> getAddlPropInfo();

  /**
   * Returns the value of the '<em><b>Article</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Article</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Article</em>' attribute.
   * @see #setArticle(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyDeclaration_Article()
   * @model
   * @generated
   */
  String getArticle();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getArticle <em>Article</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Article</em>' attribute.
   * @see #getArticle()
   * @generated
   */
  void setArticle(String value);

  /**
   * Returns the value of the '<em><b>Domain</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Domain</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Domain</em>' containment reference.
   * @see #setDomain(ResourceIdentifier)
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyDeclaration_Domain()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getDomain();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getDomain <em>Domain</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Domain</em>' containment reference.
   * @see #getDomain()
   * @generated
   */
  void setDomain(ResourceIdentifier value);

  /**
   * Returns the value of the '<em><b>Range Resource</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Range Resource</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Range Resource</em>' containment reference.
   * @see #setRangeResource(ResourceIdentifier)
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyDeclaration_RangeResource()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getRangeResource();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getRangeResource <em>Range Resource</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Range Resource</em>' containment reference.
   * @see #getRangeResource()
   * @generated
   */
  void setRangeResource(ResourceIdentifier value);

  /**
   * Returns the value of the '<em><b>Annotation Property</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Annotation Property</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Annotation Property</em>' containment reference.
   * @see #setAnnotationProperty(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getPropertyDeclaration_AnnotationProperty()
   * @model containment="true"
   * @generated
   */
  ResourceName getAnnotationProperty();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.PropertyDeclaration#getAnnotationProperty <em>Annotation Property</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Annotation Property</em>' containment reference.
   * @see #getAnnotationProperty()
   * @generated
   */
  void setAnnotationProperty(ResourceName value);

} // PropertyDeclaration
