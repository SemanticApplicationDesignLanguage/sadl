/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Instance Declaration</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.InstanceDeclaration#getTypeDecl <em>Type Decl</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.InstanceDeclaration#getAddlInfoItems <em>Addl Info Items</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.InstanceDeclaration#getArticle <em>Article</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.InstanceDeclaration#getClassName <em>Class Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.InstanceDeclaration#getInstanceName <em>Instance Name</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getInstanceDeclaration()
 * @model
 * @generated
 */
public interface InstanceDeclaration extends InstanceDeclarationStatement, EmbeddedInstanceDeclaration
{
  /**
   * Returns the value of the '<em><b>Type Decl</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Type Decl</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Type Decl</em>' containment reference.
   * @see #setTypeDecl(TypeDeclaration)
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstanceDeclaration_TypeDecl()
   * @model containment="true"
   * @generated
   */
  TypeDeclaration getTypeDecl();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InstanceDeclaration#getTypeDecl <em>Type Decl</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Type Decl</em>' containment reference.
   * @see #getTypeDecl()
   * @generated
   */
  void setTypeDecl(TypeDeclaration value);

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
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstanceDeclaration_AddlInfoItems()
   * @model containment="true"
   * @generated
   */
  EList<PropValPartialTriple> getAddlInfoItems();

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
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstanceDeclaration_Article()
   * @model
   * @generated
   */
  String getArticle();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InstanceDeclaration#getArticle <em>Article</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Article</em>' attribute.
   * @see #getArticle()
   * @generated
   */
  void setArticle(String value);

  /**
   * Returns the value of the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Class Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Class Name</em>' containment reference.
   * @see #setClassName(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstanceDeclaration_ClassName()
   * @model containment="true"
   * @generated
   */
  ResourceByName getClassName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InstanceDeclaration#getClassName <em>Class Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class Name</em>' containment reference.
   * @see #getClassName()
   * @generated
   */
  void setClassName(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Instance Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Instance Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Instance Name</em>' containment reference.
   * @see #setInstanceName(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getInstanceDeclaration_InstanceName()
   * @model containment="true"
   * @generated
   */
  ResourceName getInstanceName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.InstanceDeclaration#getInstanceName <em>Instance Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Instance Name</em>' containment reference.
   * @see #getInstanceName()
   * @generated
   */
  void setInstanceName(ResourceName value);

} // InstanceDeclaration
