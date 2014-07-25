/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Class Declaration</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.ClassDeclaration#getClassName <em>Class Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ClassDeclaration#getMustBeOneOf <em>Must Be One Of</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ClassDeclaration#getDescribedBy <em>Described By</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ClassDeclaration#getClassList <em>Class List</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.ClassDeclaration#getClassIdentifier <em>Class Identifier</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getClassDeclaration()
 * @model
 * @generated
 */
public interface ClassDeclaration extends Statement
{
  /**
   * Returns the value of the '<em><b>Class Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Class Name</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Class Name</em>' containment reference.
   * @see #setClassName(ResourceName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getClassDeclaration_ClassName()
   * @model containment="true"
   * @generated
   */
  ResourceName getClassName();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ClassDeclaration#getClassName <em>Class Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class Name</em>' containment reference.
   * @see #getClassName()
   * @generated
   */
  void setClassName(ResourceName value);

  /**
   * Returns the value of the '<em><b>Must Be One Of</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Must Be One Of</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Must Be One Of</em>' containment reference.
   * @see #setMustBeOneOf(EnumeratedInstances)
   * @see com.ge.research.sadl.sadl.SadlPackage#getClassDeclaration_MustBeOneOf()
   * @model containment="true"
   * @generated
   */
  EnumeratedInstances getMustBeOneOf();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ClassDeclaration#getMustBeOneOf <em>Must Be One Of</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Must Be One Of</em>' containment reference.
   * @see #getMustBeOneOf()
   * @generated
   */
  void setMustBeOneOf(EnumeratedInstances value);

  /**
   * Returns the value of the '<em><b>Described By</b></em>' containment reference list.
   * The list contents are of type {@link com.ge.research.sadl.sadl.AddlClassInfo}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Described By</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Described By</em>' containment reference list.
   * @see com.ge.research.sadl.sadl.SadlPackage#getClassDeclaration_DescribedBy()
   * @model containment="true"
   * @generated
   */
  EList<AddlClassInfo> getDescribedBy();

  /**
   * Returns the value of the '<em><b>Class List</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Class List</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Class List</em>' containment reference.
   * @see #setClassList(ResourceList)
   * @see com.ge.research.sadl.sadl.SadlPackage#getClassDeclaration_ClassList()
   * @model containment="true"
   * @generated
   */
  ResourceList getClassList();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ClassDeclaration#getClassList <em>Class List</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class List</em>' containment reference.
   * @see #getClassList()
   * @generated
   */
  void setClassList(ResourceList value);

  /**
   * Returns the value of the '<em><b>Class Identifier</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Class Identifier</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Class Identifier</em>' containment reference.
   * @see #setClassIdentifier(ResourceIdentifier)
   * @see com.ge.research.sadl.sadl.SadlPackage#getClassDeclaration_ClassIdentifier()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getClassIdentifier();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.ClassDeclaration#getClassIdentifier <em>Class Identifier</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class Identifier</em>' containment reference.
   * @see #getClassIdentifier()
   * @generated
   */
  void setClassIdentifier(ResourceIdentifier value);

} // ClassDeclaration
