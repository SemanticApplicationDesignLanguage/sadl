/**
 */
package com.ge.research.sadl.sadl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Disjoint Classes</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.DisjointClasses#getClass1 <em>Class1</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.DisjointClasses#getClass2 <em>Class2</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.DisjointClasses#getClasses <em>Classes</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getDisjointClasses()
 * @model
 * @generated
 */
public interface DisjointClasses extends Statement
{
  /**
   * Returns the value of the '<em><b>Class1</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Class1</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Class1</em>' containment reference.
   * @see #setClass1(ResourceByName)
   * @see com.ge.research.sadl.sadl.SadlPackage#getDisjointClasses_Class1()
   * @model containment="true"
   * @generated
   */
  ResourceByName getClass1();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.DisjointClasses#getClass1 <em>Class1</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class1</em>' containment reference.
   * @see #getClass1()
   * @generated
   */
  void setClass1(ResourceByName value);

  /**
   * Returns the value of the '<em><b>Class2</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Class2</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Class2</em>' containment reference.
   * @see #setClass2(ResourceIdentifier)
   * @see com.ge.research.sadl.sadl.SadlPackage#getDisjointClasses_Class2()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getClass2();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.DisjointClasses#getClass2 <em>Class2</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Class2</em>' containment reference.
   * @see #getClass2()
   * @generated
   */
  void setClass2(ResourceIdentifier value);

  /**
   * Returns the value of the '<em><b>Classes</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Classes</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Classes</em>' containment reference.
   * @see #setClasses(ExistingResourceList)
   * @see com.ge.research.sadl.sadl.SadlPackage#getDisjointClasses_Classes()
   * @model containment="true"
   * @generated
   */
  ExistingResourceList getClasses();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.DisjointClasses#getClasses <em>Classes</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Classes</em>' containment reference.
   * @see #getClasses()
   * @generated
   */
  void setClasses(ExistingResourceList value);

} // DisjointClasses
