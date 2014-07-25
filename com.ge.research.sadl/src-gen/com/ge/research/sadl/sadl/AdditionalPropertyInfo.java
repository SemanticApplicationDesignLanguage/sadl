/**
 */
package com.ge.research.sadl.sadl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Additional Property Info</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getDomain <em>Domain</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getCond <em>Cond</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getRange <em>Range</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsfunc <em>Isfunc</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsinvfunc <em>Isinvfunc</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsSym <em>Is Sym</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsTrans <em>Is Trans</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsInvOf <em>Is Inv Of</em>}</li>
 * </ul>
 * </p>
 *
 * @see com.ge.research.sadl.sadl.SadlPackage#getAdditionalPropertyInfo()
 * @model
 * @generated
 */
public interface AdditionalPropertyInfo extends EObject
{
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
   * @see com.ge.research.sadl.sadl.SadlPackage#getAdditionalPropertyInfo_Domain()
   * @model containment="true"
   * @generated
   */
  ResourceIdentifier getDomain();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getDomain <em>Domain</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Domain</em>' containment reference.
   * @see #getDomain()
   * @generated
   */
  void setDomain(ResourceIdentifier value);

  /**
   * Returns the value of the '<em><b>Cond</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Cond</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Cond</em>' containment reference.
   * @see #setCond(Condition)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAdditionalPropertyInfo_Cond()
   * @model containment="true"
   * @generated
   */
  Condition getCond();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getCond <em>Cond</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Cond</em>' containment reference.
   * @see #getCond()
   * @generated
   */
  void setCond(Condition value);

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
   * @see com.ge.research.sadl.sadl.SadlPackage#getAdditionalPropertyInfo_Range()
   * @model containment="true"
   * @generated
   */
  Range getRange();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getRange <em>Range</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Range</em>' containment reference.
   * @see #getRange()
   * @generated
   */
  void setRange(Range value);

  /**
   * Returns the value of the '<em><b>Isfunc</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Isfunc</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Isfunc</em>' attribute.
   * @see #setIsfunc(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAdditionalPropertyInfo_Isfunc()
   * @model
   * @generated
   */
  String getIsfunc();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsfunc <em>Isfunc</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Isfunc</em>' attribute.
   * @see #getIsfunc()
   * @generated
   */
  void setIsfunc(String value);

  /**
   * Returns the value of the '<em><b>Isinvfunc</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Isinvfunc</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Isinvfunc</em>' attribute.
   * @see #setIsinvfunc(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAdditionalPropertyInfo_Isinvfunc()
   * @model
   * @generated
   */
  String getIsinvfunc();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsinvfunc <em>Isinvfunc</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Isinvfunc</em>' attribute.
   * @see #getIsinvfunc()
   * @generated
   */
  void setIsinvfunc(String value);

  /**
   * Returns the value of the '<em><b>Is Sym</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Is Sym</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Is Sym</em>' attribute.
   * @see #setIsSym(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAdditionalPropertyInfo_IsSym()
   * @model
   * @generated
   */
  String getIsSym();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsSym <em>Is Sym</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Is Sym</em>' attribute.
   * @see #getIsSym()
   * @generated
   */
  void setIsSym(String value);

  /**
   * Returns the value of the '<em><b>Is Trans</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Is Trans</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Is Trans</em>' attribute.
   * @see #setIsTrans(String)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAdditionalPropertyInfo_IsTrans()
   * @model
   * @generated
   */
  String getIsTrans();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsTrans <em>Is Trans</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Is Trans</em>' attribute.
   * @see #getIsTrans()
   * @generated
   */
  void setIsTrans(String value);

  /**
   * Returns the value of the '<em><b>Is Inv Of</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Is Inv Of</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Is Inv Of</em>' containment reference.
   * @see #setIsInvOf(IsInverseOf)
   * @see com.ge.research.sadl.sadl.SadlPackage#getAdditionalPropertyInfo_IsInvOf()
   * @model containment="true"
   * @generated
   */
  IsInverseOf getIsInvOf();

  /**
   * Sets the value of the '{@link com.ge.research.sadl.sadl.AdditionalPropertyInfo#getIsInvOf <em>Is Inv Of</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Is Inv Of</em>' containment reference.
   * @see #getIsInvOf()
   * @generated
   */
  void setIsInvOf(IsInverseOf value);

} // AdditionalPropertyInfo
