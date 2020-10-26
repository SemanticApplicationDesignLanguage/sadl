/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.MinCardCondition;
import com.ge.research.sadl.sadl.ResourceIdentifier;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Min Card Condition</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.MinCardConditionImpl#getCard <em>Card</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.MinCardConditionImpl#getClassQualifier <em>Class Qualifier</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class MinCardConditionImpl extends ConditionImpl implements MinCardCondition
{
  /**
   * The default value of the '{@link #getCard() <em>Card</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getCard()
   * @generated
   * @ordered
   */
  protected static final String CARD_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getCard() <em>Card</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getCard()
   * @generated
   * @ordered
   */
  protected String card = CARD_EDEFAULT;

  /**
   * The cached value of the '{@link #getClassQualifier() <em>Class Qualifier</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getClassQualifier()
   * @generated
   * @ordered
   */
  protected ResourceIdentifier classQualifier;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected MinCardConditionImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return SadlPackage.Literals.MIN_CARD_CONDITION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getCard()
  {
    return card;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setCard(String newCard)
  {
    String oldCard = card;
    card = newCard;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.MIN_CARD_CONDITION__CARD, oldCard, card));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceIdentifier getClassQualifier()
  {
    return classQualifier;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetClassQualifier(ResourceIdentifier newClassQualifier, NotificationChain msgs)
  {
    ResourceIdentifier oldClassQualifier = classQualifier;
    classQualifier = newClassQualifier;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.MIN_CARD_CONDITION__CLASS_QUALIFIER, oldClassQualifier, newClassQualifier);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setClassQualifier(ResourceIdentifier newClassQualifier)
  {
    if (newClassQualifier != classQualifier)
    {
      NotificationChain msgs = null;
      if (classQualifier != null)
        msgs = ((InternalEObject)classQualifier).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.MIN_CARD_CONDITION__CLASS_QUALIFIER, null, msgs);
      if (newClassQualifier != null)
        msgs = ((InternalEObject)newClassQualifier).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.MIN_CARD_CONDITION__CLASS_QUALIFIER, null, msgs);
      msgs = basicSetClassQualifier(newClassQualifier, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.MIN_CARD_CONDITION__CLASS_QUALIFIER, newClassQualifier, newClassQualifier));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
  {
    switch (featureID)
    {
      case SadlPackage.MIN_CARD_CONDITION__CLASS_QUALIFIER:
        return basicSetClassQualifier(null, msgs);
    }
    return super.eInverseRemove(otherEnd, featureID, msgs);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case SadlPackage.MIN_CARD_CONDITION__CARD:
        return getCard();
      case SadlPackage.MIN_CARD_CONDITION__CLASS_QUALIFIER:
        return getClassQualifier();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SadlPackage.MIN_CARD_CONDITION__CARD:
        setCard((String)newValue);
        return;
      case SadlPackage.MIN_CARD_CONDITION__CLASS_QUALIFIER:
        setClassQualifier((ResourceIdentifier)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case SadlPackage.MIN_CARD_CONDITION__CARD:
        setCard(CARD_EDEFAULT);
        return;
      case SadlPackage.MIN_CARD_CONDITION__CLASS_QUALIFIER:
        setClassQualifier((ResourceIdentifier)null);
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case SadlPackage.MIN_CARD_CONDITION__CARD:
        return CARD_EDEFAULT == null ? card != null : !CARD_EDEFAULT.equals(card);
      case SadlPackage.MIN_CARD_CONDITION__CLASS_QUALIFIER:
        return classQualifier != null;
    }
    return super.eIsSet(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String toString()
  {
    if (eIsProxy()) return super.toString();

    StringBuffer result = new StringBuffer(super.toString());
    result.append(" (card: ");
    result.append(card);
    result.append(')');
    return result.toString();
  }

} //MinCardConditionImpl
