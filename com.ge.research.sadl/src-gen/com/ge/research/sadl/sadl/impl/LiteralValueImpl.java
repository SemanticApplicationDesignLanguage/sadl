/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.LiteralValue;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Literal Value</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.LiteralValueImpl#getLiteralNumber <em>Literal Number</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.LiteralValueImpl#getLiteralString <em>Literal String</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.LiteralValueImpl#getLiteralBoolean <em>Literal Boolean</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class LiteralValueImpl extends MinimalEObjectImpl.Container implements LiteralValue
{
  /**
   * The default value of the '{@link #getLiteralNumber() <em>Literal Number</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLiteralNumber()
   * @generated
   * @ordered
   */
  protected static final String LITERAL_NUMBER_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getLiteralNumber() <em>Literal Number</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLiteralNumber()
   * @generated
   * @ordered
   */
  protected String literalNumber = LITERAL_NUMBER_EDEFAULT;

  /**
   * The default value of the '{@link #getLiteralString() <em>Literal String</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLiteralString()
   * @generated
   * @ordered
   */
  protected static final String LITERAL_STRING_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getLiteralString() <em>Literal String</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLiteralString()
   * @generated
   * @ordered
   */
  protected String literalString = LITERAL_STRING_EDEFAULT;

  /**
   * The default value of the '{@link #getLiteralBoolean() <em>Literal Boolean</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLiteralBoolean()
   * @generated
   * @ordered
   */
  protected static final String LITERAL_BOOLEAN_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getLiteralBoolean() <em>Literal Boolean</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLiteralBoolean()
   * @generated
   * @ordered
   */
  protected String literalBoolean = LITERAL_BOOLEAN_EDEFAULT;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected LiteralValueImpl()
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
    return SadlPackage.Literals.LITERAL_VALUE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getLiteralNumber()
  {
    return literalNumber;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setLiteralNumber(String newLiteralNumber)
  {
    String oldLiteralNumber = literalNumber;
    literalNumber = newLiteralNumber;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.LITERAL_VALUE__LITERAL_NUMBER, oldLiteralNumber, literalNumber));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getLiteralString()
  {
    return literalString;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setLiteralString(String newLiteralString)
  {
    String oldLiteralString = literalString;
    literalString = newLiteralString;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.LITERAL_VALUE__LITERAL_STRING, oldLiteralString, literalString));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getLiteralBoolean()
  {
    return literalBoolean;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setLiteralBoolean(String newLiteralBoolean)
  {
    String oldLiteralBoolean = literalBoolean;
    literalBoolean = newLiteralBoolean;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.LITERAL_VALUE__LITERAL_BOOLEAN, oldLiteralBoolean, literalBoolean));
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
      case SadlPackage.LITERAL_VALUE__LITERAL_NUMBER:
        return getLiteralNumber();
      case SadlPackage.LITERAL_VALUE__LITERAL_STRING:
        return getLiteralString();
      case SadlPackage.LITERAL_VALUE__LITERAL_BOOLEAN:
        return getLiteralBoolean();
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
      case SadlPackage.LITERAL_VALUE__LITERAL_NUMBER:
        setLiteralNumber((String)newValue);
        return;
      case SadlPackage.LITERAL_VALUE__LITERAL_STRING:
        setLiteralString((String)newValue);
        return;
      case SadlPackage.LITERAL_VALUE__LITERAL_BOOLEAN:
        setLiteralBoolean((String)newValue);
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
      case SadlPackage.LITERAL_VALUE__LITERAL_NUMBER:
        setLiteralNumber(LITERAL_NUMBER_EDEFAULT);
        return;
      case SadlPackage.LITERAL_VALUE__LITERAL_STRING:
        setLiteralString(LITERAL_STRING_EDEFAULT);
        return;
      case SadlPackage.LITERAL_VALUE__LITERAL_BOOLEAN:
        setLiteralBoolean(LITERAL_BOOLEAN_EDEFAULT);
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
      case SadlPackage.LITERAL_VALUE__LITERAL_NUMBER:
        return LITERAL_NUMBER_EDEFAULT == null ? literalNumber != null : !LITERAL_NUMBER_EDEFAULT.equals(literalNumber);
      case SadlPackage.LITERAL_VALUE__LITERAL_STRING:
        return LITERAL_STRING_EDEFAULT == null ? literalString != null : !LITERAL_STRING_EDEFAULT.equals(literalString);
      case SadlPackage.LITERAL_VALUE__LITERAL_BOOLEAN:
        return LITERAL_BOOLEAN_EDEFAULT == null ? literalBoolean != null : !LITERAL_BOOLEAN_EDEFAULT.equals(literalBoolean);
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
    result.append(" (literalNumber: ");
    result.append(literalNumber);
    result.append(", literalString: ");
    result.append(literalString);
    result.append(", literalBoolean: ");
    result.append(literalBoolean);
    result.append(')');
    return result.toString();
  }

} //LiteralValueImpl
