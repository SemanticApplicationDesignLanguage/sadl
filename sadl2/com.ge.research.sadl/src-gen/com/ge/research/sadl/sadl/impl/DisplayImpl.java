/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.Display;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Display</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.DisplayImpl#getDisplayString <em>Display String</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.DisplayImpl#getModel <em>Model</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DisplayImpl extends ModelElementImpl implements Display
{
  /**
   * The default value of the '{@link #getDisplayString() <em>Display String</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDisplayString()
   * @generated
   * @ordered
   */
  protected static final String DISPLAY_STRING_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getDisplayString() <em>Display String</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDisplayString()
   * @generated
   * @ordered
   */
  protected String displayString = DISPLAY_STRING_EDEFAULT;

  /**
   * The default value of the '{@link #getModel() <em>Model</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getModel()
   * @generated
   * @ordered
   */
  protected static final String MODEL_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getModel() <em>Model</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getModel()
   * @generated
   * @ordered
   */
  protected String model = MODEL_EDEFAULT;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected DisplayImpl()
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
    return SadlPackage.Literals.DISPLAY;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getDisplayString()
  {
    return displayString;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setDisplayString(String newDisplayString)
  {
    String oldDisplayString = displayString;
    displayString = newDisplayString;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.DISPLAY__DISPLAY_STRING, oldDisplayString, displayString));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getModel()
  {
    return model;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setModel(String newModel)
  {
    String oldModel = model;
    model = newModel;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.DISPLAY__MODEL, oldModel, model));
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
      case SadlPackage.DISPLAY__DISPLAY_STRING:
        return getDisplayString();
      case SadlPackage.DISPLAY__MODEL:
        return getModel();
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
      case SadlPackage.DISPLAY__DISPLAY_STRING:
        setDisplayString((String)newValue);
        return;
      case SadlPackage.DISPLAY__MODEL:
        setModel((String)newValue);
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
      case SadlPackage.DISPLAY__DISPLAY_STRING:
        setDisplayString(DISPLAY_STRING_EDEFAULT);
        return;
      case SadlPackage.DISPLAY__MODEL:
        setModel(MODEL_EDEFAULT);
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
      case SadlPackage.DISPLAY__DISPLAY_STRING:
        return DISPLAY_STRING_EDEFAULT == null ? displayString != null : !DISPLAY_STRING_EDEFAULT.equals(displayString);
      case SadlPackage.DISPLAY__MODEL:
        return MODEL_EDEFAULT == null ? model != null : !MODEL_EDEFAULT.equals(model);
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
    result.append(" (displayString: ");
    result.append(displayString);
    result.append(", model: ");
    result.append(model);
    result.append(')');
    return result.toString();
  }

} //DisplayImpl
