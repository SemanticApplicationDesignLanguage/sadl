/**
 */
package com.ge.research.sadl.mapping.impl;

import com.ge.research.sadl.mapping.MappingPackage;
import com.ge.research.sadl.mapping.NewModelNS;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>New Model NS</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.mapping.impl.NewModelNSImpl#getBaseUri <em>Base Uri</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.impl.NewModelNSImpl#getPrefix <em>Prefix</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class NewModelNSImpl extends MinimalEObjectImpl.Container implements NewModelNS
{
  /**
   * The default value of the '{@link #getBaseUri() <em>Base Uri</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getBaseUri()
   * @generated
   * @ordered
   */
  protected static final String BASE_URI_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getBaseUri() <em>Base Uri</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getBaseUri()
   * @generated
   * @ordered
   */
  protected String baseUri = BASE_URI_EDEFAULT;

  /**
   * The default value of the '{@link #getPrefix() <em>Prefix</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPrefix()
   * @generated
   * @ordered
   */
  protected static final String PREFIX_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getPrefix() <em>Prefix</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPrefix()
   * @generated
   * @ordered
   */
  protected String prefix = PREFIX_EDEFAULT;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected NewModelNSImpl()
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
    return MappingPackage.Literals.NEW_MODEL_NS;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getBaseUri()
  {
    return baseUri;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setBaseUri(String newBaseUri)
  {
    String oldBaseUri = baseUri;
    baseUri = newBaseUri;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.NEW_MODEL_NS__BASE_URI, oldBaseUri, baseUri));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getPrefix()
  {
    return prefix;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPrefix(String newPrefix)
  {
    String oldPrefix = prefix;
    prefix = newPrefix;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.NEW_MODEL_NS__PREFIX, oldPrefix, prefix));
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
      case MappingPackage.NEW_MODEL_NS__BASE_URI:
        return getBaseUri();
      case MappingPackage.NEW_MODEL_NS__PREFIX:
        return getPrefix();
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
      case MappingPackage.NEW_MODEL_NS__BASE_URI:
        setBaseUri((String)newValue);
        return;
      case MappingPackage.NEW_MODEL_NS__PREFIX:
        setPrefix((String)newValue);
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
      case MappingPackage.NEW_MODEL_NS__BASE_URI:
        setBaseUri(BASE_URI_EDEFAULT);
        return;
      case MappingPackage.NEW_MODEL_NS__PREFIX:
        setPrefix(PREFIX_EDEFAULT);
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
      case MappingPackage.NEW_MODEL_NS__BASE_URI:
        return BASE_URI_EDEFAULT == null ? baseUri != null : !BASE_URI_EDEFAULT.equals(baseUri);
      case MappingPackage.NEW_MODEL_NS__PREFIX:
        return PREFIX_EDEFAULT == null ? prefix != null : !PREFIX_EDEFAULT.equals(prefix);
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
    result.append(" (baseUri: ");
    result.append(baseUri);
    result.append(", prefix: ");
    result.append(prefix);
    result.append(')');
    return result.toString();
  }

} //NewModelNSImpl
