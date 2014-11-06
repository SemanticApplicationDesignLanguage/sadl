/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ContentList;
import com.ge.research.sadl.sadl.ModelName;
import com.ge.research.sadl.sadl.SadlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Model Name</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ModelNameImpl#getBaseUri <em>Base Uri</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ModelNameImpl#getAlias <em>Alias</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ModelNameImpl#getVersion <em>Version</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ModelNameImpl#getAnnContent <em>Ann Content</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ModelNameImpl extends MinimalEObjectImpl.Container implements ModelName
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
   * The default value of the '{@link #getAlias() <em>Alias</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAlias()
   * @generated
   * @ordered
   */
  protected static final String ALIAS_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getAlias() <em>Alias</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAlias()
   * @generated
   * @ordered
   */
  protected String alias = ALIAS_EDEFAULT;

  /**
   * The default value of the '{@link #getVersion() <em>Version</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getVersion()
   * @generated
   * @ordered
   */
  protected static final String VERSION_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getVersion() <em>Version</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getVersion()
   * @generated
   * @ordered
   */
  protected String version = VERSION_EDEFAULT;

  /**
   * The cached value of the '{@link #getAnnContent() <em>Ann Content</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnContent()
   * @generated
   * @ordered
   */
  protected EList<ContentList> annContent;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ModelNameImpl()
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
    return SadlPackage.Literals.MODEL_NAME;
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
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.MODEL_NAME__BASE_URI, oldBaseUri, baseUri));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getAlias()
  {
    return alias;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setAlias(String newAlias)
  {
    String oldAlias = alias;
    alias = newAlias;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.MODEL_NAME__ALIAS, oldAlias, alias));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getVersion()
  {
    return version;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setVersion(String newVersion)
  {
    String oldVersion = version;
    version = newVersion;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.MODEL_NAME__VERSION, oldVersion, version));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<ContentList> getAnnContent()
  {
    if (annContent == null)
    {
      annContent = new EObjectContainmentEList<ContentList>(ContentList.class, this, SadlPackage.MODEL_NAME__ANN_CONTENT);
    }
    return annContent;
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
      case SadlPackage.MODEL_NAME__ANN_CONTENT:
        return ((InternalEList<?>)getAnnContent()).basicRemove(otherEnd, msgs);
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
      case SadlPackage.MODEL_NAME__BASE_URI:
        return getBaseUri();
      case SadlPackage.MODEL_NAME__ALIAS:
        return getAlias();
      case SadlPackage.MODEL_NAME__VERSION:
        return getVersion();
      case SadlPackage.MODEL_NAME__ANN_CONTENT:
        return getAnnContent();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SadlPackage.MODEL_NAME__BASE_URI:
        setBaseUri((String)newValue);
        return;
      case SadlPackage.MODEL_NAME__ALIAS:
        setAlias((String)newValue);
        return;
      case SadlPackage.MODEL_NAME__VERSION:
        setVersion((String)newValue);
        return;
      case SadlPackage.MODEL_NAME__ANN_CONTENT:
        getAnnContent().clear();
        getAnnContent().addAll((Collection<? extends ContentList>)newValue);
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
      case SadlPackage.MODEL_NAME__BASE_URI:
        setBaseUri(BASE_URI_EDEFAULT);
        return;
      case SadlPackage.MODEL_NAME__ALIAS:
        setAlias(ALIAS_EDEFAULT);
        return;
      case SadlPackage.MODEL_NAME__VERSION:
        setVersion(VERSION_EDEFAULT);
        return;
      case SadlPackage.MODEL_NAME__ANN_CONTENT:
        getAnnContent().clear();
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
      case SadlPackage.MODEL_NAME__BASE_URI:
        return BASE_URI_EDEFAULT == null ? baseUri != null : !BASE_URI_EDEFAULT.equals(baseUri);
      case SadlPackage.MODEL_NAME__ALIAS:
        return ALIAS_EDEFAULT == null ? alias != null : !ALIAS_EDEFAULT.equals(alias);
      case SadlPackage.MODEL_NAME__VERSION:
        return VERSION_EDEFAULT == null ? version != null : !VERSION_EDEFAULT.equals(version);
      case SadlPackage.MODEL_NAME__ANN_CONTENT:
        return annContent != null && !annContent.isEmpty();
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
    result.append(", alias: ");
    result.append(alias);
    result.append(", version: ");
    result.append(version);
    result.append(')');
    return result.toString();
  }

} //ModelNameImpl
