/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.AdditionalPropertyInfo;
import com.ge.research.sadl.sadl.PropertyDeclaration;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceIdentifier;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Property Declaration</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropertyDeclarationImpl#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropertyDeclarationImpl#getSuperPropName <em>Super Prop Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropertyDeclarationImpl#getAddlPropInfo <em>Addl Prop Info</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropertyDeclarationImpl#getArticle <em>Article</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropertyDeclarationImpl#getDomain <em>Domain</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropertyDeclarationImpl#getRangeResource <em>Range Resource</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropertyDeclarationImpl#getAnnotationProperty <em>Annotation Property</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class PropertyDeclarationImpl extends StatementImpl implements PropertyDeclaration
{
  /**
   * The cached value of the '{@link #getPropertyName() <em>Property Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPropertyName()
   * @generated
   * @ordered
   */
  protected ResourceName propertyName;

  /**
   * The cached value of the '{@link #getSuperPropName() <em>Super Prop Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getSuperPropName()
   * @generated
   * @ordered
   */
  protected ResourceByName superPropName;

  /**
   * The cached value of the '{@link #getAddlPropInfo() <em>Addl Prop Info</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAddlPropInfo()
   * @generated
   * @ordered
   */
  protected EList<AdditionalPropertyInfo> addlPropInfo;

  /**
   * The default value of the '{@link #getArticle() <em>Article</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getArticle()
   * @generated
   * @ordered
   */
  protected static final String ARTICLE_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getArticle() <em>Article</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getArticle()
   * @generated
   * @ordered
   */
  protected String article = ARTICLE_EDEFAULT;

  /**
   * The cached value of the '{@link #getDomain() <em>Domain</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDomain()
   * @generated
   * @ordered
   */
  protected ResourceIdentifier domain;

  /**
   * The cached value of the '{@link #getRangeResource() <em>Range Resource</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRangeResource()
   * @generated
   * @ordered
   */
  protected ResourceIdentifier rangeResource;

  /**
   * The cached value of the '{@link #getAnnotationProperty() <em>Annotation Property</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnotationProperty()
   * @generated
   * @ordered
   */
  protected ResourceName annotationProperty;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected PropertyDeclarationImpl()
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
    return SadlPackage.Literals.PROPERTY_DECLARATION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName getPropertyName()
  {
    return propertyName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPropertyName(ResourceName newPropertyName, NotificationChain msgs)
  {
    ResourceName oldPropertyName = propertyName;
    propertyName = newPropertyName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.PROPERTY_DECLARATION__PROPERTY_NAME, oldPropertyName, newPropertyName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPropertyName(ResourceName newPropertyName)
  {
    if (newPropertyName != propertyName)
    {
      NotificationChain msgs = null;
      if (propertyName != null)
        msgs = ((InternalEObject)propertyName).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROPERTY_DECLARATION__PROPERTY_NAME, null, msgs);
      if (newPropertyName != null)
        msgs = ((InternalEObject)newPropertyName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROPERTY_DECLARATION__PROPERTY_NAME, null, msgs);
      msgs = basicSetPropertyName(newPropertyName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.PROPERTY_DECLARATION__PROPERTY_NAME, newPropertyName, newPropertyName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getSuperPropName()
  {
    return superPropName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetSuperPropName(ResourceByName newSuperPropName, NotificationChain msgs)
  {
    ResourceByName oldSuperPropName = superPropName;
    superPropName = newSuperPropName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.PROPERTY_DECLARATION__SUPER_PROP_NAME, oldSuperPropName, newSuperPropName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setSuperPropName(ResourceByName newSuperPropName)
  {
    if (newSuperPropName != superPropName)
    {
      NotificationChain msgs = null;
      if (superPropName != null)
        msgs = ((InternalEObject)superPropName).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROPERTY_DECLARATION__SUPER_PROP_NAME, null, msgs);
      if (newSuperPropName != null)
        msgs = ((InternalEObject)newSuperPropName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROPERTY_DECLARATION__SUPER_PROP_NAME, null, msgs);
      msgs = basicSetSuperPropName(newSuperPropName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.PROPERTY_DECLARATION__SUPER_PROP_NAME, newSuperPropName, newSuperPropName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<AdditionalPropertyInfo> getAddlPropInfo()
  {
    if (addlPropInfo == null)
    {
      addlPropInfo = new EObjectContainmentEList<AdditionalPropertyInfo>(AdditionalPropertyInfo.class, this, SadlPackage.PROPERTY_DECLARATION__ADDL_PROP_INFO);
    }
    return addlPropInfo;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getArticle()
  {
    return article;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setArticle(String newArticle)
  {
    String oldArticle = article;
    article = newArticle;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.PROPERTY_DECLARATION__ARTICLE, oldArticle, article));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceIdentifier getDomain()
  {
    return domain;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetDomain(ResourceIdentifier newDomain, NotificationChain msgs)
  {
    ResourceIdentifier oldDomain = domain;
    domain = newDomain;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.PROPERTY_DECLARATION__DOMAIN, oldDomain, newDomain);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setDomain(ResourceIdentifier newDomain)
  {
    if (newDomain != domain)
    {
      NotificationChain msgs = null;
      if (domain != null)
        msgs = ((InternalEObject)domain).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROPERTY_DECLARATION__DOMAIN, null, msgs);
      if (newDomain != null)
        msgs = ((InternalEObject)newDomain).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROPERTY_DECLARATION__DOMAIN, null, msgs);
      msgs = basicSetDomain(newDomain, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.PROPERTY_DECLARATION__DOMAIN, newDomain, newDomain));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceIdentifier getRangeResource()
  {
    return rangeResource;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetRangeResource(ResourceIdentifier newRangeResource, NotificationChain msgs)
  {
    ResourceIdentifier oldRangeResource = rangeResource;
    rangeResource = newRangeResource;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.PROPERTY_DECLARATION__RANGE_RESOURCE, oldRangeResource, newRangeResource);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setRangeResource(ResourceIdentifier newRangeResource)
  {
    if (newRangeResource != rangeResource)
    {
      NotificationChain msgs = null;
      if (rangeResource != null)
        msgs = ((InternalEObject)rangeResource).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROPERTY_DECLARATION__RANGE_RESOURCE, null, msgs);
      if (newRangeResource != null)
        msgs = ((InternalEObject)newRangeResource).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROPERTY_DECLARATION__RANGE_RESOURCE, null, msgs);
      msgs = basicSetRangeResource(newRangeResource, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.PROPERTY_DECLARATION__RANGE_RESOURCE, newRangeResource, newRangeResource));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName getAnnotationProperty()
  {
    return annotationProperty;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetAnnotationProperty(ResourceName newAnnotationProperty, NotificationChain msgs)
  {
    ResourceName oldAnnotationProperty = annotationProperty;
    annotationProperty = newAnnotationProperty;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.PROPERTY_DECLARATION__ANNOTATION_PROPERTY, oldAnnotationProperty, newAnnotationProperty);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setAnnotationProperty(ResourceName newAnnotationProperty)
  {
    if (newAnnotationProperty != annotationProperty)
    {
      NotificationChain msgs = null;
      if (annotationProperty != null)
        msgs = ((InternalEObject)annotationProperty).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROPERTY_DECLARATION__ANNOTATION_PROPERTY, null, msgs);
      if (newAnnotationProperty != null)
        msgs = ((InternalEObject)newAnnotationProperty).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROPERTY_DECLARATION__ANNOTATION_PROPERTY, null, msgs);
      msgs = basicSetAnnotationProperty(newAnnotationProperty, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.PROPERTY_DECLARATION__ANNOTATION_PROPERTY, newAnnotationProperty, newAnnotationProperty));
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
      case SadlPackage.PROPERTY_DECLARATION__PROPERTY_NAME:
        return basicSetPropertyName(null, msgs);
      case SadlPackage.PROPERTY_DECLARATION__SUPER_PROP_NAME:
        return basicSetSuperPropName(null, msgs);
      case SadlPackage.PROPERTY_DECLARATION__ADDL_PROP_INFO:
        return ((InternalEList<?>)getAddlPropInfo()).basicRemove(otherEnd, msgs);
      case SadlPackage.PROPERTY_DECLARATION__DOMAIN:
        return basicSetDomain(null, msgs);
      case SadlPackage.PROPERTY_DECLARATION__RANGE_RESOURCE:
        return basicSetRangeResource(null, msgs);
      case SadlPackage.PROPERTY_DECLARATION__ANNOTATION_PROPERTY:
        return basicSetAnnotationProperty(null, msgs);
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
      case SadlPackage.PROPERTY_DECLARATION__PROPERTY_NAME:
        return getPropertyName();
      case SadlPackage.PROPERTY_DECLARATION__SUPER_PROP_NAME:
        return getSuperPropName();
      case SadlPackage.PROPERTY_DECLARATION__ADDL_PROP_INFO:
        return getAddlPropInfo();
      case SadlPackage.PROPERTY_DECLARATION__ARTICLE:
        return getArticle();
      case SadlPackage.PROPERTY_DECLARATION__DOMAIN:
        return getDomain();
      case SadlPackage.PROPERTY_DECLARATION__RANGE_RESOURCE:
        return getRangeResource();
      case SadlPackage.PROPERTY_DECLARATION__ANNOTATION_PROPERTY:
        return getAnnotationProperty();
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
      case SadlPackage.PROPERTY_DECLARATION__PROPERTY_NAME:
        setPropertyName((ResourceName)newValue);
        return;
      case SadlPackage.PROPERTY_DECLARATION__SUPER_PROP_NAME:
        setSuperPropName((ResourceByName)newValue);
        return;
      case SadlPackage.PROPERTY_DECLARATION__ADDL_PROP_INFO:
        getAddlPropInfo().clear();
        getAddlPropInfo().addAll((Collection<? extends AdditionalPropertyInfo>)newValue);
        return;
      case SadlPackage.PROPERTY_DECLARATION__ARTICLE:
        setArticle((String)newValue);
        return;
      case SadlPackage.PROPERTY_DECLARATION__DOMAIN:
        setDomain((ResourceIdentifier)newValue);
        return;
      case SadlPackage.PROPERTY_DECLARATION__RANGE_RESOURCE:
        setRangeResource((ResourceIdentifier)newValue);
        return;
      case SadlPackage.PROPERTY_DECLARATION__ANNOTATION_PROPERTY:
        setAnnotationProperty((ResourceName)newValue);
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
      case SadlPackage.PROPERTY_DECLARATION__PROPERTY_NAME:
        setPropertyName((ResourceName)null);
        return;
      case SadlPackage.PROPERTY_DECLARATION__SUPER_PROP_NAME:
        setSuperPropName((ResourceByName)null);
        return;
      case SadlPackage.PROPERTY_DECLARATION__ADDL_PROP_INFO:
        getAddlPropInfo().clear();
        return;
      case SadlPackage.PROPERTY_DECLARATION__ARTICLE:
        setArticle(ARTICLE_EDEFAULT);
        return;
      case SadlPackage.PROPERTY_DECLARATION__DOMAIN:
        setDomain((ResourceIdentifier)null);
        return;
      case SadlPackage.PROPERTY_DECLARATION__RANGE_RESOURCE:
        setRangeResource((ResourceIdentifier)null);
        return;
      case SadlPackage.PROPERTY_DECLARATION__ANNOTATION_PROPERTY:
        setAnnotationProperty((ResourceName)null);
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
      case SadlPackage.PROPERTY_DECLARATION__PROPERTY_NAME:
        return propertyName != null;
      case SadlPackage.PROPERTY_DECLARATION__SUPER_PROP_NAME:
        return superPropName != null;
      case SadlPackage.PROPERTY_DECLARATION__ADDL_PROP_INFO:
        return addlPropInfo != null && !addlPropInfo.isEmpty();
      case SadlPackage.PROPERTY_DECLARATION__ARTICLE:
        return ARTICLE_EDEFAULT == null ? article != null : !ARTICLE_EDEFAULT.equals(article);
      case SadlPackage.PROPERTY_DECLARATION__DOMAIN:
        return domain != null;
      case SadlPackage.PROPERTY_DECLARATION__RANGE_RESOURCE:
        return rangeResource != null;
      case SadlPackage.PROPERTY_DECLARATION__ANNOTATION_PROPERTY:
        return annotationProperty != null;
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
    result.append(" (article: ");
    result.append(article);
    result.append(')');
    return result.toString();
  }

} //PropertyDeclarationImpl
