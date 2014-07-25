/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.InstanceDeclaration;
import com.ge.research.sadl.sadl.PropValPartialTriple;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.TypeDeclaration;

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
 * An implementation of the model object '<em><b>Instance Declaration</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstanceDeclarationImpl#getTypeDecl <em>Type Decl</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstanceDeclarationImpl#getAddlInfoItems <em>Addl Info Items</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstanceDeclarationImpl#getArticle <em>Article</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstanceDeclarationImpl#getClassName <em>Class Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.InstanceDeclarationImpl#getInstanceName <em>Instance Name</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class InstanceDeclarationImpl extends InstanceDeclarationStatementImpl implements InstanceDeclaration
{
  /**
   * The cached value of the '{@link #getTypeDecl() <em>Type Decl</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getTypeDecl()
   * @generated
   * @ordered
   */
  protected TypeDeclaration typeDecl;

  /**
   * The cached value of the '{@link #getAddlInfoItems() <em>Addl Info Items</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAddlInfoItems()
   * @generated
   * @ordered
   */
  protected EList<PropValPartialTriple> addlInfoItems;

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
   * The cached value of the '{@link #getClassName() <em>Class Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getClassName()
   * @generated
   * @ordered
   */
  protected ResourceByName className;

  /**
   * The cached value of the '{@link #getInstanceName() <em>Instance Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getInstanceName()
   * @generated
   * @ordered
   */
  protected ResourceName instanceName;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected InstanceDeclarationImpl()
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
    return SadlPackage.Literals.INSTANCE_DECLARATION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public TypeDeclaration getTypeDecl()
  {
    return typeDecl;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetTypeDecl(TypeDeclaration newTypeDecl, NotificationChain msgs)
  {
    TypeDeclaration oldTypeDecl = typeDecl;
    typeDecl = newTypeDecl;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCE_DECLARATION__TYPE_DECL, oldTypeDecl, newTypeDecl);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setTypeDecl(TypeDeclaration newTypeDecl)
  {
    if (newTypeDecl != typeDecl)
    {
      NotificationChain msgs = null;
      if (typeDecl != null)
        msgs = ((InternalEObject)typeDecl).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCE_DECLARATION__TYPE_DECL, null, msgs);
      if (newTypeDecl != null)
        msgs = ((InternalEObject)newTypeDecl).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCE_DECLARATION__TYPE_DECL, null, msgs);
      msgs = basicSetTypeDecl(newTypeDecl, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCE_DECLARATION__TYPE_DECL, newTypeDecl, newTypeDecl));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<PropValPartialTriple> getAddlInfoItems()
  {
    if (addlInfoItems == null)
    {
      addlInfoItems = new EObjectContainmentEList<PropValPartialTriple>(PropValPartialTriple.class, this, SadlPackage.INSTANCE_DECLARATION__ADDL_INFO_ITEMS);
    }
    return addlInfoItems;
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
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCE_DECLARATION__ARTICLE, oldArticle, article));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getClassName()
  {
    return className;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetClassName(ResourceByName newClassName, NotificationChain msgs)
  {
    ResourceByName oldClassName = className;
    className = newClassName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCE_DECLARATION__CLASS_NAME, oldClassName, newClassName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setClassName(ResourceByName newClassName)
  {
    if (newClassName != className)
    {
      NotificationChain msgs = null;
      if (className != null)
        msgs = ((InternalEObject)className).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCE_DECLARATION__CLASS_NAME, null, msgs);
      if (newClassName != null)
        msgs = ((InternalEObject)newClassName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCE_DECLARATION__CLASS_NAME, null, msgs);
      msgs = basicSetClassName(newClassName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCE_DECLARATION__CLASS_NAME, newClassName, newClassName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName getInstanceName()
  {
    return instanceName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetInstanceName(ResourceName newInstanceName, NotificationChain msgs)
  {
    ResourceName oldInstanceName = instanceName;
    instanceName = newInstanceName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCE_DECLARATION__INSTANCE_NAME, oldInstanceName, newInstanceName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setInstanceName(ResourceName newInstanceName)
  {
    if (newInstanceName != instanceName)
    {
      NotificationChain msgs = null;
      if (instanceName != null)
        msgs = ((InternalEObject)instanceName).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCE_DECLARATION__INSTANCE_NAME, null, msgs);
      if (newInstanceName != null)
        msgs = ((InternalEObject)newInstanceName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.INSTANCE_DECLARATION__INSTANCE_NAME, null, msgs);
      msgs = basicSetInstanceName(newInstanceName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.INSTANCE_DECLARATION__INSTANCE_NAME, newInstanceName, newInstanceName));
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
      case SadlPackage.INSTANCE_DECLARATION__TYPE_DECL:
        return basicSetTypeDecl(null, msgs);
      case SadlPackage.INSTANCE_DECLARATION__ADDL_INFO_ITEMS:
        return ((InternalEList<?>)getAddlInfoItems()).basicRemove(otherEnd, msgs);
      case SadlPackage.INSTANCE_DECLARATION__CLASS_NAME:
        return basicSetClassName(null, msgs);
      case SadlPackage.INSTANCE_DECLARATION__INSTANCE_NAME:
        return basicSetInstanceName(null, msgs);
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
      case SadlPackage.INSTANCE_DECLARATION__TYPE_DECL:
        return getTypeDecl();
      case SadlPackage.INSTANCE_DECLARATION__ADDL_INFO_ITEMS:
        return getAddlInfoItems();
      case SadlPackage.INSTANCE_DECLARATION__ARTICLE:
        return getArticle();
      case SadlPackage.INSTANCE_DECLARATION__CLASS_NAME:
        return getClassName();
      case SadlPackage.INSTANCE_DECLARATION__INSTANCE_NAME:
        return getInstanceName();
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
      case SadlPackage.INSTANCE_DECLARATION__TYPE_DECL:
        setTypeDecl((TypeDeclaration)newValue);
        return;
      case SadlPackage.INSTANCE_DECLARATION__ADDL_INFO_ITEMS:
        getAddlInfoItems().clear();
        getAddlInfoItems().addAll((Collection<? extends PropValPartialTriple>)newValue);
        return;
      case SadlPackage.INSTANCE_DECLARATION__ARTICLE:
        setArticle((String)newValue);
        return;
      case SadlPackage.INSTANCE_DECLARATION__CLASS_NAME:
        setClassName((ResourceByName)newValue);
        return;
      case SadlPackage.INSTANCE_DECLARATION__INSTANCE_NAME:
        setInstanceName((ResourceName)newValue);
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
      case SadlPackage.INSTANCE_DECLARATION__TYPE_DECL:
        setTypeDecl((TypeDeclaration)null);
        return;
      case SadlPackage.INSTANCE_DECLARATION__ADDL_INFO_ITEMS:
        getAddlInfoItems().clear();
        return;
      case SadlPackage.INSTANCE_DECLARATION__ARTICLE:
        setArticle(ARTICLE_EDEFAULT);
        return;
      case SadlPackage.INSTANCE_DECLARATION__CLASS_NAME:
        setClassName((ResourceByName)null);
        return;
      case SadlPackage.INSTANCE_DECLARATION__INSTANCE_NAME:
        setInstanceName((ResourceName)null);
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
      case SadlPackage.INSTANCE_DECLARATION__TYPE_DECL:
        return typeDecl != null;
      case SadlPackage.INSTANCE_DECLARATION__ADDL_INFO_ITEMS:
        return addlInfoItems != null && !addlInfoItems.isEmpty();
      case SadlPackage.INSTANCE_DECLARATION__ARTICLE:
        return ARTICLE_EDEFAULT == null ? article != null : !ARTICLE_EDEFAULT.equals(article);
      case SadlPackage.INSTANCE_DECLARATION__CLASS_NAME:
        return className != null;
      case SadlPackage.INSTANCE_DECLARATION__INSTANCE_NAME:
        return instanceName != null;
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

} //InstanceDeclarationImpl
