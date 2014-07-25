/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.Condition;
import com.ge.research.sadl.sadl.NecessaryAndSufficient;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.TypedBNode;

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
 * An implementation of the model object '<em><b>Necessary And Sufficient</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.NecessaryAndSufficientImpl#getSuperClass <em>Super Class</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.NecessaryAndSufficientImpl#getArticle <em>Article</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.NecessaryAndSufficientImpl#getSubClass <em>Sub Class</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.NecessaryAndSufficientImpl#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.NecessaryAndSufficientImpl#getCond <em>Cond</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class NecessaryAndSufficientImpl extends StatementImpl implements NecessaryAndSufficient
{
  /**
   * The cached value of the '{@link #getSuperClass() <em>Super Class</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getSuperClass()
   * @generated
   * @ordered
   */
  protected TypedBNode superClass;

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
   * The cached value of the '{@link #getSubClass() <em>Sub Class</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getSubClass()
   * @generated
   * @ordered
   */
  protected ResourceName subClass;

  /**
   * The cached value of the '{@link #getPropertyName() <em>Property Name</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPropertyName()
   * @generated
   * @ordered
   */
  protected EList<ResourceByName> propertyName;

  /**
   * The cached value of the '{@link #getCond() <em>Cond</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getCond()
   * @generated
   * @ordered
   */
  protected EList<Condition> cond;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected NecessaryAndSufficientImpl()
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
    return SadlPackage.Literals.NECESSARY_AND_SUFFICIENT;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public TypedBNode getSuperClass()
  {
    return superClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetSuperClass(TypedBNode newSuperClass, NotificationChain msgs)
  {
    TypedBNode oldSuperClass = superClass;
    superClass = newSuperClass;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.NECESSARY_AND_SUFFICIENT__SUPER_CLASS, oldSuperClass, newSuperClass);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setSuperClass(TypedBNode newSuperClass)
  {
    if (newSuperClass != superClass)
    {
      NotificationChain msgs = null;
      if (superClass != null)
        msgs = ((InternalEObject)superClass).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.NECESSARY_AND_SUFFICIENT__SUPER_CLASS, null, msgs);
      if (newSuperClass != null)
        msgs = ((InternalEObject)newSuperClass).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.NECESSARY_AND_SUFFICIENT__SUPER_CLASS, null, msgs);
      msgs = basicSetSuperClass(newSuperClass, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.NECESSARY_AND_SUFFICIENT__SUPER_CLASS, newSuperClass, newSuperClass));
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
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.NECESSARY_AND_SUFFICIENT__ARTICLE, oldArticle, article));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName getSubClass()
  {
    return subClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetSubClass(ResourceName newSubClass, NotificationChain msgs)
  {
    ResourceName oldSubClass = subClass;
    subClass = newSubClass;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.NECESSARY_AND_SUFFICIENT__SUB_CLASS, oldSubClass, newSubClass);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setSubClass(ResourceName newSubClass)
  {
    if (newSubClass != subClass)
    {
      NotificationChain msgs = null;
      if (subClass != null)
        msgs = ((InternalEObject)subClass).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.NECESSARY_AND_SUFFICIENT__SUB_CLASS, null, msgs);
      if (newSubClass != null)
        msgs = ((InternalEObject)newSubClass).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.NECESSARY_AND_SUFFICIENT__SUB_CLASS, null, msgs);
      msgs = basicSetSubClass(newSubClass, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.NECESSARY_AND_SUFFICIENT__SUB_CLASS, newSubClass, newSubClass));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<ResourceByName> getPropertyName()
  {
    if (propertyName == null)
    {
      propertyName = new EObjectContainmentEList<ResourceByName>(ResourceByName.class, this, SadlPackage.NECESSARY_AND_SUFFICIENT__PROPERTY_NAME);
    }
    return propertyName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<Condition> getCond()
  {
    if (cond == null)
    {
      cond = new EObjectContainmentEList<Condition>(Condition.class, this, SadlPackage.NECESSARY_AND_SUFFICIENT__COND);
    }
    return cond;
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
      case SadlPackage.NECESSARY_AND_SUFFICIENT__SUPER_CLASS:
        return basicSetSuperClass(null, msgs);
      case SadlPackage.NECESSARY_AND_SUFFICIENT__SUB_CLASS:
        return basicSetSubClass(null, msgs);
      case SadlPackage.NECESSARY_AND_SUFFICIENT__PROPERTY_NAME:
        return ((InternalEList<?>)getPropertyName()).basicRemove(otherEnd, msgs);
      case SadlPackage.NECESSARY_AND_SUFFICIENT__COND:
        return ((InternalEList<?>)getCond()).basicRemove(otherEnd, msgs);
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
      case SadlPackage.NECESSARY_AND_SUFFICIENT__SUPER_CLASS:
        return getSuperClass();
      case SadlPackage.NECESSARY_AND_SUFFICIENT__ARTICLE:
        return getArticle();
      case SadlPackage.NECESSARY_AND_SUFFICIENT__SUB_CLASS:
        return getSubClass();
      case SadlPackage.NECESSARY_AND_SUFFICIENT__PROPERTY_NAME:
        return getPropertyName();
      case SadlPackage.NECESSARY_AND_SUFFICIENT__COND:
        return getCond();
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
      case SadlPackage.NECESSARY_AND_SUFFICIENT__SUPER_CLASS:
        setSuperClass((TypedBNode)newValue);
        return;
      case SadlPackage.NECESSARY_AND_SUFFICIENT__ARTICLE:
        setArticle((String)newValue);
        return;
      case SadlPackage.NECESSARY_AND_SUFFICIENT__SUB_CLASS:
        setSubClass((ResourceName)newValue);
        return;
      case SadlPackage.NECESSARY_AND_SUFFICIENT__PROPERTY_NAME:
        getPropertyName().clear();
        getPropertyName().addAll((Collection<? extends ResourceByName>)newValue);
        return;
      case SadlPackage.NECESSARY_AND_SUFFICIENT__COND:
        getCond().clear();
        getCond().addAll((Collection<? extends Condition>)newValue);
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
      case SadlPackage.NECESSARY_AND_SUFFICIENT__SUPER_CLASS:
        setSuperClass((TypedBNode)null);
        return;
      case SadlPackage.NECESSARY_AND_SUFFICIENT__ARTICLE:
        setArticle(ARTICLE_EDEFAULT);
        return;
      case SadlPackage.NECESSARY_AND_SUFFICIENT__SUB_CLASS:
        setSubClass((ResourceName)null);
        return;
      case SadlPackage.NECESSARY_AND_SUFFICIENT__PROPERTY_NAME:
        getPropertyName().clear();
        return;
      case SadlPackage.NECESSARY_AND_SUFFICIENT__COND:
        getCond().clear();
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
      case SadlPackage.NECESSARY_AND_SUFFICIENT__SUPER_CLASS:
        return superClass != null;
      case SadlPackage.NECESSARY_AND_SUFFICIENT__ARTICLE:
        return ARTICLE_EDEFAULT == null ? article != null : !ARTICLE_EDEFAULT.equals(article);
      case SadlPackage.NECESSARY_AND_SUFFICIENT__SUB_CLASS:
        return subClass != null;
      case SadlPackage.NECESSARY_AND_SUFFICIENT__PROPERTY_NAME:
        return propertyName != null && !propertyName.isEmpty();
      case SadlPackage.NECESSARY_AND_SUFFICIENT__COND:
        return cond != null && !cond.isEmpty();
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

} //NecessaryAndSufficientImpl
