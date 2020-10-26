/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ContentList;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EDataTypeEList;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Resource Name</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ResourceNameImpl#getName <em>Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ResourceNameImpl#getAnnType <em>Ann Type</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ResourceNameImpl#getAnnContent <em>Ann Content</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ResourceNameImpl extends MinimalEObjectImpl.Container implements ResourceName
{
  /**
   * The default value of the '{@link #getName() <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getName()
   * @generated
   * @ordered
   */
  protected static final String NAME_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getName()
   * @generated
   * @ordered
   */
  protected String name = NAME_EDEFAULT;

  /**
   * The cached value of the '{@link #getAnnType() <em>Ann Type</em>}' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnType()
   * @generated
   * @ordered
   */
  protected EList<String> annType;

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
  protected ResourceNameImpl()
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
    return SadlPackage.Literals.RESOURCE_NAME;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getName()
  {
    return name;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setName(String newName)
  {
    String oldName = name;
    name = newName;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RESOURCE_NAME__NAME, oldName, name));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<String> getAnnType()
  {
    if (annType == null)
    {
      annType = new EDataTypeEList<String>(String.class, this, SadlPackage.RESOURCE_NAME__ANN_TYPE);
    }
    return annType;
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
      annContent = new EObjectContainmentEList<ContentList>(ContentList.class, this, SadlPackage.RESOURCE_NAME__ANN_CONTENT);
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
      case SadlPackage.RESOURCE_NAME__ANN_CONTENT:
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
      case SadlPackage.RESOURCE_NAME__NAME:
        return getName();
      case SadlPackage.RESOURCE_NAME__ANN_TYPE:
        return getAnnType();
      case SadlPackage.RESOURCE_NAME__ANN_CONTENT:
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
      case SadlPackage.RESOURCE_NAME__NAME:
        setName((String)newValue);
        return;
      case SadlPackage.RESOURCE_NAME__ANN_TYPE:
        getAnnType().clear();
        getAnnType().addAll((Collection<? extends String>)newValue);
        return;
      case SadlPackage.RESOURCE_NAME__ANN_CONTENT:
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
      case SadlPackage.RESOURCE_NAME__NAME:
        setName(NAME_EDEFAULT);
        return;
      case SadlPackage.RESOURCE_NAME__ANN_TYPE:
        getAnnType().clear();
        return;
      case SadlPackage.RESOURCE_NAME__ANN_CONTENT:
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
      case SadlPackage.RESOURCE_NAME__NAME:
        return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
      case SadlPackage.RESOURCE_NAME__ANN_TYPE:
        return annType != null && !annType.isEmpty();
      case SadlPackage.RESOURCE_NAME__ANN_CONTENT:
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
    result.append(" (name: ");
    result.append(name);
    result.append(", annType: ");
    result.append(annType);
    result.append(')');
    return result.toString();
  }

} //ResourceNameImpl
