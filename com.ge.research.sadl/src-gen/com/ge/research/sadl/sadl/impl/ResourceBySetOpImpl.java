/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ResourceBySetOp;
import com.ge.research.sadl.sadl.ResourceIdentifier;
import com.ge.research.sadl.sadl.SadlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EDataTypeEList;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Resource By Set Op</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ResourceBySetOpImpl#getAnnType <em>Ann Type</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ResourceBySetOpImpl#getAnnContent <em>Ann Content</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ResourceBySetOpImpl#getNames <em>Names</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ResourceBySetOpImpl#getOp <em>Op</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ResourceBySetOpImpl extends ResourceIdentifierImpl implements ResourceBySetOp
{
  /**
   * The default value of the '{@link #getAnnType() <em>Ann Type</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnType()
   * @generated
   * @ordered
   */
  protected static final String ANN_TYPE_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getAnnType() <em>Ann Type</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnType()
   * @generated
   * @ordered
   */
  protected String annType = ANN_TYPE_EDEFAULT;

  /**
   * The default value of the '{@link #getAnnContent() <em>Ann Content</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnContent()
   * @generated
   * @ordered
   */
  protected static final String ANN_CONTENT_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getAnnContent() <em>Ann Content</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAnnContent()
   * @generated
   * @ordered
   */
  protected String annContent = ANN_CONTENT_EDEFAULT;

  /**
   * The cached value of the '{@link #getNames() <em>Names</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getNames()
   * @generated
   * @ordered
   */
  protected EList<ResourceIdentifier> names;

  /**
   * The cached value of the '{@link #getOp() <em>Op</em>}' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getOp()
   * @generated
   * @ordered
   */
  protected EList<String> op;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ResourceBySetOpImpl()
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
    return SadlPackage.Literals.RESOURCE_BY_SET_OP;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getAnnType()
  {
    return annType;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setAnnType(String newAnnType)
  {
    String oldAnnType = annType;
    annType = newAnnType;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RESOURCE_BY_SET_OP__ANN_TYPE, oldAnnType, annType));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getAnnContent()
  {
    return annContent;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setAnnContent(String newAnnContent)
  {
    String oldAnnContent = annContent;
    annContent = newAnnContent;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.RESOURCE_BY_SET_OP__ANN_CONTENT, oldAnnContent, annContent));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<ResourceIdentifier> getNames()
  {
    if (names == null)
    {
      names = new EObjectContainmentEList<ResourceIdentifier>(ResourceIdentifier.class, this, SadlPackage.RESOURCE_BY_SET_OP__NAMES);
    }
    return names;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<String> getOp()
  {
    if (op == null)
    {
      op = new EDataTypeEList<String>(String.class, this, SadlPackage.RESOURCE_BY_SET_OP__OP);
    }
    return op;
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
      case SadlPackage.RESOURCE_BY_SET_OP__NAMES:
        return ((InternalEList<?>)getNames()).basicRemove(otherEnd, msgs);
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
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_TYPE:
        return getAnnType();
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_CONTENT:
        return getAnnContent();
      case SadlPackage.RESOURCE_BY_SET_OP__NAMES:
        return getNames();
      case SadlPackage.RESOURCE_BY_SET_OP__OP:
        return getOp();
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
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_TYPE:
        setAnnType((String)newValue);
        return;
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_CONTENT:
        setAnnContent((String)newValue);
        return;
      case SadlPackage.RESOURCE_BY_SET_OP__NAMES:
        getNames().clear();
        getNames().addAll((Collection<? extends ResourceIdentifier>)newValue);
        return;
      case SadlPackage.RESOURCE_BY_SET_OP__OP:
        getOp().clear();
        getOp().addAll((Collection<? extends String>)newValue);
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
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_TYPE:
        setAnnType(ANN_TYPE_EDEFAULT);
        return;
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_CONTENT:
        setAnnContent(ANN_CONTENT_EDEFAULT);
        return;
      case SadlPackage.RESOURCE_BY_SET_OP__NAMES:
        getNames().clear();
        return;
      case SadlPackage.RESOURCE_BY_SET_OP__OP:
        getOp().clear();
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
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_TYPE:
        return ANN_TYPE_EDEFAULT == null ? annType != null : !ANN_TYPE_EDEFAULT.equals(annType);
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_CONTENT:
        return ANN_CONTENT_EDEFAULT == null ? annContent != null : !ANN_CONTENT_EDEFAULT.equals(annContent);
      case SadlPackage.RESOURCE_BY_SET_OP__NAMES:
        return names != null && !names.isEmpty();
      case SadlPackage.RESOURCE_BY_SET_OP__OP:
        return op != null && !op.isEmpty();
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
    result.append(" (annType: ");
    result.append(annType);
    result.append(", annContent: ");
    result.append(annContent);
    result.append(", op: ");
    result.append(op);
    result.append(')');
    return result.toString();
  }

} //ResourceBySetOpImpl
