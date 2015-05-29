/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ContentList;
import com.ge.research.sadl.sadl.ResourceBySetOp;
import com.ge.research.sadl.sadl.ResourceIdentifier;
import com.ge.research.sadl.sadl.SadlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

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
  public EList<String> getAnnType()
  {
    if (annType == null)
    {
      annType = new EDataTypeEList<String>(String.class, this, SadlPackage.RESOURCE_BY_SET_OP__ANN_TYPE);
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
      annContent = new EObjectContainmentEList<ContentList>(ContentList.class, this, SadlPackage.RESOURCE_BY_SET_OP__ANN_CONTENT);
    }
    return annContent;
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
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_CONTENT:
        return ((InternalEList<?>)getAnnContent()).basicRemove(otherEnd, msgs);
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
        getAnnType().clear();
        getAnnType().addAll((Collection<? extends String>)newValue);
        return;
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_CONTENT:
        getAnnContent().clear();
        getAnnContent().addAll((Collection<? extends ContentList>)newValue);
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
        getAnnType().clear();
        return;
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_CONTENT:
        getAnnContent().clear();
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
        return annType != null && !annType.isEmpty();
      case SadlPackage.RESOURCE_BY_SET_OP__ANN_CONTENT:
        return annContent != null && !annContent.isEmpty();
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
    result.append(", op: ");
    result.append(op);
    result.append(')');
    return result.toString();
  }

} //ResourceBySetOpImpl
