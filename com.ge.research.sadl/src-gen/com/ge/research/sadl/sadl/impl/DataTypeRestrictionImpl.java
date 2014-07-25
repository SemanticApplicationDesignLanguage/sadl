/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.DataTypeRestriction;
import com.ge.research.sadl.sadl.Facets;
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

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Data Type Restriction</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.DataTypeRestrictionImpl#getBasetype <em>Basetype</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.DataTypeRestrictionImpl#getFacets <em>Facets</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.DataTypeRestrictionImpl#getBasetypes <em>Basetypes</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DataTypeRestrictionImpl extends MinimalEObjectImpl.Container implements DataTypeRestriction
{
  /**
   * The default value of the '{@link #getBasetype() <em>Basetype</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getBasetype()
   * @generated
   * @ordered
   */
  protected static final String BASETYPE_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getBasetype() <em>Basetype</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getBasetype()
   * @generated
   * @ordered
   */
  protected String basetype = BASETYPE_EDEFAULT;

  /**
   * The cached value of the '{@link #getFacets() <em>Facets</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getFacets()
   * @generated
   * @ordered
   */
  protected Facets facets;

  /**
   * The cached value of the '{@link #getBasetypes() <em>Basetypes</em>}' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getBasetypes()
   * @generated
   * @ordered
   */
  protected EList<String> basetypes;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected DataTypeRestrictionImpl()
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
    return SadlPackage.Literals.DATA_TYPE_RESTRICTION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getBasetype()
  {
    return basetype;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setBasetype(String newBasetype)
  {
    String oldBasetype = basetype;
    basetype = newBasetype;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.DATA_TYPE_RESTRICTION__BASETYPE, oldBasetype, basetype));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Facets getFacets()
  {
    return facets;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetFacets(Facets newFacets, NotificationChain msgs)
  {
    Facets oldFacets = facets;
    facets = newFacets;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.DATA_TYPE_RESTRICTION__FACETS, oldFacets, newFacets);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setFacets(Facets newFacets)
  {
    if (newFacets != facets)
    {
      NotificationChain msgs = null;
      if (facets != null)
        msgs = ((InternalEObject)facets).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.DATA_TYPE_RESTRICTION__FACETS, null, msgs);
      if (newFacets != null)
        msgs = ((InternalEObject)newFacets).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.DATA_TYPE_RESTRICTION__FACETS, null, msgs);
      msgs = basicSetFacets(newFacets, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.DATA_TYPE_RESTRICTION__FACETS, newFacets, newFacets));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<String> getBasetypes()
  {
    if (basetypes == null)
    {
      basetypes = new EDataTypeEList<String>(String.class, this, SadlPackage.DATA_TYPE_RESTRICTION__BASETYPES);
    }
    return basetypes;
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
      case SadlPackage.DATA_TYPE_RESTRICTION__FACETS:
        return basicSetFacets(null, msgs);
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
      case SadlPackage.DATA_TYPE_RESTRICTION__BASETYPE:
        return getBasetype();
      case SadlPackage.DATA_TYPE_RESTRICTION__FACETS:
        return getFacets();
      case SadlPackage.DATA_TYPE_RESTRICTION__BASETYPES:
        return getBasetypes();
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
      case SadlPackage.DATA_TYPE_RESTRICTION__BASETYPE:
        setBasetype((String)newValue);
        return;
      case SadlPackage.DATA_TYPE_RESTRICTION__FACETS:
        setFacets((Facets)newValue);
        return;
      case SadlPackage.DATA_TYPE_RESTRICTION__BASETYPES:
        getBasetypes().clear();
        getBasetypes().addAll((Collection<? extends String>)newValue);
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
      case SadlPackage.DATA_TYPE_RESTRICTION__BASETYPE:
        setBasetype(BASETYPE_EDEFAULT);
        return;
      case SadlPackage.DATA_TYPE_RESTRICTION__FACETS:
        setFacets((Facets)null);
        return;
      case SadlPackage.DATA_TYPE_RESTRICTION__BASETYPES:
        getBasetypes().clear();
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
      case SadlPackage.DATA_TYPE_RESTRICTION__BASETYPE:
        return BASETYPE_EDEFAULT == null ? basetype != null : !BASETYPE_EDEFAULT.equals(basetype);
      case SadlPackage.DATA_TYPE_RESTRICTION__FACETS:
        return facets != null;
      case SadlPackage.DATA_TYPE_RESTRICTION__BASETYPES:
        return basetypes != null && !basetypes.isEmpty();
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
    result.append(" (basetype: ");
    result.append(basetype);
    result.append(", basetypes: ");
    result.append(basetypes);
    result.append(')');
    return result.toString();
  }

} //DataTypeRestrictionImpl
