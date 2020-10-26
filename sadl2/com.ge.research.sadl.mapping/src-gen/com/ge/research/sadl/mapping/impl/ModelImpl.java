/**
 */
package com.ge.research.sadl.mapping.impl;

import com.ge.research.sadl.mapping.Import;
import com.ge.research.sadl.mapping.MappingPackage;
import com.ge.research.sadl.mapping.Model;
import com.ge.research.sadl.mapping.NewModelNS;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Model</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.mapping.impl.ModelImpl#getUri <em>Uri</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.impl.ModelImpl#getImports <em>Imports</em>}</li>
 *   <li>{@link com.ge.research.sadl.mapping.impl.ModelImpl#getTriples <em>Triples</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ModelImpl extends MinimalEObjectImpl.Container implements Model
{
  /**
   * The cached value of the '{@link #getUri() <em>Uri</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getUri()
   * @generated
   * @ordered
   */
  protected NewModelNS uri;

  /**
   * The cached value of the '{@link #getImports() <em>Imports</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getImports()
   * @generated
   * @ordered
   */
  protected EList<Import> imports;

  /**
   * The cached value of the '{@link #getTriples() <em>Triples</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getTriples()
   * @generated
   * @ordered
   */
  protected EList<EObject> triples;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ModelImpl()
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
    return MappingPackage.Literals.MODEL;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NewModelNS getUri()
  {
    return uri;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetUri(NewModelNS newUri, NotificationChain msgs)
  {
    NewModelNS oldUri = uri;
    uri = newUri;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, MappingPackage.MODEL__URI, oldUri, newUri);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setUri(NewModelNS newUri)
  {
    if (newUri != uri)
    {
      NotificationChain msgs = null;
      if (uri != null)
        msgs = ((InternalEObject)uri).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - MappingPackage.MODEL__URI, null, msgs);
      if (newUri != null)
        msgs = ((InternalEObject)newUri).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - MappingPackage.MODEL__URI, null, msgs);
      msgs = basicSetUri(newUri, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.MODEL__URI, newUri, newUri));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<Import> getImports()
  {
    if (imports == null)
    {
      imports = new EObjectContainmentEList<Import>(Import.class, this, MappingPackage.MODEL__IMPORTS);
    }
    return imports;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<EObject> getTriples()
  {
    if (triples == null)
    {
      triples = new EObjectContainmentEList<EObject>(EObject.class, this, MappingPackage.MODEL__TRIPLES);
    }
    return triples;
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
      case MappingPackage.MODEL__URI:
        return basicSetUri(null, msgs);
      case MappingPackage.MODEL__IMPORTS:
        return ((InternalEList<?>)getImports()).basicRemove(otherEnd, msgs);
      case MappingPackage.MODEL__TRIPLES:
        return ((InternalEList<?>)getTriples()).basicRemove(otherEnd, msgs);
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
      case MappingPackage.MODEL__URI:
        return getUri();
      case MappingPackage.MODEL__IMPORTS:
        return getImports();
      case MappingPackage.MODEL__TRIPLES:
        return getTriples();
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
      case MappingPackage.MODEL__URI:
        setUri((NewModelNS)newValue);
        return;
      case MappingPackage.MODEL__IMPORTS:
        getImports().clear();
        getImports().addAll((Collection<? extends Import>)newValue);
        return;
      case MappingPackage.MODEL__TRIPLES:
        getTriples().clear();
        getTriples().addAll((Collection<? extends EObject>)newValue);
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
      case MappingPackage.MODEL__URI:
        setUri((NewModelNS)null);
        return;
      case MappingPackage.MODEL__IMPORTS:
        getImports().clear();
        return;
      case MappingPackage.MODEL__TRIPLES:
        getTriples().clear();
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
      case MappingPackage.MODEL__URI:
        return uri != null;
      case MappingPackage.MODEL__IMPORTS:
        return imports != null && !imports.isEmpty();
      case MappingPackage.MODEL__TRIPLES:
        return triples != null && !triples.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //ModelImpl
