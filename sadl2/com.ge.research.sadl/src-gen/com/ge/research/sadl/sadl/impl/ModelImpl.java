/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.sadl.ModelElement;
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
 * An implementation of the model object '<em><b>Model</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ModelImpl#getModelName <em>Model Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ModelImpl#getImports <em>Imports</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ModelImpl#getElements <em>Elements</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ModelImpl extends MinimalEObjectImpl.Container implements Model
{
  /**
   * The cached value of the '{@link #getModelName() <em>Model Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getModelName()
   * @generated
   * @ordered
   */
  protected ModelName modelName;

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
   * The cached value of the '{@link #getElements() <em>Elements</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getElements()
   * @generated
   * @ordered
   */
  protected EList<ModelElement> elements;

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
    return SadlPackage.Literals.MODEL;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ModelName getModelName()
  {
    return modelName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetModelName(ModelName newModelName, NotificationChain msgs)
  {
    ModelName oldModelName = modelName;
    modelName = newModelName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.MODEL__MODEL_NAME, oldModelName, newModelName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setModelName(ModelName newModelName)
  {
    if (newModelName != modelName)
    {
      NotificationChain msgs = null;
      if (modelName != null)
        msgs = ((InternalEObject)modelName).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.MODEL__MODEL_NAME, null, msgs);
      if (newModelName != null)
        msgs = ((InternalEObject)newModelName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.MODEL__MODEL_NAME, null, msgs);
      msgs = basicSetModelName(newModelName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.MODEL__MODEL_NAME, newModelName, newModelName));
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
      imports = new EObjectContainmentEList<Import>(Import.class, this, SadlPackage.MODEL__IMPORTS);
    }
    return imports;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<ModelElement> getElements()
  {
    if (elements == null)
    {
      elements = new EObjectContainmentEList<ModelElement>(ModelElement.class, this, SadlPackage.MODEL__ELEMENTS);
    }
    return elements;
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
      case SadlPackage.MODEL__MODEL_NAME:
        return basicSetModelName(null, msgs);
      case SadlPackage.MODEL__IMPORTS:
        return ((InternalEList<?>)getImports()).basicRemove(otherEnd, msgs);
      case SadlPackage.MODEL__ELEMENTS:
        return ((InternalEList<?>)getElements()).basicRemove(otherEnd, msgs);
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
      case SadlPackage.MODEL__MODEL_NAME:
        return getModelName();
      case SadlPackage.MODEL__IMPORTS:
        return getImports();
      case SadlPackage.MODEL__ELEMENTS:
        return getElements();
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
      case SadlPackage.MODEL__MODEL_NAME:
        setModelName((ModelName)newValue);
        return;
      case SadlPackage.MODEL__IMPORTS:
        getImports().clear();
        getImports().addAll((Collection<? extends Import>)newValue);
        return;
      case SadlPackage.MODEL__ELEMENTS:
        getElements().clear();
        getElements().addAll((Collection<? extends ModelElement>)newValue);
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
      case SadlPackage.MODEL__MODEL_NAME:
        setModelName((ModelName)null);
        return;
      case SadlPackage.MODEL__IMPORTS:
        getImports().clear();
        return;
      case SadlPackage.MODEL__ELEMENTS:
        getElements().clear();
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
      case SadlPackage.MODEL__MODEL_NAME:
        return modelName != null;
      case SadlPackage.MODEL__IMPORTS:
        return imports != null && !imports.isEmpty();
      case SadlPackage.MODEL__ELEMENTS:
        return elements != null && !elements.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //ModelImpl
