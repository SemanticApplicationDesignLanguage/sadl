/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ExplicitValue;
import com.ge.research.sadl.sadl.InstanceDeclaration;
import com.ge.research.sadl.sadl.PropValPartialTriple;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Prop Val Partial Triple</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropValPartialTripleImpl#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropValPartialTripleImpl#getObjectValue <em>Object Value</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.PropValPartialTripleImpl#getObjectValueBNode <em>Object Value BNode</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class PropValPartialTripleImpl extends MinimalEObjectImpl.Container implements PropValPartialTriple
{
  /**
   * The cached value of the '{@link #getPropertyName() <em>Property Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPropertyName()
   * @generated
   * @ordered
   */
  protected ResourceByName propertyName;

  /**
   * The cached value of the '{@link #getObjectValue() <em>Object Value</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getObjectValue()
   * @generated
   * @ordered
   */
  protected ExplicitValue objectValue;

  /**
   * The cached value of the '{@link #getObjectValueBNode() <em>Object Value BNode</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getObjectValueBNode()
   * @generated
   * @ordered
   */
  protected InstanceDeclaration objectValueBNode;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected PropValPartialTripleImpl()
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
    return SadlPackage.Literals.PROP_VAL_PARTIAL_TRIPLE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getPropertyName()
  {
    return propertyName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPropertyName(ResourceByName newPropertyName, NotificationChain msgs)
  {
    ResourceByName oldPropertyName = propertyName;
    propertyName = newPropertyName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME, oldPropertyName, newPropertyName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPropertyName(ResourceByName newPropertyName)
  {
    if (newPropertyName != propertyName)
    {
      NotificationChain msgs = null;
      if (propertyName != null)
        msgs = ((InternalEObject)propertyName).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME, null, msgs);
      if (newPropertyName != null)
        msgs = ((InternalEObject)newPropertyName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME, null, msgs);
      msgs = basicSetPropertyName(newPropertyName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME, newPropertyName, newPropertyName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExplicitValue getObjectValue()
  {
    return objectValue;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetObjectValue(ExplicitValue newObjectValue, NotificationChain msgs)
  {
    ExplicitValue oldObjectValue = objectValue;
    objectValue = newObjectValue;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE, oldObjectValue, newObjectValue);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setObjectValue(ExplicitValue newObjectValue)
  {
    if (newObjectValue != objectValue)
    {
      NotificationChain msgs = null;
      if (objectValue != null)
        msgs = ((InternalEObject)objectValue).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE, null, msgs);
      if (newObjectValue != null)
        msgs = ((InternalEObject)newObjectValue).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE, null, msgs);
      msgs = basicSetObjectValue(newObjectValue, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE, newObjectValue, newObjectValue));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public InstanceDeclaration getObjectValueBNode()
  {
    return objectValueBNode;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetObjectValueBNode(InstanceDeclaration newObjectValueBNode, NotificationChain msgs)
  {
    InstanceDeclaration oldObjectValueBNode = objectValueBNode;
    objectValueBNode = newObjectValueBNode;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE, oldObjectValueBNode, newObjectValueBNode);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setObjectValueBNode(InstanceDeclaration newObjectValueBNode)
  {
    if (newObjectValueBNode != objectValueBNode)
    {
      NotificationChain msgs = null;
      if (objectValueBNode != null)
        msgs = ((InternalEObject)objectValueBNode).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE, null, msgs);
      if (newObjectValueBNode != null)
        msgs = ((InternalEObject)newObjectValueBNode).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE, null, msgs);
      msgs = basicSetObjectValueBNode(newObjectValueBNode, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE, newObjectValueBNode, newObjectValueBNode));
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
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME:
        return basicSetPropertyName(null, msgs);
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE:
        return basicSetObjectValue(null, msgs);
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE:
        return basicSetObjectValueBNode(null, msgs);
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
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME:
        return getPropertyName();
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE:
        return getObjectValue();
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE:
        return getObjectValueBNode();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME:
        setPropertyName((ResourceByName)newValue);
        return;
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE:
        setObjectValue((ExplicitValue)newValue);
        return;
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE:
        setObjectValueBNode((InstanceDeclaration)newValue);
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
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME:
        setPropertyName((ResourceByName)null);
        return;
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE:
        setObjectValue((ExplicitValue)null);
        return;
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE:
        setObjectValueBNode((InstanceDeclaration)null);
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
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__PROPERTY_NAME:
        return propertyName != null;
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE:
        return objectValue != null;
      case SadlPackage.PROP_VAL_PARTIAL_TRIPLE__OBJECT_VALUE_BNODE:
        return objectValueBNode != null;
    }
    return super.eIsSet(featureID);
  }

} //PropValPartialTripleImpl
