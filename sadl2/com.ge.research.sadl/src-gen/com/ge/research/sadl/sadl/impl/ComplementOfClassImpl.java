/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.ComplementOfClass;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceIdentifier;
import com.ge.research.sadl.sadl.SadlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Complement Of Class</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ComplementOfClassImpl#getClass1 <em>Class1</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ComplementOfClassImpl#getClass2 <em>Class2</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ComplementOfClassImpl extends StatementImpl implements ComplementOfClass
{
  /**
   * The cached value of the '{@link #getClass1() <em>Class1</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getClass1()
   * @generated
   * @ordered
   */
  protected ResourceByName class1;

  /**
   * The cached value of the '{@link #getClass2() <em>Class2</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getClass2()
   * @generated
   * @ordered
   */
  protected ResourceIdentifier class2;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ComplementOfClassImpl()
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
    return SadlPackage.Literals.COMPLEMENT_OF_CLASS;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceByName getClass1()
  {
    return class1;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetClass1(ResourceByName newClass1, NotificationChain msgs)
  {
    ResourceByName oldClass1 = class1;
    class1 = newClass1;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.COMPLEMENT_OF_CLASS__CLASS1, oldClass1, newClass1);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setClass1(ResourceByName newClass1)
  {
    if (newClass1 != class1)
    {
      NotificationChain msgs = null;
      if (class1 != null)
        msgs = ((InternalEObject)class1).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.COMPLEMENT_OF_CLASS__CLASS1, null, msgs);
      if (newClass1 != null)
        msgs = ((InternalEObject)newClass1).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.COMPLEMENT_OF_CLASS__CLASS1, null, msgs);
      msgs = basicSetClass1(newClass1, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.COMPLEMENT_OF_CLASS__CLASS1, newClass1, newClass1));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceIdentifier getClass2()
  {
    return class2;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetClass2(ResourceIdentifier newClass2, NotificationChain msgs)
  {
    ResourceIdentifier oldClass2 = class2;
    class2 = newClass2;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.COMPLEMENT_OF_CLASS__CLASS2, oldClass2, newClass2);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setClass2(ResourceIdentifier newClass2)
  {
    if (newClass2 != class2)
    {
      NotificationChain msgs = null;
      if (class2 != null)
        msgs = ((InternalEObject)class2).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.COMPLEMENT_OF_CLASS__CLASS2, null, msgs);
      if (newClass2 != null)
        msgs = ((InternalEObject)newClass2).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.COMPLEMENT_OF_CLASS__CLASS2, null, msgs);
      msgs = basicSetClass2(newClass2, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.COMPLEMENT_OF_CLASS__CLASS2, newClass2, newClass2));
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
      case SadlPackage.COMPLEMENT_OF_CLASS__CLASS1:
        return basicSetClass1(null, msgs);
      case SadlPackage.COMPLEMENT_OF_CLASS__CLASS2:
        return basicSetClass2(null, msgs);
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
      case SadlPackage.COMPLEMENT_OF_CLASS__CLASS1:
        return getClass1();
      case SadlPackage.COMPLEMENT_OF_CLASS__CLASS2:
        return getClass2();
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
      case SadlPackage.COMPLEMENT_OF_CLASS__CLASS1:
        setClass1((ResourceByName)newValue);
        return;
      case SadlPackage.COMPLEMENT_OF_CLASS__CLASS2:
        setClass2((ResourceIdentifier)newValue);
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
      case SadlPackage.COMPLEMENT_OF_CLASS__CLASS1:
        setClass1((ResourceByName)null);
        return;
      case SadlPackage.COMPLEMENT_OF_CLASS__CLASS2:
        setClass2((ResourceIdentifier)null);
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
      case SadlPackage.COMPLEMENT_OF_CLASS__CLASS1:
        return class1 != null;
      case SadlPackage.COMPLEMENT_OF_CLASS__CLASS2:
        return class2 != null;
    }
    return super.eIsSet(featureID);
  }

} //ComplementOfClassImpl
