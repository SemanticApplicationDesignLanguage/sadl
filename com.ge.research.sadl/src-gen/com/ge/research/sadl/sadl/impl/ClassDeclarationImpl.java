/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.AddlClassInfo;
import com.ge.research.sadl.sadl.ClassDeclaration;
import com.ge.research.sadl.sadl.EnumeratedInstances;
import com.ge.research.sadl.sadl.ResourceIdentifier;
import com.ge.research.sadl.sadl.ResourceList;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlPackage;

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
 * An implementation of the model object '<em><b>Class Declaration</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ClassDeclarationImpl#getClassName <em>Class Name</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ClassDeclarationImpl#getMustBeOneOf <em>Must Be One Of</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ClassDeclarationImpl#getDescribedBy <em>Described By</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ClassDeclarationImpl#getClassList <em>Class List</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.ClassDeclarationImpl#getClassIdentifier <em>Class Identifier</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ClassDeclarationImpl extends StatementImpl implements ClassDeclaration
{
  /**
   * The cached value of the '{@link #getClassName() <em>Class Name</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getClassName()
   * @generated
   * @ordered
   */
  protected ResourceName className;

  /**
   * The cached value of the '{@link #getMustBeOneOf() <em>Must Be One Of</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMustBeOneOf()
   * @generated
   * @ordered
   */
  protected EnumeratedInstances mustBeOneOf;

  /**
   * The cached value of the '{@link #getDescribedBy() <em>Described By</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDescribedBy()
   * @generated
   * @ordered
   */
  protected EList<AddlClassInfo> describedBy;

  /**
   * The cached value of the '{@link #getClassList() <em>Class List</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getClassList()
   * @generated
   * @ordered
   */
  protected ResourceList classList;

  /**
   * The cached value of the '{@link #getClassIdentifier() <em>Class Identifier</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getClassIdentifier()
   * @generated
   * @ordered
   */
  protected ResourceIdentifier classIdentifier;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ClassDeclarationImpl()
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
    return SadlPackage.Literals.CLASS_DECLARATION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceName getClassName()
  {
    return className;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetClassName(ResourceName newClassName, NotificationChain msgs)
  {
    ResourceName oldClassName = className;
    className = newClassName;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.CLASS_DECLARATION__CLASS_NAME, oldClassName, newClassName);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setClassName(ResourceName newClassName)
  {
    if (newClassName != className)
    {
      NotificationChain msgs = null;
      if (className != null)
        msgs = ((InternalEObject)className).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CLASS_DECLARATION__CLASS_NAME, null, msgs);
      if (newClassName != null)
        msgs = ((InternalEObject)newClassName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CLASS_DECLARATION__CLASS_NAME, null, msgs);
      msgs = basicSetClassName(newClassName, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.CLASS_DECLARATION__CLASS_NAME, newClassName, newClassName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EnumeratedInstances getMustBeOneOf()
  {
    return mustBeOneOf;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetMustBeOneOf(EnumeratedInstances newMustBeOneOf, NotificationChain msgs)
  {
    EnumeratedInstances oldMustBeOneOf = mustBeOneOf;
    mustBeOneOf = newMustBeOneOf;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.CLASS_DECLARATION__MUST_BE_ONE_OF, oldMustBeOneOf, newMustBeOneOf);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setMustBeOneOf(EnumeratedInstances newMustBeOneOf)
  {
    if (newMustBeOneOf != mustBeOneOf)
    {
      NotificationChain msgs = null;
      if (mustBeOneOf != null)
        msgs = ((InternalEObject)mustBeOneOf).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CLASS_DECLARATION__MUST_BE_ONE_OF, null, msgs);
      if (newMustBeOneOf != null)
        msgs = ((InternalEObject)newMustBeOneOf).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CLASS_DECLARATION__MUST_BE_ONE_OF, null, msgs);
      msgs = basicSetMustBeOneOf(newMustBeOneOf, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.CLASS_DECLARATION__MUST_BE_ONE_OF, newMustBeOneOf, newMustBeOneOf));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<AddlClassInfo> getDescribedBy()
  {
    if (describedBy == null)
    {
      describedBy = new EObjectContainmentEList<AddlClassInfo>(AddlClassInfo.class, this, SadlPackage.CLASS_DECLARATION__DESCRIBED_BY);
    }
    return describedBy;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceList getClassList()
  {
    return classList;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetClassList(ResourceList newClassList, NotificationChain msgs)
  {
    ResourceList oldClassList = classList;
    classList = newClassList;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.CLASS_DECLARATION__CLASS_LIST, oldClassList, newClassList);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setClassList(ResourceList newClassList)
  {
    if (newClassList != classList)
    {
      NotificationChain msgs = null;
      if (classList != null)
        msgs = ((InternalEObject)classList).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CLASS_DECLARATION__CLASS_LIST, null, msgs);
      if (newClassList != null)
        msgs = ((InternalEObject)newClassList).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CLASS_DECLARATION__CLASS_LIST, null, msgs);
      msgs = basicSetClassList(newClassList, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.CLASS_DECLARATION__CLASS_LIST, newClassList, newClassList));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ResourceIdentifier getClassIdentifier()
  {
    return classIdentifier;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetClassIdentifier(ResourceIdentifier newClassIdentifier, NotificationChain msgs)
  {
    ResourceIdentifier oldClassIdentifier = classIdentifier;
    classIdentifier = newClassIdentifier;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SadlPackage.CLASS_DECLARATION__CLASS_IDENTIFIER, oldClassIdentifier, newClassIdentifier);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setClassIdentifier(ResourceIdentifier newClassIdentifier)
  {
    if (newClassIdentifier != classIdentifier)
    {
      NotificationChain msgs = null;
      if (classIdentifier != null)
        msgs = ((InternalEObject)classIdentifier).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CLASS_DECLARATION__CLASS_IDENTIFIER, null, msgs);
      if (newClassIdentifier != null)
        msgs = ((InternalEObject)newClassIdentifier).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SadlPackage.CLASS_DECLARATION__CLASS_IDENTIFIER, null, msgs);
      msgs = basicSetClassIdentifier(newClassIdentifier, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.CLASS_DECLARATION__CLASS_IDENTIFIER, newClassIdentifier, newClassIdentifier));
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
      case SadlPackage.CLASS_DECLARATION__CLASS_NAME:
        return basicSetClassName(null, msgs);
      case SadlPackage.CLASS_DECLARATION__MUST_BE_ONE_OF:
        return basicSetMustBeOneOf(null, msgs);
      case SadlPackage.CLASS_DECLARATION__DESCRIBED_BY:
        return ((InternalEList<?>)getDescribedBy()).basicRemove(otherEnd, msgs);
      case SadlPackage.CLASS_DECLARATION__CLASS_LIST:
        return basicSetClassList(null, msgs);
      case SadlPackage.CLASS_DECLARATION__CLASS_IDENTIFIER:
        return basicSetClassIdentifier(null, msgs);
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
      case SadlPackage.CLASS_DECLARATION__CLASS_NAME:
        return getClassName();
      case SadlPackage.CLASS_DECLARATION__MUST_BE_ONE_OF:
        return getMustBeOneOf();
      case SadlPackage.CLASS_DECLARATION__DESCRIBED_BY:
        return getDescribedBy();
      case SadlPackage.CLASS_DECLARATION__CLASS_LIST:
        return getClassList();
      case SadlPackage.CLASS_DECLARATION__CLASS_IDENTIFIER:
        return getClassIdentifier();
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
      case SadlPackage.CLASS_DECLARATION__CLASS_NAME:
        setClassName((ResourceName)newValue);
        return;
      case SadlPackage.CLASS_DECLARATION__MUST_BE_ONE_OF:
        setMustBeOneOf((EnumeratedInstances)newValue);
        return;
      case SadlPackage.CLASS_DECLARATION__DESCRIBED_BY:
        getDescribedBy().clear();
        getDescribedBy().addAll((Collection<? extends AddlClassInfo>)newValue);
        return;
      case SadlPackage.CLASS_DECLARATION__CLASS_LIST:
        setClassList((ResourceList)newValue);
        return;
      case SadlPackage.CLASS_DECLARATION__CLASS_IDENTIFIER:
        setClassIdentifier((ResourceIdentifier)newValue);
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
      case SadlPackage.CLASS_DECLARATION__CLASS_NAME:
        setClassName((ResourceName)null);
        return;
      case SadlPackage.CLASS_DECLARATION__MUST_BE_ONE_OF:
        setMustBeOneOf((EnumeratedInstances)null);
        return;
      case SadlPackage.CLASS_DECLARATION__DESCRIBED_BY:
        getDescribedBy().clear();
        return;
      case SadlPackage.CLASS_DECLARATION__CLASS_LIST:
        setClassList((ResourceList)null);
        return;
      case SadlPackage.CLASS_DECLARATION__CLASS_IDENTIFIER:
        setClassIdentifier((ResourceIdentifier)null);
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
      case SadlPackage.CLASS_DECLARATION__CLASS_NAME:
        return className != null;
      case SadlPackage.CLASS_DECLARATION__MUST_BE_ONE_OF:
        return mustBeOneOf != null;
      case SadlPackage.CLASS_DECLARATION__DESCRIBED_BY:
        return describedBy != null && !describedBy.isEmpty();
      case SadlPackage.CLASS_DECLARATION__CLASS_LIST:
        return classList != null;
      case SadlPackage.CLASS_DECLARATION__CLASS_IDENTIFIER:
        return classIdentifier != null;
    }
    return super.eIsSet(featureID);
  }

} //ClassDeclarationImpl
