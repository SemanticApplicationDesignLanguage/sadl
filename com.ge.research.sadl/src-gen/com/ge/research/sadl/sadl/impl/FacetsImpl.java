/**
 */
package com.ge.research.sadl.sadl.impl;

import com.ge.research.sadl.sadl.Facets;
import com.ge.research.sadl.sadl.SadlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EDataTypeEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Facets</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link com.ge.research.sadl.sadl.impl.FacetsImpl#getMinexin <em>Minexin</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.FacetsImpl#getMin <em>Min</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.FacetsImpl#getMax <em>Max</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.FacetsImpl#getMaxexin <em>Maxexin</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.FacetsImpl#getRegex <em>Regex</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.FacetsImpl#getLen <em>Len</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.FacetsImpl#getMinlen <em>Minlen</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.FacetsImpl#getMaxlen <em>Maxlen</em>}</li>
 *   <li>{@link com.ge.research.sadl.sadl.impl.FacetsImpl#getValues <em>Values</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class FacetsImpl extends MinimalEObjectImpl.Container implements Facets
{
  /**
   * The default value of the '{@link #getMinexin() <em>Minexin</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMinexin()
   * @generated
   * @ordered
   */
  protected static final String MINEXIN_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getMinexin() <em>Minexin</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMinexin()
   * @generated
   * @ordered
   */
  protected String minexin = MINEXIN_EDEFAULT;

  /**
   * The default value of the '{@link #getMin() <em>Min</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMin()
   * @generated
   * @ordered
   */
  protected static final String MIN_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getMin() <em>Min</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMin()
   * @generated
   * @ordered
   */
  protected String min = MIN_EDEFAULT;

  /**
   * The default value of the '{@link #getMax() <em>Max</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMax()
   * @generated
   * @ordered
   */
  protected static final String MAX_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getMax() <em>Max</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMax()
   * @generated
   * @ordered
   */
  protected String max = MAX_EDEFAULT;

  /**
   * The default value of the '{@link #getMaxexin() <em>Maxexin</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMaxexin()
   * @generated
   * @ordered
   */
  protected static final String MAXEXIN_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getMaxexin() <em>Maxexin</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMaxexin()
   * @generated
   * @ordered
   */
  protected String maxexin = MAXEXIN_EDEFAULT;

  /**
   * The default value of the '{@link #getRegex() <em>Regex</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRegex()
   * @generated
   * @ordered
   */
  protected static final String REGEX_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getRegex() <em>Regex</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRegex()
   * @generated
   * @ordered
   */
  protected String regex = REGEX_EDEFAULT;

  /**
   * The default value of the '{@link #getLen() <em>Len</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLen()
   * @generated
   * @ordered
   */
  protected static final String LEN_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getLen() <em>Len</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getLen()
   * @generated
   * @ordered
   */
  protected String len = LEN_EDEFAULT;

  /**
   * The default value of the '{@link #getMinlen() <em>Minlen</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMinlen()
   * @generated
   * @ordered
   */
  protected static final String MINLEN_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getMinlen() <em>Minlen</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMinlen()
   * @generated
   * @ordered
   */
  protected String minlen = MINLEN_EDEFAULT;

  /**
   * The default value of the '{@link #getMaxlen() <em>Maxlen</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMaxlen()
   * @generated
   * @ordered
   */
  protected static final String MAXLEN_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getMaxlen() <em>Maxlen</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMaxlen()
   * @generated
   * @ordered
   */
  protected String maxlen = MAXLEN_EDEFAULT;

  /**
   * The cached value of the '{@link #getValues() <em>Values</em>}' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getValues()
   * @generated
   * @ordered
   */
  protected EList<String> values;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected FacetsImpl()
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
    return SadlPackage.Literals.FACETS;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getMinexin()
  {
    return minexin;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setMinexin(String newMinexin)
  {
    String oldMinexin = minexin;
    minexin = newMinexin;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.FACETS__MINEXIN, oldMinexin, minexin));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getMin()
  {
    return min;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setMin(String newMin)
  {
    String oldMin = min;
    min = newMin;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.FACETS__MIN, oldMin, min));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getMax()
  {
    return max;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setMax(String newMax)
  {
    String oldMax = max;
    max = newMax;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.FACETS__MAX, oldMax, max));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getMaxexin()
  {
    return maxexin;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setMaxexin(String newMaxexin)
  {
    String oldMaxexin = maxexin;
    maxexin = newMaxexin;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.FACETS__MAXEXIN, oldMaxexin, maxexin));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getRegex()
  {
    return regex;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setRegex(String newRegex)
  {
    String oldRegex = regex;
    regex = newRegex;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.FACETS__REGEX, oldRegex, regex));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getLen()
  {
    return len;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setLen(String newLen)
  {
    String oldLen = len;
    len = newLen;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.FACETS__LEN, oldLen, len));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getMinlen()
  {
    return minlen;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setMinlen(String newMinlen)
  {
    String oldMinlen = minlen;
    minlen = newMinlen;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.FACETS__MINLEN, oldMinlen, minlen));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getMaxlen()
  {
    return maxlen;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setMaxlen(String newMaxlen)
  {
    String oldMaxlen = maxlen;
    maxlen = newMaxlen;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SadlPackage.FACETS__MAXLEN, oldMaxlen, maxlen));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<String> getValues()
  {
    if (values == null)
    {
      values = new EDataTypeEList<String>(String.class, this, SadlPackage.FACETS__VALUES);
    }
    return values;
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
      case SadlPackage.FACETS__MINEXIN:
        return getMinexin();
      case SadlPackage.FACETS__MIN:
        return getMin();
      case SadlPackage.FACETS__MAX:
        return getMax();
      case SadlPackage.FACETS__MAXEXIN:
        return getMaxexin();
      case SadlPackage.FACETS__REGEX:
        return getRegex();
      case SadlPackage.FACETS__LEN:
        return getLen();
      case SadlPackage.FACETS__MINLEN:
        return getMinlen();
      case SadlPackage.FACETS__MAXLEN:
        return getMaxlen();
      case SadlPackage.FACETS__VALUES:
        return getValues();
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
      case SadlPackage.FACETS__MINEXIN:
        setMinexin((String)newValue);
        return;
      case SadlPackage.FACETS__MIN:
        setMin((String)newValue);
        return;
      case SadlPackage.FACETS__MAX:
        setMax((String)newValue);
        return;
      case SadlPackage.FACETS__MAXEXIN:
        setMaxexin((String)newValue);
        return;
      case SadlPackage.FACETS__REGEX:
        setRegex((String)newValue);
        return;
      case SadlPackage.FACETS__LEN:
        setLen((String)newValue);
        return;
      case SadlPackage.FACETS__MINLEN:
        setMinlen((String)newValue);
        return;
      case SadlPackage.FACETS__MAXLEN:
        setMaxlen((String)newValue);
        return;
      case SadlPackage.FACETS__VALUES:
        getValues().clear();
        getValues().addAll((Collection<? extends String>)newValue);
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
      case SadlPackage.FACETS__MINEXIN:
        setMinexin(MINEXIN_EDEFAULT);
        return;
      case SadlPackage.FACETS__MIN:
        setMin(MIN_EDEFAULT);
        return;
      case SadlPackage.FACETS__MAX:
        setMax(MAX_EDEFAULT);
        return;
      case SadlPackage.FACETS__MAXEXIN:
        setMaxexin(MAXEXIN_EDEFAULT);
        return;
      case SadlPackage.FACETS__REGEX:
        setRegex(REGEX_EDEFAULT);
        return;
      case SadlPackage.FACETS__LEN:
        setLen(LEN_EDEFAULT);
        return;
      case SadlPackage.FACETS__MINLEN:
        setMinlen(MINLEN_EDEFAULT);
        return;
      case SadlPackage.FACETS__MAXLEN:
        setMaxlen(MAXLEN_EDEFAULT);
        return;
      case SadlPackage.FACETS__VALUES:
        getValues().clear();
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
      case SadlPackage.FACETS__MINEXIN:
        return MINEXIN_EDEFAULT == null ? minexin != null : !MINEXIN_EDEFAULT.equals(minexin);
      case SadlPackage.FACETS__MIN:
        return MIN_EDEFAULT == null ? min != null : !MIN_EDEFAULT.equals(min);
      case SadlPackage.FACETS__MAX:
        return MAX_EDEFAULT == null ? max != null : !MAX_EDEFAULT.equals(max);
      case SadlPackage.FACETS__MAXEXIN:
        return MAXEXIN_EDEFAULT == null ? maxexin != null : !MAXEXIN_EDEFAULT.equals(maxexin);
      case SadlPackage.FACETS__REGEX:
        return REGEX_EDEFAULT == null ? regex != null : !REGEX_EDEFAULT.equals(regex);
      case SadlPackage.FACETS__LEN:
        return LEN_EDEFAULT == null ? len != null : !LEN_EDEFAULT.equals(len);
      case SadlPackage.FACETS__MINLEN:
        return MINLEN_EDEFAULT == null ? minlen != null : !MINLEN_EDEFAULT.equals(minlen);
      case SadlPackage.FACETS__MAXLEN:
        return MAXLEN_EDEFAULT == null ? maxlen != null : !MAXLEN_EDEFAULT.equals(maxlen);
      case SadlPackage.FACETS__VALUES:
        return values != null && !values.isEmpty();
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
    result.append(" (minexin: ");
    result.append(minexin);
    result.append(", min: ");
    result.append(min);
    result.append(", max: ");
    result.append(max);
    result.append(", maxexin: ");
    result.append(maxexin);
    result.append(", regex: ");
    result.append(regex);
    result.append(", len: ");
    result.append(len);
    result.append(", minlen: ");
    result.append(minlen);
    result.append(", maxlen: ");
    result.append(maxlen);
    result.append(", values: ");
    result.append(values);
    result.append(')');
    return result.toString();
  }

} //FacetsImpl
