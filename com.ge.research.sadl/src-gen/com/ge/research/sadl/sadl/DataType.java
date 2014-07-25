/**
 */
package com.ge.research.sadl.sadl;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Data Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see com.ge.research.sadl.sadl.SadlPackage#getDataType()
 * @model
 * @generated
 */
public enum DataType implements Enumerator
{
  /**
   * The '<em><b>String</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #STRING_VALUE
   * @generated
   * @ordered
   */
  STRING(0, "string", "string"),

  /**
   * The '<em><b>Boolean</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #BOOLEAN_VALUE
   * @generated
   * @ordered
   */
  BOOLEAN(1, "boolean", "boolean"),

  /**
   * The '<em><b>Decimal</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #DECIMAL_VALUE
   * @generated
   * @ordered
   */
  DECIMAL(2, "decimal", "decimal"),

  /**
   * The '<em><b>Int</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #INT_VALUE
   * @generated
   * @ordered
   */
  INT(3, "int", "int"),

  /**
   * The '<em><b>Long</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #LONG_VALUE
   * @generated
   * @ordered
   */
  LONG(4, "long", "long"),

  /**
   * The '<em><b>Float</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #FLOAT_VALUE
   * @generated
   * @ordered
   */
  FLOAT(5, "float", "float"),

  /**
   * The '<em><b>Double</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #DOUBLE_VALUE
   * @generated
   * @ordered
   */
  DOUBLE(6, "double", "double"),

  /**
   * The '<em><b>Duration</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #DURATION_VALUE
   * @generated
   * @ordered
   */
  DURATION(7, "duration", "duration"),

  /**
   * The '<em><b>Date Time</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #DATE_TIME_VALUE
   * @generated
   * @ordered
   */
  DATE_TIME(8, "dateTime", "dateTime"),

  /**
   * The '<em><b>Time</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #TIME_VALUE
   * @generated
   * @ordered
   */
  TIME(9, "time", "time"),

  /**
   * The '<em><b>Date</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #DATE_VALUE
   * @generated
   * @ordered
   */
  DATE(10, "date", "date"),

  /**
   * The '<em><b>GYear Month</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #GYEAR_MONTH_VALUE
   * @generated
   * @ordered
   */
  GYEAR_MONTH(11, "gYearMonth", "gYearMonth"),

  /**
   * The '<em><b>GYear</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #GYEAR_VALUE
   * @generated
   * @ordered
   */
  GYEAR(12, "gYear", "gYear"),

  /**
   * The '<em><b>GMonth Day</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #GMONTH_DAY_VALUE
   * @generated
   * @ordered
   */
  GMONTH_DAY(13, "gMonthDay", "gMonthDay"),

  /**
   * The '<em><b>GDay</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #GDAY_VALUE
   * @generated
   * @ordered
   */
  GDAY(14, "gDay", "gDay"),

  /**
   * The '<em><b>GMonth</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #GMONTH_VALUE
   * @generated
   * @ordered
   */
  GMONTH(15, "gMonth", "gMonth"),

  /**
   * The '<em><b>Hex Binary</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #HEX_BINARY_VALUE
   * @generated
   * @ordered
   */
  HEX_BINARY(16, "hexBinary", "hexBinary"),

  /**
   * The '<em><b>Base64 Binary</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #BASE64_BINARY_VALUE
   * @generated
   * @ordered
   */
  BASE64_BINARY(17, "base64Binary", "base64Binary"),

  /**
   * The '<em><b>Any URI</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #ANY_URI_VALUE
   * @generated
   * @ordered
   */
  ANY_URI(18, "anyURI", "anyURI"),

  /**
   * The '<em><b>Data</b></em>' literal object.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #DATA_VALUE
   * @generated
   * @ordered
   */
  DATA(19, "data", "data");

  /**
   * The '<em><b>String</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>String</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #STRING
   * @model name="string"
   * @generated
   * @ordered
   */
  public static final int STRING_VALUE = 0;

  /**
   * The '<em><b>Boolean</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Boolean</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #BOOLEAN
   * @model name="boolean"
   * @generated
   * @ordered
   */
  public static final int BOOLEAN_VALUE = 1;

  /**
   * The '<em><b>Decimal</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Decimal</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #DECIMAL
   * @model name="decimal"
   * @generated
   * @ordered
   */
  public static final int DECIMAL_VALUE = 2;

  /**
   * The '<em><b>Int</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Int</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #INT
   * @model name="int"
   * @generated
   * @ordered
   */
  public static final int INT_VALUE = 3;

  /**
   * The '<em><b>Long</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Long</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #LONG
   * @model name="long"
   * @generated
   * @ordered
   */
  public static final int LONG_VALUE = 4;

  /**
   * The '<em><b>Float</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Float</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #FLOAT
   * @model name="float"
   * @generated
   * @ordered
   */
  public static final int FLOAT_VALUE = 5;

  /**
   * The '<em><b>Double</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Double</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #DOUBLE
   * @model name="double"
   * @generated
   * @ordered
   */
  public static final int DOUBLE_VALUE = 6;

  /**
   * The '<em><b>Duration</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Duration</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #DURATION
   * @model name="duration"
   * @generated
   * @ordered
   */
  public static final int DURATION_VALUE = 7;

  /**
   * The '<em><b>Date Time</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Date Time</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #DATE_TIME
   * @model name="dateTime"
   * @generated
   * @ordered
   */
  public static final int DATE_TIME_VALUE = 8;

  /**
   * The '<em><b>Time</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Time</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #TIME
   * @model name="time"
   * @generated
   * @ordered
   */
  public static final int TIME_VALUE = 9;

  /**
   * The '<em><b>Date</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Date</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #DATE
   * @model name="date"
   * @generated
   * @ordered
   */
  public static final int DATE_VALUE = 10;

  /**
   * The '<em><b>GYear Month</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>GYear Month</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #GYEAR_MONTH
   * @model name="gYearMonth"
   * @generated
   * @ordered
   */
  public static final int GYEAR_MONTH_VALUE = 11;

  /**
   * The '<em><b>GYear</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>GYear</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #GYEAR
   * @model name="gYear"
   * @generated
   * @ordered
   */
  public static final int GYEAR_VALUE = 12;

  /**
   * The '<em><b>GMonth Day</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>GMonth Day</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #GMONTH_DAY
   * @model name="gMonthDay"
   * @generated
   * @ordered
   */
  public static final int GMONTH_DAY_VALUE = 13;

  /**
   * The '<em><b>GDay</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>GDay</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #GDAY
   * @model name="gDay"
   * @generated
   * @ordered
   */
  public static final int GDAY_VALUE = 14;

  /**
   * The '<em><b>GMonth</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>GMonth</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #GMONTH
   * @model name="gMonth"
   * @generated
   * @ordered
   */
  public static final int GMONTH_VALUE = 15;

  /**
   * The '<em><b>Hex Binary</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Hex Binary</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #HEX_BINARY
   * @model name="hexBinary"
   * @generated
   * @ordered
   */
  public static final int HEX_BINARY_VALUE = 16;

  /**
   * The '<em><b>Base64 Binary</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Base64 Binary</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #BASE64_BINARY
   * @model name="base64Binary"
   * @generated
   * @ordered
   */
  public static final int BASE64_BINARY_VALUE = 17;

  /**
   * The '<em><b>Any URI</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Any URI</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #ANY_URI
   * @model name="anyURI"
   * @generated
   * @ordered
   */
  public static final int ANY_URI_VALUE = 18;

  /**
   * The '<em><b>Data</b></em>' literal value.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of '<em><b>Data</b></em>' literal object isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @see #DATA
   * @model name="data"
   * @generated
   * @ordered
   */
  public static final int DATA_VALUE = 19;

  /**
   * An array of all the '<em><b>Data Type</b></em>' enumerators.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private static final DataType[] VALUES_ARRAY =
    new DataType[]
    {
      STRING,
      BOOLEAN,
      DECIMAL,
      INT,
      LONG,
      FLOAT,
      DOUBLE,
      DURATION,
      DATE_TIME,
      TIME,
      DATE,
      GYEAR_MONTH,
      GYEAR,
      GMONTH_DAY,
      GDAY,
      GMONTH,
      HEX_BINARY,
      BASE64_BINARY,
      ANY_URI,
      DATA,
    };

  /**
   * A public read-only list of all the '<em><b>Data Type</b></em>' enumerators.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public static final List<DataType> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

  /**
   * Returns the '<em><b>Data Type</b></em>' literal with the specified literal value.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public static DataType get(String literal)
  {
    for (int i = 0; i < VALUES_ARRAY.length; ++i)
    {
      DataType result = VALUES_ARRAY[i];
      if (result.toString().equals(literal))
      {
        return result;
      }
    }
    return null;
  }

  /**
   * Returns the '<em><b>Data Type</b></em>' literal with the specified name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public static DataType getByName(String name)
  {
    for (int i = 0; i < VALUES_ARRAY.length; ++i)
    {
      DataType result = VALUES_ARRAY[i];
      if (result.getName().equals(name))
      {
        return result;
      }
    }
    return null;
  }

  /**
   * Returns the '<em><b>Data Type</b></em>' literal with the specified integer value.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public static DataType get(int value)
  {
    switch (value)
    {
      case STRING_VALUE: return STRING;
      case BOOLEAN_VALUE: return BOOLEAN;
      case DECIMAL_VALUE: return DECIMAL;
      case INT_VALUE: return INT;
      case LONG_VALUE: return LONG;
      case FLOAT_VALUE: return FLOAT;
      case DOUBLE_VALUE: return DOUBLE;
      case DURATION_VALUE: return DURATION;
      case DATE_TIME_VALUE: return DATE_TIME;
      case TIME_VALUE: return TIME;
      case DATE_VALUE: return DATE;
      case GYEAR_MONTH_VALUE: return GYEAR_MONTH;
      case GYEAR_VALUE: return GYEAR;
      case GMONTH_DAY_VALUE: return GMONTH_DAY;
      case GDAY_VALUE: return GDAY;
      case GMONTH_VALUE: return GMONTH;
      case HEX_BINARY_VALUE: return HEX_BINARY;
      case BASE64_BINARY_VALUE: return BASE64_BINARY;
      case ANY_URI_VALUE: return ANY_URI;
      case DATA_VALUE: return DATA;
    }
    return null;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private final int value;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private final String name;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private final String literal;

  /**
   * Only this class can construct instances.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private DataType(int value, String name, String literal)
  {
    this.value = value;
    this.name = name;
    this.literal = literal;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public int getValue()
  {
    return value;
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
  public String getLiteral()
  {
    return literal;
  }

  /**
   * Returns the literal value of the enumerator, which is its string representation.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String toString()
  {
    return literal;
  }
  
} //DataType
