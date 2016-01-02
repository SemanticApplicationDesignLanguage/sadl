/************************************************************************
 * Copyright \u00a9 2007-2010 - General Electric Company, All Rights Reserved
 *
 * Project: SADL
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;

/**
 * This class implements utility methods believed to of general use to
 * reasoner-specific implementations. Reasoner-specific implementations
 * can extend this class or directly implement IReasoner, as seems best.
 * Note that these methods do use Jena capabilities so if it is desired
 * to not have any Jena dependencies a reasoner should implement IReasoner
 * directly.
 *  
 * $Author: crapo $ 
 * $Revision: 1.2 $ Last modified on   $Date: 2014/10/28 14:42:28 $
 */
public abstract class Reasoner implements IReasoner {

//	private static final String XSD_TYPE_ID_DELIMITED = "<" + XSDDatatype.XSD + ">";
	protected HashMap<String, Object> configuration;

	public static synchronized Object xsdStringToObject(String objValue) {
//		if ((objValue.indexOf(XSDDatatype.XSD)) > 0) {
//			String[] valueAndType = xsdStringToTypeAndValue(objValue);
//			if (valueAndType != null) {
//				XSDDatatype dt = new XSDDatatype(valueAndType[0]);
//				String value = stripDoubleQuotes(valueAndType[1]);
//				if (dt.getURI().equals((XSDDatatype.XSDint.getURI()))) {
//					return Integer.parseInt(value);
//				}
//				else if (dt.getURI().equals(XSDDatatype.XSDinteger.getURI())) {
//					return Integer.parseInt(value);
//				}
//				else if (dt.getURI().equals(XSDDatatype.XSDfloat.getURI())) {
//					return Float.parseFloat(value);
//				}
//				else if (dt.getURI().equals(XSDDatatype.XSDdate.getURI())) {
//					return value;//.toString();
//				}
//				else if (dt.getURI().equals(XSDDatatype.XSDdateTime.getURI())) {
//					return value;//.toString();
//				}
//				else if (dt.getURI().equals(XSDDatatype.XSDdouble.getURI())) {
//					return Double.parseDouble(value);
//				}
//				else if (dt.getURI().equals(XSDDatatype.XSDstring.getURI())) {
//					return value;
//				}
//				else if (dt.getURI().equals(XSDDatatype.XSDboolean.getURI())) {
//					return Boolean.parseBoolean(value);
//				}
//			}
//		}
		
		return objValue;
	}

	public static synchronized String objectToXsdString(Object objValue) throws TranslationException {
		if (objValue == null) {
			return null;
		}
//		if (objValue instanceof Literal) {
//			return ((Literal)objValue).toString(); // getLexicalForm();
//		}
//		if (!(objValue instanceof String)) {
//			XSDDatatype dtype = null;
//			if (objValue instanceof Double) {
//				dtype = new XSDDatatype("double");
//			}
//			else if (objValue instanceof Float) {
//				dtype = new XSDDatatype("float");
//			}
//			else {
//				return null;
//			}
//			return objValue.toString() + "^^" + dtype.getURI();
//		}
//		else {
			return (String)objValue;
//		}
	}

//	protected static String[] xsdStringToTypeAndValue(String xsdString) {
//		String value = null;
//		String type = null;
//		int startTypeID = xsdString.indexOf(XSDDatatype.XSD);
//		if (startTypeID >= 0) {
//			int endVal = xsdString.indexOf("^^", startTypeID - 3);
//			if (endVal > 0) {
//				value = xsdString.substring(0, endVal);
//			}
//			else {
//				value = xsdString.substring(0, startTypeID);
//			}
//			type = xsdString.substring(startTypeID + XSDDatatype.XSD.length());
//	
//		}
//		else {
//			startTypeID = xsdString.indexOf(XSD_TYPE_ID_DELIMITED);
//			if (startTypeID > 0) {
//				value = xsdString.substring(0, startTypeID);
//				type = xsdString.substring(startTypeID + XSD_TYPE_ID_DELIMITED.length());
//			}
//		}
//		if (type != null) {
//			if (type.startsWith("#")) {
//				type = type.substring(1);
//			}
//			if (type.endsWith(">")) {
//				type = type.substring(0, type.length() - 1);
//			}
//			String[] retVal = new String[2];
//			retVal[0] = type;
//			retVal[1] = value;
//			return retVal;
//		}
//		return null;
//	}

	protected static String stripDoubleQuotes(String val) {
		return removeDelimiters(val, "\"", "\"");
	}

	protected static String removeDelimiters(String val, String delStart, String delEnd) {
		if (val.startsWith(delStart) && val.endsWith(delEnd)) {
			return val.substring(1, val.length() - 1);
		}
		return val;
	}

	public static synchronized String now() {
		String DATE_FORMAT_NOW = "yyyy-MM-dd HH:mm:ss";

	    Calendar cal = Calendar.getInstance();
		SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT_NOW);
		return sdf.format(cal.getTime());
	}

	/************ Configuration retrieval methods ***************/
	protected boolean getBooleanConfigurationValue(List<ConfigurationItem> preferences, String configName, boolean defVal) {
		Object derlog = findPreference(preferences, configName);
		if (derlog != null) {
			configure(findConfigurationItem(preferences, configName));
		}
		if (derlog == null && configuration != null) {
			derlog = configuration.get(configName);
		}
		if (derlog != null && derlog instanceof Boolean) {
			return ((Boolean)derlog).booleanValue();
		}
		return defVal;
	}
	
	protected String getStringConfigurationValue(List<ConfigurationItem> preferences, String configName, String defVal) {
		Object val = findPreference(preferences, configName);
		if (val != null) {
			configure(findConfigurationItem(preferences, configName));
		}
		if (val == null && configuration != null) {
			val = configuration.get(configName);
		}
		if (val != null && val instanceof String) {
			return (String)val;
		}
		return defVal;
	}

	protected Object findPreference(List<ConfigurationItem> preferences, String name) {
		if (preferences != null) {
			for (int i = 0; i < preferences.size(); i++) {
				ConfigurationItem citem = preferences.get(i);
				Object val = citem.getNamedValue(name);
				if (val != null) {
					return val;
				}
			}
		}
		return null;
	}
	
	protected ConfigurationItem findConfigurationItem(List<ConfigurationItem> preferences, String name) {
		if (preferences != null) {
			for (int i = 0; i < preferences.size(); i++) {
				ConfigurationItem citem = preferences.get(i);
				Object val = citem.getNamedValue(name);
				if (val != null) {
					return citem;
				}
			}
		}
		return null;
	}

}
