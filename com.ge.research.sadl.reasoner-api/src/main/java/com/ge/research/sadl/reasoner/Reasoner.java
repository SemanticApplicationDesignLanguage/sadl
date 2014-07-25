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

import com.hp.hpl.jena.datatypes.xsd.XSDDatatype;
import com.hp.hpl.jena.rdf.model.Literal;

/**
 * This class implements utility methods believed to of general use to
 * reasoner-specific implementations. Reasoner-specific implementations
 * can extend this class or directly implement IReasoner, as seems best.
 * Note that these methods do use Jena capabilities so if it is desired
 * to not have any Jena dependencies a reasoner should implement IReasoner
 * directly.
 *  
 * $Author: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/26 18:52:10 $
 */
public abstract class Reasoner implements IReasoner {

	private static final String XSD_TYPE_ID_DELIMITED = "<" + XSDDatatype.XSD + ">";

	public static Object xsdStringToObject(String objValue) {
		if ((objValue.indexOf(XSDDatatype.XSD)) > 0) {
			String[] valueAndType = xsdStringToTypeAndValue(objValue);
			if (valueAndType != null) {
				XSDDatatype dt = new XSDDatatype(valueAndType[0]);
				String value = stripDoubleQuotes(valueAndType[1]);
				if (dt.getURI().equals((XSDDatatype.XSDint.getURI()))) {
					return Integer.parseInt(value);
				}
				else if (dt.getURI().equals(XSDDatatype.XSDinteger.getURI())) {
					return Integer.parseInt(value);
				}
				else if (dt.getURI().equals(XSDDatatype.XSDfloat.getURI())) {
					return Float.parseFloat(value);
				}
				else if (dt.getURI().equals(XSDDatatype.XSDdate.getURI())) {
					return value;//.toString();
				}
				else if (dt.getURI().equals(XSDDatatype.XSDdateTime.getURI())) {
					return value;//.toString();
				}
				else if (dt.getURI().equals(XSDDatatype.XSDdouble.getURI())) {
					return Double.parseDouble(value);
				}
				else if (dt.getURI().equals(XSDDatatype.XSDstring.getURI())) {
					return value;
				}
				else if (dt.getURI().equals(XSDDatatype.XSDboolean.getURI())) {
					return Boolean.parseBoolean(value);
				}
			}
		}
		
		return objValue;
	}

	public static String objectToXsdString(Object objValue) throws TranslationException {
		if (objValue == null) {
			return null;
		}
		if (objValue instanceof Literal) {
			return ((Literal)objValue).toString(); // getLexicalForm();
		}
		if (!(objValue instanceof String)) {
			XSDDatatype dtype = null;
			if (objValue instanceof Double) {
				dtype = new XSDDatatype("double");
			}
			else if (objValue instanceof Float) {
				dtype = new XSDDatatype("float");
			}
			else {
				return null;
			}
			return objValue.toString() + "^^" + dtype.getURI();
		}
		else {
			return (String)objValue;
		}
	}

	protected static String[] xsdStringToTypeAndValue(String xsdString) {
		String value = null;
		String type = null;
		int startTypeID = xsdString.indexOf(XSDDatatype.XSD);
		if (startTypeID >= 0) {
			int endVal = xsdString.indexOf("^^", startTypeID - 3);
			if (endVal > 0) {
				value = xsdString.substring(0, endVal);
			}
			else {
				value = xsdString.substring(0, startTypeID);
			}
			type = xsdString.substring(startTypeID + XSDDatatype.XSD.length());
	
		}
		else {
			startTypeID = xsdString.indexOf(XSD_TYPE_ID_DELIMITED);
			if (startTypeID > 0) {
				value = xsdString.substring(0, startTypeID);
				type = xsdString.substring(startTypeID + XSD_TYPE_ID_DELIMITED.length());
			}
		}
		if (type != null) {
			if (type.startsWith("#")) {
				type = type.substring(1);
			}
			if (type.endsWith(">")) {
				type = type.substring(0, type.length() - 1);
			}
			String[] retVal = new String[2];
			retVal[0] = type;
			retVal[1] = value;
			return retVal;
		}
		return null;
	}

	protected static String stripDoubleQuotes(String val) {
		return removeDelimiters(val, "\"", "\"");
	}

	protected static String removeDelimiters(String val, String delStart, String delEnd) {
		if (val.startsWith(delStart) && val.endsWith(delEnd)) {
			return val.substring(1, val.length() - 1);
		}
		return val;
	}

}
