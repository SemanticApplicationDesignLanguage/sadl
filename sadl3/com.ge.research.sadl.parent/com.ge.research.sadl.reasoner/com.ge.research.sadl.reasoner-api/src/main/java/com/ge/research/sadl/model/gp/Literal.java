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

package com.ge.research.sadl.model.gp;

import java.math.BigDecimal;

import org.apache.jena.datatypes.xsd.XSDDateTime;
import org.apache.jena.datatypes.xsd.XSDDuration;

/**
 * Class to capture a literal in the SADL grammar
 * @author 200005201
 *
 */
public class Literal extends Node {
	private Object value = null;
	private String units = null;
	private LiteralType literalType = null;
	
	public enum LiteralType {BooleanLiteral, StringLiteral, NumberLiteral}
	
	private String originalText = null;
	
	public Literal() {
		super();
	}
	
	public Literal(LiteralType type) {
		super();
		setLiteralType(type);
	}
	
	public Literal(Object value, String units, LiteralType type) {
		super();
		setValue(value);
		setUnits(units);
		setLiteralType(type);
	}

	/**
	 * Set the value of the Literal
	 * @param value
	 */
	public void setValue(Object value) {
		this.value = value;
	}

	/**
	 * Get the value of the Literal
	 * @return
	 */
	public Object getValue() {
		return value;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Literal other = (Literal) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}
	
	@Override
	public String toString() {
		if (value instanceof String) {
			return "\"" + ((String)value).replace("\"", "\\\"") + "\"";
		}
		String display = value != null ? value.toString() : "null";
		String units = getUnits();
		if (units != null) {
			StringBuilder sb = new StringBuilder();
			if (getName() != null) {
				sb.append(getName());
				sb.append(" (");
				sb.append(display);
				sb.append(" \"");
				sb.append(units);
				sb.append("\")");
			}
			else {
				sb.append(display);
				sb.append(" \"");
				sb.append(units);
				sb.append("\"");
			}
			return sb.toString();
		}
		return display;
	}

	@Override
	public String toFullyQualifiedString() {
		return toString();
	}

	@Override
	public String toDescriptiveString() {
		return toString();
	}

	/**
	 * Get the original text (in the SADL file) of the Literal
	 * @return
	 */
	public String getOriginalText() {
		return originalText;
	}

	/**
	 * Set the original text (in the SADL file) of the Literal
	 * @param originalText
	 */
	public void setOriginalText(String originalText) {
		this.originalText = originalText;
	}

	/**
	 * Get units, if any, of the Literal
	 * @return
	 */
	public String getUnits() {
		return units;
	}

	/**
	 * Set units of the Literal
	 * @param units
	 */
	public void setUnits(String units) {
		this.units = units;
	}

	/**
	 * Set a UnittedQuantity Literal's URI
	 * @param name
	 */
	public void setUri(String name) {
		int hash = name.indexOf('#');
		if (hash > 0 && hash < name.length() - 1) {
			this.namespace = name.substring(0, hash + 1);
			this.name = name.substring(hash + 1);
		}
		else {
		    int colon = name.indexOf(':');
		    if (colon > 0 && colon < name.length() - 1) {
		        this.prefix = name.substring(0, colon);
		        this.name = name.substring(colon + 1);
		    }
		    else {
		        this.prefix = null;
		        this.name = name;
		    }
		}
	}

	public LiteralType getLiteralType() {
		return literalType;
	}

	private void setLiteralType(LiteralType literalType) {
		if (literalType == null && value != null) {
			if (value instanceof XSDDateTime) {
				literalType = LiteralType.StringLiteral;
			} 
			else if (value instanceof XSDDuration) {
				literalType = LiteralType.StringLiteral;
			}
			else if (value instanceof Integer || value instanceof Double ||
					value instanceof Long || value instanceof BigDecimal ||
					value instanceof Number) {
				literalType = LiteralType.NumberLiteral;
			}
			else if (value instanceof Boolean) {
				literalType = LiteralType.BooleanLiteral;
			}
			else if (value.toString().equals("true") ||
					value.toString().equals("false")) {
				literalType = LiteralType.BooleanLiteral;
			}
			else {
				literalType = LiteralType.StringLiteral;
			}
		}
		this.literalType = literalType;
	}

}
