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

/**
 * Class to capture a literal in the SADL grammar
 * @author 200005201
 *
 */
public class Literal extends Node {
	private Object value = null;
	private String units = null;
	
	private String originalText = null;

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
//			if (!StringUtils.isAlphanumeric(units)) {
				units = "\"" + units + "\"";
//			}
		}
		return (units != null) ? display + " " + units : display;
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

}
