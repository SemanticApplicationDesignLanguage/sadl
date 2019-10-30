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

/**
 * This class encapsulates all of the information necessary to a create
 * a GUI to set a particular type of configuration setting. The 
 * categoryHierarchy relates this item to the root of the configuration 
 * RDF structure. 
 * 
 * $Author: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/26 18:52:10 $
 *
 */
public class ConfigurationOption {
	private String[] categoryHierarchy;
	private String name;
	private String description;
	private int minNumValues = 0;	// default--doesn't have to be set
	private int maxNumValues = 1;	// default--single-valued = 0
	private Object value;
	private Object[] values;
	private Object[] possibleValues;
	
	public ConfigurationOption(String[] _categoryHierarchy, String _name, 
			String desc, Object _value) {
		setCategoryHierarchy(_categoryHierarchy);
		setName(_name);
		setDescription(desc);
		setValue(_value);
	}
	
	public ConfigurationOption(String[] _category, String _name, 
			String desc, Object _value, Object[] _possibleValues) {
		this(_category, _name, desc, _value);
		setPossibleValues(_possibleValues);
	}

	public ConfigurationOption(String[] _category, String _name, 
			String desc, Object _value, Object[] _possibleValues, int minNumValues, int maxNumVallues) {
		this(_category, _name, desc, _value);
		setPossibleValues(_possibleValues);
		setMinNumValues(minNumValues);
		setMaxNumValues(maxNumVallues);
	}

	private void setPossibleValues(Object[] possibleValues) {
		this.possibleValues = possibleValues;
	}

	public Object[] getPossibleValues() {
		return possibleValues;
	}

	private void setValue(Object value) {
		this.value = value;
	}

	public Object getValue() {
		return value;
	}

	private void setDescription(String description) {
		this.description = description;
	}

	public String getDescription() {
		return description;
	}

	private void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	private void setCategoryHierarchy(String[] category) {
		categoryHierarchy = category;
	}

	public String[] getCategoryHierarchy() {
		return categoryHierarchy;
	}

	public void setValues(Object[] values) {
		this.values = values;
	}

	public Object[] getValues() {
		return values;
	}

	public void setMaxNumValues(int maxNumValues) {
		this.maxNumValues = maxNumValues;
	}

	public int getMaxNumValues() {
		return maxNumValues;
	}

	public void setMinNumValues(int minNumValues) {
		this.minNumValues = minNumValues;
	}

	public int getMinNumValues() {
		return minNumValues;
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Configuration Option: " + getName());
		Object[] posvals = getPossibleValues();
		if (posvals != null && posvals.length > 0) {
			for (int i = 0; i < posvals.length; i++) {
				sb.append("  Possible value: " + posvals[i]);
			}
		}
		else {
			sb.append("  Values unconstrained");
		}
		
		return sb.toString();
	}
}
