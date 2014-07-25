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

import java.util.ArrayList;
import java.util.List;

/**
 * This class encapsulates all of the information related to a single
 * configuration setting. The categoryHierarchy relates this item to the
 * root of the configuration RDF structure. The list of name value pairs
 * provides the details of the item.
 * 
 * 
 * $Author: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/26 18:52:10 $
 *
 */
public class ConfigurationItem {
	private String[] categoryHierarchy = null;
	private List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>();
	
	public enum ConfigurationType {Bag, Sequence, SingleValue}
	
	/**
	 * The NameValuePair class encapsulates a name with one or more associated values. The cardinality of
	 * a particular ConfigurationItem is controlled by the associated ConfigurationOption.
	 * 
	 * @author 200005201
	 *
	 */
	public static class NameValuePair {
		private String name;
		private Object[] values;
		private ConfigurationType configType = ConfigurationType.SingleValue; // default
		
		public NameValuePair(String _name, Object _value) {
			name = _name;
			setValue(_value);
		}
		
		public NameValuePair(String _name, Object[] _values) {
			name = _name;
			setValues(_values);
		}
		
		public NameValuePair(String _name, Object _value, ConfigurationType _configType) {
			this(_name, _value);
			setConfigType(_configType);
		}
		
		public NameValuePair(String _name, Object[] _values, ConfigurationType _configType) {
			this(_name, _values);
			setConfigType(_configType);
		}
		
		public String getName() {
			return name;
		}

		public void setValue(Object value) {
			if (values == null) {
				values = new Object[1];
				values[0] = value;
			}
			else {
				int size = values.length + 1;
				Object[] newValues = new Object[size];
				for (int i = 0; i < values.length; i++) {
					newValues[i] = values[i];
				}
				newValues[size - 1] = value;
				values = newValues;
			}
		}

		public Object getValue() {
			if (values == null || values.length == 0) {
				return null;
			}
			return values[0];
		}

		public void setConfigType(ConfigurationType configType) {
			this.configType = configType;
		}

		public ConfigurationType getConfigType() {
			return configType;
		}

		public void setValues(Object[] values) {
			this.values = values;
		}

		public Object[] getValues() {
			return values;
		}
	}
	
	public ConfigurationItem(String[] categoryTree) {
		setCategoryHierarchy(categoryTree);
	}
	
	public void addNameValuePair(NameValuePair nvp) {
		nameValuePairs.add(nvp);
	}
	
	public List<NameValuePair> getNameValuePairs() {
		return nameValuePairs;
	}
	
	public void setNameValuePairs(List<NameValuePair> nvps) {
		nameValuePairs = nvps;
	}
	
	public void clearNameValuePairs() {
		nameValuePairs.clear();
	}

	private void setCategoryHierarchy(String[] categoryHierarchy) {
		this.categoryHierarchy = categoryHierarchy;
	}

	public String[] getCategoryHierarchy() {
		return categoryHierarchy;
	}
	
	/** Method to return the leaf (last) category of this ConfigurationItem
	 * 
	 * @return leaf category else null if no categories
	 */
	public String getLeafCategory() {
		if (categoryHierarchy != null && categoryHierarchy.length > 0) {
			return categoryHierarchy[categoryHierarchy.length - 1];
		}
		return null;
	}
	
	/**
	 * Method to return a value (if any) of this ConfigurationItem with the given name. 
	 * If there are more than one the first encountered will be returned.
	 * 
	 * @param name - name to be matched in name value pairs
	 * @return - a value if a match is found else null
	 */
	public Object getNamedValue(String name) {
		for (int i = 0; nameValuePairs != null && i < nameValuePairs.size(); i++) {
			NameValuePair nvp = nameValuePairs.get(i);
			if (nvp.getName().equals(name)) {
				return nvp.getValue();
			}
		}
		return null;
	}
	
	/**
	 * Method to return a value (if any) of this ConfigurationItem with the given name. 
	 * If there are more than one the first encountered will be returned.
	 * 
	 * @param name - name to be matched in name value pairs
	 * @return - a value if a match is found else null
	 */
	public Object[] getNamedValues(String name) {
		for (int i = 0; nameValuePairs != null && i < nameValuePairs.size(); i++) {
			NameValuePair nvp = nameValuePairs.get(i);
			if (nvp.getName().equals(name)) {
				return nvp.getValues();
			}
		}
		return null;
	}
	

	/**
	 * Method to return all of the values (if any) of this ConfigurationItem with the given name.
	 * 
	 * @param name - name to be matched in name value pairs
	 * @return - a list of one or more values associated with the name if there are any else null
	 */
	public List<Object> getAllValuesOfName(String name) {
		List<Object> results = null;
		for (int i = 0; nameValuePairs != null && i < nameValuePairs.size(); i++) {
			NameValuePair nvp = nameValuePairs.get(i);
			if (nvp.getName().equals(name)) {
				if (results == null) {
					results = new ArrayList<Object>();
				}
				Object[] values = nvp.getValues();
				for (int j = 0; j < values.length; j++) {
					results.add(values[j]);
				}
			}
		}
		return results;
	}
}
