/************************************************************************
 * Copyright 2007-2010 - General Electric Company, All Rights Reserved
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

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/26 18:52:10 $
 ***********************************************************************/
package com.ge.research.sadl.reasoner;

/**
 * Class to encapsulate information about the available plug-ins for SADL 
 * translation and reasoning.
 *
 */
public class AvailablePlugin {
	enum PluginType{Translator, Reasoner}
	
	private PluginType pluginType;
	private String categoryName;
	private String family;
	private String className;
	
	public AvailablePlugin(PluginType pt, String category, String _family, String clsNm) {
		setPluginType(pt);
		setCategoryName(category);
		setFamily(_family);
		setClassName(clsNm);
	}
	
	private void setCategoryName(String categoryName) {
		this.categoryName = categoryName;
	}
	
	public String getCategoryName() {
		return categoryName;
	}
	
	private void setFamily(String family) {
		this.family = family;
	}
	
	public String getFamily() {
		return family;
	}
	
	private void setClassName(String className) {
		this.className = className;
	}
	
	public String getClassName() {
		return className;
	}

	private void setPluginType(PluginType pluginType) {
		this.pluginType = pluginType;
	}

	public PluginType getPluginType() {
		return pluginType;
	}
	
}
