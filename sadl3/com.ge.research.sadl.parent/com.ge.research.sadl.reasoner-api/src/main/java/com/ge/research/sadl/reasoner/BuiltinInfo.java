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

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/26 18:52:10 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

/**
 * Class to encapsulate the configuration information of a builtin
 */
public class BuiltinInfo {
	private String name;
	private String className;
	private String reasonerFamily;
	private int numArgs;
	private String signature;
	
	public BuiltinInfo(String name, String className, String family) {
		setName(name);
		setClassName(className);
		setReasonerFamily(family);
	}
	
	public BuiltinInfo(String name, String className, String family, int numArgs) {
		setName(name);
		setClassName(className);
		setReasonerFamily(family);
		setNumArgs(numArgs);
	}
	
	public String getUri() {
		String uri = getClassName().substring(0, getClassName().lastIndexOf('.'));
		uri += "#";
		uri += getName();
		return uri;
	}
	
	private void setReasonerFamily(String reasonerFamily) {
		this.reasonerFamily = reasonerFamily;
	}
	
	public String getReasonerFamily() {
		return reasonerFamily;
	}
	
	private void setClassName(String className) {
		this.className = className;
	}
	
	public String getClassName() {
		return className;
	}
	
	private void setName(String name) {
		this.name = name;
	}
	
	public String getName() {
		return name;
	}
	
	private void setNumArgs(int args) {
		numArgs = args;
	}
	
	public int getNumArgs() {
		return numArgs;
	}

	public String getSignature() {
		return signature;
	}

	public void setSignature(String signature) {
		this.signature = signature;
	}
}
