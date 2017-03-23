/************************************************************************
 * Copyright 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.processing

/**
 * Service representation of a SADL implicit model fragment provider.
 * On can register a fragment provider to customize the default implicit 
 * model content.
 * 
 * <p>
 * Currently, only appending additional content to the default implicit 
 * model is supported.
 * 
 * @author akos.kitta 
 */
interface ISadlImplicitModelFragmentProvider {
	
	/**
	 * The unique extension point ID for the Eclipse-based service discovery.
	 */
	static val EXTENSION_POINT_ID = 'com.ge.research.sadl.sadl_implicit_mode_fragment_provider';
	
	/**
	 * The attribute ID of the extension point to create a new registered instance.
	 */
	static val EXECUTABLE_ATTRIBUTE_ID = 'class';
	
	/**
	 * Returns with any arbitrary implicit model fragment that
	 * will be appended to the default implicit SADL model. 
	 */
	def String getFragmentToAppend();
	
	/**
	 * Callback method that is being invoked after creating an instance of this interface.
	 * 
	 * <p>
	 * This method does nothing by default. One can override this method and for instance
	 * perform a Guice injection on demand.
	 */
	def void afterCreation() {
		
	}
	
}
