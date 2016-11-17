/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ide.editor.coloring

import org.eclipse.lsp4j.ColoringStyle

/**
 * Coloring style IDs for the {@code SADL} language.
 */
abstract class SadlColoringStyle extends ColoringStyle {

	public static val DEFAULT_ID = 27;
	public static val URI_ID = 28;
	public static val CLASS_ID = 29;
	public static val VARIABLE_ID = 30;
	public static val INSTANCE_ID = 31;
	public static val RDFDATATYPE_ID = 32;
	public static val RDF_PROPERTY_ID = 33;
	public static val FUNCTION_NAME_ID = 34;
	public static val DATA_PROPERTY_ID = 35;
	public static val OBJECT_PROPERTY_ID = 36;
	public static val ANNOTATION_PROPERTY_ID = 37;

	private new() {
		// Only for constants.
	}
	
}