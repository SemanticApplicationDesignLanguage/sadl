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

	public static val DEFAULT_ID = 0;
	public static val URI_ID = ColoringStyle.Attribute;
	public static val CLASS_ID = ColoringStyle.Type;
	public static val VARIABLE_ID = ColoringStyle.Constant;
	public static val INSTANCE_ID = ColoringStyle.Tag;
	public static val RDF_DATATYPE_ID = ColoringStyle.Warn_token;
	public static val RDF_PROPERTY_ID = ColoringStyle.Namespace;
	public static val FUNCTION_NAME_ID = ColoringStyle.Predefined;
	public static val DATA_PROPERTY_ID = ColoringStyle.Constructor;
	public static val OBJECT_PROPERTY_ID = ColoringStyle.Constructor;
	public static val ANNOTATION_PROPERTY_ID = ColoringStyle.Constructor;

	private new() {
		// Only for constants.
	}
	
}