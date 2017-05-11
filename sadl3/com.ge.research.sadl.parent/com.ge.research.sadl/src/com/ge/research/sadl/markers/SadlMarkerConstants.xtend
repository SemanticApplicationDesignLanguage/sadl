/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.markers

/**
 * Constants for the SADL marker service.
 * 
 * @author akos.kitta
 */
abstract class SadlMarkerConstants {

	/**
	 * File extension of the SADL error XML.
	 */
	public static val FILE_EXTENSION = 'err';
	
	/**
	 * The error file extension with the {@code '.'}.
	 */
	public static val FILE_EXTENSION_WITH_DOT = '''.«FILE_EXTENSION»''';

	/**
	 * The unique marker type ID.
	 * // TODO we either need to introduce o.e.c.resources or in SADL core or we move the constants to UI.
	 */
	public static val SADL_PROBLEM_MARKER = "com.ge.research.sadl.ui.sadlproblem";

	/**
	 * Marker attribute for storing the origin of the marker. This could be the project
	 * relative path of the error file resource or anything else unless this is unique.
	 */
	public static val ORIGIN_KEY = "ORIGIN_KEY";

	private new() {
	}

}
