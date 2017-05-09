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

import org.eclipse.xtend.lib.annotations.Data

/**
 * Representation of a SADL marker.
 * 
 * @author akos.kitta
 */
@Data
class SadlMarker {

	/**
	 * Returns with the project relative path to the resource where this marker belongs to. 
	 */
	val String filePath;

	/**
	 * Returns with the human readable message of this marker.
	 * Eventually, this is the description of the marker.
	 */
	val String message;

	/**
	 * The unique name of the AST element this marker attached to in the resource.
	 */
	val String astNodeName;

	/**
	 * The severity of the marker.
	 */
	val SadlMarkerSeverity severity;

}
