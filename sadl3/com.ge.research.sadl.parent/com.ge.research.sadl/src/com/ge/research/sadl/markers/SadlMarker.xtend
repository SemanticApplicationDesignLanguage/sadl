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

import com.google.common.base.Preconditions
import org.eclipse.xtend.lib.annotations.Data

/**
 * Representation of a SADL marker.
 * 
 * @author akos.kitta
 */
@Data
class SadlMarker {

	/**
	 * Copies the marker argument and returns with a new instance after setting the missing model URI.
	 * Throws an exception if the model URI is neither empty nor {@code null}.
	 */
	static def copyWithModelUri(SadlMarker it, String newModelUri) {
		Preconditions.checkArgument(!isModelUriSet, '''Expected empty or null model URI. Was: '«modelUri»'.''');
		return new SadlMarker(newModelUri, message, astNodeName, severity);
	}

	/**
	 * Returns with the URI of the model where this marker belongs to. 
	 */
	val String modelUri;

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

	/**
	 * Returns with {@code true} if the model URI is neither {@code null} nor empty string.
	 */
	def boolean isModelUriSet() {
		return modelUri !== null && !modelUri.empty;
	}

}
