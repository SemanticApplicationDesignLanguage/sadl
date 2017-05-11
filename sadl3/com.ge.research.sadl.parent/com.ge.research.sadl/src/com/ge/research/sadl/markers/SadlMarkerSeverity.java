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
package com.ge.research.sadl.markers;

import java.util.Arrays;

import com.google.common.base.Preconditions;

/**
 * Enumeration of SADL marker severities.
 * 
 * @author akos.kitta
 */
public enum SadlMarkerSeverity {

	/**
	 * Info.
	 */
	INFO,

	/**
	 * Warning.
	 */
	WARNING,

	/**
	 * Error.
	 */
	ERROR;

	/**
	 * Returns with the severity with the given name. The desired name argument
	 * is case insensitive.
	 */
	public static SadlMarkerSeverity getSeverityByName(String name) {
		Preconditions.checkNotNull(name, "name");
		return Arrays.stream(SadlMarkerSeverity.values())
				.filter(severity -> severity.toString().equals(name.toUpperCase())).findFirst()
				.orElseThrow(() -> new IllegalArgumentException("Unexpected severity type: " + name));
	}

}
