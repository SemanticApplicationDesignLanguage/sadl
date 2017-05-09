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

import com.ge.research.sadl.markers.api.SadlMarker
import com.ge.research.sadl.markers.api.SadlMarkerSeverity
import com.thoughtworks.xstream.annotations.XStreamAlias
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtend.lib.annotations.EqualsHashCode
import org.eclipse.xtend.lib.annotations.ToString

/**
 * Bare minimum representation of a SADL issue marker.
 * 
 * @author akos.kitta
 */
@EqualsHashCode
@ToString(singleLine=true)
@Accessors(PUBLIC_GETTER, PRIVATE_SETTER)
 @XStreamAlias("marker")
class SadlMarkerImpl implements SadlMarker {

	String uri;

	String message;

	String code;
	
	String name;

	SadlMarkerSeverity severity;
	
	String origin;
	
	public new(String uri, String message, String code, String name, SadlMarkerSeverity severity, String origin) {
		this.uri = uri;
		this.message = message;
		this.code = code;
		this.name = name;
		this.severity = severity;
		this.origin = origin;
	}

	/**
	 * That is used by XStream to be able to create a new instance of this class via reflection.
	 */
	private new() {
	}

}
