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
package com.ge.research.sadl.ui.markers

import com.ge.research.sadl.markers.SadlMarkerSeverity
import com.ge.research.sadl.markers.SadlMarkerSeverityMapper
import com.google.common.base.Preconditions
import com.google.common.collect.ImmutableMap

import static com.ge.research.sadl.markers.SadlMarkerSeverity.*
import static org.eclipse.core.resources.IMarker.*

/**
 * Maps the SADL marker severities values to the Eclipse-based {@code IMarker} 
 * severity constants.
 * 
 * @author akos.kitta 
 */
class EclipseMarkerSeverityMapper implements SadlMarkerSeverityMapper {

	static val MAPPING = ImmutableMap.of(INFO, SEVERITY_INFO, WARNING, SEVERITY_WARNING, ERROR, SEVERITY_ERROR);

	@Override
	override map(SadlMarkerSeverity severity) {
		val mapping = MAPPING.get(Preconditions.checkNotNull(severity, "severity"));
		return Preconditions.checkNotNull(mapping, '''Cannot find severity mapping for «severity».''');
	}

}
