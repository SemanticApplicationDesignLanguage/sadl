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
import com.google.inject.ImplementedBy
import com.google.inject.Singleton

/** 
 * Maps a SADL marker severity.
 * 
 * @author akos.kitta
 */
@ImplementedBy(SadlMarkerSeverityMapper.Default)
interface SadlMarkerSeverityMapper {
	
	/** 
	 * Maps the SADL severity argument into the corresponding mapped value and returns with it.
	 */
	def Object map(SadlMarkerSeverity severity);
	
	/**
	 * Default implementation that maps to the ordinal of the SADL marker severity.
	 */
	@Singleton
	static class Default implements SadlMarkerSeverityMapper {

		override map(SadlMarkerSeverity severity) {
			return Preconditions.checkNotNull(severity, "severity").ordinal;
		}
		
	}

}
