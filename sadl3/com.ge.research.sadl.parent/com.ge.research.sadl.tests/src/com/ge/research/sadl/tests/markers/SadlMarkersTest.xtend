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
package com.ge.research.sadl.tests.markers

import com.ge.research.sadl.markers.SadlMarkerImpl
import com.ge.research.sadl.markers.SadlMarkersImpl
import com.ge.research.sadl.markers.api.SadlMarkerSeverity
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.ge.research.sadl.utils.SerializationService
import com.google.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlMarkersTest {

	@Inject
	extension SerializationService;

	@Test
	def void foo() {
		val markers = new SadlMarkersImpl;
		markers.addMarker(new SadlMarkerImpl(
			URI.createFileURI("to/resource/path").toString,
			"some message",
			"testType",
			"name",
			SadlMarkerSeverity.ERROR,
			"project/level/file"
		));
		markers.addMarker(new SadlMarkerImpl(
			URI.createFileURI("to/resource/path").toString,
			"this is just a warning",
			"testType",
			"anotherName",
			SadlMarkerSeverity.WARNING,
			"project/level/file"
		));

		println(markers);
		println(serialize(markers))
		println(deserialize(serialize(markers)));
		//
	}

}
