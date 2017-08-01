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

import com.ge.research.sadl.markers.SadlMarkerDeserializerService
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.common.base.StandardSystemProperty
import com.google.inject.Inject
import java.nio.file.Files
import java.nio.file.Paths
import java.util.UUID
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith

import static extension com.ge.research.sadl.tests.helpers.XtendTemplateHelper.*

/**
 * Test for checking whether the default external marker deserializer service preserves the CR'LFs or not. 
 * 
 * @author akos.kitta
 */
@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class GH_216_ExternalMarkersDoesNotPreserveLFs {

	@Inject
	extension SadlMarkerDeserializerService;

	private def createTempFile(CharSequence content) {
		val tmpRoot = Paths.get(StandardSystemProperty.JAVA_IO_TMPDIR.value);
		val tmpFile = Files.createTempFile(tmpRoot, UUID.randomUUID.toString, null);
		org.eclipse.xtext.util.Files.writeStringIntoFile(tmpFile.toString, content.toString);
		return tmpFile;
	}

	@Test
	def void checkPreserveLFs() {
		val expectedMessage = '''
			some text
			
				after LF
			
			end
		'''.unifyEOL;
		val actualMessage = '''
			<?xml version="1.0" encoding="utf-8"?>
			<MarkerService>
			  <Process build="str1234" name="str1234" pn="str1234">
			    <Marker msgName="str1234" msgText="some text
			
				after LF
			
			end
			" projectMarker="true" time="2012-12-13T12:12:12" markerType="Error" priority="Low">
			      <ObjectID>str1234</ObjectID>
			    </Marker>
			  </Process>
			</MarkerService>
		'''.unifyEOL.createTempFile.deserialize.head.message;

		Assert.assertEquals(expectedMessage, actualMessage);
	}

}
