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

import com.google.common.collect.ImmutableList
import com.google.common.collect.Iterators
import com.google.inject.ImplementedBy
import java.nio.file.Path
import javax.xml.parsers.DocumentBuilderFactory
import org.eclipse.xtend.lib.annotations.Data
import org.w3c.dom.NamedNodeMap
import org.w3c.dom.Node

import static extension com.ge.research.sadl.markers.SadlMarkerSeverity.getSeverityByName

/**
 * Service for parsing the SADL error-marker ({@code .err}) files and returning
 * the markers as POJOs.
 * 
 * @author akos.kitta
 */
@ImplementedBy(SadlMarkerDeserializerService.DOM)
interface SadlMarkerDeserializerService {

	/**
	 * Deserializes the XML SADL error-marker file.
	 */
	def SadlMarkerInfos deserialize(Path path);

	/**
	 * Wraps zero to any SADL markers and their origin.
	 */
	@Data
	static class SadlMarkerInfos implements Iterable<SadlMarker> {

		/**
		 * The unique path or identifier of the error ({@code .err}) file where the markers
		 * are coming from.
		 */
		val String origin;

		/**
		 * An iterable of external SADL markers based on the content of the XML.
		 */
		val Iterable<SadlMarker> markers;
		
		@Override
		override iterator() {
			return Iterators.unmodifiableIterator(markers.iterator);
		}

	}

	/**
	 * Default DOM based parser implementation.
	 */
	static class DOM implements SadlMarkerDeserializerService {

		/**
		 * The tag name of the markers.
		 */
		static val MARKER_NAME = "Marker";

		/**
		 * This is the project relative path of the resource which has the marker attached to it. 
		 */
		static val FILE_PATH_NAME = "filePath";

		/**
		 * This is the severity of the marker.
		 */
		static val MARKER_TYPE_NAME = "markerType";

		/**
		 * The human readable text/description of the marker.
		 */
		static val MESSAGE_TEXT_NAME = "msgText";

		/**
		 * The unique name of the AST node which the marker is attached to.
		 */
		static val AST_NODE_NAME = "astNodeName";

		@Override
		override deserialize(Path path) {
			val factory = DocumentBuilderFactory.newInstance();
			val builder = factory.newDocumentBuilder();
			val doc = builder.parse(path.toFile);
			doc.documentElement.normalize;

			val markers = ImmutableList.builder;
			val origin = '''«path.parent.fileName»/«path.fileName»''';
			val elements = doc.getElementsByTagName(MARKER_NAME);
			for (var i = 0; i < elements.length; i++) {
				val element = elements.item(i);
				if (element.nodeType === Node.ELEMENT_NODE) {
					val attributes = element.attributes;
					val filePath = attributes.getTextContentOfNamedItem(FILE_PATH_NAME);
					val message = attributes.getTextContentOfNamedItem(MESSAGE_TEXT_NAME);
					val severity = attributes.getTextContentOfNamedItem(MARKER_TYPE_NAME).severityByName;
					val astNodeName = attributes.getTextContentOfNamedItem(AST_NODE_NAME);
					markers.add(new SadlMarker(filePath, message, astNodeName, severity));
				}
			}

			return new SadlMarkerInfos(origin, markers.build);
		}

		private def getTextContentOfNamedItem(NamedNodeMap attributes, String name) {
			val node = attributes.getNamedItem(name);
			return if(node === null) "" else node.textContent;
		}

	}

}
