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
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import org.eclipse.xtend.lib.annotations.Data
import org.jsoup.Jsoup

import static extension com.ge.research.sadl.markers.SadlMarkerSeverity.getSeverityByName

/**
 * Service for parsing the SADL error-marker ({@code .err}) files and returning
 * the markers as POJOs.
 * 
 * @author akos.kitta
 */
@ImplementedBy(SadlMarkerDeserializerService.JsoupDeserializer)
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

		override iterator() {
			return Iterators.unmodifiableIterator(markers.iterator);
		}

	}

	/**
	 * Default parser with a <a href="https://jsoup.org">Jsoup-based</a> implementation.
	 */
	static class JsoupDeserializer implements SadlMarkerDeserializerService {

		/**
		 * The tag name of the markers.
		 */
		static val MARKER_NAME = "Marker";

		/**
		 * This is the severity of the marker.
		 */
		static val MARKER_TYPE_NAME = "markerType";

		/**
		 * The human readable text/description of the marker.
		 */
		static val MESSAGE_TEXT_NAME = "msgText";

		/**
		 * The unique object ID that is used to identify the resource where the SADL marker is attached to. 
		 */
		static val OBJECT_ID_NAME = "ObjectID";

		/**
		 * Separator for the FQN of the object.
		 */
		static val OBJECT_ID_SEPARATOR = "#";

		override deserialize(Path path) {
			val origin = '''«path.parent.fileName»/«path.fileName»''';
			val file = path.toFile;
			if (!file.exists) {
				return new SadlMarkerInfos(origin, emptyList);
			}

			val markers = ImmutableList.builder;
			Jsoup.parse(file, StandardCharsets.UTF_8.name).getElementsByTag(MARKER_NAME).forEach [
				val message = attributes.get(MESSAGE_TEXT_NAME);
				val severity = attributes.get(MARKER_TYPE_NAME).severityByName;
				getElementsByTag(OBJECT_ID_NAME).forEach [
					val fqn = text;
					if (!fqn.nullOrEmpty) {
						val segments = fqn.split(OBJECT_ID_SEPARATOR);
						switch (segments.length) {
							case 1: markers.add(new SadlMarker(null, message, segments.head, severity))
							case 2: markers.add(new SadlMarker(segments.head, message, segments.last, severity))
							default: throw new IllegalArgumentException('''Unexpected FQN: «fqn»''')
						}
					}
				];
			];
			return new SadlMarkerInfos(origin, markers.build);
		}

	}

}
