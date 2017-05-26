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

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.SadlResource
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import com.google.inject.Singleton
import java.nio.file.Path
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.nodemodel.util.NodeModelUtils

import static extension org.eclipse.emf.ecore.util.EcoreUtil.getAllContents

/**
 * Representation of a service that provides the location of a SADL marker in
 * an arbitrary resource.
 * 
 * @author akos.kitta
 */
@ImplementedBy(SadlMarkerLocationProvider.Default)
interface SadlMarkerLocationProvider {

	@Data
	static class Location {

		/**
		 * Shared, unknown location.
		 */
		public static val UNKNOWN = new Location(0, 1, 1);

		/**
		 * An integer value indicating where a text marker starts.
		 * This attribute is zero-relative and inclusive.
		 */
		int start;

		/**
		 * An integer value indicating where a text marker ends. 
		 * This attribute is zero-relative and exclusive.
		 */
		int end;

		/**
		 * An integer value indicating the line number for a text marker. 
		 * This attribute is 1-relative.
		 */
		int lineNumber;

	}

	/**
	 * Returns with the location of the marker argument.
	 */
	def Location getLocation(SadlMarker marker, Resource resource, Path projectLocation);

	@Singleton
	static class Default implements SadlMarkerLocationProvider {

		@Inject
		extension DeclarationExtensions;

		@Override
		override getLocation(SadlMarker marker, Resource resource, Path projectLocation) {
			val eObject = getEObjectForMarker(resource, marker);
			if (eObject === null) {
				return Location.UNKNOWN;
			}
			return eObject.location;
		}

		protected def EObject getEObjectForMarker(Resource resource, SadlMarker marker) {
			return resource.getAllContents(true).filter(SadlResource).filter[concreteName == marker.astNodeName].head;
		}
		
		protected def Location getLocation(EObject eObject) {
			val node = NodeModelUtils.getNode(eObject);
			if (node === null) {
				return Location.UNKNOWN;
			}

			return new Location(node.offset, node.offset + node.length, node.startLine);
		}

	}

}
