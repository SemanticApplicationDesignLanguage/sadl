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
	 * Returns with the location of the AST node identified with its name from the resource.
	 */
	def Location getLocation(String name, Resource resource);

	/**
	 * Returns with the AST node identified with its {@code name} argument. If not found, returns {@code null}.
	 */
	def EObject getEObjectByName(String name, Resource resource);

	@Singleton
	static class Default implements SadlMarkerLocationProvider {

		@Inject
		extension DeclarationExtensions;

		override getLocation(String name, Resource resource) {
			val eObject = getEObjectByName(name, resource);
			if (eObject === null) {
				return Location.UNKNOWN;
			}
			return eObject.location;
		}

		override def EObject getEObjectByName(String name, Resource resource) {
			val sadlResources = resource.getAllContents(true).filter(SadlResource);
			return sadlResources.filter[concreteName == name].map[declaration].filterNull.head;
		}

		/**
		 * Transforms the AST node into a location instance.
		 */
		protected def Location getLocation(EObject eObject) {
			val node = NodeModelUtils.getNode(eObject);
			if (node === null) {
				return Location.UNKNOWN;
			}
			return new Location(node.offset, node.offset + node.length, node.startLine);
		}

	}

}
