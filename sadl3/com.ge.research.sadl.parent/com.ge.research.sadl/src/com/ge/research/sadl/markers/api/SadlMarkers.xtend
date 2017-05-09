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
package com.ge.research.sadl.markers.api

import com.google.common.collect.Iterators
import com.google.common.collect.Lists
import org.eclipse.emf.common.util.URI

/**
 * Representation of a bunch of SADL markers.
 * This is just a sugar for managing, storing, and serializing multiple
 * SADL markers.
 * 
 * @author akos.kitta
 */
interface SadlMarkers extends Iterable<SadlMarker> {

	/**
	 * Returns with all markers.
	 */
	def Iterable<SadlMarker> getMarkers();

	/**
	 * Returns with all markers for an individual resource.
	 */
	def Iterable<SadlMarker> getMarkers(URI uri);

	/**
	 * Adds markers.
	 */
	def void addMarker(SadlMarker marker, SadlMarker... rest) {
		addMarkers(Lists.asList(marker, rest));
	}

	/**
	 * Deletes the marker arguments from this instance.
	 */
	def void deleteMarker(SadlMarker marker, SadlMarker... rest) {
		deleteMarkers(Lists.asList(marker, rest));
	}

	/**
	 * Sets the markers for an arbitrary resource. All existing markers for
	 * the given resource will be overridden.
	 */
	def void setMarker(URI uri, SadlMarker marker, SadlMarker... rest) {
		setMarkers(uri, Lists.asList(marker, rest));
	}

	/**
	 * Adds markers.
	 */
	def void addMarkers(Iterable<? extends SadlMarker> markers);

	/**
	 * Deletes the given markers.
	 */
	def void deleteMarkers(Iterable<? extends SadlMarker> markers);

	/**
	 * Sets the given markers for the resource. All existing markers for the resource.
	 * will be overridden.
	 */
	def void setMarkers(URI uri, Iterable<? extends SadlMarker> markers);

	/**
	 * Clears all markers for the given resource.
	 */
	def void unsetMarkers(URI uri);

	@Override
	override iterator() {
		return Iterators.unmodifiableIterator(markers.iterator);
	}

}
