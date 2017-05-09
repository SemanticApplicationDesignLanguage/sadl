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
import com.ge.research.sadl.markers.api.SadlMarkers
import com.google.common.base.Preconditions
import com.google.common.collect.ImmutableList
import com.google.common.collect.Maps
import java.util.Collection
import java.util.Collections
import java.util.Map
import org.eclipse.emf.common.util.URI

import static extension com.google.common.collect.Iterables.all
import org.eclipse.xtend.lib.annotations.ToString
import org.eclipse.xtend.lib.annotations.EqualsHashCode
import com.thoughtworks.xstream.annotations.XStreamAlias

@ToString
@EqualsHashCode
 @XStreamAlias("markers")
class SadlMarkersImpl implements SadlMarkers {

	// @XStreamAlias("entries")
	val Map<String, Collection<SadlMarker>> cache = Maps.newHashMap;

	@Override
	override getMarkers() {
		return cache.values.flatten;
	}

	@Override
	override getMarkers(URI uri) {
		return cache.get(uri.toString).copyOf;
	}

	@Override
	override addMarkers(Iterable<? extends SadlMarker> markers) {
		markers.groupBy[uri].entrySet.forEach[getOrCreate(key).addAll(value)];
	}

	@Override
	override deleteMarkers(Iterable<? extends SadlMarker> markers) {
		markers.forEach [
			val existingMarkers = cache.get(uri);
			if (existingMarkers !== null) {
				existingMarkers.remove(it);
				if (existingMarkers.empty) {
					cache.remove(uri);
				}
			}
		];
	}

	@Override
	override setMarkers(URI uri, Iterable<? extends SadlMarker> markers) {
		Preconditions.checkArgument(markers.map[URI.createURI(it.uri)].all[it == uri]);
		cache.put(uri.toString, newHashSet(markers));
	}

	@Override
	override unsetMarkers(URI uri) {
		cache.remove(uri);
	}

	private def getOrCreate(String uri) {
		var markers = cache.get(uri);
		if (markers === null) {
			markers = newHashSet();
			cache.put(uri, markers);
		}
		return markers;
	}

	private def <T> copyOf(Iterable<? extends T> iterable) {
		if (iterable === null) {
			return Collections.emptyList;
		}
		return ImmutableList.copyOf(iterable);
	}

}
