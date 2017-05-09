/************************************************************************
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
package com.ge.research.sadl.utils

import com.google.common.base.Preconditions
import com.google.inject.ImplementedBy
import com.google.inject.Singleton
import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.xml.DomDriver

/**
 * Representation of SADL serialization service.
 * 
 * @author akos.kitta
 */
@ImplementedBy(SerializationService.Default)
interface SerializationService {

	/**
	 * Serializes the marker argument and returns with the serialized form as a string.
	 */
	def <T> String serialize(T object);

	/**
	 * Deserializes the XML string argument and returns with the object.
	 */
	def <T> T deserialize(String xml) {
		return deserialize(xml, SerializationService.classLoader);
	}

	/**
	 * Deserializes the XML string argument and returns with the object using the given class loader.
	 */
	def <T> T deserialize(String xml, ClassLoader classLoader);

	/**
	 * The default XStream-based serialization service.
	 */
	@Singleton
	static class Default implements SerializationService {

		@Override
		override <T> String serialize(T object) {
			val xstream = new XStream(new DomDriver());
			// TODO this does not seem to work with OSGi
			// xstream.autodetectAnnotations(true);
			return xstream.toXML(Preconditions.checkNotNull(object, 'object'));
		}

		@Override
		override <T> deserialize(String xml, ClassLoader classLoader) {
			val xstream = new XStream(new DomDriver());
			// TODO this does not seem to work with OSGi
			// xstream.autodetectAnnotations(true);
			xstream.classLoader = classLoader;
			return xstream.fromXML(Preconditions.checkNotNull(xml, 'xml')) as T;
		}

	}

}
