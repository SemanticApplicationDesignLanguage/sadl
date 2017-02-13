/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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

import com.google.inject.Inject
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.QualifiedName

import static extension com.google.common.collect.Iterables.limit

/**
 * Core service for transforming and getting a domain specific string 
 * representation of a {@link QualifiedName qualified name}.
 * 
 * <p>
 * This service class is just a workaround for
 * https://github.com/eclipse/xtext-eclipse/issues/145. Once the issue is
 * resolved, this class should be eliminated.
 * 
 * @author akos.kitta
 */
class SadlQualifiedNameToStringService {

	@Inject
	IQualifiedNameConverter qualifiedNameConverter;

	/**
	 * Returns with the string representation of the qualified name argument.
	 * 
	 * @param qn
	 *            the qualified name to transform. If {@code null}, then
	 *            {@code null} value will be provided by this method.
	 * 
	 * @return the transformed string format of the qualified name argument.
	 */
	def String toString(QualifiedName qn) {
		if (qn === null) {
			return null;
		}

		val segmentCount = qn.segmentCount;
		if (segmentCount > 1) {
			return '''«(qn.segments).limit(segmentCount - 1).join(':')»#«qn.lastSegment»''';
		}

		return qualifiedNameConverter.toString(qn);
	}

}
