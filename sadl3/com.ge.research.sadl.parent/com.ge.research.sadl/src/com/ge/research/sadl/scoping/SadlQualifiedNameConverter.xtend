/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.scoping

import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.QualifiedName

class SadlQualifiedNameConverter implements IQualifiedNameConverter {
	
	public static val SEGMENT_SEPARATOR = ':';
	
	override toQualifiedName(String qualifiedNameAsText) {
		val idx = qualifiedNameAsText.lastIndexOf(SEGMENT_SEPARATOR)
		if (idx === -1) {
			return QualifiedName.create(qualifiedNameAsText)
		}
		val lastSegment = qualifiedNameAsText.substring(idx+1)
		val chars = if (lastSegment.startsWith("^")) {
			lastSegment.toCharArray.tail.toList
		} else {
			lastSegment.toCharArray.toList
		}
		if (chars.exists[!Character.isJavaIdentifierPart(it)]) {
			return QualifiedName.create(qualifiedNameAsText)
		} else {
			return QualifiedName.create(qualifiedNameAsText.substring(0, idx), lastSegment)
		}
	}
	
	override toString(QualifiedName name) {
		return name.segments.join(SEGMENT_SEPARATOR)
	}
	
}