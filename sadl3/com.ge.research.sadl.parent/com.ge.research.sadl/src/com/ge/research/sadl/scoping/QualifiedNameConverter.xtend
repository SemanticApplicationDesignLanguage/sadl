package com.ge.research.sadl.scoping

import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.QualifiedName

class QualifiedNameConverter implements IQualifiedNameConverter {
	
	override toQualifiedName(String qualifiedNameAsText) {
		val idx = qualifiedNameAsText.lastIndexOf(':')
		if (idx === -1) {
			return QualifiedName.create(qualifiedNameAsText)
		}
		val lastSegment = qualifiedNameAsText.substring(idx+1)
		if (lastSegment.toCharArray.exists[!Character.isJavaIdentifierPart(it)]) {
			return QualifiedName.create(qualifiedNameAsText)
		} else {
			return QualifiedName.create(qualifiedNameAsText.substring(0, idx), lastSegment)
		}
	}
	
	override toString(QualifiedName name) {
		return name.segments.join(':')
	}
	
}