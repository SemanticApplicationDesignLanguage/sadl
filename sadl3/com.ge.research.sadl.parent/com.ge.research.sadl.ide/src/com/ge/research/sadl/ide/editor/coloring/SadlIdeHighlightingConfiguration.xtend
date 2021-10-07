package com.ge.research.sadl.ide.editor.coloring

import org.eclipse.xtext.ide.editor.syntaxcoloring.HighlightingStyles

interface SadlIdeHighlightingConfiguration extends HighlightingStyles {

	String URI_ID = "uri"
	String CLASS_ID = "class"
	String INSTANCE_ID = "instance"
	String STRUCTURE_NAME_ID = "structureName"
	String VARIABLE_DECL_ID = "variableDecl"
	String VARIABLE_REF_ID = "variableRef"
	String DATA_PROPERTY_ID = "dataProperty"
	String OBJECT_PROPERTY_ID = "objectProperty"
	String ANNOTATION_PROPERTY_ID = "annotationProperty"
	String RDF_PROPERTY_ID = "rdfProperty"
	String RDFDATATYPE_ID = "rdfDataType"
	String FUNCTION_NAME_ID = "functionName"

}
