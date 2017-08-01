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
package com.ge.research.sadl.model

enum OntConceptType {
	CLASS,
	CLASS_LIST,
	CLASS_PROPERTY,
	DATATYPE_PROPERTY,
	RDF_PROPERTY,
	DATATYPE,
	DATATYPE_LIST,
	ANNOTATION_PROPERTY,
	INSTANCE,
	VARIABLE,
	FUNCTION_DEFN,
	LITERAL,
	STRUCTURE_NAME
}