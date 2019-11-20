/************************************************************************
 * Copyright © 2007-2019 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.processing;

public class SparqlQueries {

	// query to get the argument names and types
	final public static String ARGUMENTS_QUERY = "select ?argName ?argType where {? <" + 
			SadlConstants.SADL_IMPLICIT_MODEL_ARGUMENTS_PROPERTY_URI + 
			"> ?arglst . ?arglst <sadllistmodel:rest>*/<sadllistmodel:first> ?member . ?member <" +
			SadlConstants.SADL_IMPLICIT_MODEL_DESCRIPTOR_NAME_PROPERTY_URI + "> ?argName . ?member <" +
			SadlConstants.SADL_IMPLICIT_MODEL_DATATYPE_PROPERTY_URI + "> ?argType}";
	
	// query to get the return types
	final public static String RETURN_TYPES_QUERY = "select ?retName ?retType where {? <" + 
			SadlConstants.SADL_IMPLICIT_MODEL_RETURN_TYPES_PROPERTY_URI + 
			"> ?retlst . ?retlst <sadllistmodel:rest>*/<sadllistmodel:first> ?member . OPTIONAL{?member <" +
			SadlConstants.SADL_IMPLICIT_MODEL_DESCRIPTOR_NAME_PROPERTY_URI + "> ?retName} . ?member <" +
			SadlConstants.SADL_IMPLICIT_MODEL_DATATYPE_PROPERTY_URI + "> ?retType}";							
}
