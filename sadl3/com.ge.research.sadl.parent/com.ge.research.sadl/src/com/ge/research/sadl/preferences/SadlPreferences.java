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
package com.ge.research.sadl.preferences;

import org.eclipse.xtext.preferences.PreferenceKey;

@SuppressWarnings("restriction")
public class SadlPreferences {
	
	public static final PreferenceKey SADL_BASE_URI= new PreferenceKey("baseUri", "");
	public static final PreferenceKey RDF_XML_FORMAT = new PreferenceKey("RDF/XML", ""); // default
	public static final PreferenceKey RDF_XML_ABBREV_FORMAT = new PreferenceKey("RDF/XML-ABBREV", "");
	public static final PreferenceKey N_TRIPLE_FORMAT = new PreferenceKey("N-TRIPLE", "");
	public static final PreferenceKey N3_FORMAT = new PreferenceKey("N3", "");
	public static final PreferenceKey JENA_TDB = new PreferenceKey("Jena TDB", "");
	public static final PreferenceKey MODEL_NAMESPACES = new PreferenceKey("ns", "");
	public static final PreferenceKey SADL_FILE_NAMES = new PreferenceKey("fn", "");
	public static final PreferenceKey PREFIXES_ONLY_AS_NEEDED = new PreferenceKey("prefixesOnlyAsNeeded", "");
	public static final PreferenceKey VALIDATE_BEFORE_TEST = new PreferenceKey("validateBeforeTest", "");
	public static final PreferenceKey NAMESPACE_IN_QUERY_RESULTS = new PreferenceKey("namespacesInQueryResults", "");
	public static final PreferenceKey SHOW_TIMING_INFORMATION = new PreferenceKey("showTimingInformation", "");
	public static final PreferenceKey DMY_ORDER_DMY = new PreferenceKey("dmy", "");
	public static final PreferenceKey DMY_ORDER_MDY = new PreferenceKey("mdy", "");
	public static final PreferenceKey DEEP_VALIDATION_OFF = new PreferenceKey("deepValidationOff", "");
	public static final PreferenceKey GRAPH_VIZ_PATH = new PreferenceKey("graphvizpath", "");
	
}
