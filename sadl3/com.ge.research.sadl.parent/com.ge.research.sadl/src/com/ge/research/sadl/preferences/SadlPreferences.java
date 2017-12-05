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

import java.util.Arrays;
import java.util.List;

import org.eclipse.xtext.preferences.PreferenceKey;

@SuppressWarnings("restriction")
public class SadlPreferences {
	
	public static final PreferenceKey SADL_BASE_URI= new PreferenceKey("baseUri", "http://sadl.org");
	public static final PreferenceKey OWL_MODEL_FORMAT = new PreferenceKey("OWL_Format", "RDF/XML-ABBREV");
	public static final PreferenceKey RDF_XML_FORMAT = new PreferenceKey("RDF/XML", ""); // default
	public static final PreferenceKey RDF_XML_ABBREV_FORMAT = new PreferenceKey("RDF/XML-ABBREV", "");
	public static final PreferenceKey N_TRIPLE_FORMAT = new PreferenceKey("N-TRIPLE", "");
	public static final PreferenceKey N3_FORMAT = new PreferenceKey("N3", "");
	public static final PreferenceKey JENA_TDB = new PreferenceKey("Jena TDB", "");
	public static final PreferenceKey MODEL_NAMESPACES = new PreferenceKey("ns", "");
	public static final PreferenceKey SADL_FILE_NAMES = new PreferenceKey("fn", "");
	public static final PreferenceKey PREFIXES_ONLY_AS_NEEDED = new PreferenceKey("prefixesOnlyAsNeeded", "");
	public static final PreferenceKey VALIDATE_BEFORE_TEST = new PreferenceKey("validateBeforeTest", "");
	public static final PreferenceKey TEST_WITH_KSERVER = new PreferenceKey("testWithKServer", "");	
	public static final PreferenceKey NAMESPACE_IN_QUERY_RESULTS = new PreferenceKey("namespacesInQueryResults", "");
	public static final PreferenceKey SHOW_TIMING_INFORMATION = new PreferenceKey("showTimingInformation", "");
	public static final PreferenceKey DMY_ORDER_DMY = new PreferenceKey("dmy", "");
	public static final PreferenceKey DMY_ORDER_MDY = new PreferenceKey("mdy", "");
	public static final PreferenceKey DEEP_VALIDATION_OFF = new PreferenceKey("deepValidationOff", "");
	public static final PreferenceKey GRAPH_RENDERER_CLASS= new PreferenceKey("graphrendererclass", "");
	public static final PreferenceKey TABULAR_DATA_IMPORTER_CLASS= new PreferenceKey("tabulardataimporterclass", "");
	public static final PreferenceKey CHECK_FOR_AMBIGUOUS_NAMES = new PreferenceKey("ambiguousNameCheckOn", String.valueOf(true));
	public static final PreferenceKey P_USE_ARTICLES_IN_VALIDATION= new PreferenceKey("use_articles_in_validation", String.valueOf(false));
	public static final PreferenceKey TYPE_CHECKING_WARNING_ONLY = new PreferenceKey("typeCheckingWarningOnly", String.valueOf(false));
	public static final PreferenceKey IGNORE_UNITTEDQUANTITIES = new PreferenceKey("ignoreUnittedQuantities", String.valueOf(false));
	public static final PreferenceKey USE_IMPLIED_PROPERTIES_IN_TRANSLATION = new PreferenceKey("impliedPropertiesInTranslation", String.valueOf(true));
	public static final PreferenceKey CREATE_DOMAIN_AND_RANGE_AS_UNION_CLASSES = new PreferenceKey("domainAndRangeAsUnionClasses", String.valueOf(true));
	//	public static final PreferenceKey ENABLE_METRICS_COLLECTION = new PreferenceKey("metricsCollectionOn", String.valueOf(false));
	public static final PreferenceKey GENERATE_METRICS_REPORT_ON_CLEAN_BUILD = new PreferenceKey("generateMetricsReport", String.valueOf(false));
	public static final PreferenceKey METRICS_QUERY_FILENAME = new PreferenceKey("metricsQueryFilename", "");
	public static final PreferenceKey GRAPH_IMPLICIT_ELEMENTS = new PreferenceKey("graphImplicitElements", String.valueOf(false));
	public static final PreferenceKey GRAPH_IMPLICIT_ELEMENT_INSTANCES = new PreferenceKey("graphImplicitElementInstances", String.valueOf(false));
	
	// Don't forget to add new property to the list below
	
	private static final PreferenceKey[] allKeys = {
			SADL_BASE_URI,
			OWL_MODEL_FORMAT,
			RDF_XML_FORMAT,
			RDF_XML_ABBREV_FORMAT,
			N_TRIPLE_FORMAT,
			N3_FORMAT,
			JENA_TDB,
			MODEL_NAMESPACES,
			SADL_FILE_NAMES,
			PREFIXES_ONLY_AS_NEEDED,
			VALIDATE_BEFORE_TEST,
			TEST_WITH_KSERVER,
			NAMESPACE_IN_QUERY_RESULTS,
			SHOW_TIMING_INFORMATION,
			DMY_ORDER_DMY,
			DMY_ORDER_MDY,
			DEEP_VALIDATION_OFF,
			GRAPH_RENDERER_CLASS,
			CHECK_FOR_AMBIGUOUS_NAMES,
//			DISABLE_TYPE_CHECKING,
			TYPE_CHECKING_WARNING_ONLY,
			IGNORE_UNITTEDQUANTITIES,
			USE_IMPLIED_PROPERTIES_IN_TRANSLATION,
			CREATE_DOMAIN_AND_RANGE_AS_UNION_CLASSES,
//			ENABLE_METRICS_COLLECTION,
			GENERATE_METRICS_REPORT_ON_CLEAN_BUILD,
			METRICS_QUERY_FILENAME, 
			GRAPH_IMPLICIT_ELEMENTS,
			GRAPH_IMPLICIT_ELEMENT_INSTANCES
	};
	
	public static final List<PreferenceKey> preferences() {
		return Arrays.asList(allKeys);
	}
}
