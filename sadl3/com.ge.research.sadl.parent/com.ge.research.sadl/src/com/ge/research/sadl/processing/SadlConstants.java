package com.ge.research.sadl.processing;

public class SadlConstants {
	public static final String SADL_BASE_MODEL_FILENAME = "SadlBaseModel";
	public static final String SADL_BASE_MODEL_URI = "http://sadl.org/sadlbasemodel";
	public static final String SADL_BASE_MODEL_PREFIX = "sadlbasemodel";
	public static final String SADL_BASE_MODEL_EQUATION_URI = SADL_BASE_MODEL_URI + "#Equation";
	public static final String SADL_BASE_MODEL_EQ_EXPRESSION_URI = SADL_BASE_MODEL_URI + "#expression";
	public static final String SADL_BASE_MODEL_EXTERNAL_URI = SADL_BASE_MODEL_URI + "#ExternalEquation";
	public static final String SADL_BASE_MODEL_EXTERNALURI_URI = SADL_BASE_MODEL_URI + "#externalURI";
	public static final String SADL_BASE_MODEL_EXTERNALURI_LOCATIOIN = SADL_BASE_MODEL_URI + "#location";
	
	public static final String SADL_LIST_MODEL_FILENAME = "SadlListModel";
	public static final String SADL_LIST_MODEL_PREFIX = "sadllistmodel";
	public static final String SADL_LIST_MODEL_URI = "http://sadl.org/sadllistmodel";
	public static final String SADL_LIST_MODEL_RANGE_ANNOTATION_PROPERTY = SADL_LIST_MODEL_URI + "#listtype";
	public static final String SADL_LIST_MODEL_LIST_URI = SADL_LIST_MODEL_URI + "#List";
	public static final String SADL_LIST_MODEL_FIRST_URI = SADL_LIST_MODEL_URI + "#first";
	public static final String SADL_LIST_MODEL_REST_URI = SADL_LIST_MODEL_URI + "#rest";
	public static final String SADL_LIST_MODEL_LENGTH_RESTRICTION_URI = SADL_LIST_MODEL_URI + "#lengthRestriction";
	public static final String SADL_LIST_MODEL_MINLENGTH_RESTRICTION_URI = SADL_LIST_MODEL_URI + "#minLengthRestriction";
	public static final String SADL_LIST_MODEL_MAXLENGTH_RESTRICTION_URI = SADL_LIST_MODEL_URI + "#maxLengthRestriction";

	public static final String SADL_IMPLICIT_MODEL_FOLDER = "ImplicitModel";
	public static final String SADL_IMPLICIT_MODEL_FILENAME = "SadlImplicitModel.sadl";	// this is a .sadl file and for now will be imported explicitly
	public static final String OWL_IMPLICIT_MODEL_FILENAME = "SadlImplicitModel.owl";
	public static final String SADL_IMPLICIT_MODEL_URI = "http://sadl.org/sadlimplicitmodel";
	public static final String SADL_IMPLICIT_MODEL_SYNTHETIC_URI = "synthetic://test/SadlImplicitModel.sadl";
	public static final String SADL_IMPLICIT_MODEL_PREFIX = "sadlimplicitmodel";
	public static final String SADL_IMPLICIT_MODEL_EVENT_URI = SADL_IMPLICIT_MODEL_URI + "#Event";
	public static final String SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI = SADL_IMPLICIT_MODEL_URI + "#UnittedQuantity";
	public static final String SADL_IMPLICIT_MODEL_UNIT_URI = SADL_IMPLICIT_MODEL_URI + "#unit";
	public static final String SADL_IMPLICIT_MODEL_VALUE_URI = SADL_IMPLICIT_MODEL_URI + "#value";
	public static final String SADL_IMPLICIT_MODEL_IMPLIED_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#impliedProperty";
	public static final String SADL_IMPLICIT_MODEL_EXPANDED_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#expandedProperty";
	public static final String SADL_IMPLICIT_MODEL_NAMEDQUERY_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#NamedQuery";
	public static final String SADL_IMPLICIT_MODEL_QUERY_STRING_URI = SADL_IMPLICIT_MODEL_URI + "#queryString";
	public static final String SADL_IMPLICIT_MODEL_RULE_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#Rule";
	
	public static final String SADL_RULE_PATTERN_URI = "http://sadl.org/rule/patterns";
	public static final String SADL_RULE_PATTERN_PREFIX = "";
	public static final String SADL_RULE_PATTERN_DATA_URI = "http://sadl.org/rule/patterns/data";
	public static final String SADL_RULE_PATTERN_DATA_PREFIX = "";
	public static final String SADL_SERIVCES_CONFIGURATION_CONCEPTS_URI = "http://com.ge.research.sadl/sadlserver/Services";
	public static final String SADL_SERIVCES_CONFIGURATION_CONCEPTS_PREFIX = "";
	public static final String SADL_SERIVCES_CONFIGURATION_URI = "http://com.ge.research.sadl/sadlserver/ServicesConfig";
	public static final String SADL_SERIVCES_CONFIGURATION_PREFIX = "";
	
	public static final String SADL_DEFAULTS_MODEL_FILENAME = "defaults";
	public static final String SADL_DEFAULTS_MODEL_PREFIX = "defs";
	public static final String SADL_DEFAULTS_MODEL_URI = "http://research.ge.com/Acuity/defaults.owl";

	public static final String SADL_BUILTIN_FUNCTIONS_FILENAME = "SadlBuiltinFunctions.sadl";
	public static final String OWL_BUILTIN_FUNCTIONS_FILENAME = "SadlBuiltinFunctions.owl";
	
	public static final String CONSTANT_NONE = "None";
	public static final String CONSTANT_PI = "PI";
	public static final String CONSTANT_E = "e";
	public static final String CONSTANT_KNOWN = "known";
		
	public static enum OWL_FLAVOR {OWL_FULL, OWL_DL, OWL_LITE};
}
