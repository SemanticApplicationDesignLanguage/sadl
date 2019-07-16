package com.ge.research.sadl.processing;

public class SadlConstants {
	public static final String SADL_BASE_MODEL_FILENAME = "SadlBaseModel";
	public static final String SADL_BASE_MODEL_URI = "http://sadl.org/sadlbasemodel";
	public static final String SADL_BASE_MODEL_PREFIX = "sadlbasemodel";
	
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
	
	public static final String SADL_IMPLICIT_MODEL_EQUATION_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#Equation";
	public static final String SADL_IMPLICIT_MODEL_EXTERNAL_EQUATION_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#ExternalEquation";
	public static final String SADL_IMPLICIT_MODEL_EXPRESSTION_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#expression";
	public static final String SADL_IMPLICIT_MODEL_SCRIPT_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#Script";
	public static final String SADL_IMPLICIT_MODEL_LANGUAGE_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#Language";
	public static final String SADL_IMPLICIT_MODEL_LANGUAGE_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#language";
	public static final String SADL_IMPLICIT_MODEL_TEXT_LANGUAGE_INST_URI = SADL_IMPLICIT_MODEL_URI + "#Text";
	public static final String SADL_IMPLICIT_MODEL_JAVA_LANGUAGE_INST_URI = SADL_IMPLICIT_MODEL_URI + "#Java";
	public static final String SADL_IMPLICIT_MODEL_PYTHON_LANGUAGE_INST_URI = SADL_IMPLICIT_MODEL_URI + "#Python";
	public static final String SADL_IMPLICIT_MODEL_SCRIPT_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#script";
	public static final String SADL_IMPLICIT_MODEL_EXTERNALURL_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#externalURI";
	public static final String SADL_IMPLICIT_MODEL_LOCATION_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#externalURL";
	public static final String SADL_IMPLICIT_MODEL_DATA_DESCRIPTOR_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#DataDescriptor";
	public static final String SADL_IMPLICIT_MODEL_DESCRIPTOR_NAME_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#descriptorName";
	public static final String SADL_IMPLICIT_MODEL_DATATYPE_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#dataType";
	public static final String SADL_IMPLICIT_MODEL_AUGMENTED_TYPE_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#augmentedType";
	public static final String SADL_IMPLICIT_MODEL_ARGUMENTS_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#arguments";
	public static final String SADL_IMPLICIT_MODEL_RETURN_TYPES_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#returnTypes";
	public static final String SADL_IMPLICIT_MODEL_SEMANTIC_TYPE_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#SemanticType";
	public static final String SADL_IMPLICIT_MODEL_SEM_TYPE_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#semType";
	public static final String SADL_IMPLICIT_MODEL_TRIPLE_PATTERN_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#TriplePattern";
	public static final String SADL_IMPLICIT_MODEL_FUNCTION_PATTERN_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#FunctionPattern";
	public static final String SADL_IMPLICIT_MODEL_SEMANTIC_CONSTRAINT_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#SemanticConstraint";
	public static final String SADL_IMPLICIT_MODEL_CONSTRAINTS_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#constraints";
	public static final String SADL_IMPLICIT_MODEL_AUG_TYPE_UNITS_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#augTypeUnits";
	public static final String SADL_IMPLICIT_MODEL_GP_SUBJECT_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#gpSubject";
	public static final String SADL_IMPLICIT_MODEL_GP_PREDICATE_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#gpPredicate";
	public static final String SADL_IMPLICIT_MODEL_GP_OBJECT_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#gpObject";
	public static final String SADL_IMPLICIT_MODEL_BUILTIN_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#builtin";
	public static final String SADL_IMPLICIT_MODEL_ARG_VALUES_PROPERTY_URI = SADL_IMPLICIT_MODEL_URI + "#argValues";
	public static final String SADL_IMPLICIT_MODEL_GP_ATOM_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#GPAtom";
	public static final String SADL_IMPLICIT_MODEL_GP_VARIABLE_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#GPVariable";
	public static final String SADL_IMPLICIT_MODEL_GP_LITERAL_VALUE_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#GPLiteralValue";
	public static final String SADL_IMPLICIT_MODEL_GP_RESOURCE_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#GPResource";
	public static final String SADL_IMPLICIT_MODEL_GP_LITERAL_VLAUE_PROPERTYS_URI = SADL_IMPLICIT_MODEL_URI + "#gpLiteralValue";
	
	public static final String SADL_IMPLICIT_MODEL_DATA_TABLE_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#DataTable";
	public static final String SADL_IMPLICIT_MODEL_DATA_TABLE_ROW_CLASS_URI = SADL_IMPLICIT_MODEL_URI + "#DataTableRow";
	public static final String SADL_IMPLICIT_MODEL_DATA_ROW_VALUES_RPOERTY_URI = SADL_IMPLICIT_MODEL_URI + "#rowValues";
	public static final String SADL_IMPLICIT_MODEL_DATA_COLUMN_DESCRIPTORS_PROPERY_URI = SADL_IMPLICIT_MODEL_URI + "#columnDescriptors";	
	public static final String SADL_IMPLICIT_MODEL_DATA_CONTENT_DESCRIPTORS_PROPERY_URI = SADL_IMPLICIT_MODEL_URI + "#dataContent";	
	public static final String SADL_IMPLICIT_MODEL_DATA_CONTENT_LOCATION_PROPERY_URI = SADL_IMPLICIT_MODEL_URI + "#dataLocation";
	
	public static final String SADL_RULE_PATTERN_URI = "http://sadl.org/rule/patterns";
	public static final String SADL_RULE_PATTERN_PREFIX = "srpp";
	public static final String SADL_RULE_PATTERN_DATA_URI = "http://sadl.org/rule/patterns/data";
	public static final String SADL_RULE_PATTERN_DATA_PREFIX = "srpdp";
	public static final String SADL_SERVICES_CONFIGURATION_FILENAME = "SadlServicesConfigurationConcepts";
	public static final String SADL_SERIVCES_CONFIGURATION_CONCEPTS_URI = "http://com.ge.research.sadl/sadlserver/Services";
	public static final String SADL_SERIVCES_CONFIGURATION_CONCEPTS_PREFIX = "ssccp";
	public static final String SADL_SERIVCES_CONFIGURATION_URI = "http://com.ge.research.sadl/sadlserver/ServicesConfig";
	
	public static final String SADL_DEFAULTS_MODEL_FILENAME = "defaults";
	public static final String SADL_DEFAULTS_MODEL_PREFIX = "defs";
	public static final String SADL_DEFAULTS_MODEL_URI = "http://research.ge.com/Acuity/defaults.owl";

	public static final String SADL_BUILTIN_FUNCTIONS_FILENAME = "SadlBuiltinFunctions.sadl";
	public static final String OWL_BUILTIN_FUNCTIONS_FILENAME = "SadlBuiltinFunctions.owl";
	
	public static final String CONSTANT_NONE = "None";
	public static final String CONSTANT_PI = "PI";
	public static final String CONSTANT_E = "e";
	public static final String CONSTANT_KNOWN = "known";
	public static final String OF_TYPE_INSTANCES = "instances";
		
	public static enum OWL_FLAVOR {OWL_FULL, OWL_DL, OWL_LITE};
}
