/**WARNING: This document is auto-generated. DO NOT manually edit this file!
*   ==Generated based on appropriate .properties file==
*
* To add new errors to this list, use the following process:
*
* 1. Add new error to associated sadlMessages/requirementsMessages.properties file
*      -File is found in either com.ge.research.sadl.errorgenerator/src or 
*       com.ge.research.sadl.requirements.errorgenerator/src
* 2. Select respective ErrorGenerator class (SadlErrorGenerator/RequirmentsErrorGenerator)
* 3. Right click -> Run As -> Java Application
*
* This process should generate a new Sadl/Req ErrorMessages class and updated html table
*
*/

package com.ge.research.sadl.errorgenerator.generator;

import com.ge.research.sadl.errorgenerator.messages.SadlErrorMessage;

public final class SadlErrorMessages {
/**
* Property ''{0}'' does not exist in the model.
**/
    public static final SadlErrorMessage PROPERTY_NOT_EXIST = new SadlErrorMessage("property_not_exist");
/**
* unable to find {0}
**/
    public static final SadlErrorMessage UNABLE_TO_FIND = new SadlErrorMessage("unable_to_find");
/**
* Unidentified expression.
**/
    public static final SadlErrorMessage UNIDENTIFIED = new SadlErrorMessage("unidentified");
/**
* range of property {0} does not match domain of property {1}
**/
    public static final SadlErrorMessage RANGE_OF_NOT_IN_DOMAIN_OF = new SadlErrorMessage("range_of_not_in_domain_of");
/**
* Must select folder (not file) for running Batch ATPT
**/
    public static final SadlErrorMessage ATPT_FOLDER_NOT_SELECT = new SadlErrorMessage("atpt_folder_not_select");
/**
* {0} is a special reserved name. Please choose a different name.
**/
    public static final SadlErrorMessage RESERVED_NAME = new SadlErrorMessage("reserved_name");
/**
* Type comparison not possible
**/
    public static final SadlErrorMessage TYPE_COMPARISON = new SadlErrorMessage("type_comparison");
/**
* ERROR-ATPT: Must select folder for Batch Mode
**/
    public static final SadlErrorMessage ATPT_BATCH_ERROR = new SadlErrorMessage("atpt_batch_error");
/**
* Only files of type {0} are valid for this command
**/
    public static final SadlErrorMessage FILE_TYPE_ERROR = new SadlErrorMessage("file_type_error");
/**
* Import of ''{0}'' is part of a circular set of imports.
**/
    public static final SadlErrorMessage CIRCULAR_IMPORT = new SadlErrorMessage("circular_import");
/**
* Only Object Properties can be symmetrical
**/
    public static final SadlErrorMessage OBJECT_PROP_SYMMETRY = new SadlErrorMessage("object_prop_symmetry");
/**
* No folder or xml file selected for ATPT
**/
    public static final SadlErrorMessage ATPT_NO_XML_SELECTED = new SadlErrorMessage("atpt_no_xml_selected");
/**
* Cannot assign {0}: property ''{1}'' already has {0} assigned to ''{2}''.
**/
    public static final SadlErrorMessage CANNOT_ASSIGN_EXISTING = new SadlErrorMessage("cannot_assign_existing");
/**
* ERROR-ATPT: Cannot run batch mode using TED files. Update ATPT preferences
**/
    public static final SadlErrorMessage ATPT_BATCH_TED_ERROR = new SadlErrorMessage("atpt_batch_ted_error");
/**
* {0} must be applied to a List ({1} is not a List).
**/
    public static final SadlErrorMessage MUST_BE_APPLIED_TO_LIST = new SadlErrorMessage("must_be_applied_to_list");
/**
* This changes the domain of property ''{0}'' which has an imported domain; are you sure that's what you want to do?
**/
    public static final SadlErrorMessage IMPORTED_DOMAIN_CHANGE = new SadlErrorMessage("imported_domain_change");
/**
* List definition is empty. Remove empty list brackets from definition. 
**/
    public static final SadlErrorMessage EMPTY_LIST_DEFINITION = new SadlErrorMessage("empty_list_definition");
/**
* Property ''{0}'' has no range; type cannot be determined so type checking is not possible.
**/
    public static final SadlErrorMessage PROPERTY_WITHOUT_RANGE = new SadlErrorMessage("property_without_range");
/**
* {0} is not defined so return type is unknown, can't do type checking
**/
    public static final SadlErrorMessage RETURN_TYPE_WARNING = new SadlErrorMessage("return_type_warning");
/**
* Unexpected: Property has more than 2 restrictions.
**/
    public static final SadlErrorMessage PROPERTY_RESTRICTIONS = new SadlErrorMessage("property_restrictions");
/**
* Controlled variable must have a range of type Boolean or a range of exactly two instances in order to be used with an "Only When" clause.
**/
    public static final SadlErrorMessage CONTROL_VARIABLE_INAPPLICABLE_FOR_ONLY_WHEN = new SadlErrorMessage("control_variable_inapplicable_for_only_when");
/**
* {0} is undefined. Please define the {1} before referencing it.
**/
    public static final SadlErrorMessage UNDEFINED = new SadlErrorMessage("undefined");
/**
* A(n) {0} cannot be converted to a(n) {1}.
**/
    public static final SadlErrorMessage CANNOT_CONVERT = new SadlErrorMessage("cannot_convert");
/**
* It is unusual to have a variable (''{0}'') rather than a defined property as rule predicate.
**/
    public static final SadlErrorMessage VARIABLE_INSTEAD_OF_PROP = new SadlErrorMessage("variable_instead_of_prop");
/**
* This {0} is not an instance of a known type. It is: {1}
**/
    public static final SadlErrorMessage UNKNOWN_TYPE = new SadlErrorMessage("unknown_type");
/**
* Unexpected type reference type: {0}
**/
    public static final SadlErrorMessage TYPE_REFERENCE_ERROR = new SadlErrorMessage("type_reference_error");
/**
* {0} is not a recognized {1} value.
**/
    public static final SadlErrorMessage UNKNOWN_VALUE = new SadlErrorMessage("unknown_value");
/**
* {0} could not be found, expected to be in ''{1}''.
**/
    public static final SadlErrorMessage NOT_FOUND = new SadlErrorMessage("not_found");
/**
* Translation exception in Test: {0}
**/
    public static final SadlErrorMessage TEST_TRANSLATION_EXCEPTION = new SadlErrorMessage("test_translation_exception");
/**
* ERROR-ATPT: No {0} selected.
**/
    public static final SadlErrorMessage ATPT_NO_SELECTION = new SadlErrorMessage("atpt_no_selection");
/**
* {0} cannot {1} with {2}.
**/
    public static final SadlErrorMessage VALIDATE_BIN_OP_ERROR = new SadlErrorMessage("validate_bin_op_error");
/**
* "Translation error in {0}: {1}
**/
    public static final SadlErrorMessage TRANSLATION_ERROR = new SadlErrorMessage("translation_error");
/**
* This expression ({0}) cannot be decomposed into a known type"
**/
    public static final SadlErrorMessage DECOMPOSITION_ERROR = new SadlErrorMessage("decomposition_error");
/**
* Function has no reference.
**/
    public static final SadlErrorMessage TYPE_CHECK_UNDEFINED_EXCEPTION = new SadlErrorMessage("type_check_undefined_exception");
/**
* Built-in function, {0}, was found, but the reasoner and translator pair does not provide further type-checking information
**/
    public static final SadlErrorMessage TYPE_CHECK_BUILTIN_EXCEPTION = new SadlErrorMessage("type_check_builtin_exception");
/**
* It is unusual to use an undefined variable (''{0}'') as a property.
**/
    public static final SadlErrorMessage VARIABLE_INSTEAD_OF_PROP2 = new SadlErrorMessage("variable_instead_of_prop2");
/**
* expected a {0}
**/
    public static final SadlErrorMessage EXPECTED_A = new SadlErrorMessage("expected_a");
/**
* unable to add {0}: {1}
**/
    public static final SadlErrorMessage UNABLE_TO_ADD = new SadlErrorMessage("unable_to_add");
/**
* Unable to do domain-range matching on a(n) {0}
**/
    public static final SadlErrorMessage DOMAIN_MATCHING = new SadlErrorMessage("domain_matching");
/**
* Using a datatype as a domain--probably not what was intended?
**/
    public static final SadlErrorMessage DATATYPE_AS_DOMAIN = new SadlErrorMessage("datatype_as_domain");
/**
* unable to create {0}: {1}
**/
    public static final SadlErrorMessage UNABLE_TO_CREATE = new SadlErrorMessage("unable_to_create");
/**
* Subject ''{0}'' did not translate to node
**/
    public static final SadlErrorMessage TRANSLATE_TO_NODE = new SadlErrorMessage("translate_to_node");
/**
* A(n) {0} exception occurred while type-checking this expression.
**/
    public static final SadlErrorMessage TYPE_CHECK_EXCEPTION = new SadlErrorMessage("type_check_exception");
/**
* {0} is expected to be a(n) {1} but it is not.
**/
    public static final SadlErrorMessage IS_NOT_A = new SadlErrorMessage("is_not_a");
/**
* Value of ''{0}'' is not in range of property ''{1}''.
**/
    public static final SadlErrorMessage NOT_IN_RANGE = new SadlErrorMessage("not_in_range");
/**
* Invalid property type: {0}.
**/
    public static final SadlErrorMessage INVALID_PROP_TYPE = new SadlErrorMessage("invalid_prop_type");
/**
* Fatal Configuration Error: {0}
**/
    public static final SadlErrorMessage CONFIGURATION_ERROR = new SadlErrorMessage("configuration_error");
/**
* Range failed to resolve to a class or datatype
**/
    public static final SadlErrorMessage RANGE_RESOLVE = new SadlErrorMessage("range_resolve");
/**
* ERROR-ATPT: Must select Common Test Procedure XML file for Single CTP File Mode
**/
    public static final SadlErrorMessage ATPT_CTP_NOT_FOUND = new SadlErrorMessage("atpt_ctp_not_found");
/**
* {0} should not be null.
**/
    public static final SadlErrorMessage INVALID_NULL = new SadlErrorMessage("invalid_null");
/**
* A {0} with name ''{1}'' already exists in the set of {0}s. {0} names must be unique.
**/
    public static final SadlErrorMessage UNIQUE_NAME = new SadlErrorMessage("unique_name");
/**
* This changes the range of property ''{0}'' which has an imported range; are you sure that's what you want to do?
**/
    public static final SadlErrorMessage IMPORTED_RANGE_CHANGE = new SadlErrorMessage("imported_range_change");
/**
* translate(Name) called with a SadlResource which resolved to null; this needs to be caught in validation
**/
    public static final SadlErrorMessage TRANSLATE_NAME_SADLRESOURCE = new SadlErrorMessage("translate_name_sadlresource");
/**
* Invalid name in {0}: {1}
**/
    public static final SadlErrorMessage INVALID_NAME = new SadlErrorMessage("invalid_name");
/**
* Did not expect a property ({0}) as a triple pattern subject.
**/
    public static final SadlErrorMessage UNEXPECTED_TRIPLE = new SadlErrorMessage("unexpected_triple");
/**
* Comparison using {0} cannot have a constant or literal on both sides.
**/
    public static final SadlErrorMessage COMPARISON_LITERALS_CONSTANTS = new SadlErrorMessage("comparison_literals_constants");
/**
* type checking doesn't handle {0}
**/
    public static final SadlErrorMessage TYPE_CHECK_HANDLE_WARNING = new SadlErrorMessage("type_check_handle_warning");
/**
* 
**/
    public static final SadlErrorMessage NULL_ONT_MODEL = new SadlErrorMessage("null_ont_model");
/**
* The {0} type may not be compatible with downstream projects
**/
    public static final SadlErrorMessage TYPE_UNSUPPORTED_DOWNSTREAM = new SadlErrorMessage("type_unsupported_downstream");
/**
* Variable {0} is of type {1} which is not in domain of property {2}
**/
    public static final SadlErrorMessage VARIABLE_NOT_IN_DOMAIN_OF_PROPERTY = new SadlErrorMessage("variable_not_in_domain_of_property");
/**
* Unhandled {0}: ''{1}''
**/
    public static final SadlErrorMessage UNHANDLED = new SadlErrorMessage("unhandled");
/**
* ''{0}'' is not a property
**/
    public static final SadlErrorMessage INVALID_USE_OF_CLASS_AS_PROPERTY = new SadlErrorMessage("invalid_use_of_class_as_property");
/**
* {0} is not in domain of property {1}
**/
    public static final SadlErrorMessage SUBJECT_NOT_IN_DOMAIN_OF_PROPERTY = new SadlErrorMessage("subject_not_in_domain_of_property");
/**
* {0} is not in domain of property {1}
**/
    public static final SadlErrorMessage PROPERTY_NOT_IN_DOMAIN = new SadlErrorMessage("property_not_in_domain");
/**
* A(n) {0} requires a {1} but it is missing.
**/
    public static final SadlErrorMessage MISSING = new SadlErrorMessage("missing");
/**
* Import resolved to a null {0}
**/
    public static final SadlErrorMessage NULL_IMPORT = new SadlErrorMessage("null_import");
/**
* Translator class ''{0}'' not found"
**/
    public static final SadlErrorMessage TYPE_CHECK_TRANSLATOR_CLASS_NOT_FOUND = new SadlErrorMessage("type_check_translator_class_not_found");
/**
* Unexpected null type error
**/
    public static final SadlErrorMessage NULL_TYPE = new SadlErrorMessage("null_type");
/**
* cannot create {0} from {1}
**/
    public static final SadlErrorMessage CANNOT_CREATE = new SadlErrorMessage("cannot_create");
/**
* There can only be one {0} in a {1}.
**/
    public static final SadlErrorMessage ONLY_ONE = new SadlErrorMessage("only_one");
/**
* Was not able to find atpt.exe in ASSERT/ATPT/atpt.exe
**/
    public static final SadlErrorMessage ATPT_EXE_NOT_FOUND = new SadlErrorMessage("atpt_exe_not_found");
/**
* Phrase 'not known' is not a valid graph pattern. Did you mean 'is not known'?
**/
    public static final SadlErrorMessage PHRASE_NOT_KNOWN = new SadlErrorMessage("phrase_not_known");
/**
* A unit has been given to a numeric value for a property whose range is not a sublcass of UnittedQuantity
**/
    public static final SadlErrorMessage UNITTED_QUANTITY_ERROR = new SadlErrorMessage("unitted_quantity_error");
/**
* "Unexpected error processing type check for {0}: {1}
**/
    public static final SadlErrorMessage UNEXPECTED_TYPE_CHECK_ERROR = new SadlErrorMessage("unexpected_type_check_error");
/**
* ''{0}'' isn't a variable as expected in query select names.
**/
    public static final SadlErrorMessage QUERY_ISNT_VARIABLE = new SadlErrorMessage("query_isnt_variable");
/**
* Invalid type in {0}: {1}
**/
    public static final SadlErrorMessage INVALID_TYPE = new SadlErrorMessage("invalid_type");
/**
* Import failed to provide an imported resource
**/
    public static final SadlErrorMessage IMPORT_FAIL = new SadlErrorMessage("import_fail");
/**
* Only OWL Full allows classes to have property values.
**/
    public static final SadlErrorMessage CLASS_PROPERTY_VALUE_OWL_FULL = new SadlErrorMessage("class_property_value_owl_full");
/**
* {0} is a reserved folder name that can only contain system-generated content. Please place this model in a different folder.
**/
    public static final SadlErrorMessage RESERVED_FOLDER = new SadlErrorMessage("reserved_folder");

}