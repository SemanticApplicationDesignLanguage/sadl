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
* Subject '{0}' did not translate to node
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
* Property '{0}' does not exist in the model.
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
* Value of '{0}' is not in range of property '{1}'.
**/
    public static final SadlErrorMessage NOT_IN_RANGE = new SadlErrorMessage("not_in_range");
/**
* {0} is a special reserved name. Please choose a different name.
**/
    public static final SadlErrorMessage RESERVED_NAME = new SadlErrorMessage("reserved_name");
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
* Type comparison not possible
**/
    public static final SadlErrorMessage TYPE_COMPARISON = new SadlErrorMessage("type_comparison");
/**
* Import of '{0}' is part of a circular set of imports.
**/
    public static final SadlErrorMessage CIRCULAR_IMPORT = new SadlErrorMessage("circular_import");
/**
* Only Object Properties can be symmetrical
**/
    public static final SadlErrorMessage OBJECT_PROP_SYMMETRY = new SadlErrorMessage("object_prop_symmetry");
/**
* {0} should not be null.
**/
    public static final SadlErrorMessage INVALID_NULL = new SadlErrorMessage("invalid_null");
/**
* Cannot assign {0}: property '{1}' already has {0} assigned to '{2}'.
**/
    public static final SadlErrorMessage CANNOT_ASSIGN_EXISTING = new SadlErrorMessage("cannot_assign_existing");
/**
* A {0} with name '{1}' already exists in the set of {0}s. {0} names must be unique.
**/
    public static final SadlErrorMessage UNIQUE_NAME = new SadlErrorMessage("unique_name");
/**
* translate(Name) called with a SadlResource which resolved to null; this needs to be caught in validation
**/
    public static final SadlErrorMessage TRANSLATE_NAME_SADLRESOURCE = new SadlErrorMessage("translate_name_sadlresource");
/**
* {0} must be applied to a List ({1} is not a List).
**/
    public static final SadlErrorMessage MUST_BE_APPLIED_TO_LIST = new SadlErrorMessage("must_be_applied_to_list");
/**
* Invalid name in {0}: {1}
**/
    public static final SadlErrorMessage INVALID_NAME = new SadlErrorMessage("invalid_name");
/**
* Did not expect a property ({0}) as a triple pattern subject.
**/
    public static final SadlErrorMessage UNEXPECTED_TRIPLE = new SadlErrorMessage("unexpected_triple");
/**
* type checking doesn't handle {0}
**/
    public static final SadlErrorMessage TYPE_CHECK_HANDLE_WARNING = new SadlErrorMessage("type_check_handle_warning");
/**
* {0} is not defined so return type is unknown, can't do type checking
**/
    public static final SadlErrorMessage RETURN_TYPE_WARNING = new SadlErrorMessage("return_type_warning");
/**
* Unexpected: Property has more than 2 restrictions.
**/
    public static final SadlErrorMessage PROPERTY_RESTRICTIONS = new SadlErrorMessage("property_restrictions");
/**
* 
**/
    public static final SadlErrorMessage NULL_ONT_MODEL = new SadlErrorMessage("null_ont_model");
/**
* {0} is undefined. Please define the {1} before referencing it.
**/
    public static final SadlErrorMessage UNDEFINED = new SadlErrorMessage("undefined");
/**
* Unhandled {0}: '{1}'
**/
    public static final SadlErrorMessage UNHANDLED = new SadlErrorMessage("unhandled");
/**
* A(n) {0} cannot be converted to a(n) {1}.
**/
    public static final SadlErrorMessage CANNOT_CONVERT = new SadlErrorMessage("cannot_convert");
/**
* It is unusual to have a variable ('{0}') rather than a defined property as rule predicate.
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
* A(n) {0} requires a {1} but it is missing.
**/
    public static final SadlErrorMessage MISSING = new SadlErrorMessage("missing");
/**
* Import resolved to a null {0}
**/
    public static final SadlErrorMessage NULL_IMPORT = new SadlErrorMessage("null_import");
/**
* {0} could not be found, expected to be in '{1}'.
**/
    public static final SadlErrorMessage NOT_FOUND = new SadlErrorMessage("not_found");
/**
* Unexpected null type error
**/
    public static final SadlErrorMessage NULL_TYPE = new SadlErrorMessage("null_type");
/**
* Translation exception in Test: {0}
**/
    public static final SadlErrorMessage TEST_TRANSLATION_EXCEPTION = new SadlErrorMessage("test_translation_exception");
/**
* {0} cannot {1} with {2}.
**/
    public static final SadlErrorMessage VALIDATE_BIN_OP_ERROR = new SadlErrorMessage("validate_bin_op_error");
/**
* cannot create {0} from {1}
**/
    public static final SadlErrorMessage CANNOT_CREATE = new SadlErrorMessage("cannot_create");
/**
* There can only be one {0} in a {1}.
**/
    public static final SadlErrorMessage ONLY_ONE = new SadlErrorMessage("only_one");
/**
* Phrase 'not known' is not a valid graph pattern. Did you mean 'is not known'?
**/
    public static final SadlErrorMessage PHRASE_NOT_KNOWN = new SadlErrorMessage("phrase_not_known");
/**
* "Translation error in {0}: {1}
**/
    public static final SadlErrorMessage TRANSLATION_ERROR = new SadlErrorMessage("translation_error");
/**
* A unit has been given to a numeric value for a property whose range is not a sublcass of UnittedQuantity
**/
    public static final SadlErrorMessage UNITTED_QUANTITY_ERROR = new SadlErrorMessage("unitted_quantity_error");
/**
* This expression ({0}) cannot be decomposed into a known type"
**/
    public static final SadlErrorMessage DECOMPOSITION_ERROR = new SadlErrorMessage("decomposition_error");
/**
* "Unexpected error processing type check for {0}: {1}
**/
    public static final SadlErrorMessage UNEXPECTED_TYPE_CHECK_ERROR = new SadlErrorMessage("unexpected_type_check_error");
/**
* '{0}' isn't a variable as expected in query select names.
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
* unable to create {0}: {1}
**/
    public static final SadlErrorMessage UNABLE_TO_CREATE = new SadlErrorMessage("unable_to_create");

}