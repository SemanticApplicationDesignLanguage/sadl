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
* Left and right sides of {0} are of incompatible types: {1} and {2}.
**/
    public static final SadlErrorMessage INCOMPATIBLE_MEMBER_TYPES = new SadlErrorMessage("incompatible_member_types");
/**
* Unexpected: Property has more than 2 restrictions.
**/
    public static final SadlErrorMessage PROPERTY_RESTRICTIONS = new SadlErrorMessage("property_restrictions");
/**
* '{0}' is expected to be a(n) {1} but it is not.
**/
    public static final SadlErrorMessage IS_NOT_A = new SadlErrorMessage("is_not_a");
/**
* Property '{0}' does not exist in the model.
**/
    public static final SadlErrorMessage PROPERTY_NOT_EXIST = new SadlErrorMessage("property_not_exist");
/**
* Unidentified expression.
**/
    public static final SadlErrorMessage UNIDENTIFIED = new SadlErrorMessage("unidentified");
/**
* Value of '{0}' is not in range of property '{1}'.
**/
    public static final SadlErrorMessage NOT_IN_RANGE = new SadlErrorMessage("not_in_range");
/**
* '{0}' is undefined. Please define the {1} before referencing it.
**/
    public static final SadlErrorMessage UNDEFINED = new SadlErrorMessage("undefined");
/**
* Unhandled {0} value: '{1}'
**/
    public static final SadlErrorMessage UNHANDLED = new SadlErrorMessage("unhandled");
/**
* A(n) {0} cannot be converted to a(n) {1}.
**/
    public static final SadlErrorMessage CANNOT_CONVERT = new SadlErrorMessage("cannot_convert");
/**
* This {0} is not an instance of a known type. It is: {1}
**/
    public static final SadlErrorMessage UNKNOWN_TYPE = new SadlErrorMessage("unknown_type");
/**
* '{0}' is a special reserved name. Please choose a different name.
**/
    public static final SadlErrorMessage RESERVED_NAME = new SadlErrorMessage("reserved_name");
/**
* Invalid property type: {0}.
**/
    public static final SadlErrorMessage INVALID_PROP_TYPE = new SadlErrorMessage("invalid_prop_type");
/**
* '{0}' is not a recognized {1} value.
**/
    public static final SadlErrorMessage UNKNOWN_VALUE = new SadlErrorMessage("unknown_value");
/**
* A(n) {0} requires a {1} but it is missing.
**/
    public static final SadlErrorMessage MISSING = new SadlErrorMessage("missing");
/**
* {0} could not be found, expected to be in '{1}'.
**/
    public static final SadlErrorMessage NOT_FOUND = new SadlErrorMessage("not_found");
/**
* Invalid Format: {0}.
**/
    public static final SadlErrorMessage INVALID_FORMAT = new SadlErrorMessage("invalid_format");
/**
* There can only be one {0} in a {1}.
**/
    public static final SadlErrorMessage ONLY_ONE = new SadlErrorMessage("only_one");
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
* Member of {0} is an invalid type: {1}.
**/
    public static final SadlErrorMessage INVALID_MEMBER_TYPE = new SadlErrorMessage("invalid_member_type");
/**
* {0} must be applied to a List ({1} is not a List).
**/
    public static final SadlErrorMessage MUST_BE_APPLIED_TO_LIST = new SadlErrorMessage("must_be_applied_to_list");
/**
* Cannot create unnamed instance with no class given.
**/
    public static final SadlErrorMessage CREATE_UNNAMED_INSTANCE = new SadlErrorMessage("create_unnamed_instance");
/**
* Unexpected value '{0}' ({1}) does not match expected range type {2}.
**/
    public static final SadlErrorMessage INCOMPATIBLE_RANGE = new SadlErrorMessage("incompatible_range");

}