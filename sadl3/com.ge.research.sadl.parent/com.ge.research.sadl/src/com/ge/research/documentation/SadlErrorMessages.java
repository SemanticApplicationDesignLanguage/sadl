package com.ge.research.documentation;

import com.ge.research.messages.ErrorMessage;

/**
 * See the following file for definitions of these messages:
 *
 */
public final class SadlErrorMessages {

	/**
	 * '{0}' is a special reserved name. Please choose a different name.
	 */
	public static final ErrorMessage RESERVED_NAME = new ErrorMessage("reserved_name", ErrorMessage.SADL_MSG);
	public static final ErrorMessage IS_NOT_A = new ErrorMessage("is_not_a", ErrorMessage.SADL_MSG);
	public static final ErrorMessage PROP_NOT_EXIST = new ErrorMessage("property_not_exist", ErrorMessage.SADL_MSG);
	public static final ErrorMessage NOT_FOUND = new ErrorMessage("not_found", ErrorMessage.SADL_MSG);
	public static final ErrorMessage UNKNOWN_TYPE = new ErrorMessage("unknown_type", ErrorMessage.SADL_MSG);
	public static final ErrorMessage UNKNOWN_VALUE = new ErrorMessage("unknown_value", ErrorMessage.SADL_MSG);
	public static final ErrorMessage UNDEFINED = new ErrorMessage("undefined", ErrorMessage.SADL_MSG);
	public static final ErrorMessage UNIQUE_NAME = new ErrorMessage("unique_name", ErrorMessage.SADL_MSG);
	public static final ErrorMessage ONLY_ONE = new ErrorMessage("only_one", ErrorMessage.SADL_MSG);
	public static final ErrorMessage INVALID_FORMAT = new ErrorMessage("invalid_format", ErrorMessage.SADL_MSG);
	public static final ErrorMessage MISSING = new ErrorMessage("missing", ErrorMessage.SADL_MSG);
	public static final ErrorMessage CANNOT_ASSIGN_EXISTING = new ErrorMessage("cannot_assign_existing", ErrorMessage.SADL_MSG);
	public static final ErrorMessage UNIDENTIFIED = new ErrorMessage("unidentified", ErrorMessage.SADL_MSG);
	public static final ErrorMessage INVALID_PROP_TYPE = new ErrorMessage("invalid_prop_type", ErrorMessage.SADL_MSG);
	public static final ErrorMessage PROPERTY_RESTRICTIONS = new ErrorMessage("property_restrictions", ErrorMessage.SADL_MSG);
	public static final ErrorMessage INVALID_MEMBER_TYPE = new ErrorMessage("invalid_member_type", ErrorMessage.SADL_MSG);
	public static final ErrorMessage INCOMPATIBLE_MEMBER_TYPES = new ErrorMessage("incompatible_member_types", ErrorMessage.SADL_MSG);
	public static final ErrorMessage NOT_IN_RANGE = new ErrorMessage("not_in_range", ErrorMessage.SADL_MSG);
	public static final ErrorMessage CANNOT_CONVERT = new ErrorMessage("cannot_convert", ErrorMessage.SADL_MSG);
	public static final ErrorMessage INCOMPATIBLE_RANGE = new ErrorMessage("incompatible_range", ErrorMessage.SADL_MSG);
	public static final ErrorMessage UNHANDLED = new ErrorMessage("unhandled", ErrorMessage.SADL_MSG);
	public static final ErrorMessage INVALID_NULL = new ErrorMessage("invalid_null", ErrorMessage.SADL_MSG);
}
