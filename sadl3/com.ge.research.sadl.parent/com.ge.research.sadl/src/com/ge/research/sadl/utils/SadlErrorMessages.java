package com.ge.research.sadl.utils;

import com.ge.research.messages.ErrorMessage;

/**
 * See the following file for definitions of these messages:
 *
 */
public final class SadlErrorMessages {

	public static final ErrorMessage TEST = new ErrorMessage("test");
	public static final ErrorMessage TEST1 = new ErrorMessage("test1");
	public static final ErrorMessage TEST2 = new ErrorMessage("test2");
	/**
	 * '{0}' is a special reserved name. Please choose a different name.
	 */
	public static final ErrorMessage RESERVED_NAME = new ErrorMessage("reserved_file_name");
	public static final ErrorMessage IS_NOT_A = new ErrorMessage("is_not_a");
	public static final ErrorMessage PROP_NOT_EXIST = new ErrorMessage("property_not_exist");
	public static final ErrorMessage NOT_FOUND = new ErrorMessage("not_found");
}
