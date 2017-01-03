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
package com.ge.research.sadl.tests.helpers

/**
 * Provides a couple of convenient, utility methods to handle Xtend templates
 * operating system independently.
 * 
 * @author akos.kitta
 */
class XtendTemplateHelper {

	static val EMPTY_STRING = '';

	static val WIN_EOL = '\r\n';
	static val UNIX_EOL = '\n';

	/**
	 * Returns with the exact length of the string.
	 * 
	 * <p>
	 * This method could come handy when trying to get the length of a Xtend
	 * template. On Windows, a new line consists of a pair of carriage return and
	 * line feed <i>("\r\n")</i> while on UNIX platforms a new line is a single 
	 * <i>("\n")</i> whitespace character.
	 * 
	 * @param it
	 *            the template to check its length. If {@code null}, returns with
	 *            {@code 0}.
	 * @return the exact length of the argument.
	 */
	static def exactLength(CharSequence it) {
		return it.unifyEOL.length;
	}

	/**
	 * Replaces all carriage return - line feed pairs with a single line feed character.
	 * Returns with a new modified string instance. Returns with an empty string if the 
	 * argument is {@code null}.
	 * 
	 * @param it
	 *            the string in which the EOL has to be escaped.
	 * @return a new escaped string instance.
	 */
	static def unifyEOL(CharSequence it) {
		return if (it === null) EMPTY_STRING else it.toString.replaceAll(WIN_EOL, UNIX_EOL);
	}

}
