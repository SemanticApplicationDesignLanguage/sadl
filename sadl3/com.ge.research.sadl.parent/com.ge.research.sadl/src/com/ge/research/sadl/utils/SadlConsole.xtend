/** 
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
 * Project: SADL
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 */
package com.ge.research.sadl.utils

import com.ge.research.sadl.builder.MessageManager.MessageType
import com.google.inject.ImplementedBy

/** 
 * Bare minimum representation of a IDE and UI agnostic console that can be used
 * to log messages.
 *
 * @author akos.kitta
 */
@ImplementedBy(typeof(SadlConsole.Default))
interface SadlConsole {

	/** 
	 * Prints the given message to the console.
	 * @param typethe severity of the message to print.
	 * @param messagethe message to print.
	 */
	def void print(MessageType type, String message)

	def void info(String message) {
		print(MessageType.INFO, message)
	}

	def void warn(String message) {
		print(MessageType.WARN, message)
	}

	def void error(String message) {
		print(MessageType::ERROR, message)
	}

	/** 
	 * The default console that logs to the STD out when the message type if either{@code INFO} or {@code WARN},
	 * and to the STD err if the message type is{@code ERROR}.
	 * 
	 * @author akos.kitta
	 */
	static package class Default implements SadlConsole {
		override void print(MessageType type, String message) {
			if (MessageType.ERROR === type) {
				System.err.print(message)
			} else {
				System.out.print(message)
			}
		}
	}

}
