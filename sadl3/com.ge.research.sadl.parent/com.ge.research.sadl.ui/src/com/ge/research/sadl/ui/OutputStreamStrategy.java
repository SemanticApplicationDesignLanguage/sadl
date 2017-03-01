/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui;

import java.io.PrintStream;

import com.ge.research.sadl.builder.MessageManager.MessageType;

/**
 * Enumeration of output stream types and strategies. This strategy is used of
 * debugging and testing purposes when one would like to see the actual stack
 * traces outside of SADL IDE.
 * 
 * @author akos.kitta
 *
 */
public enum OutputStreamStrategy {

	/**
	 * Type for the standard output. Used for testing.
	 */
	STD {

		private PrintStream OUT = System.out;
		private PrintStream ERR = System.err;

		@Override
		protected PrintStream getOut() {
			return OUT;
		}

		@Override
		protected PrintStream getErr() {
			return ERR;
		}

	},

	/**
	 * The SADL specific output stream type. Uses the Eclipse-based SADL console
	 * inside the Eclipse RCP.
	 */
	SADL {

		private PrintStream OUT = new PrintStream(SadlConsole.getOutputStream(MessageType.INFO));
		private PrintStream ERR = new PrintStream(SadlConsole.getOutputStream(MessageType.ERROR));

		@Override
		protected PrintStream getOut() {
			return OUT;
		}

		@Override
		protected PrintStream getErr() {
			return ERR;
		}

	};

	/**
	 * Returns with the print stream used for printing the standard output.
	 */
	protected abstract PrintStream getOut();

	/**
	 * Returns with the print stream for the error output.
	 */
	protected abstract PrintStream getErr();

	/**
	 * Sets and uses the current the output stream type as the default one.
	 */
	public void use() {
		System.setOut(getOut());
		System.setErr(getErr());
	}

}