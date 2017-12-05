/************************************************************************
 * Copyright \u00a9 2007-2010 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.model.gp;

public class KnownNode extends Node {
	public final static String value = "known";
	@Override
	public boolean equals(Object otherNode) {
		if (otherNode instanceof KnownNode) {
			return true;
		}
		return false;
	}
	
	@Override
	public String toString() {
		return value;
	}

	@Override
	public String toFullyQualifiedString() {
		return toString();
	}

	@Override
	public String toDescriptiveString() {
		return toString();
	}
}
