/************************************************************************
 * Copyright © 2022 - Natural Semantics, LLC. All Rights Reserved.
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
package com.ge.research.sadl.reasoner;

@SuppressWarnings("serial")
public class UnittedQuantityNotSupportedException extends TranslationException {
	private String expandedUnittedQuantityReturnType = null;

	public UnittedQuantityNotSupportedException(String message, String expUri) {
		super(message);
		setExpandedUnittedQuantityReturnType(expUri);
	}

	public UnittedQuantityNotSupportedException(String message, Exception e, String expUri) {
		super(message, e);
		setExpandedUnittedQuantityReturnType(expUri);
	}

	public String getExpandedUnittedQuantityReturnType() {
		return expandedUnittedQuantityReturnType;
	}

	private void setExpandedUnittedQuantityReturnType(String expandedUnittedQuantityReturnType) {
		this.expandedUnittedQuantityReturnType = expandedUnittedQuantityReturnType;
	}
}
