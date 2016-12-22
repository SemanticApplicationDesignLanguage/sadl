/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.editor;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.ui.editor.copyqualifiedname.DefaultCopyQualifiedNameService;

import com.ge.research.sadl.utils.SadlQualifiedNameToStringService;
import com.google.inject.Inject;

/**
 * Qualified name copy service that produces unique OWL2 URIs of knowledge
 * elements instead of internal application specific qualified names.
 * 
 * <p>
 * The produced URI has the following pattern:
 * 
 * <pre>
 * &lt;resourceURI&gt;#&lt;localName&gt;
 * </pre>
 * 
 * @author akos.kitta
 *
 */
@SuppressWarnings("restriction")
public class SadlCopyQualifiedNameService extends DefaultCopyQualifiedNameService {

	@Inject
	private SadlQualifiedNameToStringService qualifiedNameToStringService;
	
	@Override
	protected String toString(EObject it, QualifiedName fullyQualifiedName) {
		return qualifiedNameToStringService.toString(fullyQualifiedName);
	}

}
