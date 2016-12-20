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

import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;

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

	@Override
	protected String toString(EObject it, QualifiedName fullyQualifiedName) {
		if (fullyQualifiedName != null) {
			final int segmentCount = fullyQualifiedName.getSegmentCount();
			if (segmentCount > 1) {
				return new StringBuilder(
						Joiner.on(":").join(Iterables.limit(fullyQualifiedName.getSegments(), segmentCount - 1)))
								.append("#").append(fullyQualifiedName.getLastSegment()).toString();
			}
		}
		return super.toString(it, fullyQualifiedName);
	}

}
