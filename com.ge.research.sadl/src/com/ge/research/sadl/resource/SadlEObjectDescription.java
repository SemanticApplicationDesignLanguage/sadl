/************************************************************************
 * Copyright © 2007-2015 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.resource;

import java.net.MalformedURLException;
import java.util.Map;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.EObjectDescription;

import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.sadl.Import;
import com.google.inject.Inject;

public class SadlEObjectDescription extends EObjectDescription {

	@Inject
	private SadlModelManager visitor;
	
	public static final String IMPORT_KEY = "import";

	public SadlEObjectDescription(QualifiedName qualifiedName, EObject element,
			Map<String, String> userData) {
		super(qualifiedName, element, userData);
		Resource resource = element.eResource();
		handleImport(userData, resource, element);
	}

	private void handleImport(Map<String, String> userData, Resource resource,
			EObject eObject) {
		if (visitor != null && eObject instanceof Import) {
			String impUri = ((Import) eObject).getImportURI();
			if (impUri.startsWith("http:")) {
				try {
					impUri = visitor.getAltUrl(impUri, resource.getURI());
				} catch (MalformedURLException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			userData.put(IMPORT_KEY, impUri);
		}
	}
}
