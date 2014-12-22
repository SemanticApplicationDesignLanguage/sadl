/************************************************************************
 * Copyright © 2007-2014 - General Electric Company, All Rights Reserved
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
import java.util.HashMap;
import java.util.LinkedHashSet;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.impl.DefaultResourceDescriptionStrategy;
import org.eclipse.xtext.util.IAcceptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.Model;
import com.google.inject.Inject;

public class SadlResourceDescriptionStrategy extends DefaultResourceDescriptionStrategy {
	@Inject
	private SadlModelManager visitor;
	
	private final static Logger LOG = LoggerFactory.getLogger(SadlResourceDescriptionStrategy.class);

	@Override
	public boolean createEObjectDescriptions(EObject eObject, IAcceptor<IEObjectDescription> acceptor) {
		if (getQualifiedNameProvider() == null)
			return false;
		try {
			QualifiedName qualifiedName = getQualifiedNameProvider().getFullyQualifiedName(eObject);
			if (qualifiedName != null) {
				acceptor.accept(new SadlEObjectDescription(qualifiedName, eObject, new HashMap<String, String>()));
			}
		} catch (Exception exc) {
			LOG.error(exc.getMessage());
		}
		return true;
	}
	
	public LinkedHashSet<URI> getImportedURIs (Resource resource) {
		Model m = (Model) resource.getContents().get(0);
		
		LinkedHashSet<URI> importedURIs = new LinkedHashSet<URI>();
		for (Import imp: m.getImports()) {
			String altUri = imp.getImportURI();
			if (altUri.startsWith("http:")) {
				try {
					altUri = visitor.getAltUrl(altUri, resource.getURI());
					if (!imp.getImportURI().equals(altUri)) {
						importedURIs.add(URI.createURI(altUri));
					} else {
						LOG.warn(resource.getURI().lastSegment()+": Could not get alt uri for '"+altUri+"'");
					}
				} catch (MalformedURLException e) {
					e.printStackTrace();
				}
			}
		}
		return importedURIs;
	}
}
