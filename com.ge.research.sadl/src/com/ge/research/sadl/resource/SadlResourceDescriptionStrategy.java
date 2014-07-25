package com.ge.research.sadl.resource;

import java.util.HashMap;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.impl.DefaultResourceDescriptionStrategy;
import org.eclipse.xtext.util.IAcceptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SadlResourceDescriptionStrategy extends DefaultResourceDescriptionStrategy {
	
	private final static Logger LOG = LoggerFactory.getLogger(SadlResourceDescriptionStrategy.class);

	
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
}
