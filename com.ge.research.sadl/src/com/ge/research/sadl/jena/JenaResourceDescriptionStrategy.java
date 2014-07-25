package com.ge.research.sadl.jena;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.resource.IDefaultResourceDescriptionStrategy;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.IReferenceDescription;
import org.eclipse.xtext.util.IAcceptor;

public class JenaResourceDescriptionStrategy implements IDefaultResourceDescriptionStrategy {
	
	@Override
	public boolean createEObjectDescriptions(EObject eObject,
			IAcceptor<IEObjectDescription> acceptor) {
		return false;
	}

	@Override
	public boolean createReferenceDescriptions(EObject eObject,
			URI exportedContainerURI, IAcceptor<IReferenceDescription> acceptor) {
		return false;
	}

}
