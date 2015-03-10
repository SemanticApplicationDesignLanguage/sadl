package com.ge.research.sadl.scoping;

import java.util.Collections;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.xtext.linking.impl.DefaultLinkingService;
import org.eclipse.xtext.linking.impl.IllegalNodeException;
import org.eclipse.xtext.nodemodel.INode;

import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlFactory;
import com.ge.research.sadl.sadl.SadlPackage;

public class SadlLinkingService extends DefaultLinkingService {
	@Override
	public List<EObject> getLinkedObjects(EObject context, EReference ref,
			INode node) throws IllegalNodeException {
		List<EObject> linkedObjects = super.getLinkedObjects(context, ref, node);
		
		if (linkedObjects.isEmpty() && ref == SadlPackage.Literals.RESOURCE_BY_NAME__NAME) {
			ResourceName rn = SadlFactory.eINSTANCE.createResourceName();
			final String crossRefString = getCrossRefNodeAsString(node);
			rn.setName(crossRefString);
			return Collections.<EObject>singletonList(rn);
		}
		return linkedObjects;
	}
}
