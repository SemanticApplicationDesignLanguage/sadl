package com.ge.research.sadl.scoping;

import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.Resource.Factory;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;
import org.eclipse.xtext.linking.impl.DefaultLinkingService;
import org.eclipse.xtext.linking.impl.IllegalNodeException;
import org.eclipse.xtext.nodemodel.INode;

import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.SadlFactory;

public class SadlLinkingService extends DefaultLinkingService {

	private final URI SADL_URI = URI.createURI("sadl.linking.service");

    @Override
    public List<EObject> getLinkedObjects(EObject context, EReference ref,
            INode node) throws IllegalNodeException {

        List<EObject> linkedObjects = super.getLinkedObjects(context, ref, node);
        if (linkedObjects.isEmpty()) {
    		final String name = getCrossRefNodeAsString(node);
    		if (name != null) {
    			Resource resource = context.eResource().getResourceSet().getResource(SADL_URI, false);
    			if (resource == null) {
    				resource = context.eResource().getResourceSet().createResource(SADL_URI);
    				// In headless case the default XMI resource factory is missing.
    				if (resource == null) {
    					final Factory factory = new XMIResourceFactoryImpl();
    					resource = factory.createResource(SADL_URI);
    					context.eResource().getResourceSet().getResources().add(resource);
    				}
    			}
    			ResourceName resourceName = SadlFactory.eINSTANCE.createResourceName();
    			resourceName.setName(name);
    			resource.getContents().add(resourceName);
    			linkedObjects = Collections.singletonList((EObject)resourceName);
    		}
        }
        return linkedObjects;
    }

}
