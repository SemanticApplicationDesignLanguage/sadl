package com.ge.research.sadl.scoping;

import java.util.Collections;
import java.util.ConcurrentModificationException;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.linking.impl.DefaultLinkingService;
import org.eclipse.xtext.linking.impl.IllegalNodeException;
import org.eclipse.xtext.nodemodel.INode;

import com.ge.research.sadl.resource.SadlResource;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.Rule;
import com.ge.research.sadl.sadl.SadlFactory;
import com.ge.research.sadl.sadl.SadlPackage;

/**
 * In SADL a ResourceByName may link to a ResourceName defined elsewhere, but it is also possible that it
 * refers to a variable in a rule. Such a variable does not need to be declared, thus there is no object
 * that can be linked from the scope.
 * As a workaround, a virtual ResourceName is derived when the reference {@link ResourceByName#getName()}
 * cannot be linked to an object from the scope. This ResourceName is added to the root of the SadlResource,
 * but not directly in this class. It has to happen in SadlResource after the proxies are resolved. Adding
 * the object to the resource after creation would result in a {@link ConcurrentModificationException}. Thus
 * the elements are queued for addition by calling {@link SadlResource#addDerivedResourceName(ResourceName)}.
 *
 * @author thoms
 *
 */
public class SadlLinkingService extends DefaultLinkingService {
	@Override
	public List<EObject> getLinkedObjects(EObject context, EReference ref,
			INode node) throws IllegalNodeException {
		List<EObject> linkedObjects = super.getLinkedObjects(context, ref, node);

		if (linkedObjects.isEmpty() && ref == SadlPackage.Literals.RESOURCE_BY_NAME__NAME && doCreateVirtualResourceName(context)) {
			ResourceName rn = null;
			final String crossRefString = getCrossRefNodeAsString(node);
			for (EObject o: context.eResource().getContents()) {
				if (o instanceof ResourceName && ((ResourceName)o).getName().equals(crossRefString)) {
					rn = (ResourceName) o;
					break;
				}
			}
			if (rn == null) {
				rn = SadlFactory.eINSTANCE.createResourceName();
				rn.setName(crossRefString);
				((SadlResource)context.eResource()).addDerivedResourceName(rn);
			}
			return Collections.<EObject>singletonList(rn);
		}
		return linkedObjects;
	}
	
	/**
	 * Decide whether a virtual ResourceName should be created for the given context for the case that scoping failed.
	 */
	private boolean doCreateVirtualResourceName (EObject context) {
		return EcoreUtil2.getContainerOfType(context, Rule.class)!=null;
	}
}
