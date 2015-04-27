package com.ge.research.sadl.scoping;

import java.util.Collections;
import java.util.ConcurrentModificationException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.linking.impl.DefaultLinkingService;
import org.eclipse.xtext.linking.impl.IllegalNodeException;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.util.OnChangeEvictingCache;
import org.eclipse.xtext.util.OnChangeEvictingCache.CacheAdapter;

import com.ge.research.sadl.resource.SadlResource;
import com.ge.research.sadl.sadl.Query;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.sadl.ResourceName;
import com.ge.research.sadl.sadl.Rule;
import com.ge.research.sadl.sadl.SadlFactory;
import com.ge.research.sadl.sadl.SadlPackage;
import com.ge.research.sadl.sadl.Test;

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
	public List<EObject> getLinkedObjects(final EObject context, EReference ref,
			INode node) throws IllegalNodeException {
		List<EObject> linkedObjects = super.getLinkedObjects(context, ref, node);

		CacheAdapter cache = new OnChangeEvictingCache().getOrCreate(context.eResource());
		cache.addCacheListener(new OnChangeEvictingCache.Listener() {
			
			@Override
			public void onEvict(CacheAdapter cache) {
				SadlResource resource = (SadlResource) context.eResource();
				Set<ResourceName> toRemoveResourceNames = new HashSet<>();
				if (resource == null) return;
				for (EObject obj: resource.getContents()) {
					if ((obj instanceof ResourceName)) {
						resource.removeResourceName((ResourceName) obj);
						toRemoveResourceNames.add((ResourceName) obj);
					}
				}
				// unlink all ResourcyName instances that refer to deived ResourceNames
				for (ResourceByName rn : EcoreUtil2.eAllOfType(resource.getContents().get(0), ResourceByName.class)) {
					if (toRemoveResourceNames.contains(rn.getName()))
						rn.setName(null);
				}
			}
		});
		
		// TODO Invalidate cache on resource change
		if (linkedObjects.isEmpty() && ref == SadlPackage.Literals.RESOURCE_BY_NAME__NAME && doCreateVirtualResourceName(context)) {
			ResourceName rn = null;
			String resourceName = getResourceNamePrefix(context) +"."+ getCrossRefNodeAsString(node);
			for (EObject o: context.eResource().getContents()) {
				if (o instanceof ResourceName && ((ResourceName)o).getName().equals(resourceName)) {
					rn = (ResourceName) o;
					break;
				}
			}
			if (rn == null) {
				rn = SadlFactory.eINSTANCE.createResourceName();
				rn.setName(resourceName);
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
		return (EcoreUtil2.getContainerOfType(context, Rule.class)!=null || 
				EcoreUtil2.getContainerOfType(context, Query.class) != null ||
				EcoreUtil2.getContainerOfType(context, Test.class) != null);
	}
	
	private String getResourceNamePrefix (EObject context) {
		EObject containerContext = EcoreUtil2.getContainerOfType(context, Rule.class);
		int index = 0;
		if (containerContext!=null) {
			List<? extends EObject> allOfType = EcoreUtil2.eAllOfType(containerContext.eContainer(), containerContext.getClass());
			if (containerContext instanceof Rule && ((Rule)containerContext).getName() != null) {
				return ((Rule)containerContext).getName();
			}
			else {
				index = allOfType.indexOf(containerContext);
				return containerContext.eClass().getName()+"."+index;
			}
		} 
		
		containerContext = EcoreUtil2.getContainerOfType(context, Query.class);
		if (containerContext!=null) {
			List<? extends EObject> allOfType = EcoreUtil2.eAllOfType(containerContext.eContainer(), containerContext.getClass());
			index = allOfType.indexOf(containerContext);
			return containerContext.eClass().getName()+"."+index;
		} 
		
		containerContext = EcoreUtil2.getContainerOfType(context, Test.class);
		if (containerContext!=null) {
			List<? extends EObject> allOfType = EcoreUtil2.eAllOfType(containerContext.eContainer(), containerContext.getClass());
			index = allOfType.indexOf(containerContext);
			return containerContext.eClass().getName()+"."+index;
		} 
		else { 
			return "";
		}
	}
}
