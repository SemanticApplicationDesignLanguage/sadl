package com.ge.research.sadl.ui.editor;

import java.util.Iterator;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jface.text.Region;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.EObjectAtOffsetHelper;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.impl.ResourceDescriptionsProvider;
import org.eclipse.xtext.ui.editor.hyperlinking.HyperlinkHelper;
import org.eclipse.xtext.ui.editor.hyperlinking.IHyperlinkAcceptor;
import org.eclipse.xtext.ui.editor.hyperlinking.XtextHyperlink;

import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.SadlPackage;
import com.google.inject.Inject;

public class SadlHyperlinkHelper extends HyperlinkHelper {
	@Inject
	private ResourceDescriptionsProvider resourceDescriptionsProvider;

	@Inject
	private EObjectAtOffsetHelper eObjectAtOffsetHelper;

	
	@Override
	public void createHyperlinksByOffset(XtextResource resource, int offset,
			IHyperlinkAcceptor acceptor) {
		super.createHyperlinksByOffset(resource, offset, acceptor);
		EObject element = eObjectAtOffsetHelper.resolveElementAt(resource, offset);
		if (element!=null && element instanceof Import) {
			Import imp = (Import) element;
			INode node = NodeModelUtils.getNode(element);

			
			URI publicUri = URI.createURI(imp.getImportURI());
			URI uriForPublicUri = getUriForPublicUri(publicUri, resource.getResourceSet());
			if (uriForPublicUri!=null) {
				XtextHyperlink hyperlink = getHyperlinkProvider().get();
				Region region = new Region(node.getOffset(), node.getLength());
				hyperlink.setHyperlinkRegion(region);
				hyperlink.setURI(uriForPublicUri);
				hyperlink.setHyperlinkText(uriForPublicUri.toString());
				acceptor.accept(hyperlink);
			}
		}
		
	}
	
	private URI getUriForPublicUri (URI publicUri, ResourceSet resourceSet) {
		IResourceDescriptions descriptions = resourceDescriptionsProvider.getResourceDescriptions(resourceSet);
		Iterable<IEObjectDescription> matchingModels = descriptions.getExportedObjects(SadlPackage.Literals.MODEL, QualifiedName.create(publicUri.toString()), false);
		Iterator<IEObjectDescription> it = matchingModels.iterator();
		if (it.hasNext()) {
			IEObjectDescription description = it.next();
			// This will be the URI of the SADL file
			return description.getEObjectURI().trimFragment();
		}
		return null;
	}
	

}
