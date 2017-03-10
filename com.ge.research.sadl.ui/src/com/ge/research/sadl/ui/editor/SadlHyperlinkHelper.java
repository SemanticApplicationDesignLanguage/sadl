package com.ge.research.sadl.ui.editor;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.URIConverter;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.jface.text.Region;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.impl.ResourceDescriptionsProvider;
import org.eclipse.xtext.ui.editor.hyperlinking.HyperlinkHelper;
import org.eclipse.xtext.ui.editor.hyperlinking.IHyperlinkAcceptor;
import org.eclipse.xtext.ui.editor.hyperlinking.XtextHyperlink;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.naming.SadlSimpleNameProvider;
import com.ge.research.sadl.resource.SadlEObjectDescription;
import com.ge.research.sadl.sadl.ModelName;
import com.ge.research.sadl.builder.ResourceManager;
import com.google.inject.Inject;

public class SadlHyperlinkHelper extends HyperlinkHelper {

	private static final Logger logger = LoggerFactory.getLogger(SadlHyperlinkHelper.class);

	@Inject
	private ResourceDescriptionsProvider resourceDescriptionsProvider;

	@Inject
	private SadlModelManager visitor;

	@Override
	public void createHyperlinksTo(XtextResource from, Region region,
			EObject to, IHyperlinkAcceptor acceptor) {

		final URIConverter uriConverter = from.getResourceSet()
				.getURIConverter();
		final String hyperlinkText = getLabelProvider().getText(to);
		String hyperAlias = "";
		String hyperObject = hyperlinkText;
		int containsAlias = hyperlinkText.indexOf(":"); 
		if (containsAlias > 0) {
			hyperAlias = hyperlinkText.substring(0, containsAlias);
			hyperObject = hyperlinkText.substring(containsAlias+1);
		}

		IResourceDescriptions index = resourceDescriptionsProvider
				.createResourceDescriptions();
		IResourceDescription desc = index.getResourceDescription(from.getURI());
		if (desc == null) {
			logger.error("XText error collecting resources. Re-build project");
			return;
		}
		List<String> searchedImports = new ArrayList<String>();
		String initialFile = from.getURI().lastSegment();
		searchedImports.add(initialFile);
		URI normalized = null;
		try {
			normalized = getToURI(from, uriConverter, hyperAlias, hyperObject, index, desc, searchedImports);
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (normalized != null) {
			createXtextHyperlink(region, acceptor, hyperlinkText, normalized);
		} else {
			final URI owlFile = EcoreUtil.getURI(to);
			// Open an SADL file instead of an OWL file wherever possible.
			final URI sadlFile = ResourceManager
					.validateAndReturnSadlFileEquivalentOfOwlUrl(owlFile);
			// Convert the URI back to a platform URI if it was an absolute URI.
			final URI platformUri = ResourceManager
					.convertAbsoluteUriToPlatformUri(sadlFile);
			normalized = uriConverter.normalize(platformUri);
			createXtextHyperlink(region, acceptor, hyperlinkText, normalized);
		}
	}

	private URI getToURI(XtextResource from, final URIConverter uriConverter,
			String hyperAlias, String hyperObject, IResourceDescriptions index,
			IResourceDescription desc, List<String> searchedImports) throws MalformedURLException {
		URI normalized = null;
		String impUri;
		IResourceDescription dependentResource;
		for (IEObjectDescription eo : desc.getExportedObjects()) {
			if (eo.getName() != null
					&& eo.getName() != null
					&& eo.getName().getLastSegment()
							.startsWith(SadlSimpleNameProvider.MATCH_TOKEN1)) {
				impUri = eo.getUserData(SadlEObjectDescription.IMPORT_KEY);
				if (impUri == null) {
					impUri = parse(eo.getName());
				}
				if (impUri != null) {
					if (impUri.startsWith("http:")) {
						impUri = visitor.getAltUrl(impUri, from.getURI());
					}
					dependentResource = findMatchingResource(
							index.getAllResourceDescriptions(), impUri);
					if (dependentResource == null) {
						visitor.getMessageManager().warn("'" + hyperObject + "' is in model '" + impUri + "' but this does not appear to come from a SADL definition.");
					}
					else {
						String resourceAlias = "";
						if (!hyperAlias.isEmpty()) {
							resourceAlias = getResourceAlias(dependentResource);
						}
						for (IEObjectDescription eo2 : dependentResource
								.getExportedObjects()) {
							if (eo2.getName() != null && eo2.getName().getLastSegment() != null
									&& resourceAlias.equals(hyperAlias)
									&& eo2.getName().getLastSegment().equals(hyperObject)) {
								final URI platformUri = ResourceManager
										.convertAbsoluteUriToPlatformUri(eo2.getEObjectURI());
								return uriConverter.normalize(platformUri);
							}
						}
						// no matches found in this resource, now search imported resources
						// first check if we are looping
						URI impUriAsUri = URI.createURI(impUri);
						boolean h = impUriAsUri.isHierarchical();
						String fileName = URI.createURI(impUri).lastSegment();
						if (fileName.endsWith(ResourceManager.getOwlFileExtensionWithPrefix())) {  //".owl")) {
							fileName = fileName.substring(0, fileName.length()-3) + "sadl";
						}
						if (searchedImports.contains(fileName) || searchedImports.size() > 50) {
							return null;
						}
						searchedImports.add(fileName);
						URI normalized2 = getToURI(from, uriConverter, hyperAlias, hyperObject,
												index, dependentResource, searchedImports);
						if (normalized2 != null) {
							return normalized2;
						}
					}
				}
			}
		}
		return normalized;
	}

	private void createXtextHyperlink(Region region,
			IHyperlinkAcceptor acceptor, final String hyperlinkText,
			URI normalized) {
		XtextHyperlink result = getHyperlinkProvider().get();
		result.setHyperlinkRegion(region);
		result.setURI(normalized);
		result.setHyperlinkText(hyperlinkText);
		acceptor.accept(result);
	}

	private IResourceDescription findMatchingResource(
			Iterable<IResourceDescription> iterable, String impUri) {
		if (impUri != null) {
			int lastSlash = impUri.lastIndexOf('/');
			String sadlFilename = lastSlash >= 0 ? impUri.substring(lastSlash)
					: impUri;
			if (sadlFilename.endsWith(ResourceManager.getOwlFileExtensionWithPrefix())) {   // ".owl")) {
				sadlFilename = sadlFilename.substring(0,
						sadlFilename.length() - 3) + "sadl";
			}
			for (IResourceDescription resource : iterable) {
				if (resource.getURI().toString().endsWith(sadlFilename)) {
					return resource;
				}
			}
		}
		return null;
	}

	private String parse(QualifiedName name) {
		String importUri = null;
		String nameStr = name.getLastSegment();
		if (nameStr != null) {
			String match1 = "import_";
			int match1Size = match1.length();
			int index1 = nameStr.indexOf(match1);
			if (index1 >= 0) {
				index1 += match1Size;
				int index2 = nameStr.indexOf("__alias");
				if (index2 > index1) {
					importUri = nameStr.substring(index1, index2);
					importUri = importUri.replaceAll(
							SadlSimpleNameProvider.DOT_REPLACE_TOKEN, "\\.");
				}
			}
		}
		return importUri;
	}
	
	private String getResourceAlias(IResourceDescription dependentResource) {
		String alias = "";
		URI dpUri = dependentResource.getURI();
		ResourceSet rs = new ResourceSetImpl();
		Resource dpResource = rs.getResource(dpUri, true);
        for (TreeIterator<EObject> iter = EcoreUtil.getAllContents(dpResource, true); iter.hasNext();) { 
            EObject eObject = iter.next();
			if (eObject instanceof ModelName) {
				alias = ((ModelName)eObject).getAlias();
				if (alias == null) alias = "";
				return alias;
			}
        }
        return alias;
	}

}
