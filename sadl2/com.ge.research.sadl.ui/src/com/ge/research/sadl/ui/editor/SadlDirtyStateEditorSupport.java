package com.ge.research.sadl.ui.editor;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.URIConverter;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.DirtyStateEditorSupport;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.util.Pair;

import com.ge.research.sadl.builder.ResourceManager;
import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.inject.Inject;

public class SadlDirtyStateEditorSupport extends DirtyStateEditorSupport {

	@Inject
	private IStorage2UriMapper mapper;

	protected URI getOwlURI(URIConverter conv, URI sadlURI) {
		URI norm = conv.normalize(sadlURI);
		if (norm.isPlatformResource()) {
			List<String> seg = Lists.newArrayList(norm.trimFileExtension()
					.appendFileExtension(ResourceManager.getOwlFileExtension()).segmentsList()); // "owl").segmentsList());
			seg.remove(0);
			seg.add(1, "OwlModels");
			norm = URI.createPlatformResourceURI(Joiner.on("/").join(seg), false);
			for (Pair<IStorage,IProject> storages: mapper.getStorages(norm)) {
				IStorage s = storages.getFirst();
				if (s instanceof IResource) {
					norm = URI.createURI(((IResource) s).getLocationURI()
							.toString());
				}
			}
		}
		return norm;
	}

	protected Collection<Resource> collectAffectedResources(
			XtextResource resource, IResourceDescription.Event event) {
		List<Resource> result = Lists.newArrayListWithExpectedSize(4);
		ResourceSet resourceSet = resource.getResourceSet();
		URIConverter converter = resourceSet.getURIConverter();
		Set<URI> norm = Sets.newHashSetWithExpectedSize(event.getDeltas()
				.size());
		for (IResourceDescription.Delta delta : event.getDeltas()) {
			if (delta.getNew() != null) {
				norm.add(converter.normalize(delta.getNew().getURI()));
				norm.add(getOwlURI(converter, delta.getNew().getURI()));
			} else if (delta.getOld() != null) {
				norm.add(converter.normalize(delta.getOld().getURI()));
				norm.add(getOwlURI(converter, delta.getOld().getURI()));
			}
		}
		for (Resource res : resourceSet.getResources()) {
			if (res != resource && res != null) {
				URI normalized = converter.normalize(res.getURI());
				if (norm.contains(normalized))
					result.add(res);
			}
		}
		return result;
	}
}
