package com.ge.research.sadl.cli;

import static com.ge.research.sadl.builder.ResourceManager.SADLEXTWITHPREFIX;
import static com.ge.research.sadl.cli.SadlCli.error;
import static com.ge.research.sadl.cli.SadlCli.info;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.builder.IXtextBuilderParticipant.BuildType;
import org.eclipse.xtext.builder.IXtextBuilderParticipant.IBuildContext;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.IResourceDescription.Delta;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.resource.impl.ResourceDescriptionsProvider;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.Files;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.IResourceValidator;
import org.eclipse.xtext.validation.Issue;

import com.ge.research.sadl.SadlRuntimeModule;
import com.ge.research.sadl.SadlStandaloneSetup;
import com.ge.research.sadl.builder.SadlBuilder;
import com.ge.research.sadl.sadl.SadlPackage;
import com.google.common.base.Function;
import com.google.common.base.Preconditions;
import com.google.common.base.Predicate;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.inject.Guice;
import com.google.inject.Inject;
import com.google.inject.Injector;

public class SadlCliHelper {

	@Inject
	XtextResourceSet resourceSet;
	
	@Inject
	SadlBuilder builder;

	@Inject
	ResourceDescriptionsProvider resourceDescriptionsProvider;
	
	SadlCliHelper() {
		createInjector().injectMembers(this);
	}

	void transform(Path path) throws Exception {
		try {
			Iterator<Path> itr = java.nio.file.Files.walk(path).iterator();
			Iterator<File> files = Iterators.transform(itr,
					new Function<Path, File>() {

						@Override
						public File apply(Path path) {
							return path.toFile();
						}
					});
			Iterator<File> sadlFiles = Iterators.filter(files,
					new Predicate<File>() {

						@Override
						public boolean apply(File file) {
							return isSadlFile(file);
						}
					});
			Iterator<URI> fileUris = Iterators.transform(sadlFiles,
					new Function<File, URI>() {

						@Override
						public URI apply(File file) {
							return toFileUri(file);
						}
					});
			Iterator<Resource> resources = Iterators.transform(fileUris,
					new Function<URI, Resource>() {

						@Override
						public Resource apply(URI uri) {
							return toResource(uri);
						}
					});

			Multimap<Resource, Issue> errors = HashMultimap
					.<Resource, Issue> create();
			
			
			final Map<Resource, List<Issue>> mapping = Maps.newHashMap();
			while (resources.hasNext()) {
				Resource resource = resources.next();
				mapping.put(resource, Collections.<Issue>emptyList());
			}
					
			builder.build(new IBuildContext() {

				@Override
				public IProject getBuiltProject() {
					return null;
				}

				@Override
				public List<Delta> getDeltas() {
					return Lists.newArrayList(Iterables.transform(mapping.keySet(), new Function<Resource, Delta>() {

						@Override
						public Delta apply(Resource input) {
							return toDelta(input);
						}
					}));
				}

				@Override
				public ResourceSet getResourceSet() {
					return resourceSet;
				}

				@Override
				public BuildType getBuildType() {
					return BuildType.FULL;
				}

				@Override
				public void needRebuild() {
				}
				
				//@Override Required for 2.9 builder from Maven
				public boolean isSourceLevelURI(URI uri) {
					return true;
				}
				
			}, new NullProgressMonitor());
			
			for (Resource resource : mapping.keySet()) {
				info("Validating resource: " + deresolve(resource.getURI()));
				List<Issue> issues = validate(resource);
				mapping.put(resource, issues);
				if (!issues.isEmpty()) {
					for (Issue issue : issues) {
						info(" - " + issue);
						if (issue.getSeverity() == Severity.ERROR) {
							errors.put(resource, issue);
						}
					}
				}
			}
			
			if (!errors.asMap().keySet().isEmpty()) {
				error("_______________________________________ GENERATION ERRORS _______________________________________");
				for (Resource resource : errors.asMap().keySet()) {
					error(" Generation failed for resource: "
							+ deresolve(resource.getURI()));
				}
			}
		} finally {
			Iterator<Resource> itr = resourceSet.getResources().iterator();
			while (itr.hasNext()) {
				Resource resource = itr.next();
				resource.unload();
				itr.remove();
			}
		}
	}

	private boolean isSadlFile(File it) {
		return it != null && it.isFile()
				&& it.getName().endsWith(SADLEXTWITHPREFIX);
	}
	
	private Delta toDelta(final Resource resource) {
		return new Delta() {
			
			@Override
			public boolean haveEObjectDescriptionsChanged() {
				return true;
			}
			
			@Override
			public URI getUri() {
				return resource.getURI();
			}
			
			@Override
			public IResourceDescription getOld() {
				return null;
			}
			
			@Override
			public IResourceDescription getNew() {
				return resourceDescriptionsProvider.getResourceDescriptions(resource).getResourceDescription(getUri());
			}
		};
	}

	private URI toFileUri(File it) {
		try {
			return URI.createFileURI(it.getCanonicalPath());
		} catch (IOException e) {
			throw new RuntimeException("Error while trying to get canonical path for: " + it);
		}
	}

	private Resource toResource(URI it) {
		Resource resource = resourceSet.createResource(it);
		String filePath = it.toFileString();
		Preconditions.checkNotNull(filePath, "Cannot find file for URI: " + it);
		String content = Files.readFileIntoString(filePath);
		try {
			resource.load(new StringInputStream(content), null);
		} catch (IOException e) {
			throw new RuntimeException("Error while trying to load resource:l "
					+ it, e);
		}
		return resource;
	}

	private List<Issue> validate(Resource it) {
		return getValidator(it).validate(it, CheckMode.ALL,
				CancelIndicator.NullImpl);
	}

	private IResourceValidator getValidator(Resource it) {
		if (it instanceof XtextResource) {
			return ((XtextResource) it).getResourceServiceProvider().get(
					IResourceValidator.class);
		}
		return IResourceValidator.NULL;
	}

	private URI deresolve(URI it) {
		return it.deresolve(SadlCli.projectRootUri.get());
	}
	
	private static Injector createInjector() {
		SadlPackage.eINSTANCE.getNsURI();
		SadlStandaloneSetup.doSetup();
		Injector injector = Guice.createInjector(new SadlRuntimeModule());
		if (!EPackage.Registry.INSTANCE.containsKey(SadlPackage.eINSTANCE
				.getNsURI())) {
			EPackage.Registry.INSTANCE.put(SadlPackage.eINSTANCE.getNsURI(),
					SadlPackage.eINSTANCE);
		}
		new SadlStandaloneSetup().register(injector);
		return injector;
	}

}
