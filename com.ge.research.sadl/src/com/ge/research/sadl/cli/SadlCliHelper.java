package com.ge.research.sadl.cli;

import static com.ge.research.sadl.builder.ResourceManager.SADLEXTWITHPREFIX;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.Files;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.IResourceValidator;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.validation.Issue.IssueImpl;

import com.ge.research.sadl.SadlRuntimeModule;
import com.ge.research.sadl.SadlStandaloneSetup;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.sadl.SadlPackage;
import com.google.common.base.Function;
import com.google.common.base.Preconditions;
import com.google.common.base.Predicate;
import com.google.common.base.Throwables;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Iterators;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.inject.Guice;
import com.google.inject.Inject;
import com.google.inject.Injector;

public class SadlCliHelper {

	@Inject
	XtextResourceSet resourceSet;

	@Inject
	SadlModelManager modelManager;

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
			Map<Resource, List<Issue>> mapping = Maps.toMap(resources,
					new Function<Resource, List<Issue>>() {

						@Override
						public List<Issue> apply(Resource resource) {
							return validate(resource);
						}
					});

			for (Entry<Resource, List<Issue>> entry : mapping.entrySet()) {
				Resource resource = entry.getKey();
				List<Issue> issues = entry.getValue();
				if (!issues.isEmpty()) {
					info("Resource: " + deresolve(resource.getURI()));
					for (Issue issue : issues) {
						info(" - " + issue);
						if (issue.getSeverity() == Severity.ERROR) {
							errors.put(resource, issue);
						}
					}
				}
			}

			for (Resource resource : mapping.keySet()) {
				info("Processing " + deresolve(resource.getURI()) + "...");
				Collection<Issue> issues = errors.get(resource);
				if (!issues.isEmpty()) {
					error("Cannot process resource as it has errors. Resource URI: "
							+ deresolve(resource.getURI()));
					for (Issue issue : issues) {
						error(issue);
					}
				} else {
					try {
						modelManager.processModel(resource, true, false,
								SubMonitor.convert(new NullProgressMonitor()));
					} catch (CoreException e) {
						error(Throwables.getStackTraceAsString(e));
						final IssueImpl issue = new Issue.IssueImpl();
						issue.setMessage(e.getMessage());
						errors.put(resource, issue);
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

	private URI toFileUri(File it) {
		return URI.createFileURI(it.getAbsolutePath());
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

	private void info(Object it) {
		System.out.println("INFO  [SADL-CLI]: " + it);
	}

	private void error(Object it) {
		System.err.println("ERROR [SADL-CLI]: " + it);
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
