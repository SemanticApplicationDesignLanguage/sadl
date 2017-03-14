package com.ge.research.sadl.cli

import com.ge.research.sadl.SadlRuntimeModule
import com.ge.research.sadl.SadlStandaloneSetup
import com.ge.research.sadl.builder.SadlModelManager
import com.ge.research.sadl.sadl.SadlPackage
import com.google.common.base.Preconditions
import com.google.common.collect.HashMultimap
import com.google.inject.Guice
import com.google.inject.Inject
import java.io.File
import java.nio.file.Path
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.SubMonitor
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EPackage
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.diagnostics.Severity
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.resource.XtextResourceSet
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.util.Files
import org.eclipse.xtext.util.StringInputStream
import org.eclipse.xtext.validation.CheckMode
import org.eclipse.xtext.validation.IResourceValidator

import static com.ge.research.sadl.builder.ResourceManager.*

import static extension java.nio.file.Files.*
import org.eclipse.xtext.validation.Issue

package class SadlCliHelper {

	@Inject
	XtextResourceSet resourceSet;

	@Inject
	SadlModelManager modelManager;

	package new() {
		createInjector.injectMembers(this);
	}

	package def transform(Path path) {
		try {
			val resources = path.walk.iterator.map[toFile].filter[sadlFile].map[toFileUri].map[toResource];
			val errors = HashMultimap.<Resource, Issue>create;
			val mapping = resources.toInvertedMap[validate];
			mapping.forEach [ resource, issues |
				if (!issues.nullOrEmpty) {
					info('''Resource: «deresolve(resource.URI)»:''')
					issues.forEach [
						info(''' - «it»''');
						if (severity === Severity.ERROR) {
							errors.put(resource, it);
						}
					]
				}
			];
			mapping.keySet.forEach [
				info('''Processing «deresolve(URI)»...''');
				val issues = errors.get(it);
				if (!issues.nullOrEmpty) {
					error('''Cannont process resource as it has errors. Resource URI: «URI».''');
					issues.forEach [
						error(it);
					];
				} else {
					modelManager.processModel(it, true, false, SubMonitor.convert(new NullProgressMonitor));
				}
			];
			if (!errors.asMap.keySet.nullOrEmpty) {
				error('_______________________________________ GENERATION ERRORS _______________________________________')
				errors.asMap.keySet.forEach[
					error('''Generation failed for resource: «deresolve(URI)».''');
				];
			}
		} finally {
			val itr = resourceSet.resources.iterator;
			while (itr.hasNext) {
				val resource = itr.next;
				resource.unload;
				itr.remove;
			}
		}
	}

	private def isSadlFile(File it) {
		return it !== null && file && name.endsWith(SADLEXTWITHPREFIX);
	}

	private def toFileUri(File it) {
		return URI.createFileURI(absolutePath);
	}

	private def toResource(URI it) {
		val resource = resourceSet.createResource(it);
		val filePath = toFileString;
		Preconditions.checkNotNull(filePath, '''Cannot find file for URI: «it».''');
		val content = Files.readFileIntoString(filePath);
		resource.load(new StringInputStream(content), null);
		return resource;
	}

	private def validate(Resource it) {
		return getValidator(it).validate(it, CheckMode.ALL, CancelIndicator.NullImpl);
	}

	private def getValidator(Resource it) {
		if (it instanceof XtextResource) {
			return (it as XtextResource).resourceServiceProvider.get(IResourceValidator);
		}
		return IResourceValidator.NULL;
	}
	
	private def info(Object it) {
		println('''INFO  [SADL-CLI]: «it»''');
	}
	
	private def error(Object it) {
		System.err.println('''ERROR [SADL-CLI]: «it»''');
	}
	
	private def deresolve(URI it) {
		deresolve(SadlCli.projectRootUri.get())
	}

	private static def createInjector() {
		SadlPackage.eINSTANCE.nsURI;
		SadlStandaloneSetup.doSetup();
		val injector = Guice.createInjector(new SadlRuntimeModule);
		if (!EPackage.Registry.INSTANCE.containsKey(SadlPackage.eINSTANCE.getNsURI())) {
			EPackage.Registry.INSTANCE.put(SadlPackage.eINSTANCE.getNsURI(), SadlPackage.eINSTANCE);
		}
		new SadlStandaloneSetup().register(injector);
		return injector;
	}

}
