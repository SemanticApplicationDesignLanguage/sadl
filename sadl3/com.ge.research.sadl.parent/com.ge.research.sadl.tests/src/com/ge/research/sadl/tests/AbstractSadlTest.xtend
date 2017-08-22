/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/
package com.ge.research.sadl.tests

import com.ge.research.sadl.external.ExternalEmfResource
import com.ge.research.sadl.processing.ISadlImplicitModelContentProvider
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.tests.helpers.XtendTemplateHelper
import com.google.common.base.Supplier
import com.google.common.base.Suppliers
import com.google.inject.Inject
import com.google.inject.Provider
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.resource.XtextResourceSet
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.util.ParseHelper
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.eclipse.xtext.util.StringInputStream
import org.junit.Before
import org.junit.runner.RunWith
import com.ge.research.sadl.processing.OntModelProvider
import com.ge.research.sadl.processing.SadlConstants
import com.ge.research.sadl.processing.ISadlBuiltinFunctionsModelContentProvider

/**
 * Base SADL test class.
 * 
 * <p>
 * Provides automatic implicit model creation into the resource set,
 * plus disables all model processors.
 * 
 * @author akos.kitta
 */
@RunWith(XtextRunner)
@InjectWith(SADLNoopModelProcessorsInjectorProvider)
abstract class AbstractSadlTest {
	
	@Inject protected extension ValidationTestHelper;

	@Inject protected ParseHelper<SadlModel> parseHelper;
	@Inject protected Provider<XtextResourceSet> resourceSetProvider;
	@Inject protected ISadlImplicitModelContentProvider modelContentProvider;
	@Inject protected ISadlBuiltinFunctionsModelContentProvider builtinModelContentProvider;
	
	@Accessors(PROTECTED_GETTER)
	XtextResourceSet currentResourceSet;
	
	private val Supplier<Void> implicitModelSupplier = Suppliers.memoize[
		val uri = URI.createURI(SadlConstants.SADL_IMPLICIT_MODEL_SYNTHETIC_URI);
		if (!currentResourceSet.resources.map[uri.lastSegment].exists[it == SadlConstants.SADL_IMPLICIT_MODEL_FILENAME]) {
			val resource = loadResource(modelContentProvider.content, uri);
			OntModelProvider.find(resource)
		}
		val bfuri = URI.createURI(SadlConstants.SADL_BUILTIN_FUNCTIONS_SYNTHETIC_URI);
//		if (!currentResourceSet.resources.map[bfuri.lastSegment].exists[it == SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME]) {
			val resource = loadResource(builtinModelContentProvider.content, bfuri);
			OntModelProvider.find(resource)
//		}
		
		return null;
	]
	
	@Before
	def void initialize() {
		currentResourceSet = resourceSetProvider.get
	}
	
	protected def XtextResource sadl(CharSequence seq) {
		return resource(seq, 'sadl') as XtextResource;
	}
	
	protected def ExternalEmfResource owl(CharSequence seq) {
		return resource(seq, 'owl') as ExternalEmfResource;
	}
	
	protected def ExternalEmfResource nt(CharSequence seq) {
		return resource(seq, 'nt') as ExternalEmfResource;
	}
	
	protected def ExternalEmfResource n3(CharSequence seq) {
		return resource(seq, 'n3') as ExternalEmfResource;
	}
	
	protected def Resource resource(CharSequence seq, String fileExtension) {
		val name = "Resource" + currentResourceSet.resources.size + "." + fileExtension;
		return resource(seq, URI.createURI("synthetic://test/" + name));
	}
	
	protected def Resource resource(CharSequence seq, URI uri) {
		// This will create one single implicit model instance into the resource set
		// per test method no matter how many times it is invoked.
		implicitModelSupplier.get;
		return loadResource(seq, uri);
	}
	
	private def Resource loadResource(CharSequence seq, URI uri) {
		val resource = currentResourceSet.createResource(uri);
		resource.load(new StringInputStream(XtendTemplateHelper.unifyEOL(seq)), null);
		return resource;
	}
	
}
