/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.external

import com.ge.research.sadl.model.OntConceptType
import com.ge.research.sadl.sADL.SADLFactory
import com.ge.research.sadl.sADL.SadlModel
import com.google.inject.Inject
import com.google.inject.Injector
import com.hp.hpl.jena.ontology.AnnotationProperty
import com.hp.hpl.jena.ontology.DatatypeProperty
import com.hp.hpl.jena.ontology.Individual
import com.hp.hpl.jena.ontology.ObjectProperty
import com.hp.hpl.jena.ontology.OntClass
import com.hp.hpl.jena.ontology.OntModel
import com.hp.hpl.jena.ontology.OntModelSpec
import com.hp.hpl.jena.ontology.OntResource
import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.util.iterator.ExtendedIterator
import com.hp.hpl.jena.vocabulary.OWL
import com.hp.hpl.jena.vocabulary.RDF
import com.hp.hpl.jena.vocabulary.RDFS
import java.io.IOException
import java.io.InputStream
import java.util.Collection
import java.util.Map
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.impl.ResourceFactoryImpl
import org.eclipse.emf.ecore.resource.impl.ResourceImpl
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtend.lib.annotations.Delegate
import org.eclipse.xtext.generator.GeneratorDelegate
import org.eclipse.xtext.resource.IResourceServiceProvider
import org.eclipse.xtext.util.internal.EmfAdaptable
import org.eclipse.xtext.validation.IResourceValidator

import static org.eclipse.emf.common.util.URI.createURI

class ExternalEmfResource extends ResourceImpl {

	val Map<String, SadlModel> modelMapping = newHashMap();

	@Accessors(PUBLIC_GETTER)
	OntModel jenaModel;

	override public void load(Map<?, ?> options) throws IOException {
		try {
			super.load(options);
		} catch (Exception t) {
			System.err.println("Error loading '" + this.uri.toString + "': " + t.message);
		}
	}

	override protected doLoad(InputStream inputStream, Map<?, ?> options) throws IOException {
		jenaModel = initOntModel(inputStream);
		jenaModel.listSubjects.filter[URIResource].map[it -> createURI(URI)].filter[key.localName == value.fragment].
			forEach [
				val rdfResource = key;
				val uri = value;
				val baseUri = uri.trimFragment.toString;
				val model = getOrCreateSadlModel(baseUri);
				model.elements += createSadlResource(rdfResource);
			];

		jenaModel.listOntologies.forEach [
			val baseUri = URI;
			listImports.forEach [
				val model = getOrCreateSadlModel(baseUri);
				model.imports += createSadlImport(it);
			];
		];
	}

	def createSadlImport(OntResource resource) {
		println("IMPORT? " + resource.URI)
		return SADLFactory.eINSTANCE.createSadlImport => [
			// TODO load SADL models?	
		];
	}

	override protected doUnload() {
		jenaModel = null;
		modelMapping.clear();
		super.doUnload()
	}

	private def getOrCreateSadlModel(String baseUri) {
		var model = modelMapping.get(baseUri);
		if (model === null) {
			model = SADLFactory.eINSTANCE.createSadlModel => [
				it.baseUri = baseUri;
			];
			modelMapping.put(baseUri, model);
			// Attach the SADL model to the contents list of the external resource.
			this.getContents.add(model);
		}
		return model;
	}

	private def createSadlResource(Resource rdfResource) {
		return SADLFactory.eINSTANCE.createSadlResource => [
			val conceptName = rdfResource.localName;
			val conceptUri = rdfResource.URI;
			val conceptType = rdfResource.ontConceptType;
			new ExternalResourceAdapter(conceptName, conceptUri, conceptType).attachToEmfObject(it);
			name = it;
		]
	}

	private def initOntModel(InputStream is) {
		val ontModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) as OntModel;
		ontModel.documentManager.processImports = false;
		ontModel.read(is, URI.toString, serializationLanguage);
		return ontModel;
	}

	private def String getSerializationLanguage() {
		return switch URI.fileExtension {
			case 'owl': 'RDF/XML'
			case 'nt': 'N-TRIPLE'
			case 'turtle': 'TURTLE'
			case 'n3': 'N3'
			default: 'RDF/XML'
		};
	}

	private def OntConceptType getOntConceptType(Resource r) {
		var ctype = OntConceptType.CLASS
		if (r !== null) {
			if (r instanceof Individual) {
				ctype = OntConceptType.INSTANCE
			} else if (r.canAs(DatatypeProperty)) {
				ctype = OntConceptType.DATATYPE_PROPERTY
			} else if (r.canAs(ObjectProperty)) {
				ctype = OntConceptType.RDF_PROPERTY
			} else if (r.canAs(OntClass)) {
				ctype = OntConceptType.CLASS
				var ExtendedIterator<Resource> itr = r.^as(OntClass).listRDFTypes(true)
				while (itr.hasNext()) {
					if (itr.next().equals(RDFS.Datatype)) {
						ctype = OntConceptType.DATATYPE
					}
				}
			} else if (r.canAs(AnnotationProperty)) {
				ctype = OntConceptType.ANNOTATION_PROPERTY
			} else if (r.getNameSpace().equals(OWL.NAMESPACE.getNameSpace()) ||
				r.getNameSpace().equals(RDFS.getURI())) {
				// this is an OWL or RDFS concept--requires special handling
				if (r.equals(RDFS.comment) || r.equals(RDFS.label) || r.equals(RDFS.seeAlso)) {
					ctype = OntConceptType.ANNOTATION_PROPERTY
				} else if (Character.isLowerCase(r.getLocalName().charAt(0))) {
					ctype = OntConceptType.CLASS_PROPERTY
				} else {
					ctype = OntConceptType.CLASS
				} // if (r.equals(OWL.allValuesFrom) || r.equals(OWL.))
			} else if (r.equals(RDFS.subClassOf)) {
				ctype = OntConceptType.CLASS_PROPERTY
			} else // TODO this doesn't work because of trying to actually get as OntCLass.
			// else if (r.equals(RDF.Property)) {
			// ctype = OntConceptType.ONTCLASS;
			// }
			if (r.canAs(Individual)) {
				ctype = OntConceptType.INSTANCE
			} else if (r.equals(RDF.type)) {
				ctype = OntConceptType.CLASS_PROPERTY
			}
		}
		return ctype

	}

}

class ExternalEmfResourceFactory extends ResourceFactoryImpl {

	public static val Collection<String> EXTERNAL_EXTENSIONS = #{'owl', 'nt', 'n3'};

	override createResource(URI uri) {
		return new ExternalEmfResource() => [
			URI = uri;
		];
	}

}

@EmfAdaptable
@Data
class ExternalResourceAdapter {

	String concreteName;
	String conceptUri;
	OntConceptType type;

}

class ExternalEmfResourceServiceProvider implements IResourceServiceProvider {

	@Delegate
	@Inject
	IResourceServiceProvider delegate

	@Inject
	Injector injector;

	override canHandle(URI uri) {
		val name = uri.lastSegment;
		// exclude:
		// 1) SadlBaseModel.owl
		// 2) SadlListModel.owl
		if (name.endsWith("SadlBaseModel.owl") || name.endsWith("SadlListModel.owl")) {
			return false;
		}
		return ExternalEmfResourceFactory.EXTERNAL_EXTENSIONS.contains(uri.fileExtension);
	}

	override getResourceValidator() {
		return IResourceValidator.NULL;
	}

	override <T> get(Class<T> clazz) {
		if (GeneratorDelegate === clazz) {
			return injector.getInstance(ExternalEmfResourceGenerator) as T;
		}
		return delegate.get(clazz);
	}

}
