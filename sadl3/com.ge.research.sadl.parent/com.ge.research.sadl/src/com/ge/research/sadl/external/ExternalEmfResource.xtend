package com.ge.research.sadl.external

import com.ge.research.sadl.model.OntConceptType
import com.ge.research.sadl.sADL.SADLFactory
import com.google.inject.Inject
import com.google.inject.Injector
import com.hp.hpl.jena.ontology.AnnotationProperty
import com.hp.hpl.jena.ontology.DatatypeProperty
import com.hp.hpl.jena.ontology.Individual
import com.hp.hpl.jena.ontology.ObjectProperty
import com.hp.hpl.jena.ontology.OntClass
import com.hp.hpl.jena.ontology.OntModel
import com.hp.hpl.jena.ontology.OntModelSpec
import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.util.iterator.ExtendedIterator
import com.hp.hpl.jena.vocabulary.OWL
import com.hp.hpl.jena.vocabulary.RDF
import com.hp.hpl.jena.vocabulary.RDFS
import java.io.IOException
import java.io.InputStream
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

class ExternalEmfResource extends ResourceImpl {
	@Accessors OntModel jenaModel
	
	override public void load(Map<?, ?> options) throws IOException {
		try {
			super.load(options)
		}
		catch (Throwable t) {
			System.err.println("Error loading '" + this.uri.toString + "': " + t.message)
		}
    }

	override protected doLoad(InputStream inputStream, Map<?, ?> options) throws IOException {
//		val om = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) as OntModel
//		om.documentManager.processImports = false
//		om.read(inputStream, URI.toString, getLang())
//		val clsitr = om.listNamedClasses
//		while (clsitr.hasNext) {
//			val cls = clsitr.next as OntClass
//			// TODO add CLASS to index
//		}
//		val dpitr = om.listDatatypeProperties
//		while (dpitr.hasNext) {
//			val dp = dpitr.next as DatatypeProperty
//			// TODO add DATATYPE_PROPERTY to index
//		}
//		val opitr = om.listObjectProperties
//		while (opitr.hasNext) {
//			val op = opitr.next as ObjectProperty
//			// TODO add CLASS_PROPERTY to index
//		}
//		val institr = om.listIndividuals
//		while (institr.hasNext) {
//			val inst = institr.next as Individual
//			if (inst.URIResource) {
//				// TODO add INSTANCE to index
//			}
//		}
//		val annitr = om.listAnnotationProperties
//		while (annitr.hasNext) {
//			val ann = annitr.next as AnnotationProperty
//			// TODO add ANNOTATION_PROPERTY to index
//		}
//		val sitr = om.listStatements(null, RDF.type, RDF.Property)
//		while (sitr.hasNext) {
//			val rdfProp = sitr.nextStatement.subject as Resource
//			// TODO add RDF_PROPERTY to index
//		}
//		jenaModel = ModelFactory.createDefaultModel.read(inputStream, URI.toString, getLang())
		jenaModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) as OntModel
		jenaModel.documentManager.processImports = false
		jenaModel.read(inputStream, URI.toString, getLang())
		val rootMap = newHashMap()
		val iterator = jenaModel.listSubjects
		while (iterator.hasNext) {
			val rdfRes = iterator.nextResource
			if (rdfRes.isURIResource) {
				val uri = URI::createURI(rdfRes.URI)
				if (uri.fragment == rdfRes.localName) {
					val ns = uri.trimFragment.toString
					val rootModel = if (rootMap.containsKey(ns)) {
							rootMap.get(ns)
						} else {
							SADLFactory.eINSTANCE.createSadlModel => [
								baseUri = ns
								rootMap.put(ns, it)
								this.getContents += it
							]
						}
						rootModel.elements += SADLFactory.eINSTANCE.createSadlResource => [
						new ExternalResourceAdapter(rdfRes.localName, rdfRes.URI, getType(rdfRes)).attachToEmfObject(it)
						name = it;
					]
				}
//				else {
//					val importedResource = rdfRes.getPropertyResourceValue(OWL.imports)
//					System.out.println(importedResource)
//				}
			}
		}
	}
	
	def String getLang() {
		switch URI.fileExtension {
			case 'owl' : "RDF/XML"
			case 'nt'  :"N-TRIPLE"
			case 'turtle': "TURTLE"
			case 'n3' : "N3"
			default: 'RDF/XML'
		}
	}

	override protected doUnload() {
		jenaModel = null
		super.doUnload()
	}

	protected def OntConceptType getType(Resource r) {
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
	public static val String[] EXTERNAL_EXTENSIONS = #['owl','nt','n3']

	override createResource(URI uri) {
		val result = new ExternalEmfResource()
		result.URI = uri
		return result
	}

}

@EmfAdaptable @Data class ExternalResourceAdapter {
	String concreteName
	String conceptUri
	OntConceptType type
}

class ExternalEmfResourceServiceProvider implements IResourceServiceProvider {

	@Inject @Delegate IResourceServiceProvider delegate

	@Inject
	Injector injector;
	
	override canHandle(URI uri) {
		// exclude:
		// 1) SadlBaseModel.owl
		// 2) SadlListModel.owl
		// 3) any file ending in ".metrics.owl"
		val name = uri.lastSegment
		if (name.endsWith(".metrics.owl") || name.endsWith("SadlBaseModel.owl") || name.endsWith("SadlListModel.owl")) {
			return false;
		}
		return ExternalEmfResourceFactory.EXTERNAL_EXTENSIONS.contains(uri.fileExtension)
	}

	override getResourceValidator() {
		return IResourceValidator.NULL
	}
	
	override <T> get(Class<T> clazz) {
		if (GeneratorDelegate === clazz) {
			return injector.getInstance(ExternalEmfResourceGenerator) as T;
		}
		return delegate.get(clazz);
	}

}
