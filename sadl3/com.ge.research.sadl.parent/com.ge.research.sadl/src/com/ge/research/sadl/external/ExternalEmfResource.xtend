package com.ge.research.sadl.external

import com.ge.research.sadl.model.OntConceptType
import com.ge.research.sadl.sADL.SADLFactory
import com.google.inject.Inject
import com.hp.hpl.jena.ontology.AnnotationProperty
import com.hp.hpl.jena.ontology.DatatypeProperty
import com.hp.hpl.jena.ontology.Individual
import com.hp.hpl.jena.ontology.ObjectProperty
import com.hp.hpl.jena.ontology.OntClass
import com.hp.hpl.jena.rdf.model.Model
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
import org.eclipse.xtext.resource.IResourceServiceProvider
import org.eclipse.xtext.util.internal.EmfAdaptable
import org.eclipse.xtext.validation.IResourceValidator

class ExternalEmfResource extends ResourceImpl {
	@Accessors Model jenaModel

	override protected doLoad(InputStream inputStream, Map<?, ?> options) throws IOException {
		jenaModel = ModelFactory.createDefaultModel.read(inputStream, URI.toString, getLang())
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
					]
				}
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

	override canHandle(URI uri) {
		return ExternalEmfResourceFactory.EXTERNAL_EXTENSIONS.contains(uri.fileExtension)
	}

	override getResourceValidator() {
		return IResourceValidator.NULL
	}

}
