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

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory
import com.ge.research.sadl.builder.IConfigurationManagerForIDE
import com.ge.research.sadl.model.OntConceptType
import com.ge.research.sadl.reasoner.ConfigurationManager
import com.ge.research.sadl.reasoner.utils.SadlUtils
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
import com.hp.hpl.jena.rdf.model.Property
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.util.iterator.ExtendedIterator
import com.hp.hpl.jena.vocabulary.OWL
import com.hp.hpl.jena.vocabulary.RDF
import com.hp.hpl.jena.vocabulary.RDFS
import java.io.File
import java.io.IOException
import java.io.InputStream
import java.util.Collection
import java.util.Map
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.Path
import static org.eclipse.emf.common.util.URI.createPlatformResourceURI
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
import org.eclipse.emf.common.EMFPlugin
import java.nio.file.Paths

class ExternalEmfResource extends ResourceImpl {

	val Map<String, SadlModel> modelMapping = newHashMap();

	@Accessors(PUBLIC_GETTER)
	OntModel jenaModel;
	
	IConfigurationManagerForIDE configMgr
	public static final String OWL_MODELS_FOLDER_NAME = "OwlModels";

	override public void load(Map<?, ?> options) throws IOException {
		try {
			super.load(options);
		} catch (Exception t) {
			System.err.println("Error loading '" + this.uri.toString + "': " + t.message);
		}
	}

	override protected doLoad(InputStream inputStream, Map<?, ?> options) throws IOException {
		jenaModel = initOntModel(inputStream);
		val buri = getJenaModelBaseUri(jenaModel)
		jenaModel.listSubjects.filter[URIResource].map[it -> createURI(URI)].filter[key.localName == value.fragment].
			forEach [
				val rdfResource = key;
//				val uri = value;
//				val baseUri = uri.trimFragment.toString;
				var altBaseUri = buri
				if (buri != null && buri.endsWith("#")) {
					altBaseUri = buri.substring(0, buri.length() - 1);
				}	
				val model = getOrCreateSadlModel(altBaseUri, options);
				model.elements += createSadlResource(rdfResource);
			];

		jenaModel.listOntologies.forEach [
			val baseUri = URI;
			listImports.forEach [
				val resource = it
				if (resource instanceof Resource) {
					if ((resource as Resource).URIResource) {
						val importUri = resource.URI
						System.out.println("Importing " + importUri)
					}
				}
				val model = getOrCreateSadlModel(baseUri, options);
				model.imports += createSadlImport(it);
			];
		];
	}
	
	def getJenaModelBaseUri(OntModel m) {
		return m.getNsPrefixURI("")
	}

	def createSadlImport(OntResource resource) {
		println("IMPORT? " + resource.URI)
		return SADLFactory.eINSTANCE.createSadlImport => [
			// TODO load SADL models?	
			if (configMgr.containsMappingForURI(resource.URI)) {
				val altUrl = configMgr.getAltUrlFromPublicUri(resource.URI)	// this is the actual location of the imported OWL file on disk
				var prefix = jenaModel.getNsURIPrefix(resource.URI)
				if (prefix === null) {
					prefix = jenaModel.getNsURIPrefix(resource.URI + "#")
				}
				it.alias = prefix
				// load resource
				// convert file URI to platform URI
				if (EMFPlugin.IS_ECLIPSE_RUNNING) {
					val rfile = new File(configMgr.fileUrlToFileName(altUrl))
					val path = Paths.get(rfile.toURI)
					val relpath = Paths.get(ResourcesPlugin.workspace.root.locationURI).relativize(path)
					val importUri = createPlatformResourceURI(relpath.toString, true)
					val importResource = resourceSet.getResource(importUri, false)
					if (importResource != null) {
						it.importedResource = importResource.contents.head as SadlModel
					}
					else {
						// TODO add error marker to this ExternalEmfResource
						// also must clear error markers before processing
						val msg = "Import '" + resource.URI + "' not found."
						println(msg)
					}
				}
				else {
					val importUri = createURI(altUrl)
					val importResource = resourceSet.getResource(importUri, true)
					it.importedResource = importResource.contents.head as SadlModel
				}
			}
			else {
				// TODO add error marker to this ExternalEmfResource
				// also must clear error markers before processing
				val msg = "Import '" + resource.URI + "' not found."
				println(msg)
			}
			println(it)		
		];
	}

	override protected doUnload() {
		jenaModel = null;
		modelMapping.clear();
		super.doUnload()
	}

	private def getOrCreateSadlModel(String baseUri, Map<?, ?> options) {
		var model = modelMapping.get(baseUri);
		if (model === null) {
			model = SADLFactory.eINSTANCE.createSadlModel => [
				it.baseUri = baseUri;
				// is this a good place to add the mapping for the external resource to the policy file?
//  need to convert platform URI to a valid file URI--the ResourcesPlugin.getWorkspace doesn't work in tests and is Eclipse-specific
				val file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(URI.toPlatformString(true)));
				val filename = file.getRawLocation().toOSString();
				System.out.println("URL= " + filename + ", URI=" + baseUri)
				if (configMgr === null) {
					val modelFolderPath = getModelFolderPath(URI, options)
					var modelFolderPathname = toFile(modelFolderPath)
					var	format = ConfigurationManager.RDF_XML_ABBREV_FORMAT; // default
					if (isSyntheticUri(modelFolderPathname.toString, URI)) {
						modelFolderPathname = null;
						configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderPathname, format, true);
					}
					else {
						configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderPathname , format);
					}
				}
				if (filename !== null && baseUri !== null) {
					configMgr.addMapping(new SadlUtils().fileNameToFileUrl(filename), baseUri, null, true, "ExternalResource")					
				}
			];
			modelMapping.put(baseUri, model);
			// Attach the SADL model to the contents list of the external resource.
// this adds models other than for the current resource to the current resource? 
			this.getContents.add(model);
		}
		return model;
	}

	def toFile(URI uri) {
        if (uri.isFile()) {
            return uri.toFileString();
        } else if (uri.isPlatform()) {
			val workspace = ResourcesPlugin.getWorkspace();
			val root = workspace.getRoot();
			val path = root.getFile(new Path(uri.toPlatformString(true))).getLocation();
			val absolutePath = path.toString();
			return absolutePath;
//	}
//
//        	 IWorkspaceRoot.getProject(uri.segment(0))
//        	 getFile(new Path(uri.toPlatformString())).getLocation() 
//            return ResourcesPlugin.getWorkspace().getRoot().getLocation().append(uri.toPlatformString(true)).toOSString();
        }
    }
 
	private def isSyntheticUri(String modelFolderPathname, URI uri) {
		if ((modelFolderPathname === null && 
				uri.toString().startsWith("synthetic")) ||
						uri.toString().startsWith("__synthetic")) {
			return true;
		}
		return false;
	}
	
	private def getModelFolderPath(URI resourceUri, Map<?, ?> options) {
		var segCnt = resourceUri.segmentCount
		for (var dropCnt = 1; dropCnt < segCnt; dropCnt++) {
			val shortenedUri = resourceUri.trimSegments(dropCnt)
			val proposedModelFolderUri = shortenedUri.appendSegment(OWL_MODELS_FOLDER_NAME)
			if (getResourceSet.URIConverter.exists(proposedModelFolderUri, options)) {
				return getResourceSet.URIConverter.normalize(proposedModelFolderUri)
			}
		}
//		var segOff = 0
//		if (resourceUri.isFile()) 
//			segOff = 1
//		else 
//			segOff = 2
//		val modelFolderUri = resourceUri.trimSegments(segOff).appendSegment(OWL_MODELS_FOLDER_NAME)
//		
//		if (resourceUri.isPlatformResource()) {
//			 val file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(modelFolderUri.toPlatformString(true)))
//			 return file.getRawLocation().toPortableString()
//		} else {
//			val modelFolderPathname = findModelFolderPath(resourceUri)
//			if (modelFolderPathname === null) {
//				return modelFolderUri.toFileString()
//			}
//			else {
//				return modelFolderPathname
//			}
//		}
		return null
	}
	
   def String findModelFolderPath(URI uri){
    	var file = new File(uri.path())
    	if(file !== null){
    		if(file.isDirectory()){
    			if(file.getAbsolutePath().endsWith(OWL_MODELS_FOLDER_NAME)){
    				return file.getAbsolutePath()
    			}
    			
    			for(File child : file.listFiles()){
    				if(child.getAbsolutePath().endsWith(OWL_MODELS_FOLDER_NAME)){
    					return child.getAbsolutePath()
    				}
    			}
    			//Didn't find a project file in this directory, check parent
    			if(file.getParentFile() !== null){
    				return findModelFolderPath(uri.trimSegments(1))
    			}
    		}
    		if(file.isFile() && file.getParentFile() !== null){
    			return findModelFolderPath(uri.trimSegments(1))
    		}
    	}
    	
    	return null;
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
//		ontModel.documentManager.processImports = false;
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
				ctype = OntConceptType.CLASS_PROPERTY
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
			} else if (r.canAs(Individual)) {
				ctype = OntConceptType.INSTANCE
			} else if (r.canAs(Property)) {
				ctype = OntConceptType.RDF_PROPERTY
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
