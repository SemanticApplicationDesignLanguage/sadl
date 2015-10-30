package com.ge.research.sadl.jena;

import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.mwe.core.issues.IssuesImpl;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.validation.Issue.IssueImpl;

import com.ge.research.sadl.processing.ISadlModelProcessor;
import com.ge.research.sadl.sADL.SadlAnnotation;
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration;
import com.ge.research.sadl.sADL.SadlImport;
import com.ge.research.sadl.sADL.SadlModel;
import com.ge.research.sadl.sADL.SadlModelElement;
import com.ge.research.sadl.sADL.SadlResource;
import com.ge.research.sadl.sADL.SadlTypeReference;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.rdf.model.ModelFactory;

public class JenaBasedSadlModelProcessor implements ISadlModelProcessor {

	private OntModel theJenaModel;
	
	enum AnnType {ALIAS, NOTE}
	
	/**
	 * For TESTING
	 * @return
	 */
	public OntModel getTheJenaModel() {
		return theJenaModel;
	}
	
	@Override
	public void onValidate(Resource resource, IAcceptor<Issue> issueAcceptor, CancelIndicator cancelIndicator) {
		SadlModel model = (SadlModel) resource.getContents().get(0);
		String modelActualUrl =resource.getURI().toFileString();
		// directly create the Jena Model here!
		theJenaModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		String modelName = model.getBaseUri();
		String modelNamespace = assureNamespaceEndsWithHash(modelName);
		String modelAlias = model.getAlias();
		if (modelAlias == null) {
			modelAlias = "";
		}
		getTheJenaModel().setNsPrefix(modelAlias, modelNamespace);
		Ontology modelOntology = getTheJenaModel().createOntology(modelName);
		modelOntology.addComment("This ontology was created from a SADL file '"
				+ modelActualUrl + "' and should not be directly edited.", "en");
		
		String modelVersion = model.getVersion();
		if (modelVersion != null) {
			modelOntology.addVersionInfo(modelVersion);
		}

		EList<SadlAnnotation> anns = model.getAnnotations();
		Iterator<SadlAnnotation> iter = anns.iterator();
		while (iter.hasNext()) {
			SadlAnnotation ann = iter.next();
			String anntype = ann.getType();
			EList<String> annContents = ann.getContents();
			Iterator<String> anniter = annContents.iterator();
			while (anniter.hasNext()) {
				String annContent = anniter.next();
				if (anntype.equals(AnnType.ALIAS)) {
					modelOntology.addLabel(annContent, "en");
				}
				else if (anntype.equals(AnnType.NOTE)) {
					modelOntology.addComment(annContent, "en");
				}
			}
		}
		
		EList<SadlImport> implist = model.getImports();
		Iterator<SadlImport> impitr = implist.iterator();
		while (impitr.hasNext()) {
			SadlImport simport = impitr.next();
			String importUri = simport.getImportURI();
			String importPrefix = simport.getAlias();
//			theJenaModel.addImport(simport.getImportURI(), simport.getAlias());
	    	if (importUri.equals(modelName)) {
	    		// don't import to self
//	    		generate error marker--can't import self
	    	}
	    	else {
		    	// Now load import model (with setCachedModels true so it loads any indirect imports)
		       	// and add all import OntModels to importing mappings
	    		if (importPrefix == null) {
// TODO	    			// need to get the prefix from the global prefix in the imported model, if SADL, else from the ont-policy.rdf file if external
	    		}
	    		if (importPrefix != null) {
	    			getTheJenaModel().setNsPrefix(importPrefix, assureNamespaceEndsWithHash(importUri));
	    		}
		    	com.hp.hpl.jena.rdf.model.Resource importedOntology = getTheJenaModel().createResource(importUri);
		    	modelOntology.addImport(importedOntology);
	    	}
	    	
// TODO	Should the imported model actually be loaded by Jena? The OWL model, whether from SADL or external, will potenatially 
//	    	contain information that is necessary for validation. The only information that will be potentially needed for
//	    	processing the rest of the parse tree is the type of the imported concepts. For imports of SADL models, this type
//	    	information is known in the ResourceSet. Likewise for external OWL imports? 
	    	
//	    	this.getJenaDocumentMgr().setCacheModels(true);
//	   		this.getJenaDocumentMgr().setProcessImports(true);
//	   		ReadFailureHandler rfh = this.getJenaDocumentMgr().getReadFailureHandler();
//	   		if (rfh instanceof SadlReadFailureHandler) {
//	   			((SadlReadFailureHandler)rfh).setSadlConfigMgr(this);
//	   		}
		}
		
		// process rest of parse tree
		List<SadlModelElement> elements = model.getElements();
		if (elements != null) {
			Iterator<SadlModelElement> elitr = elements.iterator();
			while (elitr.hasNext()) {
				// check for cancelation from time to time
				if (cancelIndicator.isCanceled()) {
					throw new OperationCanceledException();
				}
				SadlModelElement element = elitr.next();
				if (element instanceof SadlClassOrPropertyDeclaration) {
					SadlTypeReference superElement = ((SadlClassOrPropertyDeclaration)element).getSuperElement();
					EList<SadlResource> clses = ((SadlClassOrPropertyDeclaration)element).getClassOrProperty();
					if (clses != null) {
						Iterator<SadlResource> citer = clses.iterator();
						while (citer.hasNext()) {
							SadlResource sr = citer.next();
// TODO How do I get the actual name? getName() on SadlResource returns a SadlResource, etc.							
							String nm = sr.getName().getName().toString();
							getTheJenaModel().createClass(getUri(modelNamespace, nm));
						}
					}
				}
			}
		}
	}
	
	@Override
	public void onGenerate(Resource resource, IFileSystemAccess2 fsa, CancelIndicator cancelIndicator) {
		
	}
	
	private String assureNamespaceEndsWithHash(String name) {
		name = name.trim();
		if (!name.endsWith("#")) {
			return name + "#";
		}
		return name;
	}

	private String getUri(String modelNamespace, String nm) {
		modelNamespace = assureNamespaceEndsWithHash(modelNamespace);
		return modelNamespace + nm;
	}

}
