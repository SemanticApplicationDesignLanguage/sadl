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
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration;
import com.ge.research.sadl.sADL.SadlImport;
import com.ge.research.sadl.sADL.SadlModel;
import com.ge.research.sadl.sADL.SadlModelElement;
import com.hp.hpl.jena.ontology.OntModel;

public class JenaBasedSadlModelProcessor implements ISadlModelProcessor {

	private OntModel theJenaModel;
	
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
		// directly create the Jena Model here!
		theJenaModel = null; // create fresh OntModel
		
//		theJenaModel.setModelBaseURI(model.getBaseUri());
//		theJenaModel.setModelAlias(model.getAlias());
//		
//		theJenaModel.setModelActualURL(resource.getURI().toFileString());
		
		EList<SadlImport> implist = model.getImports();
		Iterator<SadlImport> impitr = implist.iterator();
		while (impitr.hasNext()) {
			SadlImport simport = impitr.next();
//			theJenaModel.addImport(simport.getImportURI(), simport.getAlias());
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
					
				}
			}
		}
	}
	
	@Override
	public void onGenerate(Resource resource, IFileSystemAccess2 fsa, CancelIndicator cancelIndicator) {
		
	}
	
}
