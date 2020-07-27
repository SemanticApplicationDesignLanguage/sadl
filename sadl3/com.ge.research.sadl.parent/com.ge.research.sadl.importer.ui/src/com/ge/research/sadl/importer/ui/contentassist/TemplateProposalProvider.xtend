/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.importer.ui.contentassist

import com.ge.research.sadl.importer.template.TemplateModel
import java.util.Collection
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.CrossReference
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.RuleCall
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor

/**
 * See https://www.eclipse.org/Xtext/documentation/304_ide_concepts.html#content-assist
 * on how to customize the content assistant.
 */
class TemplateProposalProvider extends AbstractTemplateProposalProvider {
	
	protected static val SUPPORTED_FILE_EXTENSION = #{'sadl', 'n3', 'owl', 'ntriple', 'nt'};
	protected static val BUILTIN_FILES = #{'SadlImplicitModel.sadl', 'SadlBuiltinFunctions.sadl'};
	
	
	protected def Collection<String> getBuiltinFiles() {
		return BUILTIN_FILES;
	}

	protected def Collection<String> getSupporedFileExtensions() {
		return SUPPORTED_FILE_EXTENSION;
	}

	protected def boolean canBeImported(IEObjectDescription it, Collection<String> alreadyImportedFiles) {
		val fileExtension = EObjectURI?.fileExtension;
		val fileName = EObjectURI?.lastSegment;
		return supporedFileExtensions.contains(fileExtension) && !builtinFiles.contains(fileName) &&
			!alreadyImportedFiles.contains(name?.toString);
	}

	override void completeImport_ImportResource(EObject model, Assignment assignment,
		ContentAssistContext context, ICompletionProposalAcceptor acceptor) {

		val term = assignment.terminal;
		val templateModel = EcoreUtil2.getContainerOfType(model, TemplateModel);
		if (templateModel !== null) {
			val imports = templateModel.imports.map[importResource].filterNull.map[baseUri].filterNull.toSet;
			lookupCrossReference(term as CrossReference, context, acceptor) [
				return canBeImported(it, imports);
			];
		}
	}

	// Creates a proposal for an EOS terminal.  Xtext can't guess (at
    // the moment) what the valid values for a terminal rule are, so
    // that's why there is no automatic content assist for EOS.
	override void complete_EOL(EObject model, RuleCall ruleCall, 
	        ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		var proposalText = ".\n";
		var displayText = ". - End of Sentence";
		var image = getImage(model);
		var proposal = createCompletionProposal(proposalText, displayText, image, context);
		acceptor.accept(proposal);
	}

}
