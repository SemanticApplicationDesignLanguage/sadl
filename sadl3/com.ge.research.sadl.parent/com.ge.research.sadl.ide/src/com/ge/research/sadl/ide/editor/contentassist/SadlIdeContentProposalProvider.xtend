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
package com.ge.research.sadl.ide.editor.contentassist

import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.services.SADLGrammarAccess
import com.google.inject.Inject
import com.google.inject.Singleton
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.CrossReference
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.RuleCall
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ide.editor.contentassist.IIdeContentProposalAcceptor
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalCreator
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalPriorities
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalProvider

import static com.ge.research.sadl.processing.SadlConstants.SADL_IMPLICIT_MODEL_FILENAME

/**
 * Generic content proposal provider for the {@code SADL} language.
 * 
 * <p>
 * This content proposal provider is intended to support both the web based and the Eclipse based editor.
 * 
 * @author akos.kitta
 */
@Singleton
class SadlIdeContentProposalProvider extends IdeContentProposalProvider {

	/** A set of all file extensions that can be imported into the resource. */
	static val KNOWN_FILE_EXTENSION = #{'sadl', 'owl', 'n3', 'ntriple', 'nt'};

	@Inject 
	SADLGrammarAccess grammarAccess;
	
	@Inject
	IdeContentProposalCreator proposalCreator;
	
	@Inject
	IdeContentProposalPriorities proposalPriorities;
	
	@Override
	override protected _createProposals(RuleCall ruleCall, ContentAssistContext ctx, IIdeContentProposalAcceptor acceptor) {
		val rule = ruleCall.rule;
		switch (rule) {
			case grammarAccess.EOSRule: {
				ctx.completeEOS(acceptor);
			}
			default: {
				super._createProposals(ruleCall, ctx, acceptor);
			}
		}
	}
	
	@Override
	override protected _createProposals(Assignment assignment, ContentAssistContext ctx, IIdeContentProposalAcceptor acceptor) {
		switch (assignment) {
			case grammarAccess.sadlModelAccess.getBaseUriAssignment_1: {
				ctx.completeBaseUri(acceptor);
			}
			case grammarAccess.sadlModelAccess.aliasAssignment_2_1: {
				ctx.completeAlias(acceptor);
			}
			case grammarAccess.sadlImportAccess.importedResourceAssignment_1: {
				ctx.completeImports(acceptor);
			}
			default: {
				super._createProposals(assignment, ctx, acceptor);
			}
		}
	}
	
	@Override
	override protected getCrossrefFilter(CrossReference reference, ContentAssistContext ctx) {
		// Special case for filtering out all those resources among import proposals which are already imported.
		if (reference.eContainer == grammarAccess.sadlImportAccess.importedResourceAssignment_1) {
			val model = EcoreUtil2.getContainerOfType(ctx.currentModel, SadlModel);
			val imports = model.imports.map[importedResource].filterNull.map[baseUri].toSet;
			return [
				return SADL_IMPLICIT_MODEL_FILENAME != EObjectURI?.lastSegment
					&& KNOWN_FILE_EXTENSION.contains(EObjectURI?.fileExtension)
					&& !imports.contains(name.toString);
			];
		}
		super.getCrossrefFilter(reference, ctx);
	}

	private def completeEOS(ContentAssistContext ctx, IIdeContentProposalAcceptor it) {
		val proposalText = ".\n";
		val proposal = proposalCreator.createProposal(proposalText, ctx, [
			description = '. - End of Sentence';
		]);
		val priority = proposalPriorities.getDefaultPriority(proposal);
		accept(proposal, priority);
	}
	
	private def completeBaseUri(ContentAssistContext ctx, IIdeContentProposalAcceptor it) {
		val proposalText = '''"http://sadl.org/«ctx.resource.URI.lastSegment»"''';
		val proposal = proposalCreator.createProposal(proposalText, ctx);
		val priority = proposalPriorities.getDefaultPriority(proposal);
		accept(proposal, priority);
	}
	
	private def completeAlias(ContentAssistContext ctx, IIdeContentProposalAcceptor it) {
		val proposalText = ctx.resource.URI.trimFileExtension.lastSegment;
		val proposal = proposalCreator.createProposal(proposalText, ctx);
		val priority = proposalPriorities.getDefaultPriority(proposal);
		accept(proposal, priority);
	}

	private def completeImports(ContentAssistContext ctx, IIdeContentProposalAcceptor it) {
		val crossRef = grammarAccess.sadlImportAccess.importedResourceAssignment_1.terminal as CrossReference;
		_createProposals(crossRef, ctx, it);
	}
	
}