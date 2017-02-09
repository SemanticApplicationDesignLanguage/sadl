/**
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
 */
package com.ge.research.sadl.ide.editor.contentassist

import org.eclipse.xtext.ide.editor.contentassist.IdeCrossrefProposalProvider
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.CrossReference
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext

import static com.ge.research.sadl.sADL.SADLPackage.Literals.*

/**
 * IDE independent cross reference proposal provider for the {@code SADL} language.
 * 
 * @author akos.kitta 
 */
class SadlIdeCrossrefProposalProvider extends IdeCrossrefProposalProvider {

	@Override
	override protected createProposal(IEObjectDescription candidate, CrossReference crossRef,
		ContentAssistContext context) {

		// Need to escape the import with double quotes.
		if (SADL_MODEL == candidate.EClass && SADL_MODEL == crossRef?.type.classifier &&
			SADL_IMPORT == context?.currentModel.eClass) {
			
			val proposal = '''"«qualifiedNameConverter.toString(candidate.name)»"''';
			return proposalCreator.createProposal(proposal, context) [
				source = candidate
				description = candidate.getEClass?.name
			];

		} else {
			super.createProposal(candidate, crossRef, context)
		}
	}

}
