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
package com.ge.research.sadl.ui.contentassist

import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.ui.SadlImageRegistry
import com.ge.research.sadl.ui.SadlImageRegistry.Constants
import com.google.common.base.Function
import com.google.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.jface.text.contentassist.ICompletionProposal
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.ui.editor.contentassist.AbstractJavaBasedContentProposalProvider.ReferenceProposalCreator
import org.eclipse.xtext.ui.editor.contentassist.ConfigurableCompletionProposal

/**
 * SADL reference proposal creator to have nice icons.
 * 
 * @author akos.kitta
 */
@Singleton
class SadlReferenceProposalCreator extends ReferenceProposalCreator {

	@Override
	override protected getWrappedFactory(EObject model, EReference reference,
		Function<IEObjectDescription, ICompletionProposal> proposalFactory) {

		val factory = super.getWrappedFactory(model, reference, proposalFactory)
		return [
			val original = factory.apply(it);
			return original.adjust(it);
		];
	}

	private def ICompletionProposal adjust(ICompletionProposal proposal, IEObjectDescription desc) {
		if (proposal instanceof ConfigurableCompletionProposal) {
			val object = desc.EObjectOrProxy;
			if (object instanceof SadlResource) {
				if (object.eContainer instanceof SadlClassOrPropertyDeclaration) {
					proposal.image = SadlImageRegistry.getImage(Constants.CA_TYPE);
				}
			}
		}
		return proposal;
	}

}
