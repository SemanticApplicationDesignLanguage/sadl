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
package com.ge.research.sadl.model

import com.ge.research.sadl.sADL.Name
import com.google.inject.ImplementedBy
import com.google.inject.Singleton
import org.eclipse.emf.ecore.EObject
import com.ge.research.sadl.sADL.SubjHasProp
import com.ge.research.sadl.sADL.Declaration

/**
 * Contribution for the {@link DeclarationExtensions declaration extensions}.
 * 
 * <p>
 * Clients extending the SADL grammar may contribute customized logic to the the declaration extensions
 * by implementing this contribution interface and configuring the binding in the runtime module of the downstream
 * grammar.
 * 
 * <p>
 * In the downstream grammar (MyGrammar), one should do the followings:
 * 
 * <ul>
 * <li>Create a {@code MyGrammarDeclarationExtensionsContribution} class.</li>
 * <li>Implement the desired custom contribution by implementing all required {@code default} methods of the interface.</li>
 * <li>Enable the binding in the {@code MyGrammarRuntimeModule} by adding the following (Xtend) method:</li>
 * <pre>
 *  def Class&lt;? extends IDeclarationExtensionsContribution&gt; bindIDeclarationExtensionsContribution() {
 *    return MyGrammarIDeclarationExtensionsContribution;	
 *  }
 * </pre>
 * </ul>
 * 
 * @author akos.kitta
 */
@ImplementedBy(IDeclarationExtensionsContribution.Default)
interface IDeclarationExtensionsContribution {

	/**
	 * Returns with the ontology concept type for the container of the {@link Name named element}'s container.
	 * <p>
	 * By default, returns with {@code null}.
	 */
	def OntConceptType getOntConceptType(EObject namedElementContainer) {
		if (namedElementContainer instanceof SubjHasProp &&
			(namedElementContainer as SubjHasProp).right === null &&
			(namedElementContainer as SubjHasProp).left instanceof Declaration) {
			return OntConceptType.VARIABLE;
		}
		return null;
	}

	/**
	 * Default implementation that assumes no contribution. 
	 */
	@Singleton
	static class Default implements IDeclarationExtensionsContribution {
	}

}
