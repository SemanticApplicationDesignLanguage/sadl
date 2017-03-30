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
package com.ge.research.sadl.processing

import com.ge.research.sadl.sADL.SADLFactory
import com.ge.research.sadl.sADL.SadlResource
import com.google.common.base.Optional
import com.google.common.base.Preconditions
import com.google.inject.ImplementedBy
import com.hp.hpl.jena.ontology.OntModel
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtend.lib.annotations.Data

/**
 * Reasoner independent implementation of an ontology helper for SADL. 
 */
@ImplementedBy(SadlOntologyHelper)
interface ISadlOntologyHelper {

	/**
	 * Encapsulates the context for the ontology helper.
	 */
	static interface Context {

		/**
		 * Returns with the ontology model.
		 */
		def OntModel getOntModel();

		/**
		 * Returns with the subject SADL resource.
		 */
		def SadlResource getSubject();

		/**
		 * Returns with the validation acceptor.
		 */
		def ValidationAcceptor getAcceptor();

		/**
		 * Returns with the grammar context to be able to make additional restrictions.
		 * This context ID is the concatenation of the rule name of the containing parser
		 * rule for an assignment and the name of the feature for the assignment.
		 * When this grammar context ID is missing, then the grammar context is unknown.
		 * 
		 * <p>
		 * Grammar context ID pattern. <code>${CONTAINING_PARSER_RULE_NAME}_${FEATURE_NAME}</code>
		 */
		def Optional<String> getGrammarContextId();

		/**
		 * Returns with the restriction for the context.
		 */
		def Optional<SadlResource> getRestriction();

	}

	/**
	 * Contains a couple of grammar context ID for SADL.
	 */
	static abstract class GrammarContextIds {

		/**
		 * {@code C is a class. myC is a <|>}
		 */
		public static val SADLPRIMARYTYPEREFERENCE_TYPE = 'SADLPRIMARYTYPEREFERENCE_TYPE';

		/**
		 * {@code C is a class. myC is a <|>}
		 */
		public static val SADLPRIMARYTYPEREFERENCE_PRIMITIVETYPE = 'SADLPRIMARYTYPEREFERENCE_PRIMITIVETYPE';

		public static val SADLPROPERTYINITIALIZER_PROPERTY = 'SADLPROPERTYINITIALIZER_PROPERTY';

		public static val SADLSTATEMENT_SUPERELEMENT = 'SADLSTATEMENT_SUPERELEMENT';

		public static val PRIMARYEXPRESSION_VALUE = 'PRIMARYEXPRESSION_VALUE';

		public static val SADLPROPERTYCONDITION_PROPERTY = 'SADLPROPERTYCONDITION_PROPERTY';

		public static val SADLRESOURCE_NAME = 'SADLRESOURCE_NAME';

		/**
		 * {@code C is a class described by p1 with values of type C. myC is a C with p1 <|>}
		 */
		public static val SADLPROPERTYINITIALIZER_VALUE = 'SADLPROPERTYINITIALIZER_VALUE';

		private new() {
			/* NOOP */
		}

	}

	/**
	 * Sugar for building the context for the ontology helper.
	 */
	static class ContextBuilder {

		public static val MISSING_SUBJECT = SADLFactory.eINSTANCE.createSadlResource;

		var SadlResource subject;

		var OntModel ontModel;
		var ValidationAcceptor acceptor;
		var Optional<String> grammarContextId;
		var Optional<SadlResource> restriction;

		/**
		 * Returns with a new context builder that has no subject SADL resource.
		 */
		static def createWithoutSubject(OntModel ontModel) {
			val builder = new ContextBuilder();
			builder.ontModel = ontModel;
			return builder;
		}

		new(EObject subject) {
			this();
			Preconditions.checkNotNull(subject, 'subject');
			Preconditions.checkArgument(subject instanceof SadlResource,
				'Expected an instance of SADL resource. Was: ' + subject);
			Preconditions.checkNotNull(subject.eResource, 'Subject does not contained in a resource.');
			this.subject = subject as SadlResource;
			ontModel = OntModelProvider.find(subject.eResource)
		}

		private new() {
			subject = MISSING_SUBJECT;
			acceptor = ValidationAcceptor.NOOP;
			grammarContextId = Optional.absent;
			restriction = Optional.absent;
		}

		def setOntModel(OntModel ontModel) {
			Preconditions.checkNotNull(ontModel, 'ontModel');
			this.ontModel = ontModel;
			return this;
		}

		def setValidationAcceptor(ValidationAcceptor acceptor) {
			Preconditions.checkNotNull(acceptor, 'acceptor');
			this.acceptor = acceptor;
			return this;
		}

		def setGrammarContextId(String grammarContextId) {
			this.grammarContextId = Optional.fromNullable(grammarContextId);
			return this;
		}

		def setRestriction(SadlResource restriction) {
			this.restriction = Optional.fromNullable(restriction);
			return this;
		}

		def Context build() {
			Preconditions.checkNotNull(ontModel, 'ontModel');
			return new ContextImpl(subject, ontModel, acceptor, grammarContextId, restriction);
		}

	}

	@Data
	static class ContextImpl implements Context {
		val SadlResource subject;
		val OntModel ontModel;
		val ValidationAcceptor acceptor;
		val Optional<String> grammarContextId;
		val Optional<SadlResource> restriction;
	}

}
