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
import java.util.Collection
import org.eclipse.emf.ecore.EClass
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.Assignment

import static extension org.eclipse.xtext.GrammarUtil.containingParserRule

/**
 * Reasoner independent implementation of an ontology helper for SADL. 
 */
@ImplementedBy(SadlOntologyHelper)
interface ISadlOntologyHelper {

	/**
	 * Checks the validity of the candidate SADL resource with the ontology context. 
	 */
	def void validate(Context context, SadlResource candidate);

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
		 * Returns with the model processor for the context.
		 */
		def IModelProcessor getModelProcessor();

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
		 * Returns with the EClass associated with the context. In some cases, when the 
		 * grammar context ID is insufficient, we need to check the class of the current 
		 * model element.
		 * 
		 * <p>
		 * For instance; when having {@code Test:} and {@code Test: width of} the the grammar context
		 * ID is {@code PRIMARYEXPRESSION_VALUE} in both cases, but the EClass is a test statement in 
		 * the first case and property of subject in the second case.
		 */
		def Optional<EClass> getContextClass();

		/**
		 * Returns with the restrictions for the context.
		 */
		def Iterable<SadlResource> getRestrictions();

	}

	/**
	 * Contains a couple of grammar context ID for SADL.
	 */
	static abstract class GrammarContextIds {
		
		/**
		 * Function for transforming an assignment into a grammar context ID.
		 */
		public static val (Assignment)=>String TO_STRING = [
			return '''«containingParserRule.name»_«feature»'''.toString.toUpperCase;
		];

		/**
		 * {@code C is a class. myC is a <|>}
		 */
		public static val SADLPRIMARYTYPEREFERENCE_TYPE = 'SADLPRIMARYTYPEREFERENCE_TYPE';

		/**
		 * {@code C is a class. myC is a <|>}
		 */
		public static val SADLPRIMARYTYPEREFERENCE_PRIMITIVETYPE = 'SADLPRIMARYTYPEREFERENCE_PRIMITIVETYPE';
		
		public static val SADLPRIMARYTYPEREFERENCE_LIST = "SADLPRIMARYTYPEREFERENCE_LIST";

		public static val SADLPROPERTYINITIALIZER_PROPERTY = 'SADLPROPERTYINITIALIZER_PROPERTY';

		public static val SADLSTATEMENT_SUPERELEMENT = 'SADLSTATEMENT_SUPERELEMENT';

		public static val PRIMARYEXPRESSION_VALUE = 'PRIMARYEXPRESSION_VALUE';

		public static val SADLPROPERTYCONDITION_PROPERTY = 'SADLPROPERTYCONDITION_PROPERTY';

		public static val SADLRESOURCE_NAME = 'SADLRESOURCE_NAME';
		
		public static val SADLPROPERTYRESTRICTION_TYPEONLY = 'SADLPROPERTYRESTRICTION_TYPEONLY';
		
		public static val PROPOFSUBJECT_RIGHT = 'PROPOFSUBJECT_RIGHT';
		
		public static val PROPOFSUBJECT_PROP = 'PROPOFSUBJECT_PROP';
		
		public static val SADLPROPERTYDECLARATIONINCLASS_NAMEDECLARATIONS = 'SADLPROPERTYDECLARATIONINCLASS_NAMEDECLARATIONS';

		/**
		 * {@code C is a class described by p1 with values of type C. myC is a C with p1 <|>}
		 */
		public static val SADLPROPERTYINITIALIZER_VALUE = 'SADLPROPERTYINITIALIZER_VALUE';

		/**
		 * A subset of grammar context IDs that requires the ontology helper.
		 */
		public static val ONTOLOGY_DEPENDENT_CONTEXT_IDS = #{
			SADLPROPERTYINITIALIZER_VALUE,
			SADLPROPERTYINITIALIZER_PROPERTY,
			SADLRESOURCE_NAME,
//			SADLSTATEMENT_SUPERELEMENT,
			SADLPROPERTYRESTRICTION_TYPEONLY,
			PROPOFSUBJECT_PROP,
			PROPOFSUBJECT_RIGHT
		}

		/**
		 * A subset of grammar context IDs that does *not* require the underlying ontology,
		 * hence traversing the AST or accessing the Xtext index is sufficient.
		 */
		public static val ONTOLOGY_INDEPENDENT_CONTEXT_IDS = #{
			SADLPRIMARYTYPEREFERENCE_TYPE,
			SADLPRIMARYTYPEREFERENCE_LIST,
			SADLPRIMARYTYPEREFERENCE_PRIMITIVETYPE,
			SADLSTATEMENT_SUPERELEMENT,
			PRIMARYEXPRESSION_VALUE,
			SADLPROPERTYCONDITION_PROPERTY,
			SADLPROPERTYDECLARATIONINCLASS_NAMEDECLARATIONS
		}

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
		var IModelProcessor modelProcessor;

		var OntModel ontModel;
		var ValidationAcceptor acceptor;
		var Optional<String> grammarContextId;
		var Optional<EClass> contextClass;
		var Collection<SadlResource> restrictions;

		/**
		 * Returns with a new context builder that has no subject SADL resource.
		 */
		static def createWithoutSubject(OntModel ontModel) {
			val builder = new ContextBuilder();
			builder.ontModel = ontModel;
			return builder;
		}

		new(SadlResource subject, IModelProcessor processor) {
			this();
			Preconditions.checkNotNull(subject, 'subject');
			Preconditions.checkArgument(subject instanceof SadlResource,
				'Expected an instance of SADL resource. Was: ' + subject);
			Preconditions.checkNotNull(subject.eResource, 'Subject does not contained in a resource.');
			this.subject = subject as SadlResource;
			this.modelProcessor = Preconditions.checkNotNull(processor, 'processor');
			ontModel = OntModelProvider.find(subject.eResource);
		}

		private new() {
			subject = MISSING_SUBJECT;
			acceptor = ValidationAcceptor.NOOP;
			grammarContextId = Optional.absent;
			restrictions = newArrayList();
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

		def setContextClass(EClass contextClass) {
			this.contextClass = Optional.fromNullable(contextClass);
			return this;
		}

		def addRestriction(SadlResource restriction) {
			Preconditions.checkNotNull(restriction, 'restriction');
			restrictions.add(restriction);
			return this;
		}

		def Context build() {
			if (ontModel === null) {
				return null;
			}
			return new ContextImpl(ontModel, subject, modelProcessor, acceptor, grammarContextId, contextClass, restrictions);
		}

	}

	@Data
	static class ContextImpl implements Context {
		val OntModel ontModel;
		val SadlResource subject;
		val IModelProcessor modelProcessor;
		val ValidationAcceptor acceptor;
		val Optional<String> grammarContextId;
		val Optional<EClass> contextClass;
		val Iterable<SadlResource> restrictions;
	}

}
