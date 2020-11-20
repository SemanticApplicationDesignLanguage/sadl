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

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.model.OntConceptType
import com.ge.research.sadl.processing.OntModelProvider
import com.ge.research.sadl.processing.SadlConstants
import com.ge.research.sadl.sADL.BinaryOperation
import com.ge.research.sadl.sADL.Declaration
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.PropOfSubject
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlProperty
import com.ge.research.sadl.sADL.SadlPropertyInitializer
import com.ge.research.sadl.sADL.SadlRangeRestriction
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.ge.research.sadl.sADL.SubjHasProp
import com.ge.research.sadl.services.SADLGrammarAccess
import com.ge.research.sadl.utils.ResourceManager
import com.google.common.base.Predicate
import com.google.common.base.Predicates
import com.google.inject.Inject
import com.google.inject.Singleton
import java.util.ArrayList
import java.util.Collection
import java.util.HashMap
import java.util.List
import org.apache.jena.vocabulary.XSD
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.CrossReference
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.EnumRule
import org.eclipse.xtext.GrammarUtil
import org.eclipse.xtext.Keyword
import org.eclipse.xtext.RuleCall
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistEntry
import org.eclipse.xtext.ide.editor.contentassist.IIdeContentProposalAcceptor
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalCreator
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalPriorities
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalProvider
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.util.TextRegion

import static com.ge.research.sadl.preferences.SadlPreferences.*
import static com.ge.research.sadl.processing.ISadlOntologyHelper.GrammarContextIds.*
import static com.ge.research.sadl.processing.SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME
import static com.ge.research.sadl.processing.SadlConstants.SADL_IMPLICIT_MODEL_FILENAME
import static com.ge.research.sadl.sADL.SADLPackage.Literals.*

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

	override protected _createProposals(RuleCall ruleCall, ContentAssistContext ctx,
		IIdeContentProposalAcceptor acceptor) {
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

	override protected _createProposals(Assignment assignment, ContentAssistContext ctx,
		IIdeContentProposalAcceptor acceptor) {
		if(excludedNamespaces !== null) excludedNamespaces.clear
		if(typeRestrictions !== null) typeRestrictions.clear
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
			case grammarAccess.sadlCardinalityConditionAccess.cardinalityAssignment_2: {
				ctx.completeCardinalityAssigment(acceptor);
			}
			default: {
				super._createProposals(assignment, ctx, acceptor);
			}
		}
	}

	override protected getCrossrefFilter(CrossReference reference, ContentAssistContext ctx) {
		// Special case for filtering out all those resources among import proposals which are already imported.
		if (reference.eContainer == grammarAccess.sadlImportAccess.importedResourceAssignment_1) {
			val model = EcoreUtil2.getContainerOfType(ctx.currentModel, SadlModel);
			val imports = model.imports.map[importedResource].filterNull.map[baseUri].toSet;
			return [
				return SADL_IMPLICIT_MODEL_FILENAME != EObjectURI?.lastSegment &&
				    SADL_BUILTIN_FUNCTIONS_FILENAME != EObjectURI?.lastSegment && 
					KNOWN_FILE_EXTENSION.contains(EObjectURI?.fileExtension) && !imports.contains(name.toString);
			];
		}
		return Predicates.and(#[
			ctx.implicitModelFilter,
			lookupCrossReference(reference, ctx)
		]);
	}

	protected def Predicate<IEObjectDescription> getImplicitModelFilter(ContentAssistContext context) {
		return if (context.currentModel.shouldFilterBuiltIns)
			[
				EObjectURI.trimFragment.lastSegment != SADL_IMPLICIT_MODEL_FILENAME
			]
		else
			Predicates.alwaysTrue
	}

	protected def boolean shouldFilterBuiltIns(EObject model) {
		if (model === null || model.eIsProxy || model.eResource === null || !(model.eResource instanceof XtextResource)) {
			return false;
		}

		val serviceProvider = (model.eResource as XtextResource).resourceServiceProvider
		if (serviceProvider === null) {
			return false
		}
		val values = serviceProvider.get(IPreferenceValuesProvider).getPreferenceValues(model.eResource);
		val preference = values.getPreference(CONTENT_ASSIST__FILTER_IMPLICIT_MODEL);
		if (preference !== null) {
			return Boolean.parseBoolean(preference);
		}
		return false;
	}

	// Creates a proposal for an EOS terminal.  Xtext can't guess (at
	// the moment) what the valid values for a terminal rule are, so
	// that's why there is no automatic content assist for EOS.
	protected def completeEOS(ContentAssistContext ctx, IIdeContentProposalAcceptor it) {
		val proposalText = ".\n";
		val proposal = proposalCreator.createProposal(proposalText, ctx, [
			description = '. - End of Sentence';
		]);
		val priority = proposalPriorities.getDefaultPriority(proposal);
		accept(proposal, priority);
	}

	protected def completeBaseUri(ContentAssistContext ctx, IIdeContentProposalAcceptor it) {
		val fn = ctx.resource.URI.lastSegment
		if (fn.equals(ResourceManager.ServicesConf_SFN)) {
			val proposalText = '''"''' + ResourceManager.ServicesConfigurationURI + 
				'''" alias servicesconfig.
				
import "''' + ResourceManager.ServicesConfigurationConceptsURI + '''".

A KnowledgeBase with entryPoint (A NamedService ServiceName
     with modelName "<uri>").
				'''
			val proposal = proposalCreator.createProposal(proposalText, ctx);
			val priority = proposalPriorities.getDefaultPriority(proposal);
			accept(proposal, priority);
		}
		else {
			val proposalText = '''"http://sadl.org/«ctx.resource.URI.lastSegment»"''';
			val proposal = proposalCreator.createProposal(proposalText, ctx);
			val priority = proposalPriorities.getDefaultPriority(proposal);
			accept(proposal, priority);
		}
	}

	protected def completeAlias(ContentAssistContext ctx, IIdeContentProposalAcceptor it) {
		val proposalText = ctx.resource.URI.trimFileExtension.lastSegment;
		val proposal = proposalCreator.createProposal(proposalText, ctx);
		val priority = proposalPriorities.getDefaultPriority(proposal);
		accept(proposal, priority);
	}

	protected def completeImports(ContentAssistContext ctx, IIdeContentProposalAcceptor it) {
		val crossRef = grammarAccess.sadlImportAccess.importedResourceAssignment_1.terminal as CrossReference;
		_createProposals(crossRef, ctx, it);
	}

	protected def completeCardinalityAssigment(ContentAssistContext ctx, IIdeContentProposalAcceptor acceptor) {
		#['one', 2, 3, 4, 5].forEach [
			val entry = proposalCreator.createProposal('''«it»''', ctx);
			acceptor.accept(entry, proposalPriorities.getDefaultPriority(entry));
		]
		val proposal = 'CARDINALITY'
		val entry = proposalCreator.createProposal(proposal, ctx) [
			editPositions += new TextRegion(ctx.offset, proposal.length);
			kind = ContentAssistEntry.KIND_VALUE;
			description = 'Cardinality number'
		]
		acceptor.accept(entry, 100)
	}

	protected static val SUPPORTED_FILE_EXTENSION = #{'sadl', 'n3', 'owl', 'ntriple', 'nt'};
	protected static val BUILTIN_FILES = #{'SadlImplicitModel.sadl', 'SadlBuiltinFunctions.sadl'};

	@Inject protected DeclarationExtensions declarationExtensions;
	@Inject protected extension ProposalProviderFilterProvider;
	@Inject protected extension IOntologyContextProvider;

	val PropertyRangeKeywords = newArrayList('string', 'boolean', 'decimal', 'int', 'long', 'float', 'double',
		'duration', 'dateTime', 'time', 'date', 'gYearMonth', 'gYear', 'gMonthDay', 'gDay', 'gMonth', 'hexBinary',
		'base64Binary', 'anyURI', 'integer', 'negativeInteger', 'nonNegativeInteger', 'positiveInteger',
		'nonPositiveInteger', 'byte', 'unsignedByte', 'unsignedInt', 'anySimpleType', 'data', 'class')

	protected List<OntConceptType> typeRestrictions
	protected List<String> excludedNamespaces

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

	protected override dispatch void createProposals(Keyword keyword, ContentAssistContext context,
		IIdeContentProposalAcceptor acceptor) {

		if (filterKeyword(keyword, context)) {
			var proposalText = keyword.getValue();
			if (isInvokedDirectlyAfterKeyword(context) && requireSpaceBefore(keyword, context) &&
				!hasSpaceBefore(context)) {
				proposalText = " " + proposalText;
			}
			if (requireSpaceAfter(keyword, context) && !hasSpaceAfter(context)) {
				proposalText = proposalText + " ";
			}

			val proposal = proposalCreator.createProposal(proposalText, context);
			if (proposal !== null) {
				proposal.kind = ContentAssistEntry.KIND_KEYWORD
				acceptor.accept(proposal, proposalPriorities.getKeywordPriority(keyword.value, proposal))
			}
		}
	}

	override protected filterKeyword(Keyword keyword, ContentAssistContext context) {
		return includeKeyword(keyword, context);
	}

	/**
	 * Method to lookup cross reference but eliminating duplicates. A given SadlResource will appear twice, 
	 * once as an unqualified name, e.g., "foo" and once as a qualified name, e.g., "ns1:foo".
	 * If multiple imports contain a SadlResource with the same localname but in different namespaces, 
	 * then there will be a "foo", "ns1:foo" in one import and a "foo", "ns2:foo" in another. If this
	 * happens then we want to include the qualified name (apply return true) otherwise the unqualified name.
	 */
	protected def Predicate<IEObjectDescription> lookupCrossReference(CrossReference crossReference,
		ContentAssistContext context) {
		val criterable = getFilteredCrossReferenceList(crossReference, context) // Iterable<IEObjectDescription>
		val itr = criterable.iterator

		val pm = context.previousModel
		val cm = context.currentModel
//		displayModel(pm, "Previous")
//		displayModel(cm, "Current")
		if (pm !== null) {
			if (pm instanceof Declaration) {
				val declcontainer = (pm as Declaration).eContainer
				if (declcontainer !== null && declcontainer instanceof SubjHasProp) {
					val sadlprop = (declcontainer as SubjHasProp).prop
					restrictTypeToClass(sadlprop)
				}
			} else if (pm instanceof SubjHasProp) {
				restrictTypeToAllPropertyTypes
			} else if (pm instanceof PropOfSubject && (pm as PropOfSubject).left !== null) {
				val prop = (pm as PropOfSubject).left
				if ((pm as PropOfSubject).right !== null && (pm as PropOfSubject).right instanceof Declaration) {
					if (((pm as PropOfSubject).right as Declaration).type !== null) {
						excludeNamespace(SadlConstants.SADL_IMPLICIT_MODEL_URI)
						restrictTypeToAllPropertyTypes
					} else {
						excludeNamespace(SadlConstants.SADL_IMPLICIT_MODEL_URI)
						restrictTypeToClass(prop as SadlResource)
					}
				} else if (prop instanceof Name) {
					excludeNamespace(SadlConstants.SADL_IMPLICIT_MODEL_URI)
					restrictTypeToClassPlusVars((prop as Name).name)
				} else if (prop instanceof SadlResource) {
					excludeNamespace(SadlConstants.SADL_IMPLICIT_MODEL_URI)
					restrictTypeToClassPlusVars(prop as SadlResource)
				} else if (prop instanceof Declaration) {
					excludeNamespace(SadlConstants.SADL_IMPLICIT_MODEL_URI)
					restrictTypeToAllPropertyTypes
				}
			} else if (pm instanceof SadlResource && cm !== null && cm.eContainer instanceof SadlModel) {
				// just a name on a new line--can't be followed by another name
				return Predicates.alwaysFalse
			} else if (pm instanceof BinaryOperation) {
				val left = (pm as BinaryOperation).left
				val op = (pm as BinaryOperation).op
//				val right = (pm as BinaryOperation).right
				if (op.equals("is")) {
					if (left instanceof Name) {
						val ltype = declarationExtensions.getOntConceptType((left as Name).name)
						if (ltype.equals(OntConceptType.VARIABLE)) {
							// no restrictions
							excludeNamespace(SadlConstants.SADL_BASE_MODEL_URI)
						}
					}
				}
			} else if (pm instanceof SadlPropertyInitializer) {
				if ((pm as SadlPropertyInitializer).property === null) {
					excludeNamespace(SadlConstants.SADL_IMPLICIT_MODEL_URI)
					restrictTypeToAllPropertyTypes
				} else {
					// values of the property
				}
			} else {
				val container = pm.eContainer
				if (container instanceof SubjHasProp) {
					restrictTypeToAllPropertyTypes
				} else if (container instanceof SadlProperty) {
					val propsr = (container as SadlProperty).nameOrRef
					restrictTypeToClass(propsr)
				}
			}
		}

		val nmMap = new HashMap<String, QualifiedName> // a map of qualified names with simple name as key, qualified name as value
		val qnmList = new ArrayList<QualifiedName>
		val eliminatedNames = new ArrayList<String> // simple names of SadlReferences that require a qualified name
		try {
			if (!itr.empty) {
				while (!itr.empty) {
					val nxt = itr.next
					if (nxt.qualifiedName.segmentCount > 1) {
						val nm = nxt.qualifiedName.lastSegment
						if (nmMap.containsKey(nm) && !nxt.qualifiedName.equals(nmMap.get(nm))) {
							// we already have a qname with the same local name 
							qnmList.add(nxt.qualifiedName)
							if (!qnmList.contains(nmMap.get(nm))) {
								qnmList.add(nmMap.get(nm))
							}
							eliminatedNames.add(nxt.qualifiedName.lastSegment)
						} else {
							nmMap.put(nxt.name.lastSegment, nxt.qualifiedName)
						}
					}
				}
			}
		} catch (Throwable t) {
			t.printStackTrace
		}

		return [ input |
			val element = input.EObjectOrProxy
			if (typeRestrictions !== null && typeRestrictions.size > 0) {
				if (element instanceof SadlResource) {
					val eltype = declarationExtensions.getOntConceptType(element as SadlResource)
					if (!typeRestrictions.contains(eltype)) {
						return false;
					}
				}
			}
			if (excludedNamespaces !== null) {
				if (element instanceof SadlResource) {
					val uri = declarationExtensions.getConceptUri(element as SadlResource)
					for (ens : excludedNamespaces) {
						if (uri.startsWith(ens)) {
							return false;
						}
					}
				}
			}
			val isQName = input.qualifiedName.segmentCount > 1
			if (isQName) {
				if (qnmList.contains(input.name)) {
					// qnmList only contains qualified names that are ambiguous so return true for this qualified name
					return true
				} else {
					return false
				}
			}
			val nm = input.name.lastSegment
			if (eliminatedNames.contains(nm)) {
				return false;
			}
			return context.crossReferenceFilter.apply(input);
		]
	}

	protected def boolean includeKeyword(Keyword keyword, ContentAssistContext context) {
		val enumRule = EcoreUtil2.getContainerOfType(keyword, EnumRule);
		// https://github.com/crapo/sadlos2/issues/406
		if (enumRule !== null) {
			val ontologyContext = context.ontologyContext.orNull;
			if (ontologyContext !== null) {
				val grammarContextId = ontologyContext.grammarContextId.orNull;
				val contextClass = ontologyContext.contextClass.orNull;
				// Primitive datatype can be included only in ranges
				if (grammarContextId == SADLPRIMARYTYPEREFERENCE_PRIMITIVETYPE && contextClass === SADL_RANGE_RESTRICTION) {
					return true;
				}
				// Primitive datatype cannot be a domain of any property
				return false;
			}
		}

		var model = context.currentModel
		if (model === null) {
			model = context.previousModel
		}
		if (model !== null) {
			if (model instanceof Declaration) {
				return false
			}
			val container = model.eContainer
			val kval = keyword.value
			if (container instanceof SadlProperty) {
				if (model instanceof SadlRangeRestriction) {
					val ge = context.lastCompleteNode.grammarElement
					if (ge instanceof Keyword && (ge as Keyword).value.equals("type")) {
						if (PropertyRangeKeywords.contains(kval)) {
							return true
						} else {
							return false
						}
					}
				}
				return true
			} else if (container instanceof Declaration) {
				if (kval.equals("with") || kval.equals("only") || kval.equals("when") || kval.equals("where")) {
					return true
				} else {
					return false
				}
			} else if (container instanceof SubjHasProp) {
				if ((container as SubjHasProp).prop !== null) {
					if ((container as SubjHasProp).right === null) {
						// this is ready for a value but doesn't have one yet
						if (kval.equals("(") || kval.equals("[") || kval.equals("{")) {
							return true;
						} else {
							return false;
						}
					} else if ((container as SubjHasProp).right instanceof Name) {
						val valnm = ((container as SubjHasProp).right as Name)
						if (declarationExtensions.isProperty(valnm.name)) {
							if (!kval.equals("of")) {
								return false
							}
							return true
						}
					}
				}
			}
			if (model instanceof Name) {
				if (declarationExtensions.isProperty((model as Name).name)) {
					if (!kval.equals("of")) {
						return false
					}
				}
			} else if (model instanceof PropOfSubject) {
				val lcn = context.lastCompleteNode
				val lcnText = lcn.text
				if (lcnText.equals("a")) {
					return false
				}
				if (kval.equals("(")) {
					return true
				}
				return false
			} else if (model instanceof SadlPropertyInitializer) {
				if (kval.equals("true") || kval.equals("false") || kval.equals("PI") || kval.equals("e")) {
					val proptype = declarationExtensions.getOntConceptType((model as SadlPropertyInitializer).property)
					if (proptype.equals(OntConceptType.DATATYPE_PROPERTY)) {
						// check property range
						val ontModel = OntModelProvider.find(model.eResource)
						if (ontModel !== null) {
							val ontprop = ontModel.getOntProperty(
								declarationExtensions.getConceptUri((model as SadlPropertyInitializer).property))
							if (ontprop !== null) {
								val rnglst = ontprop.listRange
								if (rnglst !== null) {
									if (kval.equals("true") || kval.equals("false")) {
										while (rnglst.hasNext) {
											val rng = rnglst.next
											if (rng.equals(XSD.xboolean)) {
												rnglst.close
												return true
											}
										}
									} else {
										while (rnglst.hasNext) {
											val rng = rnglst.next
											if (rng.equals(XSD.decimal) || rng.equals(XSD.xdouble) ||
												rng.equals(XSD.xfloat)) {
												rnglst.close
												return true
											}
										}
									}
								}
							}
						}
					}
				}
				return false;
			}
		}
		return true
	}

	protected def void displayModel(EObject object, String label) {
		System.out.println(label + ": " + object.class.canonicalName)
		if (object instanceof SadlResource) {
			System.out.println(declarationExtensions.getConceptUri(object as SadlResource))
			System.out.println(declarationExtensions.getOntConceptType(object as SadlResource))
		} else if (object instanceof SubjHasProp) {
			displayModel((object as SubjHasProp).left, "SubjHasProp left")
			displayModel((object as SubjHasProp).prop, "SubHasProp prop")
			displayModel((object as SubjHasProp).right, "SubjHasProp right")
		}
	}

	protected def excludeNamespace(String nsuri) {
		if (excludedNamespaces === null) {
			val ens = new ArrayList<String>
			excludedNamespaces = ens
		}
		if (!excludedNamespaces.contains(nsuri)) {
			excludedNamespaces.add(nsuri)
		}
	}

	protected def restrictTypeToClassPlusVars(SadlResource resource) {
		restrictTypeToClass(resource)
		if (typeRestrictions !== null) {
			typeRestrictions.add(OntConceptType.VARIABLE)
		} else {
			val typeList = new ArrayList<OntConceptType>
			typeList.add(OntConceptType.VARIABLE)
			typeRestrictions = typeList
		}
	}

	protected def restrictTypeToClass(SadlResource propsr) {
		// only classes in the domain of the property
		if (propsr !== null) {
			// for now just filter to classes
			val typeList = new ArrayList<OntConceptType>
			typeList.add(OntConceptType.CLASS)
			typeRestrictions = typeList
		}
	}

	protected def restrictTypeToAllPropertyTypes() {
		val typeList = new ArrayList<OntConceptType>
		typeList.add(OntConceptType.ANNOTATION_PROPERTY)
		typeList.add(OntConceptType.CLASS_PROPERTY)
		typeList.add(OntConceptType.DATATYPE_PROPERTY)
		typeList.add(OntConceptType.RDF_PROPERTY)
		typeRestrictions = typeList
	}

	protected def addRestrictionType(OntConceptType type) {
		if (typeRestrictions === null) {
			typeRestrictions = new ArrayList<OntConceptType>
		}
		typeRestrictions.add(type)
	}

	protected def getFilteredCrossReferenceList(CrossReference crossReference, ContentAssistContext context) {
		val containingParserRule = GrammarUtil.containingParserRule(crossReference); // ParserRule
		if (!GrammarUtil.isDatatypeRule(containingParserRule)) {
			if (containingParserRule.isWildcard()) {
				// TODO we need better ctrl flow analysis here
				// The cross reference may come from another parser rule then the current model 
				val ref = GrammarUtil.getReference(crossReference, context.getCurrentModel().eClass());
				if (ref !== null) {
					val scope = getScopeProvider().getScope(context.currentModel, ref) as IScope; // IScope
					return scope.allElements
				}
			} else {
				if (context.currentModel instanceof SadlSimpleTypeReference) {
					return emptyList;
				}
				val ref = GrammarUtil.getReference(crossReference);
				if (ref !== null) {
					val scope = getScopeProvider().getScope(context.currentModel, ref) as IScope; // IScope
					return scope.allElements
				}
			}
		}
		return null
	}

	protected def isInvokedDirectlyAfterKeyword(ContentAssistContext context) {
		return context.getLastCompleteNode().getTotalEndOffset() == context.getOffset();
	}

	protected def requireSpaceBefore(Keyword keyword, ContentAssistContext context) {
//		if (!keywordsWithSpaceBefore.contains(keyword)) {
//			return false;
//		}
		return true;
	}

	protected def hasSpaceBefore(ContentAssistContext context) {
		// TODO: Detect space before invocation offset
		return false;
	}

	protected def requireSpaceAfter(Keyword keyword, ContentAssistContext context) {
//		if (!keywordsWithSpaceAfter.contains(keyword)) {
//			return false;
//		}
		return true;
	}

	protected def boolean hasSpaceAfter(ContentAssistContext context) {
		if(!context.getCurrentNode().hasNextSibling()) return false; // EOF
		// TODO: Detect space after invocation offset
		return false;
	}

}
