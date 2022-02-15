package com.naturalsemantics.sadl.sitl.ide.editor

import com.ge.research.sadl.external.ExternalResourceAdapter
import com.ge.research.sadl.ide.editor.coloring.SadlIdeSemanticHighlightingCalculator
import com.ge.research.sadl.model.CircularDefinitionException
import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlIsInverseOf
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlPropertyCondition
import com.ge.research.sadl.sADL.SadlPropertyInitializer
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.ge.research.sadl.utils.SadlASTUtils
import com.google.common.base.Preconditions
import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableSet
import com.google.inject.Inject
import java.util.List
import org.eclipse.xtext.ide.editor.syntaxcoloring.HighlightingStyles
import org.eclipse.xtext.ide.editor.syntaxcoloring.IHighlightedPositionAcceptor
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.util.CancelIndicator

import static com.ge.research.sadl.ide.editor.coloring.SadlIdeHighlightingConfiguration.*
import static org.eclipse.xtext.ide.editor.syntaxcoloring.HighlightingStyles.*
import com.ge.research.sadl.sADL.SadlInstance

class SitlIdeSemanticHighlightingCalculator extends SadlIdeSemanticHighlightingCalculator {

	@Inject
	DeclarationExtensions declarationExtensions

	override getAllStyleIds() {
		return ImmutableSet.copyOf(STYLE_MAPPINGS.keySet);
	}

	override toScopes(String styleId) {
		if (styleId == HighlightingStyles.TASK_ID) {
			return emptyList;
		}
		val scopes = STYLE_MAPPINGS.get(styleId);
		Preconditions.checkNotNull(scopes, '''Cannot map style ID '«styleId»' to the corresponding TextMate scopes.''');
		return scopes;
	}

	override provideHighlightingFor(XtextResource resource, IHighlightedPositionAcceptor acceptor,
		CancelIndicator cancelIndicator) {

		if (resource === null || resource.contents.isEmpty())
			return;

		var SadlModel model = resource.contents.head as SadlModel
		// Highlighting for URI strings
		for (imp : model.imports) {
			var nodesForUri = NodeModelUtils.findNodesForFeature(imp,
				SADLPackage.Literals.SADL_IMPORT__IMPORTED_RESOURCE)
			for (node : nodesForUri) {
				acceptor.addPosition(node.offset, node.length, URI_ID)
			}
		}
		if (model.alias !== null) {
			for (node : NodeModelUtils.findNodesForFeature(model, SADLPackage.Literals.SADL_MODEL__ALIAS)) {
				acceptor.addPosition(node.offset, node.length, URI_ID)
			}
		}
		for (element : model.eAllContents.toList.filter[!SadlASTUtils.isUnit(it)]) {
			var v = element
			if (element instanceof Name) {
				v = element.name
			}
			switch v {
				Name: {
					// check what getName returns. If it returns itself or something of type Name
					var highlightingId = VARIABLE_DECL_ID
					if (v.eContainer instanceof QueryStatement) {
						highlightingId = DEFAULT_ID
					} else if (v.function) {
						highlightingId = FUNCTION_NAME_ID
					}
					var node = NodeModelUtils.findNodesForFeature(element, SADLPackage.Literals.SADL_RESOURCE__NAME).
						head
					var decl = declarationExtensions.getDeclaration(v.name)
					if (!decl.equals(element)) {
						highlightingId = VARIABLE_REF_ID
					}
					acceptor.addPosition(node.offset, node.length, highlightingId)
				}
				SadlResource: {
					var nodes = NodeModelUtils.findNodesForFeature(element, SADLPackage.Literals.SADL_RESOURCE__NAME)
					var highlightingId = switch element {
						Name case element.isFunction: FUNCTION_NAME_ID
						default: getHighlightingId(v)
					}
					val start = nodes.head.offset
					val end = nodes.last.offset + nodes.last.length - start
					acceptor.addPosition(start, end, highlightingId)
				}
				SadlPropertyCondition: {
					var highlightingId = getHighlightingId(v.property)
					acceptor.highlight(element, SADLPackage.Literals.SADL_PROPERTY_CONDITION__PROPERTY, highlightingId)
				}
				SadlPropertyInitializer: {
					if (v.property !== null) {
						var highlightingId = getHighlightingId(v.property)
						acceptor.highlight(element, SADLPackage.Literals.SADL_PROPERTY_INITIALIZER__PROPERTY,
							highlightingId)
					}
				}
				SadlSimpleTypeReference: {
					var highlightingId = getHighlightingId(v.type)
					acceptor.highlight(element, SADLPackage.Literals.SADL_SIMPLE_TYPE_REFERENCE__TYPE, highlightingId)
				}
				SadlIsInverseOf: {
					var highlightingId = getHighlightingId(v.otherProperty)
					acceptor.highlight(element, SADLPackage.Literals.SADL_IS_INVERSE_OF__OTHER_PROPERTY, highlightingId)
				}
			}
		}
	}

//	protected def void highlight(IHighlightedPositionAcceptor acceptor, EObject object, EStructuralFeature feature,
//		String id) {
//
//		for (node : NodeModelUtils.findNodesForFeature(object, feature)) {
//			acceptor.addPosition(node.offset, node.length, id)
//		}
//	}

	def private String getHighlightingId(SadlResource rn) {
		val sr = rn.tryGetSadlResourceFromScope
		if (sr.eContainer instanceof SadlInstance && 
			(sr.eContainer as SadlInstance).nameOrRef !== null &&
			(sr.eContainer as SadlInstance).nameOrRef.equals(rn)) {
			return VARIABLE_DECL_ID
		}
		else {
			val type = try {
				declarationExtensions.getOntConceptType(sr)
			} catch (CircularDefinitionException e) {
				e.definitionType
			}
			switch (type) {
				case CLASS_PROPERTY: {
					return OBJECT_PROPERTY_ID
				}
				case DATATYPE_PROPERTY: {
					return DATA_PROPERTY_ID
				}
				case ANNOTATION_PROPERTY: {
					return ANNOTATION_PROPERTY_ID
				}
				case RDF_PROPERTY: {
					return RDF_PROPERTY_ID
				}
				case INSTANCE: {
					return INSTANCE_ID
				}
				case STRUCTURE_NAME: {
					return STRUCTURE_NAME_ID
				}
				case CLASS: {
					return CLASS_ID
				}
				case CLASS_LIST: {
					return CLASS_ID
				}
				case DATATYPE: {
					return RDFDATATYPE_ID
				}
				case DATATYPE_LIST: {
					return CLASS_ID
				}
				case FUNCTION_DEFN: {
					return FUNCTION_NAME_ID
				}
				default: {
					return VARIABLE_DECL_ID
				}
			}
		}
	}

	/**
	 * Method to map the argument SADL resource to its declaration via the scope
	 * provider. If we do not get the definition but use the SADL resource from the
	 * AST as-is, we can get a false negative answer from the
	 * {@link DeclarationExtensions#isExternal(SadlResource) isExternal} call and
	 * cannot retrieve the correct {@link ExternalResourceAdapter#getType concept
	 * type} for externals.
	 */
//	protected def SadlResource tryGetSadlResourceFromScope(SadlResource toMap) {
//		if (toMap === null) {
//			return null
//		}
//		val resource = toMap.eResource
//		if (resource instanceof XtextResource) {
//			val name = declarationExtensions.getConcreteName(toMap)
//			if (name === null) {
//				return toMap
//			}
//			val scopeProvider = resource.resourceServiceProvider.get(IScopeProvider)
//			val scope = scopeProvider.getScope(toMap, SADL_RESOURCE__NAME)
//			val description = scope.getSingleElement(QualifiedName.create(name))
//			if (description === null) {
//				return toMap
//			}
//			val mapped = description.EObjectOrProxy
//			if (mapped instanceof SadlResource) {
//				if (!mapped.eIsProxy) {
//					return mapped
//				}
//			}
//		}
//		return toMap
//	}

//	private static def List<String> sadl(List<String> scopes) {
//		return ImmutableList.builder.addAll(scopes).add('source.sadl').build
//	}

}
