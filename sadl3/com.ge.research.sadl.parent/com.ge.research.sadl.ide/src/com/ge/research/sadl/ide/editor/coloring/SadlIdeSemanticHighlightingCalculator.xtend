package com.ge.research.sadl.ide.editor.coloring

import com.ge.research.sadl.external.ExternalResourceAdapter
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
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.xtext.ide.editor.syntaxcoloring.HighlightingStyles
import org.eclipse.xtext.ide.editor.syntaxcoloring.IHighlightedPositionAcceptor
import org.eclipse.xtext.ide.editor.syntaxcoloring.ISemanticHighlightingCalculator
import org.eclipse.xtext.ide.server.semanticHighlight.ISemanticHighlightingStyleToTokenMapper
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.scoping.IScopeProvider
import org.eclipse.xtext.util.CancelIndicator

import static com.ge.research.sadl.ide.editor.coloring.SadlIdeHighlightingConfiguration.*
import static com.ge.research.sadl.sADL.SADLPackage.Literals.*
import static org.eclipse.xtext.ide.editor.syntaxcoloring.HighlightingStyles.*

class SadlIdeSemanticHighlightingCalculator implements ISemanticHighlightingCalculator, ISemanticHighlightingStyleToTokenMapper {

	static interface Styles {
		val URI_STYLE = URI_ID;
		val CLASS_STYLE = CLASS_ID;
		val INSTANCE_STYLE = INSTANCE_ID;
		val STRUCTURE_NAME_STYLE = STRUCTURE_NAME_ID;
		val VARIABLE_STYLE = VARIABLE_ID;
		val DATA_PROPERTY_STYLE = DATA_PROPERTY_ID;
		val OBJECT_PROPERTY_STYLE = OBJECT_PROPERTY_ID;
		val ANNOTATION_PROPERTY_STYLE = ANNOTATION_PROPERTY_ID;
		val RDF_PROPERTY_STYLE = RDF_PROPERTY_ID;
		val RDFDATATYPE_STYLE = RDFDATATYPE_ID;
		val FUNCTION_NAME_STYLE = FUNCTION_NAME_ID;
	}

	static interface Scopes {
		val URI_SCOPES = #['keyword.control'].sadl;
		val CLASS_SCOPES = #['beginning.punctuation.definition.list.markdown'].sadl;
		val INSTANCE_SCOPES = #['support.type.property-name'].sadl;
		val STRUCTURE_NAME_SCOPES = #['punctuation.definition.tag'].sadl;
		val VARIABLE_SCOPES = #['emphasis'].sadl;
		val DATA_PROPERTY_SCOPES = #['strong'].sadl;
		val OBJECT_PROPERTY_SCOPES = #['support.type.property-name'].sadl;
		val ANNOTATION_PROPERTY_SCOPES = #['constant.regexp'].sadl;
		val RDF_PROPERTY_SCOPES = #['keyword.other.unit'].sadl;
		val RDFDATATYPE_SCOPES = #['keyword.operator'].sadl;
		val FUNCTION_NAME_SCOPES = #['string.regexp'].sadl;
	}

	public static val STYLE_MAPPINGS = #{
		Styles.URI_STYLE -> Scopes.URI_SCOPES,
		Styles.CLASS_STYLE -> Scopes.CLASS_SCOPES,
		Styles.INSTANCE_STYLE -> Scopes.INSTANCE_SCOPES,
		Styles.STRUCTURE_NAME_STYLE -> Scopes.STRUCTURE_NAME_SCOPES,
		Styles.VARIABLE_STYLE -> Scopes.VARIABLE_SCOPES,
		Styles.DATA_PROPERTY_STYLE -> Scopes.DATA_PROPERTY_SCOPES,
		Styles.OBJECT_PROPERTY_STYLE -> Scopes.OBJECT_PROPERTY_SCOPES,
		Styles.ANNOTATION_PROPERTY_STYLE -> Scopes.ANNOTATION_PROPERTY_SCOPES,
		Styles.RDF_PROPERTY_STYLE -> Scopes.RDF_PROPERTY_SCOPES,
		Styles.RDFDATATYPE_STYLE -> Scopes.RDFDATATYPE_SCOPES,
		Styles.FUNCTION_NAME_STYLE -> Scopes.FUNCTION_NAME_SCOPES
	};

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
					var highlightingId = VARIABLE_ID
					if (v.eContainer instanceof QueryStatement) {
						highlightingId = DEFAULT_ID
					} else if (v.function) {
						highlightingId = FUNCTION_NAME_ID
					}
					var node = NodeModelUtils.findNodesForFeature(element, SADLPackage.Literals.SADL_RESOURCE__NAME).
						head
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

	protected def void highlight(IHighlightedPositionAcceptor acceptor, EObject object, EStructuralFeature feature,
		String id) {

		for (node : NodeModelUtils.findNodesForFeature(object, feature)) {
			acceptor.addPosition(node.offset, node.length, id)
		}
	}

	def private String getHighlightingId(SadlResource rn) {
		val type = try {
				declarationExtensions.getOntConceptType(rn.tryGetSadlResourceFromScope)
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
				return VARIABLE_ID
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
	protected def SadlResource tryGetSadlResourceFromScope(SadlResource toMap) {
		if (toMap === null) {
			return null
		}
		val resource = toMap.eResource
		if (resource instanceof XtextResource) {
			val name = declarationExtensions.getConcreteName(toMap)
			if (name === null) {
				return toMap
			}
			val scopeProvider = resource.resourceServiceProvider.get(IScopeProvider)
			val scope = scopeProvider.getScope(toMap, SADL_RESOURCE__NAME)
			val description = scope.getSingleElement(QualifiedName.create(name))
			if (description === null) {
				return toMap
			}
			val mapped = description.EObjectOrProxy
			if (mapped instanceof SadlResource) {
				if (!mapped.eIsProxy) {
					return mapped
				}
			}
		}
		return toMap
	}

	private static def List<String> sadl(List<String> scopes) {
		return ImmutableList.builder.addAll(scopes).add('source.sadl').build
	}

}
