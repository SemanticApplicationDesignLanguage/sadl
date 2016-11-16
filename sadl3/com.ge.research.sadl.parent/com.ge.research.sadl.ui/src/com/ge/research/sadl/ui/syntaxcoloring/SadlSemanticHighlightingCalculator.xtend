/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.syntaxcoloring

import com.ge.research.sadl.model.CircularDefinitionException
import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.preferences.SadlPreferences
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlIsInverseOf
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlPropertyCondition
import com.ge.research.sadl.sADL.SadlPropertyInitializer
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.ge.research.sadl.scoping.TestScopeProvider
import com.google.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.emf.ecore.resource.impl.ResourceImpl
import org.eclipse.xtext.ide.editor.syntaxcoloring.IHighlightedPositionAcceptor
import org.eclipse.xtext.ide.editor.syntaxcoloring.ISemanticHighlightingCalculator
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.ui.resource.ProjectByResourceProvider

class SadlSemanticHighlightingCalculator implements ISemanticHighlightingCalculator {
	@Inject package DeclarationExtensions declarationExtensions
    @Inject IPreferenceStoreAccess preferenceStoreAccess
	@Inject ProjectByResourceProvider projectProvider
	
	override void provideHighlightingFor(XtextResource resource, IHighlightedPositionAcceptor acceptor,
		CancelIndicator cancelIndicator) {
		if (resource === null || resource.contents.isEmpty())
			return;
			
		TestScopeProvider.registerResource(resource, false);
		var SadlModel model = resource.contents.head as SadlModel
		// Highlighting for URI strings
		for (imp : model.imports) {
			var nodesForUri = NodeModelUtils.findNodesForFeature(imp, SADLPackage.Literals.SADL_IMPORT__IMPORTED_RESOURCE)
			for (node : nodesForUri) {
				acceptor.addPosition(node.offset, node.length, SadlHighlightingConfiguration.URI_ID)
			}
		}
		if (model.alias !== null) {
			for (node : NodeModelUtils.findNodesForFeature(model, SADLPackage.Literals.SADL_MODEL__ALIAS)) {
				acceptor.addPosition(node.offset, node.length, SadlHighlightingConfiguration.URI_ID)
			}
		}
		for (element : model.eAllContents.toList) {
			var v = element
			if (element instanceof Name) {
				v = element.name
			}
			switch v {
				Name : {
					// check what getName returns. If it returns itself or something of type Name
					var highlightingId = SadlHighlightingConfiguration.VARIABLE_ID
					if (v.eContainer instanceof QueryStatement) {
						highlightingId = SadlHighlightingConfiguration.DEFAULT_ID
					}
					else if (v.function) {
						highlightingId = SadlHighlightingConfiguration.FUNCTION_NAME_ID
					}
					var node = NodeModelUtils.findNodesForFeature(element, SADLPackage.Literals.SADL_RESOURCE__NAME).head
					acceptor.addPosition(node.offset, node.length, highlightingId)
				}
				SadlResource : {
					var nodes = NodeModelUtils.findNodesForFeature(element, SADLPackage.Literals.SADL_RESOURCE__NAME)
					var highlightingId = switch element {
						Name case element.isFunction : SadlHighlightingConfiguration.FUNCTION_NAME_ID
						default : getHighlightingId(v)
					}
					val start = nodes.head.offset
					val end =  nodes.last.offset + nodes.last.length - start
					acceptor.addPosition(start, end, highlightingId)
				}
				SadlPropertyCondition : {
					var highlightingId = getHighlightingId(v.property)
					acceptor.highlight(element, SADLPackage.Literals.SADL_PROPERTY_CONDITION__PROPERTY, highlightingId)
				}
				SadlPropertyInitializer : {
					if (v.property != null) {
						var highlightingId = getHighlightingId(v.property)
						acceptor.highlight(element, SADLPackage.Literals.SADL_PROPERTY_INITIALIZER__PROPERTY, highlightingId)
					}
				}
				SadlSimpleTypeReference : {
					var highlightingId = getHighlightingId(v.type)
					acceptor.highlight(element, SADLPackage.Literals.SADL_SIMPLE_TYPE_REFERENCE__TYPE, highlightingId)
				}
				SadlIsInverseOf : {
					var highlightingId = getHighlightingId(v.otherProperty)
					acceptor.highlight(element, SADLPackage.Literals.SADL_IS_INVERSE_OF__OTHER_PROPERTY, highlightingId)
				}
			}
		}
				
		// get the SADL preferences from a pseudo SADL resource and apply the preference setting to the current 
        // resource (so it applies to both SADL and derived types)				
        val r = new ResourceImpl();
        r.setURI(URI.createFileURI("/"));
        val ambiguousnames = getPrefStore(resource).getBoolean(SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.id);
        TestScopeProvider.registerResource(resource, ambiguousnames);
    }

    def protected getPrefStore(XtextResource resource) {
        val projectContext = projectProvider.getProjectContext(resource)
        if (projectContext !== null) {
            return preferenceStoreAccess.getContextPreferenceStore(projectContext)
        }
        return preferenceStoreAccess.preferenceStore
    }
	
	protected def void highlight(IHighlightedPositionAcceptor acceptor, EObject object, EStructuralFeature feature, String id) {
		for (node : NodeModelUtils.findNodesForFeature(object, feature)) {
			acceptor.addPosition(node.offset, node.length, id)
		}
	}
	
	def private String getHighlightingId(SadlResource rn) {
		val type = try {
			declarationExtensions.getOntConceptType(rn)
		} catch (CircularDefinitionException e) {
			e.definitionType
		}
		switch (type) {
			case CLASS_PROPERTY: {
				return SadlHighlightingConfiguration.OBJECT_PROPERTY_ID
			}
			case DATATYPE_PROPERTY: {
				return SadlHighlightingConfiguration.DATA_PROPERTY_ID
			}
			case ANNOTATION_PROPERTY: {
				return SadlHighlightingConfiguration.ANNOTATION_PROPERTY_ID
			}
			case RDF_PROPERTY: {
				return SadlHighlightingConfiguration.RDF_PROPERTY_ID
			}
			case INSTANCE: {
				return SadlHighlightingConfiguration.INSTANCE_ID
			}
			case CLASS: {
				return SadlHighlightingConfiguration.CLASS_ID
			}
			case CLASS_LIST: {
				return SadlHighlightingConfiguration.CLASS_ID
			}
			case DATATYPE: {
				return SadlHighlightingConfiguration.RDFDATATYPE_ID
			}
			case DATATYPE_LIST: {
				return SadlHighlightingConfiguration.CLASS_ID
			}
			case FUNCTION_DEFN: {
				return SadlHighlightingConfiguration.FUNCTION_NAME_ID
			}
			default: {
				return SadlHighlightingConfiguration.VARIABLE_ID
			}
		}
	}
}
