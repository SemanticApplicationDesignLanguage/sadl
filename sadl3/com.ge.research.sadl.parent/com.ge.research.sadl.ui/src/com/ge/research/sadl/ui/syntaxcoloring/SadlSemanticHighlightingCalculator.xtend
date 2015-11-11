package com.ge.research.sadl.ui.syntaxcoloring

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlImport
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlResource
import com.google.inject.Inject
import java.util.List
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.ide.editor.syntaxcoloring.IHighlightedPositionAcceptor
import org.eclipse.xtext.ide.editor.syntaxcoloring.ISemanticHighlightingCalculator
import org.eclipse.xtext.nodemodel.ICompositeNode
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.util.CancelIndicator

class SadlSemanticHighlightingCalculator implements ISemanticHighlightingCalculator {
	@Inject package DeclarationExtensions declarationExtensions

	override void provideHighlightingFor(XtextResource resource, IHighlightedPositionAcceptor acceptor,
		CancelIndicator cancelIndicator) {
		if (resource === null || resource.contents.isEmpty())
			return;
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
		for (SadlResource rn : EcoreUtil2.getAllContentsOfType(model, SadlResource)) {
			var node = NodeModelUtils.findActualNodeFor(rn)
			var highlightingId = getHighlightingId(rn)
			acceptor.addPosition(node.offset, node.length, highlightingId)
		}
	}

	def private String getHighlightingId(SadlResource rn) {
		switch (declarationExtensions.getOntConceptType(rn)) {
			case CLASS_PROPERTY: {
				return SadlHighlightingConfiguration.OBJECT_PROPERTY_ID
			}
			case DATATYPE_PROPERTY: {
				return SadlHighlightingConfiguration.DATA_PROPERTY_ID
			}
			case ANNOTATION_PROPERTY: {
				return SadlHighlightingConfiguration.ANNOTATION_PROPERTY_ID
			}
			case INSTANCE: {
				return SadlHighlightingConfiguration.INSTANCE_ID
			}
			case CLASS: {
				return SadlHighlightingConfiguration.CLASS_ID
			}
			case DATATYPE: {
				return SadlHighlightingConfiguration.CLASS_ID
			}
			default: {
				return SadlHighlightingConfiguration.VARIABLE_ID
			}
		}
	}
}
