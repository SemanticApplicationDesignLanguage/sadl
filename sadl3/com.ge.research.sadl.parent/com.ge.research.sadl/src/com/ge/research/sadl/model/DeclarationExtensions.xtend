package com.ge.research.sadl.validation

import com.ge.research.sadl.sADL.Declaration
import com.ge.research.sadl.sADL.SADLPackage
import org.eclipse.xtext.linking.impl.LinkingDiagnosticMessageProvider
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import com.google.inject.Inject
import org.eclipse.xtext.util.OnChangeEvictingCache

class SoftDeclarationReference {
	
	@Inject OnChangeEvictingCache cache
	
	def String getConcreteName(Declaration it) {
		cache.get(it->'concreteName', eResource) [
			val nodes = NodeModelUtils.findNodesForFeature(it, SADLPackage.Literals.DECLARATION__NAME)
			val name = nodes.join('')[NodeModelUtils.getTokenText(it)].trim
			if (name.isNullOrEmpty)
				return null
			return name
		]
	}
	
	static class SoftLinkingMessageProvider extends LinkingDiagnosticMessageProvider {
	
		override getUnresolvedProxyMessage(ILinkingDiagnosticContext context) {
			if (context.reference === SADLPackage.Literals.DECLARATION__NAME) {
				// treated as declaration. 
				return null
			}
			super.getUnresolvedProxyMessage(context)
		}
		
	}
}