package com.ge.research.sadl.model

import com.ge.research.sadl.sADL.Declaration
import com.ge.research.sadl.sADL.SADLPackage
import com.google.inject.Inject
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.util.OnChangeEvictingCache

class DeclarationExtensions {
	
	@Inject OnChangeEvictingCache cache
	
	def String getConcreteName(Declaration it) {
		cache.get(it->'concreteName', eResource) [
			val nodes = NodeModelUtils.findNodesForFeature(it, SADLPackage.Literals.DECLARATION__NAME)
			val name = nodes.join('')[NodeModelUtils.getTokenText(it)].trim
			if (name.isNullOrEmpty)
				return null
			if (name.startsWith('^'))
				return name.substring(1)
			return name
		]
	}
	
}
