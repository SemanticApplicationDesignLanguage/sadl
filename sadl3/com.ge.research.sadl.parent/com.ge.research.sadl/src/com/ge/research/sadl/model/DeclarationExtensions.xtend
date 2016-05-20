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
package com.ge.research.sadl.model

import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlCanOnlyBeOneOf
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration
//import com.ge.research.sadl.sADL.SadlDataTypeDeclaration
import com.ge.research.sadl.sADL.SadlInstance
import com.ge.research.sadl.sADL.SadlIntersectionType
import com.ge.research.sadl.sADL.SadlMustBeOneOf
import com.ge.research.sadl.sADL.SadlPrimitiveDataType
import com.ge.research.sadl.sADL.SadlProperty
import com.ge.research.sadl.sADL.SadlRangeRestriction
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.ge.research.sadl.sADL.SadlTypeReference
import com.ge.research.sadl.sADL.SadlUnionType
import com.ge.research.sadl.sADL.SadlValueList
import com.google.inject.Inject
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.util.OnChangeEvictingCache
import com.ge.research.sadl.sADL.SadlNecessaryAndSufficient
import org.eclipse.xtext.EcoreUtil2
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlIsAnnotation
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.SadlParameterDeclaration
import com.ge.research.sadl.sADL.ExternalEquationStatement
import com.ge.research.sadl.sADL.EquationStatement
import java.util.Set
import java.util.HashSet

class DeclarationExtensions {
	
	@Inject OnChangeEvictingCache cache
	
	def String getConcreteName(SadlResource it) {
		cache.get(it->'concreteName', eResource) [
			val nodes = NodeModelUtils.findNodesForFeature(it, SADLPackage.Literals.SADL_RESOURCE__NAME)
			val name = nodes.join('')[NodeModelUtils.getTokenText(it)].trim
			if (name.isNullOrEmpty)
				return null
			if (name.startsWith('^'))
				return name.substring(1)
			return name
		]
	}
	
	def String getConceptUri(SadlResource it) {
		val declaration = declaration
		if (declaration != null) {	
			val part1 = EcoreUtil2.getContainerOfType(declaration, SadlModel)
			if (part1 != null) {
				val part2 = part1.baseUri
				if (part2 != null) {
					return part2 +"#"+declaration.concreteName
				}
			}
		}
		return null
	}
	
	def String getConceptQualifiedName(SadlResource it) {
		val declaration = declaration
		if (declaration != null) {	
			val part1 = EcoreUtil2.getContainerOfType(declaration, SadlModel)
			if (part1 != null) {
				val part2 = part1.alias
				if (part2 != null) {
					return part2 +":"+declaration.concreteName
				}
			}
		}
		return null
	}
	
	def SadlResource getDeclaration(SadlResource resource) {
		if (resource.name !== null && !resource.name.eIsProxy) {
			return resource.name
		}
		return resource
	}
	
	def String getConceptNamespace(SadlResource it) {
		val declaration = declaration
		if (declaration != null) {	
			val part1 = EcoreUtil2.getContainerOfType(declaration, SadlModel)
			if (part1 != null) {
				val part2 = part1.baseUri
				if (part2 != null) {
					return part2 + "#"
				}
			}
		}
		return null
	}
	
	def String getConceptPrefix(SadlResource it) {
		val declaration = declaration
		if (declaration != null) {	
			val part1 = EcoreUtil2.getContainerOfType(declaration, SadlModel)
			if (part1 != null) {
				val part2 = part1.alias
				if (part2 != null) {
					return part2
				}
			}
		}
		return null
	}
	
	private ThreadLocal<Set<SadlResource>> recursionDetection = new ThreadLocal<Set<SadlResource>>();
	
	def OntConceptType getOntConceptType(SadlResource resource) throws CircularDefinitionException {
		if (recursionDetection.get == null) {
			recursionDetection.set(new HashSet)
		}
		if (!recursionDetection.get.add(resource)) {
			// Recursion detected. Should be an error.
//			return OntConceptType.CLASS
			throw new CircularDefinitionException("Concept is not properly defined", OntConceptType.CLASS)
		}
		try {
			if (resource instanceof Name) {
				return OntConceptType.VARIABLE
			}
			
			switch e: resource.declaration.eContainer {
				
				EquationStatement, 
				ExternalEquationStatement :
					OntConceptType.FUNCTION_DEFN
					
				SadlClassOrPropertyDeclaration case e.restrictions.exists[it instanceof SadlIsAnnotation],
				SadlProperty case e.restrictions.exists[it instanceof SadlIsAnnotation] :
					OntConceptType.ANNOTATION_PROPERTY
					
				SadlClassOrPropertyDeclaration case e.superElement.referencedSadlResources.exists[ontConceptType === OntConceptType.CLASS_PROPERTY] :
					OntConceptType.CLASS_PROPERTY
					
				SadlClassOrPropertyDeclaration case e.superElement.referencedSadlResources.exists[ontConceptType === OntConceptType.CLASS_PROPERTY] :
					OntConceptType.CLASS_PROPERTY
				
				SadlClassOrPropertyDeclaration case e.superElement.referencedSadlResources.exists[ontConceptType === OntConceptType.DATATYPE_PROPERTY] :
					OntConceptType.DATATYPE_PROPERTY
					 
				SadlClassOrPropertyDeclaration case e.superElement.referencedSadlResources.exists[ontConceptType === OntConceptType.ANNOTATION_PROPERTY] :
					OntConceptType.ANNOTATION_PROPERTY
					 
				SadlClassOrPropertyDeclaration case e.superElement!==null && e.superElement.isDatatype : 
					OntConceptType.DATATYPE
					
				SadlNecessaryAndSufficient,
				SadlClassOrPropertyDeclaration : 
					OntConceptType.CLASS
					
	//			SadlDataTypeDeclaration :
	//				OntConceptType.DATATYPE
					
				SadlProperty case e.restrictions.filter(SadlRangeRestriction).exists[range.isDatatype]: 
					OntConceptType.DATATYPE_PROPERTY
					
				SadlProperty : 
					OntConceptType.CLASS_PROPERTY
					
				SadlParameterDeclaration :
					OntConceptType.VARIABLE				
					
				SadlInstance,
				SadlCanOnlyBeOneOf,
				SadlValueList,
				SadlMustBeOneOf :
					OntConceptType.INSTANCE
					
				default: OntConceptType.VARIABLE // linking errors and the like
			}
		} finally {
			recursionDetection.get.remove(resource)
		}
	}
	
	def Iterable<? extends SadlResource> getReferencedSadlResources(SadlTypeReference typeRef) {
		if (typeRef === null)
			return #[]
		return switch typeRef {
			SadlPrimitiveDataType : #[]
			SadlSimpleTypeReference : #[typeRef.type]
			SadlIntersectionType : getReferencedSadlResources(typeRef.left) + getReferencedSadlResources(typeRef.right)
			SadlUnionType : getReferencedSadlResources(typeRef.left) + getReferencedSadlResources(typeRef.right)
			default : throw new IllegalStateException("typeRef "+typeRef+" not handled.") 
		}
	}
	
	protected def isDatatype(SadlTypeReference typeRef) {
		typeRef instanceof SadlPrimitiveDataType 
		|| (typeRef != null && typeRef.eAllContents.exists[it instanceof SadlPrimitiveDataType])
		|| (typeRef != null && typeRef.referencedSadlResources.exists[ontConceptType === OntConceptType.DATATYPE])
	}
	
}
