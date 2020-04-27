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

import com.ge.research.sadl.ValueConverterService
import com.ge.research.sadl.external.ExternalEmfResource
import com.ge.research.sadl.external.ExternalResourceAdapter
import com.ge.research.sadl.sADL.EquationStatement
import com.ge.research.sadl.sADL.ExternalEquationStatement
import com.ge.research.sadl.sADL.Name
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.RuleStatement
import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlCanOnlyBeOneOf
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration
import com.ge.research.sadl.sADL.SadlInstance
import com.ge.research.sadl.sADL.SadlIntersectionType
import com.ge.research.sadl.sADL.SadlIsAnnotation
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlMustBeOneOf
import com.ge.research.sadl.sADL.SadlNecessaryAndSufficient
import com.ge.research.sadl.sADL.SadlParameterDeclaration
import com.ge.research.sadl.sADL.SadlPrimitiveDataType
import com.ge.research.sadl.sADL.SadlProperty
import com.ge.research.sadl.sADL.SadlPropertyCondition
import com.ge.research.sadl.sADL.SadlRangeRestriction
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SadlSimpleTypeReference
import com.ge.research.sadl.sADL.SadlTableDeclaration
import com.ge.research.sadl.sADL.SadlTypeReference
import com.ge.research.sadl.sADL.SadlUnionType
import com.ge.research.sadl.sADL.SadlValueList
import com.ge.research.sadl.sADL.UpdateStatement
import com.ge.research.sadl.scoping.SadlQualifiedNameConverter
import com.google.common.base.Supplier
import com.google.inject.Inject
import java.util.HashSet
import java.util.Set
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.util.internal.EmfAdaptable

class DeclarationExtensions {
	
	@Inject ValueConverterService.QNameConverter converter
	
	/**
	 * Returns with the concrete name of the SADL resource argument. Any leading prefixes will be removed
	 * from the name.
	 * <p>
	 * This method is equivalent with calling {@link getConcreteName(SadlResource, true)}.
	 */
	def String getConcreteName(SadlResource it) {
		return getConcreteName(it, true);
	}
	
	/**
	 * Unlike {@link #getConcreteName(SadlResource)} this can be configured, whether the any leading prefixes
	 * has to be trimmed from the concrete name or not. Let assume the following SADL model:
	 * 
	 * <pre>
	 * uri "http://sadl.org/Current.sadl" alias current.
	 * current:Foo is a class.
	 * </pre>
	 * Then
	 * <pre>
	 * val extensions = // ...
	 * val resource = // ...
	 * 
	 * println(extensions.getConcreteName(resource)); // Foo
	 * println(extensions.getConcreteName(resource, true)); // Foo
	 * println(extensions.getConcreteName(resource, false)); // current:Foo
	 * </pre>
	 * 
	 * @param it the SADL resource who's name we are looking for.
	 * @param trimPrefix when {@code true} any leading prefixes (if any) will be omitted from the result.
	 */
	def String getConcreteName(SadlResource it, boolean trimPrefix) {
		if (isExternal) {
			return getExternalResourceAdapter.concreteName;
		}
		val nameAdapter = NewNameAdapter.findInEmfObject(it);
		if (nameAdapter !== null && !nameAdapter.name.nullOrEmpty) {
			return nameAdapter.name;
		}
		val resource = it.eResource as XtextResource
		val nameSupplier = getConcreteNameSupplier(it, resource, trimPrefix);
		if (resource === null) {
			return nameSupplier.get;
		}
		return resource.cache.get(it -> '''concreteName[trimPrefix«trimPrefix»]''', eResource, [nameSupplier.get])
	}
	
	protected def Supplier<String> getConcreteNameSupplier(SadlResource it, Resource resource, boolean trimPrefix) {
		return [
			val nodes = findNamedNodes;
			var name = nodes.map[NodeModelUtils.getTokenText(it)].join('').trim;
			if (name.isNullOrEmpty) {
				return null;
			}
			if (trimPrefix) {
				val index = name.lastIndexOf(SadlQualifiedNameConverter.SEGMENT_SEPARATOR);
				if (index !== -1) {
					val ()=>String aliasSupplier = [
						EcoreUtil2.getContainerOfType(it, SadlModel)?.alias;
					];
					val alias = if (resource instanceof XtextResource) {
						resource.cache.get(it -> 'alias', resource, aliasSupplier);
					} else {
						aliasSupplier.apply;
					};
					if (alias == name.substring(0, index) && name.length >= (index + 1)) {
						name = name.substring(index + 1);
					}
				}
			}
			// this will be null when a resource is open in the editor and a clean/build is performed ??
			// And if the extensions instance is not injected into the context but instantiated via its constructor. 
			if (converter === null) {
				return name;
			}
			return converter.toValue(name, null);
		];
	} 

	private def dispatch findNamedNodes(SadlResource it) {
		return NodeModelUtils.findNodesForFeature(it, SADLPackage.Literals.SADL_RESOURCE__NAME);
	}

	private def dispatch findNamedNodes(Name it) {
		val node = NodeModelUtils.getNode(it) as INode;
		return if(node === null) emptyList else #[node];
	}
 
	def String getConceptUri(SadlResource it) {
		if (isExternal) {
			return getExternalResourceAdapter.conceptUri
		}
		val declaration = declaration
		if (declaration !== null) {	
			val part1 = EcoreUtil2.getContainerOfType(declaration, SadlModel)
			if (part1 !== null) {
				val part2 = part1.baseUri
				if (part2 !== null) {
					return part2 +"#"+declaration.concreteName
				}
			}
		}
		return null
	}
	
	def String getConceptQualifiedName(SadlResource it) {
		val declaration = declaration
		if (declaration !== null) {	
			val concreteName = declaration.concreteName
			if (concreteName.indexOf(':') > 0) {
				return concreteName
			}
			val part1 = EcoreUtil2.getContainerOfType(declaration, SadlModel)
			if (part1 !== null) {
				val part2 = part1.alias
				if (part2 !== null) {
					return part2 +":"+concreteName
				}
			}
		}
		return null
	}
	
	def SadlResource getDeclaration(SadlResource resource) {
		if (resource !== null && resource.name !== null && !resource.name.eIsProxy) {
			return resource.name
		}
		return resource
	}
	
	def String getConceptNamespace(SadlResource it) {
		val declaration = declaration
		if (declaration !== null) {
			val part1 = EcoreUtil2.getContainerOfType(declaration, SadlModel)
			if (part1 !== null) {
				val part2 = part1.baseUri
				if (part2 !== null) {
					return part2 + "#"
				}
			}
		}
		return null
	}
	
	def String getConceptPrefix(SadlResource it) {
		val declaration = declaration
		if (declaration !== null) {	
			val part1 = EcoreUtil2.getContainerOfType(declaration, SadlModel)
			if (part1 !== null) {
				val part2 = part1.alias
				if (part2 !== null) {
					return part2
				}
			}
		}
		return null
	}
	
	ThreadLocal<Set<SadlResource>> recursionDetection = new ThreadLocal<Set<SadlResource>>();
	
	def OntConceptType getOntConceptType(SadlResource resource) throws CircularDefinitionException {
		if (resource.isExternal) {
			return resource.getExternalResourceAdapter.type
		}
		if (recursionDetection.get === null) {
			recursionDetection.set(new HashSet)
		}
		if (!recursionDetection.get.add(resource)) {
			// Recursion detected. Should be an error.
//			return OntConceptType.CLASS
			throw new CircularDefinitionException("Concept is not properly defined", OntConceptType.CLASS)
		}
		try {
			if (resource instanceof Name) {
				if (resource.eContainer instanceof QueryStatement) {
					return OntConceptType.INSTANCE
				}
				else if (resource.function) {
					return OntConceptType.FUNCTION_DEFN
				}
				else if (resource.name !== null && (resource !== resource.name)) {
					return getOntConceptType(resource.name)
				}
				return OntConceptType.VARIABLE
			}
			
			switch e: resource.declaration?.eContainer {
				
				EquationStatement, 
				ExternalEquationStatement :
					OntConceptType.FUNCTION_DEFN
					
				SadlClassOrPropertyDeclaration case e.restrictions.exists[it instanceof SadlIsAnnotation],
				SadlProperty case e.restrictions.exists[it instanceof SadlIsAnnotation]:
					OntConceptType.ANNOTATION_PROPERTY
					
				SadlClassOrPropertyDeclaration case e.superElement.referencedSadlResources.exists[ontConceptType === OntConceptType.CLASS_PROPERTY]:
					OntConceptType.CLASS_PROPERTY
					
				SadlClassOrPropertyDeclaration case e.superElement.referencedSadlResources.exists[ontConceptType === OntConceptType.CLASS_PROPERTY]:
					OntConceptType.CLASS_PROPERTY
				
				SadlClassOrPropertyDeclaration case e.superElement.referencedSadlResources.exists[ontConceptType === OntConceptType.DATATYPE_PROPERTY]:
					OntConceptType.DATATYPE_PROPERTY
					 
				SadlClassOrPropertyDeclaration case e.superElement.referencedSadlResources.exists[ontConceptType === OntConceptType.ANNOTATION_PROPERTY]:
					OntConceptType.ANNOTATION_PROPERTY
					 
				SadlClassOrPropertyDeclaration case e.superElement.isList: 
					if (e.superElement.isDatatype) OntConceptType.DATATYPE_LIST
					else OntConceptType.CLASS_LIST
				
				SadlClassOrPropertyDeclaration case e.superElement!==null && e.superElement.isDatatype: 
					OntConceptType.DATATYPE
					
				SadlClassOrPropertyDeclaration case e.oftype !== null && e.oftype.equals('instances'):
					OntConceptType.INSTANCE
						
				SadlNecessaryAndSufficient,
				SadlClassOrPropertyDeclaration: 
					OntConceptType.CLASS						
					
	//			SadlDataTypeDeclaration :
	//				OntConceptType.DATATYPE
	
	//			if it is a SadlProperty and 
	//				typeonly is class => OntConceptType.CLASS_PROPERTY
	//				typeonly is "data" => OntConceptType.DATATYPE_PROPERTY
	//				typeonly is null && range is null => OntConceptType.RDF_PROPERPTY
	//				conditions below
					
				SadlProperty case e.restrictions.filter(SadlRangeRestriction).exists[typeonly=="class"]: 
					OntConceptType.CLASS_PROPERTY
					
				SadlProperty case e.restrictions.filter(SadlRangeRestriction).exists[typeonly=="data"]: 
					OntConceptType.DATATYPE_PROPERTY
					
				SadlProperty case e.restrictions.filter(SadlRangeRestriction).exists[range.isDatatype]: 
					OntConceptType.DATATYPE_PROPERTY

				SadlProperty case e.restrictions.filter(SadlRangeRestriction).exists[!range.isDatatype]: 
					OntConceptType.CLASS_PROPERTY

				SadlProperty case e.hasFromToRestriction:
					e.inferrConceptTypeFromToType

				SadlProperty: 
					OntConceptType.RDF_PROPERTY
					
				SadlParameterDeclaration:
					OntConceptType.VARIABLE				
					
				SadlInstance,
				SadlCanOnlyBeOneOf,
				SadlValueList,
				SadlMustBeOneOf:
					OntConceptType.INSTANCE
					
				QueryStatement,
				UpdateStatement,
				RuleStatement:
					OntConceptType.STRUCTURE_NAME					
					
				default: {
					if (resource !== null && resource.eResource instanceof XtextResource) {
						val xtextResource = resource.eResource as XtextResource;
						val contribution = xtextResource.resourceServiceProvider.get(IDeclarationExtensionsContribution);
						if (contribution !== null) {
							val ontConceptType = contribution.getOntConceptType(e);
							if (ontConceptType !== null) {
								return ontConceptType;
							}
						}
					}
					OntConceptType.VARIABLE // linking errors and the like
				}
			}
		} finally {
			recursionDetection.get.remove(resource)
		}
	}
	
	/**
	 * Returns with the SADL resource of the `TO` of the given SADL property argument.
	 * Returns {@code null} if the argument is {@code null}, if any of the {@code from} or 
	 * {@code to} type references are not given. Also provides a {@code null} return value
	 * if the {@code to} is not a type of the simple SADL type reference. 
	 */
	private def SadlResource getToType(SadlProperty it) {
		if (it !== null && from !== null && to instanceof SadlSimpleTypeReference) {
			return (to as SadlSimpleTypeReference).type as SadlResource;
		}
		return null;
	}
	
	private def boolean hasFromToRestriction(SadlProperty it) {
		return null !== toType;
	}
	
	private def OntConceptType inferrConceptTypeFromToType(SadlProperty it) {
		return switch (toType.ontConceptType) {
			case CLASS: OntConceptType.CLASS_PROPERTY
			case DATATYPE: OntConceptType.DATATYPE_PROPERTY
			default: OntConceptType.RDF_PROPERTY
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
			SadlPropertyCondition : #[]
			SadlTableDeclaration : #[]
			default : throw new IllegalStateException("typeRef "+typeRef+" not handled.") 
		}
	}
	
	protected dispatch def boolean isList(SadlTypeReference typeRef) {
		return false
	}
	
	protected dispatch def  boolean isList(SadlPrimitiveDataType typeRef) {
		typeRef.list
	}

	protected dispatch def  boolean isList(SadlSimpleTypeReference typeRef) {
		typeRef.list
	}

	protected dispatch def  boolean isList(Void typeRef) {
		return false
	}

	protected def isDatatype(SadlTypeReference typeRef) {
		typeRef instanceof SadlPrimitiveDataType 
		|| (typeRef !== null && typeRef.eAllContents.exists[it instanceof SadlPrimitiveDataType])
		|| (typeRef !== null && typeRef.referencedSadlResources.exists[ontConceptType === OntConceptType.DATATYPE])
	}
	
	def boolean isExternal(SadlResource resource) {
		if (resource === null) return false
		return resource.eResource instanceof ExternalEmfResource
	}
	
	def boolean isProperty(SadlResource resource) {
		if (resource instanceof Name && !(resource as Name).name.equals(resource)) {
			return isProperty((resource as Name).name)
		}
		val octype = getOntConceptType(resource)
		if (octype !== null && 
			(octype.equals(OntConceptType.DATATYPE_PROPERTY) || 
				octype.equals(OntConceptType.CLASS_PROPERTY) || 
				octype.equals(OntConceptType.RDF_PROPERTY) ||
				octype.equals(OntConceptType.ANNOTATION_PROPERTY))) {
					return true;
				}
		return false;
	}
	
	def ExternalResourceAdapter getExternalResourceAdapter(SadlResource resource) {
		ExternalResourceAdapter.findInEmfObject(resource)
	}

	/**
	 * Used by rename/refactoring.
	 */
	@Data
	@EmfAdaptable
	static class NewNameAdapter {
		val String name;
	}
}