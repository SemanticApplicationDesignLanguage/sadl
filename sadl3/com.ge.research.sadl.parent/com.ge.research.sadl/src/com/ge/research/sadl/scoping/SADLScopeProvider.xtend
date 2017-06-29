/*
 * generated by Xtext 2.9.0-SNAPSHOT
 */
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
package com.ge.research.sadl.scoping

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.BinaryOperation
import com.ge.research.sadl.sADL.EquationStatement
import com.ge.research.sadl.sADL.Expression
import com.ge.research.sadl.sADL.ExternalEquationStatement
import com.ge.research.sadl.sADL.PropOfSubject
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.RuleStatement
import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration
import com.ge.research.sadl.sADL.SadlImport
import com.ge.research.sadl.sADL.SadlInstance
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlMustBeOneOf
import com.ge.research.sadl.sADL.SadlParameterDeclaration
import com.ge.research.sadl.sADL.SadlProperty
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SubjHasProp
import com.ge.research.sadl.sADL.TestStatement
import com.google.common.base.Predicate
import com.google.inject.Inject
import java.util.Map
import java.util.Set
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtend.lib.annotations.Data
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.resource.EObjectDescription
import org.eclipse.xtext.resource.ForwardingEObjectDescription
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.impl.AbstractGlobalScopeDelegatingScopeProvider
import org.eclipse.xtext.scoping.impl.MapBasedScope
import org.eclipse.xtext.util.OnChangeEvictingCache
import org.eclipse.emf.ecore.util.EcoreUtil

/**
 * This class contains custom scoping description.
 * 
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#scoping
 * on how and when to use it.
 */
class SADLScopeProvider extends AbstractGlobalScopeDelegatingScopeProvider {

	@Inject extension DeclarationExtensions
	@Inject OnChangeEvictingCache cache
	@Inject IQualifiedNameConverter converter
	
	boolean ambiguousNameDetection;
	
	override getScope(EObject context, EReference reference) {
		val ctxrsrc = context.eResource();
		setAmbiguousNameDetection(TestScopeProvider.getDetectAmbiguousNames(ctxrsrc));
		// resolving imports against external models goes directly to the global scope
		if (reference.EReferenceType === SADLPackage.Literals.SADL_MODEL) {
			return super.getGlobalScope(context.eResource, reference)
		}
		if (SADLPackage.Literals.SADL_RESOURCE.isSuperTypeOf(reference.EReferenceType)) {
			val result = getSadlResourceScope(context, reference)
			return result
		}
		throw new UnsupportedOperationException(
			"Couldn't build scope for elements of type " + reference.EReferenceType.name)
	}
	
	def setAmbiguousNameDetection(boolean bval) {
		ambiguousNameDetection = bval
	}

	protected def IScope getSadlResourceScope(EObject context, EReference reference) {
		val parent = createResourceScope(context.eResource, null, newHashSet)
		
		val rule = EcoreUtil2.getContainerOfType(context, RuleStatement)
		if (rule !== null) {
			return getLocalVariableScope(rule.ifs + rule.thens, parent)
		}
		val equation = EcoreUtil2.getContainerOfType(context, EquationStatement)
		if (equation !== null) {
			return MapBasedScope.createScope(parent, 
				equation.parameter.map[EObjectDescription.create(name.concreteName, it.name)])
		}
		val ask = EcoreUtil2.getContainerOfType(context, QueryStatement)
		if (ask?.expr !== null) {
			return getLocalVariableScope(ask.expr, parent)
		}
		val test = EcoreUtil2.getContainerOfType(context, TestStatement)
		if (test?.tests !== null) {
			return getLocalVariableScope(test.tests, parent)
		}
		return parent
	}
	
	protected def IScope getLocalVariableScope(Iterable<Expression> expressions, IScope parent) {
		if (expressions.empty)
			return parent;
		var newParent = doGetLocalVariableScope(expressions, parent) [
			var container = eContainer
			if (container instanceof PropOfSubject || container instanceof SubjHasProp) {
				container = container.eContainer
			}
			if (container instanceof BinaryOperation) {
				if (container.op == 'is' || container.op == '==' || container.op == '=') 
					return true
			}
			return false
		]
		return doGetLocalVariableScope(expressions, newParent) [true]
	}
	
	protected def IScope getLocalVariableScope(Expression expression, IScope parent) {
		var newParent = doGetLocalVariableScope(expression, parent) [
			var container = eContainer
			if (container instanceof PropOfSubject || container instanceof SubjHasProp) {
				container = container.eContainer
			}
			if (container instanceof BinaryOperation) {
				if (container.op == 'is' || container.op == '==' || container.op == '=') 
					return true
			}
			return false
		]
		return doGetLocalVariableScope(expression, newParent) [true]
	}
	
	protected def IScope doGetLocalVariableScope(Iterable<Expression> expressions, IScope parent, Predicate<SadlResource> predicate) {
		 if (expressions.empty)
			return parent;
		val map = newHashMap
		for (expression : expressions) {
			val iter = EcoreUtil2.getAllContents(expression, false).filter(SadlResource).filter(predicate)
			while (iter.hasNext) {
				val name = iter.next
				val concreteName = name.concreteName
				if (concreteName !== null) {
					val qn = QualifiedName.create(concreteName)
					if (!map.containsKey(qn) && parent.getSingleElement(qn) === null) {
						map.put(qn, new EObjectDescription(qn, name, emptyMap))
					}
				}
			}
		}
		return MapBasedScope.createScope(parent, map.values)
	}
	
	
	protected def IScope doGetLocalVariableScope(Expression expression, IScope parent, Predicate<SadlResource> predicate) {
		val map = newHashMap
		val iter = EcoreUtil2.getAllContents(expression, false).filter(SadlResource).filter(predicate)
		while (iter.hasNext) {
			val name = iter.next
			val concreteName = name.concreteName
			if (concreteName !== null) {
				val qn = QualifiedName.create(concreteName)
				if (!map.containsKey(qn) && parent.getSingleElement(qn) === null) {
					map.put(qn, new EObjectDescription(qn, name, emptyMap))
				}
			}
		}
		return MapBasedScope.createScope(parent, map.values)
	}
	
	@Data static class LocalSymbols {
	}
	
	protected def IScope createResourceScope(Resource resource, String alias, Set<Resource> importedResources) {
		return cache.get('resource_scope' -> alias, resource) [
			val shouldWrap = importedResources.empty
			if (!importedResources.add(resource)) {
				return IScope.NULLSCOPE
			}
			
			var newParent = createImportScope(resource, importedResources)
			if (shouldWrap)
				newParent = wrap(newParent)
			val aliasToUse = alias ?: resource.getAlias
			val namespace = if (aliasToUse!==null) QualifiedName.create(aliasToUse) else null
			newParent = getLocalScope1(resource, namespace, newParent)
			newParent = getLocalScope2(resource, namespace, newParent)
			newParent = getLocalScope3(resource, namespace, newParent)
			newParent = getLocalScope4(resource, namespace, newParent)
			// finally all the rest
			newParent = internalGetLocalResourceScope(resource, namespace, newParent) [true]
			return newParent
		]
	}
	
	protected def getLocalScope4(Resource resource, QualifiedName namespace, IScope parentScope) {
		return internalGetLocalResourceScope(resource, namespace, parentScope) [
			if (it instanceof SadlResource) {
				return eContainer instanceof SadlMustBeOneOf && eContainingFeature == SADLPackage.Literals.SADL_MUST_BE_ONE_OF__VALUES
			} 
			return false
		]
	}
	
	protected def getLocalScope3(Resource resource, QualifiedName namespace, IScope parentScope) {
		return internalGetLocalResourceScope(resource, namespace, parentScope) [
			if (it instanceof SadlResource) {
				return eContainer instanceof SadlInstance && eContainingFeature == SADLPackage.Literals.SADL_INSTANCE__NAME_OR_REF
			} 
			return false
		]
	}
	
	protected def getLocalScope2(Resource resource, QualifiedName namespace, IScope parentScope) {
		return internalGetLocalResourceScope(resource, namespace, parentScope) [
			if (it instanceof SadlResource) {
				return eContainer instanceof SadlProperty && eContainingFeature == SADLPackage.Literals.SADL_PROPERTY__NAME_OR_REF
					|| eContainer instanceof SadlProperty && eContainingFeature == SADLPackage.Literals.SADL_PROPERTY__NAME_DECLARATIONS
			} 
			return false
		]
	}
	
	protected def getLocalScope1(Resource resource, QualifiedName namespace, IScope parentScope) {
		return internalGetLocalResourceScope(resource, namespace, parentScope) [
			if (it instanceof SadlResource) {
				return eContainer instanceof SadlClassOrPropertyDeclaration && eContainingFeature == SADLPackage.Literals.SADL_CLASS_OR_PROPERTY_DECLARATION__CLASS_OR_PROPERTY
					|| eContainer instanceof SadlProperty && (eContainer as SadlProperty).isPrimaryDeclaration() && eContainingFeature == SADLPackage.Literals.SADL_PROPERTY__NAME_OR_REF
					
			} 
			return false
		]
	}
	
	def IScope internalGetLocalResourceScope(Resource resource, QualifiedName namespace, IScope parentScope, Predicate<EObject> isIncluded) {
		val map = <QualifiedName, IEObjectDescription>newHashMap
		val iter = resource.allContents
		while (iter.hasNext) {
			val it = iter.next
			if (isIncluded.apply(it)) {
				switch it {
					SadlResource case concreteName !== null: {
						val name1 = converter.toQualifiedName(concreteName)
						if (parentScope.getSingleElement(name1) === null) {
							map.addElement(name1, it)
						}
						val name2 = if (name1.segments.size==1) namespace?.append(name1) else name1.skipFirst(1)
						if (name2 !== null && parentScope.getSingleElement(name2) === null) {
							map.addElement(name2, it)
						}
					}
					EquationStatement: {
						val name = converter.toQualifiedName(it.name.concreteName)
						map.addElement(name, it.name)
						if (name.segmentCount > 1) {
							map.addElement(name.skipFirst(1), it.name)
						} else if (namespace !== null) {
							map.addElement(namespace.append(name), it.name)
						}
					}
					QueryStatement: {
						// Ignore `anonymous` query statements. Nothing to put into the scope.
						if (it?.name?.concreteName !== null) {
							val name = converter.toQualifiedName(it.name.concreteName)
							map.addElement(name, it.name)
							if (name.segmentCount > 1) {
								map.addElement(name.skipFirst(1), it.name)
							} else if (namespace !== null) {
								map.addElement(namespace.append(name), it.name)
							}
							// Make sure we do not expose the parameters from the query expression to the scope.
							// Stop processing the subtree of the current AST element by pruning the iterator.
							// For instance, we do not let `c` into the scope. 
							// `C is a class. Ask myQuery: select c where c is a C.` 
							iter.prune
						}
					}
					RuleStatement: {
						if (it?.name?.concreteName !== null) {
							val name = converter.toQualifiedName(it.name.concreteName)
							map.addElement(name, it.name)
							if (name.segmentCount > 1) {
								map.addElement(name.skipFirst(1), it.name)
							}
							else if (namespace !== null) {
								map.addElement(namespace.append(name), it.name)
							}
							iter.prune
						}
					}
					default :
						if (pruneScope(it)) {
							iter.prune
						}
				}
			}
		}
		return MapBasedScope.createScope(parentScope, map.values)
	}
	
	protected def pruneScope(EObject object) {
		return object instanceof RuleStatement || object instanceof EquationStatement || object instanceof QueryStatement || object instanceof TestStatement
	}
	
	protected def String getAlias(Resource resource) {
		(resource.contents.head as SadlModel).alias
	}
	
	protected def IScope createImportScope(Resource resource, Set<Resource> importedResources) {
		val imports = resource.contents.head.eContents.filter(SadlImport).toList.reverseView
		val importedSymbols = <QualifiedName, IEObjectDescription>newHashMap
		for (imp : imports) {
			val externalResource = imp.importedResource
			if (externalResource !== null && !externalResource.eIsProxy) {
				createResourceScope(externalResource.eResource, imp.alias, importedResources).allElements.forEach[
					val existing = importedSymbols.put(name, it)
					val duplicateProblem = checkDuplicate(existing, it)
					if (duplicateProblem !== null) {
						importedSymbols.put(duplicateProblem.name, duplicateProblem)
					}
				]
			}
				
		}
		if (importedSymbols.isEmpty) {
			if (!resource.URI.toString.endsWith("SadlImplicitModel.sadl")) {
				val element = getGlobalScope(resource, SADLPackage.Literals.SADL_IMPORT__IMPORTED_RESOURCE).getSingleElement(QualifiedName.create("http://sadl.org/sadlimplicitmodel"))
				if (element !== null) {
					val eobject = resource.resourceSet.getEObject(element.EObjectURI, true)
					if (eobject !== null) {
						createResourceScope(eobject.eResource, null, importedResources).allElements.forEach[
							importedSymbols.put(name, it)
						]
					}
				}
			}
		}
		
		if (!resource.URI.toString.endsWith("SadlBuiltinFunctions.sadl")) {
			val element = getGlobalScope(resource, SADLPackage.Literals.SADL_IMPORT__IMPORTED_RESOURCE).getSingleElement(QualifiedName.create("http://sadl.org/builtinfunctions"))
			if (element !== null) {
				val eobject = resource.resourceSet.getEObject(element.EObjectURI, true)
				if (eobject !== null) {
					createResourceScope(eobject.eResource, null, importedResources).allElements.forEach[
						importedSymbols.put(name, it)
					]
				}
			}
		}
		
		return new MapScope(IScope.NULLSCOPE, importedSymbols, false)
	}
	
	def private IEObjectDescription checkDuplicate(IEObjectDescription first, IEObjectDescription second) {
		if (!ambiguousNameDetection || first === null || second === null
			|| EcoreUtil.getURI(first.EObjectOrProxy) == EcoreUtil.getURI(second.EObjectOrProxy)) {
			return null
		}
		val imports = #[first, second].map[EObjectOrProxy.eResource.allContents.filter(SadlModel).head.baseUri]
		val message = '''Ambiguously imported name '«first.name»' from «imports.map["'"+it+"'"].join(", ")». Please use an alias or choose different names.'''
		
		return new ForwardingEObjectDescription(first) {
			override getUserData(String key) {
				if (key.equals(ErrorAddingLinkingService.ERROR)) {
					return message
				}
				if (key.equals(ErrorAddingLinkingService.ALTERNATIVES)) {
					return first.qualifiedName+","+second.qualifiedName
				}
				super.getUserData(key)
			}
		}
	}

	static class MapScope extends MapBasedScope {
	
		new(IScope parent, Map<QualifiedName, IEObjectDescription> elements, boolean ignoreCase) {
			super(parent, elements, ignoreCase)
		}
		
	}
	
	private def void addElement(Map<QualifiedName, IEObjectDescription> scope, QualifiedName qn, EObject obj) {

		if (obj instanceof SadlResource) {
			// Do not put parameters of external and local equation statements into the scope.
			if (obj.eContainer instanceof SadlParameterDeclaration) {
				val declaration = obj.eContainer as SadlParameterDeclaration;
				val container = declaration.eContainer;
				if (container instanceof ExternalEquationStatement || container instanceof EquationStatement) {
					return;
				}
			} else if (EcoreUtil2.getContainerOfType(obj, BinaryOperation) !== null) {
				// Also filter out resources from the expression of any equations.
				return;
			} else if (obj.eContainer instanceof QueryStatement) {
				// The SADL resource from the use-site should not go into the scope.
				// In such cases the statement does not have an expression.
				val queryStatement = obj.eContainer as QueryStatement
				if (queryStatement.expr === null) {
					return;
				}
			}
			else if (obj.eContainer instanceof RuleStatement) {
				val ruleStatement = obj.eContainer as RuleStatement
				if (ruleStatement.thens === null) {
					return;
				}
			}
		}

		if (!scope.containsKey(qn)) {
			scope.put(qn, new EObjectDescription(qn, obj, emptyMap))
		}
	}
	
}