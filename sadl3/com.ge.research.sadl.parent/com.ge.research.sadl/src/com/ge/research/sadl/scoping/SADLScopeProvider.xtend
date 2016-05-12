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
import com.ge.research.sadl.sADL.PropOfSubject
import com.ge.research.sadl.sADL.RuleStatement
import com.ge.research.sadl.sADL.SADLPackage
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration
import com.ge.research.sadl.sADL.SadlImport
import com.ge.research.sadl.sADL.SadlInstance
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.sADL.SadlMustBeOneOf
import com.ge.research.sadl.sADL.SadlProperty
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SubjHasProp
import com.google.common.base.Predicate
import com.google.inject.Inject
import java.util.Map
import java.util.Set
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.resource.EObjectDescription
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.impl.AbstractGlobalScopeDelegatingScopeProvider
import org.eclipse.xtext.scoping.impl.MapBasedScope
import org.eclipse.xtext.util.OnChangeEvictingCache

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

	override getScope(EObject context, EReference reference) {
		// resolving imports against external models goes directly to the global scope
		if (reference.EReferenceType === SADLPackage.Literals.SADL_MODEL) {
			return super.getGlobalScope(context.eResource, reference)
		}
		if (SADLPackage.Literals.SADL_RESOURCE.isSuperTypeOf(reference.EReferenceType)) {
			return getSadlResourceScope(context, reference)
		}
		throw new UnsupportedOperationException(
			"Couldn't build scope for elements of type " + reference.EReferenceType.name)
	}

	protected def IScope getSadlResourceScope(EObject context, EReference reference) {
		val parent = createResourceScope(context.eResource, null, IScope.NULLSCOPE, newHashSet)
		val rule = EcoreUtil2.getContainerOfType(context, RuleStatement)
		if (rule !== null) {
			return getLocalVariableScope(rule.ifs + rule.thens, parent)
		}
		val equation = EcoreUtil2.getContainerOfType(context, EquationStatement)
		if (equation !== null) {
			return MapBasedScope.createScope(parent, 
				equation.parameter.map[EObjectDescription.create(name.concreteName, it.name)])
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
	
	
	protected def IScope createResourceScope(Resource resource, String alias, IScope parent, Set<Resource> importedResources) {
		val shouldWrap = importedResources.empty
		if (!importedResources.add(resource)) {
			return parent
		}
		{//cache.get('resource_scope'+alias, resource) [
			var newParent = createImportScope(resource, parent, importedResources)
			if (shouldWrap)
				newParent = wrap(newParent)
			val aliasToUse = alias ?: resource.getAlias
			val namespace = if (aliasToUse!==null) QualifiedName.create(aliasToUse) else null
			newParent = getPrimaryLocalResourceScope(resource, namespace, newParent)
			newParent = getSecondaryLocalResourceScope(resource, namespace, newParent)
			return getTertiaryLocalResourceScope(resource, namespace, newParent)
		}
	}
	
	protected def getTertiaryLocalResourceScope(Resource resource, QualifiedName namespace, IScope parentScope) {
		return internalGetLocalResourceScope(resource, namespace, parentScope) [true]
	}
	
	protected def getSecondaryLocalResourceScope(Resource resource, QualifiedName namespace, IScope parentScope) {
		return internalGetLocalResourceScope(resource, namespace, parentScope) [
			if (it instanceof SadlResource) {
				return eContainer instanceof SadlMustBeOneOf && eContainingFeature == SADLPackage.Literals.SADL_MUST_BE_ONE_OF__VALUES
			} 
			return false
		]
	}
	
	protected def getPrimaryLocalResourceScope(Resource resource, QualifiedName namespace, IScope parentScope) {
		return internalGetLocalResourceScope(resource, namespace, parentScope) [
			if (it instanceof SadlResource) {
				return eContainer instanceof SadlClassOrPropertyDeclaration && eContainingFeature == SADLPackage.Literals.SADL_CLASS_OR_PROPERTY_DECLARATION__CLASS_OR_PROPERTY
					|| eContainer instanceof SadlProperty && (eContainer as SadlProperty).isPrimaryDeclaration() && eContainingFeature == SADLPackage.Literals.SADL_PROPERTY__NAME_OR_REF
					|| eContainer instanceof SadlInstance && eContainingFeature == SADLPackage.Literals.SADL_INSTANCE__NAME_OR_REF
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
					EquationStatement : {
						val name = converter.toQualifiedName(it.name.concreteName)
						map.addElement(name, it.name)
						if (name.segmentCount > 1) {
							map.addElement(name.skipFirst(1), it.name)
						} else if (namespace !== null) {
							map.addElement(namespace.append(name), it.name)
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
		return object instanceof RuleStatement || object instanceof EquationStatement
	}
	
	protected def String getAlias(Resource resource) {
		(resource.contents.head as SadlModel).alias
	}
	
	protected def IScope createImportScope(Resource resource, IScope parent, Set<Resource> importedResources) {
		val imports = resource.contents.head.eContents.filter(SadlImport).toList.reverseView
		var newParent = parent
		for (imp : imports) {
			val externalResource = imp.importedResource
			if (!externalResource.eIsProxy)
				newParent = createResourceScope(externalResource.eResource, imp.alias, newParent, importedResources)
		}
		return newParent
	}

	private def void addElement(Map<QualifiedName, IEObjectDescription> scope, QualifiedName qn, EObject obj) {
		if (!scope.containsKey(qn)) {
			scope.put(qn, new EObjectDescription(qn, obj, emptyMap))
		}
	}

}
