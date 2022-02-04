/************************************************************************
 * 
 * Project: SADL
 * Copyright 2007-2022 - General Electric Company, All Rights Reserved
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
/*
 * SADL Extension for SADL Import Template Language (SITL)
 * Copyright 2022 - Natural Semantics, LLC, All Rights Reserved
 */
package com.naturalsemantics.sadl.sitl.scoping;

import com.ge.research.sadl.scoping.SADLScopeProvider;
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.xtext.EcoreUtil2
import com.ge.research.sadl.sADL.ExpressionScope
import com.ge.research.sadl.sADL.SadlModel

/**
 * This class contains custom scoping description.
 * 
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#scoping
 * on how and when to use it.
 */
class SITLScopeProvider extends SADLScopeProvider {

	override protected getSadlResourceScope(EObject obj, EReference reference) {
		val parent = createResourceScope(obj.eResource, null, newHashSet);
		val statement = EcoreUtil2.getContainerOfType(obj, ExpressionScope)
		if (statement !== null) {
			val model = EcoreUtil2.getContainerOfType(statement, SadlModel)
			var newParent = parent
//			for (context : model.elements.filter(RequirementContext)) {
//				if (context !== statement) {
//					newParent = getLocalVariableScope(#[context.expr], newParent)
//				}
//			}
//			if (statement instanceof RequirementContext) {
//				if (statement.expr !== null) {
//					newParent = getLocalVariableScope(#[statement.expr], newParent)
//				}
//			}
//			if (statement instanceof WherePart) {
//				if (statement.where !== null) {
//					newParent = getLocalVariableScope(#[statement.where], newParent)
//				}
//			}
//			if (statement instanceof WithWhenPart) {
//				if (statement.when !== null) {
//					newParent = getLocalVariableScope(#[statement.when], newParent);
//					if (obj.eContainer instanceof BinaryOperation) {
//						val container = obj.eContainer as BinaryOperation;
//						if ((container.op == 'is' || container.op == '=') && container.left == obj) {
//							val importedNamespace = converter.toQualifiedName(statement.name.concreteName);
//							newParent = newParent.doWrap(importedNamespace);
//						}
//					}
//				}
//			}
			return newParent
		}
		return super.getSadlResourceScope(obj, reference)
	}

}
