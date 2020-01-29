/************************************************************************
 * Copyright 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.utils

import com.ge.research.sadl.model.DeclarationExtensions
import com.ge.research.sadl.sADL.EquationStatement
import com.ge.research.sadl.sADL.NumberLiteral
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SubjHasProp
import com.ge.research.sadl.sADL.UnitExpression
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.resource.XtextResource

import static com.ge.research.sadl.sADL.SADLPackage.Literals.*

import static extension org.eclipse.xtext.EcoreUtil2.getContainerOfType

/**
 * Static utility class for SADL AST elements.
 * 
 * For instance, this class can be used to check whether an expression is a comma separated abbreviated expression.
 * Also, it provides some utilities for checking whether a model element represents a unit or not.
 * 
 * This class can be used without injection. 
 * 
 * @author akos.kitta
 */
class SadlASTUtils {

	/**
	 * {@code true} if the SADL model element argument is a comma separated abbreviated expression. Otherwise {@code false}.
	 */
	static def boolean isCommaSeparatedAbbreviatedExpression(EObject it) {
		return if(it instanceof SubjHasProp) comma else false;
	}

	/**
	 * Returns {@code true} if the argument is a unit expression like. More formally, when the argument is an instance of
	 * {@link SubjHasProp}, the left hand side is a number literal, and the 
	 */
	static def boolean isUnitExpression(EObject it) {
		if (it instanceof SubjHasProp) {
			return right === null && !it.comma && prop.unit;
		}
		return it instanceof UnitExpression;
	}

	/**
	 * Returns with the unit from the unit expression like argument as string.
	 * Returns with {@code null} if the argument is *not* a unit expression, more formally,
	 * when {@link #isUnitExpression(EObject)} is {@code false}, or the property is {@code null}.
	 */
	static def String getUnitAsString(EObject it) {
		if (unitExpression) {
			val prop = (it as SubjHasProp).prop;
			return if(prop === null) null else declarationExtensions.getConcreteName(prop);
		} else if (it instanceof UnitExpression) {
			return unit;
		}
		return null;
	}

	/**
	 * Returns with the number literal of the argument, if and only if the argument is unit expression
	 * like. Otherwise, returns {@code null}.
	 */
	static def NumberLiteral getValue(EObject it) {
		if (it instanceof UnitExpression) {
			return it.left as NumberLiteral
		} else if (unitExpression) {
			return (it as SubjHasProp).left as NumberLiteral;
		}
		return null;
	}

	/**
	 * {@code true} if the argument is a SADL resource which represents a non-quoted unit in a unit expression like construct.
	 * Otherwise, {@code false}.
	 */
	static def boolean isUnit(EObject it) {
		if (it instanceof SadlResource && eContainer instanceof SubjHasProp) {
			val container = eContainer as SubjHasProp;
			return eContainingFeature === SUBJ_HAS_PROP__PROP && container.right === null &&
			container.left instanceof NumberLiteral // && declarationExtensions.getConceptUri(container.prop.name) === null
//				!container.inQueryStatement && !container.inEquationStatement;
		}
		return false;
	}

	// TOOD this logic should be much smarter.
	private static def isInQueryStatement(EObject it) {
		return getContainerOfType(QueryStatement) !== null;
	}
	
	private static def isInEquationStatement(EObject it) {
		return getContainerOfType(EquationStatement) !== null;
	}

	/**
	 * Helper for getting the declaration extension in code that does not use Guice.
	 */
	private static def getDeclarationExtensions(EObject it) {
		if (eResource instanceof XtextResource) {
			val serviceProvider = (eResource as XtextResource).resourceServiceProvider;
			return serviceProvider.get(DeclarationExtensions);
		}
		// LOGGER.warn('''Object «it» is not contained in an «XtextResource.simpleName». Creating and using a new «DeclarationExtensions.simpleName» instance.''');
		return new DeclarationExtensions();
	}

}
