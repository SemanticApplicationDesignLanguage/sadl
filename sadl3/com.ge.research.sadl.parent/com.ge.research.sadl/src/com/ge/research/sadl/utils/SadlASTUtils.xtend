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
import com.ge.research.sadl.sADL.Declaration
import com.ge.research.sadl.sADL.EquationStatement
import com.ge.research.sadl.sADL.ExpressionStatement
import com.ge.research.sadl.sADL.NumberLiteral
import com.ge.research.sadl.sADL.QueryStatement
import com.ge.research.sadl.sADL.RuleStatement
import com.ge.research.sadl.sADL.SadlResource
import com.ge.research.sadl.sADL.SubjHasProp
import com.ge.research.sadl.sADL.TestStatement
import com.ge.research.sadl.sADL.UnitExpression
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.IScopeProvider

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
			return right === null && !(left instanceof Declaration) && !it.comma && prop.unit;
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
	 * This is called when not doing scoping and calls the main method with null for EReference and IScopeProvider
	 */
	static def boolean isUnit(EObject it) {
		return isUnit(it, null, null);
	}


	/**
	 * {@code true} if the argument is a SADL resource which represents a non-quoted unit in a unit expression like construct.
	 * Otherwise, {@code false}.
	 */
	static def boolean isUnit(EObject it, EReference ref, IScopeProvider scopeProvider) {
		if (it instanceof SadlResource && eContainer instanceof SubjHasProp) {
			val iTlName = declarationExtensions.getConcreteName(it as SadlResource)
			if (!isInExpressionGrammar) return false	// the SubjHasProp unit statement only exists in the expression grammar			
			val container = eContainer as SubjHasProp
			if (isInEquationStatement && container.left instanceof Declaration) {
				// in equation bodies there can be constructs of the form "a Someclass sc"
				return false
			}
//			val propLName = container.prop instanceof SadlResource ? declarationExtensions.getConcreteName(container.prop as SadlResource) : null
			if (eContainingFeature === SUBJ_HAS_PROP__PROP && container.right === null) {
				if (it.equals(container.prop)) {
					if (scopeProvider !== null) {
						val IScope scope = scopeProvider.getScope(it, ref)
						if (scope !== null) {
							val elements = scope.allElements
							var boolean firstEncounterMatch = false
							for (element : elements) {
								val qn = (element as IEObjectDescription).qualifiedName
								var String qnln
								var int segCnt
								if (qn.segmentCount == 1) {
									if (qn.firstSegment.indexOf(':') > 0 ) {
										qnln = qn.firstSegment.substring(qn.firstSegment.indexOf(':') + 1)
										segCnt = 2
									}
									else {
										qnln = qn.firstSegment
										segCnt = 1
									}
								}
								else {
									qnln = qn.getSegment(qn.segmentCount - 1)
									segCnt = 2
								}
									
								if (qnln.equals(iTlName)) {
									if (!firstEncounterMatch && segCnt == 1) {
										// this is the first encounter has no prefix
										firstEncounterMatch = true
									}
									else if (firstEncounterMatch) {	
										if (segCnt > 1) {
											// segment count of > 1 means that it is defined outside of this statement
											return false
										}
										else {
											return true
										}
									}
								}
							}
							return true
						}
						return false	// this shouldn't happen....
					}
					else {
						val SadlResource decl = declarationExtensions.getDeclaration(it as SadlResource)
						if (decl.equals(it)) {
							return true
						}
					}
					return false;
				}
				var result = true
				return result
			}
		}
		return false
	}

	private static def isInExpressionGrammar(EObject it) {
		if (isInRuleStatement) { return true }
		if (isInQueryStatement) { return true }
		if (isInEquationStatement) { return true }
		if (isInExpressionStatement) { return true }
		if (isInTestStatement) { return true }
		return false
	}
	
	private static def isInQueryStatement(EObject it) {
		return getContainerOfType(QueryStatement) !== null;
	}
	
	private static def isInEquationStatement(EObject it) {
		return getContainerOfType(EquationStatement) !== null;
	}
	
	private static def isInRuleStatement(EObject it) {
		return getContainerOfType(RuleStatement) !== null;
	}

	private static def isInExpressionStatement(EObject it) {
		return getContainerOfType(ExpressionStatement) !== null;
	}
	
	private static def isInTestStatement(EObject it) {
		return getContainerOfType(TestStatement) !== null;
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
