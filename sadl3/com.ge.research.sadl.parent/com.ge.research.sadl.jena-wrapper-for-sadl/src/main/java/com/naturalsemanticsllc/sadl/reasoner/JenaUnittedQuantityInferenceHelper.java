/************************************************************************
 * Copyright © 2022 - Natural Semantics, LLC. All Rights Reserved.
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
 ***********************************************************************/package com.naturalsemanticsllc.sadl.reasoner;

import org.apache.jena.graph.Node;
import org.apache.jena.reasoner.rulesys.RuleContext;

import com.ge.research.sadl.jena.reasoner.builtin.GeUtils;
import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper;
import com.ge.research.sadl.reasoner.UnittedQuantityHandlerException;
import com.ge.research.sadl.reasoner.UnittedQuantityUnitMismatchException;

public class JenaUnittedQuantityInferenceHelper implements IUnittedQuantityInferenceHelper {
	
	@Override
	public Object combineUnits(Object reasonerContext, Object operator, Object arg1Units, Object arg2Units) throws UnittedQuantityHandlerException {
		if (!(operator instanceof Node)) {
			throw new UnittedQuantityHandlerException("The operator is not a Node " + operator.getClass().getCanonicalName());
		}
		String opStr = ((Node)operator).getLiteralValue().toString();
		if (opStr.equals("*") || opStr.equals("/")) {
			if (arg1Units == null && arg2Units != null) {
				if (arg2Units instanceof Node) {
					return ((Node) arg2Units).getLiteralValue();
				}
			}
			else if (arg2Units == null && arg1Units !=  null) {
				if (arg1Units instanceof Node) {
					return ((Node)arg1Units).getLiteralValue();
				}
			}
		}
		if (!(arg1Units instanceof Node)) {
			throw new UnittedQuantityHandlerException("First argument is not a Node " + arg1Units.getClass().getCanonicalName());
		}
		if (!(arg2Units instanceof Node)) {
			throw new UnittedQuantityHandlerException("Second argument is not a Node " + arg2Units.getClass().getCanonicalName());
		}
		StringBuilder sb = new StringBuilder();
		Object v2 = ((Node) arg1Units).getLiteralValue();
		sb.append(v2.toString());
		Object v1 = ((Node) operator).getLiteralValue();
		sb.append(v1.toString());
		Object v3 = ((Node) arg2Units).getLiteralValue();
		sb.append(v3.toString());
		return sb.toString();
	}

	@Override
	public boolean validateOperation(Object reasonerContext, BuiltinUnittedQuantityStatus operatorType, Object arg1Units, Object arg2Units)
			throws UnittedQuantityHandlerException {
		if (!(reasonerContext instanceof RuleContext)) {
			throw new UnittedQuantityHandlerException("The reasoner context is not a Jena RuleContext: " + reasonerContext.getClass().getCanonicalName());
		}
		if (!(arg1Units instanceof Node)) {
			throw new UnittedQuantityHandlerException("First argument is not a Node: " + arg1Units.getClass().getCanonicalName());
		}
		if (!(arg2Units instanceof Node)) {
			throw new UnittedQuantityHandlerException("Second argument is not a Node: " + arg2Units.getClass().getCanonicalName());
		}
		Node unit1 = GeUtils.getUnittedQuantityUnit((Node) arg1Units, (RuleContext) reasonerContext);
		Node unit2 = GeUtils.getUnittedQuantityUnit((Node) arg2Units, (RuleContext) reasonerContext);
		if (operatorType.equals(BuiltinUnittedQuantityStatus.SameUnitsRequired)) {
// TODO need to add Average, Max, Min
			if(!unit1.equals(unit2)) {
				throw new UnittedQuantityUnitMismatchException("'+' operation requires arguments to have the same units (" + unit1.toString() + "!= " + unit2.toString() + ")");
			}
		}
		else if (operatorType.equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly)) {
			if (unit1 == null) {
				throw new UnittedQuantityUnitMismatchException("'+' operation requires at least the first argument to have the same units");
			}
		}

		return true;
	}

	@Override
	public Object[] performUnitConversions(Object operator, Number arg1Value, Object arg1Units, Number arg2Value,
			Object arg2Units) throws UnittedQuantityHandlerException {
		throw new UnittedQuantityHandlerException("This handler does not support unit conversion");
	}

	@Override
	public boolean handlerSupportsUnitConversions(Object operator, Object arg1Units, Object arg2Units) {
		return false;
	}


}
