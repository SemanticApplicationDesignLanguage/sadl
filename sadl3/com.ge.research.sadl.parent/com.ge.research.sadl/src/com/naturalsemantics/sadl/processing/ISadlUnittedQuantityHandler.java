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
 ***********************************************************************/
package com.naturalsemantics.sadl.processing;

import java.util.List;

import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.processing.I_IntermediateFormTranslator;
import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.BuiltinUnittedQuantityStatus;
import com.ge.research.sadl.reasoner.TranslationException;

public interface ISadlUnittedQuantityHandler {
	/**
	 * Method to determine the built-in UnittedQuantity status for common functions for use in expansion when
	 * the built-in itself doesn't have this information.
	 * @param be
	 * @return
	 */
	default BuiltinUnittedQuantityStatus getBuiltinUnittedQuantityStatusForExpansion(BuiltinElement be) {
		BuiltinType bit = be.getFuncType();
		if (bit.equals(BuiltinType.GT) ||
				bit.equals(BuiltinType.GTE) ||
				bit.equals(BuiltinType.LT) ||
				bit.equals(BuiltinType.LTE) ||
				bit.equals(BuiltinType.Equal) ||
				bit.equals(BuiltinType.NotEqual) ||
				bit.equals(BuiltinType.Plus) ||
				bit.equals(BuiltinType.Minus)) {
			return BuiltinUnittedQuantityStatus.SameUnitsRequired;
		}
		else if (bit.equals(BuiltinType.Multiply) ||
				bit.equals(BuiltinType.Divide)) {
			return BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly;
		}
		else if (bit.equals(BuiltinType.Power)) {
			return BuiltinUnittedQuantityStatus.SingleArgument;
		}
		return null;
	}
	
	abstract void setIntermediateFormTranslator(I_IntermediateFormTranslator ift);
	
	/*
	 * Methods used in translation of statements containing references to UnittedQuantity when 
	 * the SADL preference "Expand Unitted Quantities in translation" is checked.
	 *  
	 */
	
	/**
	 * Method to add any combineUnits and there exists constructs to the rule at the end of rule processing.
	 * @param rule
	 * @return
	 * @throws TranslationException 
	 */
	abstract Rule checkRuleForNewUnittedQuantityCreation(Rule rule) throws TranslationException;
	
	/**
	 * Method to expand a binary operation on UnittedQuantity arguments. The TripleElement instances needed to expand 
	 * the UnittedQuantity instances or variables to patterns referencing the UnittedQuantity unit and value property
	 * values are identifed and returned. The new arguments for the value objects to be passed to the binary operator
	 * are put into the BuiltinElement's args list.
	 * @param gpe
	 * @return
	 * @throws TranslationException
	 * */
	abstract List<GraphPatternElement> expandUnittedQuantities(List<GraphPatternElement> patterns, BuiltinElement be,
			boolean isRuleThen) throws TranslationException;
	
	/**
	 * Method to reset the instance of ISadlUnittedQuantityHandler for a new rule, query, or test.
	 */
	abstract void reset();
	
	/*
	 * Methods used in inference (called by Jena built-in functions) when
	 * the SADL preference "Expand Unitted Quantities in translation" is not checked
	 * are invoked by Java reflection so the methods are in the specific implementation classes.
	 */
	
	/**
	 * Method to get the name of the implementation of IUnittedQuantityInferenceHelper Interface class
	 * to be used during inference to handle UnittedQuantity arguments to built-in functions.
	 * @return
	 */
	abstract String getUnittedQuantityInferenceHelperClassname();
}
