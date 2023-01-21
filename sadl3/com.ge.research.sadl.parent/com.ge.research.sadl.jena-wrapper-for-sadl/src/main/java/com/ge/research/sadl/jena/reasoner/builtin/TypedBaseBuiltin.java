/************************************************************************
 * Copyright © 2007-2023 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.jena.reasoner.builtin;

import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.reasoner.rulesys.builtins.BaseBuiltin;

import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.FunctionSignature;
import com.ge.research.sadl.reasoner.TranslationException;
import com.naturalsemanticsllc.sadl.reasoner.ITypedBuiltinFunctionHelper.UnittedQuantityBuiltinHandlingType;

public abstract class TypedBaseBuiltin extends BaseBuiltin implements ITypedBaseBuiltin {
	
	public FunctionSignature getFunctionSignature() {
		return new FunctionSignature(getFunctionSignatureString(), this.getURI());
	}
	@Override
	public boolean canProcessListArgument() {
		return false;
	}

	@Override
	public boolean canProcessUnittedQuantityArguments() {
		return false;
	}
	
	@Override
	public boolean canProcessGraphPatternArguments() {
		return false;
	}

	@Override
	public UnittedQuantityBuiltinHandlingType getUnittedQuantityProcessingConstraint() {
		return UnittedQuantityBuiltinHandlingType.UnitsNotSupported;
	}
	
	@Override
	public com.ge.research.sadl.model.gp.Node[] validateArgumentTypes(OntModel model, BuiltinElement be,
			List<com.ge.research.sadl.model.gp.Node> args, List<com.ge.research.sadl.model.gp.Node> argTypes) throws TranslationException {
		be.setCanProcessListArgument(canProcessListArgument());
		be.setCanProcessUnittedQuantityArguments(canProcessUnittedQuantityArguments());
		be.setCanprocessGraphPatternArguments(canProcessGraphPatternArguments());
		be.setUnittedQuantityProcessingCapability(getUnittedQuantityProcessingConstraint());
		return null;
	}
}
