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

package com.ge.research.sadl.jena.reasoner.builtin;

import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.reasoner.rulesys.Util;

import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.reasoner.TranslationException;
import com.naturalsemanticsllc.sadl.reasoner.ITypedBuiltinFunctionHelper.UnittedQuantityBuiltinHandlingType;

public class Acos extends TypedBaseBuiltin {

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "acos";
    }
    
    /**
     * Return the expected number of arguments for this functor or 0 if the number is flexible.
     */
    public int getArgLength() {
        return 2;
    }

    /**
     * This method is invoked when the builtin is called in a rule body.
     * @param args the array of argument values for the builtin, this is an array 
     * of Nodes, some of which may be Node_RuleVariables.
     * @param length the length of the argument list, may be less than the length of the args array
     * for some rule engines
     * @param context an execution context giving access to other relevant data
     * @return return true if the buildin predicate is deemed to have succeeded in
     * the current environment
     */
    public boolean bodyCall(Node[] args, int length, RuleContext context) {
        checkArgs(length, context);
        BindingEnvironment env = context.getEnv();
        Node n1 = getArg(0, args, context);
         if (n1.isLiteral()) {
            Object v1 = n1.getLiteralValue();
            Node acos = null;
            if (v1 instanceof Number) {
                Number nv1 = (Number)v1;
               	double absd = Math.acos(nv1.doubleValue());
                acos = Util.makeDoubleNode(absd);
                return env.bind(args[1], acos);
            }
        }
        // Doesn't (yet) handle partially bound cases
        return false;
    }

	@Override
	public String getFunctionSignatureString() {
		return "acos(decimal)decimal";
	}

	@Override
	public UnittedQuantityBuiltinHandlingType getUnittedQuantityProcessingConstraint() {
		return UnittedQuantityBuiltinHandlingType.UnitsNotSupported;
	}

	@Override
	public boolean canProcessListArgument() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public com.ge.research.sadl.model.gp.Node[] validateArgumentTypes(OntModel model, BuiltinElement be,
			List<com.ge.research.sadl.model.gp.Node> args, List<com.ge.research.sadl.model.gp.Node> argTypes) throws TranslationException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int numOutputArgs() {
		return 1;
	}

}
