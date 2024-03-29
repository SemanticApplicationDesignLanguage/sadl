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

package com.naturalsemantics.sadl.jena.reasoner.builtin;

import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.impl.LiteralLabelFactory;
import org.apache.jena.reasoner.rulesys.BindingEnvironment;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.RuleContext;

import com.ge.research.sadl.jena.reasoner.builtin.CancellableBuiltin;
import com.ge.research.sadl.jena.reasoner.builtin.GeUtils;
import com.ge.research.sadl.jena.reasoner.builtin.utils.Utils;

/**

 */
public class CombineUnits extends CancellableBuiltin {

	public static final String methodName = "combineUnits";
	private int argLength = 4;
	
    /**
     * Return the expected number of arguments for this functor or 0 if the number is flexible.
     */
    public int getArgLength() {
        return argLength;
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
        /*
         * Arguments should be:
         *  1) the binary operator
         *  2) the units of the 1st argument
         *  3) the units of the 2nd argument
         *  4) the Node_RuleVariable used to return the combined units
         *  
         *  OR
         *  
         *  1) the operator 
         *  2) the list of operands
         *  3) the type of the operands (not used)
         *  4) the Node_RuleVariable used to return the combined units
         */
        
        Node n1 = getArg(0, args, context);
        Node n2 = getArg(1, args, context);
        Node n3 = getArg(2, args, context);
        
        List<Node> n2List = null;
        if (!n1.isLiteral()) {
			throw new BuiltinException(this, context, "Expected 1st argument of combineUnits to be a literal but was: " + n1.toString());
        }
        if (!n2.isLiteral()) {
             n2List = GeUtils.getListItems(context.getGraph(), null, n2);
            if (n2List != null) {
//                JenaUnittedQuantityInferenceHelper juqih = new JenaUnittedQuantityInferenceHelper();
                Node lastUnitNode = null;
                for (Node n : n2List) {
//            		try {
//						if (juqih.isUnittedQuantity(n, context)) {
//							Node unitNode = juqih.getUnittedQuantityUnit(n, context);
//							if (lastUnitNode !=  null) {
//								String units = Utils.combineUnits(context, n1, lastUnitNode, unitNode);
//								Node combinedUnits = NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral( units ));
//								lastUnitNode = combinedUnits;							
//							}
//							else {
//								lastUnitNode = unitNode;
//							}
//						}
//					} catch (TypedBuiltinFunctionException e) {
//						// TODO Auto-generated catch block
//						e.printStackTrace();
//					}
            	}
               	return env.bind(args[length - 1], lastUnitNode);
            }
            else {
            	throw new BuiltinException(this, context, "Expected 2nd argument of combineUnits to be a literal or a list but was: " + n2.toString());
            }
        }
        if (n2List == null && !n3.isLiteral()) {
			throw new BuiltinException(this, context, "Expected 3rd argument of combineUnits to be a literal but was: " + n3.toString());
        }
        String units;
		try {
			units = Utils.combineUnits(context, n1, n2, n3);
		} catch (Exception e) {
			throw new BuiltinException(this, context, "Failed to combine units (" + n1.toString() + ", " + n2.toString() + ", " + n3.toString() + "): " + e.getMessage());
		} 
		Node combinedUnits = NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral( units ));
       	return env.bind(args[length - 1], combinedUnits);
        
    }

	@Override
	public String getName() {
		return methodName;
	}
    
}
