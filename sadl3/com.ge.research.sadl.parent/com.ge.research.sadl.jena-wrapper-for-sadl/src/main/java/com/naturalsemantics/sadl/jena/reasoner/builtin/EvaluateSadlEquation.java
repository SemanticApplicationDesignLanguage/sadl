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

import java.util.ArrayList;
import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.util.iterator.ClosableIterator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.jena.reasoner.builtin.CancellableBuiltin;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.TranslationException;

/**
 * Class to wrap a SADL equation for execution in the Jena built-in environment.
 * 
 * @author Natural Semantics, LLC
 *
 */
public class EvaluateSadlEquation extends CancellableBuiltin {

	@Override
	public String getName() {
		return "evaluateSadlEquation";
	}

    private static final Logger _logger = LoggerFactory.getLogger (EvaluateSadlEquation.class) ;
	private boolean doTransitiveClosure = true;

	/**
     * This method is invoked when the builtin is called in a rule head.
     * Such a use is only valid in a forward rule.
     * Exected args are the instance to be annotated, the property to use and the type
     * of the resulting bNode.
     * @param allArgs the array of argument values for the builtin, this is an array 
     * of Nodes.
     * @param context an execution context giving access to other relevant data
     */
    public void headAction(Node[] allArgs, int length, RuleContext context) {
		checkCanceled(allArgs, context);
		Node method = getArg(0, allArgs, context);
		if (method.isLiteral()) {
			String exturi = method.getLiteralValue().toString();
			List<Node> restOfArgs =  new ArrayList<Node>();
			for (int i = 1; i < allArgs.length; i++) {
				restOfArgs.add(allArgs[i]);
			}
			List<Node>returnTypes = null; 
			(new EvaluateSadlEquationUtils(null)).evaluateSadlEquation(exturi, null, restOfArgs, false, returnTypes);
		}
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
 		Node extEqUri = getArg(0, args, context);
		List<Node> restOfArgs =  new ArrayList<Node>();
		for (int i = 1; i < args.length - 1; i++) {
			restOfArgs.add(args[i]);
		}
		
		if (extEqUri.isLiteral()) {
			String extEqUriStr = extEqUri.getLiteralValue().toString();
			Resource eqInst = ResourceFactory.createResource(extEqUriStr);
			if (eqInst == null) {
				System.err.println("External equation not found in model");
				return false;
			}
	    	Property p = ResourceFactory.createProperty(SadlConstants.SADL_IMPLICIT_MODEL_EXTERNALURL_PROPERTY_URI);
	    	if (p == null) {
	    		System.err.append("Property " + SadlConstants.SADL_IMPLICIT_MODEL_EXTERNALURL_PROPERTY_URI + "not found in model");
	    		return false;
	    	}
	    	ClosableIterator<Triple> citr = context.find(eqInst.asNode(), p.asNode(), null);
	    	if (!citr.hasNext()) {
	    		ClosableIterator<Triple> citrWhat = context.find(eqInst.asNode(), null, null);
	    		boolean foundInst = false;
	    		while (citrWhat.hasNext()) {
	    			System.out.println(citrWhat.next().toString());
	    			foundInst = true;
	    		}
	    		if (!foundInst) {
	    			ClosableIterator<Triple> citrWhatWhat = context.find(null, null, null);
	    			while (citrWhatWhat.hasNext()) {
	    				System.out.println(citrWhatWhat.next().toString());
	    			}
	    			String lastChance = context.getGraph().toString();
	    			System.out.println(lastChance);
	    		}
	    		System.err.append(this.getName() + ": " + p.getURI() + " of " + eqInst.getURI() + " not found.\n");
	    		System.out.println(context.getGraph().getRawGraph().toString());
	    		return false;
	    	}
	    	while (citr.hasNext()) {
	    		Node javaUriLiteral = citr.next().getObject();
	    		if (javaUriLiteral.isLiteral()) {
	    			String javaUriStr = javaUriLiteral.getLiteralValue().toString();
	    			// now get the argument types
	    			List<Node> restOfArgTypes = new ArrayList<Node>();
	    			List<Node> returnTypes = new ArrayList<Node>();
	    			boolean varArgs = false;
	    			Property argsProp = ResourceFactory.createProperty(SadlConstants.SADL_IMPLICIT_MODEL_ARGUMENTS_PROPERTY_URI);
    				Property stlfirstProp = ResourceFactory.createProperty(SadlConstants.SADL_LIST_MODEL_FIRST_URI);
    				Property stlrestProp = ResourceFactory.createProperty(SadlConstants.SADL_LIST_MODEL_REST_URI);
	    			Property dataTypeProp = ResourceFactory.createProperty(SadlConstants.SADL_IMPLICIT_MODEL_DATATYPE_PROPERTY_URI);
	    			ClosableIterator<Triple> citr2 = context.find(eqInst.asNode(), argsProp.asNode(), null);
	    			while (citr2.hasNext()) {
	    				Node dt = citr2.next().getObject();	// this is the subject with the list of arguments as object
	    				while (dt != null) {
		    				ClosableIterator<Triple> citr4 = context.find(dt, stlfirstProp.asNode(), null);
		    				while (citr4.hasNext()) {
		    					Node dtfirst = citr4.next().getObject();
				    			ClosableIterator<Triple> citr5 = context.find(dtfirst, dataTypeProp.asNode(), null);
				    			while (citr5.hasNext()) {
				    				Node dataType = citr5.next().getObject();
				    				restOfArgTypes.add(dataType);
				    			}
				    			Property varArgsProp = ResourceFactory.createProperty(SadlConstants.SADL_IMPLICIT_MODEL_VARIABLE_NUM_ARGUMENTS_PROPERTY_URI);
				    			ClosableIterator<Triple> citr9 = context.find(dtfirst, varArgsProp.asNode(), null);
				    			while (citr9.hasNext()) {
				    				Node va = citr9.next().getObject();
				    				if (va.isLiteral() && va.getLiteral().getValue().equals(Boolean.TRUE)) {
				    					varArgs = true;
				    				}
				    			}

		    				}
		    				ClosableIterator<Triple> citr6 = context.find(dt, stlrestProp.asNode(), null);
		    				if (citr6.hasNext()) {
		    					dt = citr6.next().getObject();
		    				}
		    				else {
		    					dt =  null;
		    				}
		    				citr6.close();
	    				}
	    			}
    				// now get the return types
	    			Property returnTypesProp = ResourceFactory.createProperty(SadlConstants.SADL_IMPLICIT_MODEL_RETURN_TYPES_PROPERTY_URI);
	    			ClosableIterator<Triple> citr7 = context.find(eqInst.asNode(), returnTypesProp.asNode(), null);
	    			while (citr7.hasNext()) {
	    				Node rdt = citr7.next().getObject();
	    				while (rdt != null) {
		    				ClosableIterator<Triple> citr8 = context.find(rdt, stlfirstProp.asNode(), null);
		    				while (citr8.hasNext()) {
		    					Node rdtfirst = citr8.next().getObject();
				    			ClosableIterator<Triple> citr9 = context.find(rdtfirst, dataTypeProp.asNode(), null);
				    			while (citr9.hasNext()) {
				    				Node rdataType = citr9.next().getObject();
				    				returnTypes.add(rdataType);
				    			}
		    				}
		    				ClosableIterator<Triple> citr6 = context.find(rdt, stlrestProp.asNode(), null);
		    				if (citr6.hasNext()) {
		    					rdt = citr6.next().getObject();
		    				}
		    				else {
		    					rdt =  null;
		    				}
		    				citr6.close();
	    				}
	    			}	    				
	    			Node result = (new EvaluateSadlEquationUtils(null)).evaluateSadlEquation(javaUriStr, restOfArgs, restOfArgTypes, varArgs, returnTypes);
	           		return context.getEnv().bind(args[length - 1], result);	     
	    		}
	    		else {
	    			System.err.println("Failed to get external URI from node '" + javaUriLiteral.toString());
	    			return false;
	    		}
	    	}
		}
		return false;
    }
    
    /**
     * Returns false if this builtin has side effects when run in a body clause,
     * other than the binding of environment variables.
     */
    public boolean isSafe() {
         return true;
    }
    
    /**
     * Returns false if this builtin is non-monotonic. This includes non-monotonic checks like noValue
     * and non-monotonic actions like remove/drop. A non-monotonic call in a head is assumed to 
     * be an action and makes the overall rule and ruleset non-monotonic. 
     * Most JenaRules are monotonic deductive closure rules in which this should be false.
     */
    public boolean isMonotonic() {
        return true;
    }
 }
