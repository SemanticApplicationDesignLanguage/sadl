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
package com.naturalsemanticsllc.sadl.reasoner;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.reasoner.rulesys.Builtin;
import org.apache.jena.reasoner.rulesys.BuiltinException;
import org.apache.jena.reasoner.rulesys.Node_RuleVariable;
import org.apache.jena.reasoner.rulesys.RuleContext;
import org.apache.jena.util.iterator.ClosableIterator;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;

import com.ge.research.sadl.jena.reasoner.builtin.Product;
import com.ge.research.sadl.jena.reasoner.builtin.Utils;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper;
import com.ge.research.sadl.reasoner.UnittedQuantityHandlerException;
import com.ge.research.sadl.reasoner.UnittedQuantityUnitMismatchException;
import com.ge.research.sadl.reasoner.IUnittedQuantityInferenceHelper.BuiltinUnittedQuantityStatus;

public class JenaUnittedQuantityInferenceHelper implements IUnittedQuantityInferenceHelper {
	
	@Override
	public Object combineUnits(Object reasonerContext, Object operator, Object arg1Units, Object arg2Units) throws UnittedQuantityHandlerException {
		if (!(operator instanceof Node)) {
			throw new UnittedQuantityHandlerException("The operator is not a Node " + operator.getClass().getCanonicalName());
		}
		String opStr = ((Node)operator).getLiteralValue().toString();
		if (opStr.equals("*") || opStr.equals("product")|| opStr.equals("/") || opStr.equals("quotient")) {
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
		else if (opStr.equals("+") || opStr.equals("sum")|| opStr.equals("-") || opStr.equals("difference")) {
			if (arg1Units == null || arg2Units == null || !arg1Units.equals(arg2Units)) {
				throw new UnittedQuantityHandlerException("Units are not all the same (" + arg1Units.toString() + " != " + arg2Units.toString() + ")" );
			}
			else {
				return arg1Units;
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
		if (arg2Units != null && !(arg2Units instanceof Node)) {
			throw new UnittedQuantityHandlerException("Second argument is not a Node: " + arg2Units.getClass().getCanonicalName());
		}
		Node unit1 = (Node) arg1Units;
		Node unit2 = (Node) arg2Units;
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

	@Override
	public boolean isUnittedQuantity(Node node, Object context) throws UnittedQuantityHandlerException {
		if (context instanceof RuleContext) {
			return isUnittedQuantity(node, (RuleContext)context);
		}
		throw new UnittedQuantityHandlerException("Invalid context in isUnittedQuantity");
	}

	/**
	 * Method to determine if a Node is an instance of the SadlImplicitModel UnittedQuantity class.
	 * Since this is during inference, we can assume that transient closure over class hierarchies
	 * has made every instance of a subclass of UnittedQuantity also an instance of UnittedQuantity.
	 * @param node
	 * @param context
	 * @return
	 */
	private boolean isUnittedQuantity(Node node, RuleContext context) {
		if (node instanceof Node_RuleVariable) {
			return false;
		}
		Node UQCls =  NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
		ClosableIterator<Triple> citr = context.find(node, RDF.type.asNode(), UQCls);
		if (citr.hasNext()) {
			citr.close();
			return true;
		}
		return false;
	}

	/**
	 * Method to determine if a Node is an instance of the SadlImplicitModel UnittedQuantity class.
	 * Since this is during inference, we can assume that transient closure over class hierarchies
	 * has made every instance of a subclass of UnittedQuantity also an instance of UnittedQuantity.
	 * @param node
	 * @param context
	 * @return
	 */
	public boolean isUnittedQuantity(com.ge.research.sadl.model.gp.Node node, OntModel model) {
		if (!(node instanceof NamedNode)) {
			return false;
		}
		OntClass nodeCls = model.getOntClass(node.getURI());
		if (nodeCls != null) {
			OntClass UQCls =  model.getOntClass(SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
			if (UQCls != null) {
				if (UQCls.equals(nodeCls)) {
					return true;
				}
				if (model.contains(nodeCls, RDFS.subClassOf, UQCls)) {
					return true;
				}
			}
		}
		return false;
	}

	@Override
	public boolean listContainsUnittedQuantity(List<Node> nodes, Object context) throws UnittedQuantityHandlerException {
		if (context instanceof RuleContext) {
			return listContainsUnittedQuantity(nodes, (RuleContext)context);
		}
		throw new UnittedQuantityHandlerException("Invalid RuleContext passed to listContainsUnittedQuantity");
	}

	private boolean listContainsUnittedQuantity(List<Node> nodes, RuleContext context) {
		if (nodes != null) {
			for (Node node : nodes) {
				if (isUnittedQuantity(node, context)) {
					return true;
				}
			}
		}
		return false;
	}

	@Override
	public List<UnittedQuantity> getUnittedQuantityArgumentList(Object bi, List<Node> nodes,
			BuiltinUnittedQuantityStatus builtinUqStatus, Object context) throws UnittedQuantityHandlerException {
		if (bi instanceof Builtin && context instanceof RuleContext) {
			return getUnittedQuantityArgumentList((Builtin)bi, nodes, builtinUqStatus, (RuleContext)context);
		}
		throw new UnittedQuantityHandlerException("Invalid Builtin and/or RuleContext passed to getUnittedQuantityArgumentList");
	}
	
	public List<UnittedQuantity> getUnittedQuantityArgumentList(Builtin bi, List<Node> nodes,  
			BuiltinUnittedQuantityStatus builtinUqStatus, RuleContext context) throws UnittedQuantityHandlerException {
		List<UnittedQuantity> uqList = new ArrayList<UnittedQuantity>();
		boolean firstNode = true;
		for (Node node : nodes) {
			Node unit = getUnittedQuantityUnit(node, context);
			Node value = null;
			if (!firstNode && unit == null) {
				if (!builtinUqStatus.equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly) && 
						!builtinUqStatus.equals(BuiltinUnittedQuantityStatus.LeftUnitsOnly)) {
					throw new BuiltinException(bi, context, "Encountered a non-UnittedQuantity or invalid UnittedQuantity while processing UnittedQuantities");
				}
				else if (node.isLiteral()) {
					value = node;
				}
			}
			else {
				value = getUnittedQuantityValue(node, context);
			}
			UnittedQuantity uq = new UnittedQuantity(value, unit);
			uqList.add(uq);
			firstNode = false;
		}
		return uqList;
	}

	@Override
	public Node createUnittedQuantity(Object context, Node value, Node unit) throws UnittedQuantityHandlerException {
		if (context instanceof RuleContext) {
			return createUnittedQuantity((RuleContext)context, value, unit);
		}
		throw new UnittedQuantityHandlerException("Invalid RuleContext passed to createUnittedQuantity");
	}
	
	public Node createUnittedQuantity(RuleContext context, Node valNode, Node unitNode) {
		Node uQinst = Utils.createInstanceOfClass(context, SadlConstants.SADL_IMPLICIT_MODEL_UNITTEDQUANTITY_URI);
		Node valPred = NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI);
		Utils.addValue(context, uQinst, valPred, valNode);
		Node unitPred = NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI);
		Utils.addValue(context, uQinst, unitPred, unitNode);
		return uQinst;

	}

	public boolean validateUnittedQuantityArgs(RuleContext context, BuiltinUnittedQuantityStatus operatorType, List<Node> nodeLst) throws UnittedQuantityHandlerException {
		String className = IUnittedQuantityInferenceHelper.getUnittedQuantityInferenceHelperClassname(context.getGraph().getReasoner());
		try {
			Class<?> c = Class.forName(className);
			Constructor<?> cons = c.getConstructor();
			Object inst = cons.newInstance();
			if (inst instanceof IUnittedQuantityInferenceHelper) {
				Node lastArg = null;
				for (Node arg : nodeLst) {
					if (lastArg != null) {
						boolean result = ((IUnittedQuantityInferenceHelper)inst).validateOperation(context, operatorType, lastArg, arg);
					}
					lastArg = arg;
				}
				return true;
			}
			else {
				throw new UnittedQuantityHandlerException("Failed to instantiate IUnittedQuantityInferenceHandler class '" + className + "'");
			}
		} catch (Exception e) {
			throw new UnittedQuantityHandlerException("Unable to invoke IUnittedQuantityInferenceHandler method 'validateUnittedQuantityArgs'", e);
		} 
	}
	
	/**
	 * Method to obtain the value of a Node which is an instance of the SadlImplicitModel's
	 * UnittedQuantity class
	 * @param node
	 * @param context
	 * @return
	 */
	public static Node getUnittedQuantityValue(Node node, RuleContext context) {
		Node valuePred =  NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_VALUE_URI);
		ClosableIterator<Triple> citr = context.find(node, valuePred, null);
		if (citr.hasNext()) {
			Node val = citr.next().getObject();
			citr.close();
			return val;
		}
		return null;
	}
	
//	/**
//	 * Method to get a list of Nodes which are the values of a list of instances of the SadlImplicitModel's
//	 * UnittedQuantity class
//	 * @param bi
//	 * @param nodes
//	 * @param context
//	 * @return
//	 */
//	private List<Node> getUnittedQuantityValues(Builtin bi, List<Node> nodes, BuiltinUnittedQuantityStatus builtinUqStatus, RuleContext context) {
//		List<Node> values = new ArrayList<Node>();
//		boolean firstNode = true;
//		for (Node node : nodes) {
//			Node value = getUnittedQuantityValue(node, context);
//			if (!firstNode && value == null) {
//				if (builtinUqStatus.equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly) || 
//						builtinUqStatus.equals(BuiltinUnittedQuantityStatus.LeftUnitsOnly)) {
//					value = node;
//				}
//				else {
//					throw new BuiltinException(bi, context, "Encountered a non-UnittedQuantity or invalid UnittedQuantity while processing UnittedQuantities");
//				}
//			}
//			values.add(value);
//			firstNode = false;
//		}
//		return values;
//	}

	@Override
	public Node getUnittedQuantityUnit(Node node,  RuleContext context) {
		Node unitPred =  NodeFactory.createURI(SadlConstants.SADL_IMPLICIT_MODEL_UNIT_URI);
		ClosableIterator<Triple> citr = context.find(node, unitPred, null);
		if (citr.hasNext()) {
			Node unit =  citr.next().getObject();
			citr.close();
			return unit;
		}
		return null;
	}

//	/**
//	 * Method to get a list of Nodes which are the units of a list of instances of the SadlImplicitModel's
//	 * UnittedQuantity class
//	 * @param bi
//	 * @param nodes
//	 * @param context
//	 * @return
//	 */
//	private List<Node> getUnittedQuantityUnits(Builtin bi, List<Node> nodes,  BuiltinUnittedQuantityStatus builtinUqStatus, RuleContext context) {
//		List<Node> units = new ArrayList<Node>();
//		boolean firstNode = true;
//		for (Node node : nodes) {
//			Node unit = getUnittedQuantityUnit(node, context);
//			if (!firstNode && unit == null) {
//				if (!builtinUqStatus.equals(BuiltinUnittedQuantityStatus.DifferentUnitsAllowedOrLeftOnly) && 
//						!builtinUqStatus.equals(BuiltinUnittedQuantityStatus.LeftUnitsOnly)) {
//					throw new BuiltinException(bi, context, "Encountered a non-UnittedQuantity or invalid UnittedQuantity while processing UnittedQuantities");
//				}
//			}
//			units.add(unit);
//			firstNode = false;
//		}
//		return units;
//	}

}
