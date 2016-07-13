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
package com.ge.research.sadl.processing;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;

import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.ProxyNode;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.Test.ComparisonType;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.TripleElement.TripleSourceType;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.BooleanLiteral;
import com.ge.research.sadl.sADL.Expression;
import com.ge.research.sadl.sADL.NumberLiteral;
import com.ge.research.sadl.sADL.SadlDataType;
import com.ge.research.sadl.sADL.SadlPrimitiveDataType;
import com.ge.research.sadl.sADL.SadlSimpleTypeReference;
import com.ge.research.sadl.sADL.SadlTypeReference;
import com.ge.research.sadl.sADL.StringLiteral;
import com.google.inject.Inject;


public abstract class SadlModelProcessor implements IModelProcessor {
    private static final Logger logger = Logger.getLogger(SadlModelProcessor.class);
    private Object target = null;	// the instance of Rule, Query, or Test into which we are trying to put the translation
    private List<IFTranslationError> errors = null;
    private Object encapsulatingTarget = null;	// when a query is in a test
    public enum RulePart {PREMISE, CONCLUSION, NOT_A_RULE}
    private RulePart rulePart = RulePart.NOT_A_RULE;

	@Inject
	public DeclarationExtensions declarationExtensions;

	public abstract Object translate(Expression expr) throws InvalidNameException, InvalidTypeException, TranslationException ;
	
//	protected Object translate(BinaryOperation expr) throws InvalidNameException, InvalidTypeException, TranslationException {
////		StringBuilder sb = new StringBuilder();
////		String op = expr.getOp();
////		sb.append(translate(expr.getLeft()));
////		sb.append(" ");
////		sb.append(op);
////		sb.append(" ");
////		sb.append(translate(expr.getRight()));
////		return sb.toString();
//		String op = expr.getOp();
//		BuiltinType optype = BuiltinType.getType(op);
//		
//		Expression lexpr = expr.getLeft();
//		Expression rexpr = expr.getRight();
//		Object lobj = translate(lexpr);
//		Object robj = translate(rexpr);
//		
//		if (optype == BuiltinType.Equal || optype == BuiltinType.NotEqual) {
//			// If we're doing an assignment, we can simplify the pattern.
//			Node assignedNode = null;
//			Object pattern = null;
//			if (lobj instanceof NamedNode && !(lobj instanceof VariableNode) && hasCommonVariableSubject(robj)) {
//				TripleElement trel = (TripleElement)robj;
//				while (trel != null) {
//					trel.setSubject((Node) lobj);
//					trel = (TripleElement) trel.getNext();
//				}
//				return robj;
//			}
//			if ((lobj instanceof TripleElement || (lobj instanceof Literal && isSparqlQuery(((Literal)lobj).toString())))
//					&& robj instanceof BuiltinElement) {
//				if (isModifiedTriple(((BuiltinElement)robj).getFuncType())) {
//					assignedNode = ((BuiltinElement)robj).getArguments().get(0);
//					optype = ((BuiltinElement)robj).getFuncType();
//					pattern = lobj;
//				}
//				else if (isComparisonBuiltin(((BuiltinElement)robj).getFuncName())) {
//					if ( ((BuiltinElement)robj).getArguments().get(0) instanceof Literal) {
//						((TripleElement)lobj).setObject(nodeCheck(robj));
//						return lobj;
//					}
//					else {
//						return createBinaryBuiltin(rexpr, ((BuiltinElement)robj).getFuncName(), lobj, ((BuiltinElement)robj).getArguments().get(0));
//					}
//				}
//			}
//			else if (lobj instanceof Node && robj instanceof TripleElement) {
//				assignedNode = validateNode((Node) lobj);
//				pattern = (TripleElement) robj;
//			}
//			else if (robj instanceof Node && lobj instanceof TripleElement) {
//				assignedNode = validateNode((Node) robj);
//				pattern = (TripleElement) lobj;
//			}
//			if (assignedNode != null && pattern != null) {
//				// We're expressing the type of a named thing.
//				if (pattern instanceof TripleElement && ((TripleElement)pattern).getSubject() == null) {
//					if (isModifiedTripleViaBuiltin(robj)) {
//						optype = ((BuiltinElement)((TripleElement)pattern).getNext()).getFuncType();	
//						((TripleElement)pattern).setNext(null);
//					}
//					((TripleElement)pattern).setSubject(assignedNode);
//					if (optype != BuiltinType.Equal) {
//						((TripleElement)pattern).setType(getTripleModifierType(optype));
//					}
//				}
//				else if (pattern instanceof TripleElement && ((TripleElement)pattern).getObject() == null && 
//						(((TripleElement)pattern).getSourceType().equals(TripleSourceType.PSnewV) 
//								|| ((TripleElement)pattern).getSourceType().equals(TripleSourceType.PSV))) {
//					if (isModifiedTripleViaBuiltin(robj)) {
//						optype = ((BuiltinElement)((TripleElement)pattern).getNext()).getFuncType();	
//						((TripleElement)pattern).setNext(null);
//					}
//					((TripleElement)pattern).setObject(assignedNode);
//					if (optype != BuiltinType.Equal) {
//						((TripleElement)pattern).setType(getTripleModifierType(optype));
//					}
//				}
//				else if (pattern instanceof TripleElement && ((TripleElement)pattern).getSourceType().equals(TripleSourceType.SPV)
//						&& assignedNode instanceof NamedNode && getProxyWithNullSubject(((TripleElement)pattern)) != null) {
//					TripleElement proxyFor = getProxyWithNullSubject(((TripleElement)pattern));
//					assignNullSubjectInProxies(((TripleElement)pattern), proxyFor, assignedNode);
//					if (optype != BuiltinType.Equal) {
//						proxyFor.setType(getTripleModifierType(optype));
//					}
//				}
//				else if (isModifiedTriple(optype) || 
//						(optype.equals(BuiltinType.Equal) && pattern instanceof TripleElement && 
//								(((TripleElement)pattern).getObject() == null || 
//										((TripleElement)pattern).getObject() instanceof NamedNode ||
//										((TripleElement)pattern).getObject() instanceof Literal))){
//					if (pattern instanceof TripleElement && isModifiedTripleViaBuiltin(robj)) {
//						optype = ((BuiltinElement)((TripleElement)pattern).getNext()).getFuncType();
//						((TripleElement)pattern).setObject(assignedNode);
//						((TripleElement)pattern).setNext(null);
//						((TripleElement)pattern).setType(getTripleModifierType(optype));
//					}
//					else if (isComparisonViaBuiltin(robj, lobj)) {
//						BuiltinElement be = (BuiltinElement)((TripleElement)robj).getNext();
//						be.addMissingArgument((Node) lobj);
//						return pattern;
//					}
//					else if (pattern instanceof TripleElement){
//						TripleElement lastPattern = (TripleElement)pattern;
//						// this while may need additional conditions to narrow application to nested triples?
//						while (lastPattern.getNext() != null && lastPattern instanceof TripleElement) {
//							lastPattern = (TripleElement) lastPattern.getNext();
//						}
//						if (getEncapsulatingTarget() instanceof Test) {
//							((Test)getEncapsulatingTarget()).setRhs(assignedNode);
//							((Test)getEncapsulatingTarget()).setCompName(optype);
//						}
//						else if (getEncapsulatingTarget() instanceof Query && getTarget() instanceof Test) {
//							((Test)getTarget()).setRhs(getEncapsulatingTarget());
//							((Test)getTarget()).setLhs(assignedNode);
//							((Test)getTarget()).setCompName(optype);
//						}
//						else if (getTarget() instanceof Test && assignedNode != null) {
//							((Test)getTarget()).setLhs(pattern);
//							((Test)getTarget()).setRhs(assignedNode);
//							((Test)getTarget()).setCompName(optype);
//							((TripleElement) pattern).setType(TripleModifierType.None);
//							optype = BuiltinType.Equal;
//						}
//						else {
//							lastPattern.setObject(assignedNode);
//						}
//						if (!optype.equals(BuiltinType.Equal)) {
//							((TripleElement)pattern).setType(getTripleModifierType(optype));
//						}
//					}
//					else {
//						if (getTarget() instanceof Test) {
//							((Test)getTarget()).setLhs(lobj);
//							((Test)getTarget()).setRhs(assignedNode);
//							((Test)getTarget()).setCompName(optype);
//						}
//					}
//				}
//				else if (getEncapsulatingTarget() instanceof Test) {
//					((Test)getEncapsulatingTarget()).setRhs(assignedNode);
//					((Test)getEncapsulatingTarget()).setCompName(optype);
//				}
//				else if (getTarget() instanceof Rule && pattern instanceof TripleElement && ((TripleElement)pattern).getSourceType().equals(TripleSourceType.ITC) && 
//						((TripleElement)pattern).getSubject() instanceof VariableNode && assignedNode instanceof VariableNode) {
//					// in a rule of this type we just want to replace the pivot node variable
//					doVariableSubstitution(((TripleElement)pattern), (VariableNode)((TripleElement)pattern).getSubject(), (VariableNode)assignedNode);
//				}
//				return pattern;
//			}
//			BuiltinElement bin = null;
//			boolean binOnRight = false;
//			Object retObj = null;
//			if (lobj instanceof Node && robj instanceof BuiltinElement) {
//				assignedNode = validateNode((Node)lobj);
//				bin = (BuiltinElement)robj;
//				retObj = robj;
//				binOnRight = true;
//			}
//			else if (robj instanceof Node && lobj instanceof BuiltinElement) {
//				assignedNode = validateNode((Node)robj);
//				bin = (BuiltinElement)lobj;
//				retObj = lobj;
//				binOnRight = false;
//			}
//			if (bin != null && assignedNode != null) {
//				if ((assignedNode instanceof VariableNode ||
//					(assignedNode instanceof NamedNode && ((NamedNode)assignedNode).getNodeType().equals(NodeType.VariableNode)))) {
//					while (bin.getNext() instanceof BuiltinElement) {
//						bin = (BuiltinElement) bin.getNext();
//					}
//					if (bin.isCreatedFromInterval()) {
//						bin.addArgument(0, assignedNode);
//					}
//					else {
//						bin.addArgument(assignedNode);
//					}
//					return retObj;
//				}
//				else if (assignedNode instanceof Node && isComparisonBuiltin(bin.getFuncName())) {
//					// this is a comparison with an extra "is"
//					if (bin.getArguments().size() == 1) {
//						if (bin.isCreatedFromInterval() || binOnRight) {
//							bin.addArgument(0, assignedNode);
//						}
//						else {
//							bin.addArgument(assignedNode);
//						}
//						return bin;
//					}
//				}
//			}
//			// We're describing a thing with a graph pattern.
//			Set<VariableNode> vars = pattern instanceof TripleElement ? getSelectVariables(((TripleElement)pattern)) : null; 
//			if (vars != null && vars.size() == 1) {
//				// Find where the unbound variable occurred in the pattern
//				// and replace each place with the assigned node.
//				VariableNode var = vars.iterator().next();
//				GraphPatternElement gpe = ((TripleElement)pattern);
//				while (gpe instanceof TripleElement) {
//					TripleElement triple = (TripleElement) gpe;
//					if (var.equals(triple.getSubject())) {
//						triple.setSubject(assignedNode);
//					}
//					if (var.equals(triple.getObject())) {
//						triple.setObject(assignedNode);
//					}
//					gpe = gpe.getNext();
//				}
//				return pattern;
//			}
//		}
//		// if we get to here we want to actually create a BuiltinElement for the BinaryOpExpression
//		// However, if the type is equal ("is", "equal") and the left side is a VariableNode and the right side is a literal
//		//	and the VariableNode hasn't already been bound, change from type equal to type assign.
//		if (optype == BuiltinType.Equal && getTarget() instanceof Rule && lobj instanceof VariableNode && robj instanceof Literal && 
//				!variableIsBound((Rule)getTarget(), null, (VariableNode)lobj)) {
//			return createBinaryBuiltin(expr, "assign", robj, lobj);
//		}
//		return createBinaryBuiltin(expr, op, lobj, robj);
//	}

//	protected String translate(SubjHasProp expr) throws InvalidNameException, InvalidTypeException, TranslationException {
//		StringBuilder sb = new StringBuilder();
//		sb.append(translate(expr.getLeft()));
//		sb.append(" ");
//		sb.append(translate(expr.getProp()));
//		sb.append(" ");
//		sb.append(translate(expr.getRight()));
//		return sb.toString();
//	}

//	protected String translate(PropOfSubject expr) throws InvalidNameException, InvalidTypeException, TranslationException {
//		StringBuilder sb = new StringBuilder();
//		sb.append(translate(expr.getLeft()));
//		sb.append(" of ");
//		sb.append(translate(expr.getRight()));
//		return sb.toString();
//	}

//	protected String translate(Name expr) {
//		return declarationExtensions.getConceptUri(expr.getName());
//	
//	}

//	protected String translate(Declaration expr) throws InvalidNameException, InvalidTypeException, TranslationException {
//		StringBuilder sb = new StringBuilder();
//		String article = expr.getArticle();
//		sb.append(article);
//		sb.append(" ");
//		sb.append(translate(expr.getType()));
//		if (expr.getNewName() != null) {
//			sb.append(" ");
//			sb.append(expr.getNewName());
//		}
//		return sb.toString();
//	}

	private Object translate(SadlTypeReference type) throws TranslationException, InvalidNameException, InvalidTypeException {
		if (type instanceof SadlSimpleTypeReference) {
			return translate(((SadlSimpleTypeReference)type).getType());
		}
		else if (type instanceof SadlPrimitiveDataType) {
			SadlDataType pt = ((SadlPrimitiveDataType)type).getPrimitiveType();
			String typeStr = pt.getLiteral();
			return typeStr;
		}
		throw new TranslationException("Unhandled type of SadlTypeReference");
	}

	protected Object translate(BooleanLiteral expr) {
		Literal lit = new Literal();
		String val = expr.getValue();
		if (val.equals("true")) {
			lit.setValue(true);
		}
		else {
			lit.setValue(false);
		}
		lit.setOriginalText(val);
		return lit;
	}

	protected Object translate(NumberLiteral expr) {
		Literal lit = new Literal();
		BigDecimal val = expr.getValue();
		lit.setValue(val);
		lit.setOriginalText(expr.getValue().toPlainString());
		return lit;
	}

	protected Object translate(StringLiteral expr) {
		Literal lit = new Literal();
		lit.setValue(expr.getValue());
		lit.setOriginalText(expr.getValue());
		return lit;
	}

//	protected String translate(Constant expr) throws InvalidNameException, InvalidTypeException, TranslationException {
//		return expr.getConstant();
//	}

	private boolean hasCommonVariableSubject(Object robj) {
		if (robj instanceof TripleElement && 
				(((TripleElement)robj).getSubject() instanceof VariableNode && 
						(((TripleElement)robj).getSourceType().equals(TripleSourceType.SPV)) ||
						((TripleElement)robj).getSourceType().equals(TripleSourceType.ITC))) {
			VariableNode subjvar = (VariableNode) ((TripleElement)robj).getSubject();
			Object trel = robj;
			while (trel != null && trel instanceof TripleElement) {
				if (!(trel instanceof TripleElement) || 
						(((TripleElement)trel).getSubject() != null &&!(((TripleElement)trel).getSubject().equals(subjvar)))) {
					return false;
				}
				trel = ((TripleElement)trel).getNext();
			}
			if (trel == null) {
				return true;
			}
		}
		return false;
	}

	public static boolean isSparqlQuery(String litObj) {
		litObj = litObj.trim();
		SadlUtils su = new SadlUtils();
		litObj = su.stripQuotes(litObj);
		litObj = litObj.trim();
		if (litObj.toLowerCase().indexOf("where") > 0 &&
				(( litObj.toLowerCase().indexOf("select ") == 0 && ((String) litObj).indexOf("?") > 0) ||
				litObj.toLowerCase().indexOf("construct ") == 0)) {
			return true;
		}
		else if (litObj.length() > 4){
			int askidx = litObj.indexOf("ask ");
			int opidx = litObj.indexOf('{');
			int cpidx = litObj.indexOf('}');	
			if (askidx == 0 && opidx > 3 && cpidx > opidx) {
				return true;
			}
		}
		return false;
	}

	public static boolean isModifiedTriple(BuiltinType type) {
		if (type.equals(BuiltinType.Not) || type.equals(BuiltinType.NotEqual) || type.equals(BuiltinType.Only)||  type.equals(BuiltinType.NotOnly)) {
			return true;
		}
		return false;
	}
	
	public static boolean isComparisonBuiltin(String builtinName) {
		ComparisonType[] types = ComparisonType.values();
		for (ComparisonType type : types) {
			if (type.matches(builtinName)) {
				return true;
			}
		}
		return false;
	}

	private Node nodeCheck(Object nodeObj) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (nodeObj == null) {
//			throw new InvalidTypeException("nodeCheck called with null argument; this should not happen.");
			return null;
		}
		if (nodeObj instanceof Node) {
			return (Node) nodeObj; 
		}
		else if (nodeObj instanceof TripleElement) {
			if (((TripleElement)nodeObj).getPredicate() == null 
					&& ((TripleElement)nodeObj).getObject() == null
					&& ((TripleElement)nodeObj).getSubject() != null) {
				return ((TripleElement)nodeObj).getSubject();
			}
		}
		return new ProxyNode(nodeObj);
	}

	private GraphPatternElement createBinaryBuiltin(Expression expr, String name, Object lobj, Object robj) throws InvalidNameException, InvalidTypeException, TranslationException {
		BuiltinElement builtin = new BuiltinElement();
		builtin.setFuncName(name);
		if (lobj != null) {
			builtin.addArgument(nodeCheck(lobj));
		}
		if (robj != null) {
			builtin.addArgument(nodeCheck(robj));
		}
		return builtin;
	}
	
	private Junction createJunction(Expression expr, String name, Object lobj, Object robj) {
		Junction junction = new Junction();
		junction.setJunctionName(name);
		junction.setLhs(lobj);
		junction.setRhs(robj);
		return junction;
	}

	private Object createUnaryBuiltin(Expression sexpr, String name, Object sobj) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (sobj instanceof Literal && BuiltinType.getType(name).equals(BuiltinType.Minus)) {
			Object theVal = ((Literal)sobj).getValue();
			if (theVal instanceof Integer) {
				theVal = ((Integer)theVal) * -1;
			}
			else if (theVal instanceof Long) {
				theVal = ((Long)theVal) * -1;
			}
			else if (theVal instanceof Float) {
				theVal = ((Float)theVal) * -1;
			}
			else if (theVal instanceof Double) {
				theVal = ((Double)theVal) * -1;
			}
			((Literal)sobj).setValue(theVal);
			((Literal)sobj).setOriginalText("-" + ((Literal)sobj).getOriginalText());
			return sobj;
		}
		if (sobj instanceof Junction) {
			// If the junction has two literal values, apply the op to both of them.
			Junction junc = (Junction) sobj;
			Object lhs = junc.getLhs();
			Object rhs = junc.getRhs();
			if (lhs instanceof Literal && rhs instanceof Literal) {
				lhs = createUnaryBuiltin(sexpr, name, lhs);
				rhs = createUnaryBuiltin(sexpr, name, rhs);
				junc.setLhs(lhs);
				junc.setRhs(rhs);
			}
			return junc;
		}
		if (BuiltinType.getType(name).equals(BuiltinType.Equal)) {
			if (sobj instanceof BuiltinElement) {
				if (isComparisonBuiltin(((BuiltinElement)sobj).getFuncName())) {
					// this is a "is <comparison>"--translates to <comparsion> (ignore is)
					return sobj;
				}
			}
			else if (sobj instanceof Literal || sobj instanceof NamedNode) {
				// an "=" interval value of a value is just the value
				return sobj;
			}
		}
		BuiltinElement builtin = new BuiltinElement();
		builtin.setFuncName(name);
		if (isModifiedTriple(builtin.getFuncType())) {
			if (sobj instanceof TripleElement) {
				((TripleElement)sobj).setType(getTripleModifierType(builtin.getFuncType()));
				return sobj;
			}
		}
		if (sobj != null) {
			builtin.addArgument(nodeCheck(sobj));
		}
		return builtin;
	}

	private TripleModifierType getTripleModifierType(BuiltinType btype) {
		if (btype.equals(BuiltinType.Not) || btype.equals(BuiltinType.NotEqual)) {
			return TripleModifierType.Not;
		}
		else if (btype.equals(BuiltinType.Only)) {
			return TripleModifierType.Only;
		}
		else if (btype.equals(BuiltinType.NotOnly)) {
			return TripleModifierType.NotOnly;
		}
		return null;
	}

	/**
	 * This method returns true if the argument node is bound in some other element of the rule
	 * 
	 * @param rule
	 * @param gpe
	 * @param v
	 * @return
	 */
	public static boolean variableIsBound(Rule rule, GraphPatternElement gpe,
			Node v) {
		if (v instanceof NamedNode) {
			if (((NamedNode)v).getNodeType() != null && !(((NamedNode)v).getNodeType().equals(NodeType.VariableNode))) {
				return true;
			}
		}
		// Variable is bound if it appears in a triple or as the return argument of a built-in
		List<GraphPatternElement> givens = rule.getGivens();
		if (variableIsBoundInOtherElement(givens, 0, gpe, true, false, v)) {
			return true;
		}
		List<GraphPatternElement> ifs = rule.getIfs();
		if (variableIsBoundInOtherElement(ifs, 0, gpe, true, false, v)) {
			return true;
		}
		List<GraphPatternElement> thens = rule.getThens();
		if (variableIsBoundInOtherElement(thens, 0, gpe, false, true, v)) {
			return true;
		}
		return false;
	}

	/**
	 * This method checks the list of GraphPatternElements to see if the specified variable is bound in these elements
	 * 
	 * @param gpes - list of GraphPatternElements to check
	 * @param startingIndex - where to start in the list
	 * @param gp - the element in which this variable appears 
	 * @param boundIfEqual - use the current element for test?
	 * @param matchMustBeAfter - must the binding be after the current element
	 * @param v - the variable Node being checked
	 * @return - true if the variable is bound else false
	 */
	public static boolean variableIsBoundInOtherElement(List<GraphPatternElement> gpes, int startingIndex, GraphPatternElement gp, 
			boolean boundIfEqual, boolean matchMustBeAfter, Node v) {
		boolean reachedSame = false;
		for (int i = startingIndex; gpes != null && i < gpes.size(); i++) {
			GraphPatternElement gpe = gpes.get(i);
			while (gpe != null) {
				boolean same = gp == null ? false : gp.equals(gpe);
				if (same) {
					reachedSame = true;
				}
				boolean okToTest = false;
				if (matchMustBeAfter && reachedSame && !same) {
					okToTest = true;
				}
				if (!matchMustBeAfter && (!same || (same && boundIfEqual))) {
					okToTest = true;
				}
				if (okToTest && variableIsBound(gpe, v)) {
					return true;
				}
				gpe = gpe.getNext();
			}
		}
		return false;
	}
	
	private static boolean variableIsBound(GraphPatternElement gpe, Node v) {
		if (gpe instanceof TripleElement) {
			if ((((TripleElement)gpe).getSubject() != null &&((TripleElement)gpe).getSubject().equals(v)) || 
					(((TripleElement)gpe).getObject() != null && ((TripleElement)gpe).getObject().equals(v))) {
				return true;
			}
		}
		else if (gpe instanceof BuiltinElement) {
			List<Node> args = ((BuiltinElement)gpe).getArguments();
			// TODO built-ins can actually have more than the last argument as output, but we can only tell this
			//	if we have special knowledge of the builtin. Current SADL grammar doesn't allow this to occur.
			if (args != null && args.get(args.size() - 1) != null && args.get(args.size() - 1).equals(v)) {
				return true;
			}
		}
		else if (gpe instanceof Junction) {
			Object lhsobj = ((Junction)gpe).getLhs();
			if (lhsobj instanceof GraphPatternElement && variableIsBound((GraphPatternElement)lhsobj, v)) {
				return true;
			}
			else if (lhsobj instanceof VariableNode && ((VariableNode)lhsobj).equals(v)) {
				return true;
			}
			Object rhsobj = ((Junction)gpe).getRhs();
			if (rhsobj instanceof GraphPatternElement && variableIsBound((GraphPatternElement)rhsobj, v)) {
				return true;
			}
			else if (rhsobj instanceof VariableNode && ((VariableNode)rhsobj).equals(v)) {
				return true;
			}
		}
		return false;
	}

	public Set<VariableNode> getSelectVariables(GraphPatternElement pattern) {
		Set<VariableNode> vars = new LinkedHashSet<VariableNode>();
		if (pattern instanceof TripleElement) {
			TripleElement triple = (TripleElement) pattern;
			if (triple.getSubject() instanceof VariableNode &&
					triple.getPredicate() instanceof RDFTypeNode) {
				VariableNode var = (VariableNode) triple.getSubject();
				vars.add(var);
			}
			else if (triple.getSubject() instanceof VariableNode) {
				vars.add(((VariableNode)triple.getSubject()));
			}
			else if (triple.getObject() instanceof VariableNode) {
				vars.add(((VariableNode)triple.getObject()));
			}
			else if (triple.getPredicate() instanceof VariableNode) {
				vars.add(((VariableNode)triple.getPredicate()));
			}
		}
		else if (pattern instanceof BuiltinElement) {
			List<Node> args = ((BuiltinElement)pattern).getArguments();
			if (args != null) {
				// right now we have no mechanism to know which variables are unbound so
				//	assume the last one is
				// TODO
				Node arg = args.get(args.size() - 1);
				if (arg instanceof VariableNode) {
					vars.add((VariableNode) arg);
				}
			}
		}
		else if (pattern instanceof Junction) {
			Object lhs = ((Junction)pattern).getLhs();
			Object rhs = ((Junction)pattern).getRhs();
			if (lhs instanceof GraphPatternElement) {
				Set<VariableNode> lhsvars = getSelectVariables((GraphPatternElement) lhs);
				if (lhsvars != null) {
					vars.addAll(lhsvars);
				}
			}
			if (rhs instanceof GraphPatternElement) {
				Set<VariableNode> rhsvars = getSelectVariables((GraphPatternElement) rhs);
				if (rhsvars != null) {
					vars.addAll(rhsvars);
				}
			}
		}
		return vars;
	}

	/**
	 * This method fills in missing information in a NamedNode: 
	 * the prefix, the namespace, the type
	 * 
	 * @param namedNode
	 * @return
	 * @throws InvalidNameException 
	 * @throws TranslationException 
	 */
	protected Node validateNode(Node node) throws InvalidNameException, TranslationException {
		if (node instanceof NamedNode) {
			if (!((NamedNode)node).isValidated()) {
				if (node instanceof VariableNode) {
					((VariableNode) node).setNodeType(NodeType.VariableNode);
//					userDefinedVariables.add(((NamedNode) node).getName());
				}
				else if (node instanceof RDFTypeNode) {
					((RDFTypeNode) node).setNodeType(NodeType.PropertyNode);
				}
				else {
					if (!(node instanceof VariableNode) && ((NamedNode)node).getNodeType().equals(NodeType.VariableNode)) {
						VariableNode vnode = new VariableNode(((NamedNode)node).getName());
						vnode.setNamespace(((NamedNode) node).getNamespace());
						vnode.setNodeType(((NamedNode)node).getNodeType());
						vnode.setPrefix(((NamedNode)node).getPrefix());
						node = vnode;
//			    		userDefinedVariables.add(((NamedNode) node).getName());
			    	}
				}
				((NamedNode)node).setValidated(true);
			}
		}
		return node;
	}

	private ConceptType nodeTypeToConceptType(NodeType nt) throws TranslationException {
		if (nt.equals(NodeType.ClassNode)) {
			return ConceptType.ONTCLASS;
		}
		else if (nt.equals(NodeType.InstanceNode)) {
			return ConceptType.INDIVIDUAL;
		}
		else if (nt.equals(NodeType.DataTypeProperty)) {
			return ConceptType.DATATYPEPROPERTY;
		}
		else if (nt.equals(NodeType.ObjectProperty)) {
			return ConceptType.OBJECTPROPERTY;
		}
		else if (nt.equals(NodeType.VariableNode)) {
			return ConceptType.VARIABLE;
		}
		else {
			throw new TranslationException("NodeType '" + nt.toString() + "' cannot be converted to a ConceptType");
		}
	}

	private ConceptName validateConceptName(ConceptName conceptName) {
		// TODO Auto-generated method stub
		return conceptName;
	}
	
	private void addError(IFTranslationError error) {
		if (errors == null) {
			errors = new ArrayList<IFTranslationError>();
		}
		errors.add(error);
	}

	public List<IFTranslationError> getErrors() {
		return errors;
	}
	
	private boolean doVariableSubstitution(GraphPatternElement gpe, VariableNode v1, VariableNode v2) {
		boolean retval = false;
		do {
			if (gpe instanceof TripleElement) {
				if (((TripleElement)gpe).getSubject().equals(v1)) {
					((TripleElement)gpe).setSubject(v2);
					retval = true;
				}
				else if (((TripleElement)gpe).getObject().equals(v1)) {
					((TripleElement)gpe).setObject(v2);
					retval = true;
				}
			}
			else if (gpe instanceof BuiltinElement) {
				List<Node> args = ((BuiltinElement)gpe).getArguments();
				for (int j = 0; j < args.size(); j++) {
					if (args.get(j).equals(v1)) {
						args.set(j, v2);
						retval = true;
					}
				}
			}
			else if (gpe instanceof Junction) {
//				logger.error("Not yet handled");
			}
			gpe = gpe.getNext();
		} while (gpe != null);
		return retval;
	}

	private boolean isModifiedTripleViaBuiltin(Object robj) {
		if (robj instanceof TripleElement && ((TripleElement)robj).getNext() instanceof BuiltinElement) {
			BuiltinElement be = (BuiltinElement) ((TripleElement)robj).getNext();
			if (((TripleElement)robj).getPredicate() instanceof RDFTypeNode) {
				if (isModifiedTriple(be.getFuncType())) {
					Node subj = ((TripleElement)robj).getSubject();
					Node arg = (be.getArguments() != null && be.getArguments().size() > 0) ? be.getArguments().get(0) : null;
					if (subj == null && arg == null) {
						return true;
					}
					if (subj != null && arg != null && subj.equals(arg)) {
						return true;
					}
				}
			}
			else {
				if (isModifiedTriple(be.getFuncType()) && ((TripleElement)robj).getObject().equals(be.getArguments().get(0))) {
					return true;
				}
			}
		}
		return false;
	}

	private boolean isComparisonViaBuiltin(Object robj, Object lobj) {
		if (robj instanceof TripleElement && lobj instanceof Node &&
				((TripleElement)robj).getNext() instanceof BuiltinElement) {
			BuiltinElement be = (BuiltinElement) ((TripleElement)robj).getNext();
			if (isComparisonBuiltin(be.getFuncName()) && be.getArguments().size() == 1) {
				return true;
			}
		}
		return false;
	}

	private TripleElement getProxyWithNullSubject(TripleElement pattern) {
		if (pattern.getSubject() instanceof ProxyNode) {
			Object proxy = ((ProxyNode)pattern.getSubject()).getProxyFor();
			if (proxy instanceof TripleElement) {
				if (((TripleElement)proxy).getSubject() == null) {
					return (TripleElement)proxy;
				}
				else {
					return getProxyWithNullSubject(((TripleElement)proxy));
				}
			}
		}
		return null;
	}

	/**
	 * Returns the bottom triple whose subject was replaced.
	 * @param pattern
	 * @param proxyFor
	 * @param assignedNode
	 * @return
	 */
	private TripleElement assignNullSubjectInProxies(TripleElement pattern,
			TripleElement proxyFor, Node assignedNode) {
		if (pattern.getSubject() instanceof ProxyNode) {
			Object proxy = ((ProxyNode)pattern.getSubject()).getProxyFor();
			if (proxy instanceof TripleElement) {
//				((ProxyNode)pattern.getSubject()).setReplacementNode(assignedNode);
				if (((TripleElement)proxy).getSubject() == null) {
					// this is the bottom of the recursion
					((TripleElement)proxy).setSubject(assignedNode);
					return (TripleElement) proxy;
				}
				else {
					// recurse down
					TripleElement bottom = assignNullSubjectInProxies(((TripleElement)proxy), proxyFor, assignedNode);
					// make the proxy next and reassign this subject as assignedNode
					((ProxyNode)((TripleElement)proxy).getSubject()).setReplacementNode(assignedNode);
					((TripleElement)proxy).setSubject(assignedNode);
					if (bottom.getNext() == null) {
						bottom.setNext(pattern);
					}
					return bottom;
				}
			}
		}
		return null;
	}

	public NodeType ontConceptTypeToNodeType(OntConceptType octype) throws TranslationException {
//		public static enum NodeType {ClassNode, InstanceNode, PropertyNode, DataTypeProperty, ObjectProperty, VariableNode}
//		enum OntConceptType {
//			CLASS,
//			CLASS_PROPERTY,
//			DATATYPE_PROPERTY,
//			DATATYPE,
//			ANNOTATION_PROPERTY,
//			INSTANCE,
//			VARIABLE
		if (octype == null) {
			throw new TranslationException("ontConceptTypeToNodeType called with null type");
		}
		if (octype.equals(OntConceptType.CLASS)){
			return NodeType.ClassNode;
		}
		if (octype.equals(OntConceptType.CLASS_PROPERTY)) {
			return NodeType.ObjectProperty;
		}
		if (octype.equals(OntConceptType.DATATYPE_PROPERTY)) {
			return NodeType.DataTypeProperty;
		}
		if (octype.equals(OntConceptType.RDF_PROPERTY)) {
			return NodeType.PropertyNode;
		}
		if (octype.equals(OntConceptType.INSTANCE)) {
			return NodeType.InstanceNode;
		}
		if (octype.equals(OntConceptType.VARIABLE)) {
			return NodeType.VariableNode;
		}
		if (octype.equals(OntConceptType.ANNOTATION_PROPERTY)) {
			return NodeType.PropertyNode;
		}
		else if (octype.equals(OntConceptType.FUNCTION_DEFN)) {
//			System.err.println("Trying to convert OntConceptType FUNCTION_DEFN to a Node Type; this needs resolution.");
			return NodeType.InstanceNode;
		}
		throw new TranslationException("OntConceptType '" + octype.toString() + "' not yet mapped to NodeType");
	}

	protected Object getTarget() {
		return target;
	}

	protected void setTarget(Object target) {
		this.target = target;
	}

	protected Object getEncapsulatingTarget() {
		return encapsulatingTarget;
	}

	protected void setEncapsulatingTarget(Object encapsulatingTarget) {
		this.encapsulatingTarget = encapsulatingTarget;
	}

	protected RulePart getRulePart() {
		return rulePart;
	}

	protected void setRulePart(RulePart rulePart) {
		this.rulePart = rulePart;
	}
}
