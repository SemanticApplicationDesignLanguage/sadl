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

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.impl.CompositeNodeWithSemanticElement;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextSyntaxDiagnostic;
//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.Junction.JunctionType;
import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.Literal.LiteralType;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.ProxyNode;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.model.gp.Test.ComparisonType;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.model.persistence.SadlPersistenceFormat;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.refactoring.RefactoringHelper;
import com.ge.research.sadl.sADL.BinaryOperation;
import com.ge.research.sadl.sADL.BooleanLiteral;
import com.ge.research.sadl.sADL.Declaration;
import com.ge.research.sadl.sADL.Expression;
import com.ge.research.sadl.sADL.Name;
import com.ge.research.sadl.sADL.NumberLiteral;
import com.ge.research.sadl.sADL.SadlModel;
import com.ge.research.sadl.sADL.StringLiteral;
import com.ge.research.sadl.sADL.SubjHasProp;
import com.ge.research.sadl.sADL.UnaryExpression;
import com.ge.research.sadl.utils.SadlProjectHelper;
import com.google.common.collect.Iterables;
import com.google.inject.Inject;


public abstract class SadlModelProcessor implements IModelProcessor {
public static final String THERE_EXISTS = "thereExists";
	//    private static final Logger logger = LoggerFactory.getLogger(SadlModelProcessor.class);
    private Object target = null;	// the instance of Rule, Query, Equation, External, or Test into which we are trying to put the translation
    private List<IFTranslationError> errors = null;
    private Object encapsulatingTarget = null;	// when a query is in a test
    public enum RulePart {PREMISE, CONCLUSION, NOT_A_RULE}
    private RulePart rulePart = RulePart.NOT_A_RULE;
    
    @Inject
    private ISadlImplicitModelContentProvider implicitModelContentProvider;
    
    @Inject
    protected SadlProjectHelper projectHelper;
//    
	@Inject
	protected RefactoringHelper refactoringHelper;

	public abstract Object processExpression(EObject expr) throws InvalidNameException, InvalidTypeException, TranslationException ;
	
	public static String getOwlModelFormat(ProcessorContext context) {
		String format = SadlPersistenceFormat.RDF_XML_ABBREV_FORMAT; // default
		if (context != null) {
			String pv = context.getPreferenceValues().getPreference(SadlPreferences.OWL_MODEL_FORMAT);
			if (pv != null && pv.length() > 0) {
				format = pv;
			}
		}
		return format;
	}
	
	/**
	 * Method to check the SadlModel AST and determine if it has any syntax errors
	 * @param model -- the SadlModel
	 * @return == true if valid syntax else false
	 */
	public boolean isAstSyntaxValid(SadlModel model) {
	    Iterable<XtextSyntaxDiagnostic> syntaxErrors = Iterables.<XtextSyntaxDiagnostic>filter(model.eResource().getErrors(), XtextSyntaxDiagnostic.class);
	    return !syntaxErrors.iterator().hasNext();
	}

	protected Object processExpression(BooleanLiteral expr) {
		Literal lit = new Literal(LiteralType.BooleanLiteral);
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

	protected Object processExpression(NumberLiteral expr) {
		Literal lit = new Literal(LiteralType.NumberLiteral);
		String origTxt = expr.getValue().toPlainString();
		lit.setOriginalText(origTxt);
		BigDecimal val = expr.getValue();
		try {
			if (!origTxt.contains(".") && !origTxt.contains("e")) {
				if (val.longValue() >= Integer.MAX_VALUE || val.longValue() <= Integer.MIN_VALUE) {
					long lval = Long.parseLong(origTxt);
					lit.setValue(lval);
					return lit;
				}
				else {
					int ival = Integer.parseInt(origTxt);
					lit.setValue(ival);
					return lit;
				}
			}
			if (val.doubleValue() >= Float.MAX_VALUE || val.doubleValue() <= Float.MIN_VALUE) {
				double dval = Double.parseDouble(origTxt);
				lit.setValue(dval);
				return lit;
			}
			else {
				float fval = Float.parseFloat(origTxt);
				lit.setValue(fval);
				return lit;
			}
		}
		catch (Throwable t) {
			lit.setValue(val);
			return lit;
		}
	}

	protected Object processExpression(StringLiteral expr) {
		Literal lit = new Literal(LiteralType.StringLiteral);
		lit.setValue(expr.getValue());
		lit.setOriginalText(expr.getValue());
		return lit;
	}

	/**
	 * Method to determine if an EObject is the declaration of that object or just a reference. Useful in
	 * distinguishing between an assignment (declaration) and a comparison, is or equals
	 * @param expr
	 * @return
	 */
	public static boolean isDeclaration(EObject expr) {
		if (expr instanceof SubjHasProp) {
			return isDeclaration(((SubjHasProp) expr).getLeft());
		} else if (expr instanceof BinaryOperation) {
			if (isDeclaration(((BinaryOperation) expr).getLeft())) {
				return true;
			}
			if (isDeclaration(((BinaryOperation) expr).getRight())) {
				return true;
			}
		} else if (expr instanceof UnaryExpression && ((UnaryExpression) expr).getExpr() instanceof Declaration) {
			return true;
		} else if (expr instanceof Declaration) {
			return true;
		}
		else if (expr instanceof Name) {
			if (((Name)expr).getName().equals(expr)) {
				return true;
			}
		}
		return false;
	}
	
	public static boolean isSparqlQuery(String litObj) {
		litObj = litObj.trim();
		litObj = SadlUtils.stripQuotes(litObj);
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

	public static Node nodeCheck(Object nodeObj) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (nodeObj == null) {
			return null;
		}
		if (nodeObj instanceof Node) {
			return (Node) nodeObj; 
		}
		else if (nodeObj instanceof TripleElement && ((TripleElement)nodeObj).getPredicate() == null 
					&& ((TripleElement)nodeObj).getObject() == null
					&& ((TripleElement)nodeObj).getSubject() != null) {
			return ((TripleElement)nodeObj).getSubject();
		}
		else if (nodeObj instanceof GraphPatternElement) {
			return new ProxyNode((GraphPatternElement) nodeObj);
		}
		throw new TranslationException("nodeCheck called with non-Node, non-GraphPatternElement argument: " + nodeObj.getClass().getCanonicalName());
	}
	
	/**
	 * Method to create a unary BuiltinElement
	 * @param sexpr
	 * @param unaryOperationName
	 * @param sobj
	 * @return
	 * @throws InvalidNameException
	 * @throws InvalidTypeException
	 * @throws TranslationException
	 */
	protected Object createUnaryBuiltin(EObject sexpr, String unaryOperationName, Object sobj) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (sobj instanceof Literal && BuiltinType.getType(unaryOperationName).equals(BuiltinType.Minus)) {
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
				lhs = createUnaryBuiltin(sexpr, unaryOperationName, lhs);
				rhs = createUnaryBuiltin(sexpr, unaryOperationName, rhs);
				junc.setLhs(lhs);
				junc.setRhs(rhs);
			}
			return junc;
		}
		if (BuiltinType.getType(unaryOperationName).equals(BuiltinType.Equal)) {
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
		BuiltinElement builtin = new BuiltinElement(unaryOperationName, sexpr);
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

	protected TripleModifierType getTripleModifierType(BuiltinType btype) {
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
	 * Method to create a binary BuiltinElement with the given name, context, and arguments
	 * 
	 * @param builtinName -- name of the BuiltinElement to be created
	 * @param context -- the Xtext parse tree object associated with the BuiltinElement
	 * @param lobj -- the result of processing the left side of the binary operation
	 * @param robj -- the result of processing the right side of the binary opearation
	 * 
	 */
	public GraphPatternElement createBinaryBuiltin(String builtinName, EObject context, Object lobj, Object robj)
			throws InvalidNameException, InvalidTypeException, TranslationException {
				if (builtinName.equals(JunctionType.AND_ALPHA) || builtinName.equals(JunctionType.AND_SYMBOL)
						|| builtinName.equals(JunctionType.OR_ALPHA) || builtinName.equals(JunctionType.OR_SYMBOL)) {
					Junction jct = new Junction();
					jct.setJunctionName(builtinName);
					jct.setLhs(nodeCheck(lobj));
					jct.setRhs(nodeCheck(robj));
					return jct;
				} else if (builtinName.equals("is") && getTarget() instanceof Test) {
					if (lobj instanceof NamedNode && ((NamedNode)lobj).getNodeType().equals(NodeType.InstanceNode) &&
							robj instanceof VariableNode && ((VariableNode)robj).getType() instanceof NamedNode &&
							((NamedNode) ((VariableNode)robj).getType()).getNodeType().equals(NodeType.ClassNode) &&
							((VariableNode)robj).isCRulesVariable()) {
						// the right was a Declaration so this is of the form <inst> is a <class>
						TripleElement te = new TripleElement((Node) lobj, new RDFTypeNode(), ((VariableNode)robj).getType());
						return te;
					}
					else {
						((Test)getTarget()).setLhs(lobj);
						((Test)getTarget()).setRhs(robj);
						((Test)getTarget()).setCompName(builtinName);
						return null;
						}
				} else {
					BuiltinElement builtin = new BuiltinElement(transformOpName(builtinName), context);
					if (lobj != null) {
						builtin.addArgument(nodeCheck(lobj));
					}
					if (robj != null) {
						builtin.addArgument(nodeCheck(robj));
					}
					return builtin;
				}
			}

	protected String transformOpName(String op) {
		if (op.equals("there exists")) {
			return THERE_EXISTS;
		} else if (op.equals("=") || op.equals("==")) {
			return "is";
		}
		return op;
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

	public void createSadlImplicitModel(File implicitModelFile) throws IOException {
		implicitModelContentProvider.createImplicitModel(implicitModelFile, true);
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
	public Node validateNode(Node node) throws InvalidNameException, TranslationException {
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
				validateNamedNode((NamedNode)node);
				((NamedNode)node).setValidated(true);
			}
		}
		return node;
	}
	
	public abstract NamedNode validateNamedNode(NamedNode node);

	public ConceptType nodeTypeToConceptType(NodeType nt) throws TranslationException {
		if (nt.equals(NodeType.ClassNode)) {
			return ConceptType.ONTCLASS;
		}
		else if (nt.equals(NodeType.ClassListNode)) {
			return ConceptType.ONTCLASSLIST;
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
		else if (nt.equals(NodeType.AnnotationProperty)) {
			return ConceptType.ANNOTATIONPROPERTY;
		}
		else if (nt.equals(NodeType.VariableNode)) {
			return ConceptType.VARIABLE;
		}
		else if (nt.equals(NodeType.DataTypeNode)) {
			return ConceptType.RDFDATATYPE;
		}
		else if (nt.equals(NodeType.DataTypeListNode)) {
			return ConceptType.RDFDATATYPELIST;
		}
		else if (nt.equals(NodeType.PropertyNode)) {
			return ConceptType.RDFPROPERTY;
		}
		else if (nt.equals(NodeType.FunctionNode)) {
			return ConceptType.FUNCTION_DEFN;
		}
		else {
			throw new TranslationException("NodeType '" + nt.toString() + "' cannot be converted to a ConceptType");
		}
	}

	public static NodeType conceptTypeToNodeType(ConceptType ct) throws TranslationException {
		if (ct.equals(ConceptType.ONTCLASS)) {
			return NodeType.ClassNode;
		}
		else if (ct.equals(ConceptType.ONTCLASSLIST)) {
			return NodeType.ClassListNode;
		}
		else if (ct.equals(ConceptType.INDIVIDUAL)) {
			return NodeType.InstanceNode;
		}
		else if (ct.equals(ConceptType.DATATYPEPROPERTY)) {
			return NodeType.DataTypeProperty;
		}
		else if (ct.equals(ConceptType.OBJECTPROPERTY)) {
			return NodeType.ObjectProperty;
		}
		else if (ct.equals(ConceptType.VARIABLE)) {
			return NodeType.VariableNode;
		}
		else if (ct.equals(ConceptType.RDFDATATYPE)) {
			return NodeType.DataTypeNode;
		}
		else if (ct.equals(ConceptType.RDFDATATYPELIST)) {
			return NodeType.DataTypeListNode;
		}
		else if (ct.equals(ConceptType.FUNCTION_DEFN)) {
			return NodeType.FunctionNode;
		}
		else if (ct.equals(ConceptType.RDFPROPERTY)) {
			return NodeType.PropertyNode;
		}
		else {
			throw new TranslationException("ConceptType '" + ct.toString() + "' cannot be converted to a NodeType");
		}
	}

//	private ConceptName validateConceptName(ConceptName conceptName) {
//		// TODO Auto-generated method stub
//		return conceptName;
//	}
//	
//	private void addError(IFTranslationError error) {
//		if (errors == null) {
//			errors = new ArrayList<IFTranslationError>();
//		}
//		errors.add(error);
//	}

	public List<IFTranslationError> getErrors() {
		return errors;
	}
	
//	private boolean doVariableSubstitution(GraphPatternElement gpe, VariableNode v1, VariableNode v2) {
//		boolean retval = false;
//		do {
//			if (gpe instanceof TripleElement) {
//				if (((TripleElement)gpe).getSubject().equals(v1)) {
//					((TripleElement)gpe).setSubject(v2);
//					retval = true;
//				}
//				else if (((TripleElement)gpe).getObject().equals(v1)) {
//					((TripleElement)gpe).setObject(v2);
//					retval = true;
//				}
//			}
//			else if (gpe instanceof BuiltinElement) {
//				List<Node> args = ((BuiltinElement)gpe).getArguments();
//				for (int j = 0; j < args.size(); j++) {
//					if (args.get(j).equals(v1)) {
//						args.set(j, v2);
//						retval = true;
//					}
//				}
//			}
//			else if (gpe instanceof Junction) {
////				logger.error("Not yet handled");
//			}
//			gpe = gpe.getNext();
//		} while (gpe != null);
//		return retval;
//	}
//
//	private boolean isModifiedTripleViaBuiltin(Object robj) {
//		if (robj instanceof TripleElement && ((TripleElement)robj).getNext() instanceof BuiltinElement) {
//			BuiltinElement be = (BuiltinElement) ((TripleElement)robj).getNext();
//			if (((TripleElement)robj).getPredicate() instanceof RDFTypeNode) {
//				if (isModifiedTriple(be.getFuncType())) {
//					Node subj = ((TripleElement)robj).getSubject();
//					Node arg = (be.getArguments() != null && be.getArguments().size() > 0) ? be.getArguments().get(0) : null;
//					if (subj == null && arg == null) {
//						return true;
//					}
//					if (subj != null && arg != null && subj.equals(arg)) {
//						return true;
//					}
//				}
//			}
//			else {
//				if (isModifiedTriple(be.getFuncType()) && ((TripleElement)robj).getObject().equals(be.getArguments().get(0))) {
//					return true;
//				}
//			}
//		}
//		return false;
//	}
//
//	private boolean isComparisonViaBuiltin(Object robj, Object lobj) {
//		if (robj instanceof TripleElement && lobj instanceof Node &&
//				((TripleElement)robj).getNext() instanceof BuiltinElement) {
//			BuiltinElement be = (BuiltinElement) ((TripleElement)robj).getNext();
//			if (isComparisonBuiltin(be.getFuncName()) && be.getArguments().size() == 1) {
//				return true;
//			}
//		}
//		return false;
//	}
//
//	private TripleElement getProxyWithNullSubject(TripleElement pattern) {
//		if (pattern.getSubject() instanceof ProxyNode) {
//			Object proxy = ((ProxyNode)pattern.getSubject()).getProxyFor();
//			if (proxy instanceof TripleElement) {
//				if (((TripleElement)proxy).getSubject() == null) {
//					return (TripleElement)proxy;
//				}
//				else {
//					return getProxyWithNullSubject(((TripleElement)proxy));
//				}
//			}
//		}
//		return null;
//	}
//
//	/**
//	 * Returns the bottom triple whose subject was replaced.
//	 * @param pattern
//	 * @param proxyFor
//	 * @param assignedNode
//	 * @return
//	 */
//	private TripleElement assignNullSubjectInProxies(TripleElement pattern,
//			TripleElement proxyFor, Node assignedNode) {
//		if (pattern.getSubject() instanceof ProxyNode) {
//			Object proxy = ((ProxyNode)pattern.getSubject()).getProxyFor();
//			if (proxy instanceof TripleElement) {
////				((ProxyNode)pattern.getSubject()).setReplacementNode(assignedNode);
//				if (((TripleElement)proxy).getSubject() == null) {
//					// this is the bottom of the recursion
//					((TripleElement)proxy).setSubject(assignedNode);
//					return (TripleElement) proxy;
//				}
//				else {
//					// recurse down
//					TripleElement bottom = assignNullSubjectInProxies(((TripleElement)proxy), proxyFor, assignedNode);
//					// make the proxy next and reassign this subject as assignedNode
//					((ProxyNode)((TripleElement)proxy).getSubject()).setReplacementNode(assignedNode);
//					((TripleElement)proxy).setSubject(assignedNode);
//					if (bottom.getNext() == null) {
//						bottom.setNext(pattern);
//					}
//					return bottom;
//				}
//			}
//		}
//		return null;
//	}

	/**
	 * Method to convert a SADL Xtext OntConceptType to the intermediate form's NodeType
	 * @param octype
	 * @return
	 * @throws TranslationException
	 */
	public NodeType ontConceptTypeToNodeType(OntConceptType octype) throws TranslationException {
		if (octype == null) {
			throw new TranslationException("ontConceptTypeToNodeType called with null type");
		}
		if (octype.equals(OntConceptType.CLASS)){
			return NodeType.ClassNode;
		}
		if (octype.equals(OntConceptType.CLASS_LIST)) {
			return NodeType.ClassListNode;
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
			return NodeType.AnnotationProperty;
		}
		else if (octype.equals(OntConceptType.FUNCTION_DEFN)) {
//			System.err.println("Trying to convert OntConceptType FUNCTION_DEFN to a Node Type; this needs resolution.");
			return NodeType.FunctionNode;
		}
		else if (octype.equals(OntConceptType.STRUCTURE_NAME)) {
			return NodeType.InstanceNode;
		}
		else if (octype.equals(OntConceptType.DATATYPE)) {
			return NodeType.DataTypeNode;
		}
		else if (octype.equals(OntConceptType.DATATYPE_LIST)) {
			return NodeType.DataTypeListNode;
		}
		throw new TranslationException("OntConceptType '" + octype.toString() + "' not yet mapped to NodeType");
	}

	public Object getTarget() {
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

	public RulePart getRulePart() {
		return rulePart;
	}

	protected void setRulePart(RulePart rulePart) {
		this.rulePart = rulePart;
	}

	public String getSourceText(EObject po) {
		INode node = getParserObjectNode(po);
		if (node != null) {
			String txt = NodeModelUtils.getTokenText(node);
			return txt.trim() + ".\n";
		}
		return null;
	}

	protected INode getParserObjectNode(EObject po) {
		Object r = po.eResource();
		if (r instanceof XtextResource) {
			INode root = ((XtextResource) r).getParseResult().getRootNode();
	        for(INode node : root.getAsTreeIterable()) {   
	        	if (node instanceof CompositeNodeWithSemanticElement) {
	        		EObject semElt = ((CompositeNodeWithSemanticElement)node).getSemanticElement();
	        		if (semElt != null && semElt.equals(po)) {
	        			// this is the one!
       					return node;
	        		}
	        	}
	        }
		}
		org.eclipse.emf.common.util.TreeIterator<EObject> titr = po.eAllContents();
		while (titr.hasNext()) {
			EObject el = titr.next();
//TODO what's supposed to happen here?
			int i = 0;
		}
		return null;
	}

}
