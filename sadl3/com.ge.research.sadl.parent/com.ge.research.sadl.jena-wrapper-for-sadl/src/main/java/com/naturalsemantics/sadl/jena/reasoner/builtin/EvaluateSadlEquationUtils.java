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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.datatypes.RDFDatatype;
import org.apache.jena.datatypes.xsd.impl.XSDBaseNumericType;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Node_Literal;
import org.apache.jena.graph.impl.LiteralLabelFactory;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.vocabulary.XSD;

import com.ge.research.sadl.jena.reasoner.builtin.Utils;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.Equation;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.Literal.LiteralType;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.sADL.Constant;

/**
 * This class is used to evaluate a SADL equation. An equation can be evaluated in two different circumstances:
 *   1. During editing with an Expr: statement.
 *   2. During inference when used in a Jena rule.
 * 
 * @author Natural Semantics, LLC
 *
 */
public class EvaluateSadlEquationUtils {

	private List<ModelError> newErrors = null;

	public List<ModelError> getErrors() {
		List<ModelError> returning = newErrors;
		newErrors = null;
		return returning;
	}

	protected void addError(ModelError newError) {
		if (newErrors == null) {
			newErrors = new ArrayList<ModelError>();
		}
		newErrors.add(newError);
	}

	/**
	 * Method to evaluate a BuiltinElement containing a SADL Equation directly, 
	 * as in the processing of an "Expr: <builtin>." statement.
	 * 
	 * @param bi -- the BuiltinElement to be evaluated
	 * @return -- a SADL Node containing the result of the evaluation
	 */
	public Node evaluateSadlEquation(BuiltinElement bi) {
		Equation eq = bi.getInModelReferencedEquation();
		if (eq.isExternal()) {
			// this is an equation whose implementation is outside of the SADL environment
			String externaluri = eq.getExternalUri();
			if (externaluri !=  null) {
				int lastDot = externaluri.lastIndexOf('.');
				if (lastDot > 0) {
					String classname = externaluri.substring(0, lastDot);
					String methname =  externaluri.substring(lastDot + 1);
					Class<?> clazz = getMatchingClassOfExternalUri(classname);
					Object arg0AsInstanceOfClazz = null;
					List<Method> matchingStaticMethods = getMatchingMethodsOfExternalUri(clazz, methname, true);
					Method bestMatch = getBestMatch(bi, matchingStaticMethods, false);
					boolean methodOnClassOfFirstArgument = false;
					if (bestMatch == null) {
						Class<?> arg0Class = null;
						int numArgs = 0;
						if (bi.getArguments() != null && bi.getArguments().size() > 0) {
							numArgs = bi.getArguments().size();
							Node arg0 = bi.getArguments().get(0);
							if (arg0 instanceof com.ge.research.sadl.model.gp.Literal) {
								arg0AsInstanceOfClazz = sadlNodeAsInstanceOfArgClass(clazz, arg0);
								if (arg0AsInstanceOfClazz != null) {
									arg0Class = arg0AsInstanceOfClazz.getClass();
								}
							}
						}
						if (arg0Class != null && clazz != null && arg0Class.equals(clazz)) {
							// we want to find the method that will be invoked for the instance of
							// this class which is the first argument
							methodOnClassOfFirstArgument = true;
						}

						List<Method> matchingMethods = getMatchingMethodsOfExternalUri(clazz, methname, false);
						// now find one that matches the arguments if possible
					
						bestMatch = getBestMatch(bi, matchingMethods, methodOnClassOfFirstArgument);
					}
					if (bestMatch != null) {
						bestMatch.setAccessible(true);
				        Object[] args = getArgs(bestMatch, bi.getArguments(), eq, methodOnClassOfFirstArgument);
						try {
							Object result;
							boolean isStatic = Modifier.isStatic(bestMatch.getModifiers());
							if (isStatic) {
								result = bestMatch.invoke(null, args);
							}
							else if (methodOnClassOfFirstArgument) {
								if (arg0AsInstanceOfClazz != null) {
									result = bestMatch.invoke(arg0AsInstanceOfClazz, args);
								}
								else {
									addError(new ModelError("first argument must be an instance of the class containing the method", ErrorType.ERROR));
									result = null;
								}
							}
							else {
								result = bestMatch.invoke(clazz.getDeclaredConstructor().newInstance(), args);								
							}
							return convertResultToNode(result, bi);
						} catch (IllegalAccessException e) {
							addError(new ModelError(e.getMessage(), ErrorType.ERROR));
						} catch (IllegalArgumentException e) {
							addError(new ModelError(e.getMessage(), ErrorType.ERROR));
						} catch (InvocationTargetException e) {
							addError(new ModelError(e.getMessage(), ErrorType.ERROR));
						} catch (InstantiationException e) {
							addError(new ModelError(e.getMessage(), ErrorType.ERROR));
						} catch (NoSuchMethodException e) {
							addError(new ModelError(e.getMessage(), ErrorType.ERROR));
						} catch (SecurityException e) {
							addError(new ModelError(e.getMessage(), ErrorType.ERROR));
						}
					}
				}
				else {
					addError(new ModelError("Invalid class identifier: " + externaluri, ErrorType.ERROR));
				}
			}
			addError(new ModelError("no method found matching '" + externaluri + "'", ErrorType.ERROR));
		}
		return null;
	}

	/**
	 * Method to try to see the SADL Node arg as the Class paramClass
	 * @param paramClass
	 * @param arg
	 * @return
	 */
	private Object sadlNodeAsInstanceOfArgClass(Class<?> paramClass, Node arg) {
		if (arg instanceof com.ge.research.sadl.model.gp.Literal) {
			LiteralType dt = ((com.ge.research.sadl.model.gp.Literal)arg).getLiteralType();
			if (dt != null && paramClass != null) {
				if (dt.equals(LiteralType.StringLiteral)) {
					return ((com.ge.research.sadl.model.gp.Literal)arg).getOriginalText();
				}
				else if (dt.equals(LiteralType.BooleanLiteral)) {
					try {
						Boolean b = Boolean.valueOf(((com.ge.research.sadl.model.gp.Literal)arg).getOriginalText());
						return b;
					}
					catch (Exception e) {	
					}
				}
				else if (dt.equals(LiteralType.NumberLiteral)) {
					String numberText = ((com.ge.research.sadl.model.gp.Literal)arg).getOriginalText();
					return stringValueToTypedNumber(paramClass, numberText);
				}
			}
		}
		return null;
	}

	protected Object stringValueToTypedNumber(Class<?> paramClass, String numberText) {
		if (paramClass.equals(Double.class)) {
			try {
				Double d = Double.parseDouble(numberText);
				return d;
			}
			catch (Exception e) {			
			}
		}
		else if (paramClass.equals(Float.class)) {
			try {
				Float f = Float.parseFloat(numberText);
				return f;
			}
			catch (Exception e) {	
			}
		}
		else if (paramClass.equals(Integer.class)) {
			try {
				Integer i = Integer.parseInt(numberText);
				return i;
			}
			catch (Exception e) {	
			}
		}
		else if (paramClass.equals(Long.class)) {
			try {
				Long l = Long.parseLong(numberText);
				return l;
			}
			catch (Exception e) {
			}
		}
		return null;
	}

	/**
	 * Method to call a Java class method using reflection. This is called from the Jena built-in
	 * EvaluateSadlEquation.
	 * 
	 * @param externaluri -- the package, class, and method name, e.g., "java.lang.String.substring", to be invoked
	 * @param restOfArgs -- the arguments to be passed into the method
	 * @param argTypes 
	 * @param varArgs 
	 * @param returnTypes 
	 * @return -- a Jena Node containing the results of the evaluation
	 */
	public org.apache.jena.graph.Node evaluateSadlEquation(String externaluri, List<org.apache.jena.graph.Node> restOfArgs, List<org.apache.jena.graph.Node> argTypes, boolean varArgs, List<org.apache.jena.graph.Node> returnTypes) {
		int lastDot = externaluri.lastIndexOf('.');
		if (lastDot > 0) {
			String classname = externaluri.substring(0, lastDot);
			String methname =  externaluri.substring(lastDot + 1);
			Class<?> clazz = getMatchingClassOfExternalUri(classname);
			Object arg0AsInstanceOfClazz = null;
			List<Method> matchingStaticMethods = getMatchingMethodsOfExternalUri(clazz, methname, true);
			Method bestMatch = getBestMatch(argTypes, restOfArgs, varArgs, matchingStaticMethods, false);	
			boolean methodOnClassOfFirstArgument = false;
			if (bestMatch == null) {
				Class<?> arg0Class = null;
				int numArgs =0;
				if (restOfArgs != null && restOfArgs.size() > 0) {
					numArgs = restOfArgs.size();
					org.apache.jena.graph.Node arg0 = restOfArgs.get(0);
					RDFDatatype dt = null;
					if (arg0 instanceof Node_Literal) {
						arg0AsInstanceOfClazz = jenaNodeAsInstanceOfArgClass(clazz, arg0);
						if (arg0AsInstanceOfClazz != null) {
							arg0Class = arg0AsInstanceOfClazz.getClass();
						}
					}
				}
				if (arg0Class != null && clazz != null && arg0Class.equals(clazz)) {
					// we want to find the method that will be invoked for the instance of
					// this class which is the first argument
					methodOnClassOfFirstArgument = true;
				}
	
				
				List<Method> matchingMethods = getMatchingMethodsOfExternalUri(clazz, methname, false);
				// now find one that matches the arguments if possible
				
				bestMatch = getBestMatch(argTypes, restOfArgs, varArgs, matchingMethods, methodOnClassOfFirstArgument);
			}
			if (bestMatch != null) {
				bestMatch.setAccessible(true);
		        Object[] args = getArgsFromJenaBuiltinCall(bestMatch, restOfArgs, varArgs, methodOnClassOfFirstArgument);
				try {
					Object result;
					boolean isStatic = Modifier.isStatic(bestMatch.getModifiers());
					if (isStatic) {
						result = bestMatch.invoke(null, args);
					}
					else if (methodOnClassOfFirstArgument) {
						if (arg0AsInstanceOfClazz != null) {
							result = bestMatch.invoke(arg0AsInstanceOfClazz, args);
						}
						else {
							addError(new ModelError("first argument must be an instance of the class containing the method", ErrorType.ERROR));
							result = null;
						}
					}
					else {
						result = bestMatch.invoke(clazz.getDeclaredConstructor().newInstance(), args);								
					}
//			        Object inst = clazz.getConstructor(returnType.class).newInstance(base);
					return convertResultToNode(result);
				} catch (IllegalAccessException e) {
					addError(new ModelError(e.getMessage(), ErrorType.ERROR));
				} catch (IllegalArgumentException e) {
					addError(new ModelError(e.getMessage(), ErrorType.ERROR));
				} catch (InvocationTargetException e) {
					addError(new ModelError(e.getMessage(), ErrorType.ERROR));
				} catch (InstantiationException e) {
					addError(new ModelError(e.getMessage(), ErrorType.ERROR));
				} catch (NoSuchMethodException e) {
					addError(new ModelError(e.getMessage(), ErrorType.ERROR));
				} catch (SecurityException e) {
					addError(new ModelError(e.getMessage(), ErrorType.ERROR));
				}
			}
			else {
				addError(new ModelError("no method found matching '" + externaluri + "'", ErrorType.ERROR));
			}
		}
		else {
			addError(new ModelError("Invalid class identifier: " + externaluri, ErrorType.ERROR));
		}
		return null;
	}

	private Object jenaNodeAsInstanceOfArgClass(Class<?> paramClass, org.apache.jena.graph.Node arg) {
		if (arg instanceof Node_Literal) {
			if (isNumber(paramClass)) {
				String argValue = ((Node_Literal)arg).getLiteralValue().toString();
				return stringValueToTypedNumber(paramClass, argValue);
			}
			else if (paramClass.equals(String.class)) {
				return ((Node_Literal)arg).getLiteralValue().toString();
			}
			else if (paramClass.equals(Boolean.class)) {
				try {
					Boolean b = Boolean.valueOf(((Node_Literal)arg).getLiteralValue().toString());
					return b;
				}
				catch (Exception e) {	
				}
			}
		}
		return null;
	}

	/**
	 * Method to convert the return from invocation of the method to a Jena node
	 * @param result -- returned value of the reflection invocation
	 * @return -- the Jena Node
	 */
	private org.apache.jena.graph.Node convertResultToNode(Object result) {
//		if (isNumber(result)) {
			return NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral(result));
//		}
//		else {
//			return NodeFactory.createLiteral(LiteralLabelFactory.createTypedLiteral(result));
//		}
	}

	/**
	 * Method to convert the return from Equation invocation to a SADL node containing the desired return type.
	 * @param result
	 * @param bi
	 * @return
	 */
	private Node convertResultToNode(Object result, BuiltinElement bi) {
		if (isNumber(result)) {
			return new com.ge.research.sadl.model.gp.Literal(result, null, LiteralType.NumberLiteral);
		}
		else {
			return new com.ge.research.sadl.model.gp.Literal(result.toString(), null, LiteralType.StringLiteral);
		}
	}

	/**
	 * Method to get the arguments from the BuiltinElement, making them compatible with the Method to be invoked.
	 * @param bestMatch -- the Method to be invoked
	 * @param arguments -- the arguments as SADL nodes
	 * @param eq 
	 * @param methodOnClassOfFirstArgument -- true if the method should be invoked on the instance of the class identified by the first argument
	 * @return
	 */
	private Object[] getArgs(Method bestMatch, List<com.ge.research.sadl.model.gp.Node> arguments, Equation eq, boolean methodOnClassOfFirstArgument) {
		Class<?>[] paramTypes = bestMatch.getParameterTypes();
		int numParamTypes = paramTypes.length;
		int numArguments = arguments.size();
		int argIndexOffset = methodOnClassOfFirstArgument ? 1 : 0;
		Object[] argsPlus = new Object[numParamTypes];
		int variableNumArgsIndex = -1;
		Object[] argArray = null;
		for(int i = argIndexOffset; i < numArguments; i++){
			int ptIndex = variableNumArgsIndex < 0 ? i - argIndexOffset : variableNumArgsIndex;
			Class<?> argtype = paramTypes[ptIndex];
			Node arg = arguments.get(i);
			if (arg instanceof com.ge.research.sadl.model.gp.Literal) {
				Object val = ((com.ge.research.sadl.model.gp.Literal)arg).getValue();
				if (eq != null && eq.getArgumentTypes() != null && eq.getArgumentTypes().size() > i) {
					Node eqArgType = eq.getArgumentTypes().get(i);
					if (eqArgType instanceof NamedNode) {
						if(eqArgType.getURI().equals(XSD.xint.getURI())) {
							argsPlus[ptIndex] = Integer.parseInt(val.toString());
						}
						else if (eqArgType.getURI().equals(XSD.xlong.getURI())) {
							argsPlus[ptIndex] = Long.parseLong(val.toString());
						}
						else if (eqArgType.getURI().equals(XSD.xfloat.getURI())) {
							argsPlus[ptIndex] = Float.parseFloat(val.toString());
						}
						else if (eqArgType.getURI().equals(XSD.xdouble.getURI())) {
							argsPlus[ptIndex] = Double.parseDouble(val.toString());
						}
						else if (eqArgType.getURI().equals(XSD.xboolean.getURI())) {
							argsPlus[ptIndex] = Boolean.parseBoolean(val.toString());
						}
						else if (eqArgType.getURI().equals(XSD.decimal.getURI())) {
							// need to try to create an argument matching the expected argtype
							Object simpleType;
							if (((com.ge.research.sadl.model.gp.Literal) arg).getOriginalText() != null) {
								simpleType = getSimpleTypeFromVal(arg, argtype, ((com.ge.research.sadl.model.gp.Literal) arg).getOriginalText());
							}
							else {
								simpleType = getSimpleTypeFromVal(arg, argtype, val.toString());
							}
							if (simpleType != null) {
								argsPlus[ptIndex] = simpleType;
							}
							else {
								addError(new ModelError("Unable to convert xsd:decimal to desired type ("+ argtype.getCanonicalName() + ")", ErrorType.ERROR));
							}
						}
						else if (eqArgType.getURI().equals(XSD.xstring.getURI())) {
							argsPlus[ptIndex] = val.toString();
						}
					}
				}
				else {
					Object simpleType = getSimpleTypeFromVal(arg, argtype, val);
					if (simpleType != null) {
						argsPlus[ptIndex] = simpleType;
					}
					else if (argtype.getName().contentEquals("[Ljava.lang.Object;")){
						if (argArray == null) {
							argArray = new Object[numArguments - ptIndex];
							argArray[0] = val;
							argsPlus[ptIndex] = argArray;
							variableNumArgsIndex = ptIndex;
						}
						else {
							argArray[i - variableNumArgsIndex] = val;
						}
					}
				}
			}
		}
		return argsPlus;
	}

	private Object getSimpleTypeFromVal(Node arg, Class<?> argtype, Object val) {
		if (argtype.getCanonicalName().equals("long")) {
			return Long.parseLong(val.toString());
		}
		else if (argtype.getName().equals("int")) {
			return Integer.parseInt(val.toString());
		}
		else if (argtype.getName().equals("float")) {
			return Float.parseFloat(val.toString());
		}
		else if (argtype.getName().equals("double")) {
			return Double.parseDouble(val.toString());
		}
		else if (argtype.getName().equals("boolean")) {
			return Boolean.parseBoolean(val.toString());
		}
		else if (argtype.getName().contentEquals("java.lang.String")) {
			return val.toString();
		}
		else if (argtype.getName().equals("java.lang.Object")) {
			return val;
		}
		return null;
	}

	/**
	 * Method to get the arguments passed into the Jena built-in, making them compatible with the Method to be invoked
	 * @param bestMatch -- the Method to be invoked
	 * @param arguments -- the arguments (Jena Nodes) passed into the Jena built-in
	 * @param methodOnClassOfFirstArgument -- true if the method should be invoked on the instance of the class identified by the first argument
	 * @return
	 */
	private Object[] getArgsFromJenaBuiltinCall(Method bestMatch, List<org.apache.jena.graph.Node> arguments, boolean varArgs, boolean methodOnClassOfFirstArgument) {
		Class<?>[] paramTypes = bestMatch.getParameterTypes();
		int numParamTypes = paramTypes.length;
		int numArguments = arguments.size();
		int argIndexOffset = methodOnClassOfFirstArgument ? 1 : 0;
		Object[] argsPlus = new Object[numParamTypes];
		int variableNumArgsIndex = -1;
		Object[] argArray = null;
		for(int i = argIndexOffset; i < numArguments; i++){
			int ptIndex = variableNumArgsIndex < 0 ? i - argIndexOffset : variableNumArgsIndex;
			Class<?> argtype = paramTypes[ptIndex];
			org.apache.jena.graph.Node arg = arguments.get(i);
			if (arg instanceof Node_Literal) {
				Object val = ((Node_Literal)arg).getLiteralValue();
				if (argtype.getCanonicalName().equals("long")) {
					argsPlus[ptIndex] = Long.parseLong(val.toString());
				}
				else if (argtype.getCanonicalName().equals("int")) {
					argsPlus[ptIndex] = Integer.parseInt(val.toString());
				}
				else if (argtype.getName().equals("float")) {
					argsPlus[ptIndex] = Float.parseFloat(val.toString());
				}
				else if (argtype.getName().equals("double")) {
					argsPlus[ptIndex] = Double.parseDouble(val.toString());
				}
				else if (argtype.getName().equals("boolean")) {
					argsPlus[ptIndex] = Boolean.parseBoolean(val.toString());
				}
				else if (argtype.getName().contentEquals("java.lang.String")) {
					argsPlus[ptIndex] = val.toString();
				}
				else if (argtype.getName().contentEquals("[Ljava.lang.Object;")){
					if (argArray == null) {
						argArray = new Object[numArguments - ptIndex];
						argArray[0] = val;
						argsPlus[ptIndex] = argArray;
						variableNumArgsIndex = ptIndex;
					}
					else {
						argArray[i - variableNumArgsIndex] = val;
					}
				}
			}
		}
		return argsPlus;
	}

	/**
	 * Method to determine the best match to the input arguments from among the matching methods of the identified Java Class
	 * @param bi -- the BuiltinElement containing the arguments and Java Class identification
	 * @param matchingMethods -- the list of Java Methods matching the identifying URI
	 * @param methodOnClassOfFirstArgument -- true if the method should be invoked on the instance of the class identified by the first argument
	 * @return -- the Java Method best matching the args (if any)
	 */
	private Method getBestMatch(BuiltinElement bi, List<Method> matchingMethods, boolean methodOnClassOfFirstArgument) {
		if (matchingMethods != null) {
			int effectiveNumArguments = methodOnClassOfFirstArgument ? bi.getArguments().size() - 1 : bi.getArguments().size();
			for (Method m : matchingMethods) {
				Class<?>[] paramTypes = m.getParameterTypes();
				int numParams = paramTypes.length;
				boolean variableNumParams = false;
				if (paramTypes != null && paramTypes.length > 0) {
					// is the last paramType a variable number of args?
					if (paramTypes[paramTypes.length - 1].getName().equals("[Ljava.lang.Object;")) {
						variableNumParams = true;
					}
					// check to see if this is a method on the class of the first argument
					if (!variableNumParams && !(numParams == effectiveNumArguments)) {
						continue;
					}
					boolean match = true;
					for (int i = 0;  i < paramTypes.length; i++) {
						Class<?> pt = paramTypes[i];
						String ptstr = pt.getTypeName();
						Node arg = methodOnClassOfFirstArgument ? bi.getArguments().get(i + 1) : bi.getArguments().get(i);
						LiteralType dt = null;
						if (arg instanceof com.ge.research.sadl.model.gp.Literal) {
							dt = ((com.ge.research.sadl.model.gp.Literal)arg).getLiteralType();
						}
						else if (arg instanceof Constant) {
							if(((Constant)arg).equals(SadlConstants.CONSTANT_PI) || ((Constant)arg).equals(SadlConstants.CONSTANT_E)){
								dt = LiteralType.NumberLiteral;
							}
							else {
								dt = LiteralType.StringLiteral;
							}
						}
						if (dt != null && ptstr != null) {
							if (isNumber(dt) && isNumber(ptstr)) {
								String ot = ((com.ge.research.sadl.model.gp.Literal)arg).getOriginalText();
								if (ot != null && ot.indexOf('.') < 0) {
									// int or long
									if (!ptstr.equals("long") && !ptstr.equals("int")) {
										match = false;
										break;
									}
								}
							}
							else if (ptstr.equals("java.lang.Object[]") || ptstr.equals("java.lang.Object")) {
								// any dt will work
							}
							else if (dt.equals(LiteralType.StringLiteral) && !ptstr.equals("java.lang.String")){
								match = false;
								break;
							}
							else if (dt.equals(LiteralType.BooleanLiteral) && !ptstr.equals("boolean")) {
								match = false;
								break;
							}
						}
					}
					if (match) {
						return m;
					}
				}
			}
		}
		return null;
	}

	/**
	 * Method to determine the best match to the input arguments from among the matching methods of the identified Java Class
	 * @param argTypes -- the arguments passed into the Jena built-in
	 * @param args 
	 * @param varArgs 
	 * @param matchingMethods -- the list of Java Methods matching the identifying URI
	 * @param methodOnClassOfFirstArgument -- true if the method should be invoked on the instance of the class identified by the first argument
	 * @return -- the Java Method best matching the args (if any)
	 */
	private Method getBestMatch(List<org.apache.jena.graph.Node> argTypes, List<org.apache.jena.graph.Node> args, boolean varArgs, List<Method> matchingMethods, boolean methodOnClassOfFirstArgument) {
		if (matchingMethods != null) {
			int effectiveNumArguments = methodOnClassOfFirstArgument ? argTypes.size() - 1 : argTypes.size();
			for (Method m : matchingMethods) {
				Class<?>[] paramTypes = m.getParameterTypes();
				int numParams = paramTypes.length;
				boolean variableNumParams = false;
				if (paramTypes != null && paramTypes.length > 0) {
					// is the last paramType a variable number of args?
					if (paramTypes[paramTypes.length - 1].getName().equals("[Ljava.lang.Object;")) {
						variableNumParams = true;
					}
					// check to see if this is a method on the class of the first argument
					if (!variableNumParams && !(numParams == effectiveNumArguments)) {
						continue;
					}
					boolean match = true;
					for (int i = 0;  i < paramTypes.length; i++) {
						Class<?> pt = paramTypes[i];
						String ptstr = pt.getTypeName();
						int argIndex;
						org.apache.jena.graph.Node argType = null;
						if (i >= paramTypes.length) {
							// this is either an error or variable arguments
							if (varArgs) {
								// get type from actual argument
								org.apache.jena.graph.Node argVal = args.get(i+1);
								if (argVal.isLiteral()) {
									argType = argVal;
								}
							}
						}
						else {
							argType = methodOnClassOfFirstArgument ? args.get(i + 1) : args.get(i);
						}
						String ot;
						if (argType instanceof Node_Literal) {
							ot = ((Node_Literal)argType).getLiteralDatatypeURI();
						}
						else {
							ot = null; //LiteralType.StringLiteral;
						}
						if (ot != null && ptstr != null) {
							if (isNumber(ot) && isNumber(ptstr)) {
								if (ot != null && ot.indexOf('#') > 0) {
									String ln = ot.substring(ot.indexOf('#') + 1);
									// int or long
									if (!ln.equals("long") && !ln.equals("int") && !ln.equals("float") && !ln.equals("double")) {
										match = false;
										break;
									}
								}
							}
							else if (ptstr.equals("java.lang.Object[]") || ptstr.equals("java.lang.Object")) {
								// any dt will work
							}
							else if (ot.equals(XSD.xstring.getURI()) && !ptstr.equals("java.lang.String")) {
								match = false;
								break;
							}
							else if (ot.equals(XSD.xboolean.getURI()) && !ptstr.contentEquals("boolean")) {
								match = false;
								break;
							}
						}
					}
					if (match) {
						return m;
					}
				}
			}
		}
		return null;
	}

	/**
	 * Method to determine if the string extracted from the Method parameter types indicates a number
	 * @param ptstr
	 * @return
	 */
	private boolean isNumber(String ptstr) {
		if (ptstr.equals("int") || 
				ptstr.equals("http://www.w3.org/2001/XMLSchema#int") ||
				ptstr.equals("long") ||
				ptstr.equals("http://www.w3.org/2001/XMLSchema#long") ||
				ptstr.equals("float") ||
				ptstr.equals("http://www.w3.org/2001/XMLSchema#flot") ||
				ptstr.equals("double") ||
				ptstr.equals("http://www.w3.org/2001/XMLSchema#double")) {
			return true;		
		}
		return false;
	}

	/**
	 * Method to determine if the SADL LiteralType extracted from an input SADL Literal indicates a number
	 * @param dt
	 * @return
	 */
	private boolean isNumber(LiteralType dt) {
		return dt.equals(LiteralType.NumberLiteral);
	}

	/**
	 * Method to determine if a given Java Object is a number 
	 * @param value
	 * @return
	 */
	private boolean isNumber(Object value) {
		if ((value instanceof Long) ||
				(value instanceof Integer) ||
				(value instanceof Float) ||
				(value instanceof Double) ||
				(value instanceof BigDecimal)) {
			return true;
		}
		else if (value instanceof XSDBaseNumericType) {
			return true;
		}
		return false;
	}

	/**
	 * Method to determine if a given Java Object is a number 
	 * @param value
	 * @return
	 */
	private boolean isNumber(Class<?> clazz) {
		if (clazz.equals(Long.class) ||
				clazz.equals(Integer.class) ||
				clazz.equals(Float.class) ||
				clazz.equals(Double.class) ||
				clazz.equals(BigDecimal.class)) {
			return true;
		}
		return false;
	}

	/**
	 * Method to find the Java Class identified by a SADL External equation URI string
	 * @param exturistr
	 * @return
	 */
	private Class<?> getMatchingClassOfExternalUri(String classname) {
		Class<?> fctCls;
		try {
			fctCls = Class.forName(classname);
			if (fctCls != null) {
				return fctCls;
			}
		} catch (ClassNotFoundException e) {
			addError(new ModelError("External Java class '" + classname + "' not found.", ErrorType.ERROR));
		}
		addError(new ModelError("Invalid class name: " + classname, ErrorType.ERROR));
		return null;
	}
	/**
	 * Method to find the Java Class method(s) that match a SADL External equation external URI string
	 * @param fctCls -- the Java Class already identified
	 * @param methname -- the name specified for the Method
	 * @param staticMethods -- true if this call should only return static methods; false if this call should only return instance methods
	 * @return -- a list of Java Methods matching the conditions
	 */
	private List<Method> getMatchingMethodsOfExternalUri(Class<?> fctCls, String methname, boolean staticMethods) {
		if (fctCls != null) {
			List<Method> results = new ArrayList<Method>();
			Method[] fctMethods = fctCls.getMethods();
			for (Method fctMeth : fctMethods) {
				if (fctMeth.getName().equals(methname)) {
					boolean isStatic = Modifier.isStatic(fctMeth.getModifiers());
					if (isStatic== staticMethods) {
						results.add(fctMeth);
					}
				}
			}
			return results;
		}
		else {
			addError(new ModelError("External Java class cannot be null.", ErrorType.ERROR));
		}
		return null;
	}

}