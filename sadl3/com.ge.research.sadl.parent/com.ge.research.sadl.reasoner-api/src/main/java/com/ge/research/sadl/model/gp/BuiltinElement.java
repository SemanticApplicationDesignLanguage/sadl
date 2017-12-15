/************************************************************************
 * Copyright \u00a9 2007-2010 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.model.gp;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BuiltinElement extends GraphPatternElement {
    private static final Logger logger = LoggerFactory.getLogger(BuiltinElement.class);
	private String funcName = null;
	private String funcUri = null;	// URI of function if given
	private BuiltinType funcType = null;
	private List<Node> arguments = null;
	private int expectedArgCount = 0;
	private boolean createdFromInterval = false;
	
	public static enum BuiltinType {
		Equal, NotEqual, Only, NotOnly, LT,	 				// these are boolean built-ins requiring two input arguments to be bound
		LTE, GT, GTE, Assign,
		Plus, Minus, Multiply, Divide, Power, Modulus,		// these built-ins are binary--they take two input arguments followed by
															// an output variable
		Not, Negative,										// these built-ins are unary--they take a single input argument followed by
															// an output variable
		BuiltinFunction,									// some rule languages have built-in functions, e.g., X is min(Y,Z) in Prolog
		UserAdded;

		public boolean isBooleanBuiltin;
		public boolean isBinaryBuiltin;
		public boolean isUnaryBuiltin;
		private String[] tokens;
		
		private BuiltinType setBooleanBuiltin(boolean isBooleanBuiltin) {
			this.isBooleanBuiltin = isBooleanBuiltin;
			return this;
		}
		private BuiltinType setBinaryBuiltin(boolean isBinaryBuiltin) {
			this.isBinaryBuiltin = isBinaryBuiltin;
			return this;
		}
		private BuiltinType setUnaryBuiltin(boolean isUnaryBuiltin) {
			this.isUnaryBuiltin = isUnaryBuiltin;
			return this;
		}
		private BuiltinType setTokens(String... tokens) {
			this.tokens = tokens;
			return this;
		}
		public boolean matches(String token) {
			if (tokens != null) {
				for (String match : tokens) {
					if (match.equals(token)) {
						return true;
					}
				}
			}
			return false;
		}
		public static synchronized BuiltinType getType(String token) {
			for (BuiltinType type : values()) {
				if (type.matches(token)) {
					return type;
				}
			}
			return UserAdded;
		}
		static {
			Equal.setBooleanBuiltin(true).setTokens("=", "==", "is", "to");
			NotEqual.setBooleanBuiltin(true).setTokens("!=");
			LT.setBooleanBuiltin(true).setTokens("<");
			LTE.setBooleanBuiltin(true).setTokens("<=");
			GT.setBooleanBuiltin(true).setTokens(">");
			GTE.setBooleanBuiltin(true).setTokens(">=");
			Assign.setBooleanBuiltin(true).setTokens("assign");

			Plus.setBinaryBuiltin(true).setTokens("+");
			Minus.setBinaryBuiltin(true).setTokens("-");
			Multiply.setBinaryBuiltin(true).setTokens("*");
			Divide.setBinaryBuiltin(true).setTokens("/");
			Power.setBinaryBuiltin(true).setTokens("^");
			Modulus.setBinaryBuiltin(true).setTokens("%");

			Not.setUnaryBuiltin(true).setTokens("!", "not");
			Only.setUnaryBuiltin(true).setTokens("only");
			NotOnly.setUnaryBuiltin(true).setTokens("not only");
			Negative.setUnaryBuiltin(true).setTokens("-");
		}
	}

	public BuiltinElement() {
	}

	public String getFuncName() {
		return funcName;
	}

	public BuiltinType getFuncType() {
		return funcType;
	}
	
	public BuiltinType setFuncType(BuiltinType ft) {
		BuiltinType oldft = funcType;
		funcType = ft;
		return oldft;
	}
	
	public List<Node> getArguments() {
		return arguments;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getFuncName());
		sb.append("(");
		for (int i = 0; arguments != null && i < arguments.size(); i++) {
			if (i > 0) {
				sb.append(",");
			}
			Node arg = arguments.get(i);
			if (arg instanceof ProxyNode && 
					(((ProxyNode)arg).getProxyFor().equals(this) ||
							(((ProxyNode)arg).getProxyFor() instanceof List<?> &&
									((List<?>)((ProxyNode)arg).getProxyFor()).get(0).equals(this)))) {
				sb.append("bi arg self-referencing!");
			}
			else {
				sb.append(arg != null ? arg.toString() : "null");
			}
		}
		sb.append(")");
		return sb.toString();
	}

	public String toFullyQualifiedString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getFuncName());
		sb.append("(");
		for (int i = 0; arguments != null && i < arguments.size(); i++) {
			if (i > 0) {
				sb.append(",");
			}
			Node arg = arguments.get(i);
			if (arg instanceof ProxyNode &&
					(((ProxyNode)arg).getProxyFor().equals(this) ||
							(((ProxyNode)arg).getProxyFor() instanceof List<?> &&
									((List<?>)((ProxyNode)arg).getProxyFor()).get(0).equals(this)))) {
				sb.append("bi arg self-referencing!");
			}
			else {
				if (arg == null){
					sb.append("null");
				}
				else if (arg instanceof NamedNode){
					sb.append(((NamedNode) arg).toFullyQualifiedString());
				}
				else if (arg instanceof ProxyNode) {
					Object pfn = ((ProxyNode)arg).getProxyFor();
					if (pfn instanceof GraphPatternElement) {
						sb.append(((GraphPatternElement)pfn).toFullyQualifiedString());
					}
					else {
						sb.append(arg.toFullyQualifiedString());
					}
				}
				else{
					sb.append(arg.toFullyQualifiedString());
				}
			}
		}
		sb.append(")");
		return sb.toString();
	}
    
	@Override
	public String toDescriptiveString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getFuncName());
		if (getLeftImpliedPropertyUsed() != null || getRightImpliedPropertyUsed() != null || getExpandedPropertiesToBeUsed() != null) {
			sb.append("(");
			boolean needComma = false;
			if (getLeftImpliedPropertyUsed() != null) {
				sb.append("leftImpliedProperty ");
				sb.append(getLeftImpliedPropertyUsed().toDescriptiveString());
				needComma = true;
			}
			if (getRightImpliedPropertyUsed() != null) {
				if (needComma) sb.append(",");
				sb.append("rightImpliedProperty ");
				sb.append(getRightImpliedPropertyUsed().toDescriptiveString());
				needComma = true;
			}
			if (getExpandedPropertiesToBeUsed() != null) {
				if (needComma) sb.append(",");
				needComma = false;
				sb.append("expandedProperties [");
				Iterator<NamedNode> epitr = getExpandedPropertiesToBeUsed().iterator();
				while (epitr.hasNext()) {
					if (needComma) sb.append(",");
					sb.append(epitr.next().toDescriptiveString());
					needComma = true;
				}
				sb.append("]");
			}
			sb.append(")");
		}
		sb.append("(");
		for (int i = 0; arguments != null && i < arguments.size(); i++) {
			if (i > 0) {
				sb.append(",");
			}
			Node arg = arguments.get(i);
			if (arg instanceof ProxyNode &&
					(((ProxyNode)arg).getProxyFor().equals(this) ||
							(((ProxyNode)arg).getProxyFor() instanceof List<?> &&
									((List<?>)((ProxyNode)arg).getProxyFor()).get(0).equals(this)))) {
				sb.append("bi arg self-referencing!");
			}
			else {
				if (arg == null){
					sb.append("null");
				}
				else if (arg instanceof NamedNode){
					sb.append(((NamedNode) arg).toDescriptiveString());
				}
				else if (arg instanceof ProxyNode) {
					Object pfn = ((ProxyNode)arg).getProxyFor();
					if (pfn instanceof GraphPatternElement) {
						sb.append(((GraphPatternElement)pfn).toDescriptiveString());
					}
					else {
						sb.append(arg.toDescriptiveString());
					}
				}
				else{
					sb.append(arg.toDescriptiveString());
				}
			}
		}
		sb.append(")");
		return sb.toString();
	}

	public void setFuncName(String name) {
		this.funcName = name;
		this.funcType = BuiltinType.getType(name);
		if (funcType.isBooleanBuiltin || funcType.isUnaryBuiltin) {
			expectedArgCount = 2;
		}
		else if (funcType.isBinaryBuiltin) {
			expectedArgCount = 3;
		}
	}

	public void addArgument(Node argument) {
		if (arguments == null) {
			arguments = new ArrayList<Node>();
		}
		arguments.add(argument);
//		if (argument instanceof VariableNode) {
//			((VariableNode)argument).incrementReferences();
//		}
		if (expectedArgCount > 0 && arguments.size() > expectedArgCount) {
			logger.warn("Added too many arguments to {}", this);
		}
	}
	
	public void addArgument(int index, Node argument) {
		if (arguments == null) {
			arguments = new ArrayList<Node>();
			arguments.add(argument);
		}
		else {
			if (index < arguments.size()) {
				arguments.add(index, argument);
			}
			else {
				logger.error("Argument '" + argument.toString() + "' can't be added at location " + index + "; adding at end.");
				arguments.add(argument);
			}
		}
//		if (argument instanceof VariableNode) {
//			((VariableNode)argument).incrementReferences();
//		}
		if (expectedArgCount > 0 && arguments.size() > expectedArgCount) {
			logger.warn("Added too many arguments to {}", this);
		}
	}
	
	public int getExpectedArgCount() {
		return expectedArgCount;
	}

	public boolean hasMissingArgument() {
		if (expectedArgCount > 0) { // && funcType != null && !funcType.isBooleanBuiltin) {
			if (arguments == null || arguments.size() < expectedArgCount) {
				return true;
			}
		}
		return false;
	}
	
	public void addMissingArgument(Node argument) {
		if (arguments == null) {
			arguments = new ArrayList<Node>();
		}
		if (funcType != null && funcType.isBooleanBuiltin) {
			arguments.add(0, argument);
		}
		else {
			arguments.add(argument);
		}
//		if (argument instanceof VariableNode) {
//			((VariableNode)argument).incrementReferences();
//		}
		if (expectedArgCount > 0 && arguments.size() > expectedArgCount) {
			logger.warn("Added too many (missing) arguments to {}", this);
		}
	}

	public void setCreatedFromInterval(boolean createdFromInterval) {
		this.createdFromInterval = createdFromInterval;
	}

	public boolean isCreatedFromInterval() {
		return createdFromInterval;
	}

	public String getFuncUri() {
		return funcUri;
	}

	public void setFuncUri(String funcUri) {
		this.funcUri = funcUri;
	}

}
