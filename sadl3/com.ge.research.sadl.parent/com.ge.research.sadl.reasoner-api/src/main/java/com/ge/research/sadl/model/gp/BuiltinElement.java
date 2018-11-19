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

/**
 * Class to represent any function in the Intermediate Form
 * @author 200005201
 *
 */
public class BuiltinElement extends GraphPatternElement {
    private static final Logger logger = LoggerFactory.getLogger(BuiltinElement.class);
	private String funcName = null;
	private String funcUri = null;	// URI of function if given
	private String funcPrefix = null;  // QName prefix if given
	private BuiltinType funcType = null;	// the BuiltinType of this BuiltinElement
	private List<Node> arguments = null;
	private int expectedArgCount = 0;
	private boolean createdFromInterval = false;
	private NamedNode mImpliedPropertyNode = null;		// contains the implied property information of the named node

	private List<Node> argumentTypes = null;
	private List<Node> returnTypes = null;

	
	public static enum BuiltinType {
		Equal, NotEqual, Only, NotOnly, LT,	 				// these are boolean built-ins requiring two input arguments to be bound
		LTE, GT, GTE, Assign,
		Plus, Minus, Multiply, Divide, Power, Modulus,		// these built-ins are binary--they take two input arguments followed by
															// an output variable
		Not, Negative,										// these built-ins are unary--they take a single input argument followed by
															// an output variable
		BuiltinFunction,									// some rule languages have built-in functions, e.g., X is min(Y,Z) in Prolog
		UserAdded;											// this is for functions that are added by the user

		public boolean isBooleanBuiltin;	// true if evaluated value of BuiltinElement is boolean (xsd:boolean)
		public boolean isBinaryBuiltin;		// true if BuiltinElement is binary (takes two input arguments) (Note that there might be an output argument)
		public boolean isUnaryBuiltin;		// true if BuiltinElement is uanry (takes a single input argument) (Note that there migth be an output argument)
		private String[] tokens;			// the tokens associated with this BuiltinElement
		
		/**
		 * Set isBooleanBuiltin to true or false
		 * @param isBooleanBuiltin
		 * @return
		 */
		private BuiltinType setBooleanBuiltin(boolean isBooleanBuiltin) {
			this.isBooleanBuiltin = isBooleanBuiltin;
			return this;
		}
		
		/**
		 * Set isBinaryBuiltin to true or false
		 * @param isBinaryBuiltin
		 * @return
		 */
		private BuiltinType setBinaryBuiltin(boolean isBinaryBuiltin) {
			this.isBinaryBuiltin = isBinaryBuiltin;
			return this;
		}
		
		/**
		 * Set isUnaryBuiltin to true or false
		 * @param isUnaryBuiltin
		 * @return
		 */
		private BuiltinType setUnaryBuiltin(boolean isUnaryBuiltin) {
			this.isUnaryBuiltin = isUnaryBuiltin;
			return this;
		}
		
		/**
		 * Set tokens for this BuiltinElement
		 * @param tokens
		 * @return
		 */
		private BuiltinType setTokens(String... tokens) {
			this.tokens = tokens;
			return this;
		}
		
		/**
		 * Determine if the given token matches any of this BuiltinElement's tokens
		 * @param token
		 * @return -- return true on match else false
		 */
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
		
		/**
		 * Get the BuiltinType of a token
		 * @param token
		 * @return -- matching BuiltinType, BuiltinType.UserAdded returned if no match
		 */
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

	/**
	 * Null argument constructor.
	 */
	public BuiltinElement() {
	}

	/**
	 * Get the BuiltinElement's function name
	 * @return
	 */
	public String getFuncName() {
		return funcName;
	}

	/**
	 * Method to set BuiltinElement's name (which also sets its BuiltinType and possibly other fields)
	 * @param name
	 */
	public void setFuncName(String name) {
		this.funcName = name;
		this.funcType = BuiltinType.getType(name);
		if (funcType.isBooleanBuiltin || funcType.isUnaryBuiltin) {
			setExpectedArgCount(2);
		}
		else if (funcType.isBinaryBuiltin) {
			setExpectedArgCount(3);
		}
	}

	/**
	 * Get the BuiltinElement's BuiltinType
	 * @return
	 */
	public BuiltinType getFuncType() {
		return funcType;
	}
	
	/**
	 * Set the BuiltinElement's BuiltinType.
	 * @param ft
	 * @return -- the previous BuiltinType
	 */
	public BuiltinType setFuncType(BuiltinType ft) {
		BuiltinType oldft = funcType;
		funcType = ft;
		return oldft;
	}
	
	/**
	 * Get the BuiltinElement's arguments (list of Nodes)
	 * @return
	 */
	public List<Node> getArguments() {
		return arguments;
	}

	/**
	 * Add an argument Node to the BuiltinElement
	 * @param argument
	 */
	public void addArgument(Node argument) {
		if (arguments == null) {
			arguments = new ArrayList<Node>();
		}
		arguments.add(argument);
//		if (argument instanceof VariableNode) {
//			((VariableNode)argument).incrementReferences();
//		}
		if (getExpectedArgCount() > 0 && arguments.size() > getExpectedArgCount()) {
			logger.warn("Added too many arguments to {}", this);
		}
	}
	
	/**
	 * Add an argument Node to the BuiltinElement at the specified argument location
	 * @param index
	 * @param argument
	 */
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
		if (getExpectedArgCount() > 0 && arguments.size() > getExpectedArgCount()) {
			logger.warn("Added too many arguments to {}", this);
		}
	}
	
	/**
	 * Get the number of expected arguments (default 0)
	 * @return
	 */
	public int getExpectedArgCount() {
		return expectedArgCount;
	}

	/**
	 * Default method to convert the BuiltinElement to a string
	 */
	@Override
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

	/**
	 * Convert this BuiltinElement to a string in which each named concept from the ontology 
	 * is identified by a complete URI
	 * @return
	 */
	@Override
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
					sb.append(arg.toFullyQualifiedString());
				}
				else{
					sb.append(arg.toFullyQualifiedString());
				}
			}
		}
		sb.append(")");
		return sb.toString();
	}
    
	/**
	 * Convert this BuiltinElement to the most descriptive string available
	 * @return
	 */
	@Override
	public String toDescriptiveString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getFuncName());

		NamedNode leftImpliedPropertyUsed = null;
		NamedNode rightImpliedPropertyUsed = null;
		
		if(getFuncName() == "shallSet" && 
		   getArguments().get(0) instanceof NamedNode && 
		   getArguments().get(1) instanceof ProxyNode) {
		
			// get the real builtinElement
			 GraphPatternElement proxyFor = ((ProxyNode) getArguments().get(1)).getProxyFor();
			 if(proxyFor instanceof BuiltinElement) {
				 if((((BuiltinElement) proxyFor).getArguments().get(0)) instanceof NamedNode) {
					 leftImpliedPropertyUsed = ((NamedNode)((BuiltinElement) proxyFor).getArguments().get(0)).getImpliedPropertyNode();
				 }
				 if((((BuiltinElement) proxyFor).getArguments().get(1)) instanceof NamedNode) {
					 rightImpliedPropertyUsed = ((NamedNode)((BuiltinElement) proxyFor).getArguments().get(1)).getImpliedPropertyNode();
				 }
			 }
		}
		
		if (leftImpliedPropertyUsed != null || rightImpliedPropertyUsed != null || getExpandedPropertiesToBeUsed() != null) {
			sb.append("(");
			boolean needComma = false;
			if (leftImpliedPropertyUsed != null) {
				sb.append("leftImpliedProperty ");
				sb.append(leftImpliedPropertyUsed.toDescriptiveString());
				needComma = true;
			}
			if (rightImpliedPropertyUsed != null) {
				if (needComma) sb.append(",");
				sb.append("rightImpliedProperty ");
				sb.append(rightImpliedPropertyUsed.toDescriptiveString());
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
					sb.append(arg.toDescriptiveString());
				}
				else{
					sb.append(arg.toDescriptiveString());
				}
			}
		}
		sb.append(")");
		return sb.toString();
	}

	/**
	 * Method to determine if the BuiltinElement has fewer than the expected number of arguments
	 * @return -- true if expected argument count is > 0 and > actual argument count else false
	 */
	public boolean hasMissingArgument() {
		if (getExpectedArgCount() > 0) { // && funcType != null && !funcType.isBooleanBuiltin) {
			if (arguments == null || arguments.size() < getExpectedArgCount()) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Add an argument node to the BuitlinElement, log a warning if expected is exceeded
	 * @param argument
	 */
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
		if (getExpectedArgCount() > 0 && arguments.size() > getExpectedArgCount()) {
			logger.warn("Added too many (missing) arguments to {}", this);
		}
	}

	/**
	 * BuiltinElement is created from Junction of Literals
	 * @param createdFromInterval
	 */
	public void setCreatedFromInterval(boolean createdFromInterval) {
		this.createdFromInterval = createdFromInterval;
	}

	/**
	 * Is BuiltinElement created from Junction of Literals?
	 * @return
	 */
	public boolean isCreatedFromInterval() {
		return createdFromInterval;
	}

	/**
	 * Get URI of this BuiltinElement
	 * @return
	 */
	public String getFuncUri() {
		return funcUri;
	}

	/**
	 * Set URI of BuiltinElement
	 * @param funcUri
	 */
	public void setFuncUri(String funcUri) {
		this.funcUri = funcUri;
	}

	/**
	 * Get QName prefix for BiultinElement
	 * @return
	 */
	public String getFuncPrefix() {
		return funcPrefix;
	}

	/**
	 * Set QName prefix for BiultinElement
	 * @param funcPrefix
	 */
	public void setFuncPrefix(String funcPrefix) {
		this.funcPrefix = funcPrefix;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((arguments == null) ? 0 : arguments.hashCode());
		result = prime * result + ((funcName == null) ? 0 : funcName.hashCode());
		result = prime * result + ((funcUri == null) ? 0 : funcUri.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		BuiltinElement other = (BuiltinElement) obj;
		if (arguments == null) {
			if (other.arguments != null)
				return false;
		} else if (!arguments.equals(other.arguments))
			return false;
		if (funcName == null) {
			if (other.funcName != null)
				return false;
		} else if (!funcName.equals(other.funcName))
			return false;
		if (funcUri == null) {
			if (other.funcUri != null)
				return false;
		} else if (!funcUri.equals(other.funcUri))
			return false;
		return true;
	}
	
	/**
	 * get the node's implied property
	 * @return mImpliedPropertyNode
	 */
	public NamedNode getImpliedPropertyNode() {
		return mImpliedPropertyNode;
	}
	
	/**
	 * set the node's implied property
	 * @param aImpliedPropertyNode
	 */
	public void setImpliedPropertyNode(NamedNode aImpliedPropertyNode) {
		this.mImpliedPropertyNode = aImpliedPropertyNode;
	}

	/**
	 * Method to get the argument types (if known)
	 * @return
	 */
	public List<Node> getArgumentTypes() {
		return argumentTypes;
	}

	/**
	 * Method to set the argument types
	 * @param argumentTypes
	 */
	public void setArgumentTypes(List<Node> argumentTypes) {
		this.argumentTypes = argumentTypes;
	}

	/**
	 * Method to get the return types (if known)
	 * @return
	 */
	public List<Node> getReturnTypes() {
		return returnTypes;
	}

	/**
	 * Method to set the return types
	 * @param returnTypes
	 */
	public void setReturnTypes(List<Node> returnTypes) {
		this.returnTypes = returnTypes;
	}

	/**
	 * Method to set the expected arg count when appropriate to the context
	 * @param expectedArgCount
	 */
	public void setExpectedArgCount(int expectedArgCount) {
		this.expectedArgCount = expectedArgCount;
	}
}
