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

import java.util.List;

import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;


/**
 * A Test can be in one of two forms:
 * 1) a comparison operator, which will then have a left-hand side and a
 *    right-hand side which are to be compared to see if the test passes,
 * or
 * 2) an Expression, in which case the test is whether that expression can
 *    be satisfied in the triple store. Note that in this case 
 *    
 * Note: if there is an Expression only, the lhs will be that expression,
 * the rhs will be null, and the compType will be null.    
 * 
 * @author crapo
 *
 */
public class Test extends SadlCommand {
	private Object lhs = null;
	private Object rhs = null;
	private String compName = null;
	private ComparisonType compType = null;
	private List<String> lhsVariables = null;
	private List<String> rhsVariables = null;
	
	public static enum ComparisonType {Eq, Neq, IsNot, IsOnly, IsNotOnly, LT, LTE, GT, GTE;		
	private String[] tokens;

	private ComparisonType setTokens(String... tokens) {
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
	public static ComparisonType getType(String token) {
		for (ComparisonType type : values()) {
			if (type.matches(token)) {
				return type;
			}
		}
		// TODO throw exception? There's no default here--this shouldn't happen
		return null;
	}
	
	//('=' | '==' | 'is' | '!=' | IsNot | IsOnly | IsNotOnly | '<' | '<=' | '>' | '>='
	static {
		Eq.setTokens("=", "==", "is");
		Neq.setTokens("!=");
		IsNot.setTokens("is not", "not");
		IsOnly.setTokens("is only", "only");
		IsNotOnly.setTokens("is not only", "not only");
		LT.setTokens("<");
		LTE.setTokens("<=");
		GT.setTokens(">");
		GTE.setTokens(">=");
	}
}

	public Test() {
	}

	public void setLhs(Object _lhs) {
		lhs = _lhs;
	}
	
	public Object getLhs() {
		return lhs;
	}
	
	public void setRhs(Object _rhs) {
		rhs = _rhs;
	}
	
	public Object getRhs() {
		return rhs;
	}
	
	public void setLhsVariables(List<String> variables) {
		this.lhsVariables = variables;
	}
	public List<String> getLhsVariables() {
		return lhsVariables;
	}
	
	public void setRhsVariables(List<String> variables) {
		this.rhsVariables = variables;
	}
	public List<String> getRhsVariables() {
		return rhsVariables;
	}
	
	public void setCompName(String name) {
		this.compName = name;
		if (name != null) {
			this.compType = ComparisonType.getType(name);
		}
		else {
			compType = null;
		}
	}
	
	public void setCompName(BuiltinType type) {
		if (type.equals(BuiltinType.Equal)) {
			setCompName("==");
		}
		else if (type.equals(BuiltinType.NotEqual) || type.equals(BuiltinType.Not) || type.equals(BuiltinType.Negative)) {
			setCompName("!=");
		}
		else if (type.equals(BuiltinType.LT)) {
			setCompName("<");
		}
		else if (type.equals(BuiltinType.LTE)) {
			setCompName("<=");
		}
		else if (type.equals(BuiltinType.GT)) {
			setCompName(">");
		}
		else if (type.equals(BuiltinType.GTE)) {
			setCompName(">=");
		}
		else if (type.equals(BuiltinType.Only)) {
			setCompName("is only");
		}
		else if (type.equals(BuiltinType.NotOnly)) {
			setCompName("is not only");
		}
		else {
			try {
				throw new Exception("unable to convert type '" + type.toString() + "' to a Test comparison type.");
			}
			catch(Exception e) {
				e.printStackTrace();
			}
		}
	}

	public String getCompName() {
		return compName;
	}
	
	public ComparisonType getCompType() {
		return compType;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (lhsVariables != null && lhsVariables.size() > 0) {
			sb.append("[");
			for (int i = 0; i < lhsVariables.size(); i++) {
				if (i > 0) sb.append(", ");
				sb.append(lhsVariables.get(i));
			}
			sb.append("]:");
		}
		if (lhs != null) {
			sb.append(lhs.toString());
		}
		if (compName != null) {
			sb.append(" ");
			sb.append(compName);
			sb.append(" ");
		}
		if (rhsVariables != null && rhsVariables.size() > 0) {
			sb.append("[");
			for (int i = 0; i < rhsVariables.size(); i++) {
				if (i > 0) sb.append(", ");
				sb.append(rhsVariables.get(i));
			}
			sb.append("]:");
		}
		if (rhs != null) {
			sb.append(rhs.toString());
		}
		return sb.toString();
	}

}
