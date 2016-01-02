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

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.4 $ Last modified on   $Date: 2015/03/04 21:10:31 $
 ***********************************************************************/

package com.ge.research.sadl.model.gp;

public class Junction extends GraphPatternElement {
	private String junctionName = null;
	private JunctionType junctionType = null;
	
	private Object lhs = null;
	private Object rhs = null;

	public static enum JunctionType {
		Conj, Disj;
		private String[] tokens;

		private JunctionType setTokens(String... tokens) {
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
		public static JunctionType getType(String token) {
			for (JunctionType type : values()) {
				if (type.matches(token)) {
					return type;
				}
			}
			// TODO throw exception? There's no default here--this shouldn't happen
			return null;
		}
		
		static {
			Conj.setTokens("&&", "and");
			Disj.setTokens("||", "or");
		}
	}

	public Junction() {
	}

	public void setJunctionName(String name) {
		this.junctionName = name;
		this.junctionType = JunctionType.getType(name);
	}
	
	public String getJunctionName() {
		return junctionName;
	}

	public JunctionType getJunctionType() {
		return junctionType;
	}

	public void setLhs(Object lhs) {
		this.lhs = lhs;
	}

	public Object getLhs() {
		return lhs;
	}

	public void setRhs(Object rhs) {
		this.rhs = rhs;
	}

	public Object getRhs() {
		return rhs;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getJunctionName());
		sb.append("(");
		if (getLhs() != null) {
			sb.append(getLhs().toString());
//			sb.append(") ");
		}
		sb.append(", ");
		if (getRhs() != null) {
			sb.append(getRhs().toString());
		}
		
		sb.append(")");
		if (getNext() != null) {
			sb.append(" . ");
			sb.append(getNext().toString());
		}
		return sb.toString();
	}

	@Override
	public String toFullyQualifiedString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getJunctionName());
		sb.append("(");
		if (getLhs() != null) {
			Object lhs = getLhs();
			if (lhs instanceof GraphPatternElement) {
				sb.append(((GraphPatternElement) lhs).toFullyQualifiedString());
			}
			else if (lhs instanceof Node) {
				sb.append(((Node) lhs).toFullyQualifiedString());
			}
			else {
				sb.append(lhs.toString());
			}
//			sb.append(") ");
		}
		sb.append(",");
		if (getRhs() != null) {
			Object rhs = getRhs();
			if (rhs instanceof GraphPatternElement) {
				sb.append(((GraphPatternElement) rhs).toFullyQualifiedString());
			}
			else if (rhs instanceof Node) {
				sb.append(((Node) rhs).toFullyQualifiedString());
			}
			else {
				sb.append(rhs.toString());
			}
		}
		
		sb.append(")");
//		if (getNext() != null) {
//			sb.append(" . ");
//			sb.append(getNext().toFullyQualifiedString());
//		}
		return sb.toString();
	}
}
