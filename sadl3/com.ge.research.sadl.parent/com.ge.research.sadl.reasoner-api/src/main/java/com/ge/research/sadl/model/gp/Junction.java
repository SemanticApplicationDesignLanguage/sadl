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

/**
 * Class to capture conjunction or disjunction as a pattern
 * @author 200005201
 *
 */
public class Junction extends GraphPatternElement {
	private String junctionName = null;
	private JunctionType junctionType = null;
	
	private Object lhs = null;
	private Object rhs = null;

	/**
	 * Representation of the Junction's type, including different possible symbols that can be used
	 * @author 200005201
	 *
	 */
	public static enum JunctionType {
		Conj, Disj;
		public static final String OR_SYMBOL = "||";
		public static final String AND_SYMBOL = "&&";
		public static final String OR_ALPHA = "or";
		public static final String AND_ALPHA = "and";
		public String[] tokens;

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
			Conj.setTokens(AND_SYMBOL, AND_ALPHA);
			Disj.setTokens(OR_SYMBOL, OR_ALPHA);
		}
	}

	/**
	 * Null argument constructor
	 */
	public Junction() {
	}

	/**
	 * Set the Junction name, which will also set its JunctionType
	 * @param name
	 */
	public void setJunctionName(String name) {
		this.junctionName = name;
		this.junctionType = JunctionType.getType(name);
	}
	
	/**
	 * Get the Junction name
	 * @return
	 */
	public String getJunctionName() {
		return junctionName;
	}

	/**
	 * Get the Junction type
	 * @return
	 */
	public JunctionType getJunctionType() {
		return junctionType;
	}

	/**
	 * Set Junction's left-hand side (should be a Node)
	 * @param lhs
	 */
	public void setLhs(Object lhs) {
		this.lhs = lhs;
	}

	/**
	 * Get the Junction's left-hand side (should be a Node)
	 * @return
	 */
	public Object getLhs() {
		return lhs;
	}

	/**
	 * Set Junction's right-hand side (should be a Node)
	 * @param lhs
	 */
	public void setRhs(Object rhs) {
		this.rhs = rhs;
	}

	/**
	 * Get the Junction's right-hand side (should be a Node)
	 * @return
	 */
	public Object getRhs() {
		return rhs;
	}

	/**
	 * Default method to convert the Junction to a string
	 */
	@Override
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

	/**
	 * Convert this Junction to a string in which each named concept from the ontology 
	 * is identified by a complete URI
	 * @return
	 */
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

	/**
	 * Convert this Junction to the most descriptive string available
	 * @return
	 */
	@Override
	public String toDescriptiveString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getJunctionName());
		sb.append("(");
		if (getLhs() != null) {
			Object lhs = getLhs();
			if (lhs instanceof GraphPatternElement) {
				sb.append(((GraphPatternElement) lhs).toDescriptiveString());
			}
			else if (lhs instanceof Node) {
				sb.append(((Node) lhs).toDescriptiveString());
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
				sb.append(((GraphPatternElement) rhs).toDescriptiveString());
			}
			else if (rhs instanceof Node) {
				sb.append(((Node) rhs).toDescriptiveString());
			}
			else {
				sb.append(rhs.toString());
			}
		}
		
		sb.append(")");
		return sb.toString();	
	}
	
	/**
	 * Method to compare Junction with another Object 
	 * @return -- true if obj is a Junction and they have the same name, same left and right hand sides
	 */
	@Override 
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		
		Junction junction = (Junction)obj;
		if(junctionName != junction.getJunctionName()) {
			return false;
		}
		if(!junctionType.equals(junction.getJunctionType())) {
			return false;
		}
		if(!lhs.equals(junction.getLhs())) {
			return false;
		}
		if(!rhs.equals(junction.getRhs())) {
			return false;
		}
		
		return true;
	}
}
