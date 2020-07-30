/************************************************************************
 * Copyright � 2007-2010 - General Electric Company, All Rights Reserved
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
 * $Revision: 1.3 $ Last modified on   $Date: 2015/07/25 16:25:16 $
 ***********************************************************************/

package com.ge.research.sadl.model.gp;

import java.util.ArrayList;
import java.util.List;

/**
 * A Query is an Expression with an optional list of variables to be returned
 * (which can either be explicit variable names of the names of properties
 * whose values are to be returned, which could be ambiguous in some cases)
 * and a list of GraphPatternElements which can include conjunctions, disjunctions,
 * triple patterns, and built-ins (which will translate to filters). 
 * 
 * @author crapo
 *
 */
public class Query extends SadlCommand {
	private String fqName;
	private String keyword = null;
	private List<VariableNode> variables = null;
	private List<GraphPatternElement> patterns = null;
	protected String sparqlQueryString = null;
	private String preparedQueryString = null;
	private List<OrderingPair> orderBy = null;
	private boolean distinct = false;
	private boolean toBeEvaluated = false;
	private boolean isGraph = false;
	private boolean isUpdate = false;
	private List<Object> parameterizedValues = null;

	public enum Order {ASC, DESC}
	
	public class OrderingPair {
		private String variable = null;
		private Order order = Order.ASC;
		
		public OrderingPair(String var, Order ord) {
			variable = var;
			order = ord;
		}
		
		public void setVariable(String variable) {
			this.variable = variable;
		}
		public String getVariable() {
			return variable;
		}
		public void setOrder(Order order) {
			this.order = order;
		}
		public Order getOrder() {
			return order;
		}
	}
	
	public Query() {
	}
	
	public void setVariables(List<VariableNode> variables) {
		this.variables = variables;
	}
	
	public boolean addVariable(VariableNode var) {
		if (variables == null) {
			variables = new ArrayList<VariableNode>();
		}
		if (!variables.contains(var)) {
			variables.add(var);
			return true;
		}
		return false;
	}
	
	public List<VariableNode> getVariables() {
		return variables;
	}
	
	public VariableNode getVariable(String name) {
		if (variables != null) {
			for (int i = 0; i < variables.size(); i++) {
				if (variables.get(i).toFullyQualifiedString().equals(name)) {
					return variables.get(i);
				}
			}
		}
		return null;
	}
	
	public void setPatterns(List<GraphPatternElement> _pattern) {
		patterns = _pattern;
	}
	public List<GraphPatternElement> getPatterns() {
		return patterns;
	}

	public void addPattern(GraphPatternElement element) {
		if (patterns == null) {
			patterns = new ArrayList<GraphPatternElement>();
		}
		patterns.add(element);
	}

	public GraphPatternElement getFirstPattern() {
		if (patterns != null && !patterns.isEmpty()) {
			return patterns.get(0);
		}
		return null;
	}
	public GraphPatternElement getLastPattern() {
		if (patterns != null && !patterns.isEmpty()) {
			return patterns.get(patterns.size() - 1);
		}
		return null;
	}
	
	public String toString() {
		if (sparqlQueryString != null) {
			return sparqlQueryString;
		}
		return toString(false);
	}

	private String toString(boolean fullyQualified) {
		StringBuilder sb = new StringBuilder();
		if (variables != null) {
			sb.append(getKeyword());
			if (isDistinct()) {
				sb.append(" distinct");
			}
			if (variables.size() > 0) {
				for (int i = 0; i < variables.size(); i++) {
					sb.append(" ");
					sb.append(variables.get(i));
				}
			}
			else {
				sb.append(" *");
			}
			sb.append(" where ");
		}
		if (patterns != null) {
			for (int i = 0; i < patterns.size(); i++) {
				if (i > 0) {
					sb.append(" . ");
				}
				if (fullyQualified) {
					sb.append(patterns.get(i).toFullyQualifiedString());
				}
				else {
					sb.append(patterns.get(i).toString());
				}
			}
		}
		if (getOrderBy() != null) {
			sb.append(" order by ");
			List<OrderingPair> ops = getOrderBy();
			for (int i = 0; i < ops.size(); i++) {
				OrderingPair op = ops.get(i);
				if (i > 0) {
					sb.append(", ");
				}
				if (op.getOrder() != null) {
					if (op.getOrder().equals(Order.DESC)) {
						sb.append(" desc ");
					}
					else {
						sb.append(" asc ");
					}
				}
				sb.append(op.getVariable());
			}
		}
		return sb.toString();
	}
	
	public String toFullyQualifiedString() {
		if (sparqlQueryString != null) {
			return sparqlQueryString;
		}
		return toString(true);
	}
	
	public String toDescriptiveString() {
		if (sparqlQueryString != null) {
			return sparqlQueryString;
		}
		StringBuilder sb = new StringBuilder();
		if (variables != null) {
			sb.append(getKeyword());
			if (isDistinct()) {
				sb.append(" distinct");
			}
			if (variables.size() > 0) {
				for (int i = 0; i < variables.size(); i++) {
					sb.append(" ");
					sb.append(variables.get(i).toDescriptiveString());
				}
			}
			else {
				sb.append(" *");
			}
			sb.append(" where ");
		}
		if (patterns != null) {
			for (int i = 0; i < patterns.size(); i++) {
				if (i > 0) {
					sb.append(" . ");
				}
				sb.append(patterns.get(i).toDescriptiveString());
			}
		}
		if (getOrderBy() != null) {
			sb.append(" order by ");
			List<OrderingPair> ops = getOrderBy();
			for (int i = 0; i < ops.size(); i++) {
				OrderingPair op = ops.get(i);
				if (i > 0) {
					sb.append(", ");
				}
				if (op.getOrder() != null) {
					if (op.getOrder().equals(Order.DESC)) {
						sb.append(" desc ");
					}
					else {
						sb.append(" asc ");
					}
				}
				sb.append(op.getVariable());
			}
		}
		return sb.toString();
	}

	public void setSparqlQueryString(String sparqlQueryString) {
		this.sparqlQueryString = sparqlQueryString;
	}

	public String getSparqlQueryString() {
		return sparqlQueryString;
	}

	public void setOrderBy(List<OrderingPair> orderBy) {
		this.orderBy = orderBy;
	}
	
	public void addOrderBy(String var, Order order) {
		if (orderBy == null) {
			orderBy = new ArrayList<OrderingPair>();
		}
		orderBy.add(new OrderingPair(var, order));
	}

	public List<OrderingPair> getOrderBy() {
		return orderBy;
	}

	public void setDistinct(boolean distinct) {
		this.distinct = distinct;
	}

	public boolean isDistinct() {
		return distinct;
	}

	public boolean isToBeEvaluated() {
		return toBeEvaluated;
	}

	public void setToBeEvaluated(boolean toBeEvaluated) {
		this.toBeEvaluated = toBeEvaluated;
	}

	public String getKeyword() {
		return keyword;
	}

	public void setKeyword(String keyword) {
		this.keyword = keyword;
	}

	public String getName() {
		if (fqName != null) {
			if (fqName.contains("#")) {
				return fqName.substring(fqName.indexOf("#") + 1);
			}
		}
		return fqName;
	}

	public String getFqName() {
		return fqName;
	}

	public void setFqName(String fqName) {
		this.fqName = fqName;
	}

	public boolean isGraph() {
		return isGraph;
	}

	public void setGraph(boolean isGraph) {
		this.isGraph = isGraph;
	}

	public boolean isUpdate() {
		return isUpdate;
	}

	public void setUpdate(boolean isUpdate) {
		this.isUpdate = isUpdate;
	}

	public List<Object> getParameterizedValues() {
		return parameterizedValues;
	}

	public void setParameterizedValues(List<Object> parameterizedValues) {
		this.parameterizedValues = parameterizedValues;
	}

	public String getPreparedQueryString() {
		return preparedQueryString;
	}

	/**
	 * Method to set the final query string rendered by expanding names and qnames to full URIs
	 * @param preparedQueryString
	 */
	public void setPreparedQueryString(String preparedQueryString) {
		this.preparedQueryString = preparedQueryString;
	}
}
