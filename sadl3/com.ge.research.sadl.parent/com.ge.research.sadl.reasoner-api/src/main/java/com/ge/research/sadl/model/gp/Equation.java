/************************************************************************
 * Copyright \u00a9 2007-2016 - General Electric Company, All Rights Reserved
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
import java.util.List;

/**
 * Class to encapsulate the results of parsing an EquationStatement
 * @author 200005201
 *
 */
public class Equation {
	private boolean isExternal = false;
	private String name = null;
	private String namespace = null;
	private List<Node> arguments = null;
	private List<Node> argumentTypes = null;
	private Node returnType = null;
	private List<GraphPatternElement> body = null;
	private String uri = null;
	private String location = null;
	
	public Equation( String nm) {
		setName(nm);
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public List<Node> getArguments() {
		return arguments;
	}
	
	public void setArguments(List<Node> arguments) {
		this.arguments = arguments;
	}
	
	public Node getReturnType() {
		return returnType;
	}
	
	public void setReturnType(Node returnType) {
		this.returnType = returnType;
	}
	
	public List<GraphPatternElement> getBody() {
		return body;
	}
	
	public void setBody(List<GraphPatternElement> body) {
		this.body = body;
	}
	
	public void addBodyElement(GraphPatternElement bdyelement) {
		if (body == null) {
			body = new ArrayList<GraphPatternElement>();
		}
		body.add(bdyelement);
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (getReturnType() != null) {
			sb.append(getReturnType().toString());
		}
		else {
			sb.append("--");
		}
		sb.append(" ");
		sb.append(getName());
		sb.append("(");
		List<Node> args = getArguments();
		List<Node> argtypes = getArgumentTypes();
		if (!(args.size() == argtypes.size())) {
			System.err.println("Error: equation arguments and argument types are not the same size");
		}
		else {
			for (int i = 0; args != null && i < args.size(); i++) {
				Node n = args.get(i);
				Node nt = argtypes.get(i);
				if (i > 0) sb.append(",");
				sb.append(nt.toString());
				sb.append(" ");
				sb.append(n.toString());
			}
		}
		sb.append(")");
		sb.append(": ");
		if (isExternal()) {
			 sb.append("uri(\"");
			 sb.append(getUri());
			 sb.append("\")");
			 if (getLocation() != null) {
				 sb.append(" location(\"");
				 sb.append(getLocation());
				 sb.append("\")");
			 }
		}
		else {
			List<GraphPatternElement> exprs = getBody();
			for (int i = 0; exprs != null && i < exprs.size(); i++) {
				GraphPatternElement expr = exprs.get(i);
				if (i > 0) sb.append(",");
				sb.append(expr.toString());
			}
		}
		return sb.toString();
	}
	
	public String toDescriptiveString() {
		StringBuilder sb = new StringBuilder();
		if (getReturnType() != null) {
			sb.append(getReturnType().toDescriptiveString());
		}
		else {
			sb.append("--");
		}
		sb.append(" ");
		sb.append(getName());
		sb.append("(");
		List<Node> args = getArguments();
		List<Node> argtypes = getArgumentTypes();
		if (!(args.size() == argtypes.size())) {
			System.err.println("Error: equation arguments and argument types are not the same size");
		}
		else {
			for (int i = 0; args != null && i < args.size(); i++) {
				Node n = args.get(i);
				Node nt = argtypes.get(i);
				if (i > 0) sb.append(",");
				sb.append(nt.toDescriptiveString());
				sb.append(" ");
				sb.append(n.toDescriptiveString());
			}
		}
		sb.append(")");
		sb.append(": ");
		if (isExternal()) {
			 sb.append("uri(\"");
			 sb.append(getUri());
			 sb.append("\")");
			 if (getLocation() != null) {
				 sb.append(" location(\"");
				 sb.append(getLocation());
				 sb.append("\")");
			 }
		}
		else {
			List<GraphPatternElement> exprs = getBody();
			for (int i = 0; exprs != null && i < exprs.size(); i++) {
				GraphPatternElement expr = exprs.get(i);
				if (i > 0) sb.append(",");
				sb.append(expr.toDescriptiveString());
			}
		}
		return sb.toString();
	}
	
	public String toFullyQualifiedString() {
		StringBuilder sb = new StringBuilder();
		if (getReturnType() != null) {
			sb.append(getReturnType().toFullyQualifiedString());
		}
		else {
			sb.append("--");
		}
		sb.append(" ");
		sb.append(getNamespace());
		sb.append(getName());
		sb.append("(");
		List<Node> args = getArguments();
		List<Node> argtypes = getArgumentTypes();
		if ((args == null && argtypes != null) || (args != null && argtypes == null) || (args != null && argtypes != null && !(args.size() == argtypes.size()))) {
			System.err.println("Error: equation arguments and argument types are not the same size");
		}
		else {
			for (int i = 0; args != null && i < args.size(); i++) {
				Node n = args.get(i);
				Node nt = argtypes.get(i);
				if (i > 0) sb.append(",");
				sb.append(nt.toFullyQualifiedString());
				sb.append(" ");
				sb.append(n.toFullyQualifiedString());
			}
		}
		sb.append(")");
		sb.append(": ");
		if (isExternal()) {
			 sb.append("uri(\"");
			 sb.append(getUri());
			 sb.append("\")");
			 if (getLocation() != null) {
				 sb.append(" location(\"");
				 sb.append(getLocation());
				 sb.append("\")");
			 }
		}
		else {
			List<GraphPatternElement> exprs = getBody();
			for (int i = 0; exprs != null && i < exprs.size(); i++) {
				GraphPatternElement expr = exprs.get(i);
				if (i > 0) sb.append(",");
				sb.append(expr.toFullyQualifiedString());
			}
		}
		return sb.toString();		
	}

	public String getNamespace() {
		return namespace;
	}

	public void setNamespace(String namespace) {
		this.namespace = namespace;
	}

	public List<Node> getArgumentTypes() {
		return argumentTypes;
	}

	public void setArgumentTypes(List<Node> argumentTypes) {
		this.argumentTypes = argumentTypes;
	}

	public boolean isExternal() {
		return isExternal;
	}

	public void setExternal(boolean isExternal) {
		this.isExternal = isExternal;
	}

	public String getUri() {
		return uri;
	}

	public void setUri(String uri) {
		this.uri = uri;
	}

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}
}
