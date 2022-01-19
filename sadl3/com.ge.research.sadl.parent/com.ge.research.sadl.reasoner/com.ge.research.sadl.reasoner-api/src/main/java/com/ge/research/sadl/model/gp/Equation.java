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
	private boolean varArgs = false;		// does this equation have a variable number or arguments?
	private List<Node> returnTypes = null;
	private List<GraphPatternElement> body = null;
	private List<VariableNode> equationVariables = null;
	private List<GraphPatternElement> wheres = null;
	private List<Node> returnNodes = null;
	private String uri = null;			// identity in the current model
	private String externalUri = null;	// external identity if given (External equation)
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
	
	public List<Node> getReturnTypes() {
		return returnTypes;
	}
	
	public void setReturnTypes(List<Node> returnTypes) {
		this.returnTypes = returnTypes;
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
	
	public List<GraphPatternElement> getWheres() {
		return wheres;
	}
	
	public void setWheres(List<GraphPatternElement> wheres) {
		this.wheres = wheres;
	}
	
	public void addWhereElement(GraphPatternElement whereelement) {
		if (wheres == null) {
			wheres = new ArrayList<GraphPatternElement>();
		}
		wheres.add(whereelement);
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (getReturnTypes() != null) {
			List<Node> rtypes = getReturnTypes();
			int cntr = 0;
			for (Node rt : rtypes) {
				if (cntr++ > 0) {
					sb.append(",");
				}
				if (rt != null) {
					sb.append(rt.toString());
				}
				else {
					sb.append("--");
				}
			}
		}
		else {
			sb.append("--");
		}
		sb.append(" ");
		sb.append(getName());
		sb.append("(");
		List<Node> args = getArguments();
		List<Node> argtypes = getArgumentTypes();
		if (args != null && argtypes != null) {
			if (!(args.size() == argtypes.size())) {
				System.err.println("Error: equation arguments and argument types are not the same size");
			}
			else {
				for (int i = 0; args != null && i < args.size(); i++) {
					Node n = args.get(i);
					Node nt = argtypes.get(i);
					if (i > 0) sb.append(",");
					sb.append(nt != null ? nt.toString() : "<error>");
					sb.append(" ");
					sb.append(n != null ? n.toString() : "<error>");
				}
			}
		}
		sb.append(")");
		sb.append(": ");
		if (isExternal()) {
			 sb.append("uri(\"");
			 sb.append(getExternalUri());
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
		if (getReturnTypes() != null) {
			List<Node> rtypes = getReturnTypes();
			int cntr = 0;
			for (Node rt : rtypes) {
				if (cntr++ > 0) {
					sb.append(",");
				}
				if (rt != null) {
					sb.append(rt.toDescriptiveString());
				}
				else {
					sb.append("--");
				}
			}
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
				sb.append(nt != null ? nt.toDescriptiveString() : "<error>");
				sb.append(" ");
				sb.append(n != null ? n.toDescriptiveString() : "<error>");
			}
		}
		sb.append(")");
		sb.append(": ");
		if (isExternal()) {
			 sb.append("uri(\"");
			 sb.append(getExternalUri());
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
		if (getReturnTypes() != null) {
			List<Node> rtypes = getReturnTypes();
			int cntr = 0;
			for (Node rt : rtypes) {
				if (cntr++ > 0) {
					sb.append(",");
				}
				if (rt != null) {
					sb.append(rt.toFullyQualifiedString());
				}
				else {
					sb.append("--");
				}
			}
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
				sb.append(nt != null ? nt.toFullyQualifiedString() : "<error>");
				sb.append(" ");
				sb.append(n != null ? n.toFullyQualifiedString() : "<error>");
			}
		}
		sb.append(")");
		sb.append(": ");
		if (isExternal()) {
			 sb.append("uri(\"");
			 sb.append(getExternalUri());
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
		if (uri == null && namespace != null && name != null) {
			return namespace + name;
		}
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((argumentTypes == null) ? 0 : argumentTypes.hashCode());
		result = prime * result + ((arguments == null) ? 0 : arguments.hashCode());
		result = prime * result + ((body == null) ? 0 : body.hashCode());
		result = prime * result + (isExternal ? 1231 : 1237);
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((namespace == null) ? 0 : namespace.hashCode());
		result = prime * result + ((returnTypes == null) ? 0 : returnTypes.hashCode());
		result = prime * result + ((uri == null) ? 0 : uri.hashCode());
		return result;
	}

	public boolean addVariable(VariableNode ruleVariable) {
		if (equationVariables == null) {
			equationVariables = new ArrayList<VariableNode>();
		}
		equationVariables.add(ruleVariable);
		return true;
	}
	
	public VariableNode getVariable(String uri) {
		if (equationVariables != null) {
			for (int i = 0; i < equationVariables.size(); i++) {
				if (equationVariables.get(i).toFullyQualifiedString().equals(uri)) {
					return equationVariables.get(i);
				}
			}
		}
		return null;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Equation other = (Equation) obj;
		if (argumentTypes == null) {
			if (other.argumentTypes != null)
				return false;
		} else if (!argumentTypes.equals(other.argumentTypes))
			return false;
		if (arguments == null) {
			if (other.arguments != null)
				return false;
		} else if (!arguments.equals(other.arguments))
			return false;
		if (body == null) {
			if (other.body != null)
				return false;
		} else if (!body.equals(other.body))
			return false;
		if (isExternal != other.isExternal)
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (namespace == null) {
			if (other.namespace != null)
				return false;
		} else if (!namespace.equals(other.namespace))
			return false;
		if (returnTypes == null) {
			if (other.returnTypes != null)
				return false;
		} else if (!returnTypes.equals(other.returnTypes))
			return false;
		if (uri == null) {
			if (other.uri != null)
				return false;
		} else if (!uri.equals(other.uri))
			return false;
		return true;
	}

	public List<Node> getReturnNodes() {
		return returnNodes;
	}

	public void setReturnNodes(List<Node> returnNodes) {
		this.returnNodes = returnNodes;
	}
	
	public void addReturnNode(Node retNode) {
		if (returnNodes == null) {
			returnNodes = new ArrayList<Node>();
		}
		returnNodes.add(retNode);
	}

	public String getExternalUri() {
		return externalUri;
	}

	public void setExternalUri(String externalUri) {
		this.externalUri = externalUri;
	}

	public boolean isVarArgs() {
		return varArgs;
	}

	public void setVarArgs(boolean varArgs) {
		this.varArgs = varArgs;
	}
}
