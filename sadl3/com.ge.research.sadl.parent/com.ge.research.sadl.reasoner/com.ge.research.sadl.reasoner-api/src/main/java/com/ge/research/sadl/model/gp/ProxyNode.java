package com.ge.research.sadl.model.gp;

import java.util.List;

import com.ge.research.sadl.reasoner.InvalidTypeException;

/**
 * This class stand as a proxy for an embedded element such as a GraphPatternElement
 * in another element such as a TripleElement or a BuiltinElement 
 * or a Query at the Test level.
 * 
 * @author 200005201
 *
 */
public class ProxyNode extends Node {
	
	private GraphPatternElement proxyFor = null;
	private Node replacementNode = null;
	
	/**
	 * Constructor with single argument for the GraphPatternElement for which it is proxy
	 * @param _proxyObj
	 * @throws InvalidTypeException
	 */
	public ProxyNode(GraphPatternElement _proxyObj) throws InvalidTypeException {
		if (_proxyObj == null) {
			throw new InvalidTypeException("ProxyNode constructor called with null argument; this should not happen.");
		}
		setProxyFor(_proxyObj);
	}
	
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	public Object clone() throws CloneNotSupportedException {
		GraphPatternElement gpe = getProxyFor();
		try {
			return new ProxyNode(newCopyOfGraphPatternElement(gpe));
		} catch (InvalidTypeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
	/**
	 * Method to create a new, independent copy of a GraphPatternElement
	 * @param gpe
	 * @return
	 * @throws CloneNotSupportedException 
	 */
	private GraphPatternElement newCopyOfGraphPatternElement(GraphPatternElement gpe) throws CloneNotSupportedException {
		GraphPatternElement newGPE = null;
		if (gpe instanceof TripleElement) {
			newGPE = new TripleElement(((TripleElement)gpe).getSubject(), ((TripleElement)gpe).getPredicate(), ((TripleElement)gpe).getObject());
		}
		else if (gpe instanceof BuiltinElement) {
			BuiltinElement oldBE = (BuiltinElement)gpe;
			BuiltinElement newBE = new BuiltinElement();
			newBE.setFuncName(oldBE.getFuncName());
			newBE.setFuncUri(oldBE.getFuncUri());
			newBE.setFuncPrefix(oldBE.getFuncPrefix());
			newBE.setArgumentTypes(oldBE.getArgumentTypes());
			newBE.setReturnTypes(oldBE.getReturnTypes());
			newBE.setContext(oldBE.getContext());
			for (Node arg : oldBE.getArguments()) {
				if (arg instanceof ProxyNode) {
					newBE.addArgument((Node) ((ProxyNode)arg).clone());
				}
				else {
					newBE.addArgument(arg);
				}
			}
			newGPE = newBE;
		}
		else if (gpe instanceof Junction) {
			Junction oldJ = (Junction) gpe;
			Junction newJ = new Junction();
			newJ.setContext(oldJ.getContext());
			newJ.setJunctionName(oldJ.getJunctionName());
			if (oldJ.getLhs() instanceof ProxyNode) {
				newJ.setLhs(((ProxyNode)oldJ.getLhs()).clone());
			}
			else {
				newJ.setLhs(oldJ.getLhs());
			}
			if (oldJ.getRhs() instanceof ProxyNode) {
				newJ.setRhs(((ProxyNode)oldJ.getRhs()).clone());
			}
			else {
				newJ.setRhs(oldJ.getRhs());
			}
			newGPE = newJ;
		}
		return newGPE;
	}
	
	/**
	 * Set the GraphPatternElement for which this ProxyNode is proxy
	 * @param _proxyObj
	 */
	public void setProxyFor(GraphPatternElement _proxyObj) {
		proxyFor = _proxyObj;
	}

	/**
	 * Get the GraphPatternElement for which this ProxyNode is proxy
	 * @return
	 */
	public GraphPatternElement getProxyFor() {
		return proxyFor;
	}
	
	/**
	 * Node to replace this ProxyNode, if any
	 * @param replacementNode
	 */
	public void setReplacementNode(Node replacementNode) {
		this.replacementNode = replacementNode;
	}

	/**
	 * Get ProxyNode replacement Node
	 * @return
	 */
	public Node getReplacementNode() {
		return replacementNode;
	}

	@Override
	public String toString() {
		try {
			if (proxyFor != null) {
				if (proxyFor instanceof List<?>) {
					if (((List<?>)proxyFor).size() == 1) {
						return ((List<?>)proxyFor).get(0).toString();					
					}
					else {
						return "(invalid ProxyNode)";
					}
				}
				return proxyFor.toString();
			}
			return "(null proxy content)";
		}
		catch (Throwable t) {
			t.printStackTrace();
			return "(Exception encountered: " + t.getMessage() + ")";
		}
	}

	@Override
	public String toFullyQualifiedString() {
		try {
			if (proxyFor != null) {
				if (proxyFor instanceof List<?>) {
					if (((List<?>)proxyFor).size() == 1) {
						Object pfr = ((List<?>)proxyFor).get(0);
						if (pfr instanceof Node) {
							return ((Node) pfr).toFullyQualifiedString();					
						}
						else if (pfr instanceof GraphPatternElement) {
							return proxyFor.toFullyQualifiedString();
						}
						else {
							return ((List<?>)proxyFor).get(0).toString();					
						}
					}
					else {
						return "(invalid ProxyNode)";
					}
				}
				else if (proxyFor instanceof GraphPatternElement) {
					return proxyFor.toFullyQualifiedString();
				}
//				else if (proxyFor instanceof Node) {
//					return ((Node) proxyFor).toFullyQualifiedString();
//				}
				else {
					return proxyFor.toString();
					
				}
			}
			return "(null proxy content)";
		}
		catch (Throwable t) {
			t.printStackTrace();
			return "(Exception encountered: " + t.getMessage() + ")";
		}
	}

	@Override
	public String toDescriptiveString() {
		try {
			if (proxyFor != null) {
				if (proxyFor instanceof List<?>) {
					if (((List<?>)proxyFor).size() == 1) {
						Object pfr = ((List<?>)proxyFor).get(0);
						if (pfr instanceof Node) {
							return ((Node) pfr).toDescriptiveString();					
						}
						else if (pfr instanceof GraphPatternElement) {
							return proxyFor.toDescriptiveString();
						}
						else {
							return  ((List<?>)proxyFor).get(0).toString();					
						}
					}
					else {
						return "(invalid ProxyNode)";
					}
				}
				else if (proxyFor instanceof GraphPatternElement) {
					return proxyFor.toDescriptiveString();
				}
//				else if (proxyFor instanceof Node) {
//					return ((Node) proxyFor).toDescriptiveString();
//				}
				else {
					return proxyFor.toString();
					
				}
			}
			return "(null proxy content)";
		}
		catch (Throwable t) {
			t.printStackTrace();
			return "(Exception encountered: " + t.getMessage() + ")";
		}
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((proxyFor == null) ? 0 : proxyFor.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ProxyNode other = (ProxyNode) obj;
		if (proxyFor == null) {
			if (other.proxyFor != null)
				return false;
		} else if (!proxyFor.equals(other.proxyFor))
			return false;
		return true;
	}
}
