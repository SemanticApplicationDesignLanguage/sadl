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
	
	private Node replacementNode = null;
	
	public ProxyNode(Object _proxyObj) throws InvalidTypeException {
		if (_proxyObj == null) {
			throw new InvalidTypeException("ProxyNode constructor called with null argument; this should not happen.");
		}
		setProxyFor(_proxyObj);
	}
	
	private Object proxyFor = null;

	public void setProxyFor(Object _proxyObj) {
		proxyFor = _proxyObj;
	}

	public Object getProxyFor() {
		return proxyFor;
	}
	
	public String toString() {
		try {
			if (proxyFor != null) {
				if (proxyFor instanceof List<?>) {
					if (((List<?>)proxyFor).size() == 1) {
						return "(" + ((List<?>)proxyFor).get(0).toString() + ")";					
					}
					else {
						return "(invalid ProxyNode)";
					}
				}
				return "(" + proxyFor.toString() + ")";
			}
			return "(null proxy content)";
		}
		catch (Throwable t) {
			t.printStackTrace();
			return "(Exception encountered: " + t.getMessage() + ")";
		}
	}

	public void setReplacementNode(Node replacementNode) {
		this.replacementNode = replacementNode;
	}

	public Node getReplacementNode() {
		return replacementNode;
	}
}
