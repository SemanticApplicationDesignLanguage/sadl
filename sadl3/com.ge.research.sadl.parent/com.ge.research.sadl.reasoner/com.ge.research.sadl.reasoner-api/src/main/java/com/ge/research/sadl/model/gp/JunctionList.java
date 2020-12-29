package com.ge.research.sadl.model.gp;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * Class to add a Junction type (Conj, Disj) to the list of GraphPatternElements in the Junction
 * @author camfe
 *
 */
@SuppressWarnings("serial")
public class JunctionList extends ArrayList<GraphPatternElement> {
	private Junction.JunctionType junctionType;
	private Object context;		// the editor object associated with this Junction

	public Junction.JunctionType getJunctionType() {
		return junctionType;
	}

	public void setJunctionType(Junction.JunctionType junctionType) {
		this.junctionType = junctionType;
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder(getJunctionType().toString());
		sb.append("(");
		Iterator<GraphPatternElement> itr = this.iterator();
		while (itr.hasNext()) {
			sb.append(itr.next().toString());
		}
		sb.append(")");
		return sb.toString();
	}

	public Object getContext() {
		return context;
	}

	public void setContext(Object context) {
		this.context = context;
	}
}
