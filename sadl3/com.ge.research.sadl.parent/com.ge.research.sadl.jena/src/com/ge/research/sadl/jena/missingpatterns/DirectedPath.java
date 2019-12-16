package com.ge.research.sadl.jena.missingpatterns;

import java.util.ArrayList;
import java.util.List;

import com.ge.research.sadl.jena.missingpatterns.PathFinder.PatternType;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.reasoner.TranslationException;
import com.hp.hpl.jena.rdf.model.Resource;

public class DirectedPath {
	private Resource subject;
	private Object connection;
	private Resource object;
	private DirectedPath previous = null;
	private List<DirectedPath> next = null;
	private GraphPatternElement existingGraphPattern = null;
	private PatternType patternType = PatternType.Exploratory;
	public enum DirectedPathSource {LocalRestriction, OntologyRestriction, ClassHierarchy, OntologyRange}
	private DirectedPathSource pathSource = null;
	
	public DirectedPath(Resource s, Object p, Resource o) {
		setSubject(s);
		setConnection(p);
		setObject(o);
	}
	
	public DirectedPath(Resource s, Object p, Resource o, PatternType ptype) {
		setSubject(s);
		setConnection(p);
		setObject(o);
		setPatternType(ptype);
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("directedPath(");
		sb.append(getSubject() != null ? getSubject().toString() : "<null>");
		sb.append(",");
		sb.append(getConnection() != null ? getConnection().toString() : "<null>");
		sb.append(",");
		sb.append(getObject() != null ? getObject().toString() : "<null>");
		sb.append(")");
		return sb.toString();
	}
	
	/**
	 * Serialize not only this DirectedPath but all of those following paths (next)
	 * to the end of the chain
	 * @return
	 */
	public String entirePathString() {
		if (next == null || next.size() == 0) {
			return toString();
		}
		else if (next.size() > 1) {
			return "Directed path has branches in it; cannot serialize";
		}
		StringBuilder sb = new StringBuilder("[");
		sb.append(toString());
		sb.append(",");
		sb.append(next.get(0).entirePathString());
		sb.append("]");
		return sb.toString();
	}

	/**
	 * Method to walk to the end of a linked list of DirectedPaths and return the last path.
	 * @param directedPath
	 * @return
	 * @throws TranslationException
	 */
	public DirectedPath getLastDirectedPathInChain() throws TranslationException {
		if (getNext() != null) {
			if (getNext().size() != 1) {
				throw new TranslationException("Method call only valid for one next DirectedPath: " + toString());
			}
			else {
				return getNext().get(0).getLastDirectedPathInChain();
			}
		}
		return this;
	}

	public List<DirectedPath> getNext() {
		return next;
	}

	public void addNext(DirectedPath next) {
		if (this.next == null) {
			this.next = new ArrayList<DirectedPath>();
		}
		this.next.add(next);
	}

	/**
	 * Method to determine if two DirectedPaths are equal
	 */
    public boolean equals(Object p) {
        if (!(p instanceof DirectedPath)) return false;
        if (getSubject() == null && ((DirectedPath)p).getSubject() != null) return false;
        if (((DirectedPath)p).getSubject() == null && getSubject() != null) return false;
        if (getSubject() != null && ((DirectedPath)p).getSubject() != null && !getSubject().equals(((DirectedPath)p).getSubject())) return false;
        
        if (getConnection() == null && ((DirectedPath)p).getConnection() != null) return false;
        if (((DirectedPath)p).getConnection() == null && getConnection() != null) return false;
        if (getConnection() != null && ((DirectedPath)p).getConnection() != null && !getConnection().equals(((DirectedPath)p).getConnection())) return false;

        if (getObject() == null && ((DirectedPath)p).getObject() != null) return false;
        if (((DirectedPath)p).getObject() == null && getObject() != null) return false;
        if (getObject() != null && ((DirectedPath)p).getObject() != null && !getObject().equals(((DirectedPath)p).getObject())) return false;
        return true;
    }

	public Resource getSubject() {
		return subject;
	}

	public void setSubject(Resource subject) {
		this.subject = subject;
	}

	public Resource getObject() {
		return object;
	}

	public void setObject(Resource object) {
		this.object = object;
	}

	public Object getConnection() {
		return connection;
	}

	public void setConnection(Object connection) {
		this.connection = connection;
	}

	public GraphPatternElement getExistingGraphPattern() {
		return existingGraphPattern;
	}

	protected void setExistingGraphPattern(GraphPatternElement existingGraphPattern) {
		this.existingGraphPattern = existingGraphPattern;
	}

	public PatternType getPatternType() {
		return patternType;
	}

	protected void setPatternType(PatternType patternType) {
		this.patternType = patternType;
	}

	public DirectedPath getPrevious() {
		return previous;
	}

	public void setPrevious(DirectedPath previous) {
		this.previous = previous;
	}

	public DirectedPathSource getPathSource() {
		return pathSource;
	}

	public void setPathSource(DirectedPathSource pathSource) {
		this.pathSource = pathSource;
	}
}
/* End DirectedPath class definition */
