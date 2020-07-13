package com.ge.research.sadl.jena.missingpatterns;

import com.ge.research.sadl.model.gp.NamedNode;
import org.apache.jena.rdf.model.Resource;

public class MultiplePathsFoundException extends Exception {
	/**
	 * Class to communicate an ambiguity found during path finding.
	 */
	private static final long serialVersionUID = -8791042365230521850L;
	private NamedNode intersection;
	
	private Resource topResource;
	private Resource bottomResource;
	private DirectedPath dPath1;
	private DirectedPath dPath2;
	
	public MultiplePathsFoundException(Resource top, Resource bottom, DirectedPath dp1, DirectedPath dp2) {
		super();
		setTopResource(top);
		setBottomResource(bottom);
		setdPath1(dp1);
		setdPath2(dp2);
	}

	public MultiplePathsFoundException(Resource top, Resource bottom, DirectedPath dp1, DirectedPath dp2, String msg) {
		super(msg);
		setTopResource(top);
		setBottomResource(bottom);
		setdPath1(dp1);
		setdPath2(dp2);
	}

	public NamedNode getIntersection() {
		return intersection;
	}

	private void setIntersection(NamedNode intersection) {
		this.intersection = intersection;
	}
	
	public String getMessage() {
		if (super.getMessage() != null) {
			return super.getMessage();
		}
		else if (getTopResource() != null) {
			StringBuilder sb = new StringBuilder("Ambiguity found: two paths lead from '");
			sb.append(getTopResource().toString());
			sb.append("' to '");
			sb.append(getBottomResource().toString());
			sb.append("', one by '");
			sb.append(getdPath1().entirePathString());
			sb.append("'");
			sb.append(" and one by '");
			sb.append(getdPath2().entirePathString());
			sb.append("'");
			return sb.toString();
		}
		else {
			return "Invalid content of MultiplePathsFoundException";
		}
	}

	public Resource getTopResource() {
		return topResource;
	}

	private void setTopResource(Resource topResource) {
		this.topResource = topResource;
	}

	public DirectedPath getdPath1() {
		return dPath1;
	}

	private void setdPath1(DirectedPath dPath1) {
		this.dPath1 = dPath1;
	}

	public DirectedPath getdPath2() {
		return dPath2;
	}

	private void setdPath2(DirectedPath dPath2) {
		this.dPath2 = dPath2;
	}

	public Resource getBottomResource() {
		return bottomResource;
	}

	private void setBottomResource(Resource bottomResource) {
		this.bottomResource = bottomResource;
	}
}
