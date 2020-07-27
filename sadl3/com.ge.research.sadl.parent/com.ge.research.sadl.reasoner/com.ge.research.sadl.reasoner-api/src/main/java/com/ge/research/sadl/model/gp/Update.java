package com.ge.research.sadl.model.gp;

import java.util.ArrayList;
import java.util.List;

public class Update extends Query {
	private String secondKeyword = null;
	private boolean deleteData = false;
	private boolean insertData = false;
	private List<GraphPatternElement> deletePatterns = null;
	private List<GraphPatternElement> insertPatterns = null;


	public String getSecondKeyword() {
		return secondKeyword;
	}

	public void setSecondKeyword(String secondKeyword) {
		this.secondKeyword = secondKeyword;
	}

	public boolean isDeleteData() {
		return deleteData;
	}

	public void setDeleteData(boolean deleteData) {
		this.deleteData = deleteData;
	}

	public boolean isInsertData() {
		return insertData;
	}

	public void setInsertData(boolean insertData) {
		this.insertData = insertData;
	}

	public List<GraphPatternElement> getDeletePatterns() {
		return deletePatterns;
	}

	public void addDeletePattern(GraphPatternElement deletePattern) {
		if (deletePatterns == null) {
			deletePatterns = new ArrayList<GraphPatternElement>();
		}
		deletePatterns.add(deletePattern);
	}

	public List<GraphPatternElement> getInsertPatterns() {
		return insertPatterns;
	}

	public void addInsertPattern(GraphPatternElement insertPattern) {
		if (insertPatterns == null) {
			insertPatterns = new ArrayList<GraphPatternElement>();
		}
		insertPatterns.add(insertPattern);
	}
	
	public String toString() {
		if (sparqlQueryString != null) {
			return sparqlQueryString;
		}
		return toString(false, false);
	}

	public String toFullyQualifiedString() {
		if (sparqlQueryString != null) {
			return sparqlQueryString;
		}
		return toString(true, false);
	}
	
	public String toDescriptiveString() {
		if (sparqlQueryString != null) {
			return sparqlQueryString;
		}
		return toString(true, true);
	}

	private String toString(boolean fullyQualified, boolean descriptive) {
		StringBuilder sb = new StringBuilder();
		if (getDeletePatterns() != null) {
			sb.append("delete ");
			for (int i = 0; i < getDeletePatterns().size(); i++) {
				if (i > 0) {
					sb.append(" . ");
				}
				if (!fullyQualified && !descriptive) {
					sb.append(getDeletePatterns().get(i).toString());
				}
				else if (fullyQualified && !descriptive) {
					sb.append(getDeletePatterns().get(i).toFullyQualifiedString());
				}
				else {
					sb.append(getDeletePatterns().get(i).toDescriptiveString());
				}
			}
			sb.append(" ");
		}
		
		if (getInsertPatterns() != null) {
			sb.append("insert ");
			for (int i = 0; i < getInsertPatterns().size(); i++) {
				if (i > 0) {
					sb.append(" . ");
				}
				if (!fullyQualified && !descriptive) {
					sb.append(getInsertPatterns().get(i).toString());
				}
				else if (fullyQualified && !descriptive) {
					sb.append(getInsertPatterns().get(i).toFullyQualifiedString());
				}
				else {
					sb.append(getInsertPatterns().get(i).toDescriptiveString());
				}
			}
		}

		if (getPatterns() != null) {
			sb.append(" where ");
			for (int i = 0; i < getPatterns().size(); i++) {
				if (i > 0) {
					sb.append(" . ");
				}
				if (!fullyQualified && !descriptive) {
					sb.append(getPatterns().get(i).toString());
				}
				else if (fullyQualified && !descriptive) {
					sb.append(getPatterns().get(i).toFullyQualifiedString());
				}
				else {
					sb.append(getPatterns().get(i).toDescriptiveString());
				}
			}
		}
		return sb.toString();
	}
}
