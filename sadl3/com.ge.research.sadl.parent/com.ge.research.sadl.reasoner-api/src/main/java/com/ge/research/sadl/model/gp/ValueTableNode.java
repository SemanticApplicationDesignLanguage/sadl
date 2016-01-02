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

package com.ge.research.sadl.model.gp;

import java.util.List;

import com.ge.research.sadl.reasoner.ResultSet;


public class ValueTableNode extends Node {
	private List<List<Node>> rows = null;

	public void setRows(List<List<Node>> rows) {
		this.rows = rows;
	}

	public List<List<Node>> getRows() {
		return rows;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((rows == null) ? 0 : rows.hashCode());
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
		ValueTableNode other = (ValueTableNode) obj;
		if (rows == null) {
			if (other.rows != null)
				return false;
		} else if (!rows.equals(other.rows))
			return false;
		return true;
	}
	
	/**
	 * convert this ValueTableNode to a ResultSet for a common comparison format
	 * @return
	 */
	public ResultSet toResultSet() {
		if (rows == null || rows.size() == 0) {
			return null;
		}
		int cols = 0;
		for (int irow = 0; irow < rows.size(); irow++) {
			if (rows.get(irow).size() > cols) {
				cols = rows.get(irow).size();
			}
		}
		Object[][] rsTable = new Object[rows.size()][cols];
		for (int irow = 0; irow < rows.size(); irow++) {
			List<Node> row = rows.get(irow);
			for (int icol = 0; icol < row.size(); icol++) {
				Node nval = row.get(icol);
				if (nval instanceof KnownNode) {
					rsTable[irow][icol] = nval;
				}
				else if (nval instanceof com.ge.research.sadl.model.gp.Literal) {
					rsTable[irow][icol] = ((com.ge.research.sadl.model.gp.Literal)nval).getValue();
				}
				else if (nval instanceof NamedNode) {
					rsTable[irow][icol] = ((NamedNode)nval).toFullyQualifiedString();
				}
				else if (nval instanceof ValueTableNode) {
					rsTable[irow][icol] = ((ValueTableNode)nval).toResultSet();
				}
			}
		}
		return new ResultSet(rsTable);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("{");
		for (int i = 0; i < rows.size(); i++) {
			if (i > 0) {
				sb.append(",");
			}
			List<Node> row = rows.get(i);
			sb.append("[");
			for (int j = 0; j < row.size(); j++) {
				if (j > 0) {
					sb.append(",");
				}
				sb.append(row.get(j).toString());
			}
			sb.append("]");
		}
		sb.append("}");
		return sb.toString();
	}

	@Override
	public String toFullyQualifiedString() {
		StringBuilder sb = new StringBuilder("{");
		for (int i = 0; i < rows.size(); i++) {
			if (i > 0) {
				sb.append(",");
			}
			List<Node> row = rows.get(i);
			sb.append("[");
			for (int j = 0; j < row.size(); j++) {
				if (j > 0) {
					sb.append(",");
				}
				sb.append(row.get(j).toFullyQualifiedString());
			}
			sb.append("]");
		}
		sb.append("}");
		return sb.toString();
	}
}
