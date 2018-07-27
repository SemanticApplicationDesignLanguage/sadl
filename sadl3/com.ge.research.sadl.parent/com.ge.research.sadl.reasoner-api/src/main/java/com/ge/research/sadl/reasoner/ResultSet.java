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

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.3 $ Last modified on   $Date: 2014/06/03 17:39:56 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner;

import java.util.ArrayList;
import java.util.List;

import com.ge.research.sadl.model.gp.ConstantNode;
import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.ValueTableNode;

public class ResultSet {
	private Object[][] table;
	private String[] header;
	public  int cursor;
	private boolean showNamespaces = true;  // default is to show namespaces of URIs
	
	public ResultSet(Object[][] data){		
		table = new Object[data.length][data[0].length];
		insertData(data);		
	}
	
	public ResultSet(String[] columnNames, Object[][] data){		
		header = new String[columnNames.length];		
		table = new Object[data.length][data[0].length]; 
		setColumnNames(columnNames);
		insertData(data);
	}
	
	public boolean hasNext() {
		int rowCount = getRowCount();
		if(cursor < rowCount)			
			return true;
		else
			return false;
	}
	public Object[] next(){
		if (cursor >= getRowCount()){
			return null;
		}			
		Object[] result = table[cursor];
		cursor++;
		return result;
	}
	public Object[] previous(){
		cursor--;
		if (cursor < 0){
			return null;
		}
		Object[] result = table[cursor];		
		return result;		
	}	
	
	public Object[] first(){
		cursor = 0;
		return table[cursor];
	}
	
	public Object[] last(){		
		cursor = getRowCount()-1;
		return table[cursor];
	}
	
	public int getRowCount(){
		return table.length;
	}
	
	public int getColumnCount(){
		return table[0].length;		
	}
	
	private int setColumnNames(String[] columnNames){
		int columnCount = columnNames.length;
		for(int i=0;i<columnCount;i++)
			header[i] = columnNames[i];
		return columnNames.length;
	}
	
	public String[] getColumnNames(){
		return header;
	}
	
	/**
	 * Method to return all of the data values as a 2-dimensional array
	 * @return the ResultSet data
	 */
	public Object[][] getData() {
		return table;
	}
		
	public Object getResultAt(int r, int c){
		if (!showNamespaces) {
			return extractLocalName(table[r][c]);
		}
		return table[r][c];
	}
	
	private void insertData(Object[][] data){
		for(int i=0;i<data.length;i++)
			for(int j=0; j<data[i].length;j++)
				table[i][j] = data[i][j];
	}
	
	/**
	 * Compare this ResultSet to the argument ResultSet.
	 * Two ResultSets are equal if every row in one is equal to some row in the other and there are no extra rows.
	 */
	public boolean equals(Object otherObj) {
		if (!(otherObj instanceof ResultSet)) {
			return false;
		}
		ResultSet ors = (ResultSet)otherObj;
		if (ors.getColumnCount() != getColumnCount()) {
			return false;
		}
		if (ors.getRowCount() != getRowCount()) {
			return false;
		}
		
		// look for a match for each row of this ResultSet
		for (int irow = 0; irow < getRowCount(); irow++) {
			// there is a match only if there is some row in the other ResultSet in which each column matches
			boolean foundMatchingRow = false;
			for (int otherRow = 0; otherRow < getRowCount(); otherRow++) {
				boolean rowMatches = true;
				for (int icol = 0; icol < getColumnCount(); icol++) {
					if (!valuesMatch(getResultAt(irow, icol),ors.getResultAt(otherRow, icol))) {
						// this row doesn't match
						rowMatches = false;
						break;
					}
				}
				if (rowMatches) {
					// we've found a match for this row
					foundMatchingRow = true;
					break;
				}
			}
			if (!foundMatchingRow) {
				return false;
			}
		}
		return true;
	}
	
	private boolean containedIn(ResultSet ors) {
		if (ors.getColumnCount() != getColumnCount()) {
			return false;
		}
		// look for a match for each row of this ResultSet
		for (int irow = 0; irow < getRowCount(); irow++) {
			// there is a match only if there is some row in the other ResultSet in which each column matches
			boolean foundMatchingRow = false;
			for (int otherRow = 0; otherRow < ors.getRowCount(); otherRow++) {
				boolean rowMatches = true;
				for (int icol = 0; icol < getColumnCount(); icol++) {
					if (!valuesMatch(getResultAt(irow, icol), ors.getResultAt(otherRow, icol))) {
						// this row doesn't match
						rowMatches = false;
						break;
					}
				}
				if (rowMatches) {
					// we've found a match for this row
					foundMatchingRow = true;
					break;
				}
			}
			if (!foundMatchingRow) {
				return false;
			}
		}
		return false;
	}
	
	private boolean containedIn(Object val) {
		// Can't apply this to another result set as it will recurse and give true if any element matches
		if (!(val instanceof ResultSet)) {
			for (int irow = 0; irow < getRowCount(); irow++) {
				for (int icol = 0; icol < getColumnCount(); icol++) {
					if (valuesMatch(getResultAt(irow, icol), val)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/**
	 * Static method to compare two object values that supports ConstantNode "known"
	 * 
	 * @param lhval
	 * @param rhval
	 * @return
	 */
	public static synchronized boolean valuesMatch(Object lhval, Object rhval) {
		if (ITranslator.isKnownNode(lhval) && rhval != null) {
			return true;
		}
		if (ITranslator.isKnownNode(rhval) && lhval != null) {
			return true;
		}
		if (lhval instanceof Number && rhval instanceof Number) {
			double lhdbl = ((Number)lhval).doubleValue();
			double rhdbl = ((Number)rhval).doubleValue();
			return areDoublesEqual(lhdbl, rhdbl);
		}
		if (lhval != null && rhval != null) {
			if (lhval.equals(rhval)) {
				return true;
			}
			if (lhval instanceof ResultSet && rhval instanceof ResultSet &&
					((ResultSet)rhval).containedIn((ResultSet)lhval)) {
				return true;
			}
			if (lhval instanceof ResultSet && rhval instanceof ResultSet &&
					((ResultSet)lhval).containedIn((ResultSet)rhval)) {
				return true;
			}
			if (rhval instanceof ResultSet) {
				if (((ResultSet)rhval).containedIn(lhval)) {
					return true;
				}
			}
			else if (lhval instanceof ResultSet) {
				if (((ResultSet)lhval).containedIn(rhval)) {
					return true;
				}
			}
			else if (lhval instanceof NamedNode && rhval instanceof String) {
				if (((NamedNode)lhval).toFullyQualifiedString().equals(rhval)) {
					return true;
				}
			}
			else if (rhval instanceof NamedNode && lhval instanceof String) {
				if (((NamedNode)rhval).toFullyQualifiedString().equals(lhval)) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Method to determine if first argument is numerically less than second argument
	 * 
	 * @param lhval
	 * @param rhval
	 * @return
	 */
	public static boolean lessThan(Object lhval, Object rhval) {
		if (lhval instanceof Number && rhval instanceof Number) {
			double lhdbl = ((Number)lhval).doubleValue();
			double rhdbl = ((Number)rhval).doubleValue();
			return (lhdbl < rhdbl);
		}
		else if (lhval instanceof String && rhval instanceof String) {
			int compval = ((String)lhval).compareTo((String)rhval);
			if (compval < 0) {
				return true;
			}
		}
		else if (lhval instanceof ResultSet) {
			int cols = ((ResultSet)lhval).getColumnCount();
			int rows = ((ResultSet)lhval).getRowCount();
			for (int ir = 0; ir < rows; ir++) {
				for (int ic = 0; ic < cols; ic++) {
					Object rcval = ((ResultSet)lhval).getResultAt(ir, ic);
					if (!lessThan(rcval, rhval)) {
						return false;
					}
				}
			}
			return true;
		}
		else if (rhval instanceof ResultSet) {
			int cols = ((ResultSet)rhval).getColumnCount();
			int rows = ((ResultSet)rhval).getRowCount();
			for (int ir = 0; ir < rows; ir++) {
				for (int ic = 0; ic < cols; ic++) {
					Object rcval = ((ResultSet)rhval).getResultAt(ir, ic);
					if (!lessThan(lhval, rcval)) {
						return false;
					}
				}
			}
			return true;
		}
		return false;
	}

	/**
	 * Method to determine that two sets match exaclty--have the same members although order doesn't matter
	 * 
	 * @param lhval
	 * @param rhval
	 * @return
	 */
	public static synchronized boolean valuesMatchExactly(Object lhval, Object rhval) {
		if (ITranslator.isKnownNode(lhval) && rhval != null) {
			return true;
		}
		if (ITranslator.isKnownNode(rhval) && lhval != null) {
			return true;
		}
		if (lhval instanceof Number && rhval instanceof Number) {
			double lhdbl = ((Number)lhval).doubleValue();
			double rhdbl = ((Number)rhval).doubleValue();
			return areDoublesEqual(lhdbl, rhdbl);
		}
		if (lhval != null && rhval != null) {
			if (lhval.equals(rhval)) {
				return true;
			}
			if (lhval instanceof ResultSet && rhval instanceof ResultSet &&
					((ResultSet)rhval).containedIn((ResultSet)lhval)) {
				return true;
			}
			if (lhval != null && rhval != null) {
				return lhval.equals(rhval);
			}
		}
		return false;
	}

	public static synchronized boolean areDoublesEqual(double testVal, double actVal) {
		String tv = new Double(testVal).toString();
		while (tv.length() > 0 && !Character.isDigit(tv.charAt(0))) {
			tv = tv.substring(1);
		}
		int numDigits;
		if (tv.indexOf('.') > 0) {
			numDigits = tv.length() - 1;
		}
		else {
			numDigits = tv.length();
		}
		double relErrorMax = 1.0 / (numDigits * 100.0);
		if (testVal != 0.0) {
			double relDiff = Math.abs((testVal - actVal))/Math.abs(testVal);
			if (relDiff < relErrorMax) {
				return true;
			}
		}
		else if (Math.abs(testVal - actVal) < relErrorMax) {
			return true;
		}
		return false;
	}

	/**
	 * Simple output of headers followed by row(s) of data, all comma-separated.
	 */
	public String toString() {
		return toStringWithIndent(0);
	}
	
	/**
	 * Method to output headers followed by row(s) of data, all comma-separated, but with each line indented the indicated number of spaces
	 * @param indent
	 * @return
	 */
	public String toStringWithIndent(int indent) {
		StringBuilder sb = new StringBuilder();
		if (table != null && table[0] != null) {
			if (table.length > 1) {
				// there is more than 1 row of data
				if (header != null && header.length > 0) {
					for (int j = 0; j < indent; j++) {
						sb.append(" ");
					}
					for (int i = 0; i < header.length; i++) {
						if (i > 0) sb.append(",");
						sb.append(quoteAsNeeded(header[i]));
					}
					sb.append(System.getProperty("line.separator"));
				}
				for (int i = 0; i < table.length; i++) {
					for (int j = 0; j < indent; j++) {
						sb.append(" ");
					}
					for (int j = 0; j < table[i].length; j++) {
						if (j > 0) sb.append(",");
						Object val = getResultAt(i,j);
						if (val instanceof String) {
							sb.append(val != null ? quoteAsNeeded(val) : val);
						}
						else {
							sb.append(val != null ? val.toString() : val);
						}
					}
					sb.append(System.getProperty("line.separator"));
				}
			}
			else {
				// there's just one row
				for (int j = 0; j < indent; j++) {
					sb.append(" ");
				}
				for (int i = 0; i < table[0].length; i++) {
					if (i > 0) {
						sb.append(", ");
					}
					if (header != null && i < header.length) {
						sb.append(quoteAsNeeded(header[i]));
						sb.append(" = ");
					}
					Object val = getResultAt(0,i);
					if (val instanceof String) {
						sb.append(val != null ? quoteAsNeeded(val) : val);
					}
					else {
						sb.append(val != null ? val.toString() : val);
					}
				}
				sb.append(System.getProperty("line.separator"));
			}
		}	
		return sb.toString();
	}
	
	/**
	 * Method to place strings in double quotes and to replace any object's toString containing whitespace or a double quote(s) with a quoted string
	 * @param val
	 * @return
	 */
	private Object quoteAsNeeded(Object val) {
		if (val instanceof String) {
			StringBuilder sb = new StringBuilder();
			sb.append("\"");
			sb.append(((String)val).replaceAll("\"", "\"\""));
			sb.append("\"");
			return sb.toString();
		}
		else if (val != null &&  containsWhitespaceOrQuote(val.toString())) {
			StringBuilder sb = new StringBuilder();
			sb.append("\"");
			sb.append(val.toString().replaceAll("\"", "\"\""));
			sb.append("\"");
			return sb.toString();
		}
		else {
			return val;
		}
	}
	
	/**
	 * Method to determine if a string contains whitespace of double quotes
	 * @param str
	 * @return
	 */
	public static boolean containsWhitespaceOrQuote(String str) {
		int strLen = str.length();
		for (int i = 0; i < strLen; i++) {
			if (Character.isWhitespace(str.charAt(i))) {
				return true;
			}
			else if (str.charAt(i) == '"') {
				return true;
			}
		}
		return false;
	}

	public ValueTableNode toValueTableNode() {
		ValueTableNode vtn = new ValueTableNode();
		int cols = getColumnCount();
		int rows = getRowCount();
		List<List<Node>> contents = new ArrayList<List<Node>>();
		for (int irow = 0; irow < rows; irow++) {
			List<Node> row = new ArrayList<Node>();
			for (int icol = 0; icol < cols; icol++) {
				Object val = getResultAt(irow, icol);
				if (val instanceof String) {
					if (((String)val).startsWith("http://") && ((String)val).indexOf("#") > 0) {
						int ilb = ((String)val).indexOf("#");
						NamedNode n = new NamedNode(((String)val).substring(ilb + 1));
						n.setNamespace(((String)val).substring(0, ilb + 1));
						row.add(n);
					}
					else {
						com.ge.research.sadl.model.gp.Literal litval = new com.ge.research.sadl.model.gp.Literal();
						litval.setValue(val);
						row.add(litval);
					}
				}
				else {
					Literal litval = new Literal();
					litval.setValue(val);
					row.add(litval);
				}
			}
			contents.add(row);
		}
		vtn.setRows(contents);
		return vtn;
	}

	public String extractLocalName(Object val) {
		if (val instanceof String) {
			if ((((String)val).startsWith("http://") || ((String)val).startsWith("file:/"))  && ((String)val).indexOf("#") > 0) {
				int ilb = ((String)val).indexOf("#");
				return ((String)val).substring(ilb + 1);
			}
		}
		return (val != null) ? val.toString() : null;
	}

	public void setShowNamespaces(boolean _showNamespaces) {
		showNamespaces = _showNamespaces;
	}
	
	public boolean getShowNamespaces() {
		return showNamespaces;
	}

}
