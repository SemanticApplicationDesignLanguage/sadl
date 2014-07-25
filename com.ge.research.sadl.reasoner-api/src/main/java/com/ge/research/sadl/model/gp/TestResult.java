package com.ge.research.sadl.model.gp;

import java.util.ArrayList;
import java.util.List;

import com.ge.research.sadl.model.gp.Test.ComparisonType;

public class TestResult {
	private boolean passed;
	
	private Object lhsResult;
	private Object rhsResult;
	private ComparisonType type;
	
	private String msg;

	public TestResult() {
		
	}
	
	public TestResult(boolean _passed) {
		setPassed(_passed);
	}

	public void setPassed(boolean _passed) {
		this.passed = _passed;
	}

	public boolean isPassed() {
		return passed;
	}
	
	@SuppressWarnings("unchecked")
	public void addLhsResult(Object newLhsResult) {
		if (lhsResult == null) {
			lhsResult = newLhsResult;
		}
		else if (lhsResult instanceof List<?>) {
			((List<Object>)lhsResult).add(newLhsResult);
		}
		else {
			List<Object> temp = new ArrayList<Object>();
			temp.add(lhsResult);
			temp.add(newLhsResult);
			lhsResult = temp;
		}
	}
	
	public Object getLhsResult() {
		return lhsResult;
	}
	
	@SuppressWarnings("unchecked")
	public void addRhsResult(Object newRhsResult) {
		if (rhsResult == null) {
			rhsResult = newRhsResult;
		}
		else if (rhsResult instanceof List<?>) {
			((List<Object>)rhsResult).add(newRhsResult);
		}
		else {
			List<Object> temp = new ArrayList<Object>();
			temp.add(temp);
			temp.add(newRhsResult);
			rhsResult = temp;
		}
	}
	
	public Object getRhsResult() {
		return rhsResult;
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (lhsResult != null && rhsResult != null) {
			sb.append(lhsResult.toString());
			sb.append(" ");
			sb.append((type != null) ? type.toString() : " ? ");
			sb.append("?");
			sb.append(" ");
			sb.append(rhsResult.toString());
		}
		else if (lhsResult != null) {
			sb.append(lhsResult.toString());
		}
		else if (rhsResult != null) {
			sb.append(rhsResult.toString());
		}
		if (getMsg() != null) {
			sb.append(getMsg());
		}
		return sb.toString();
	}
	
	public String toString(Test test) {
		StringBuilder sb = new StringBuilder();
		if (test.getRhs() == null) {
			// this test must be a triple pattern--the LHS 
			// the expected results will be in the TestResult.rhsResult
			if (lhsResult != null && rhsResult != null) {
				sb.append(rhsResult.toString());
				sb.append(" ");
				sb.append((type != null) ? type.toString() : " ? ");
				sb.append("?");
				sb.append(" ");
				sb.append(lhsResult.toString());
			}
			else if (rhsResult != null) {
				sb.append("(no match found) ");
				sb.append((type != null) ? type.toString() : " ? ");
				sb.append("?");
				sb.append(" ");
				sb.append(rhsResult.toString());
				sb.append("\n");
			}
			else if (lhsResult != null) {
				sb.append(lhsResult.toString());
				sb.append((type != null) ? type.toString() : " ? ");
				sb.append("?");
				sb.append(" (no match found)\n");
			}
			return sb.toString();
		}
		else {
			return toString();
		}
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}

	public String getMsg() {
		return msg;
	}

	public void setType(ComparisonType type) {
		this.type = type;
	}

	public ComparisonType getType() {
		return type;
	}
}
