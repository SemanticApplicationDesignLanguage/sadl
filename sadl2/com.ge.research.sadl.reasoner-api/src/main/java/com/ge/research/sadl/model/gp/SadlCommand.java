package com.ge.research.sadl.model.gp;

public class SadlCommand {
	private int lineNo;
	private int length;
	private int offset;
	
	public SadlCommand() {
		super();
	}

	public void setLineNo(int lineNo) {
		this.lineNo = lineNo;
	}

	public int getLineNo() {
		return lineNo;
	}

	public void setLength(int length) {
		this.length = length;
	}

	public int getLength() {
		return length;
	}

	public void setOffset(int offset) {
		this.offset = offset;
	}

	public int getOffset() {
		return offset;
	}

}