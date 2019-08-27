package com.ge.research.sadl.model.gp;

abstract public class SadlCommand {
	private int lineNo;
	private int length;
	private int offset;
	
	// editor object for marker addition to editor
	private Object context;
	
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

	public Object getContext() {
		return context;
	}

	public void setContext(Object context) {
		this.context = context;
	}

}