package com.ge.research.sadl.parser;

import java.io.Reader;

import org.antlr.runtime.CharStream;

public class ReaderCharStream implements CharStream {

	private Reader reader;

	public ReaderCharStream(Reader reader) {
		this.reader = reader;
	}
	
	public Reader getReader() {
		return reader;
	}
	
	@Override
	public void consume() {
		throw new UnsupportedOperationException();
	}

	@Override
	public int LA(int i) {
		throw new UnsupportedOperationException();
	}

	@Override
	public int mark() {
		throw new UnsupportedOperationException();
	}

	@Override
	public int index() {
		throw new UnsupportedOperationException();
	}

	@Override
	public void rewind(int marker) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void rewind() {
		throw new UnsupportedOperationException();
	}

	@Override
	public void release(int marker) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void seek(int index) {
		throw new UnsupportedOperationException();
	}

	@Override
	public int size() {
		throw new UnsupportedOperationException();
	}

	@Override
	public String getSourceName() {
		throw new UnsupportedOperationException();
	}

	@Override
	public String substring(int start, int stop) {
		throw new UnsupportedOperationException();
	}

	@Override
	public int LT(int i) {
		throw new UnsupportedOperationException();
	}

	@Override
	public int getLine() {
		throw new UnsupportedOperationException();
	}

	@Override
	public void setLine(int line) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void setCharPositionInLine(int pos) {
		throw new UnsupportedOperationException();
	}

	@Override
	public int getCharPositionInLine() {
		throw new UnsupportedOperationException();
	}
	
}