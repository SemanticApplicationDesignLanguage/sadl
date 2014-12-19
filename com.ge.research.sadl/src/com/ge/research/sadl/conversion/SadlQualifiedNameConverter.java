package com.ge.research.sadl.conversion;

import org.eclipse.xtext.naming.IQualifiedNameConverter.DefaultImpl;

/**
 * SADL uses ':' to for qualified names, Xtext assumes '.' by default.
 * @author thoms
 */
public class SadlQualifiedNameConverter extends DefaultImpl {
	@Override
	public String getDelimiter() {
		return ":";
	}
}
