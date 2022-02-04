package com.naturalsemanticsllc.sadl.evaleq;

import static org.junit.Assert.*;

import java.util.MissingFormatArgumentException;

import org.junit.Test;

/**
 * Examining way VarArgs works in Java. No arguments is allowed, in general, although there can be
 * more specific exceptions as shown by tests below.
 * 
 * @author Natural Semantics, LLC
 *
 */
public class TestJavaVarArgs {

	@Test
	public void test1() {
		//formatString("name is %s", "sonoo")
		String fmt = "name is %s";
		try {
			System.out.println(String.format(fmt));
			fail();
		}
		catch (MissingFormatArgumentException e) {
			System.out.println("Exception " + e.getClass().getName() + ": " + e.getMessage());
		}
	}

	@Test
	public void test2() {
		//formatString("name is %s", "sonoo")
		String fmt = "name is";
		try {
			System.out.println(String.format(fmt));
		}
		catch (Exception e) {
			System.out.println(e.getMessage());
			fail();
		}
	}
}
