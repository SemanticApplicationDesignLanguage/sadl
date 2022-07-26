package com.ge.research.sadl.reasoner.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class TestSadlUtils {

	@Test
	public void test_01() {
		assertEquals("3.14", SadlUtils.formatNumber(Double.toString(Math.PI), 3));
	}

	@Test
	public void test_02() {
		assertEquals("0.0272", SadlUtils.formatNumber(Double.toString(Math.E/100.0), 3));
	}

	@Test
	public void test_03() {
		assertEquals("3.14E-06", SadlUtils.formatNumber(Double.toString(Math.PI/1000000.), 3));
	}

	@Test
	public void test_04() {
		assertEquals("3.14E+05", SadlUtils.formatNumber(Double.toString(Math.PI*100000.), 3));
	}

	@Test
	public void test_05() {
		assertEquals("314", SadlUtils.formatNumber(Double.toString(Math.PI*100.), 3));
	}
	
	@Test
	public void test_06() {
		String n1 = Double.toString(Math.PI);
		String n2 = Double.toString(Math.E/100.0);
		String n3 = Double.toString(Math.PI/1000000.);
		String n4 = Double.toString(Math.PI*100000.);
		String n5 = Double.toString(Math.PI*100.);
		String numList = "[" + n1 + ", " + n2 + "," + n3 + " ," + n4 + " , " + n5 + "]";
		assertEquals("[3.14, 0.0272, 3.14E-06, 3.14E+05, 314]", SadlUtils.formatNumberList(numList, 3));
	}
	
	@Test
	public void test_07() {
		String testExe = "pwd";
		assertTrue(SadlUtils.canExecute(testExe));
	}
	
	@Test
	public void test_08() {
		String testExe = "paddywag";
		assertFalse(SadlUtils.canExecute(testExe));
	}

}
