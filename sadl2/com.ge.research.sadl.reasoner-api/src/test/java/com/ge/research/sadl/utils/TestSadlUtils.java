package com.ge.research.sadl.utils;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestSadlUtils {
    protected static final Logger logger = LoggerFactory.getLogger(TestSadlUtils.class);

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testSripQuotes() {
		SadlUtils su = new SadlUtils();
		assertTrue(su.stripQuotes("\"test\"").equals("test"));
		logger.info("it passed");
	}

}
