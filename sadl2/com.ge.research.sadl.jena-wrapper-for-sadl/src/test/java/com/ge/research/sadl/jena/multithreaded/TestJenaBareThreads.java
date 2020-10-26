package com.ge.research.sadl.jena.multithreaded;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestJenaBareThreads {
    private static Logger logger = LoggerFactory.getLogger("TestJenaBareThreads");

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testThreads() {
		long timestamp = System.currentTimeMillis();
		String tsStr = "" + timestamp;
		String dataNS = "http://com.ge.research.sadl/allstest" + timestamp + "#";
		for (int i = 0; i < 500; i++) {
			JenaBareThread myThread = new JenaBareThread();
			myThread.start();
			System.out.println("Thread " + (i + 1) + " started.");
		}
	}

	public static void main (String[] args) {
		 try {
			logger.debug("ready to create instance of client");
			logger.debug("client instance created");
			new TestJenaBareThreads().testThreads();
			System.out.println("Test returned");
		} catch (Throwable t) {
			// TODO Auto-generated catch block
			t.printStackTrace();
			System.out.println("Test threw exception: " + t.getMessage());
		} 
	 }
}
