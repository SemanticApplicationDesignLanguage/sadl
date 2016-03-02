/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
