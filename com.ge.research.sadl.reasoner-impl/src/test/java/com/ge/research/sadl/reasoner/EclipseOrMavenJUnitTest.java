package com.ge.research.sadl.reasoner;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

import junit.framework.TestCase;

import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.junit.Ignore;

@Ignore
public class EclipseOrMavenJUnitTest extends TestCase {
	
	protected String getKbRoot() throws Exception {
		super.setUp();
		String kbroot = ClassLoader.getSystemResource("Models").getFile();
		List<Logger> loggers = Collections.<Logger>list(LogManager.getCurrentLoggers());
		loggers.add(LogManager.getRootLogger());
		for ( Logger logger : loggers ) {
		    logger.setLevel(Level.OFF);
		}
		System.out.println("kbroot: " + kbroot);
		File check = new File(kbroot);
		if (!check.exists()) {
			throw new IOException("kbroot '" + kbroot + "' does not exist. Something is wrong.");
		}
		return kbroot;
	}

}
