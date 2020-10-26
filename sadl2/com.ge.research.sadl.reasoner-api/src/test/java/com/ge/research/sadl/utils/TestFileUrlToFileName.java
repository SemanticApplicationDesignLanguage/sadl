package com.ge.research.sadl.utils;

import static org.junit.Assert.*;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.regex.Matcher;

import org.junit.Ignore;
import org.junit.Test;

public class TestFileUrlToFileName {

	@Test
	@Ignore
	public void test() throws URISyntaxException, MalformedURLException {
		String dir = System.getProperty("testSourceDir");
		String fname = System.getProperty("fileName");
		String fn = dir + File.separator + "com" + File.separator + "ge" + File.separator + "research"
				 + File.separator + "sadl" + File.separator + "utils" + File.separator + fname;
		System.out.println(fn);
		SadlUtils su = new SadlUtils();
		String url = su.fileNameToFileUrl(fn);
		System.out.println(url);
		String fn2 = su.fileUrlToFileName(url);
		System.out.println(fn2);
		assertTrue(fn2.substring(1).equals(fn.replace('\\', '/')));
	}

}
