package com.ge.research.sadl.utils;

import static org.junit.Assert.*;

import java.net.MalformedURLException;
import java.net.URISyntaxException;

import org.junit.Test;

public class TestFileUrlToFileName {

	@Test
	public void test() throws URISyntaxException, MalformedURLException {
		String fn = "/C:\\Users\\200005201\\git\\sadl-os\\com.ge.research.sadl.reasoner-api\\src\\test\\java\\com\\ge\\research\\sadl\\utils\\TestFileUrlToFileName.java";
		System.out.println(fn);
		String url = SadlUtils.fileNameToFileUrl(fn);
		System.out.println(url);
		fn = SadlUtils.fileUrlToFileName(url);
		System.out.println(fn);
	}

}
