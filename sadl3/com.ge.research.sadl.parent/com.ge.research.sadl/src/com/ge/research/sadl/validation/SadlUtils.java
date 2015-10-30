package com.ge.research.sadl.validation;

import java.net.URL;

public class SadlUtils {
	
	public static String validateUri(String uri) throws java.net.MalformedURLException {
	    final URL url;
	    try {
	        url = new URL(uri);
	    }
	    catch (Exception e1) {
	    	return ("'" + uri + "' is not a valid URL: " + e1.getLocalizedMessage());
	    }
	    if (!"http".equals(url.getProtocol())) {
	    	return ("'" + uri + "' is not a valid URL: Model name must use http protocol.");
	    }
	    return null;
	}

}
