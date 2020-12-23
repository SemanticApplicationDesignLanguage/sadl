package com.ge.research.sadl.swi_prolog.plinterface;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.concurrent.atomic.AtomicBoolean;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
 
public class PlServiceInterfaceRunnable implements Runnable {
    protected static final Logger logger = LoggerFactory.getLogger(PlServiceInterfaceRunnable.class);
 
	private final String USER_AGENT = "Mozilla/5.0";

	private String url = null;

	private String urlParameters = null;
	
	private String responseString = null;
	
	private String exceptionString = null;
	
	private final AtomicBoolean running = new AtomicBoolean(false);
 
	public static void main(String[] args) throws Exception {
 
		PlServiceInterfaceRunnable http = new PlServiceInterfaceRunnable();
 
		logger.debug("\nTesting 2 - Send Http POST request");
		http.sendPost();
 
	}
 
	
 
	// HTTP POST request
	private void sendPost() throws Exception {
 
		String url = "http://localhost:5000/result";
		URL obj = new URL(url);
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();
 
		//add request header
		con.setRequestMethod("POST");
		con.setRequestProperty("User-Agent", USER_AGENT);
		con.setRequestProperty("Accept-Language", "en-US,en;q=0.5");
 
		String urlParameters = "query=";
		urlParameters += "targetVar(['X','Y'])." + "\n";
		urlParameters += "qresult([X,Y]) :- p(X), q(Y)." + "\n";
		urlParameters += "p(a). q(b).";
 
		// Send post request
		con.setDoOutput(true);
		DataOutputStream wr = new DataOutputStream(con.getOutputStream());
		wr.writeBytes(urlParameters);
		wr.flush();
		wr.close();
 
		int responseCode = con.getResponseCode();
		logger.debug("\nSending 'POST' request to URL : " + url);
		logger.debug("Post parameters : " + urlParameters);
		logger.debug("Response Code : " + responseCode);
 
		BufferedReader in = new BufferedReader(
		        new InputStreamReader(con.getInputStream()));
		String inputLine;
		StringBuffer response = new StringBuffer();
 
		while ((inputLine = in.readLine()) != null) {
			response.append(inputLine);
		}
		in.close();
 
		//print result
		logger.debug(response.toString());
 
	}
	
	// Prepare for HTTP POST request
	public void sendPrologQuery(String url, String urlParameters) throws Exception {
		//String url = "http://localhost:5000/result";
		this.setUrl(url);
		this.setUrlParameters(urlParameters);
	}

	public void interrupt() {
		running.set(false);
	}

	@Override
	public void run() {
		// Do HTTP POST request
		running.set(true);
		URL obj;
		try {
			obj = new URL(url);
			HttpURLConnection con = (HttpURLConnection) obj.openConnection();
			 
			//add request header
			con.setRequestMethod("POST");
			con.setRequestProperty("User-Agent", USER_AGENT);
			con.setRequestProperty("Accept-Language", "en-US,en;q=0.5");
	 
//			String urlParameters = "query=";
//			urlParameters += "targetVar(['X','Y'])." + "\n";
//			urlParameters += "qresult([X,Y]) :- p(X), q(Y)." + "\n";
//			urlParameters += "p(a). q(b).";
	 
			// Send post request
			con.setDoOutput(true);
			DataOutputStream wr = new DataOutputStream(con.getOutputStream());
			wr.writeBytes(urlParameters);
			wr.flush();
			wr.close();
	 
			int timeout = con.getConnectTimeout();
			logger.debug("Time out: " + timeout);
			int responseCode = con.getResponseCode(); 
			logger.debug("\nSending 'POST' request to URL : " + url);
			logger.debug("Post parameters : " + urlParameters);
			logger.debug("Response Code : " + responseCode);
	 
			BufferedReader in = new BufferedReader(
			        new InputStreamReader(con.getInputStream()));
			String inputLine;
			StringBuffer response = new StringBuffer();
	 
			while ((inputLine = in.readLine()) != null) {
				response.append(inputLine);
			}
			in.close();
	 
			//return result
			responseString = response.toString();
		} catch (Exception e) {
			StringBuilder sb = new StringBuilder();
			sb.append(e.getClass().getName());
			sb.append(": ");
			sb.append(e.getMessage());
			setResponseString(sb.toString());
		}
	}



	public String getUrl() {
		return url;
	}



	public void setUrl(String url) {
		this.url = url;
	}



	public String getUrlParameters() {
		return urlParameters;
	}



	public void setUrlParameters(String urlParameters) {
		this.urlParameters = urlParameters;
	}



	public String getResponseString() {
		return responseString;
	}



	public void setResponseString(String responseString) {
		this.responseString = responseString;
	}



	public String getExceptionString() {
		return exceptionString;
	}



	public void setExceptionString(String exceptionString) {
		this.exceptionString = exceptionString;
	}
 
}