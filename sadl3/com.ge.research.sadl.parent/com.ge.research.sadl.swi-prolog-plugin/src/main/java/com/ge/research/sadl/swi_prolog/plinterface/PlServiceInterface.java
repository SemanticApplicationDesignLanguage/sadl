package com.ge.research.sadl.swi_prolog.plinterface;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
 
public class PlServiceInterface {
    protected static final Logger logger = LoggerFactory.getLogger(PlServiceInterface.class);
 
	private final String USER_AGENT = "Mozilla/5.0";
 
	public static void main(String[] args) throws Exception {
 
		PlServiceInterface http = new PlServiceInterface();
 
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
	
	// HTTP POST request
		public String sendPrologQuery(String url, String urlParameters) throws Exception {
	 
			//String url = "http://localhost:5000/result";
			URL obj = new URL(url);
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
			return response.toString();
	 
		}
 
}