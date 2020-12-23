package com.ge.research.sadl.swi_prolog.plinterface;

import java.net.ConnectException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class SWIPrologServiceInterfaceThreaded implements ISWIPrologServiceInterface {
	
	private String rules = "";
	private int usageCounter = 0;
	private long sleepTime = 1L;
	private long timeoutTime = 500L;
	private PlServiceInterfaceRunnable plhttp = null;
	private Thread thread = null;
	private Iterator<String> remainingItr = null;
	
	public SWIPrologServiceInterfaceThreaded(){
		rules = "";
	}
	
	@Override
	public boolean addPlRules(String inRules){
		rules += inRules + "\n";
		return true;
	}
	
	@Override
	public boolean clearPlRules(){
		rules = "";
		return true;
	}
	
	
	@Override
	public String getPlRules(){
		return rules;
	}
	
	@Override
	public boolean runPlQueryNoArgs(String url, String query, boolean defineQueryPred) throws Exception{
		String urlParameters = "query=";
		urlParameters += "targetVar(['" + "_DummyVar" + "'])." + "\n";
		if (defineQueryPred)
			urlParameters += rules + "qresult([" + true + "]) :- " + query + "." + "\n";
		else
			urlParameters += rules + query + "\n";
		
//		debugOutput(url, "NoArgs");
//		PlServiceInterfaceRunnable plhttp = new PlServiceInterfaceRunnable();
//		plhttp.sendPrologQuery(url, urlParameters);
		String html = startAndMonitorPlService(url, urlParameters);
		if (html.startsWith("java.net.ConnectException")) {
			throw new ConnectException(html);
		}
		List<String> tList = new ArrayList<String>();
		tList.add("_DummyVar");
		
		List<Hashtable> result = htmlToHashtable(tList,html);
		if (result.isEmpty())
			return false;
		
		return true;
	}
	
	
	@Override
	public List<Hashtable> runPlQuery(String url, String query, String target, boolean defineQueryPred) throws Exception{
		String urlParameters = "query=";
		urlParameters += "targetVar(['" + target + "'])." + "\n";
		if (defineQueryPred)
			urlParameters += rules + "qresult([" + target + "]) :- " + query + "." + "\n";
		else
			urlParameters += rules + query + "\n";
		
//		debugOutput(url, "runPlQuery");
//		PlServiceInterfaceRunnable plhttp = new PlServiceInterfaceRunnable();
//		plhttp.sendPrologQuery(url, urlParameters);
		String html = startAndMonitorPlService(url, urlParameters);
		
		List<String> tList = new ArrayList<String>();
		tList.add(target);
		
		return htmlToHashtable(tList,html); 
	}
	
//	private void debugOutput(String url, String srsc) {
//		System.out.println("Usage #(" + srsc + "): " + usageCounter++);
//		Runtime runtime = Runtime.getRuntime();
//		NumberFormat format = NumberFormat.getInstance();
//
//		StringBuilder sb = new StringBuilder();
//		long maxMemory = runtime.maxMemory();
//		long allocatedMemory = runtime.totalMemory();
//		long freeMemory = runtime.freeMemory();
//
//		sb.append("free memory: " + format.format(freeMemory / 1024) + "<br/>");
//		sb.append("allocated memory: " + format.format(allocatedMemory / 1024) + "<br/>");
//		sb.append("max memory: " + format.format(maxMemory / 1024) + "<br/>");
//		sb.append("total free memory: " + format.format((freeMemory + (maxMemory - allocatedMemory)) / 1024) + "<br/>");
//		System.out.println(sb.toString());
//		System.out.println("\n");
//	}

	@Override
	public List<Hashtable> runPlQueryMultipleArgs(String url, String query, List<String> tList, boolean defineQueryPred) throws Exception{
		String urlParameters = "query=";
		urlParameters += "targetVar([";
		int first = 0;
		for (String target : tList){
			if (first == 0)
				urlParameters += "'" + target + "'";
			else
				urlParameters += "," + "'" + target + "'";
			
			first += 1;
		}
		urlParameters += "])." + "\n";
		
		if (defineQueryPred){
			urlParameters += rules + "qresult([";
			first = 0;
			for (String target : tList){
				if (first == 0)
					urlParameters += target;
				else
					urlParameters += "," + target;
				
				first += 1;
			}
			
			urlParameters += "]) :- " + query + "." + "\n";
		}
		else{
			urlParameters += rules + query + "\n";
		}
		
//		debugOutput(url, "MultipleArgs");
//		PlServiceInterfaceRunnable plhttp = new PlServiceInterfaceRunnable();
//		plhttp.sendPrologQuery(url, urlParameters);
		String html = startAndMonitorPlService(url, urlParameters);
		
		return htmlToHashtable(tList,html); 
	}
	
	private PlServiceInterfaceRunnable getPlServiceInterface(String url) {
		if (this.plhttp  == null) {
			this.plhttp = new PlServiceInterfaceRunnable(url);
			setThread(plhttp);
		}
		return this.plhttp;
	}
	
	private boolean setThread(PlServiceInterfaceRunnable plhttp) {
		this.thread  = new Thread(plhttp);
		this.thread.start();
		return true;
	}
	
	private Thread getThread() {
		return this.thread;
	}
	
	private String startAndMonitorPlService(String url, String urlParameters) throws PlServiceFailedException, InterruptedException {
		PlServiceInterfaceRunnable pls = getPlServiceInterface(url);
		long startTime = System.currentTimeMillis();
		if (remainingItr != null) {
			// add any remaining requests from a prior PlServiceInterfaceRunnable to queue
			while (remainingItr.hasNext()) {
				pls.queue.add(remainingItr.next());
			}
			remainingItr = null;
		}
//		TimeUnit.SECONDS.sleep(1);
		
		while (getThread().isAlive() && plhttp.getResponseString() == null) {
			long nowTime = System.currentTimeMillis();
			if (nowTime - startTime > timeoutTime) {
				plhttp.interrupt();
				
				// get any unprocessed requests out of the queue for re-requests
				setRemainingItr(pls.queue.iterator());
				
				throw new PlServiceFailedException(nowTime - startTime);
			}
			pls.queue.add(urlParameters);
//			t.sleep(sleepTime);
			TimeUnit.SECONDS.sleep(1);
		}
		return plhttp.getResponseString();
	}
	
	private List<Hashtable> htmlToHashtable(List<String> tList, String html){
//		System.out.println(html);
		List<Hashtable> result = new ArrayList<Hashtable>();
		int index = html.indexOf("</th>");
		int curIndex = 0;
		do{
			curIndex = html.indexOf("<tr>",index);
			if (curIndex >= 0){
				Hashtable tempHashtable = new Hashtable();
				for (String var : tList){
					int sIndex = html.indexOf("<td>",curIndex);
					int eIndex = html.indexOf("</td>",curIndex);
					String val = html.substring(sIndex+4, eIndex);
					//System.out.println("Adding " + var);
					tempHashtable.put(var, val);
					curIndex = eIndex + 5;
				}
				result.add(tempHashtable);
				index = curIndex;
			}
		}while(curIndex >= 0);
		
		return result;
	}

	private Iterator<String> getRemainingItr() {
		return remainingItr;
	}

	private void setRemainingItr(Iterator<String> remainingItr) {
		this.remainingItr = remainingItr;
	}
}