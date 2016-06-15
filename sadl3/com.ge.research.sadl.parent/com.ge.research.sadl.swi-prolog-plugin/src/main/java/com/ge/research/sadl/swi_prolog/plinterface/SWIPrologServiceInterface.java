package com.ge.research.sadl.swi_prolog.plinterface;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

public class SWIPrologServiceInterface {
	
	private String rules = "";
	
	public SWIPrologServiceInterface(){
		rules = "";
	}
	
	public boolean addPlRules(String inRules){
		rules += inRules + "\n";
		return true;
	}
	
	public boolean clearPlRules(){
		rules = "";
		return true;
	}
	
	
	public String getPlRules(){
		return rules;
	}
	
	public boolean runPlQueryNoArgs(String url, String query, boolean defineQueryPred) throws Exception{
		String urlParameters = "query=";
		urlParameters += "targetVar(['" + "_DummyVar" + "'])." + "\n";
		if (defineQueryPred)
			urlParameters += rules + "qresult([" + true + "]) :- " + query + "." + "\n";
		else
			urlParameters += rules + query + "\n";
		
		PlServiceInterface plhttp = new PlServiceInterface();
		String html = plhttp.sendPrologQuery(url, urlParameters);
		
		List<String> tList = new ArrayList<String>();
		tList.add("_DummyVar");
		
		List<Hashtable> result = htmlToHashtable(tList,html);
		if (result.isEmpty())
			return false;
		
		return true;
	}
	
	
	public List<Hashtable> runPlQuery(String url, String query, String target, boolean defineQueryPred) throws Exception{
		String urlParameters = "query=";
		urlParameters += "targetVar(['" + target + "'])." + "\n";
		if (defineQueryPred)
			urlParameters += rules + "qresult([" + target + "]) :- " + query + "." + "\n";
		else
			urlParameters += rules + query + "\n";
		
		PlServiceInterface plhttp = new PlServiceInterface();
		String html = plhttp.sendPrologQuery(url, urlParameters);
		
		List<String> tList = new ArrayList<String>();
		tList.add(target);
		
		return htmlToHashtable(tList,html); 
	}
	
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
		
		PlServiceInterface plhttp = new PlServiceInterface();
		String html = plhttp.sendPrologQuery(url, urlParameters);
		
		return htmlToHashtable(tList,html); 
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
}