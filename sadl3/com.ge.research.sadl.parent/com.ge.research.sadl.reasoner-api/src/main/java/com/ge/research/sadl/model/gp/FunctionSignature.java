package com.ge.research.sadl.model.gp;

import java.util.List;

public class FunctionSignature {
	
	private String name;
	private String uri;
	private List<String> parameters;
	
	public String getName(){
		return this.name;
	}
	
	public String getUri(){
		return this.uri;
	}
	
	public List<String> getParameters(){
		return this.parameters;
	}
	
	public FunctionSignature(String name, String uri, List<String> parameters){
		this.name = name;
		this.uri = uri;
		this.parameters = parameters;
	}
}
