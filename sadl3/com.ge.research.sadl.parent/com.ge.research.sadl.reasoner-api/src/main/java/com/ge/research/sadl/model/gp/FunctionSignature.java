package com.ge.research.sadl.model.gp;

public class FunctionSignature {
	
	private String name;
	private String[] parameterTypes;
	private String returnType;
	private String uri;
	
	public FunctionSignature(String fullEquationDefinition, String uri){
		String[] equationSplit = fullEquationDefinition.split("\\(|\\)", -1);
		this.name = equationSplit[0];
		this.parameterTypes = equationSplit[1].split(",", -1);
		this.returnType = equationSplit[2];
		this.uri = uri;		
	}
	
	public String FunctionSignatureToSadlModelFormat(){
		StringBuilder sb = new StringBuilder();
		sb.append("External ");
		sb.append(this.name);
		sb.append("(");		
		for(int i = 0; i < this.parameterTypes.length; i++){
			if(!this.parameterTypes[i].isEmpty()){
				sb.append(this.parameterTypes[i].toUpperCase());
				sb.append(" X");
				if(i != this.parameterTypes.length - 1){
					sb.append(", ");
				}
			}
		}
		sb.append(") returns ");
		sb.append(this.returnType.toUpperCase());
		sb.append(":\n\""); 
		sb.append(this.uri);
		sb.append("\".");
		
		return sb.toString();
	}
}
