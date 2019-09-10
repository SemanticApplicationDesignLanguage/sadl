package com.ge.research.sadl.model.gp;

import java.util.List;

public class FunctionSignature {
	
	private String name;
	private String[] parameterTypes;
	private String[] returnTypes;
	private String uri;
	
	public FunctionSignature(String fullEquationDefinition, String uri){
		String[] equationSplit = fullEquationDefinition.split("\\(|\\)", -1);
		String nm = equationSplit[0];
		this.setName(nm);
		this.setParameterTypes(equationSplit[1].split(",", -1));
		this.setReturnTypes(equationSplit[2].split(",", -1));
		this.setUri(uri);	
	}
	
	public String FunctionSignatureToSadlModelFormat(List<String> reservedWords){
		StringBuilder sb = new StringBuilder();
		sb.append("External ");
		String nm = this.getName();
		if (reservedWords != null && reservedWords.contains(nm)) {
			sb.append("^");
		}
		sb.append(nm);
		sb.append("(");		
		for(int i = 0; i < this.getParameterTypes().length; i++){
			if(!this.getParameterTypes()[i].isEmpty()){
				sb.append(this.getParameterTypes()[i]);
				if (!this.getParameterTypes()[i].equals("--")) {
					if (!this.getParameterTypes()[i].equals("...")) {
						sb.append(" X");
					}
					if(i != this.getParameterTypes().length - 1){
						sb.append(", ");
					}
				}
			}
		}
		sb.append(") returns ");
		for (int i = 0; i < this.getReturnTypes().length; i++) {
			if (!this.getReturnTypes()[i].isEmpty()) {
				sb.append(this.getReturnTypes()[i]);
				if ( i != this.getReturnTypes().length - 1) {
					sb.append(", ");
				}
			}
		}
		sb.append(":\n\""); 
		sb.append(this.getUri());
		sb.append("\".");
		
		return sb.toString();
	}

	public String getName() {
		return name;
	}

	private void setName(String name) {
		this.name = name;
	}

	public String getUri() {
		return uri;
	}

	private void setUri(String uri) {
		this.uri = uri;
	}

	public String[] getReturnTypes() {
		return returnTypes;
	}

	private void setReturnTypes(String[] returnTypes) {
		this.returnTypes = returnTypes;
	}

	public String[] getParameterTypes() {
		return parameterTypes;
	}

	private void setParameterTypes(String[] parameterTypes) {
		this.parameterTypes = parameterTypes;
	}
}
