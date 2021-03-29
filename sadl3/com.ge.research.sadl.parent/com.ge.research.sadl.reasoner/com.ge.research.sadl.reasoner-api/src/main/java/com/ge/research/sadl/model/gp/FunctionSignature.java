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
		if (isReservedKeyword(reservedWords, nm)) {
			sb.append("^");
		}
		sb.append(nm);
		sb.append("(");		
		for(int i = 0; i < this.getParameterTypes().length; i++){
			if(!this.getParameterTypes()[i].isEmpty()){
				sb.append(this.getParameterTypes()[i].trim());
				if (!this.getParameterTypes()[i].trim().equals("--")) {
					if (!this.getParameterTypes()[i].trim().equals("...")) {
						sb.append(" X");
					}
				}
				if(i != this.getParameterTypes().length - 1){
					sb.append(", ");
				}
			}
		}
		sb.append(")");
		if (hasReturns()) {
			sb.append(" returns ");
			for (int i = 0; i < this.getReturnTypes().length; i++) {
				if (!this.getReturnTypes()[i].trim().isEmpty()) {
					sb.append(this.getReturnTypes()[i].trim());
					if ( i != this.getReturnTypes().length - 1) {
						sb.append(", ");
					}
				}
			}
		}
		sb.append(":\n\""); 
		sb.append(this.getUri());
		sb.append("\".");
		
		return sb.toString();
	}

	private boolean isReservedKeyword(List<String> reservedWords, String nm) {
		return reservedWords != null && reservedWords.contains(nm);
	}

	/**
	 * Method to determine if the function has a return
	 * @return
	 */
	private boolean hasReturns() {
		String[] rettypes = this.getReturnTypes();
		if (rettypes == null || rettypes.length == 0) {
			return false;
		}
		for (String rt : rettypes) {
			if (rt != null && !rt.isEmpty()) {
				return true;
			}
		}
		return false;
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
