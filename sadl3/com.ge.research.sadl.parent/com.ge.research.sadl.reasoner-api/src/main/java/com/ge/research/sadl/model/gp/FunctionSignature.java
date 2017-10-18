package com.ge.research.sadl.model.gp;

public class FunctionSignature {
	
	private String name;
	private String[] parameterTypes;
	private String returnType;
	private String uri;
	
	public FunctionSignature(String fullEquationDefinition, String uri){
		String[] equationSplit = fullEquationDefinition.split("\\(|\\)", -1);
		this.setName(equationSplit[0]);
		this.setParameterTypes(equationSplit[1].split(",", -1));
		this.setReturnType(equationSplit[2]);
		this.setUri(uri);		
	}
	
	public String FunctionSignatureToSadlModelFormat(){
		StringBuilder sb = new StringBuilder();
		sb.append("External ");
		sb.append(this.getName());
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
		sb.append(this.getReturnType());
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

	public String getReturnType() {
		return returnType;
	}

	private void setReturnType(String returnType) {
		this.returnType = returnType;
	}

	public String[] getParameterTypes() {
		return parameterTypes;
	}

	private void setParameterTypes(String[] parameterTypes) {
		this.parameterTypes = parameterTypes;
	}
}
