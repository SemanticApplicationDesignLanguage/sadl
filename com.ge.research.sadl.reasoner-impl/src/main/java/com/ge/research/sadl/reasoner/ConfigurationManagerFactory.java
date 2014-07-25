package com.ge.research.sadl.reasoner;

public class ConfigurationManagerFactory {
	
	public static IConfigurationManager getConfigurationManager(String modelFolderPath, String repoType) throws ConfigurationException {
		return new ConfigurationManager(modelFolderPath, repoType);
	}
	
}
