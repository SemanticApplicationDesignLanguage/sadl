package com.ge.research.sadl.builder;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.reasoner.ConfigurationException;

public class ConfigurationManagerForIdeFactory {
	protected static final Logger logger = LoggerFactory.getLogger(ConfigurationManagerForIdeFactory.class);
	
	private static Map<String,ConfigurationManagerForIDE> configManagers = null;
	
	public ConfigurationManagerForIdeFactory() {
		
	}
	
	public static ConfigurationManagerForIDE getConfigurationManagerForIDE(String modelFolder, String format) throws ConfigurationException {
		if (configManagers == null) {
			configManagers = new HashMap<String, ConfigurationManagerForIDE>();
		}
		if (!configManagers.containsKey(modelFolder)) {
			ConfigurationManagerForIDE newCM = new ConfigurationManagerForIDE(modelFolder, format);
			configManagers.put(modelFolder, newCM);
			logger.debug("ConfigurationManagerForIdeFactory returning newly created config mgr for '" + modelFolder + "'");
			return newCM;
		}
		else {
			logger.debug("ConfigurationManagerForIdeFactory returning existing config mgr for '" + modelFolder + "'");
			return configManagers.get(modelFolder);
		}
	}
	
	
	public static ConfigurationManagerForIDE getConfigurationManagerForIDE(String modelFolder, String format, boolean noModelFolderNeeded) throws ConfigurationException {
		if (configManagers == null) {
			configManagers = new HashMap<String, ConfigurationManagerForIDE>();
		}
		if (!configManagers.containsKey(modelFolder)) {
			ConfigurationManagerForIDE newCM = new ConfigurationManagerForIDE(modelFolder, format, noModelFolderNeeded);
			configManagers.put(modelFolder, newCM);
			logger.debug("ConfigurationManagerForIdeFactory returning newly created config mgr for '" + modelFolder + "'");
			return newCM;
		}
		else {
			logger.debug("ConfigurationManagerForIdeFactory returning existing config mgr for '" + modelFolder + "'");
			return configManagers.get(modelFolder);
		}
	}

}
