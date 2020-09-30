package com.ge.research.sadl.builder;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Platform;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.reasoner.ConfigurationException;

public class ConfigurationManagerForIdeFactory {
	protected static final Logger logger = LoggerFactory.getLogger(ConfigurationManagerForIdeFactory.class);
	
	private static Map<String,ConfigurationManagerForIDE> configManagers = null;
	
	public ConfigurationManagerForIdeFactory() {
		
	}

	/**
	 * Discards the configuration manager state for a project if the platform is
	 * not running. This is just a workaround to handle the asynchronous build
	 * event related issue before running the reasoner.
	 * 
	 * @param modelFolder
	 *            the folder path to discard the configuration state.
	 * @return {@code true} if the state was discarded, otherwise {@code false}.
	 */
	public static boolean discardConfigurationManagerState(String modelFolder) {
		if (configManagers != null && !Platform.isRunning()) {
			return configManagers.remove(modelFolder) != null;
		}
		return false;
	}
	
	public static ConfigurationManagerForIDE getConfigurationManagerForIDE(String modelFolder, String format) throws ConfigurationException {
		if (configManagers == null) {
			configManagers = new HashMap<String, ConfigurationManagerForIDE>();
		}
		// this next section makes sure that leading "/" on Windows is removed so that modelFolder is always the saem
		File mff = new File(modelFolder);
		if (mff.exists() && mff.isDirectory()) {
			modelFolder = mff.getAbsolutePath();
		}
		modelFolder = modelFolder != null ? formatPathRemoveBackslashes(modelFolder) : null;
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
	
	public static String formatPathRemoveBackslashes(String path) {
		if(path == null){
			return null;
		}
		return path.replace("\\", "/");
	}

}
