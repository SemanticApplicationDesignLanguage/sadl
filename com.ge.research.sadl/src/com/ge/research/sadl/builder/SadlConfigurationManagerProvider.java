package com.ge.research.sadl.builder;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Hashtable;

import org.eclipse.emf.common.util.URI;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.SadlJenaModelGetterPutter;
import com.ge.research.sadl.utils.SadlUtils;

public class SadlConfigurationManagerProvider {
    // there is one ConfigurationManager per project
    private Hashtable<URI, IConfigurationManagerForIDE> configurationMgrMap = new Hashtable<URI, IConfigurationManagerForIDE>();
    private SadlUtils sadlUtils = null;

    public IConfigurationManagerForIDE getConfigurationManager(URI projectUri) throws ConfigurationException, IOException {
    	URI prjUri = ResourceManager.normalizeProjectUri(projectUri);
    	if (configurationMgrMap == null) {
    		configurationMgrMap = new Hashtable<URI, IConfigurationManagerForIDE>();
    	}
    	else if (configurationMgrMap.containsKey(prjUri)) {
			return configurationMgrMap.get(prjUri);
		}
    	String modelFolderPath = prjUri.appendSegment(ResourceManager.OWLDIR).toString();
//    	modelFolderPath = getSadlUtils().fileUrlToFileName(modelFolderPath);
    	modelFolderPath = ResourceManager.normalizeProjectUri(URI.createURI(modelFolderPath)).toString();
    	IConfigurationManagerForIDE cm = new ConfigurationManagerForIDE(modelFolderPath, ConfigurationManagerForIDE.getOWLFormat());
		cm.setProjectFolderPath(prjUri.toString(), modelFolderPath);   
    	String format = ConfigurationManagerForIDE.getOWLFormat();	
    	SadlJenaModelGetterPutter modelGetter = new SadlJenaModelGetterPutter(cm, cm.getTdbFolder(), format);
    	cm.setModelGetter(modelGetter);
		cm.getModelGetter().setTdbFolder(cm.getTdbFolder());
    	configurationMgrMap.put(prjUri, cm);
    	return cm;
    }
    
	public IConfigurationManagerForIDE getConfigurationManager(String owlModelsFolder) throws ConfigurationException, IOException {
    	File omf = new File(owlModelsFolder);
    	if (omf.exists()) {
    		return getConfigurationManager(URI.createURI(omf.getParent()));
    	}
    	return null;
    }
    
//    private URI normalizeProjectUri(URI projectUri) throws IOException {
//		String pfstr = projectUri.toString();
//		if (pfstr == null) {
//			return projectUri;
//		}
//		pfstr = getSadlUtils().fileUrlToFileName(pfstr);
//		File prjFile = new File(pfstr);
//		pfstr = prjFile.getCanonicalPath();
//		pfstr = pfstr.replace('\\', '/');
//		return URI.createURI(pfstr);
//	}

//	public SadlUtils getSadlUtils() {
//		if (sadlUtils == null) {
//			sadlUtils = new SadlUtils();
//		}
//		return sadlUtils;
//	}

}
