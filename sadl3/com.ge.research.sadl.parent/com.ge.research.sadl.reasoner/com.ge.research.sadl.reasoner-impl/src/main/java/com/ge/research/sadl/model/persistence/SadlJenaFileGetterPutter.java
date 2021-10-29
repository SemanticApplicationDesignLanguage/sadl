package com.ge.research.sadl.model.persistence;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URISyntaxException;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.riot.RDFDataMgr;

import com.ge.research.sadl.model.persistence.ISadlModelGetterPutter;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;

public class SadlJenaFileGetterPutter extends SadlJenaFileGetter implements ISadlModelGetterPutter {
    
	public SadlJenaFileGetterPutter(IConfigurationManager mgr, String format) {
		super(mgr, format);
	}

	@Override
	public boolean saveModel(Model m, String modelNamespace, String publicUri, String owlFileName, String format) throws TranslationException {
		try ( OutputStream out = new FileOutputStream(owlFileName) ) {
		     RDFDataMgr.write(out, m, SadlPersistenceFormat.getRDFFormat(format));
		     out.close();
		} catch (FileNotFoundException e) {
			System.err.println(e.getMessage());
		} catch (IOException e) {
			System.err.println(e.getMessage());
		}
		return false;
	}

	@Override
	public boolean removeModel(String uri) throws ConfigurationException, IOException, URISyntaxException {
		if (getConfigMgr() instanceof IConfigurationManagerForEditing) {
			String altUrl = getConfigMgr().getAltUrlFromPublicUri(uri);
			if (altUrl != null) {
				if (altUrl.startsWith("http:")) {
					throw new IOException(
							"Can't delete a model with only an 'http://' URL");
				}
				File auf = new File((new SadlUtils()).fileUrlToFileName(uri));
				if (auf.exists()) {
					boolean stat = auf.delete();
					if (stat) {
						((IConfigurationManagerForEditing)getConfigMgr()).deleteMapping(altUrl, uri);
						return true;
					}
				}
			}
		} else {
			throw new IOException("Can't remove models with the current ConfigurationManager class: " + getConfigMgr().getClass().getCanonicalName());
		}
		return false;
	}
	
	@Override
	public int clean() throws ConfigurationException {
		// TODO Auto-generated method stub
		return 0;
	}

}
