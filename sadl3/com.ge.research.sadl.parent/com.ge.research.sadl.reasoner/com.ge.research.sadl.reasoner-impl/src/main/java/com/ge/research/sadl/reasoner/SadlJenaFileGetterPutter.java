package com.ge.research.sadl.reasoner;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.riot.RDFDataMgr;

import com.ge.research.sadl.model.SadlSerializationFormat;

public class SadlJenaFileGetterPutter extends SadlJenaFileGetter implements ISadlModelGetterPutter{
    
	public SadlJenaFileGetterPutter(IConfigurationManager mgr, String format) {
		super(mgr, format);
	}

	@Override
	public boolean saveModel(Model m, String modelNamespace, String publicUri, String owlFileName, String format) throws TranslationException {
		try ( OutputStream out = new FileOutputStream(owlFileName) ) {
		     RDFDataMgr.write(out, m, SadlSerializationFormat.getRDFFormat(format));
		     out.close();
		} catch (FileNotFoundException e) {
			System.err.println(e.getMessage());
		} catch (IOException e) {
			System.err.println(e.getMessage());
		}
		return false;
	}
	
}
