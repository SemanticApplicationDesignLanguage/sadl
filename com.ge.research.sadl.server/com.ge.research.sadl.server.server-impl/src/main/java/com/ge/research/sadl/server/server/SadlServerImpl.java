/************************************************************************
 * Copyright (c) 2007-2014 - General Electric Company, All Rights Reserved
 *
 * Project: SADL Knowledge Server
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language. 
 * 
 * The SADL Knowledge Server is a set of Java classes implementing 
 * a service interface for deploying ontology-based knowledge bases
 * for use in a client-server environment.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/09 14:06:51 $
 ***********************************************************************/

package com.ge.research.sadl.server.server;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ge.research.sadl.server.ISadlServer;
import com.ge.research.sadl.server.NamedServiceNotFoundException;
import com.ge.research.sadl.server.SessionNotFoundException;
import com.ge.research.sadl.utils.SadlUtils;
import com.ge.research.sadl.utils.StringDataSource;
import com.ge.research.sadl.utils.UtilsForJena;

import javax.activation.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationManagerFactory;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.InvalidDerivationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ReasonerTiming;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.SadlJenaModelGetter;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.Syntax;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.vocabulary.RDF;

/**
 * This class provides the implementation of the ISadlService interface
 * contract.
 *
 * $Author: crapo $
 * $Revision: 1.16 $ Last modified on   $Date: 2014/11/03 20:09:59 $
 *
 */
public class SadlServerImpl implements ISadlServer {
    protected final Logger logger = LoggerFactory.getLogger(getClass());
    protected IConfigurationManager configurationMgr = null;
    protected IReasoner reasoner = null;
    private boolean collectTimingInformation = false;
    protected String modelName = null;
	private String serviceVersion = "$Revision: 1.16 $";
	protected String kbaseRoot = null;
	protected String defaultInstanceDataNS = null;
	protected Map<String, String[]> serviceNameMap = null;

    public SadlServerImpl() {
    }
    
    public SadlServerImpl(String _kbaseRoot) throws ConfigurationException {
    	setKbaseRoot(_kbaseRoot);
    }
    
	public String getClassName() throws SessionNotFoundException {
		return this.getClass().getName();
	}
    
	public String getServiceVersion() throws SessionNotFoundException {
		return this.getClass().getSimpleName() + " version " + serviceVersion;
	}

    public ResultSet query(String sparql)
            throws QueryParseException, ReasonerNotFoundException, QueryCancelledException {
        logger.info("Calling query(\"{}\")", sparql);
        if (reasoner != null) {
 			ResultSet rs = reasoner.ask(sparql);
 			if (rs == null) {
 				logger.info("query returned no values");
 			}
 			return rs;
        }
        else {
        	throw new ReasonerNotFoundException("No reasoner found.");
        }
    }
    
    public ReasonerTiming[] getTimingInformation() {
    	List<com.ge.research.sadl.reasoner.ReasonerTiming> timings = reasoner.getTimingInformation();
    	if (timings != null && timings.size() > 0) {
    		ReasonerTiming[] serviceTimings = new ReasonerTiming[timings.size()];
    		for (int i = 0; i < timings.size(); i++) {
    			ReasonerTiming timing = timings.get(i);
    			ReasonerTiming serverTiming = 
    				new ReasonerTiming(timing.getTimingCategory(),
    					timing.getDescription(), timing.getMilliseconds());
    			serviceTimings[i] = serverTiming;
    		}
    		return serviceTimings;
    	}
    	return null;
    }

    
    public DataSource construct(String sparql) throws QueryParseException, QueryCancelledException {
    	logger.info("Calling construct(\"{}\")", sparql);
		return reasoner.construct(sparql);
    }

	public ResultSet ask(String subjName, String propName,
            Object objValue) throws TripleNotFoundException, ReasonerNotFoundException {
        logger.info("Calling ask(\"{}\", \"{}\", {})",
                new Object[] {subjName, propName, objValue});
        if (reasoner != null) {
        	ResultSet rs = reasoner.ask(subjName, propName, (objValue != null ? objValue.toString() : null));
        	if (rs == null) {
        		logger.info("ask returned no values");
        		return null;
        	}
        	return rs;
        }
		throw new ReasonerNotFoundException("No reasoner found.");
    }

	public boolean loadData(String serverDataLocator)
            throws IOException, ReasonerNotFoundException, ConfigurationException {
        logger.info("Calling loadData(\"{}\")", serverDataLocator);
        if (reasoner != null) {
        	try {
        		SadlUtils su = new SadlUtils();
				return reasoner.loadInstanceData(su.fileNameToFileUrl(serverDataLocator));
			} catch (URISyntaxException e) {
				throw new IOException("Invalid URI", e);
			}
        }
		throw new ReasonerNotFoundException("No reasoner found.");
    }

    public String selectServiceModel(String serviceName) throws ConfigurationException, ReasonerNotFoundException {
        logger.info("Calling selectServiceModel with serviceName (\"{}\")", serviceName);
        if (kbaseRoot == null) {
        	throw new ConfigurationException("Kbase root is null; has it been set?");
        }
    	String kbid = getKbIdentifierByServiceName(serviceName);
    	String modnm = getModelNameByServiceName(serviceName);
    	return selectServiceModel(kbid, modnm);
    }
    
	public String selectServiceModel(String serviceName,
			List<ConfigurationItem> preferences) throws ConfigurationException,
			ReasonerNotFoundException, NamedServiceNotFoundException {
    	String kbid = getKbIdentifierByServiceName(serviceName);
    	String modnm = getModelNameByServiceName(serviceName);
    	if (kbid == null || modnm == null) {
    		throw new NamedServiceNotFoundException("Service '" + serviceName + "' not found.");
    	}
    	return selectServiceModel(kbid, modnm, preferences);
	}
	
    public String selectServiceModel(String knowledgeBaseIdentifier, String modelName) throws ConfigurationException, ReasonerNotFoundException {
    	return selectServiceModel(knowledgeBaseIdentifier, modelName, null);
    }

	public String selectServiceModel(String knowledgeBaseIdentifier,
			String modelName, List<ConfigurationItem> preferences) throws ConfigurationException, ReasonerNotFoundException {
        logger.info("Calling selectServiceModel(\"{}\", \"{}\")", knowledgeBaseIdentifier, modelName);
    	String kbid = getKbIdentifierByServiceName(knowledgeBaseIdentifier);
    	if (kbid != null) {
    		// this was a named service
    		String modnm = getModelNameByServiceName(knowledgeBaseIdentifier);
    		if (modnm != null) {
    			throw new ConfigurationException("Service name '" + knowledgeBaseIdentifier + "' already has a model name (" + modnm + ") specified.");
    		}
    		else {
    			knowledgeBaseIdentifier = kbid;
    		}
    	}
    	String repoType;
		try {
			repoType = getRepoType(knowledgeBaseIdentifier + "/TDB");
		} catch (MalformedURLException e1) {
			throw new ConfigurationException("Error setting repository type: " + e1.getMessage());
		}
        setConfigurationMgr(ConfigurationManagerFactory.getConfigurationManager(knowledgeBaseIdentifier, repoType));
        if (this.getConfigurationMgr() == null) {
        	logger.info("Failed to get configuration manager");
        } else {
        	logger.info("Got configuration manager");
        }
        setConfigurationManagerModelGetter();

       reasoner = getConfigurationMgr().getReasoner();
        if (this.reasoner == null) {
        	logger.info("Failed to get reasoner");
        } else {
        	logger.info("Got reasoner");
        }
 		reasoner.collectTimingInformation(collectTimingInformation);
		List<com.ge.research.sadl.reasoner.ConfigurationItem> transPrefs = null;
		if (preferences != null && preferences.size() > 0) {
			transPrefs = new ArrayList<com.ge.research.sadl.reasoner.ConfigurationItem>();
    		for (int i = 0; preferences != null && i < preferences.size(); i++) {
    			ConfigurationItem ci = preferences.get(i);
    			String[] ch = ci.getCategoryHierarchy();
    			ch[0] = reasoner.getConfigurationCategory();
    			ConfigurationItem transCi = new ConfigurationItem(ch);
    			transCi.setNameValuePairs(ci.getNameValuePairs());
    			transPrefs.add(transCi);
    		}
		}
        int iStatus = reasoner.initializeReasoner(knowledgeBaseIdentifier, modelName, transPrefs, repoType);
        this.modelName = modelName;
        if (this.reasoner == null) {
        	logger.info("Failed to get reasoner");
        } else if (iStatus == 0){
        	logger.info("Got reasoner but initialization returned failed status");
        } else {
        	logger.info("Got good reasoner");
        }
        
        return "SadlServer" + System.currentTimeMillis();
	}

	protected void setConfigurationManagerModelGetter()
			throws ConfigurationException {
		if (getConfigurationMgr().getModelGetter() == null) {
        	try {
				getConfigurationMgr().setModelGetter(new SadlJenaModelGetter(getConfigurationMgr().getModelFolder() + "/TDB"));
			} catch (IOException e) {
				logger.error("Exception setting ModelGetter: " + e.getMessage());
				e.printStackTrace();
			}
        }
	}
	
	protected String getRepoType(String tdbFolder) throws MalformedURLException {
		SadlUtils su = new SadlUtils();
		String fname = su.fileUrlToFileName(tdbFolder);
    	File tdbFile = new File(fname);
    	if (tdbFile.exists()) {
    		logger.debug("Repo type set to TDB as the folder '" + fname + "' exists.");
    		return IConfigurationManager.JENA_TDB;	
    	}
    	else {
    		logger.debug("Repo type set to RDF/XML ABBREV as folder '" + tdbFolder + "' does not exist.");
    		return IConfigurationManager.RDF_XML_ABBREV_FORMAT;
    	}
	}
	
    public boolean sendData(DataSource dataSrc)
            throws IOException, ReasonerNotFoundException, ConfigurationException {
        logger.info("Calling sendData({})", dataSrc);
        if (reasoner != null) {
        	String inputFormat = "N-TRIPLE";
    		String srcName = dataSrc.getName();
			if (srcName != null) {
				if (srcName.endsWith("n-triple") || srcName.endsWith("nt")) {
					inputFormat = "N-TRIPLE";
				}
				else if (srcName.endsWith("n3")) {
					inputFormat = "N3";
				}
				else if (srcName.endsWith("owl")) {
					inputFormat = "RDF/XML";
				}
			}
        	return reasoner.loadInstanceData(dataSrc.getInputStream(), inputFormat);
        }
		throw new ReasonerNotFoundException("No reasoner found.");
    }

    public boolean sendData(DataSource dataSrc, String inputFormat)
            throws IOException, ReasonerNotFoundException, ConfigurationException {
        logger.info("Calling sendData({},{})", dataSrc,inputFormat);
        if (reasoner != null) {
        	return reasoner.loadInstanceData(dataSrc.getInputStream(), inputFormat);
        }
		throw new ReasonerNotFoundException("No reasoner found.");
    }

	public boolean addTriple(String subjName, String predName,
			Object objValue) throws ConfigurationException, TripleNotFoundException, ReasonerNotFoundException {
		if (reasoner != null) {
			String strval;
			if (objValue instanceof String) {
				strval = (String)objValue;
			}
			else {
				strval = reasoner.objectValueToStringValue(objValue, predName);
			}
			return reasoner.addTriple(subjName, predName, strval);
		}
		throw new ReasonerNotFoundException("No reasoner found.");
	}

	public boolean deleteTriple(String subjName, String predName,
			Object objValue) throws ConfigurationException, TripleNotFoundException, ReasonerNotFoundException {
		if (reasoner != null) {
			String strval = null;
			if (objValue != null) {
				if (objValue instanceof String) {
					strval = (String)objValue;
				}
				else {
					strval = reasoner.objectValueToStringValue(objValue, predName);
				}
			}
			return reasoner.deleteTriple(subjName, predName, strval);
		}
		throw new ReasonerNotFoundException("No reasoner found.");
	}

	public boolean reset() throws ReasonerNotFoundException {
		if (reasoner != null) {
			return reasoner.reset();
		}
		throw new ReasonerNotFoundException("No reasoner found.");
	}

	public String prepareQuery(String query) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException {
		if (reasoner != null) {
			return reasoner.prepareQuery(query);
		}
		throw new ReasonerNotFoundException("No reasoner found.");
	}

    private String getModelNameByServiceName(String serviceName) {
		if (this.serviceNameMap != null && this.serviceNameMap.containsKey(serviceName)) {
			return this.serviceNameMap.get(serviceName)[1];
		}
		return null;
	}

	private String getKbIdentifierByServiceName(String serviceName) {
		if (this.serviceNameMap != null && this.serviceNameMap.containsKey(serviceName)) {
			return this.serviceNameMap.get(serviceName)[0];
		}
		return null;
	}


	public void setOwlFileOutputFormat(String outputFormat) {
		if (reasoner != null) {
			reasoner.setOutputFormat(outputFormat);
		}
	}

	public String getKBaseIdentifier() throws ConfigurationException {
		if (getConfigurationMgr() != null) {
			try {
				return getConfigurationMgr().getModelFolder();
			} catch (IOException e) {
				throw new ConfigurationException("Model folder not found", e);
			}
		}
		return null;
	}

	public String getModelName() throws IOException {
		return modelName;
	}

	public String getReasonerVersion() throws ConfigurationException {
		IReasoner irsnr = getReasoner();
		if (irsnr != null) {
			return irsnr.getClass().getCanonicalName() + ": " + irsnr.getReasonerVersion();
		}
		return null;
	}

	protected IReasoner getReasoner() throws ConfigurationException {
		if (getConfigurationMgr() != null) {
			return getConfigurationMgr().getReasoner();
		}
		return null;
	}

	public DataSource getDerivations() throws ConfigurationException, InvalidDerivationException {
		if (this.reasoner != null) {
			DataSource ds = null;
			ds = this.reasoner.getDerivations();
			if (ds == null) {
				ds = new StringDataSource("", "text/plain");
				((StringDataSource) ds).setName("Derivations");
			}
			return ds;
		}
		logger.info("Reasoner is null");
		throw new ConfigurationException("Reasoner is null");
	}

	public void setServiceNameMap(Map<String, String[]> serviceNameMap) {
		this.serviceNameMap = serviceNameMap;
	}

	public Map<String, String[]> getServiceNameMap() {
		return serviceNameMap;
	}

	public void collectTimingInformation(boolean bCollect) {
		collectTimingInformation = bCollect;
	}

	public void setKbaseRoot(String _kbaseRoot) throws ConfigurationException {
		String kbrootpath;
    	if (_kbaseRoot != null) {
    		if (!_kbaseRoot.startsWith("file:")) {
    			kbrootpath = _kbaseRoot;
    			java.io.File kbrfile = new java.io.File(kbrootpath);
    			_kbaseRoot = kbrfile.toURI().toString(); 
    		}
    		else {
    			kbrootpath = _kbaseRoot.substring(5);
    		}
    		this.kbaseRoot = _kbaseRoot;
    		java.io.File kbrootFile = new java.io.File(kbrootpath);
    		if (kbrootFile.exists() && kbrootFile.isDirectory()) {
    			Map[] results = scanFolderForServices(kbrootFile, null, null);
    			if (results != null) {
    				if (results.length > 0 && results[0] instanceof Map<?,?>) {
    					Map<String, String[]> serviceMap = (Map<String, String[]>) results[0];
		    			if (serviceMap != null) {
		    				setServiceNameMap(serviceMap);
		    			}
    				}
    			}
    		}
    	}
	}

	public String getKbaseRoot() {
		return kbaseRoot;
	}

	private Map[] scanFolderForServices(java.io.File folder, Map<String, String[]> map, Map<String, java.io.File> sources) throws ConfigurationException {
		java.io.File[] files = folder.listFiles();
		for (int i = 0; i < files.length; i++) {
			java.io.File file = files[i];
			if (file.isFile() && file.getName().equals(IConfigurationManager.ServicesConf_FN)) {
				if (sources == null) {
					sources = new HashMap<String, java.io.File>();
				}
				List<Object[]> contentList;
				try {
					contentList = getServicesFromConfigFile(file);
					if (contentList != null) {
						for (int j = 0; j < contentList.size(); j++) {
							Object[] content = contentList.get(j);
							if (content.length == 2) {
								if (content[0] instanceof String && content[1] instanceof String[]) {
									if (map == null) {
										map = new HashMap<String, String[]>();
									}
									if (map.containsKey((String)content[0])) {
										// this is a duplicate named service
										String[] existing = map.get((String)content[0]);
										String msg = "Found duplicate named service: '" + content[0] +  "':\n    '" + 
										existing[0] + "', '" + existing[1] + 
											"' (from '" + file.toString() + "')\n  and\n    '" + 
										((String[])content[1])[0] + "','" + ((String[])content[1])[1] + 
											"' (from '" + sources.get((String)content[0]) + "')";
										logger.error(msg);
										throw new ConfigurationException(msg);
									}
									map.put((String)content[0], (String[])content[1]);
									sources.put((String)content[0], file);
								}
							}
						}
					}
				} catch (FileNotFoundException e) {
					throw new ConfigurationException("File '" + file.toString() + "' not found", e);
				} catch (IOException e) {
					throw new ConfigurationException("Error scanning folder for services: " + e.getMessage());
				}
			}
			else if (file.isDirectory()) {
				Map[] results = scanFolderForServices(file, map, sources);
				map = (Map<String, String[]>) results[0];
				sources = (Map<String, File>) results[1];
			}
		}
		Map[] results = new Map[2];
		results[0] = map;
		results[1] = sources;
		return results;
	}

	private List<Object[]> getServicesFromConfigFile(java.io.File file) throws ConfigurationException, IOException {
		List<Object[]> contents = null;
//		logger.info("Classloader of this is "+this.getClass().getClassLoader().toString());
//		ClassLoader mcl = Model.class.getClassLoader();
//		logger.info("Classloader of Model is "+mcl.toString());
		Model m = ModelFactory.createMemModelMaker().createFreshModel();
//		logger.info("Model created");
		FileInputStream is = new FileInputStream(file);
		m = m.read(is, null);
		QueryExecution qexec = null;		
		com.hp.hpl.jena.query.ResultSet results = null;		
		String query = "select ?url ?sn ?mn where {?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://com.ge.research.sadl/sadlserver/Services#KnowledgeBase> . " +
				"?x <http://com.ge.research.sadl/sadlserver/Services#entryPoint> ?sn . " +
				"OPTIONAL{?sn <http://com.ge.research.sadl/sadlserver/Services#modelName> ?mn} . " +
				"OPTIONAL{?x <http://com.ge.research.sadl/sadlserver/Services#url> ?url} }";
		
		qexec = QueryExecutionFactory.create(QueryFactory.create(query, Syntax.syntaxARQ), m);
		results = qexec.execSelect();
		if (results != null) {
			Property tlprop = m.getProperty("http://com.ge.research.sadl/sadlserver/Services#csvTemplateLocator");
			while (results.hasNext()) {
				QuerySolution soln = results.nextSolution();
				String url = null;
				String sn = null;
				String mn = null;
				RDFNode urlnode = soln.get("?url");
				RDFNode snnode = soln.get("?sn");
				RDFNode mnnode = soln.get("?mn");
				if (urlnode != null) {
					if (urlnode instanceof Literal) {
						url = ((Literal)urlnode).getString();
					}
				}
				if (snnode instanceof Resource) {
					sn = ((Resource)snnode).getLocalName();
				}
				if (mnnode instanceof Literal) {
					mn = ((Literal)mnnode).getString();
				}
				if (sn != null) {
					if (url != null && url.length() > 0) {
						if (!url.startsWith("http:") && !url.startsWith("file:")) {
							// relative url
							String kbr = getKbaseRoot();
							url = kbr + (kbr.endsWith("/") || url.startsWith("//") ? "" : "/") + url;
						}
					}
					else {
						try {
							SadlUtils su = new SadlUtils();
							url = su.fileNameToFileUrl(file.getParentFile().getCanonicalPath());
						} catch (IOException e) {
							throw new ConfigurationException("Parent of file '" + file.getCanonicalPath() + "' not found", e);
						} catch (URISyntaxException e) {
							throw new ConfigurationException("Parent of file '" + file.getCanonicalPath() + "' is invalid URL", e);
						}
					}
					if (contents == null) {
						contents = new ArrayList<Object[]>();
					}
					String tl = null;
					if (snnode instanceof Resource) {
						// now check for template locators--note that this is a multi-valued property, which is why we do a separate query
						
						StmtIterator tlitr = ((Resource)snnode).listProperties(tlprop);
						while (tlitr.hasNext()) {
							RDFNode tlnode = tlitr.nextStatement().getObject();
							if (tlnode instanceof Literal) {
								String tls = ((Literal)tlnode).getString();
								if (!(tls.startsWith("http:") || tls.startsWith("file:"))) {
									tls = "file:///" + tls;
								}
								if (tl == null) {
									tl = tls;
								}
								else {
									tl = tl + "," + tls;
								}
							}
						}
					}
					String[] info = new String[tl != null ? 3 : 2];
					info[0] = url;
					info[1] = mn;
					if (tl != null) {
						info[2] = tl;
					}
				
					Object[] content = new Object[2];
					content[0] = sn;
					content[1] = info;
					contents.add(content);
				}
			}
		}
		
		return contents;
	}

	public ResultSet[] atomicQuery(String serviceName, DataSource dataSrc, String inputFormat,
			String[] sparql) throws IOException, ConfigurationException,
			NamedServiceNotFoundException, QueryParseException,
			ReasonerNotFoundException, SessionNotFoundException, InvalidNameException, QueryCancelledException {
		ResultSet[] results = null;
		if (serviceName == null || serviceName.length() == 0) {
			throw new NamedServiceNotFoundException("Service name is null");
		}
		if (sparql != null && sparql.length > 0) {
			if (selectServiceModel(serviceName) == null) {
				throw new NamedServiceNotFoundException("Service '" + serviceName + "' did not return a valid session.");
			}
			if (dataSrc != null) {
				if (inputFormat != null && inputFormat.length() > 0) {
					if (!sendData(dataSrc, inputFormat)) {
						throw new IOException("Failed to send data to service: (" + dataSrc.toString() + ")");
					}
				}
				else {
					if (!sendData(dataSrc)) {
						throw new IOException("Failed to send data to service: (" + dataSrc.toString() + ")");
					}
				}
			}
			results = new ResultSet[sparql.length];
			for (int i = 0; i < sparql.length; i++) {
				results[i] = query(prepareQuery(sparql[i]));
			}
		}
		return results;
	}

	public String setInstanceDataNamespace(String namespace) throws InvalidNameException {
		if (!namespace.endsWith("#")) {
			throw new InvalidNameException("A namespace should end with '#' ('" + namespace + "' does not)");
		}
		String oldNS = defaultInstanceDataNS;
		defaultInstanceDataNS = namespace;
		if (reasoner != null) {
			reasoner.setInstanceDataNamespace(namespace);
		}
		return oldNS;
	}
	
	public String getInstanceDataNamespace() {
		if (reasoner != null) {
			return reasoner.getInstanceDataNamespace();
		}
		return defaultInstanceDataNS;
	}
	
	public String createInstance(String name, String className) throws ConfigurationException, InvalidNameException, IOException {
		String error = UtilsForJena.validateRdfUri(name);
		if (error != null) {
			throw new InvalidNameException("Invalid instance name (" + name + "): " + error);
		}
		if (name == null) {
			throw new InvalidNameException("instance name cannot be null");
		}
		if (name.indexOf(':') < 0 && name.indexOf('#') < 0) {
			// this name is a fragment; assume it is in the default namespace
			name = defaultInstanceDataNS + name;
		}
		try {
			reasoner.addTriple(name, RDF.type.getURI(), className);
		} catch (com.ge.research.sadl.reasoner.TripleNotFoundException e) {
			throw new ConfigurationException(e.getMessage());
		}
		return name;
	}
	
	private String convertDataSourceToString(DataSource ds) throws IOException {
		InputStream is = ds.getInputStream();
		if (is != null) {
			Writer writer = new StringWriter();
			char[] buffer = new char[1024];
			try {
				Reader reader = new BufferedReader(new InputStreamReader(is, "UTF-8"));
				int n;
				while ((n = reader.read(buffer)) != -1) {
					writer.write(buffer, 0, n);
				}
			} finally {
				is.close();
			}
			return writer.toString();
		} else {       
			return "";
		}
	}
	
	public void setQueryTimeout(long timeout) throws ReasonerNotFoundException, SessionNotFoundException  {
		if (reasoner != null) {
			Class<? extends IReasoner> rClass = reasoner.getClass();
			Method setQueryTimeoutMethod = null;
			try {
				setQueryTimeoutMethod = rClass.getMethod("setQueryTimeout", long.class);
			} catch (NoSuchMethodException e) {
				logger.info("No setQueryTimeout method on reasoner") ;
			} catch (SecurityException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			try {
				logger.info("Setting query timeout to "+timeout+" ms.");
				setQueryTimeoutMethod.invoke(reasoner, timeout);
			} catch (IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else {
			logger.error("No reasoner found in call to setQueryTimeout");
        	throw new ReasonerNotFoundException("No reasoner found.");
		}
		
	}

	protected IConfigurationManager getConfigurationMgr() throws ConfigurationException {
		return configurationMgr;
	}

	protected void setConfigurationMgr(IConfigurationManager configurationMgr) {
		this.configurationMgr = configurationMgr;
	}

	protected String getModelNamespace(String modelName) {
		if (!modelName.endsWith("#")) {
			return modelName + "#";
		}
		return modelName;
	}

	@Override
	public boolean clearCache() throws InvalidNameException, SessionNotFoundException {
		if (reasoner != null) {
			return reasoner.clearCache();
		}
		return false;
	}

}
