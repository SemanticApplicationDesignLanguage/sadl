/************************************************************************
 * Copyright © 2021 - Natural Semantics, LLC. All Rights Reserved.
 *
 * Project: SADL
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/
package com.naturalsemanticsllc.sadl.reasoner;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.jena.atlas.web.HttpException;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntDocumentManager.ReadFailureHandler;
import org.apache.jena.rdf.model.InfModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ModelGetter;
import org.apache.jena.reasoner.rulesys.GenericRuleReasoner;
import org.apache.jena.reasoner.rulesys.Rule;
import org.apache.jena.reasoner.rulesys.Rule.ParserException;
import org.apache.jena.shared.RulesetNotFoundException;

import com.ge.research.sadl.jena.reasoner.JenaReasonerPlugin;
import com.ge.research.sadl.jena.reasoner.SadlReadFailureHandler;
import com.ge.research.sadl.model.ImportMapping;
import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.ReasonerTiming;
import com.ge.research.sadl.reasoner.RuleNotFoundException;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.utils.SadlUtils;


/*
 * This class differs from its parent in that rules are loaded in stages and when certain operations 
 * occur, namely changing rules or changing the data, rules must also be unloaded to restart the staged
 * sequence of rule application to create the inferred model.
 */
public class JenaAugmentedReasonerPlugin extends JenaReasonerPlugin {
    private static final String DEFAULT_TRANSLATOR_CLASSNAME = "com.naturalsemanticsllc.sadl.translator.JenaAugmentedTranslatorPlugin";

	protected Map<Integer, List<String>> ruleFilesLoadedMap = null;
	protected Map<Integer, List<Rule>> ruleListMap = null;
	private int lastRuleStageLoaded;

	@Override
	public String getConfigurationCategory() {
		return "Augmented-" + super.getConfigurationCategory();
//		return super.getConfigurationCategory();
	}
	
	@Override
	public String getDefaultTranslatorClassName() {
		return DEFAULT_TRANSLATOR_CLASSNAME;
	}

	@Override
	public GenericRuleReasoner getReasonerOnlyWhenNeeded() throws ConfigurationException {
		if (reasoner != null) {
			return reasoner;
		}
				
		try {
			if (!configurationMgr.getModelGetter().modelExists(getModelName(), tbox)) {
				if (tbox.equals(getModelName())) {
					throw new ConfigurationException("The model '" + getModelName() + "' does not have a mapping and was not found.");
				}
				else {
					throw new ConfigurationException("The model with actual URL '" + tbox + "' and name '" + getModelName() + "' does not appear to exist.");
				}
			}
		} catch (MalformedURLException e) {
			throw new ConfigurationException("The actual file URL '" + tbox + "' for model '" + getModelName() + "' is not well-formed.");
		}
		if (explanationsEnabled) {
			derivationLogging = true;
		}
		else {
			String derval = getStringConfigurationValue(preferences , pDerivationLogging, DERIVATION_NONE);
			derivationLogging = (derval != null && !derval.equals(DERIVATION_NONE));
		}
		modelSpec = getModelSpec(preferences);	// get this for later use when creating InfModel

		logger.debug("JenaReasonerPlugin.initializeReasoner, tbox = "+tbox);
		try {
			if (!tbox.startsWith("file:") && !tbox.startsWith("http:")) {
				//assume local file
				SadlUtils su = new SadlUtils();
				tbox = su.fileNameToFileUrl(tbox);
				logger.debug("JenaReasonerPlugin.initializeReasoner, modified tbox = "+tbox);
			}

			String format = repoType;
			if (!validateFormat(format)) {
				throw new ConfigurationException("Format '" + format + "' is not supported by reasoner '" + getConfigurationCategory() + "'.");
			}
			if (format.equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
				schemaModel = configurationMgr.getModelGetter().getOntModel(getModelName(), tbox, format);	
				schemaModel.getDocumentManager().setProcessImports(true);
				schemaModel.loadImports();
			}
			else {
				if (tbox.endsWith(".TDB/")) {
					// this is a cached inferred TDB model
					schemaModel = configurationMgr.getModelGetter().getOntModel(getModelName(), tbox, format);
					schemaModelIsCachedInferredModel = true;
					return null;
				}
				else {
					schemaModel = ModelFactory.createOntologyModel(configurationMgr.getOntModelSpec(null));
					ReadFailureHandler rfHandler = new SadlReadFailureHandler(logger);
					schemaModel.getDocumentManager().setProcessImports(true);
					schemaModel.getDocumentManager().setReadFailureHandler(rfHandler );
					schemaModel.getSpecification().setImportModelGetter((ModelGetter) configurationMgr.getModelGetter());
					schemaModel.read(tbox, SadlSerializationFormat.getRDFFormat(format).toString());
				}
			}
		} catch (Exception e1) {
			e1.printStackTrace();
		}	
		
//		if (logger.isDebugEnabled()) {
//			logger.debug("schemaModel '" + getModelName() + "' with  tbox '" + tbox + "' loaded");
//			dumpModelToLogger(schemaModel);
//		}
		loadImports();

		logger.debug("JenaReasonerPlugin.initializeReasoner, imports size = " + (imports == null ? 0 : imports.size()));

		long t2 = System.currentTimeMillis();
		loadRules(schemaModel, getModelName());
		logger.debug("JenaReasonerPluging.initialize, number of rule stages is "+ruleListMap.size());
		
		// load only first stage rules at this point
		lastRuleStageLoaded = 0;
		reasoner = new GenericRuleReasoner(ruleListMap.get(lastRuleStageLoaded));
		reasoner.setDerivationLogging(derivationLogging);
		logger.debug("JenaReasonerPluging.initialize, size of ruleList from reasoner = "+reasoner.getRules().size());
		reasoner.setMode(getRuleMode(preferences));
		long t3 = System.currentTimeMillis();
		if (collectTimingInfo) {
			timingInfo.add(new ReasonerTiming(TIMING_LOAD_MODEL, "load ontology model", t2 - tboxLoadTime));
			int numRules = ruleList.size();
			timingInfo.add(new ReasonerTiming(TIMING_LOAD_RULES, "load model " + numRules + " rules", t3 - t2));
		}

		long t4;
		if (collectTimingInfo) {
			t4 = System.currentTimeMillis();
			timingInfo.add(new ReasonerTiming(TIMING_LOAD_RULES, "bind schema to reasoner", t4 - t3));			
		}
		boolean transitiveClosure = getBooleanConfigurationValue(preferences, pTransitiveClosureCaching, false);
		reasoner.setTransitiveClosureCaching(transitiveClosure);
		reasoner.setOWLTranslation(getBooleanConfigurationValue(preferences, pOWLTranslation, false));
		boolean bTrace = getBooleanConfigurationValue(preferences, pTrace, false);
		reasoner.setTraceOn(bTrace);
		if (bTrace) {
//			traceAppender = new FileAppender();
			// configure the appender here, with file location, etc
			File tboxfile = null;
			try {
				SadlUtils su = new SadlUtils();
				tboxfile = new File(su.fileUrlToFileName(tbox));
			} catch (MalformedURLException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		}
		else {
//			traceAppender = null;
		}
		if (getBooleanConfigurationValue(preferences, pUseLuceneIndexer, false)) {
			luceneIndexerClass = getStringConfigurationValue(preferences, pLuceneIndexerClass, "com.ge.research.sadl.jena.reasoner.LuceneModelIndexerImpl");
		}
		
		String strTimeOut = getStringConfigurationValue(preferences, pTimeOut, "-1");
		try {
			queryTimeout = Long.parseLong(strTimeOut.trim());
		}
		catch (NumberFormatException e) {
			String msg = "Invalid timeout value '" + strTimeOut + "'";
			logger.error(msg); addError(new ModelError(msg, ErrorType.ERROR));

		}
		return reasoner;
	}
		
	@Override
	protected void prepareInfModel() throws ConfigurationException {
		getReasonerOnlyWhenNeeded();
		if (infModel == null || newInputFlag == true) {
			if (schemaModelIsCachedInferredModel) {
				infModel = schemaModel;
			}
			else {
				long t1 = System.currentTimeMillis();
				generateTboxModelWithSpec();
				logger.debug("In prepareInfModel, modelSpec: "+modelSpec.toString());
				logger.debug("In prepareInfModel, reasoner rule count: "+getReasonerOnlyWhenNeeded().getRules().size());
//				dumpModelToLogger(tboxModelWithSpec);
				infModel = ModelFactory.createInfModel(reasoner, tboxModelWithSpec);
//		        InfGraph graph = reasoner.bind(tboxModelWithSpec.getGraph());
//		        infModel = new InfModelImpl(graph);

				synchronized(ReasonerFamily) {
					infModel.size();	// this forces instantiation of the inference model
					if (collectTimingInfo) {
						long t2 = System.currentTimeMillis();
						timingInfo.add(new ReasonerTiming(TIMING_PREPARE_INFMODEL, "prepare inference model stage 0", t2 - t1));
					}
				}
				if (ruleListMap.size() > 1) {
					long tsl = System.currentTimeMillis();
					for (Integer stage = 1; stage < ruleListMap.size(); stage++) {
						reasoner.addRules(ruleListMap.get(stage));
						infModel.size();
						if (collectTimingInfo) {
							long t2 = System.currentTimeMillis();
							timingInfo.add(new ReasonerTiming(TIMING_PREPARE_INFMODEL, "prepare inference model stage " + stage, t2 - tsl));
						}
					}
				}
			}
		}
		else if(newInputFlag == true) {
			logger.debug("In prepareInfModel, reusing infModel with newInputFlag is true");
			if (infModel instanceof InfModel) {
				synchronized(ReasonerFamily) {
					logger.debug("In prepareInfModel, reusing infModel, rebinding existing infModel");
					((InfModel) infModel).rebind();
					infModel.size();	// force re-instantiation?
				}
			}
		} else {
			logger.debug("In prepareInfModel, reusing infModel without any changes, newInputFlag is false");
		}
		newInputFlag = false;			
	}

	@Override
	protected void loadRules(OntModel m, String modelName) {
		if (ruleFilesLoadedMap == null) {
			ruleFilesLoadedMap = new HashMap<Integer, List<String>>();
		}
		else {
			ruleFilesLoadedMap.clear();
		}
		if (ruleListMap == null) {
			ruleListMap = new HashMap<Integer, List<Rule>>();
		}
		else {
			ruleListMap.clear();
		}
		try {	
			String altUrl = configurationMgr.getAltUrlFromPublicUri(modelName);

			if (altUrl == null) {
				throw new ConfigurationException("Model URI '" + modelName + "' not found in mappings!");
			}

			if (altUrl != null) {
				logger.debug("modelName: " + modelName);
				logger.debug("altUrl: " + altUrl);
				String altFN = new SadlUtils().fileUrlToFileName(altUrl);
				logger.debug("altFN: " + altFN);
				String ruleBaseFn = altFN.substring(0, altFN.lastIndexOf(".")) + ".rules";
				logger.debug("ruleBaseFn: " + ruleBaseFn);
				boolean foundMoreRules = true;;
				for (Integer stage = 0; foundMoreRules; stage++) {
					String rulefn = ruleBaseFn;
					if (stage > 0) {
						rulefn = ruleBaseFn + "-stage" + stage;
					}

					if (ruleFilesLoadedMap.get(stage) == null || !ruleFilesLoadedMap.get(stage).contains(ruleBaseFn)) {
						if (loadRules(stage, rulefn)) {
							if (ruleFilesLoadedMap.get(stage) == null) {
								ruleFilesLoadedMap.put(stage, new ArrayList<String>());
							}
							ruleFilesLoadedMap.get(stage).add(rulefn);
							foundMoreRules = true;
						}
						else {
							foundMoreRules = false;
						}
					}
				}
				
				if (imports != null) {
					for (int i = 0; i < imports.size(); i++) {
						ImportMapping impMap = imports.get(i);
						String impUri = impMap.getPublicURI();
						altUrl = impMap.getActualURL();
						if (altUrl == null) {
							altUrl = configurationMgr.getAltUrlFromPublicUri(impUri);
						}
						if (altUrl != null) {
							ruleBaseFn = altUrl.contains(".") ? altUrl.substring(0, altUrl.lastIndexOf(".")) + ".rules" :
								altUrl + ".rules";
							boolean foundMoreImportedRules = true;
							for (Integer stage = 0; foundMoreImportedRules; stage++) {					
								String rulefn = ruleBaseFn;
								if (stage > 0) {
									rulefn = ruleBaseFn + "-stage" + stage;
								}

								if (ruleFilesLoadedMap.get(stage) == null || !ruleFilesLoadedMap.get(stage).contains(rulefn)) {
									if (loadRules(stage, rulefn)) {
										if (ruleFilesLoadedMap.get(stage) == null) {
											ruleFilesLoadedMap.put(stage, new ArrayList<String>());
										}
										ruleFilesLoadedMap.get(stage).add(rulefn);
										foundMoreImportedRules = true;
									}
									else {
										foundMoreImportedRules = false;
									}
								}
							}
						}
					}
				}
			}
		} catch (Throwable e) {
			// TODO Auto-generated catch block
//			e.printStackTrace();
			addError(new ModelError(e.getMessage(), ErrorType.ERROR));
		}
	}
	
	private boolean loadRules(Integer stage, String ruleFileName) throws IOException {
		if (ruleFileName != null) {
			try {
		    	File f = new File((new SadlUtils()).fileUrlToFileName(ruleFileName));
		    	if (f.exists()) {
		    		logger.debug(ruleFileName + " exists");
					InputStream in = configurationMgr.getJenaDocumentMgr().getFileManager().open(ruleFileName);
					if (in != null) {
					    try {
					    	InputStreamReader isr = new InputStreamReader(in);
					    	BufferedReader br = new BufferedReader(isr);
							List<Rule> rules = Rule.parseRules(Rule.rulesParserFromReader(br));
							if (rules != null) {
								ruleListMap.put(stage, rules);
								newInputFlag = true;
								return true;
							}
					    } catch (ParserException e) {
					    	String msg = "Error reading rule file '" + ruleFileName + "': " + e.getMessage();
					    	logger.error(msg);
					    	addError(new ModelError(msg, ErrorType.ERROR));
					    }
					    finally {
					    	in.close();
					    }
					}
		    	}
		    	else {
		    		logger.debug(ruleFileName + " does not exit");
		    	}
			}
			catch (RulesetNotFoundException e) {
				// ok if not found
				return false;
			}
			catch (HttpException e) {
				// ok if not found
				return false;
			}
		}		
//		dataModelSourceCount++;
		return false;
	}

	@Override
	public boolean addRule(String rule) {
		return super.addRule(rule);
	}
	
	@Override
	public boolean deleteRule(String ruleName) throws RuleNotFoundException {
		return super.deleteRule(ruleName);
	}
		
	@Override
	public boolean addRules(List<String> rules) {
		return super.addRules(rules);
	}
	
	@Override
	public boolean addTriple(String sub, String pred, String obj)
			throws TripleNotFoundException, ConfigurationException {
		return super.addTriple(sub, pred, obj);
	}
	
	@Override
	public boolean deleteTriple(String sub, String pred, String obj)
			throws TripleNotFoundException, ConfigurationException {
		return super.deleteTriple(sub, pred, obj);
	}
	
	@Override
	public boolean loadInstanceData(InputStream is, String format) throws IOException, ConfigurationException {		
		return super.loadInstanceData(is, format);
	}
	
	@Override
	public boolean loadInstanceData(Object model) throws ConfigurationException {
		return super.loadInstanceData(model);
	}
	
	@Override
	public boolean loadInstanceData(OntModel model) throws ConfigurationException {
		return super.loadInstanceData(model);
	}
	
	@Override
	public boolean loadInstanceData(String instanceDatafile) throws IOException, ConfigurationException {		
		return super.loadInstanceData(instanceDatafile);
	}
	
	@Override
	public boolean loadInstanceData(URI instanceDatafile) throws IOException, ConfigurationException {
		return super.loadInstanceData(instanceDatafile);
	}

}
