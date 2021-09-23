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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.jena.atlas.web.HttpException;
import org.apache.jena.graph.Triple;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.InfModel;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFWriter;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.reasoner.Derivation;
import org.apache.jena.reasoner.rulesys.GenericRuleReasoner;
import org.apache.jena.reasoner.rulesys.Rule;
import org.apache.jena.reasoner.rulesys.Rule.ParserException;
import org.apache.jena.shared.RulesetNotFoundException;

import com.ge.research.sadl.jena.reasoner.JenaReasonerPlugin;
import com.ge.research.sadl.model.ImportMapping;
import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.InvalidDerivationException;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.ReasonerTiming;
import com.ge.research.sadl.reasoner.RuleNotFoundException;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.reasoner.utils.StringDataSource;

import jakarta.activation.DataSource;


/*
 * This class differs from its parent in that rules are loaded in stages and when certain operations 
 * occur, namely changing rules or changing the data, rules must also be unloaded to restart the staged
 * sequence of rule application to create the inferred model.
 */
public class JenaAugmentedReasonerPlugin extends JenaReasonerPlugin implements IReasoner {
    private static final String DEFAULT_TRANSLATOR_CLASSNAME = "com.naturalsemanticsllc.sadl.translator.JenaAugmentedTranslatorPlugin";

    public static final String version = "date 12 April 2021";

	protected Map<Integer, List<String>> ruleFilesLoadedMap = null;
	protected Map<Integer, List<Rule>> ruleListMap = null;
	private int lastRuleStageLoaded;
	
	private OntModel deductionsModel = null;
	private List<Derivation> derivations = null;

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
	public String getReasonerVersion() {
		return version;
	}


	@Override
	public GenericRuleReasoner getReasonerOnlyWhenNeeded() throws ConfigurationException {
		if (reasoner != null) {
			return reasoner;
		}
				
		try {
			if (!configurationMgr.getSadlModelGetter(null).modelExists(getModelName())) {
				if (tbox.equals(getModelName())) {
					throw new ConfigurationException("The model '" + getModelName() + "' does not have a mapping and was not found.");
				}
				else {
					throw new ConfigurationException("The model with actual URL '" + tbox + "' and name '" + getModelName() + "' does not appear to exist.");
				}
			}
		} catch (ConfigurationException e) {
			throw e;
		} catch (Exception e) {
			throw new ConfigurationException(e.getMessage(), e);
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
			schemaModel = configurationMgr.getSadlModelGetter(format).getOntModel(getModelName());	
			schemaModel.getDocumentManager().setProcessImports(true);
			schemaModel.loadImports();
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
		if (collectTimingInfo) {
			timingInfo.add(new ReasonerTiming(TIMING_LOAD_MODEL, "load ontology model", t2 - tboxLoadTime));
		}
 
		loadRules(schemaModel, getModelName());
		logger.debug("JenaReasonerPluging.initialize, number of rule stages is "+ruleListMap.size());
		long t3 = System.currentTimeMillis();
		if (collectTimingInfo) {
			int numRules = getNumRules();
			timingInfo.add(new ReasonerTiming(TIMING_LOAD_RULES, "read " + numRules + " rules from file(s)", t3 - t2));
		}
		
		// load only first stage rules at this point
		lastRuleStageLoaded = 0;
		reasoner = createReasonerAndLoadRules(ruleListMap.get(lastRuleStageLoaded), 0);
		return reasoner;
	}

	/**
	 * Method to count the number of rules loaded from rule files
	 * @return
	 */
	private int getNumRules() {
		int numRules = 0;
		if (ruleListMap != null) {
			Collection<List<Rule>> rlmvalues = ruleListMap.values();
			for (List<Rule> rlst : rlmvalues) {
				numRules += rlst.size();
			}
		}
		return numRules;
	}

	/**
	 * Method to create a reasoner and load the cumulative set of rules
	 * @param rules 
	 * @param stage
	 * @return
	 * @throws ConfigurationException
	 */
	private GenericRuleReasoner createReasonerAndLoadRules(List<Rule> rules, int stage) throws ConfigurationException {
		long tLoad1 = System.currentTimeMillis();
		GenericRuleReasoner newReasoner = new GenericRuleReasoner(rules);
		newReasoner.setDerivationLogging(derivationLogging);
		logger.debug("JenaReasonerPluging.initialize, size of ruleList from reasoner = "+newReasoner.getRules().size());
		newReasoner.setMode(getRuleMode(preferences));

		boolean transitiveClosure = getBooleanConfigurationValue(preferences, pTransitiveClosureCaching, false);
		newReasoner.setTransitiveClosureCaching(transitiveClosure);
		newReasoner.setOWLTranslation(getBooleanConfigurationValue(preferences, pOWLTranslation, false));
		boolean bTrace = getBooleanConfigurationValue(preferences, pTrace, false);
		newReasoner.setTraceOn(bTrace);
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
		long tLoad2 = System.currentTimeMillis();
		if (collectTimingInfo) {
			int numRules = newReasoner.getRules() != null ? newReasoner.getRules().size() : 0;
			timingInfo.add(new ReasonerTiming(TIMING_LOAD_RULES, "add " + numRules + " rule(s) to reasoner for stage " + stage, tLoad2 - tLoad1));
		}
		return newReasoner;
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
				infModel = ModelFactory.createInfModel(reasoner, tboxModelWithSpec);

				synchronized(ReasonerFamily) {
					infModel.size();	// this forces instantiation of the inference model
					if (collectTimingInfo) {
						long t2 = System.currentTimeMillis();
						timingInfo.add(new ReasonerTiming(TIMING_PREPARE_INFMODEL, "prepare inference model stage 0", t2 - t1));
					}
				}
				/*
				 * The call to getReasonerOnlyWhenNeeded causes all rules to be read from the rule files
				 * and creates the first reasoner so that it has the first rules loaded. An InfModel
				 * is then created using this reasoner and the model provided to the reasoner plugin.
				 * If there are multiple rule files, the loop below 
				 * 	1. gets the rules already loaded from the current reasoner
				 *  2. adds the next set of rules to that
				 *  3. creates a new reasoner with the new rule set
				 *  4. Creates a new InfModel using new reasoner and the previous infModel
				 *  5. sets infModel and reasoner to the new InfModel and Reasoner created in steps 3 & 4.
				 */
				if (ruleListMap.size() > 1) {
					boolean collectDerivations = !getDerivationLevel().equals(DERIVATION_NONE);
					long tsl = System.currentTimeMillis();
					for (Integer stage = 1; stage < ruleListMap.size(); stage++) {
						// Might need to have a flag and only do this if preference is set for getting deductions...
						// Likewise for derivations?
						if (collectDerivations) {
							if (deductionsModel == null) {
								deductionsModel = ModelFactory.createOntologyModel(configurationMgr.getOntModelSpec(null), ((InfModel) infModel).getDeductionsModel());				
								addDerivations(deductionsModel);
							}
							else {
								Model moreDM = ((InfModel) infModel).getDeductionsModel();
								addDerivations(moreDM);
								deductionsModel.add(moreDM);
							}
						}
						List<Rule> rules = reasoner.getRules();
						List<Rule> newRules = ruleListMap.get(stage);
						rules.addAll(newRules);
						GenericRuleReasoner newReasoner = createReasonerAndLoadRules(rules, stage);
						InfModel newInfModel = ModelFactory.createInfModel(newReasoner, infModel);
						newInfModel.size();   // this forces instantiation of the inference model
						infModel = newInfModel;
						reasoner = newReasoner;
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

	/**
	 * Method to add the derivations in the input model to the list of derivations field
	 * @param moreDM
	 */
	private void addDerivations(Model moreDM) {
		if (derivations == null) {
			derivations = new ArrayList<Derivation>();
		}
		StmtIterator sitr = moreDM.listStatements();
		while (sitr.hasNext()) {
			Triple s = sitr.nextStatement().asTriple();
			Iterator<Derivation> itr = getDerivation(s);
			while (itr.hasNext()) {
				derivations.add(itr.next());
			}
		}
	}

	@Override
	public boolean saveInferredModel(String filename, String modelname, boolean deductionsOnly) throws FileNotFoundException {
		try {
			prepareInfModel();
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		OntModel m;
		if (deductionsOnly) {
			if (deductionsModel == null) {
				m = ModelFactory.createOntologyModel(configurationMgr.getOntModelSpec(null), ((InfModel) infModel).getDeductionsModel());				
			}
			else {
				m = deductionsModel;
			}
		}
		else {
			m = ModelFactory.createOntologyModel(configurationMgr.getOntModelSpec(null), infModel);
		}

		if (m != null) {
			String format = SadlSerializationFormat.RDF_XML_ABBREV_FORMAT;	
		    FileOutputStream fps = new FileOutputStream(filename);
	        RDFWriter rdfw = m.getWriter(format);
	        rdfw.write(m, fps, modelname);
	        try {
				fps.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
	        return true;
		}
		return false;
	}

	@Override
	public DataSource getDerivations() throws InvalidDerivationException, ConfigurationException {
		if (getDerivationLevel().equals(DERIVATION_NONE)){
			return null;
		}
		try {
			getReasonerOnlyWhenNeeded();
			prepareInfModel();
			if (deductionsModel == null) {
				deductionsModel = ModelFactory.createOntologyModel(configurationMgr.getOntModelSpec(null), ((InfModel) infModel).getDeductionsModel());				
				addDerivations(deductionsModel);
			}
			if (derivations == null || derivations.isEmpty()) {
				return null;
			}
			StringWriter swriter = new StringWriter();
			PrintWriter out = new PrintWriter(swriter);
			out.println("Derivations from instance data combined with model '" + tbox + "', " + now() + "\n");
			int cnt = 0;
			HashSet<Derivation> seen = null;
			for (Derivation d : derivations) {
				d.printTrace(out, true);
//				if (getDerivationLevel().equals(DERIVATION_SHALLOW)) {
//					printShallowDerivationTrace(infModel.getGraph(), d, out, 0, 0, false);
//				}
//				else {
//					if (!derivationAlreadyShown(d, seen, out, 0)) {
//						// must be DERIVATION_DEEP
//						if (seen == null) {
//							seen = new HashSet<Derivation>();
//						}
//						printDeepDerivationTrace(infModel.getGraph(), d, out, true, 0, 0, seen, false);
//					}
//				}
				cnt++;
			}
			if (cnt > 0) {
				out.print("\n");
			}
			String derivations = swriter.toString();
			out.close();
			StringDataSource ds = new StringDataSource(derivations, "text/plain");
			ds.setName("Derivations");
			return ds;
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}		
		return null;
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
		    		return loadRulesFromFile(ruleFileName, stage);
		    	}
		    	else {
		    		String ending = ".rules-stage" + stage;
		    		if (!ruleFileName.endsWith(ending)) {
						String rulefn = ruleFileName + "-stage" + stage;
				    	File f2 = new File((new SadlUtils()).fileUrlToFileName(rulefn));
				    	if (f2.exists()) {
				    		return loadRulesFromFile(rulefn, stage);
				    	}
				    	else {
				    		logger.debug("No stage " + stage + " rule file found for base name " + ruleFileName + ".");
				    	}
		    		}
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

	private boolean loadRulesFromFile(String ruleFileName, Integer stage) throws IOException {
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
