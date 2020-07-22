/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ide.handlers;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.XtextResource;

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.external.ExternalEmfResource;
import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.SadlCommandResult;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.google.common.base.Supplier;

/**
 * IDE agnostic way for running a SADL/SPARQL query.
 * 
 * The underlying logic was taken from {@code com.ge.research.sadl.ui.handlers.RunQuery.execute(ExecutionEvent)}.
 * 
 * @author akos.kitta
 *
 */
public class SadlRunQueryHandler extends SadlIdeActionHandler {
	
	public void run(Path path, Supplier<XtextResource> resourceSupplier, String query) {
		run(path, resourceSupplier, query, getPreferences(resourceSupplier.get()));
	}
	
	public void runEER(Path path, Supplier<ExternalEmfResource> resourceSupplier, String query) {
		ExternalEmfResource eer = resourceSupplier.get();
		// what needs to happen to an ExternalEmfResource before it can be queried?
		// any way to get preferences here?
		run(path, null, query, null);
	}

	public void run(Path path, Supplier<XtextResource> resourceSupplier, String query, Map<String, String> properties) {
		try {
			// XXX: this is not used anyway!
			String owlFileName = null;
			boolean tryAddingOwlExtension = true;
			String fmt = properties.get(SadlPreferences.OWL_MODEL_FORMAT.getId());
			final String format = fmt != null ? fmt : SadlSerializationFormat.RDF_XML_ABBREV_FORMAT;
			String ext = "." + SadlSerializationFormat.getFileExtension(SadlSerializationFormat.getRDFFormat(format));
			if (path.getFileName().toString().endsWith(".sadl")) {
				// run query on this model
//				Resource res = prepareActionHandler(target[2]);
				console.info("Adhoc Query of '" + path.toString() + "' requested.\n");
				
//				final List<Issue> issues = new ArrayList<Issue>();
//				processor.processAdhocQuery(res, new ValidationAcceptor(new IAcceptor<Issue>(){

//					@Override
//					public void accept(Issue t) {
//						issues.add(t);
//					}
					
//				}),  new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(res)), query);
//				if (issues.size() > 0) {
//					for (Issue issue: issues) {
//						output.writeToConsole(MessageType.ERROR, issue.getMessage() + "\n");
//					}
//				}
				owlFileName = path.getFileName().toString().replaceFirst("[.][^.]+$", ext);
			}
			else if (path.getFileName().toString().endsWith(".owl")) {
				// run query on this model
//				Resource res = prepareActionHandler(trgtFile);
				console.info("Adhoc Query of '" + path.toString() + "' requested.\n");
				owlFileName = path.getFileName().toString();
			}
			else if (tryAddingOwlExtension) {
				owlFileName = path.getFileName().toString().concat(".owl");
			}
			String modelFolderUri = getOwlModelsFolderPath(path).toString(); 
			IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format);
			new SadlUtils();
			query = SadlUtils.stripQuotes(query);
			File qf = new File(query);
			List<String> qlist = null;
			if (qf.exists()) {
				List<String[]> queries = new SadlUtils().parseQueries(qf);
				if (queries != null && queries.size() > 0) {
					qlist = new ArrayList<String>();
					for (int i = 0; i < queries.size(); i++) {
						qlist.add(queries.get(i)[1]);
					}
				}
			}
			else {
				qlist = new ArrayList<String>();
				qlist.add(query);
			}
			{
				IReasoner reasoner = configMgr.getReasoner();
				if (!reasoner.isInitialized()) {
					reasoner.setConfigurationManager(configMgr);
					String modelName;
					try {
						modelName = configMgr.getPublicUriFromActualUrl(new SadlUtils().fileNameToFileUrl(modelFolderUri + "/" + owlFileName));
					}
					catch (ConfigurationException e) {
						modelName = configMgr.getPublicUriFromActualUrl(new SadlUtils().fileNameToFileUrl(path.toString()));
					}
					reasoner.initializeReasoner(modelFolderUri, modelName, format);
				}
				if (reasoner != null) {	
					int qidx = 0;
					while (query != null) {
						if (qlist != null) {
							if (qidx < qlist.size()) {
								query = qlist.get(qidx++);
							}
							else {
								break;
							}
						}
						try {
							boolean success = false;
							String currentQuery = reasoner.prepareQuery(query);
							if (!currentQuery.contains(" ") && !currentQuery.contains("?")) {
								// this might be a named query
								Resource res = prepareResource(resourceSupplier.get());
								Object[] results = inferenceProcessor.runNamedQuery(res, currentQuery);
								if (results != null && results.length > 0 && results[0] != null) {
									SadlCommandResult result = (SadlCommandResult) results[0];
									String qdisplay = currentQuery;
									if (result.getClass() != null && result.getCmd() instanceof Query && ((Query)result.getCmd()).getSparqlQueryString() != null) {
										qdisplay += " (" + ((Query)result.getCmd()).getSparqlQueryString() + ")";
									}
									if (result.getResults() != null) {
										if (result.getResults() instanceof ResultSet) {
											console.info("Query '" + qdisplay + "' returned:\n");
											console.info(((ResultSet)result.getResults()).toStringWithIndent(5));
											success = true;
										}
										else {
											console.warn("Query '" + qdisplay + "' returned no results\n");
										}
									}
									else {
										console.warn("Query '" + qdisplay + "' returned no results\n");
									}
									if (result.getErrors() != null) {
										List<ModelError> errors = result.getErrors();
										if (errors.size() > 0) {
											console.error("Query '" + qdisplay + "' errors:\n");
										}
										for (int i = 0; i < errors.size(); i++) {
											console.error(errors.get(i).toString() + "\n");
										}
									}
								}

//								String queryquery = null;
//								queryquery = "select ?nq ?qnq where {?nq <rdf:type> <NamedQuery> . ?nq <http://www.w3.org/2000/01/rdf-schema#isDefinedBy> ?qnq}";
//								queryquery = reasoner.prepareQuery(queryquery);
//								ResultSet qrs = reasoner.ask(queryquery);
//								if (qrs != null && qrs.getRowCount() > 0) {
//									if (qrs.getResultAt(0, 0).toString().endsWith(currentQuery)) {
//										currentQuery = qrs.getResultAt(0, 1).toString();
//									}
//								}
								
							}
							if (!success) {
								ResultSet rs = reasoner.ask(currentQuery);
								if (rs != null) {
									if (currentQuery.toLowerCase().startsWith("construct")) {
										String desc = "Adhoc query Graph";
	        							String baseFileName = path.getFileName().toString() + System.currentTimeMillis(); 							
		        						resultSetToGraph(path, rs, desc, baseFileName, null, properties);
									}
									else {
										console.info(rs.toStringWithIndent(5));
									}
								}
								else {
									console.warn("Query '" + currentQuery + "' returned no results\n");
								}
							}
						}
						catch (Throwable t) {
							System.err.println("Error processing query '" + query + "'");
							System.err.println("   " + t.getMessage());
						}
					}
					configMgr.clearReasoner();
				}
			}
//			else if (trgtFile.getName().endsWith("test")) {
//				// run test suite
//				output.writeToConsole(MessageType.INFO, "Testing of suite '" +  trgtFile.getName() + "' requested.\n");
//			}
		
		} catch (Exception e) {
			console.error(e.getMessage());
		}
	}

}
