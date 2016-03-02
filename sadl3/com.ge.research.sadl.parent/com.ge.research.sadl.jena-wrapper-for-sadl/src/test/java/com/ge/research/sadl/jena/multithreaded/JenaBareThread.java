/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.jena.multithreaded;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.atlas.web.HttpException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.jena.reasoner.SadlReadFailureHandler;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.OntDocumentManager.ReadFailureHandler;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.query.Syntax;
import com.hp.hpl.jena.rdf.model.InfModel;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.ModelGetter;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.reasoner.rulesys.GenericRuleReasoner;
import com.hp.hpl.jena.reasoner.rulesys.Rule;
import com.hp.hpl.jena.reasoner.rulesys.Rule.ParserException;
import com.hp.hpl.jena.shared.Lock;
import com.hp.hpl.jena.shared.RulesetNotFoundException;
import com.hp.hpl.jena.util.FileManager;

public class JenaBareThread extends Thread {
	private static Logger logger = LoggerFactory.getLogger("JenaBareThread");
	
	public void run() {
		String modelFolderDir = "file:/E:/crapo/workspaceSadlGEOnlyDistrib/com.ge.research.sadl.jena-wrapper-for-sadl/src/test/resources/DataModels/Shapes/OwlModels";
		String tbox = modelFolderDir + "/Rule.owl";
		String abox = modelFolderDir + "/Test.owl";
		String ontPolicy = modelFolderDir + "/ont-policy.rdf";
		OntModel schemaModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		try {
			schemaModel.enterCriticalSection(Lock.WRITE);
			ReadFailureHandler rfHandler = new SadlReadFailureHandler(logger);
			schemaModel.getDocumentManager().setMetadataSearchPath(ontPolicy, true);
			schemaModel.getDocumentManager().setProcessImports(true);
			schemaModel.getDocumentManager().setReadFailureHandler(rfHandler );
//			schemaModel.getSpecification().setImportModelGetter((ModelGetter) configurationMgr.getModelGetter());
			schemaModel.read(tbox);
		}
		finally {
			schemaModel.leaveCriticalSection();
		}
		
		OntModel dataModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		try {
			schemaModel.enterCriticalSection(Lock.READ);
			dataModel.enterCriticalSection(Lock.WRITE);
			dataModel.getDocumentManager().setProcessImports(true);
			dataModel.read(abox);
		}
		finally {
			schemaModel.leaveCriticalSection();
			dataModel.leaveCriticalSection();
		}
		
		String rulefn = modelFolderDir + "/Rule.rules";
		List<Rule> ruleList = new ArrayList<Rule>();
		try {
			InputStream in = FileManager.get().open(rulefn);
			if (in != null) {
			    try {
			    	InputStreamReader isr = new InputStreamReader(in);
			    	BufferedReader br = new BufferedReader(isr);
					List<Rule> rules = Rule.parseRules(Rule.rulesParserFromReader(br));
					if (rules != null) {
						ruleList.addAll(rules);
					}
			    } catch (ParserException e) {
			    	logger.error("Error reading rule file '" + rulefn + "': " + e.getMessage());
			    }
			    finally {
			    	try {
						in.close();
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
			    }
			    GenericRuleReasoner reasoner = new GenericRuleReasoner(ruleList);
				reasoner.setDerivationLogging(true);
				logger.debug("JenaReasonerPluging.initialize, size of ruleList from reasoner = "+reasoner.getRules().size());
				reasoner.setMode(GenericRuleReasoner.HYBRID);
				InfModel infModel = ModelFactory.createInfModel(reasoner, dataModel);
				String askQuery = "select ?s ?a where {?s <http://sadl.org/Shapes/Shapes#area> ?a}";
				QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(askQuery, Syntax.syntaxARQ), infModel);
				ResultSet results = qexec.execSelect();
				List<String> queryVars = results.getResultVars();
				String[] columnName = new String[queryVars.size()];
				columnName = queryVars.toArray(columnName);
				while (results.hasNext()) {
					QuerySolution soln = results.next();
					ArrayList<Object> temp = new ArrayList<Object>();
					for (int j = 0; j < columnName.length; j++) {
						RDFNode n = soln.get(columnName[j]);
						if (n != null && n.isLiteral()) {
							Object val = ((Literal)n).getValue();
							System.out.println("Value: " + val);
							if (val.equals(13.75)) {
								System.out.println("Test passed");
							}
							else {
								System.out.println("Test failed");								
							}
						}
					}
				}
			}
		}
		catch (RulesetNotFoundException e) {
			e.printStackTrace();
		}
		catch (HttpException e) {
			e.printStackTrace();
		}
		

	}

}
