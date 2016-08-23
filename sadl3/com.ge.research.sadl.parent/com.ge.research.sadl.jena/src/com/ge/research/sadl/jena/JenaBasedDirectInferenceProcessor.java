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
package com.ge.research.sadl.jena;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;

import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.KnownNode;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.model.gp.TestResult;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.ValueTableNode;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.model.gp.Test.ComparisonType;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.server.SessionNotFoundException;
import com.hp.hpl.jena.datatypes.xsd.XSDDateTime;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.vocabulary.XSD;

public class JenaBasedDirectInferenceProcessor extends JenaBasedInferenceProcessor implements I_InferenceProcessor {
	private OntModel theJenaModel;
	private IReasoner reasoner;
	public JenaBasedDirectInferenceProcessor(Resource rsrc, OntModel owlModel, JenaBasedSadlModelProcessor _modelProcessor, ProcessorContext context) {
		setCurrentResource(rsrc);
		setTheJenaModel(owlModel);
		modelProcessor = _modelProcessor;
		setCurrentContext(context);
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.jena.I_InferenceProcessor#validateModel(org.eclipse.emf.ecore.resource.Resource, com.ge.research.sadl.processing.ValidationAcceptor, com.ge.research.sadl.processing.IModelProcessor.ProcessorContext)
	 */
	@Override
	public boolean validateModel() throws ConfigurationException, ReasonerNotFoundException {
		List<com.ge.research.sadl.reasoner.ModelError> validityErrors = reasoner.checkModelValidity();
		int errorCnt = 0;
		for (int i = 0; i < validityErrors.size(); i++) {
			com.ge.research.sadl.reasoner.ModelError verror = validityErrors.get(i);
			if(verror.getErrorType().equals(ErrorType.ERROR)) {
				errorCnt++;
				modelProcessor.addError(verror.getErrorMsg(),null);
			}
			else if (verror.getErrorType().equals(ErrorType.WARNING)) {
				modelProcessor.addWarning(verror.getErrorMsg(), null);
			}
			else {
				modelProcessor.addInfo(verror.getErrorMsg(), null);
			}
		}
		if (errorCnt == 0) {
			return true;
		}
		return false;
	}
	
	private IReasoner getReasoner() throws ConfigurationException, ReasonerNotFoundException {
		if (reasoner == null) {
			String modelFolderPathname = modelProcessor.getModelFolderPath(getCurrentResource());
			reasoner = modelProcessor.getConfigMgr(getCurrentResource(), getRepoType()).getReasoner();
			if (!reasoner.isInitialized()) {
				reasoner.setConfigurationManager(modelProcessor.getConfigMgr(getCurrentResource(), getRepoType()));
				reasoner.initializeReasoner(modelFolderPathname, modelProcessor.getModelName(), getRepoType());
			}

		}
		return reasoner;
	}
	
	private String queryStringFromQueryCommand(Query cmd)
			throws TranslationException, InvalidNameException, ConfigurationException {
		String query = ((Query)cmd).getSparqlQueryString();
		if (query == null) {
			query = getTranslator().translateQuery(getTheJenaModel(), (Query) cmd);
		}
		query = SadlUtils.stripQuotes(query);
		return query;
	}

	private OntModel getTheJenaModel() {
		return theJenaModel;
	}

	private void setTheJenaModel(OntModel theJenaModel) {
		this.theJenaModel = theJenaModel;
	}
	
	public ResultSet matchTriplePattern(String subject, String predicate, String objval) throws TripleNotFoundException, ConfigurationException, ReasonerNotFoundException {
		return getReasoner().ask(subject, predicate, objval);
	}
	
	public String sadlLiteralToString(com.ge.research.sadl.model.gp.Literal lv) {
		return getTheJenaModel().createTypedLiteral(lv.getValue()).toString();
	}

	public TestResult testTriple(TripleElement triple)
			throws TripleNotFoundException, InvalidTypeException, ConfigurationException, ReasonerNotFoundException {
		Node sn = triple.getSubject();
		String subject = null;
		if (sn instanceof NamedNode) {
			subject = ((NamedNode) sn).toFullyQualifiedString();
		}
		Node pn = triple.getPredicate();
		String predicate = null;
		if (pn instanceof NamedNode) {
			predicate = ((NamedNode) pn).toFullyQualifiedString();
		}
		Node on = triple.getObject();
		String object = null;
		if (triple.getModifierType().equals(TripleModifierType.None)
				&& (pn instanceof RDFTypeNode || (on instanceof NamedNode
						&& sn instanceof NamedNode && !(sn instanceof VariableNode)))) {
			object = ((NamedNode) on).toFullyQualifiedString();
		}
		if (subject != null && predicate != null) {
			ResultSet rs = getReasoner().ask(subject, predicate, object);
			if (logger.isInfoEnabled())
				logger.debug("ResultSet: "
						+ (rs != null ? rs.toString() : "null"));
			if (on instanceof NamedNode) { // object != null) {
				if (triple.getModifierType().equals(TripleModifierType.None)) {
					if (rs != null && rs.getRowCount() > 0) {
						TestResult testResult = new TestResult();
						testResult.setPassed(true);
						return testResult;
					} else {
						TestResult tr = new TestResult(false);
						tr.setMsg("Triple '" + triple.toString()
								+ "' not found in model.");
						return tr;
					}
				} else {
					if (triple.getModifierType().equals(TripleModifierType.Not)) {
						return doTestComparison(on, rs, ComparisonType.Neq);
					} else if (triple.getModifierType().equals(
							TripleModifierType.Only)) {
						return doTestComparison(on, rs, ComparisonType.IsOnly);
					} else if (triple.getModifierType().equals(
							TripleModifierType.NotOnly)) {
						return doTestComparison(on, rs,
								ComparisonType.IsNotOnly);
					} else {
						throw new InvalidTypeException(
								"Triple test has unknown type '"
										+ triple.getModifierType() + "'");
					}
				}
			} else {
				TestResult testResult = new TestResult();
				if (on instanceof com.ge.research.sadl.model.gp.Literal
						|| on instanceof ValueTableNode
						|| on instanceof KnownNode) {
					int rcnt;
					if (rs != null && (rcnt = rs.getRowCount()) > 0) {
						// see if we can get a match on the literal
						if (triple.getModifierType().equals(
								TripleModifierType.None)) {
							OntProperty oprop = getTheJenaModel().getOntProperty(
									predicate);
							if (!(on instanceof ValueTableNode) && oprop != null && oprop.isDatatypeProperty()) {
								OntResource rngrsrc = null;
								if (oprop != null) {
									rngrsrc = oprop.getRange();
								}
								for (int i = 0; i < rcnt; i++) {
									Object result = rs.getResultAt(i, 0);
									if (compareValues(on, result, rngrsrc)) {
										testResult.setPassed(true);
										return testResult;
									}
								}
							} else {
								return doTestComparison(on, rs,
										ComparisonType.Eq);
							}
						} else {
							if (triple.getModifierType().equals(
									TripleModifierType.Not)) {
								return doTestComparison(on, rs,
										ComparisonType.Neq);
							} else if (triple.getModifierType().equals(
									TripleModifierType.Only)) {
								return doTestComparison(on, rs,
										ComparisonType.IsOnly);
							} else if (triple.getModifierType().equals(
									TripleModifierType.NotOnly)) {
								return doTestComparison(on, rs,
										ComparisonType.IsNotOnly);
							} else {
								throw new InvalidTypeException(
										"Triple test has unknown type '"
												+ triple.getModifierType()
												+ "'");
							}
						}
					} else if (triple.getModifierType().equals(
							TripleModifierType.Not)) {
						return new TestResult(true);
					}
				}
				testResult.setPassed(false);
				testResult.setType(ComparisonType.Eq);
				testResult.addLhsResult(rs);
				testResult.addRhsResult(on);
				return testResult;
			}
		} else {
			TestResult testResult = new TestResult();
			testResult.setPassed(false);
			testResult
					.setMsg("Triple pattern without subject and predicate not yet supported.");
			return testResult;
		}
	}

	public Literal valueToJenaLiteral(Object val, String rangeUri) {
		return getTheJenaModel().createTypedLiteral(val, rangeUri);
	}

	public ResultSet processQuery(Query query) throws ConfigurationException, ReasonerNotFoundException, TranslationException, InvalidNameException, JenaProcessorException, QueryParseException, QueryCancelledException {
		String queryString = getTranslator().translateQuery(getTheJenaModel(), query);
		ResultSet results =  getReasoner().ask(queryString);
		return results;
	}

}
