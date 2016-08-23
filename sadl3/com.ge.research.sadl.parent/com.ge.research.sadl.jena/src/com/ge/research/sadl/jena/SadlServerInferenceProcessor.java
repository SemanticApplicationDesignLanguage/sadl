package com.ge.research.sadl.jena;

import java.io.IOException;

import org.eclipse.emf.ecore.resource.Resource;

import com.ge.research.sadl.model.gp.KnownNode;
import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.model.gp.Test.ComparisonType;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.TestResult;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.ValueTableNode;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.server.ISadlServer;
import com.ge.research.sadl.server.SessionNotFoundException;
import com.ge.research.sadl.server.server.SadlServerImpl;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;

public class SadlServerInferenceProcessor extends JenaBasedInferenceProcessor implements I_InferenceProcessor {
	private OntModel theJenaModel;
	protected ISadlServer kServer = null;

	public SadlServerInferenceProcessor(Resource rsrc, OntModel owlModel,
			JenaBasedSadlModelProcessor _modelProcessor, ProcessorContext context) {
		setCurrentResource(rsrc);
		setTheJenaModel(owlModel);
		modelProcessor = _modelProcessor;
		setCurrentContext(context);
	}


	@Override
	public boolean validateModel() throws ConfigurationException, ReasonerNotFoundException {
		// TODO Auto-generated method stub
		return false;
	}

	private OntModel getTheJenaModel() {
		return theJenaModel;
	}

	private void setTheJenaModel(OntModel theJenaModel) {
		this.theJenaModel = theJenaModel;
	}

	private ISadlServer getKServer() {
		if (kServer == null) {
			String modelFolder;
			try {
				modelFolder = modelProcessor.getConfigMgr(getCurrentResource(), getRepoType()).getModelFolder();
				kServer = new SadlServerImpl(modelFolder);
				kServer.selectServiceModel(modelFolder, modelProcessor.getModelName());
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ReasonerNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (SessionNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return kServer;
	}


//	@Override
//	public int testModel(Test cmd)
//			throws TranslationException, InvalidNameException, ConfigurationException, ReasonerNotFoundException,
//			JenaProcessorException, QueryParseException, QueryCancelledException, TripleNotFoundException,
//			InvalidTypeException {
//		return super.testModel(cmd);
//	}


	@Override
	public ResultSet processQuery(Query sadlQuery)
			throws ConfigurationException, ReasonerNotFoundException, TranslationException, InvalidNameException,
			JenaProcessorException, QueryParseException, QueryCancelledException {
		String query = modelProcessor.getTranslator(getCurrentResource(), getCurrentContext()).translateQuery(getTheJenaModel(), sadlQuery);
		try {
			return getKServer().query(query);
		} catch (SessionNotFoundException e) {
			throw new JenaProcessorException(e.getMessage(), e);
		}
	}

	@Override
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
			ResultSet rs = null;
			try {
				rs = getKServer().ask(subject, predicate, object);
			} catch (QueryCancelledException e) {
				TestResult tr = new TestResult(false);
				tr.setMsg("Operation cancelled.");
				return tr;
			} catch (SessionNotFoundException e) {
				throw new ConfigurationException(e.getMessage(), e);
			}
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

	@Override
	public String sadlLiteralToString(Literal lv) {
		// TODO this isn't really the right thing--need to add method to SadlServer?
		return lv.toFullyQualifiedString();
	}


	@Override
	public ResultSet matchTriplePattern(String subject, String predicate, String objval)
			throws TripleNotFoundException, ConfigurationException, ReasonerNotFoundException {
		try {
			return getKServer().ask(subject, predicate, objval);
		} catch (QueryCancelledException e) {
			return null;
		} catch (SessionNotFoundException e) {
			throw new ConfigurationException(e.getMessage(), e);
		}
	}


	@Override
	public com.hp.hpl.jena.rdf.model.Literal valueToJenaLiteral(Object val, String rangeUri) {
		// TODO Auto-generated method stub
		return null;
	}

}
