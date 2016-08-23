package com.ge.research.sadl.jena;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.KnownNode;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.model.gp.Test.ComparisonType;
import com.ge.research.sadl.model.gp.TestResult;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.ValueTableNode;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.hp.hpl.jena.datatypes.xsd.XSDDateTime;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.vocabulary.XSD;

abstract public class JenaBasedInferenceProcessor {

	protected static final Logger logger = LoggerFactory.getLogger(JenaBasedDirectInferenceProcessor.class);
	private Resource currentResource;
	private ProcessorContext currentContext;
	protected JenaBasedSadlModelProcessor modelProcessor;
	private String repoType;
	private ITranslator translator;
	/**
	 * This enum is for internal use to determine how to display values in
	 * TestResult on fail
	 */
	protected enum DisplayType {
		ValueTableNodeDisplay, LiteralDisplay
	}

	public JenaBasedInferenceProcessor() {
		super();
	}

	protected String getRepoType() {
		if (repoType == null) {
			repoType = ConfigurationManager.RDF_XML_ABBREV_FORMAT; // default
			String pref = getCurrentContext().getPreferenceValues().getPreference(SadlPreferences.OWL_MODEL_FORMAT);
			if (pref != null) {
				repoType = pref;
			}
		}
		return repoType;
	}

	public int testModel(Test cmd) throws TranslationException, InvalidNameException, ConfigurationException, ReasonerNotFoundException, JenaProcessorException, QueryParseException, QueryCancelledException, TripleNotFoundException, InvalidTypeException {
		int testsPassed = 0;
		TestResult testResult = null;
		Object lhs = ((Test) cmd).getLhs();
		Object rhs = ((Test) cmd).getRhs();
		if (lhs == null || rhs == null) {
			// this is just a pass (true) or fail (false) test,
			// not a comparison
			if (lhs instanceof Query) {
				ResultSet rs = processQuery((Query) lhs);
				modelProcessor.addError("Error: this Query case is not implemented!", null);
			} else {
				TripleElement triple = null;
				if (lhs instanceof TripleElement) {
					triple = (TripleElement) lhs;
				} else if (rhs instanceof TripleElement) {
					triple = (TripleElement) rhs;
				}
				if (triple != null) {
					testResult = testTriple(triple);
				} else if (lhs instanceof List<?>
				&& ((List<?>) lhs).size() == 2) {
					testResult = testFilteredQuery((List<?>) lhs);
				} else if (rhs instanceof List<?>
				&& ((List<?>) rhs).size() == 2) {
					testResult = testFilteredQuery((List<?>) rhs);
				} else if (lhs instanceof List<?>
				&& rhs == null) {
					Object lhobj = convertToComparableObject(lhs, ((Test) cmd).getLhsVariables());
					if (lhobj instanceof ResultSet
							&& ((ResultSet) lhobj)
							.getColumnCount() > 0) {
						testResult = new TestResult(true);
					}
				} else {
					testResult = new TestResult(false);
					if (lhs == null && rhs == null) {
						testResult.setMsg("Unable to convert '"
								+ cmd.toString()
								+ "' to a test.");
					} else if (lhs == null) {
						testResult.setMsg("'" + rhs.toString()
						+ "' did not return a value.");
					} else if (rhs == null) {
						testResult.setMsg("'" + lhs.toString()
						+ "' did not return a value.");
					}
				}
			}
		} else {
			Object lhobj = convertToComparableObject(((Test) cmd).getLhs(),
					((Test) cmd).getLhsVariables());
			Object rhobj = convertToComparableObject(((Test) cmd).getRhs(),
					((Test) cmd).getRhsVariables());
			ComparisonType type = ((Test) cmd).getCompType();
			if (type != null
					&& (type.equals(ComparisonType.IsNot) || type
							.equals(ComparisonType.Neq))
					&& ((lhobj == null && (rhobj instanceof KnownNode || rhobj != null)) || (rhobj == null && (lhobj instanceof KnownNode || lhobj != null)))) {
				testResult = new TestResult(true);

			} else if (lhobj != null && rhobj != null
					&& type != null) {
				testResult = doTestComparison(lhobj, rhobj,
						type);
			} else {
				testResult = new TestResult(false);
				String msg = "";
				if (type == null) {
					msg += "Test has no comparison operator. ";
				}
				if (lhobj == null) {
					msg += "'" + lhs.toString()
					+ "' did not return a value. ";
				}
				if (rhobj == null) {
					msg += "'" + rhs.toString()
					+ "' did not return a value. ";
				}
				testResult.setMsg(msg);
			}
		}
		if (testResult == null) {
			String msg = "Test result is null. This should not happen. Test is: "
					+ cmd.toString();
			modelProcessor.addError(msg, null);
		} else if (testResult.isPassed()) {
			testsPassed++;
			String msg = "Test passed: " + cmd.toString()
			+ "\n";
			modelProcessor.addInfo(msg, null);
		} else {
			String msg = "Test failed: " + cmd.toString()
			+ "\n";
			if (testResult.getMsg() != null) {
				msg += "    " + testResult.getMsg() + "\n";
			}
			modelProcessor.addError(msg, null);
		}
		return testsPassed;
	}	

	private TestResult testFilteredQuery(List<?> lst)
			throws QueryParseException, ConfigurationException, ReasonerNotFoundException {
		if (lst.size() != 2) {
			throw new QueryParseException(
					"Filtered Query not of expected size 2");
		}
		if (!(lst.get(0) instanceof TripleElement)) {
			throw new QueryParseException(
					"First element of Filtered Query not a Triple");
		}
		// if (!(lst.get(1) instanceof Junction)) {
		// throw new
		// QueryParseException("Second element of FilteredQuery not a Junction");
		// }
		Query newQuery = new Query();
		try {
			newQuery.setPatterns((List<GraphPatternElement>) lst);
			setVariablesFromPatterns(newQuery);
			ResultSet lhResultSet = processQuery(newQuery);
//			String queryStr = getTranslator().translateQuery(getTheJenaModel(), newQuery);
//			logger.debug("Translated query: " + queryStr);
//			ResultSet lhResultSet = getReasoner().ask(queryStr);
			if (lhResultSet != null) {
				return new TestResult(true);
			}
			TestResult tr = new TestResult(false);
			tr.setMsg("Test query '" + newQuery.toFullyQualifiedString() + "' did not return a value");
			return tr;
		} catch (Exception e) {
			TestResult tr = new TestResult(false);
			tr.setMsg("Encountered error evaluating test: "
					+ e.getLocalizedMessage());
			return tr;
		}
	}
	
	public Resource getCurrentResource() {
		return currentResource;
	}

	protected void setCurrentResource(Resource currentResource) {
		this.currentResource = currentResource;
	}

	public ProcessorContext getCurrentContext() {
		return currentContext;
	}

	protected void setCurrentContext(ProcessorContext currentContext) {
		this.currentContext = currentContext;
	}

	public TestResult doTestComparison(Object lhobj, Object rhobj, ComparisonType type) {
		Object lhval = toComparableObject(lhobj);
		Object rhval = toComparableObject(rhobj);
		if (lhval != null && rhval != null) {
			if (ResultSet.valuesMatch(lhval, rhval)) {
				if (type.equals(ComparisonType.Eq)
						|| type.equals(ComparisonType.GTE)
						|| type.equals(ComparisonType.LTE)) {
					return new TestResult(true);
				} else if (type.equals(ComparisonType.LT)
						|| type.equals(ComparisonType.GT)) {
					return createTestFailed(lhobj, lhval, rhobj, rhval, type);
				} else if (type.equals(ComparisonType.Neq)
						|| type.equals(ComparisonType.IsNot)) {
					return createTestFailed(lhobj, lhval, rhobj, rhval, type);
				}
			}
			if (type.equals(ComparisonType.Neq)
					|| type.equals(ComparisonType.IsNot)) {
				return new TestResult(true);
			}
			// this leaves GT, GTE, LT, & LTE when not Eq, IsOnly, IsNotOnly
			if (type.equals(ComparisonType.GTE)
					|| type.equals(ComparisonType.GT)) {
				if (ResultSet.lessThan(rhval, lhval)) {
					return new TestResult(true);
				}
			}
			if (type.equals(ComparisonType.LTE)
					|| type.equals(ComparisonType.LT)) {
				if (ResultSet.lessThan(lhval, rhval)) {
					return new TestResult(true);
				}
			}
			if (type.equals(ComparisonType.IsOnly)) {
				if (ResultSet.valuesMatchExactly(lhval, rhval)) {
					return new TestResult(true);
				}
			}
			if (type.equals(ComparisonType.IsNotOnly)) {
				if (!ResultSet.valuesMatchExactly(lhval, rhval)) {
					return new TestResult(true);
				}
			}
		}
		if ((type.equals(ComparisonType.IsNot) || type
				.equals(ComparisonType.Neq))) {
			if ((rhval instanceof KnownNode && lhval == null)
					|| (lhval instanceof KnownNode && rhval == null)) {
				return new TestResult(true);
			}
			if ((lhval instanceof ResultSet && rhval == null)
					|| (rhval instanceof ResultSet && lhval == null)) {
				return new TestResult(true);
			} else if ((rhval == null && lhval != null)
					|| (lhval == null && rhval != null)) {
				return new TestResult(true);
			}
		}
		return createTestFailed(lhobj, lhval, rhobj, rhval, type);
	}

	TestResult createTestFailed(Object lhobj, Object lhval,
			Object rhobj, Object rhval, ComparisonType type) {
		TestResult result = new TestResult(false);
		try {
			DisplayType dtype = determineDisplayType(lhobj, rhobj, lhval, rhval);
			result.addLhsResult(toDisplayString(lhval != null ? lhval : lhobj,
					dtype));
			result.addRhsResult(toDisplayString(rhval != null ? rhval : rhobj,
					dtype));
		} catch (Throwable t) {
			result.setMsg("Unexpected exception running test: "
					+ t.getLocalizedMessage());
		}
		result.setType(type);
		return result;
	}
	
	public DisplayType determineDisplayType(Object lobj, Object robj,
			Object lval, Object rval) {
		if (((lobj != null
				&& (lobj instanceof ValueTableNode || lobj instanceof ResultSet) || (lval != null && (lval instanceof ValueTableNode || lval instanceof ResultSet)))
				&& ((robj != null && (robj instanceof ValueTableNode || robj instanceof ResultSet))) || (rval != null && (rval instanceof ValueTableNode || rval instanceof ResultSet)))) {
			return DisplayType.ValueTableNodeDisplay;
		}
		return DisplayType.LiteralDisplay;
	}

	private String toDisplayString(Object obj, DisplayType dtype) {
		boolean displayNS = false;
		if (obj == null) {
			return null;
		}
		if (dtype.equals(DisplayType.ValueTableNodeDisplay)) {
			if (obj instanceof ValueTableNode) {
				return ((ValueTableNode) obj).toString();
			} else if (obj instanceof ResultSet) {
				return ((ResultSet) obj).toValueTableNode().toString();
			} else {
				return toDisplayString(obj, DisplayType.LiteralDisplay);
			}
		} else if (dtype.equals(DisplayType.LiteralDisplay)) {
			if (obj instanceof com.ge.research.sadl.model.gp.Literal) {
				return ((com.ge.research.sadl.model.gp.Literal) obj).getValue()
						.toString();
			} else if (!displayNS && obj instanceof String
					&& ((String) obj).indexOf("#") > 0) {
				return ((String) obj)
						.substring(((String) obj).indexOf("#") + 1);
			}
			return obj.toString();
		} else {
			return obj.toString();
		}
	}

	Object toComparableObject(Object obj) {
		if (obj instanceof com.ge.research.sadl.model.gp.Literal) {
			return ((com.ge.research.sadl.model.gp.Literal) obj).getValue();
		} else if (obj instanceof ResultSet) {
			ResultSet rs = (ResultSet) obj;
			if (rs.getRowCount() < 1 || rs.getColumnCount() < 1) {
				return null;
			}
			if (rs.getColumnCount() == 1 && rs.getRowCount() == 1) {
				return rs.getResultAt(0, 0);
			} else {
				return rs;
			}
		} else if (obj instanceof ValueTableNode) {
			return ((ValueTableNode) obj).toResultSet();
		} else if (obj instanceof TripleElement) {
			Object[][] data = new Object[1][];
			Object[] row = new Object[3];
			data[0] = row;
			row[0] = toComparableObject(((TripleElement) obj).getSubject());
			row[1] = toComparableObject(((TripleElement) obj).getPredicate());
			row[2] = toComparableObject(((TripleElement) obj).getObject());
			ResultSet rs = new ResultSet(data);
			return rs;
		} else if (obj instanceof BuiltinElement) {
			// see if we can evaluate the builtin
			// TODO
		} else if (obj instanceof NamedNode) {
			return ((NamedNode) obj).toFullyQualifiedString();
		} else if (obj instanceof KnownNode) {
			return obj;
		}
		return obj;
	}

	protected boolean compareValues(Node node, Object objVal, OntResource rngrsrc) {
		Object v1 = node;
		Object v2 = objVal;
		if (node instanceof Literal) {
			v1 = ((Literal) node).getValue();
		} else if (node instanceof com.ge.research.sadl.model.gp.Literal) {
			v1 = ((com.ge.research.sadl.model.gp.Literal) node).getValue();
		}
		if (objVal instanceof Literal) {
			v2 = ((Literal) objVal).getValue();
		} else if (objVal instanceof com.ge.research.sadl.model.gp.Literal) {
			v2 = ((com.ge.research.sadl.model.gp.Literal) objVal).getValue();
		}
		return compareObjects(v1, v2, rngrsrc);
	}

	protected ITranslator getTranslator() throws ConfigurationException {
		if (translator == null) {
			translator = modelProcessor.getConfigMgr(getCurrentResource(), getRepoType()).getTranslator();
		}
		return translator;
	}

	protected Object convertToComparableObject(Object obj, List<String> testVars)
			throws TranslationException, ConfigurationException,
			QueryParseException, TripleNotFoundException, QueryCancelledException, ReasonerNotFoundException, InvalidNameException, JenaProcessorException {
		if (obj instanceof Query) {
			if (((Query) obj).getVariables() == null && testVars != null) {
				((Query) obj).setVariables(testVars);
			}
		} else if (obj instanceof TripleElement) {
			Query newQuery = new Query();
			newQuery.addPattern((TripleElement) obj);
			if (testVars != null) {
				newQuery.setVariables(testVars);
			}
			obj = newQuery;
		} else if (obj instanceof List<?>
				&& ((List<?>) obj).get(0) instanceof GraphPatternElement) {
			Query newQuery = new Query();
			newQuery.setPatterns((List<GraphPatternElement>) obj);
			if (testVars != null) {
				newQuery.setVariables(testVars);
			}
			obj = newQuery;
		} else if (obj instanceof ValueTableNode) {
			return obj;
		} else if (obj instanceof NamedNode) {
			return obj;
		} else if (obj instanceof com.ge.research.sadl.model.gp.Literal) {
			Object litObj = ((com.ge.research.sadl.model.gp.Literal) obj)
					.getValue();
			if (litObj instanceof String) {
				if (SadlModelProcessor.isSparqlQuery((String)litObj)) {
					Query tmpQuery = new Query();
					tmpQuery.setSparqlQueryString(litObj.toString());
					obj = processQuery(tmpQuery);
//						String queryStr = getTranslator().translateQuery(getTheJenaModel(), tmpQuery);
//						logger.debug("Found SPARQL query as literal: "
//								+ queryStr);
//						obj = getReasoner().ask(queryStr);
				}
			}
			return obj;
		} else if (obj instanceof KnownNode) {
			return obj;
		} else {
			throw new TranslationException("Conversion of '" + obj.toString()
					+ "' to a comparable object not currently supported.");
		}
		if (obj instanceof Query) {
			if (((Query) obj).getVariables() == null) {
				setVariablesFromPatterns((Query) obj);
			}
			if (((Query) obj).getVariables() != null) {
				obj = processQuery((Query)obj);
//				String queryStr = null;
//				try {
//					queryStr = getTranslator().translateQuery(getTheJenaModel(), (Query) obj);
//					logger.debug("Translated query: " + queryStr);
//					ResultSet lhResultSet = getReasoner().ask(queryStr);
//					obj = lhResultSet;
//				} catch (InvalidNameException e) {
//					throw new TranslationException("Translation failed: ", e);
//				}
			} else {
				List<GraphPatternElement> elements = ((Query) obj)
						.getPatterns();
				for (int i = 0; elements != null && i < elements.size(); i++) {
					GraphPatternElement gpe = elements.get(i);
					if (gpe instanceof TripleElement) {
						// these should all be NamedNodes or there would have
						// been some variable found in the if clause
						Node subj = ((TripleElement) gpe).getSubject();
						Node pred = ((TripleElement) gpe).getPredicate();
						Node objval = ((TripleElement) gpe).getObject();
						String strObjVal;
						if (objval instanceof com.ge.research.sadl.model.gp.Literal) {
							strObjVal = sadlLiteralToString((com.ge.research.sadl.model.gp.Literal) objval);
						} else {
							strObjVal = ((NamedNode) objval)
									.toFullyQualifiedString();
						}
						ResultSet results = matchTriplePattern(((NamedNode) subj).toFullyQualifiedString(),
								((NamedNode) pred).toFullyQualifiedString(),
								strObjVal);
//						ResultSet results = getReasoner().ask(
//								((NamedNode) subj).toFullyQualifiedString(),
//								((NamedNode) pred).toFullyQualifiedString(),
//								strObjVal);
						if (results != null && results.hasNext()) {
							obj = new Boolean(true);
						} else {
							obj = new Boolean(false);
						}
					}
				}
			}
		}
		return obj;
	}

	protected void setVariablesFromPatterns(Query query) {
		List<GraphPatternElement> elements = query.getPatterns();
		List<String> vars = new ArrayList<String>();
		for (int i = 0; i < elements.size(); i++) {
			GraphPatternElement gpe = elements.get(i);
			vars = getVarsInGraphPatternElement(vars, gpe);
		}
		if (vars.size() > 0) {
			if (query.getVariables() == null) {
				query.setVariables(vars);
			} else {
				query.getVariables().addAll(vars);
			}
		}
	}

	private List<String> getVarsInGraphPatternElement(List<String> vars,
			GraphPatternElement gpe) {
		if (gpe instanceof TripleElement) {
			Node subj = ((TripleElement) gpe).getSubject();
			Node obj = ((TripleElement) gpe).getObject();
			if (subj instanceof VariableNode
					&& !vars.contains(((VariableNode) subj).getName())) {
				vars.add(((VariableNode) subj).getName());
			}
			if (obj instanceof VariableNode
					&& !vars.contains(((VariableNode) obj).getName())) {
				vars.add(((VariableNode) obj).getName());
			}
		} else if (gpe instanceof Junction) {
			vars = getVarsInGraphPatternElement(vars,
					(GraphPatternElement) ((Junction) gpe).getLhs());
			vars = getVarsInGraphPatternElement(vars,
					(GraphPatternElement) ((Junction) gpe).getRhs());
		}
		return vars;
	}

	public boolean compareObjects(Object lval, Object objVal,
			OntResource rngrsrc) {
		if ((lval instanceof KnownNode && objVal != null)
				|| (objVal instanceof KnownNode && lval != null)) {
			return true;
		}
		if (lval instanceof String && objVal instanceof String
				&& lval.equals(objVal)) {
			return true;
		} else if (lval instanceof Boolean && objVal instanceof Boolean) {
			if (lval.equals(objVal)) {
				return true;
			} else {
				return false;
			}
		} else if (lval instanceof Number && objVal instanceof Number) {
			if (lval.equals(objVal)) {
				return true;
			} else {
				double d1 = ((Number) lval).doubleValue();
				double d2 = ((Number) objVal).doubleValue();
				// double diff = d1 - d2;
				// if (Math.abs((double)diff / Math.max(Math.max(d1, d2),
				// .0000001)) < .0000001) {
				return ResultSet.areDoublesEqual(d1, d2); // true;
				// }
			}
		} else if (lval != null && objVal != null && lval.equals(objVal)) {
			return true;
		} else if (rngrsrc != null) {
			// we know the range so let's try to convert each value to the
			// desired value type and compare
			Literal v1 = valueToJenaLiteral(lval, rngrsrc.getURI());
//					getTheJenaModel().createTypedLiteral(lval,
//					rngrsrc.getURI());
			Literal v2 = valueToJenaLiteral(objVal, rngrsrc.getURI());
//					getTheJenaModel().createTypedLiteral(objVal,
//					rngrsrc.getURI());
			if (v1 != null && v2 != null && v1.equals(v2)) {
				return true;
			}
			if (rngrsrc.equals(XSD.date)
					&& v1.getDatatypeURI().equals(XSD.date.getURI())
					&& v2.getDatatypeURI().equals(XSD.date.getURI())) {
				Object v1obj = v1.getValue();
				Object v2obj = v2.getValue();
				if (v1obj instanceof XSDDateTime
						&& v2obj instanceof XSDDateTime
						&& ((XSDDateTime) v1obj).getYears() == ((XSDDateTime) v2obj)
								.getYears()
						&& ((XSDDateTime) v1obj).getMonths() == ((XSDDateTime) v2obj)
								.getMonths()
						&& ((XSDDateTime) v1obj).getDays() == ((XSDDateTime) v2obj)
								.getDays()) {
					return true;
				}
			}
		}
		return false;
	}

	abstract TestResult testTriple(TripleElement triple) throws TripleNotFoundException, InvalidTypeException, ConfigurationException, ReasonerNotFoundException;

	abstract ResultSet processQuery(Query obj) throws ConfigurationException, ReasonerNotFoundException, TranslationException, InvalidNameException, JenaProcessorException, QueryParseException, QueryCancelledException;

	abstract Literal valueToJenaLiteral(Object objVal, String uri);

	abstract String sadlLiteralToString(com.ge.research.sadl.model.gp.Literal objval);

	abstract ResultSet matchTriplePattern(String fullyQualifiedString, String fullyQualifiedString2, String strObjVal) throws TripleNotFoundException, ConfigurationException, ReasonerNotFoundException;
}