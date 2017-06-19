package com.ge.research.sadl.jena;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.importer.ITabularDataImporter;
import com.ge.research.sadl.importer.TemplateException;
import com.ge.research.sadl.model.Explanation;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.EndWrite;
import com.ge.research.sadl.model.gp.Explain;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.KnownNode;
import com.ge.research.sadl.model.gp.Literal;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.Print;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Read;
import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.model.gp.StartWrite;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.model.gp.Test.ComparisonType;
import com.ge.research.sadl.model.gp.TestResult;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.ValueTableNode;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.model.gp.Junction.JunctionType;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.IModelProcessor;
import com.ge.research.sadl.processing.ISadlInferenceProcessor;
import com.ge.research.sadl.processing.OntModelProvider;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SadlInferenceException;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.processing.ValidationAcceptorImpl;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.ModelError;
import com.ge.research.sadl.reasoner.ModelError.ErrorType;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.SadlCommandResult;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.TripleNotFoundException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.SadlModel;
import com.google.inject.Inject;
import com.hp.hpl.jena.datatypes.xsd.XSDDateTime;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.vocabulary.XSD;

public class JenaBasedSadlInferenceProcessor implements ISadlInferenceProcessor {
	private static final Logger logger = LoggerFactory.getLogger(JenaBasedSadlInferenceProcessor.class);
	private Resource currentResource;
	private OntModel theJenaModel;
	private String modelName;
	private String modelFolderPath;

	/**
	 * This enum is for internal use to determine how to display values in
	 * TestResult on fail
	 */
	private enum DisplayType {
		ValueTableNodeDisplay, LiteralDisplay
	}

	@Inject
	private IPreferenceValuesProvider preferenceProvider;
	private IConfigurationManagerForIDE configMgr;
	private Map<String, String> preferenceMap;
	private String _repoType = null;
	private int testCnt;

	@Override
	public Object[] runInference(Resource resource, String owlModelPath, String modelFolderPath, Map<String,String> prefMap) throws SadlInferenceException {
		try {
			setModelFolderPath(modelFolderPath);
			setCurrentResource(resource);
			setModelName(getConfigMgr(getOwlFormat()).getPublicUriFromActualUrl(new SadlUtils().fileNameToFileUrl(owlModelPath)));
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (URISyntaxException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		// clear old reasoner
		try {
			getConfigMgr(getOwlFormat()).clearReasoner();
		} catch (ConfigurationException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}
		preferenceMap = prefMap;
		OntModel om = OntModelProvider.find(resource);
		if (om == null) {
			// This should never happen as the SadlActionHandler makes a call to validate the Resource. However, not deleting this code yet... AWC 9/21/2016
			SadlModelProcessor modelProcessor = new JenaBasedSadlModelProcessor();
			final List<Issue> issues = CollectionLiterals.<Issue>newArrayList();
			final IAcceptor<Issue> _function = new IAcceptor<Issue>() {
				@Override
				public void accept(final Issue it) {
					issues.add(it);
				}
			};
			ValidationAcceptor _validationAcceptor = new ValidationAcceptorImpl(_function);
			IPreferenceValues _preferenceValues = this.preferenceProvider.getPreferenceValues(resource);
			IModelProcessor.ProcessorContext _processorContext = new IModelProcessor.ProcessorContext(CancelIndicator.NullImpl, _preferenceValues);
			modelProcessor.onValidate(resource,_validationAcceptor,  CheckMode.FAST_ONLY, _processorContext);
			om = OntModelProvider.find(resource);
			if (om == null) {
				throw new SadlInferenceException("Unable to find OWL model for Resource '" + resource.getURI().toString() + "'");
			}
		}
		List<SadlCommand> cmds = OntModelProvider.getSadlCommands(resource);
		if (cmds == null || cmds.size() < 1) {
			Object[] results = new Object[3];
			List<String> errors = new ArrayList<String>();
			errors.add("No commands to process. Inference complete.");
			results[2] = errors;
			return results;
		}
		setTheJenaModel(OntModelProvider.find(resource));
		
		try {
			checkIfExplanationNeeded(cmds);
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (ReasonerNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
			
		StartWrite writeInEffect = null;
		StringBuilder writeAccumulator = null;
		List<SadlCommandResult> results = new ArrayList<SadlCommandResult>();
		for (int cmdIndex = 0; cmdIndex < cmds.size(); cmdIndex++) {
			SadlCommand cmd =cmds.get(cmdIndex);
			try {
				if (cmd instanceof Explain) {
					results.add(processExplain((Explain) cmd));
				}
				else if (cmd instanceof Print) {
					results.add(processPrint((Print) cmd));
				}
				else if (cmd instanceof Query) {
					results.add(processAdhocQuery((Query) cmd));
				}
				else if (cmd instanceof Read) {
					results.add(processRead((Read)cmd));
				}
				else if (cmd instanceof Test) {
					results.add(processTest((Test)cmd));
				}
				else if (cmd instanceof StartWrite) {
					writeInEffect = (StartWrite) cmd;
					if (writeAccumulator == null) {
						writeAccumulator = new StringBuilder();
					}
					else if (writeAccumulator.length() > 0){
						writeAccumulator.delete(0, writeAccumulator.length());
					}
				}
				else if (cmd instanceof EndWrite) {
					String ofn = ((EndWrite)cmd).getOutputFilename(); 
					File of = new File(ofn); 
					if (of.getParentFile() != null && of.getParentFile().exists()
							&& of.getParentFile().isDirectory()) {
						// this must be an absolute path
					}
					else {
						// assume a relative path
						File modelFolderFile = new File(getModelFolderPath());
						String prjDir = modelFolderFile.getParent();
						String filepath = prjDir + File.separator + ofn;
						of = new File(filepath);
					}
					getConfigMgr(getOwlFormat()).getSadlUtils().stringToFile(of, writeAccumulator.toString(), false);
					writeInEffect = null;
					SadlCommandResult result = new SadlCommandResult(cmd);
					result.setResults(new SadlUtils().fileNameToFileUrl(of.getCanonicalPath()));
					results.add(result);
				}
			} catch (Throwable t) {
				results.add(convertCmdExceptionToSadlCommandError(cmd, t));
			} 
		}
		return results.toArray(new SadlCommandResult[results.size()]);
	}
	
	protected ITabularDataImporter getTabularDataImporter(IConfigurationManagerForEditing configMgr) {
		String tabularImporterClass = preferenceMap.get(SadlPreferences.TABULAR_DATA_IMPORTER_CLASS.getId());
		
		List<ITabularDataImporter> importers = configMgr.getAvailableTabularImporters();
	
		if (importers != null && importers.size() > 0) {
			ITabularDataImporter importer = importers.get(0);		// replace this by selection and setting preference
			return importer;
		}
		return null;
	}

	private SadlCommandResult processRead(Read cmd) throws ConfigurationException, IOException, TemplateException, URISyntaxException, InvalidNameException, ReasonerNotFoundException {
		SadlCommandResult result = new SadlCommandResult(cmd);
		String ifn = ((Read)cmd).getInputFilename();
		String tfn = ((Read)cmd).getTemplateFilename();
		File inFile = getConfigMgr(getOwlFormat()).resolveFilename(ifn);
		if (inFile == null) {
			String msg = "Failed to find Read file '" + ifn + ".\n";
			addErrorToSadlCommand(result, msg, ErrorType.ERROR);
			return result;
		}
		if (tfn != null) {		// this is a mapped read with template
			File tf = getConfigMgr(getOwlFormat()).resolveFilename(tfn);
			if (tf == null) {
				String msg = "Failed to find Template file '" + tfn + "'.\n";
				addErrorToSadlCommand(result, msg, ErrorType.ERROR);
				return result;
			}
			else {
				ITabularDataImporter importer = getTabularDataImporter(getConfigMgr(getOwlFormat()));
				if (importer == null) {
					String msg = "Failed to find an importer implementing ITabularDataImporter.\n";
					addErrorToSadlCommand(result, msg, ErrorType.ERROR);
					return result;
				}
				importer.setImportFilename(inFile.getAbsolutePath(), true);
				importer.setModelFolder(getConfigMgr(getOwlFormat()).getModelFolder());
//				importer.setImportModelNamespace(defaultInstanceDataNS);		
//				Object[] o = scanTemplateForImports(csvTemplateString);
//				int numImports = 0;
//				if (o != null && o.length > 0) {
//					if (o[1] != null && o[1] instanceof List && ((List)o[1]).size() > 0) {
//						List<String> templateImports = (List<String>) o[1];
//						importer.setImports(templateImports);
//						numImports = templateImports.size();
//					}
//					else {
//						importer.setImports(getModelName());
//						numImports = 1;
//					}
//					String processedTemplate = (String) o[0];
//					importer.setTemplates(processedTemplate);
//					logger.info("Scanned template statistics: Original size = "+csvTemplateString.length()+
//							"  Processed size = "+processedTemplate.length()+
//							"  Number of imports = "+numImports);
//				}
				importer.setTemplates(getConfigMgr(getOwlFormat()).getSadlUtils().fileNameToFileUrl(tf.getAbsolutePath()));
				Object om;
				try {
					om = importer.getOwlModel();
					if (logger.isDebugEnabled()) {
						ByteArrayOutputStream baos = new ByteArrayOutputStream();
						if (om instanceof OntModel) {
							((OntModel) om).write(baos, "N3");
						}
						logger.debug(baos.toString());
					}
					getInitializedReasoner().loadInstanceData(om);
				} catch (ReasonerNotFoundException e) {
					throw new ConfigurationException(e.getMessage(), e);
				}
				
			}
		}
		else {	// this is a straight read of an input file
			getInitializedReasoner().loadInstanceData(inFile.getAbsolutePath());						
		}
		return null;
	}

	private void addErrorToSadlCommand(SadlCommandResult scr, String msg, ErrorType errorType) {
		ModelError me = new ModelError(msg, errorType);
		List<ModelError> errors = new ArrayList<ModelError>();
		errors.add(me);
		scr.setErrors(errors);
	}

	private SadlCommandResult convertCmdExceptionToSadlCommandError(SadlCommand cmd, Throwable t) {
		SadlCommandResult scr = new SadlCommandResult(cmd);
		String msg;
		ErrorType etype;
		if (t instanceof QueryCancelledException) {
			msg = "Query cancelled by user";
			etype = ErrorType.INFO;
		}
		else {
			msg = t.getMessage();
			etype = ErrorType.ERROR;
		}
		addErrorToSadlCommand(scr, msg, etype);
		return scr;
	}

	private void checkIfExplanationNeeded(List<SadlCommand> cmds) throws ConfigurationException, ReasonerNotFoundException {
		if (cmds != null) {
			for (int i = 0; i < cmds.size(); i++) {
				SadlCommand cmd = cmds.get(i);
				if (cmd instanceof Explain) {
					IReasoner reasoner = getInitializedReasoner();
					reasoner.enableExplanation(true);
					break;
				}
			}
		}
		
	}

	private String getOwlFormat() {
		if (_repoType == null) {
			_repoType = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
			if (preferenceMap != null) {
				if (preferenceMap.containsKey(SadlPreferences.OWL_MODEL_FORMAT.getId())) {
					_repoType = preferenceMap.get(SadlPreferences.OWL_MODEL_FORMAT.getId());
				}
			}
		}
		return _repoType;
	}

	private SadlCommandResult processTest(Test cmd) throws TranslationException, ConfigurationException, ReasonerNotFoundException {
		testCnt ++;
		SadlCommandResult result = new SadlCommandResult(cmd);
		TestResult testResult = null;
		Object lhs = ((Test) cmd).getLhs();
		Object rhs = ((Test) cmd).getRhs();
		try {
		if (lhs == null || rhs == null) {
			// this is just a pass (true) or fail (false) test,
			// not a comparison
			if (lhs instanceof Query) {
					ResultSet rs = processAdhocQuery(getConfigMgr(getOwlFormat()).getTranslator(), (Query) lhs);
//				addError(new ModelError("Error: this Query case is not implemented!", ErrorType.ERROR));
			} else {
				TripleElement triple = null;
				if (lhs instanceof TripleElement) {
					triple = (TripleElement) lhs;
				} else if (rhs instanceof TripleElement) {
					triple = (TripleElement) rhs;
				}
				if (triple != null) {
					testResult = testTriple(getInitializedReasoner(), triple);
				} else if (lhs instanceof List<?>
						&& ((List<?>) lhs).size() == 2) {
					testResult = testFilteredQuery(getInitializedReasoner(),
							(List<?>) lhs);
				} else if (rhs instanceof List<?>
						&& ((List<?>) rhs).size() == 2) {
					testResult = testFilteredQuery(getInitializedReasoner(),
							(List<?>) rhs);
				} else if (lhs instanceof List<?>
						&& rhs == null) {
					Object lhobj = convertToComparableObject(
							getModelFolderPath(), getInitializedReasoner(),
							lhs, ((Test) cmd).getLhsVariables());
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
			Object lhobj = convertToComparableObject(
					getModelFolderPath(), getInitializedReasoner(),
					((Test) cmd).getLhs(),
					((Test) cmd).getLhsVariables());
			Object rhobj = convertToComparableObject(
					getModelFolderPath(), getInitializedReasoner(),
					((Test) cmd).getRhs(),
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
//		if (testResult == null) {
//			String msg = "Test result is null. This should not happen. Test is: "
//					+ cmd.toString();
//			getMessageManager().error(
//					msg,
//					getMessageManager().new HyperlinkInfo(
//							getModelActualUrl().toFileString(),
//							cmd.getLineNo(), cmd.getOffset(),
//							cmd.getLength(), 13, -1));
//			if (writeInEffect != null && !writeInEffect.isDataOnly()) {
//				writeAccumulator.append(msg);
//			}
//		} else if (testResult.isPassed()) {
//			testsPassed++;
//			String msg = "Test passed: " + cmd.toString()
//					+ "\n";
//			getMessageManager().info(
//					msg,
//					getMessageManager().new HyperlinkInfo(
//							getModelActualUrl().toFileString(),
//							cmd.getLineNo(), cmd.getOffset(),
//							cmd.getLength(), 13, -1));
//			if (writeInEffect != null && !writeInEffect.isDataOnly()) {
//				writeAccumulator.append(msg);
//			}
//		} else {
//			String msg = "Test failed: " + cmd.toString()
//					+ "\n";
//			if (testResult.getMsg() != null) {
//				msg += "    " + testResult.getMsg() + "\n";
//			}
//			getMessageManager().error(
//					msg,
//					getMessageManager().new HyperlinkInfo(
//							getModelActualUrl().toFileString(),
//							cmd.getLineNo(), cmd.getOffset(),
//							cmd.getLength(), 13, -1));
//			getMessageManager().error(
//					"    " + testResult.toString((Test) cmd)
//							+ "\n");
//			if (writeInEffect != null && !writeInEffect.isDataOnly()) {
//				writeAccumulator.append(msg);
//			}
//		}
//	}
//} catch (
//		private void addError(ModelError modelError) {
//		// TODO Auto-generated method stub
//		
//	}
//TranslationException e) {
//	getMessageManager().error(
//			"Translation error: " + e.getLocalizedMessage()
//					+ "\n",
//			getMessageManager().new HyperlinkInfo(
//					getModelActualUrl().toFileString(), cmd
//							.getLineNo(), cmd.getOffset(), cmd
//							.getLength()));
//} catch (QueryParseException e) {
//	getMessageManager().error(
//			"Query parse error: " + e.getLocalizedMessage()
//					+ "\n",
//			getMessageManager().new HyperlinkInfo(
//					getModelActualUrl().toFileString(), cmd
//							.getLineNo(), cmd.getOffset(), cmd
//							.getLength()));
//} catch (QueryCancelledException e) {
//	getMessageManager().info(e.getLocalizedMessage(),
//	getMessageManager().new HyperlinkInfo(
//			getModelActualUrl().toFileString(), cmd
//					.getLineNo(), cmd.getOffset(), cmd
//					.getLength()));
//	cancelled++;
//} catch (InferenceCanceledException e) {
//	getMessageManager().info(e.getLocalizedMessage());
//	break;
//} catch (TripleNotFoundException e) {
//	getMessageManager().error(
//			"Query execution error: " + e.getLocalizedMessage()
//					+ "\n",
//			getMessageManager().new HyperlinkInfo(
//					getModelActualUrl().toFileString(), cmd
//							.getLineNo(), cmd.getOffset(), cmd
//							.getLength()));
//} catch (InvalidNameException e) {
//	getMessageManager().error(
//			"Query execution error: " + e.getLocalizedMessage()
//					+ "\n",
//			getMessageManager().new HyperlinkInfo(
//					getModelActualUrl().toFileString(), cmd
//							.getLineNo(), cmd.getOffset(), cmd
//							.getLength()));
//} catch (Throwable t
//	// catch anything inside the for so that only one test is
//	// "lost"
//	t.printStackTrace();
//}
		
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TranslationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvalidNameException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ReasonerNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (QueryParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (QueryCancelledException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TripleNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvalidTypeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		result.setResults(testResult);
		result.setErrors(getInitializedReasoner().getErrors());
		return result;
	}

	private TestResult testTriple(IReasoner reasoner, TripleElement triple)
			throws TripleNotFoundException, InvalidTypeException {
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
			ResultSet rs = reasoner.ask(subject, predicate, object);
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

	private TestResult testFilteredQuery(IReasoner reasoner, List<?> lst)
			throws QueryParseException {
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
			String queryStr = getConfigMgr(getOwlFormat()).getTranslator()
					.translateQuery(getTheJenaModel(), newQuery);
			logger.debug("Translated query: " + queryStr);
			ResultSet lhResultSet = reasoner.ask(queryStr);
			if (lhResultSet != null) {
				return new TestResult(true);
			}
			TestResult tr = new TestResult(false);
			tr.setMsg("Test query '" + queryStr + "' did not return a value");
			return tr;
		} catch (Exception e) {
			TestResult tr = new TestResult(false);
			tr.setMsg("Encountered error evaluating test: "
					+ e.getLocalizedMessage());
			return tr;
		}
	}

	private boolean compareValues(Node node, Object objVal, OntResource rngrsrc) {
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

	private boolean compareObjects(Object lval, Object objVal,
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
			com.hp.hpl.jena.rdf.model.Literal v1 = getTheJenaModel().createTypedLiteral(lval,
					rngrsrc.getURI());
			com.hp.hpl.jena.rdf.model.Literal v2 = getTheJenaModel().createTypedLiteral(objVal,
					rngrsrc.getURI());
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

	private Object convertToComparableObject(String knowledgeBaseIdentifier,
			IReasoner reasoner, Object obj, List<String> testVars)
			throws TranslationException, ConfigurationException,
			QueryParseException, TripleNotFoundException, QueryCancelledException {
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
		} 
		else if (obj instanceof Junction) {
			if (((Junction)obj).getJunctionType().equals(JunctionType.Conj)) {
				Object lhs = ((Junction)obj).getLhs();
				Object rhs = ((Junction)obj).getRhs();
				Query newQuery = new Query();
				if (lhs instanceof GraphPatternElement && rhs instanceof GraphPatternElement) {
					newQuery.addPattern((GraphPatternElement) lhs);
					newQuery.addPattern((GraphPatternElement) rhs);
					obj = newQuery;
				}
				else {
					throw new TranslationException("Conversion of disjunction encountered unexpected non-GraphPatternElement content.");
				}
			}
			else {
				throw new TranslationException("Conversion of disjunction to a comparable object not currently supported.");
			}
		}
		else if (obj instanceof List<?>
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
				if (isSparqlQuery((String)litObj)) {
					Query tmpQuery = new Query();
					tmpQuery.setSparqlQueryString(litObj.toString());
					try {
						String queryStr = getConfigMgr(getOwlFormat()).getTranslator()
								.translateQuery(getTheJenaModel(), tmpQuery);
						logger.debug("Found SPARQL query as literal: "
								+ queryStr);
						obj = reasoner.ask(queryStr);
					} catch (InvalidNameException e) {
						e.printStackTrace();
						throw new TranslationException(
								"Unable to prepare SPARQL query", e);
					}
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
				String queryStr = null;
				try {
					queryStr = getConfigMgr(getOwlFormat()).getTranslator()
							.translateQuery(getTheJenaModel(), (Query) obj);
					logger.debug("Translated query: " + queryStr);
					ResultSet lhResultSet = reasoner.ask(queryStr);
					obj = lhResultSet;
				} catch (InvalidNameException e) {
					throw new TranslationException("Translation failed: ", e);
				}
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
							strObjVal = getTheJenaModel()
									.createTypedLiteral(
											((com.ge.research.sadl.model.gp.Literal) objval)
													.getValue()).toString();
						} else {
							strObjVal = ((NamedNode) objval)
									.toFullyQualifiedString();
						}
						ResultSet results = reasoner.ask(
								((NamedNode) subj).toFullyQualifiedString(),
								((NamedNode) pred).toFullyQualifiedString(),
								strObjVal);
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
	
	public static boolean isSparqlQuery(String litObj) {
		litObj = litObj.trim();
		SadlUtils su = new SadlUtils();
		litObj = su.stripQuotes(litObj);
		litObj = litObj.trim();
		if (litObj.toLowerCase().indexOf("where") > 0 &&
				(( litObj.toLowerCase().indexOf("select ") == 0 && ((String) litObj).indexOf("?") > 0) ||
				litObj.toLowerCase().indexOf("construct ") == 0)) {
			return true;
		}
		else if (litObj.length() > 4){
			int askidx = litObj.indexOf("ask ");
			int opidx = litObj.indexOf('{');
			int cpidx = litObj.indexOf('}');	
			if (askidx == 0 && opidx > 3 && cpidx > opidx) {
				return true;
			}
		}
		return false;
	}

	private void setVariablesFromPatterns(Query query) {
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

	private TestResult doTestComparison(Object lhobj, Object rhobj,
			ComparisonType type) {
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

	private TestResult createTestFailed(Object lhobj, Object lhval,
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
	
	private DisplayType determineDisplayType(Object lobj, Object robj,
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

	private Object toComparableObject(Object obj) {
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

	private SadlCommandResult processPrint(Print cmd) throws FileNotFoundException, ConfigurationException, ReasonerNotFoundException, TranslationException, URISyntaxException {
		String dispStr = cmd.getDisplayString();
		SadlCommandResult result = new SadlCommandResult(cmd);
		if (dispStr != null) {
			result.setResults(dispStr);
		}
		else {
			String model = cmd.getModel();
			String outfn = getPrintModelFileName(model);
			getInitializedReasoner().saveInferredModel(
					outfn, modelName,
					(model.equals("Model") ? false
							: true));
			result.setResults(new SadlUtils().fileNameToFileUrl(outfn));
			result.setErrors(getInitializedReasoner().getErrors());
		}
		return result;
		
	}

	private SadlCommandResult processExplain(Explain cmd) throws TranslationException, ConfigurationException, ReasonerNotFoundException {
		SadlCommandResult result = new SadlCommandResult(cmd);
		List<Explanation> explanations = null;
		if (cmd.getRuleName() != null) {
			explanations = getInitializedReasoner().explain(cmd.getRuleName());
		}
		else if (cmd.getPatterns() != null) {
			explanations = getInitializedReasoner().explain(cmd.getPatterns());
		}
		result.setResults(explanations);
		result.setErrors(getInitializedReasoner().getErrors());
		return result;
	}

	private String getPrintModelFileName(String model) {
		URI mfp = URI.createURI(modelFolderPath);
		mfp = mfp.trimSegments(1);
		mfp = mfp.appendSegment("Temp");
		mfp = mfp.appendSegment(getCurrentResource().getURI().lastSegment()+ "." + model + ".owl.txt");
		return mfp.toString();
	}

	private SadlCommandResult processAdhocQuery(Query cmd) throws ConfigurationException, JenaProcessorException, TranslationException, InvalidNameException, ReasonerNotFoundException, QueryParseException, QueryCancelledException {
		String queryString;
		String query = cmd.getSparqlQueryString();
		if (query == null) {
			query = getConfigMgr(null).getTranslator().translateQuery(getTheJenaModel(), cmd);
		}
		query = SadlUtils.stripQuotes(query);
		if (getTheJenaModel() == null) {
			// it always is?
			XtextResource xtrsrc = (XtextResource) getCurrentResource().getContents().get(0).eResource();
			if (xtrsrc != null) {
				URI resourceUri = xtrsrc.getURI();
				OntModel ontModel = OntModelProvider.find(xtrsrc);
				if (ontModel != null) {
					setTheJenaModel(ontModel);
				}
			}
			if (getTheJenaModel() == null) {
				// TODO
				// load from OWL file
			}
		}
		ITranslator translator = getConfigMgr(getOwlFormat()).getTranslator();
		Query q = processQuery(query);
		SadlCommandResult result = new SadlCommandResult(cmd);
		result.setResults(processAdhocQuery(translator, q));
		result.setErrors(getInitializedReasoner().getErrors());
		return result;
	}
	
	private ResultSet processAdhocQuery(ITranslator translator, Query q) throws ConfigurationException, TranslationException, InvalidNameException, ReasonerNotFoundException, QueryParseException, QueryCancelledException {
		String queryString = translator.translateQuery(getTheJenaModel(), q);
		if (queryString == null && q.getSparqlQueryString() != null) {
			queryString = q.getSparqlQueryString();
		}
		IReasoner reasoner = getInitializedReasoner();
		queryString = reasoner.prepareQuery(queryString);
		ResultSet results =  reasoner.ask(queryString);
		return results;
	}

	private IReasoner getInitializedReasoner() throws ConfigurationException, ReasonerNotFoundException {
		IReasoner reasoner = getConfigMgr(getOwlFormat()).getReasoner();
		if (!reasoner.isInitialized()) {
			reasoner.setConfigurationManager(getConfigMgr(getOwlFormat()));
			reasoner.initializeReasoner(getModelFolderPath(), getModelName(), getOwlFormat());
		}
		return reasoner;
	}

	private Query processQuery(Object qobj) throws JenaProcessorException {
		String qstr;
		if (qobj instanceof com.ge.research.sadl.model.gp.Literal) {
			qstr = ((com.ge.research.sadl.model.gp.Literal)qobj).getValue().toString();
		}
		else if (qobj instanceof String) {
			qstr = qobj.toString();
		}
		else {
			throw new JenaProcessorException("Unexpected query type: " + qobj.getClass().getCanonicalName());
		}
		Query q = new Query();
		q.setSparqlQueryString(qstr);
		return q;
	}
	
	public ResultSet processNamedQuery(String queryName) throws JenaProcessorException {
		SadlModel model = (SadlModel) getCurrentResource().getContents().get(0);
		if (model != null) {
			URI resourceUri = getCurrentResource().getURI();
			XtextResource xtrsrc = (XtextResource) model.eResource();
			if (xtrsrc != null) {
				URI importedResourceUri = xtrsrc.getURI();
				OntModel m = OntModelProvider.find(xtrsrc);
				if (m == null) {
					throw new JenaProcessorException("Unable to retrieve OntModel for resource '" + resourceUri + "'");
				}
				OntResource query = m.getOntResource(queryName);
				Statement s = query.getProperty(m.getProperty(SadlConstants.SADL_IMPLICIT_MODEL_QUERY_STRING_URI));
// TODO what happens to s?				
			}
		}
		return null;
	}
	
	private IConfigurationManagerForIDE getConfigMgr(String format) throws ConfigurationException {
		if (configMgr == null) {
			if (format == null) {
				format = ConfigurationManager.RDF_XML_ABBREV_FORMAT; // default
			}
			if ((getModelFolderPath() == null && 
					getCurrentResource().getURI().toString().startsWith("synthetic")) ||
							getCurrentResource().getURI().toString().startsWith(SadlModelProcessor.SYNTHETIC_FROM_TEST)) {
				configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(getModelFolderPath(), format, true);
			}
			else {
				configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(getModelFolderPath() , format);
			}
		}
		return configMgr;
	}

	private Resource getCurrentResource() {
		return currentResource;
	}

	private void setCurrentResource(Resource currentResource) {
		this.currentResource = currentResource;
	}

	protected OntModel getTheJenaModel() {
		return theJenaModel;
	}

	protected void setTheJenaModel(OntModel theJenaModel) {
		this.theJenaModel = theJenaModel;
	}

	private String getModelFolderPath() {
		return modelFolderPath;
	}

	private void setModelFolderPath(String modelFolderPath) {
		this.modelFolderPath = modelFolderPath;
	}

	private String getModelName() {
		if (modelName == null) {
			
		}
		return modelName;
	}

	private void setModelName(String modelName) {
		this.modelName = modelName;
	}
	
}
