package com.ge.research.sadl.jena;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Iterator;
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

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.model.gp.EndWrite;
import com.ge.research.sadl.model.gp.Explain;
import com.ge.research.sadl.model.gp.Print;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.Read;
import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.model.gp.StartWrite;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.IModelProcessor;
import com.ge.research.sadl.processing.ISadlInferenceProcessor;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SadlInferenceException;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.SadlModel;
import com.google.inject.Inject;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.rdf.model.Statement;

public class JenaBasedSadlInferenceProcessor implements ISadlInferenceProcessor {
	private Resource currentResource;
	private OntModel theJenaModel;
	private String modelName;
	private String modelFolderPath;

	@Inject
	private IPreferenceValuesProvider preferenceProvider;
	private IConfigurationManagerForIDE configMgr;
	private Map<String, String> preferenceMap;
	private String _repoType = null;

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
			JenaBasedSadlModelProcessor modelProcessor = new JenaBasedSadlModelProcessor();
			final List<Issue> issues = CollectionLiterals.<Issue>newArrayList();
			final IAcceptor<Issue> _function = new IAcceptor<Issue>() {
				@Override
				public void accept(final Issue it) {
					issues.add(it);
				}
			};
			ValidationAcceptor _validationAcceptor = new ValidationAcceptor(_function);
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
		}
			
		Object[] results = new Object[3];	// [0] = commands, [1] = inference results, [2] = errors
		results[0] = cmds;
		List<Object> infresults = new ArrayList<Object>();
		results[1] = infresults;
		Iterator<SadlCommand> tpitr = cmds.iterator();
		while (tpitr.hasNext()) {
			SadlCommand cmd = tpitr.next();
			try {
				if (cmd instanceof Explain) {
					
				}
				else if (cmd instanceof Print) {
					results =  processPrint(cmds, (Print) cmd, results);
				}
				else if (cmd instanceof Query) {
					String query = ((Query)cmd).getSparqlQueryString();
					if (query == null) {
						query = getConfigMgr(null).getTranslator().translateQuery(getTheJenaModel(), (Query) cmd);
					}
					query = SadlUtils.stripQuotes(query);
//					if (useKSever) {
////						getKServer(resource).query(query);
//					}
//					else {
						ResultSet rs = processAdhocQuery(query);
						infresults.add(rs);
//					}
				}
				else if (cmd instanceof Read) {
					
				}
				else if (cmd instanceof Test) {
					results = processTest(cmds, (Test)cmd, results);
				}
				else if (cmd instanceof StartWrite) {
					
				}
				else if (cmd instanceof EndWrite) {
					
				}
			} catch (TranslationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvalidNameException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} //catch (QueryCancelledException e) {
				// OK to cancel--no error
				//issueAcceptor.addInfo("Query cancelled by user", (EObject) cmd.getContext());
			//} catch (QueryParseException e) {
				// TODO Auto-generated catch block
				//e.printStackTrace();
			//} catch (ReasonerNotFoundException e) {
				// TODO Auto-generated catch block
				//e.printStackTrace();
			//} //catch (SessionNotFoundException e) {
				// TODO Auto-generated catch block
				//e.printStackTrace();
			//}
		}
		return results;
	}

	private void checkIfExplanationNeeded(List<SadlCommand> cmds) throws ConfigurationException {
		if (cmds != null) {
			for (int i = 0; i < cmds.size(); i++) {
				SadlCommand cmd = cmds.get(i);
				if (cmd instanceof Explain) {
					IReasoner reasoner = getConfigMgr(getOwlFormat()).getReasoner();
					reasoner.enableExplanation(true);
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

	private Object[] processTest(List<SadlCommand> cmds, Test cmd, Object[] results) {
		
		return results;
	}

	private Object[] processPrint(List<SadlCommand> cmds, Print cmd, Object[] results) {
		String dispStr = cmd.getDisplayString();
		if (dispStr != null) {
			results[1] = dispStr;
		}
		else {
			String model = cmd.getModel();
			String outfn = getPrintModelFileName(model);
//			msg = "Printing "
//					+ ((Print) cmd).getModel();
//			reasoner.saveInferredModel(
//					outputfilename, modelName,
//					(type.equals("Model") ? false
//							: true));

		}
		return results;
		
	}

	private String getPrintModelFileName(String model) {
		URI mfp = URI.createURI(modelFolderPath);
		mfp = mfp.trimSegments(1);
		mfp = mfp.appendSegment(getCurrentResource().getURI().lastSegment()+ "." + model + ".owl");
		return mfp.toString();
	}

	public ResultSet processAdhocQuery(String query) {
		String queryString;
		
		try {
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
			IReasoner reasoner = getConfigMgr(getOwlFormat()).getReasoner();
			if (!reasoner.isInitialized()) {
				reasoner.setConfigurationManager(getConfigMgr(getOwlFormat()));
				reasoner.initializeReasoner(getModelFolderPath(), getModelName(), getOwlFormat());
			}
			queryString = translator.translateQuery(getTheJenaModel(), processQuery(query));
			ResultSet results =  reasoner.ask(queryString);
			return results;
		} catch (com.ge.research.sadl.reasoner.ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TranslationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvalidNameException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (QueryParseException e) {
			System.err.println("Query parse exception: " + e.getMessage());
		} catch (QueryCancelledException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ReasonerNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (JenaProcessorException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
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
							getCurrentResource().getURI().toString().startsWith(JenaBasedSadlModelProcessor.SYNTHETIC_FROM_TEST)) {
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
