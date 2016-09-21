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
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.SadlCommand;
import com.ge.research.sadl.processing.IModelProcessor;
import com.ge.research.sadl.processing.ISadlInferenceProcessor;
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

	@Override
	public Object[] runInference(Resource resource, String owlModelPath, String modelFolderPath, Map<String,String> prefMap) throws SadlInferenceException {
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
			Object[] results = new Object[2];
			List<String> errors = new ArrayList<String>();
			errors.add("No commands to process. Inference complete.");
			results[1] = errors;
			return results;
		}
		setModelFolderPath(modelFolderPath);
		setCurrentResource(resource);
		try {
			setModelName(getConfigMgr(null).getPublicUriFromActualUrl(new SadlUtils().fileNameToFileUrl(owlModelPath)));
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (URISyntaxException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		setTheJenaModel(OntModelProvider.find(resource));
		Object[] results = new Object[2];
		List<String> infresults = new ArrayList<String>();
		results[0] = infresults;
		Iterator<SadlCommand> tpitr = cmds.iterator();
		while (tpitr.hasNext()) {
			SadlCommand cmd = tpitr.next();
			try {
				if (cmd instanceof Query) {
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
						results[0] = rs;
//					}
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

	public ResultSet processAdhocQuery(String query) {
		String queryString;
		
		String _repoType = ConfigurationManager.RDF_XML_ABBREV_FORMAT; // default
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
			ITranslator translator = getConfigMgr(_repoType).getTranslator();
			IReasoner reasoner = getConfigMgr(_repoType).getReasoner();
			if (!reasoner.isInitialized()) {
				reasoner.setConfigurationManager(getConfigMgr(_repoType));
				reasoner.initializeReasoner(getModelFolderPath(), getModelName(), _repoType);
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
				Statement s = query.getProperty(m.getProperty(JenaBasedSadlModelProcessor.SADL_IMPLICIT_MODEL_QUERY_STRING_URI));
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
