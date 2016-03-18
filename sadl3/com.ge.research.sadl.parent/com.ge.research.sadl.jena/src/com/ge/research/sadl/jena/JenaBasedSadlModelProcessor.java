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

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.builder.EclipseResourceFileSystemAccess2;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.preferences.PreferenceKey;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.util.CancelIndicator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.model.ModelError;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.gp.BuiltinElement;
import com.ge.research.sadl.model.gp.BuiltinElement.BuiltinType;
import com.ge.research.sadl.model.gp.Equation;
import com.ge.research.sadl.model.gp.GraphPatternElement;
import com.ge.research.sadl.model.gp.Junction;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.model.gp.Node;
import com.ge.research.sadl.model.gp.ProxyNode;
import com.ge.research.sadl.model.gp.Query;
import com.ge.research.sadl.model.gp.RDFTypeNode;
import com.ge.research.sadl.model.gp.Rule;
import com.ge.research.sadl.model.gp.Test;
import com.ge.research.sadl.model.gp.TripleElement;
import com.ge.research.sadl.model.gp.TripleElement.TripleModifierType;
import com.ge.research.sadl.model.gp.TripleElement.TripleSourceType;
import com.ge.research.sadl.model.gp.VariableNode;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.ConfigurationManagerFactory;
import com.ge.research.sadl.reasoner.ITranslator;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.InvalidTypeException;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.BinaryOperation;
import com.ge.research.sadl.sADL.BooleanLiteral;
import com.ge.research.sadl.sADL.Constant;
import com.ge.research.sadl.sADL.Declaration;
import com.ge.research.sadl.sADL.EndWriteStatement;
import com.ge.research.sadl.sADL.EquationStatement;
import com.ge.research.sadl.sADL.ExplainStatement;
import com.ge.research.sadl.sADL.Expression;
import com.ge.research.sadl.sADL.Function;
import com.ge.research.sadl.sADL.Name;
import com.ge.research.sadl.sADL.NumberLiteral;
import com.ge.research.sadl.sADL.PrintStatement;
import com.ge.research.sadl.sADL.PropOfSubject;
import com.ge.research.sadl.sADL.ReadStatement;
import com.ge.research.sadl.sADL.RuleStatement;
import com.ge.research.sadl.sADL.SADLPackage;
import com.ge.research.sadl.sADL.SadlAllValuesCondition;
import com.ge.research.sadl.sADL.SadlAnnotation;
import com.ge.research.sadl.sADL.SadlBooleanLiteral;
import com.ge.research.sadl.sADL.SadlCardinalityCondition;
import com.ge.research.sadl.sADL.SadlClassOrPropertyDeclaration;
import com.ge.research.sadl.sADL.SadlCondition;
import com.ge.research.sadl.sADL.SadlConstantLiteral;
import com.ge.research.sadl.sADL.SadlDataType;
import com.ge.research.sadl.sADL.SadlDataTypeFacet;
import com.ge.research.sadl.sADL.SadlDifferentFrom;
import com.ge.research.sadl.sADL.SadlDisjointClasses;
import com.ge.research.sadl.sADL.SadlExplicitValue;
import com.ge.research.sadl.sADL.SadlHasValueCondition;
import com.ge.research.sadl.sADL.SadlImport;
import com.ge.research.sadl.sADL.SadlInstance;
import com.ge.research.sadl.sADL.SadlIntersectionType;
import com.ge.research.sadl.sADL.SadlIsAnnotation;
import com.ge.research.sadl.sADL.SadlIsTransitive;
import com.ge.research.sadl.sADL.SadlModel;
import com.ge.research.sadl.sADL.SadlModelElement;
import com.ge.research.sadl.sADL.SadlNecessaryAndSufficient;
import com.ge.research.sadl.sADL.SadlNumberLiteral;
import com.ge.research.sadl.sADL.SadlParameterDeclaration;
import com.ge.research.sadl.sADL.SadlPrimitiveDataType;
import com.ge.research.sadl.sADL.SadlProperty;
import com.ge.research.sadl.sADL.SadlPropertyCondition;
import com.ge.research.sadl.sADL.SadlPropertyInitializer;
import com.ge.research.sadl.sADL.SadlPropertyRestriction;
import com.ge.research.sadl.sADL.SadlRangeRestriction;
import com.ge.research.sadl.sADL.SadlResource;
import com.ge.research.sadl.sADL.SadlSameAs;
import com.ge.research.sadl.sADL.SadlSimpleTypeReference;
import com.ge.research.sadl.sADL.SadlStringLiteral;
import com.ge.research.sadl.sADL.SadlTypeAssociation;
import com.ge.research.sadl.sADL.SadlTypeReference;
import com.ge.research.sadl.sADL.SadlUnionType;
import com.ge.research.sadl.sADL.SadlValueList;
import com.ge.research.sadl.sADL.StartWriteStatement;
import com.ge.research.sadl.sADL.StringLiteral;
import com.ge.research.sadl.sADL.SubjHasProp;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.inject.Inject;
import com.hp.hpl.jena.ontology.AllValuesFromRestriction;
import com.hp.hpl.jena.ontology.AnnotationProperty;
import com.hp.hpl.jena.ontology.CardinalityRestriction;
import com.hp.hpl.jena.ontology.ComplementClass;
import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.HasValueRestriction;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.IntersectionClass;
import com.hp.hpl.jena.ontology.MaxCardinalityRestriction;
import com.hp.hpl.jena.ontology.MinCardinalityRestriction;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.ontology.Restriction;
import com.hp.hpl.jena.ontology.SomeValuesFromRestriction;
import com.hp.hpl.jena.ontology.UnionClass;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFList;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.RDFWriter;
import com.hp.hpl.jena.rdf.model.ResourceFactory;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.OWL2;
import com.hp.hpl.jena.vocabulary.RDFS;
import com.hp.hpl.jena.vocabulary.XSD;

public class JenaBasedSadlModelProcessor extends SadlModelProcessor {
	private static final Logger logger = LoggerFactory.getLogger(JenaBasedSadlModelProcessor.class);

    public final static String XSDNS = XSD.getURI();

    public final static Property xsdProperty( String local )
        { return ResourceFactory.createProperty( XSDNS + local ); }

	private OntModel theJenaModel;
	private OntModelSpec spec;
	
	enum AnnType {ALIAS, NOTE}
	public enum RangeValueType {CLASS_OR_DT, LIST, LISTS}
	private int vNum = 0;	// used to create unique variables
	private List<String> userDefinedVariables = new ArrayList<String>();
	
	@Inject
	public DeclarationExtensions declarationExtensions;
	private String modelName;
	private String modelAlias;
	private String modelNamespace;
	private OntDocumentManager jenaDocumentMgr;
	private static final String LIST_RANGE_ANNOTATION_PROPERTY = "http://sadl.org/range/annotation/listtype";
	
	private ValidationAcceptor issueAcceptor = null;
	private CancelIndicator cancelIndicator = null;

	private List<String> importsInOrderOfAppearance = null;	// an ordered set of import URIs, ordered by appearance in file.
	private List<Rule> rules = null;
	private List<Equation> equations = null;
	
	public JenaBasedSadlModelProcessor() {
		logger.debug("New " + this.getClass().getCanonicalName() + "' created");
	}
	/**
	 * For TESTING
	 * @return
	 */
	public OntModel getTheJenaModel() {
		return theJenaModel;
	}
	
	@SuppressWarnings("restriction")
	@Override
	public void onGenerate(Resource resource, IFileSystemAccess2 fsa, ProcessorContext context) {
		// save the model
		if (getTheJenaModel() == null) {
			// it always is?
			onValidate(resource, null, context);
		}
		if (fsa !=null) {
			IPreferenceValues testPrefs = context.getPreferenceValues();
			String baseuri = context.getPreferenceValues().getPreference(SadlPreferences.SADL_BASE_URI);
			String _repoType = ConfigurationManager.RDF_XML_ABBREV_FORMAT; // default
			String pv = context.getPreferenceValues().getPreference(SadlPreferences.RDF_XML_FORMAT); 
			pv = context.getPreferenceValues().getPreference(SadlPreferences.RDF_XML_ABBREV_FORMAT);
			pv = context.getPreferenceValues().getPreference(SadlPreferences.N_TRIPLE_FORMAT);
			pv = context.getPreferenceValues().getPreference(SadlPreferences.N3_FORMAT);
			pv = context.getPreferenceValues().getPreference(SadlPreferences.JENA_TDB);

			logger.debug("Base SADL URI is: " + baseuri); 

			// Output the OWL file for the ontology model
			URI lastSeg = fsa.getURI(resource.getURI().lastSegment());
			String owlFN = lastSeg.trimFileExtension().appendFileExtension("owl").lastSegment().toString();
			String format = "RDF/XML-ABBREV";
			RDFWriter w = getTheJenaModel().getWriter(format);
			w.setProperty("xmlbase",getModelName());
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			w.write(getTheJenaModel(), out, getModelName());
			Charset charset = Charset.forName("UTF-8"); 
			CharSequence seq = new String(out.toByteArray(), charset);
			fsa.generateFile(owlFN, seq);
			
			// Output the ont-policy.rdf mapping file: the mapping will have been updated already via onValidate
			String pfileContent = fsa.readTextFile(UtilsForJena.ONT_POLICY_FILENAME).toString();
			URI actUrl = fsa.getURI(owlFN);
			//TODO this has to be converted to actual url? (or just save relative?)
			List<String> segs = actUrl.segmentsList();
			String altUrl = "";
			for (int i = 2; i < segs.size(); i++) {
				if (i > 2) {
					altUrl += "/";
				}
				altUrl += segs.get(i);
			}
			String revisedContent;
			try {
				revisedContent = new UtilsForJena().addMappingToPolicyFile(pfileContent, getModelName(), altUrl, getModelAlias(), "SADL");
				fsa.generateFile(UtilsForJena.ONT_POLICY_FILENAME, revisedContent);
			} catch (JenaProcessorException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			// Output the Rules and any other knowledge structures via the specified translator
			URI prjuri = ResourceManager.getProjectUri(resource);
			URI mdlfldruri = prjuri.appendSegment(ResourceManager.OWLDIR);
			try {
				String modelFolderPathname;
				if (mdlfldruri.isPlatform()) {
					if (mdlfldruri.isFile()) {
						modelFolderPathname = mdlfldruri.toFileString();
					}
					else {
					    String platformString = mdlfldruri.toPlatformString(true);
					    IPath myFile = ResourcesPlugin.getWorkspace().getRoot().getLocation();
					    modelFolderPathname = myFile.append(platformString).toOSString();
					}
				}
				else {
					modelFolderPathname = new SadlUtils().fileUrlToFileName(mdlfldruri.toString());				
				}
				IConfigurationManagerForIDE configMgr = new ConfigurationManagerForIDE(modelFolderPathname , _repoType);
				ITranslator translator = configMgr.getTranslator();
				List<ModelError> results = translator
						.translateAndSaveModel(getTheJenaModel(), rules,
								modelFolderPathname, getModelName(), getImportsInOrderOfAppearance(), 
								owlFN);
				if (results != null) {
					modelErrorsToOutput(resource, results);
				}
				else if (getOtherKnowledgeStructure() != null) {
					results = translator.translateAndSaveModelWithOtherStructure(getTheJenaModel(), getOtherKnowledgeStructure(), 
							modelFolderPathname, getModelName(), getImportsInOrderOfAppearance(), owlFN);
					if (results != null) {
						modelErrorsToOutput(resource, results);
					}
				}
			} catch (MalformedURLException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (com.ge.research.sadl.reasoner.ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (TranslationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			
		}
	}
	
	private Object getOtherKnowledgeStructure() {
		if (getEquations() != null) {
			return getEquations();
		}
		return null;
	}
	private void modelErrorsToOutput(Resource resource, List<ModelError> errors) {
		for (int i = 0; errors != null && i < errors.size(); i++) {
			ModelError err = errors.get(i);
			addError(err.getErrorMsg(), resource.getContents().get(0));
		}
	}
	
	/**
	 * Method to retrieve a list of the model's imports ordered according to appearance
	 * @return
	 */
	public List<String> getImportsInOrderOfAppearance() {
		return importsInOrderOfAppearance;
	}

	private void addOrderedImport(String importUri) {
		if (importsInOrderOfAppearance == null) {
			importsInOrderOfAppearance = new ArrayList<String>();
		}
		if (!importsInOrderOfAppearance.contains(importUri)) {
			importsInOrderOfAppearance.add(importUri);
		}
		
	}
	@Override
	public void onValidate(Resource resource, ValidationAcceptor issueAcceptor, ProcessorContext context) {
		setIssueAcceptor(issueAcceptor);
		setCancelIndicator(cancelIndicator);
		if (resource.getContents().size() < 1) {
			return;
		}
		SadlModel model = (SadlModel) resource.getContents().get(0);
		String modelActualUrl =resource.getURI().lastSegment();
		// directly create the Jena Model here!
//		theJenaModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		String modelName = model.getBaseUri();
		setModelName(modelName);
		setModelNamespace(assureNamespaceEndsWithHash(modelName));
		setModelAlias(model.getAlias());
		if (getModelAlias() == null) {
			setModelAlias("");
		}
		
		try {
			UtilsForJena ufj = new UtilsForJena();
			String policyFileUrl = ufj.getPolicyFilename(resource);
			String policyFilename = policyFileUrl != null ? ufj.fileUrlToFileName(policyFileUrl) : null;
			theJenaModel = ufj.createAndInitializeJenaModel(policyFilename, OntModelSpec.OWL_MEM, true);
			OntDocumentManager owlDocMgr = getTheJenaModel().getDocumentManager();
			setSpec(getTheJenaModel().getSpecification());
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			addError(e1.getMessage(), model);
			return; // this is a fatal error
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			addError(e1.getMessage(), model);
			return; // this is a fatal error
		} catch (URISyntaxException e) {
			e.printStackTrace();
			addError(e.getMessage(), model);
			return; // this is a fatal error
		} catch (JenaProcessorException e) {	// this isn't fatal??
			e.printStackTrace();
			addError(e.getMessage(), model);
		}
		getTheJenaModel().setNsPrefix(getModelAlias(), getModelNamespace());
		Ontology modelOntology = getTheJenaModel().createOntology(modelName);
		logger.debug("Ontology '" + modelName + "' created");
		modelOntology.addComment("This ontology was created from a SADL file '"
				+ modelActualUrl + "' and should not be directly edited.", "en");
		
		String modelVersion = model.getVersion();
		if (modelVersion != null) {
			modelOntology.addVersionInfo(modelVersion);
		}

		EList<SadlAnnotation> anns = model.getAnnotations();
		Iterator<SadlAnnotation> iter = anns.iterator();
		while (iter.hasNext()) {
			SadlAnnotation ann = iter.next();
			String anntype = ann.getType();
			EList<String> annContents = ann.getContents();
			Iterator<String> anniter = annContents.iterator();
			while (anniter.hasNext()) {
				String annContent = anniter.next();
				if (anntype.equalsIgnoreCase(AnnType.ALIAS.toString())) {
					modelOntology.addLabel(annContent, "en");
				}
				else if (anntype.equalsIgnoreCase(AnnType.NOTE.toString())) {
					modelOntology.addComment(annContent, "en");
				}
			}
		}
		
		EList<SadlImport> implist = model.getImports();
		Iterator<SadlImport> impitr = implist.iterator();
		while (impitr.hasNext()) {
			SadlImport simport = impitr.next();
			SadlModel importedResource = simport.getImportedResource();
			if (importedResource != null) {
				String importUri = importedResource.getBaseUri();
				String importPrefix = simport.getAlias();
	//			theJenaModel.addImport(simport.getImportURI(), simport.getAlias());
		    	if (importUri != null) {
		    		if (importUri.equals(modelName)) {
			    		// don't import to self
		//	    		generate error marker--can't import self
			    	}
			    	else {
				    	// Now add import model (with setCachedModels false so that the actual imports are not loaded. If then need to be loaded 
			    		//	at some point to do semantic validation, they will be loaded then.
			    		if (importPrefix == null) {
		// TODO	    		// need to get the prefix from the global prefix in the imported model, if SADL, else from the ont-policy.rdf file if external
		//	    			OntologyModel importedModel;//  = simport.getImportedNamespace().getURI();	// this will not work until the grammar treats the import as a reference.
			    		}
			    		if (importPrefix != null) {
			    			getTheJenaModel().setNsPrefix(importPrefix, assureNamespaceEndsWithHash(importUri));
			    		}
				    	com.hp.hpl.jena.rdf.model.Resource importedOntology = getTheJenaModel().createResource(importUri);
				    	logger.debug("Imported ontology resource '" + importUri + "' created.");
				    	modelOntology.addImport(importedOntology);
				    	addOrderedImport(importUri);
			    	}
		    	}
			}
	    	
// TODO	Should the imported model actually be loaded by Jena? The OWL model, whether from SADL or external, will potenatially 
//	    	contain information that is necessary for validation. The only information that will be potentially needed for
//	    	processing the rest of the parse tree is the type of the imported concepts. For imports of SADL models, this type
//	    	information is known in the ResourceSet. Likewise for external OWL imports? 
	    	
//	    	this.getJenaDocumentMgr().setCacheModels(true);
//	   		this.getJenaDocumentMgr().setProcessImports(true);
//	   		ReadFailureHandler rfh = this.getJenaDocumentMgr().getReadFailureHandler();
//	   		if (rfh instanceof SadlReadFailureHandler) {
//	   			((SadlReadFailureHandler)rfh).setSadlConfigMgr(this);
//	   		}
//	    	Conclusion: if checking is needed that requires Jena imported models, then it happens here. 
//	    		Otherwise it happens in the validator.
	    	theJenaModel.loadImports();
		}
		
		// process rest of parse tree
		List<SadlModelElement> elements = model.getElements();
		if (elements != null) {
			Iterator<SadlModelElement> elitr = elements.iterator();
			while (elitr.hasNext()) {
				// check for cancelation from time to time
				if (context.getCancelIndicator().isCanceled()) {
					throw new OperationCanceledException();
				}
				SadlModelElement element = elitr.next();
				try {
					if (element instanceof SadlClassOrPropertyDeclaration) {
						processSadlClassOrPropertyDeclaration((SadlClassOrPropertyDeclaration) element);	
					}
					else if (element instanceof SadlProperty) {
						processSadlProperty((SadlProperty) element);
					}
					else if (element instanceof SadlNecessaryAndSufficient) {
						processSadlNecessaryAndSufficient((SadlNecessaryAndSufficient)element);
					}
					else if (element instanceof SadlDifferentFrom) {
						processSadlDifferentFrom((SadlDifferentFrom)element);
					}
					else if (element instanceof SadlInstance) {
						processSadlInstance((SadlInstance) element);
					}
					else if (element instanceof SadlDisjointClasses) {
						processSadlDisjointClasses((SadlDisjointClasses)element);
					}
					else if (element instanceof SadlSameAs) {
						processSadlSameAs((SadlSameAs)element);
					}
					else if (element instanceof RuleStatement) {
						processStatement((RuleStatement)element);
					}
					else if (element instanceof EquationStatement) {
						processStatement((EquationStatement)element);
					}
					else if (element instanceof PrintStatement) {
						processStatement((PrintStatement)element);
					}
					else if (element instanceof ReadStatement) {
						processStatement((ReadStatement)element);
					}
					else if (element instanceof StartWriteStatement) {
						processStatement((StartWriteStatement)element);
					}
					else if (element instanceof EndWriteStatement) {
						processStatement((EndWriteStatement)element);
					}
					else if (element instanceof ExplainStatement) {
						processStatement((ExplainStatement)element);
					}
					else {
						throw new JenaProcessorException("onValidate for element of type '" + element.getClass().getCanonicalName() + "' not implemented");
					}
				}
				catch (JenaProcessorException e) {
					addError(e.getMessage(), element);
				} catch (InvalidNameException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (InvalidTypeException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (TranslationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
	}

	private void processStatement(ExplainStatement element) throws JenaProcessorException {
		throw new JenaProcessorException("Processing for " + element.getClass().getCanonicalName() + " not yet implmeneted");		
	}
	
	private void processStatement(EndWriteStatement element) throws JenaProcessorException {
		throw new JenaProcessorException("Processing for " + element.getClass().getCanonicalName() + " not yet implmeneted");		
	}
	
	private void processStatement(StartWriteStatement element) throws JenaProcessorException {
		throw new JenaProcessorException("Processing for " + element.getClass().getCanonicalName() + " not yet implmeneted");		
	}
	
	private void processStatement(ReadStatement element) throws JenaProcessorException {
		throw new JenaProcessorException("Processing for " + element.getClass().getCanonicalName() + " not yet implmeneted");		
	}
	
	private void processStatement(PrintStatement element) throws JenaProcessorException {
		throw new JenaProcessorException("Processing for " + element.getClass().getCanonicalName() + " not yet implmeneted");		
	}
	
	private void processStatement(EquationStatement element) throws JenaProcessorException, InvalidNameException, InvalidTypeException, TranslationException {
		SadlResource nm = element.getName();
		EList<SadlParameterDeclaration> params = element.getParameter();
		SadlTypeReference rtype = element.getReturnType();
		Expression bdy = element.getBody();
		Equation eq = new Equation(declarationExtensions.getConcreteName(nm));
		eq.setNamespace(declarationExtensions.getConceptNamespace(nm));
		Node rtnode = sadlTypeReferenceToNode(rtype);
		eq.setReturnType(rtnode);
		if (params != null && params.size() > 0) {
			List<Node> args = new ArrayList<Node>();
			List<Node> argtypes = new ArrayList<Node>();
			for (int i = 0; i < params.size(); i++) {
				SadlParameterDeclaration param = params.get(i);
				SadlResource pr = param.getName();
				Object pn = processExpression(pr);
				args.add((Node) pn);
				SadlTypeReference prtype = param.getType();
				Node prtnode = sadlTypeReferenceToNode(prtype); 
				argtypes.add(prtnode);
			}
			eq.setArguments(args);
			eq.setArgumentTypes(argtypes);
		}
		Object bdyobj = processExpression(bdy);
		if (bdyobj instanceof List<?>) {
			eq.setBody((List<GraphPatternElement>) bdyobj);
		}
		else if (bdyobj instanceof GraphPatternElement) {
			eq.addBodyElement((GraphPatternElement)bdyobj);
		}
		logger.debug("Equation: " + eq.toFullyQualifiedString());
		addEquation(eq);
	}
	
	private NamedNode sadlTypeReferenceToNode(SadlTypeReference rtype) throws JenaProcessorException {
		Object rtobj = sadlTypeReferenceToObject(rtype);
		if (rtobj instanceof com.hp.hpl.jena.rdf.model.Resource) {
			if (((com.hp.hpl.jena.rdf.model.Resource)rtobj).isURIResource()) {
				NamedNode rtnn = new NamedNode(((com.hp.hpl.jena.rdf.model.Resource)rtobj).getLocalName());
				rtnn.setNamespace(((com.hp.hpl.jena.rdf.model.Resource)rtobj).getNameSpace());
				return rtnn;
			}
			else {
				throw new JenaProcessorException("SadlTypeReference is not a URI resource");
			}
		}
		else {
			throw new JenaProcessorException("SadlTypeReference is a type (" + rtobj.getClass().getCanonicalName() + ") which is not yet handled");		
		}
	}
	
	private void addEquation(Equation eq) {
		if (equations == null) {
			equations = new ArrayList<Equation>();
		}
		equations.add(eq);
	}
	
	public List<Equation> getEquations() {
		return equations;
	}
	
	private void processStatement(RuleStatement element) throws InvalidNameException, InvalidTypeException, TranslationException {
		String ruleName = element.getName();
		EList<Expression> ifs = element.getIfs();
		EList<Expression> thens = element.getThens();
		for (int i = 0; ifs != null && i < ifs.size(); i++) {
			Expression expr = ifs.get(i);
			Object result = processExpression(expr);
			System.out.println("If expression: " + result);
		}
		for (int i = 0; thens != null && i < thens.size(); i++) {
			Expression expr = thens.get(i);
			Object result = processExpression(expr);
			System.out.println("Then expression: " + result);
		}
		int i = 0;
	}
	
	@Override
	public Object translate(Expression expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		return processExpression(expr);
	}
	
	public Object processExpression(final Expression expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (expr instanceof BinaryOperation) {
			return processExpression((BinaryOperation)expr);
		}
		else if (expr instanceof BooleanLiteral) {
			return processExpression((BooleanLiteral)expr);
		}
		else if (expr instanceof Constant) {
			return processExpression((Constant)expr);
		}
		else if (expr instanceof Declaration) {
			return processExpression((Declaration)expr);
		}
		else if (expr instanceof Name) {
			return processExpression((Name)expr);
		}
		else if (expr instanceof NumberLiteral) {
			return processExpression((NumberLiteral)expr);
		}
		else if (expr instanceof PropOfSubject) {
			return processExpression((PropOfSubject)expr);
		}
		else if (expr instanceof StringLiteral) {
			return processExpression((StringLiteral)expr);
		}
		else if (expr instanceof SubjHasProp) {
			return processExpression((SubjHasProp)expr);
		}
		else if (expr instanceof SadlResource) {
			return processExpression((SadlResource)expr);
		}
		else if (expr instanceof Function) {
			return processExpression((Function)expr);
		}
		else {
			System.err.println("Unhanded rule expression type: " + expr.getClass().getCanonicalName());
		}
		return null;
	}
	
	public Object processExpression(BinaryOperation expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		String op = expr.getOp();
		BuiltinType optype = BuiltinType.getType(op);
		
		Expression lexpr = expr.getLeft();
		Expression rexpr = expr.getRight();
		Object lobj = translate(lexpr);
		Object robj = translate(rexpr);
				
		if (optype == BuiltinType.Equal || optype == BuiltinType.NotEqual) {
			// If we're doing an assignment, we can simplify the pattern.
			Node assignedNode = null;
			Object pattern = null;
			if (rexpr instanceof Declaration) {
				TripleElement trel = new TripleElement((Node)lobj, new RDFTypeNode(), (Node)robj);
				return trel;
			}
			if (lobj instanceof NamedNode && !(lobj instanceof VariableNode) && hasCommonVariableSubject(robj)) {
				TripleElement trel = (TripleElement)robj;
				while (trel != null) {
					trel.setSubject((Node) lobj);
					trel = (TripleElement) trel.getNext();
				}
				return robj;
			}
			if ((lobj instanceof TripleElement || (lobj instanceof Literal && isSparqlQuery(((Literal)lobj).toString())))
					&& robj instanceof BuiltinElement) {
				if (isModifiedTriple(((BuiltinElement)robj).getFuncType())) {
					assignedNode = ((BuiltinElement)robj).getArguments().get(0);
					optype = ((BuiltinElement)robj).getFuncType();
					pattern = lobj;
				}
				else if (isComparisonBuiltin(((BuiltinElement)robj).getFuncName())) {
					if ( ((BuiltinElement)robj).getArguments().get(0) instanceof Literal) {
						((TripleElement)lobj).setObject(nodeCheck(robj));
						return lobj;
					}
					else {
						return createBinaryBuiltin(rexpr, ((BuiltinElement)robj).getFuncName(), lobj, ((BuiltinElement)robj).getArguments().get(0));
					}
				}
			}
			else if (lobj instanceof Node && robj instanceof TripleElement) {
				assignedNode = validateNode((Node) lobj);
				pattern = (TripleElement) robj;
			}
			else if (robj instanceof Node && lobj instanceof TripleElement) {
				assignedNode = validateNode((Node) robj);
				pattern = (TripleElement) lobj;
			}
			if (assignedNode != null && pattern != null) {
				// We're expressing the type of a named thing.
				if (pattern instanceof TripleElement && ((TripleElement)pattern).getSubject() == null) {
					if (isModifiedTripleViaBuitin(robj)) {
						optype = ((BuiltinElement)((TripleElement)pattern).getNext()).getFuncType();	
						((TripleElement)pattern).setNext(null);
					}
					((TripleElement)pattern).setSubject(assignedNode);
					if (optype != BuiltinType.Equal) {
						((TripleElement)pattern).setType(getTripleModifierType(optype));
					}
				}
				else if (pattern instanceof TripleElement && ((TripleElement)pattern).getObject() == null && 
						(((TripleElement)pattern).getSourceType().equals(TripleSourceType.PSnewV) 
								|| ((TripleElement)pattern).getSourceType().equals(TripleSourceType.PSV))) {
					if (isModifiedTripleViaBuitin(robj)) {
						optype = ((BuiltinElement)((TripleElement)pattern).getNext()).getFuncType();	
						((TripleElement)pattern).setNext(null);
					}
					((TripleElement)pattern).setObject(assignedNode);
					if (optype != BuiltinType.Equal) {
						((TripleElement)pattern).setType(getTripleModifierType(optype));
					}
				}
				else if (pattern instanceof TripleElement && ((TripleElement)pattern).getSourceType().equals(TripleSourceType.SPV)
						&& assignedNode instanceof NamedNode && getProxyWithNullSubject(((TripleElement)pattern)) != null) {
					TripleElement proxyFor = getProxyWithNullSubject(((TripleElement)pattern));
					assignNullSubjectInProxies(((TripleElement)pattern), proxyFor, assignedNode);
					if (optype != BuiltinType.Equal) {
						proxyFor.setType(getTripleModifierType(optype));
					}
				}
				else if (isModifiedTriple(optype) || 
						(optype.equals(BuiltinType.Equal) && pattern instanceof TripleElement && 
								(((TripleElement)pattern).getObject() == null || 
										((TripleElement)pattern).getObject() instanceof NamedNode ||
										((TripleElement)pattern).getObject() instanceof Literal))){
					if (pattern instanceof TripleElement && isModifiedTripleViaBuitin(robj)) {
						optype = ((BuiltinElement)((TripleElement)pattern).getNext()).getFuncType();
						((TripleElement)pattern).setObject(assignedNode);
						((TripleElement)pattern).setNext(null);
						((TripleElement)pattern).setType(getTripleModifierType(optype));
					}
					else if (isComparisonViaBuiltin(robj, lobj)) {
						BuiltinElement be = (BuiltinElement)((TripleElement)robj).getNext();
						be.addMissingArgument((Node) lobj);
						return pattern;
					}
					else if (pattern instanceof TripleElement){
						TripleElement lastPattern = (TripleElement)pattern;
						// this while may need additional conditions to narrow application to nested triples?
						while (lastPattern.getNext() != null && lastPattern instanceof TripleElement) {
							lastPattern = (TripleElement) lastPattern.getNext();
						}
						if (encapsulatingTarget instanceof Test) {
							((Test)encapsulatingTarget).setRhs(assignedNode);
							((Test)encapsulatingTarget).setCompName(optype);
						}
						else if (encapsulatingTarget instanceof Query && target instanceof Test) {
							((Test)target).setRhs(encapsulatingTarget);
							((Test)target).setLhs(assignedNode);
							((Test)target).setCompName(optype);
						}
						else if (target instanceof Test && assignedNode != null) {
							((Test)target).setLhs(pattern);
							((Test)target).setRhs(assignedNode);
							((Test)target).setCompName(optype);
							((TripleElement) pattern).setType(TripleModifierType.None);
							optype = BuiltinType.Equal;
						}
						else {
							lastPattern.setObject(assignedNode);
						}
						if (!optype.equals(BuiltinType.Equal)) {
							((TripleElement)pattern).setType(getTripleModifierType(optype));
						}
					}
					else {
						if (target instanceof Test) {
							((Test)target).setLhs(lobj);
							((Test)target).setRhs(assignedNode);
							((Test)target).setCompName(optype);
						}
					}
				}
				else if (encapsulatingTarget instanceof Test) {
					((Test)encapsulatingTarget).setRhs(assignedNode);
					((Test)encapsulatingTarget).setCompName(optype);
				}
				else if (target instanceof Rule && pattern instanceof TripleElement && ((TripleElement)pattern).getSourceType().equals(TripleSourceType.ITC) && 
						((TripleElement)pattern).getSubject() instanceof VariableNode && assignedNode instanceof VariableNode) {
					// in a rule of this type we just want to replace the pivot node variable
					doVariableSubstitution(((TripleElement)pattern), (VariableNode)((TripleElement)pattern).getSubject(), (VariableNode)assignedNode);
				}
				return pattern;
			}
			BuiltinElement bin = null;
			boolean binOnRight = false;
			Object retObj = null;
			if (lobj instanceof Node && robj instanceof BuiltinElement) {
				assignedNode = validateNode((Node)lobj);
				bin = (BuiltinElement)robj;
				retObj = robj;
				binOnRight = true;
			}
			else if (robj instanceof Node && lobj instanceof BuiltinElement) {
				assignedNode = validateNode((Node)robj);
				bin = (BuiltinElement)lobj;
				retObj = lobj;
				binOnRight = false;
			}
			if (bin != null && assignedNode != null) {
				if ((assignedNode instanceof VariableNode ||
					(assignedNode instanceof NamedNode && ((NamedNode)assignedNode).getNodeType().equals(NodeType.VariableNode)))) {
					while (bin.getNext() instanceof BuiltinElement) {
						bin = (BuiltinElement) bin.getNext();
					}
					if (bin.isCreatedFromInterval()) {
						bin.addArgument(0, assignedNode);
					}
					else {
						bin.addArgument(assignedNode);
					}
					return retObj;
				}
				else if (assignedNode instanceof Node && isComparisonBuiltin(bin.getFuncName())) {
					// this is a comparison with an extra "is"
					if (bin.getArguments().size() == 1) {
						if (bin.isCreatedFromInterval() || binOnRight) {
							bin.addArgument(0, assignedNode);
						}
						else {
							bin.addArgument(assignedNode);
						}
						return bin;
					}
				}
			}
			// We're describing a thing with a graph pattern.
			Set<VariableNode> vars = pattern instanceof TripleElement ? getSelectVariables(((TripleElement)pattern)) : null; 
			if (vars != null && vars.size() == 1) {
				// Find where the unbound variable occurred in the pattern
				// and replace each place with the assigned node.
				VariableNode var = vars.iterator().next();
				GraphPatternElement gpe = ((TripleElement)pattern);
				while (gpe instanceof TripleElement) {
					TripleElement triple = (TripleElement) gpe;
					if (var.equals(triple.getSubject())) {
						triple.setSubject(assignedNode);
					}
					if (var.equals(triple.getObject())) {
						triple.setObject(assignedNode);
					}
					gpe = gpe.getNext();
				}
				return pattern;
			}
		}
		// if we get to here we want to actually create a BuiltinElement for the BinaryOpExpression
		// However, if the type is equal ("is", "equal") and the left side is a VariableNode and the right side is a literal
		//	and the VariableNode hasn't already been bound, change from type equal to type assign.
		if (optype == BuiltinType.Equal && target instanceof Rule && lobj instanceof VariableNode && robj instanceof Literal && 
				!variableIsBound((Rule)target, null, (VariableNode)lobj)) {
			return createBinaryBuiltin(expr, "assign", robj, lobj);
		}
		return createBinaryBuiltin(expr, op, lobj, robj);
	}
	
	private Object processExpression(Function expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		EList<Expression> arglist = expr.getArglist();
		String funcname = expr.getCalled();
		BuiltinElement builtin = new BuiltinElement();
		builtin.setFuncName(funcname);
		if (arglist != null && arglist.size() > 0) {
			List<Object> args = new ArrayList<Object>();
			for (int i = 0; i < arglist.size(); i++) {
				args.add(processExpression(arglist.get(i)));
			}
			if (args != null) {
				for (Object arg : args) {
					builtin.addArgument(nodeCheck(arg));
					if (arg instanceof GraphPatternElement) {
						((GraphPatternElement)arg).setEmbedded(true);
					}
				}
			}
		}
		return builtin;
	}
	
	private boolean hasCommonVariableSubject(Object robj) {
		if (robj instanceof TripleElement && 
				(((TripleElement)robj).getSubject() instanceof VariableNode && 
						(((TripleElement)robj).getSourceType().equals(TripleSourceType.SPV)) ||
						((TripleElement)robj).getSourceType().equals(TripleSourceType.ITC))) {
			VariableNode subjvar = (VariableNode) ((TripleElement)robj).getSubject();
			Object trel = robj;
			while (trel != null && trel instanceof TripleElement) {
				if (!(trel instanceof TripleElement) || 
						(((TripleElement)trel).getSubject() != null &&!(((TripleElement)trel).getSubject().equals(subjvar)))) {
					return false;
				}
				trel = ((TripleElement)trel).getNext();
			}
			if (trel == null) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the bottom triple whose subject was replaced.
	 * @param pattern
	 * @param proxyFor
	 * @param assignedNode
	 * @return
	 */
	private TripleElement assignNullSubjectInProxies(TripleElement pattern,
			TripleElement proxyFor, Node assignedNode) {
		if (pattern.getSubject() instanceof ProxyNode) {
			Object proxy = ((ProxyNode)pattern.getSubject()).getProxyFor();
			if (proxy instanceof TripleElement) {
//				((ProxyNode)pattern.getSubject()).setReplacementNode(assignedNode);
				if (((TripleElement)proxy).getSubject() == null) {
					// this is the bottom of the recursion
					((TripleElement)proxy).setSubject(assignedNode);
					return (TripleElement) proxy;
				}
				else {
					// recurse down
					TripleElement bottom = assignNullSubjectInProxies(((TripleElement)proxy), proxyFor, assignedNode);
					// make the proxy next and reassign this subject as assignedNode
					((ProxyNode)((TripleElement)proxy).getSubject()).setReplacementNode(assignedNode);
					((TripleElement)proxy).setSubject(assignedNode);
					if (bottom.getNext() == null) {
						bottom.setNext(pattern);
					}
					return bottom;
				}
			}
		}
		return null;
	}

	private TripleElement getProxyWithNullSubject(TripleElement pattern) {
		if (pattern.getSubject() instanceof ProxyNode) {
			Object proxy = ((ProxyNode)pattern.getSubject()).getProxyFor();
			if (proxy instanceof TripleElement) {
				if (((TripleElement)proxy).getSubject() == null) {
					return (TripleElement)proxy;
				}
				else {
					return getProxyWithNullSubject(((TripleElement)proxy));
				}
			}
		}
		return null;
	}

	private boolean isComparisonViaBuiltin(Object robj, Object lobj) {
		if (robj instanceof TripleElement && lobj instanceof Node &&
				((TripleElement)robj).getNext() instanceof BuiltinElement) {
			BuiltinElement be = (BuiltinElement) ((TripleElement)robj).getNext();
			if (isComparisonBuiltin(be.getFuncName()) && be.getArguments().size() == 1) {
				return true;
			}
		}
		return false;
	}

	private boolean isModifiedTripleViaBuitin(Object robj) {
		if (robj instanceof TripleElement && ((TripleElement)robj).getNext() instanceof BuiltinElement) {
			BuiltinElement be = (BuiltinElement) ((TripleElement)robj).getNext();
			if (((TripleElement)robj).getPredicate() instanceof RDFTypeNode) {
				if (isModifiedTriple(be.getFuncType())) {
					Node subj = ((TripleElement)robj).getSubject();
					Node arg = (be.getArguments() != null && be.getArguments().size() > 0) ? be.getArguments().get(0) : null;
					if (subj == null && arg == null) {
						return true;
					}
					if (subj != null && arg != null && subj.equals(arg)) {
						return true;
					}
				}
			}
			else {
				if (isModifiedTriple(be.getFuncType()) && ((TripleElement)robj).getObject().equals(be.getArguments().get(0))) {
					return true;
				}
			}
		}
		return false;
	}

	private boolean doVariableSubstitution(GraphPatternElement gpe, VariableNode v1, VariableNode v2) {
		boolean retval = false;
		do {
			if (gpe instanceof TripleElement) {
				if (((TripleElement)gpe).getSubject().equals(v1)) {
					((TripleElement)gpe).setSubject(v2);
					retval = true;
				}
				else if (((TripleElement)gpe).getObject().equals(v1)) {
					((TripleElement)gpe).setObject(v2);
					retval = true;
				}
			}
			else if (gpe instanceof BuiltinElement) {
				List<Node> args = ((BuiltinElement)gpe).getArguments();
				for (int j = 0; j < args.size(); j++) {
					if (args.get(j).equals(v1)) {
						args.set(j, v2);
						retval = true;
					}
				}
			}
			else if (gpe instanceof Junction) {
				logger.error("Not yet handled");
			}
			gpe = gpe.getNext();
		} while (gpe != null);
		return retval;
	}

	/**
	 * This method returns true if the argument node is bound in some other element of the rule
	 * 
	 * @param rule
	 * @param gpe
	 * @param v
	 * @return
	 */
	public static boolean variableIsBound(Rule rule, GraphPatternElement gpe,
			Node v) {
		if (v instanceof NamedNode) {
			if (((NamedNode)v).getNodeType() != null && !(((NamedNode)v).getNodeType().equals(NodeType.VariableNode))) {
				return true;
			}
		}
		// Variable is bound if it appears in a triple or as the return argument of a built-in
		List<GraphPatternElement> givens = rule.getGivens();
		if (variableIsBoundInOtherElement(givens, 0, gpe, true, false, v)) {
			return true;
		}
		List<GraphPatternElement> ifs = rule.getIfs();
		if (variableIsBoundInOtherElement(ifs, 0, gpe, true, false, v)) {
			return true;
		}
		List<GraphPatternElement> thens = rule.getThens();
		if (variableIsBoundInOtherElement(thens, 0, gpe, false, true, v)) {
			return true;
		}
		return false;
	}

	private GraphPatternElement createBinaryBuiltin(Expression expr, String name, Object lobj, Object robj) throws InvalidNameException, InvalidTypeException, TranslationException {
		BuiltinElement builtin = new BuiltinElement();
		builtin.setFuncName(name);
		if (lobj != null) {
			builtin.addArgument(nodeCheck(lobj));
		}
		if (robj != null) {
			builtin.addArgument(nodeCheck(robj));
		}
		return builtin;
	}
	
	private Junction createJunction(Expression expr, String name, Object lobj, Object robj) {
		Junction junction = new Junction();
		junction.setJunctionName(name);
		junction.setLhs(lobj);
		junction.setRhs(robj);
		return junction;
	}

	private Object createUnaryBuiltin(Expression sexpr, String name, Object sobj) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (sobj instanceof com.ge.research.sadl.model.gp.Literal && BuiltinType.getType(name).equals(BuiltinType.Minus)) {
			Object theVal = ((com.ge.research.sadl.model.gp.Literal)sobj).getValue();
			if (theVal instanceof Integer) {
				theVal = ((Integer)theVal) * -1;
			}
			else if (theVal instanceof Long) {
				theVal = ((Long)theVal) * -1;
			}
			else if (theVal instanceof Float) {
				theVal = ((Float)theVal) * -1;
			}
			else if (theVal instanceof Double) {
				theVal = ((Double)theVal) * -1;
			}
			((com.ge.research.sadl.model.gp.Literal)sobj).setValue(theVal);
			((com.ge.research.sadl.model.gp.Literal)sobj).setOriginalText("-" + ((com.ge.research.sadl.model.gp.Literal)sobj).getOriginalText());
			return sobj;
		}
		if (sobj instanceof Junction) {
			// If the junction has two literal values, apply the op to both of them.
			Junction junc = (Junction) sobj;
			Object lhs = junc.getLhs();
			Object rhs = junc.getRhs();
			if (lhs instanceof com.ge.research.sadl.model.gp.Literal && rhs instanceof com.ge.research.sadl.model.gp.Literal) {
				lhs = createUnaryBuiltin(sexpr, name, lhs);
				rhs = createUnaryBuiltin(sexpr, name, rhs);
				junc.setLhs(lhs);
				junc.setRhs(rhs);
			}
			return junc;
		}
		if (BuiltinType.getType(name).equals(BuiltinType.Equal)) {
			if (sobj instanceof BuiltinElement) {
				if (isComparisonBuiltin(((BuiltinElement)sobj).getFuncName())) {
					// this is a "is <comparison>"--translates to <comparsion> (ignore is)
					return sobj;
				}
			}
			else if (sobj instanceof com.ge.research.sadl.model.gp.Literal || sobj instanceof NamedNode) {
				// an "=" interval value of a value is just the value
				return sobj;
			}
		}
		BuiltinElement builtin = new BuiltinElement();
		builtin.setFuncName(name);
		if (isModifiedTriple(builtin.getFuncType())) {
			if (sobj instanceof TripleElement) {
				((TripleElement)sobj).setType(getTripleModifierType(builtin.getFuncType()));
				return sobj;
			}
		}
		if (sobj != null) {
			builtin.addArgument(nodeCheck(sobj));
		}
		return builtin;
	}

	private Node nodeCheck(Object nodeObj) throws InvalidNameException, InvalidTypeException, TranslationException {
		if (nodeObj == null) {
//			throw new InvalidTypeException("nodeCheck called with null argument; this should not happen.");
			return null;
		}
		if (nodeObj instanceof Node) {
			return (Node) nodeObj; 
		}
		else if (nodeObj instanceof TripleElement) {
			if (((TripleElement)nodeObj).getPredicate() == null 
					&& ((TripleElement)nodeObj).getObject() == null
					&& ((TripleElement)nodeObj).getSubject() != null) {
				return ((TripleElement)nodeObj).getSubject();
			}
		}
		return new ProxyNode(nodeObj);
	}

	private TripleModifierType getTripleModifierType(BuiltinType btype) {
		if (btype.equals(BuiltinType.Not) || btype.equals(BuiltinType.NotEqual)) {
			return TripleModifierType.Not;
		}
		else if (btype.equals(BuiltinType.Only)) {
			return TripleModifierType.Only;
		}
		else if (btype.equals(BuiltinType.NotOnly)) {
			return TripleModifierType.NotOnly;
		}
		return null;
	}

	public String processExpression(BooleanLiteral expr) {
		System.out.println("processing " + expr.getClass().getCanonicalName() + ": " + expr.getValue());
		return expr.getValue();
	}
	
	public String processExpression(Constant expr) {
		System.out.println("processing " + expr.getClass().getCanonicalName() + ": " + expr.getConstant());
		return expr.getConstant();
	}
	
	public Object processExpression(Declaration expr) throws TranslationException {
		String nn = expr.getNewName();
		SadlResource type = expr.getType();
		Node typenode = processExpression(type);
		return typenode;
	}
	
	public Object processExpression(Name expr) throws TranslationException {
		SadlResource qnm =expr.getName();
		String nm = declarationExtensions.getConcreteName(qnm);
		if (nm == null) {
			SadlResource srnm = qnm.getName();
			if (srnm != null) {
				return processExpression(srnm);
			}
			addError("translate(Name) called with a SadlResource which resolved to null; this needs to be caught in validation", expr);
			nm = "nullvarname";
		}
		return processExpression(qnm);
	}
	
	private String getPrefix(String qn) {
		if (qn.contains(":")) {
			return qn.substring(0,qn.indexOf(":"));
		}
		return qn;
	}
	public String processExpression(NumberLiteral expr) {
		System.out.println("processing " + expr.getClass().getCanonicalName() + ": " + expr.getValue());
		return expr.getValue().toString();
	}
	
	public Object processExpression(PropOfSubject expr) throws InvalidNameException, InvalidTypeException, TranslationException {
		Expression predicate = expr.getLeft();
		Expression subject = expr.getRight();
		Node subjNode = null;
		Node predNode = null;
		if (predicate instanceof Constant) {
			// this is a pseudo PropOfSubject; the predicate is a constant
			String cnstval = ((Constant)predicate).getConstant();
			if (cnstval.equals("length")) {
			}
			else if (cnstval.equals("count")) {
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject)subject).getLeft();
					subject = ((PropOfSubject)subject).getRight();
				}
				Object elobj = translate(predicate);
			}
			else if (cnstval.equals("index")) {
				if (subject instanceof PropOfSubject) {
					predicate = ((PropOfSubject)subject).getLeft();
					subject = ((PropOfSubject)subject).getRight();
				}
				Object idxobj = translate(predicate);
			}
			else if (cnstval.equals("first element")) {
			}
			else if (cnstval.equals("last element")) {
			}
			else {
				System.err.println("Unhandled constant property in translate PropOfSubj: " + cnstval);
			}
		}
		else {
			Object predobj = translate(predicate);
			if(predobj instanceof Node) {
				predNode = (Node) predobj;
			}
			else {
				throw new TranslationException("Predicate '" + predobj.toString() + "' did not translate to Node");
			}
		}
		Object sn = translate(subject);
		if (sn instanceof Node) {
			subjNode = (Node) sn;
		}
		else {
			throw new TranslationException("Subject '" + sn.toString() + "' did not translate to Node");
		}
		TripleElement returnTriple = null;
		if (predNode != null) {
			returnTriple = new TripleElement(subjNode, predNode, null);
			returnTriple.setSourceType(TripleSourceType.PSV);
		}
		else {
			predNode = new RDFTypeNode();			
			Node variable = getVariableNode(expr, null, predNode, subjNode);
			returnTriple = new TripleElement();
			returnTriple.setSubject(variable);
			returnTriple.setPredicate(predNode);
			returnTriple.setObject(subjNode);
			if (subjNode instanceof NamedNode && !((NamedNode)subjNode).getNodeType().equals(NodeType.ClassNode)) {
				addError("'" + subjNode.toString() + "' is not a Class.", subject);
			}
			returnTriple.setSourceType(TripleSourceType.ITC);
		}

		return returnTriple;
	}
	
	public Node processExpression(SadlResource expr) throws TranslationException {
		String nm =  declarationExtensions.getConcreteName(expr);
		String ns = declarationExtensions.getConceptNamespace(expr);
		String prfx = declarationExtensions.getConceptPrefix(expr);
		OntConceptType type = declarationExtensions.getOntConceptType(expr);
		if (type.equals(OntConceptType.VARIABLE)) {
			VariableNode vn = new VariableNode(nm);
			vn.setNamespace(ns);
			vn.setPrefix(prfx);
			vn.setNodeType(ontConceptTypeToNodeType(type));
			return vn;
		}
		else {
			NamedNode n = new NamedNode(nm, ontConceptTypeToNodeType(type));
			n.setNamespace(ns);
			n.setPrefix(prfx);
			return n;
		}
	}
	
	public String processExpression(StringLiteral expr) {
		System.out.println("processing " + expr.getClass().getCanonicalName() + ": " + expr.getValue());
		return expr.getValue();
	}
	
	public String processExpression(SubjHasProp expr) {
		System.out.println("processing " + expr.getClass().getCanonicalName() + ": " + expr.getProp().toString());
		return expr.getProp().toString();
	}
	
	private void processSadlSameAs(SadlSameAs element) throws JenaProcessorException {
		SadlResource sr = element.getNameOrRef();
		String uri = declarationExtensions.getConceptUri(sr);
		OntResource rsrc = getTheJenaModel().getOntResource(uri);
		SadlTypeReference smas = element.getSameAs();
		OntConceptType sameAsType;
		if (rsrc == null) {
			// concept does not exist--try to get the type from the sameAs
			sameAsType = getSadlTypeReferenceType(smas);
			
		}
		else {
			sameAsType = declarationExtensions.getOntConceptType(sr);
		}
		if (sameAsType.equals(OntConceptType.CLASS)) {
			OntClass smasCls = sadlTypeReferenceToOntResource(smas).asClass();
			// this is a class axiom
			OntClass cls = getTheJenaModel().getOntClass(uri);
			if (cls == null) {
				// this is OK--create class
				cls = createOntClass(declarationExtensions.getConcreteName(sr), (String)null);
			}
			if (element.isComplement()) {
				ComplementClass cc = getTheJenaModel().createComplementClass(cls.getURI(), smasCls);
				logger.debug("New complement class '" + cls.getURI() + "' created");
			}
			else {
				cls.addEquivalentClass(smasCls);
				logger.debug("Class '" + cls.toString() + "' given equivalent class '" + smasCls.toString() + "'");
			}
		}
		else if (sameAsType.equals(OntConceptType.INSTANCE)) {
			OntResource smasInst = sadlTypeReferenceToOntResource(smas);
			rsrc.addSameAs(smasInst);
			logger.debug("Instance '" + rsrc.toString() + "' declared same as '" + smas.toString() + "'");
		}
		else {
			throw new JenaProcessorException("Unexpected concept type for same as statement: " + sameAsType.toString());
		}
	}

	private List<OntResource> processSadlClassOrPropertyDeclaration(SadlClassOrPropertyDeclaration element) throws JenaProcessorException {
		// Get the names of the declared concepts and store in a list
		List<String> newNames = new ArrayList<String>();
		EList<SadlResource> clses = element.getClassOrProperty();
		if (clses != null) {
			Iterator<SadlResource> citer = clses.iterator();
			while (citer.hasNext()) {
				SadlResource sr = citer.next();
				String nm = declarationExtensions.getConceptUri(sr);
				newNames.add(nm);
			}
		}
		
		if (newNames.size() < 1) {
			throw new JenaProcessorException("No names passed to processSadlClassOrPropertyDeclaration");
		}
		List<OntResource> rsrcList = new ArrayList<OntResource>();
		// The declared concept(s) will be of type class, property, or datatype. 
		//	Determining which will depend on the structure, including the superElement....
		// 	Get the superElement
		SadlTypeReference superElement = element.getSuperElement();
		//		1) if superElement is null then it is a top-level class declaration
		if (superElement == null) {
			OntClass cls = createOntClass(newNames.get(0), (OntClass)null);
			rsrcList.add(cls);
		}
		//  	2) if superElement is not null then the type of the new concept is the same as the type of the superElement
		// 			the superElement can be:
		// 				a) a SadlSimpleTypeReference
		else if (superElement instanceof SadlSimpleTypeReference) {
			SadlResource superSR = ((SadlSimpleTypeReference)superElement).getType();
			String superSRUri = declarationExtensions.getConceptUri(superSR);	
			OntConceptType superElementType = declarationExtensions.getOntConceptType(superSR);
			if (superElementType.equals(OntConceptType.CLASS)) {
				for (int i = 0; i < newNames.size(); i++) {
					rsrcList.add(createOntClass(newNames.get(i), superSRUri));
				}
			}
			else if (superElementType.equals(OntConceptType.CLASS_PROPERTY)) {
				for (int i = 0; i < newNames.size(); i++) {
					rsrcList.add(createObjectProperty(newNames.get(i), superSRUri));
				}
			}
			else if (superElementType.equals(OntConceptType.DATATYPE_PROPERTY)) {
				for (int i = 0; i < newNames.size(); i++) {
					rsrcList.add(createDatatypeProperty(newNames.get(i), superSRUri));
				}
			}
			else if (superElementType.equals(OntConceptType.ANNOTATION_PROPERTY)) {
				for (int i = 0; i < newNames.size(); i++) {
					rsrcList.add(createAnnotationProperty(newNames.get(i)));
				}
			}
		}
		//				b) a SadlPrimitiveDataType
		else if (superElement instanceof SadlPrimitiveDataType) {
			com.hp.hpl.jena.rdf.model.Resource spdt = processSadlPrimitiveDataType(element, (SadlPrimitiveDataType) superElement, newNames.get(0));
			if (spdt instanceof OntClass) {
				rsrcList.add((OntClass)spdt);
			}
			else if (spdt.canAs(OntResource.class)){
				rsrcList.add(spdt.as(OntResource.class));
			}
			else {
				throw new JenaProcessorException("Expected OntResource to be returned");  // .add(spdt);
			}
		}
		//				c) a SadlPropertyCondition
		else if (superElement instanceof SadlPropertyCondition) {
			OntClass propCond = processSadlPropertyCondition((SadlPropertyCondition) superElement);
			rsrcList.add(propCond);
		}
		//				d) a SadlTypeReference
		else if (superElement instanceof SadlTypeReference) {
			// this can only be a class; can't create a property as a SadlTypeReference
			OntClass superCls = sadlTypeReferenceToOntResource(superElement).asClass();
			if (superCls != null) {
				rsrcList.add(createOntClass(newNames.get(0), superCls));
			}
		}
		for (int i = 0; i < rsrcList.size(); i++) {
			Iterator<SadlProperty> dbiter = element.getDescribedBy().iterator();
			while (dbiter.hasNext()) {
				SadlProperty sp = dbiter.next();
				OntProperty prop = processSadlProperty(sp);
				addPropertyDomain(prop, rsrcList.get(i));
			}

		}
		return rsrcList;
	}

	private OntProperty processSadlProperty(SadlProperty element) throws JenaProcessorException {
		// this has two forms:
		//	1) <name> is a property...
		//	2) relationship of <Domain> to <Range> is <name>
		SadlResource sr = sadlResourceFromSadlProperty(element);
		String propUri = declarationExtensions.getConceptUri(sr);
		OntConceptType propType = declarationExtensions.getOntConceptType(sr);
		
		
		Iterator<SadlPropertyRestriction> spitr = element.getRestrictions().iterator();
		while (spitr.hasNext()) {
			SadlPropertyRestriction spr = spitr.next();
			if (spr instanceof SadlRangeRestriction) {
				RangeValueType rngValueType = RangeValueType.CLASS_OR_DT;	// default
				if (((SadlRangeRestriction)spr).isList()) {
					rngValueType = RangeValueType.LIST;
				}
				else if (((SadlRangeRestriction)spr).isLists()) {
					rngValueType = RangeValueType.LISTS;
				}
				SadlTypeReference rng = ((SadlRangeRestriction)spr).getRange();
				if (rng instanceof SadlPrimitiveDataType) {
					String rngName = ((SadlPrimitiveDataType)rng).getPrimitiveType().getName();
					RDFNode rngNode = primitiveDatatypeToRDFNode(rngName);
					DatatypeProperty prop = null;
					if (!checkForExistingCompatibleDatatypeProperty(propUri, rngNode)) {
						prop = createDatatypeProperty(propUri, null);
						addPropertyRange(prop, rngNode, rngValueType, rng);
					}
					else {
						prop = getTheJenaModel().getDatatypeProperty(propUri);
					}
					return prop;
				}
				else {				
					OntResource rngRsrc = sadlTypeReferenceToOntResource(rng);
					if (rngRsrc == null) {
						throw new JenaProcessorException("Range failed to resolve to a class or datatype");
					}
					if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
						OntClass rngCls = rngRsrc.asClass();
						ObjectProperty prop = getOrCreateObjectProperty(propUri);
						addPropertyRange(prop, rngCls, rngValueType, rng);
						return prop;
					}
					else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
						DatatypeProperty prop = getOrCreateDatatypeProperty(propUri);
						addPropertyRange(prop, rngRsrc, rngValueType, rng);
						return prop;
					}
					else {
						throw new JenaProcessorException("Processing of non-Ontology properpty not yet handled.");
					}
				}
			}
			else if (spr instanceof SadlCondition) {
				if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
					ObjectProperty prop = getOrCreateObjectProperty(propUri);
					OntClass condCls = processSadlCondition((SadlCondition) spr, prop, propType);
					addPropertyRange(prop, condCls, RangeValueType.CLASS_OR_DT, spr);		// use default?
					//TODO don't we need to add this class as superclass??
					return prop;
				}
				else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
					ObjectProperty prop = getOrCreateObjectProperty(propUri);
					OntClass condCls = processSadlCondition((SadlCondition) spr, prop, propType);
					//TODO don't we need to add this class as superclass??
					return prop;
//					throw new JenaProcessorException("SadlCondition on data type property not handled");
				}
				else {
					throw new JenaProcessorException("Invalid property type: " + propType.toString());
				}
			}
			else if (spr instanceof SadlTypeAssociation) {
				SadlTypeReference domain = ((SadlTypeAssociation)spr).getDomain();
				OntResource domainrsrc = sadlTypeReferenceToOntResource(domain);
				ObjectProperty prop = getOrCreateObjectProperty(propUri);
				addPropertyDomain(prop, domainrsrc);
				SadlTypeReference from = element.getFrom();
				if (from != null) {
					OntResource fromrsrc = sadlTypeReferenceToOntResource(from);
					throw new JenaProcessorException("What is 'from'?");
				}
				SadlTypeReference to = element.getTo();
				if (to != null) {
					OntResource torsrc = sadlTypeReferenceToOntResource(to);
					throw new JenaProcessorException("What is 'to'?");
				}
			}
			else if (spr instanceof SadlIsAnnotation) {
				return getTheJenaModel().createAnnotationProperty(propUri);
			}
			else if (spr instanceof SadlIsTransitive) {
				OntProperty pr = getOrCreateObjectProperty(propUri);
				pr.convertToTransitiveProperty();
				return getTheJenaModel().createTransitiveProperty(pr.getURI());
			}
			else {
				throw new JenaProcessorException("Unhandled SadlPropertyRestriction type: " + spr.getClass().getCanonicalName());
			}
		} // end while

		Iterator<SadlPropertyRestriction> itr = element.getRestrictions().iterator();
		if (itr.hasNext()) {
			while (itr.hasNext()) {
				SadlPropertyRestriction rest = itr.next();
				if (rest instanceof SadlIsAnnotation) {
					return createAnnotationProperty(propUri);
				}
			}
		}
		else {
			OntProperty ontProp = createObjectProperty(propUri, null);
			Iterator<SadlPropertyRestriction> restiter = element.getRestrictions().iterator();
			while (restiter.hasNext()) {
				throw new JenaProcessorException("Restricitons on object property in this context not yet handled");
			}
			return ontProp;
		}
		return null;
	}

	private SadlResource sadlResourceFromSadlProperty(SadlProperty element) {
		SadlResource sr = element.getNameOrRef();
		if (sr == null) {
			sr = element.getProperty();
		}
		if (sr == null) {
			sr = element.getNameDeclarations().iterator().next();
		}
		return sr;
	}

	private void addPropertyRange(OntProperty prop, RDFNode rngNode, RangeValueType rngValueType, EObject context) throws JenaProcessorException {
		OntResource existingRange = prop.getRange();
		if (existingRange != null) {
			if (rngNode.equals(existingRange)) {
				// do nothing
				return;
			}
			if (prop.isObjectProperty()) {
				// is the new range a subclass of the existing range, or vice versa?
				if (rngNode.isResource() && rngNode.asResource().canAs(OntClass.class)) {
					OntClass newRngCls = rngNode.asResource().as(OntClass.class);
					updateObjectPropertyRange(prop, newRngCls, rngValueType, context);
				}
				else {
					throw new JenaProcessorException("Unable to convert object property range to a class");
				}
			}
			else {
				// TODO issue warning--datatype property range mismatch	
				addError("Property '" + prop.getURI() + "' has range '" + existingRange.getURI() + "' so can't assign range '" + rngNode.toString(), context);
			}
		}
		else {
			prop.addRange(rngNode.asResource());
			addRangeListAnnotationToProperty(prop, rngValueType);
			if (logger.isDebugEnabled()) {
				StringBuffer sb = new StringBuffer();
				if (rngValueType.equals(RangeValueType.LIST)) {
					sb.append("List of values of type ");
				}
				else if (rngValueType.equals(RangeValueType.LISTS)) {
					sb.append("Lists of values of type ");
				}
				sb.append(rngNode.toString());
				logger.debug("Range '" + sb.toString() + "' given to property '" + prop.toString() + "'");
			}
		}
	}

//	private boolean updatePropertyDomain(int argIdx, OntProperty prop, OntResource domainCls) throws JenaProcessorException {
//		boolean retval = false;
//		OntResource existingDomain = prop.getDomain();
//		OntProperty testProp = getTheJenaModel().getOntProperty(prop.getURI());
//		if (testProp != null) {
//			OntResource testDomain = testProp.getDomain();
//			if (testDomain != null) {
//				existingDomain = testDomain;
//			}
//		}
//		if (domainCls != null) {
//			if (existingDomain != null && !existingDomain.equals(domainCls)) {
//				// there's already a domain specified
//				if (!prop.getNameSpace().equals(getModelNamespace())) {
//					// this is changing the domain of a property defined in a different model
//					if (classIsSubclassOf((OntClass) domainCls, existingDomain, true)) {
//						addWarning("The domain is a subclass of the domain of property '" + prop.getURI() + "' which is defined in an imported model; perhaps you mean an 'only has values of type' restricion?");						
//					}
//					else {
//						addWarning("This changes the domain of property '" + prop.getURI() + "' which is defined in an imported model; are you sure that is what you want to do?");
//					}
//				}
//				domainCls = addClassToUnionClass(existingDomain, (OntClass) domainCls);
//				if (!prop.getNameSpace().equals(getModelNamespace())) {
//					prop = createObjectPropertyInCurrentModel(prop);
//				}
//				else {
//					prop.removeDomain(existingDomain);
//				}
//				prop.addDomain(domainCls);
//				retval = true; // return true if it was already the domain
//			} else {
//				// this is the first domain class given
//				if (!prop.getNameSpace().equals(getModelNamespace())) {
//					prop = createObjectPropertyInCurrentModel(prop);
//				}
//				prop.addDomain(domainCls);
//				retval = true;
//			}
//		} else {
//			addError("Unable to convert domain (" + domainCls.toString() + ") to a class.");
//		}
//		return retval;
//	}
//
	private boolean updateObjectPropertyRange(OntProperty prop, OntResource rangeCls, RangeValueType rngValueType, EObject context) throws JenaProcessorException {
		boolean retval = false;
		OntResource existingRange = prop.getRange();
		OntProperty testProp = getTheJenaModel().getOntProperty(prop.getURI());
		if (testProp != null) {
			OntResource testRange = testProp.getRange();
			if (testRange != null) {
				existingRange = testRange;
			}
		}
		if (rangeCls != null) {
			if (existingRange != null && !existingRange.equals(rangeCls)) {
				// there's already a range class specified
				if (!prop.getNameSpace().equals(getModelNamespace())) {
					// this is changing the range of a property defined in a different model
					if (classIsSubclassOf((OntClass) rangeCls, existingRange, true)) {
						addWarning("The range is a subclass of the range of property '" + prop.getURI() + "' which is defined in an imported model; perhaps you mean an 'only has values of type' restricion?", context);						
					}
					else {
						addWarning("This changes the range of property '" + prop.getURI() + "' which is defined in an imported model; are you sure that's what you want to do?", context);
					}
				}
				OntResource newRange = addClassToUnionClass(existingRange, rangeCls);
				if (!prop.getNameSpace().equals(getModelNamespace())) {
					prop = createObjectPropertyInCurrentModel(prop);
				}
				else {
					prop.removeRange(existingRange);
				}
				if (prop != null) {
					prop.addRange(newRange); 
					addRangeListAnnotationToProperty(prop, rngValueType);
					retval = false;
				}
				else {
					retval = true;
				}
			} else {
				// this is the first range class given
				if (!prop.getNameSpace().equals(getModelNamespace())) {
					prop = createObjectPropertyInCurrentModel(prop);
				}
				prop.addRange(rangeCls);
				addRangeListAnnotationToProperty(prop, rngValueType);
				retval = true;
			}
		} else {
			addError("Range not found.", context);
		}
		return retval;
	}

	private void addRangeListAnnotationToProperty(OntProperty prop,	RangeValueType rngType) {
		if (rngType.equals(RangeValueType.LIST)) {
			AnnotationProperty annprop = getTheJenaModel().createAnnotationProperty(LIST_RANGE_ANNOTATION_PROPERTY);
			prop.addProperty(annprop, RangeValueType.LIST.toString());
		}
		else if (rngType.equals(RangeValueType.LISTS)) {
			AnnotationProperty annprop = getTheJenaModel().createAnnotationProperty(LIST_RANGE_ANNOTATION_PROPERTY);
			prop.addProperty(annprop, RangeValueType.LISTS.toString());
		}
		
	}

	private OntProperty createObjectPropertyInCurrentModel(OntProperty prop) throws JenaProcessorException {
//		throw new JenaProcessorException("Creation of copy of object property in current model not yet implemented");
		return null;
	}

	private void addError(String msg, EObject context) {
		if (getIssueAcceptor() != null) {
			getIssueAcceptor().addError(msg, context);
		}
		System.err.println(msg);
	}

	private void addWarning(String msg, EObject context) {
		if (getIssueAcceptor() != null) {
			getIssueAcceptor().addWarning(msg, context);
		}
		else {
			System.out.println(msg);			
		}
	}

	private OntResource addClassToUnionClass(OntResource existingCls,
			OntResource cls) throws JenaProcessorException {
		if (existingCls != null && !existingCls.equals(cls)) {
			if (existingCls.canAs(OntClass.class) && classIsSubclassOf(existingCls.as(OntClass.class), cls, true)) {
				return cls;
			}
			else if (cls.canAs(OntClass.class) && classIsSubclassOf(cls.as(OntClass.class), existingCls, true)) {
				return existingCls;
			}
			else {
				RDFList classes = null;
				if (existingCls.canAs(UnionClass.class)) {
					try {
						 UnionClass ucls = existingCls.as(UnionClass.class);
						 ucls.addOperand(cls);
						 return ucls;
					} catch (Exception e) {
						// don't know why this is happening
						logger.error("Union class error that hasn't been resolved or understood.");
						return cls;
					}
				} else {
					if (cls.equals(existingCls)) {
						return existingCls;
					}
					classes = getTheJenaModel().createList();
					OntResource inCurrentModel = null;
					if (existingCls.isURIResource()) {
						inCurrentModel = getTheJenaModel().getOntResource(existingCls.getURI());
					}
					if (inCurrentModel != null) {
						classes = classes.with(inCurrentModel);
					}
					else {
						classes = classes.with(existingCls);
					}
					classes = classes.with(cls);
				}
				OntResource unionClass = getTheJenaModel().createUnionClass(null,
						classes);
				return unionClass;
			}
		} else {
			return cls;
		}
	}

	private void processSadlNecessaryAndSufficient(SadlNecessaryAndSufficient element) throws JenaProcessorException {
		OntClass supercls = sadlTypeReferenceToOntResource(element.getSubject()).asClass();
		OntClass rolecls = getOrCreateOntClass(declarationExtensions.getConcreteName(element.getObject()));
		Iterator<SadlPropertyCondition> itr = element.getPropConditions().iterator();
		List<OntClass> conditionClasses = new ArrayList<OntClass>();
		while (itr.hasNext()) {
			SadlPropertyCondition nxt = itr.next();
			conditionClasses.add(processSadlPropertyCondition(nxt));
		}
		// we have all the parts--create the equivalence class
		if (conditionClasses.size() == 1) {
			if (supercls != null && conditionClasses.get(0) != null) {
				IntersectionClass eqcls = createIntersectionClass(supercls, conditionClasses.get(0));
				rolecls.setEquivalentClass(eqcls);
				logger.debug("New intersection class created as equivalent of '" + rolecls.getURI() + "'");
			} else if (conditionClasses.get(0) != null) {
				rolecls.setEquivalentClass(conditionClasses.get(0));
				logger.debug("Equivalent class set for '" + rolecls.getURI() + "'");
			}
			else {
				throw new JenaProcessorException("Necessary and sufficient conditions appears to have invalid input.");
			}
		}
		else {
			int base = supercls != null ? 1 : 0;
			RDFNode[] memberList = new RDFNode[base + conditionClasses.size()];
			if (base > 0) {
				memberList[0] = supercls;
			}
			for (int i = 0; i < conditionClasses.size(); i++) {
				memberList[base + i] = conditionClasses.get(i);
			}
			IntersectionClass eqcls = createIntersectionClass(memberList);
			rolecls.setEquivalentClass(eqcls);
			logger.debug("New intersection class created as equivalent of '" + rolecls.getURI() + "'");
		}
	}

	private void processSadlDifferentFrom(SadlDifferentFrom element) throws JenaProcessorException {
		List<Individual> differentFrom = new ArrayList<Individual>();
		Iterator<SadlClassOrPropertyDeclaration> dcitr = element.getTypes().iterator();
		while(dcitr.hasNext()) {
			SadlClassOrPropertyDeclaration decl = dcitr.next();
			Iterator<SadlResource> djitr = decl.getClassOrProperty().iterator();
			while (djitr.hasNext()) {
				SadlResource sr = djitr.next();
				String declUri = declarationExtensions.getConceptUri(sr);
				if (declUri == null) {
					throw new JenaProcessorException("Failed to get concept URI for SadlResource in processSadlDifferentFrom");
				}
				Individual inst = getTheJenaModel().getIndividual(declUri);
				differentFrom.add(inst);
			}
		}
		SadlTypeReference nsas = element.getNotTheSameAs();
		if (nsas != null) {
			OntResource nsasrsrc = sadlTypeReferenceToOntResource(nsas);
			differentFrom.add(nsasrsrc.asIndividual());
			SadlResource sr = element.getNameOrRef();
			Individual otherInst = getTheJenaModel().getIndividual(declarationExtensions.getConceptUri(sr));
			differentFrom.add(otherInst);
		}
		RDFNode[] nodeArray = null;
		if (differentFrom.size() > 0) {
			nodeArray = differentFrom.toArray(new Individual[differentFrom.size()]);
		}
		else {
			throw new JenaProcessorException("Unexpect empty array in processSadlDifferentFrom");
		}
		RDFList differentMembers = getTheJenaModel().createList(nodeArray);
		getTheJenaModel().createAllDifferent(differentMembers);
		logger.debug("New all different from created");
	}

	private Individual processSadlInstance(SadlInstance element) throws JenaProcessorException {
		// this has two forms:
		//	1) <name> is a <type> ...
		//	2) a <type> <name> ....
		SadlTypeReference type = element.getType();
		SadlResource sr = sadlResourceFromSadlInstance(element);
		String instUri = null;
		if (sr != null) {
			instUri = declarationExtensions.getConceptUri(sr);
			if (instUri == null) {
				throw new JenaProcessorException("Failed to get concept URI of SadlResource in processSadlInstance");
			}
		}
		OntClass cls = null;
		if (type != null) {
			cls = sadlTypeReferenceToOntResource(type).asClass();
		} 
		Individual inst;
		if (cls != null) {
			if (sr != null) {
				inst = createIndividual(instUri, cls);
			}
			else {
				inst = createIndividual(null, cls);
			}
		}
		else if (instUri != null) {
			inst = createIndividual(instUri, (OntClass)null);
		}
		else {
			throw new JenaProcessorException("Can't create an unnamed instance with no class given");
		}
		
		Iterator<SadlPropertyInitializer> itr = element.getPropertyInitializers().iterator();
		while (itr.hasNext()) {
			SadlPropertyInitializer propinit = itr.next();
			SadlResource prop = propinit.getProperty();
			EObject val = propinit.getValue();
			assignInstancePropertyValue(inst, prop, val);
		}
		return inst;
	}

	private void assignInstancePropertyValue(Individual inst, SadlResource prop, EObject val) throws JenaProcessorException {
		OntConceptType type = declarationExtensions.getOntConceptType(prop);
		String propuri = declarationExtensions.getConceptUri(prop);
		if (type.equals(OntConceptType.CLASS_PROPERTY)) {
			ObjectProperty oprop = getTheJenaModel().getObjectProperty(propuri);
			if (oprop == null) {
				addError("Property '" + propuri + "' does not exist", prop);
			}
			else {
				if (val instanceof SadlInstance) {
					Individual instval = processSadlInstance((SadlInstance) val);
					inst.addProperty(oprop, instval);
					logger.debug("added value '" + instval.toString() + "' to property '" + propuri + "' for instance '" + inst.toString() + "'");
				}
				else if (val instanceof SadlResource) {
					String uri = declarationExtensions.getConceptUri((SadlResource) val);
					com.hp.hpl.jena.rdf.model.Resource rsrc = getTheJenaModel().getResource(uri);
					if (rsrc.canAs(Individual.class)){
						inst.addProperty(oprop, rsrc.as(Individual.class));
					}
					else {
						throw new JenaProcessorException("unhandled value type SadlResource that isn't an instance (URI is '" + uri + "')");
					}
				}
				else {
					throw new JenaProcessorException("unhandled value type for object property");
				}
			}
		}
		else if (type.equals(OntConceptType.DATATYPE_PROPERTY)) {
			DatatypeProperty dprop = getTheJenaModel().getDatatypeProperty(propuri);
			if (dprop == null) {
				addError("Property '" + propuri + "' does not exist", prop);
			}
			else {
				if (val instanceof SadlExplicitValue) {
					Literal lval = sadlExplicitValueToLiteral((SadlExplicitValue)val, dprop);
					inst.addProperty(dprop, lval);
					logger.debug("added value '" + lval.toString() + "' to property '" + propuri + "' for instance '" + inst.toString() + "'");
				}
				else {
					throw new JenaProcessorException("unhandled value type for data property");
				}
			}
		}
		else {
			throw new JenaProcessorException("unhandled property type");
		}
	}

	private SadlResource sadlResourceFromSadlInstance(SadlInstance element) throws JenaProcessorException {
		SadlResource sr = element.getNameOrRef();
		if (sr == null) {
			return element.getInstance();
		}
		return sr;
	}

	private void processSadlDisjointClasses(SadlDisjointClasses element) throws JenaProcessorException {
		List<OntClass> disjointClses = new ArrayList<OntClass>();
		if (element.getClasses() != null) {
			Iterator<SadlResource> dcitr = element.getClasses().iterator();
			while (dcitr.hasNext()) {
				SadlResource sr = dcitr.next();
				String declUri = declarationExtensions.getConceptUri(sr);
				if (declUri == null) {
					throw new JenaProcessorException("Failed to get concept URI of SadlResource in processSadlDisjointClasses");
				}
				OntClass cls = getTheJenaModel().getOntClass(declUri);
				if (cls == null) {
					throw new JenaProcessorException("Failed to get class '" + declUri + "' from Jena model.");
				}
				disjointClses.add(cls.asClass());
			}
		}
		Iterator<SadlClassOrPropertyDeclaration> dcitr = element.getTypes().iterator();
		while(dcitr.hasNext()) {
			SadlClassOrPropertyDeclaration decl = dcitr.next();
			Iterator<SadlResource> djitr = decl.getClassOrProperty().iterator();
			while (djitr.hasNext()) {
				SadlResource sr = djitr.next();
				String declUri = declarationExtensions.getConceptUri(sr);
				if (declUri == null) {
					throw new JenaProcessorException("Failed to get concept URI of SadlResource in processSadlDisjointClasses");
				}
				OntClass cls = getTheJenaModel().getOntClass(declUri);
				disjointClses.add(cls.asClass());
			}
		}
		// must set them disjoint pairwise
		for (int i = 0; i < disjointClses.size(); i++) {
			for (int j = i + 1; j < disjointClses.size(); j++) {
				disjointClses.get(i).addDisjointWith(disjointClses.get(j));
			}
		}
	}

	private ObjectProperty getOrCreateObjectProperty(String propName) {
		ObjectProperty prop = getTheJenaModel().getObjectProperty(propName);
		if (prop == null) {
			prop = getTheJenaModel().createObjectProperty(propName);
			logger.debug("New object property '" + prop.getURI() + "' created");
		}
		return prop;
	}

	private DatatypeProperty getOrCreateDatatypeProperty(String propUri) throws JenaProcessorException {
		DatatypeProperty prop = getTheJenaModel().getDatatypeProperty(propUri);
		if (prop == null) {
			prop = createDatatypeProperty(propUri, null);
		}
		return prop;
	}

	private boolean checkForExistingCompatibleDatatypeProperty(
			String propUri, RDFNode rngNode) {
		DatatypeProperty prop = getTheJenaModel().getDatatypeProperty(propUri);
		if (prop != null) {
			OntResource rng = prop.getRange();
			if (rng.equals(rngNode)) {
				return true;
			}
		}
		return false;
	}

	private void addPropertyDomain(OntProperty prop, OntResource cls) {
		prop.addDomain(cls);
		logger.debug("Domain '" + cls.toString() + "' added to property '" + prop.getURI() + "'");
	}

	private RDFNode primitiveDatatypeToRDFNode(String name) {
		return getTheJenaModel().getResource(XSD.getURI() + name);
	}

	private OntClass getOrCreateOntClass(String name) {
		OntClass cls = getTheJenaModel().getOntClass(name);
		if (cls == null) {
			cls = createOntClass(name, (OntClass)null);
		}
		return cls;
	}
	
	private OntClass createOntClass(String newName, String superSRUri) {
		if (superSRUri != null) {
			OntClass superCls = getTheJenaModel().getOntClass(superSRUri);
			return createOntClass(newName, superCls);
		}
		return createOntClass(newName, (OntClass)null);
	}
	
	private OntClass createOntClass(String newName, OntClass superCls) {
		OntClass newCls = getTheJenaModel().createClass(newName);
		logger.debug("New class '" + newCls.getURI() + "' created");
		if (superCls != null) {
			newCls.addSuperClass(superCls);
			logger.debug("    Class '" + newCls.getURI() + "' given super class '" + superCls.toString() + "'");
		}
		return newCls;
	}
	
	private OntProperty createObjectProperty(String newName, String superSRUri) throws JenaProcessorException {
		OntProperty newProp = getTheJenaModel().createObjectProperty(newName);
		logger.debug("New object property '" + newProp.getURI() + "' created");
		if (superSRUri != null) {
			OntProperty superProp = getTheJenaModel().getOntProperty(superSRUri);
			if (superProp == null) {
				throw new JenaProcessorException("Unable to find super property '" + superSRUri + "'");
			}
			newProp.addSuperProperty(superProp);
			logger.debug("   Object property '" + newProp.getURI() + "' given super property '" + superSRUri + "'");
		}
		return newProp;
	}
	
	private DatatypeProperty createDatatypeProperty(String newName, String superSRUri) throws JenaProcessorException {
		DatatypeProperty newProp = getTheJenaModel().createDatatypeProperty(newName);
		logger.debug("New datatype property '" + newProp.getURI() + "' created");
		if (superSRUri != null) {
			OntProperty superProp = getTheJenaModel().getOntProperty(superSRUri);
			if (superProp == null) {
				throw new JenaProcessorException("Unable to find super property '" + superSRUri + "'");
			}
			newProp.addSuperProperty(superProp);
			logger.debug("    Datatype property '" + newProp.getURI() + "' given super property '" + superSRUri + "'");
		}
		return newProp;
	}
	
	private Individual createIndividual(String newName, String superSRUri) {
		OntClass cls = getTheJenaModel().getOntClass(superSRUri);
		return createIndividual(newName, cls);
	}
	
	private Individual createIndividual(String newName, OntClass supercls) {
		Individual inst = getTheJenaModel().createIndividual(newName, supercls);
		logger.debug("New instance '" + (newName != null ? newName : "(bnode)") + "' created");
		return inst;
	}
	
	private AnnotationProperty createAnnotationProperty(String newName) {
		AnnotationProperty annProp = getTheJenaModel().createAnnotationProperty(newName);
		logger.debug("New annotation property '" + annProp.getURI() + "' created");
		return annProp;
	}
	
	private OntResource sadlTypeReferenceToOntResource(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		Object obj = sadlTypeReferenceToObject(sadlTypeRef);
		if (obj instanceof OntResource) {
			return (OntResource)obj;
		}
		else if (obj instanceof RDFNode) {
			if (((RDFNode)obj).canAs(OntResource.class)) {
				return ((RDFNode)obj).as(OntResource.class);
			}
		}
		throw new JenaProcessorException("Unable to convert SadlTypeReference '" + sadlTypeRef + "' to OntResource");
	}

	private Object sadlTypeReferenceToObject(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		OntResource rsrc = null;
		// TODO How do we tell if this is a union versus an intersection?						
		if (sadlTypeRef instanceof SadlSimpleTypeReference) {
			SadlResource strSR = ((SadlSimpleTypeReference)sadlTypeRef).getType();
			//TODO check for proxy, i.e. unresolved references
			OntConceptType ctype = declarationExtensions.getOntConceptType(strSR);
			String strSRUri = declarationExtensions.getConceptUri(strSR);	
			if (strSRUri == null) {
				throw new JenaProcessorException("Failed to get concept URI of SadlResource in sadlTypeReferenceToObject");
			}
			if (ctype.equals(OntConceptType.CLASS)) {
				rsrc = getTheJenaModel().getOntClass(strSRUri);
				if (rsrc == null) {
					return createOntClass(strSRUri, (OntClass)null);
				}
			}
			else if (ctype.equals(OntConceptType.INSTANCE)) {
				rsrc = getTheJenaModel().getIndividual(strSRUri);
				if (rsrc == null) {
					// is it OK to create Individual without knowing class??
					return createIndividual(strSRUri, (OntClass)null);
				}
			}
			else if (ctype.equals(OntConceptType.DATATYPE)) {				
				OntResource dt = getTheJenaModel().getOntResource(strSRUri);
				if (dt == null) {
					throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri + "' not found; it should exist as there isn't enough information to create it.");
				}
				return dt;
			}
			else if (ctype.equals(OntConceptType.CLASS_PROPERTY)) {
				OntProperty otp = getTheJenaModel().getOntProperty(strSRUri);
				if (otp == null) {
					throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri + "' not found; should have found an ObjectProperty");
				}
				return otp;
			}
			else if (ctype.equals(OntConceptType.DATATYPE_PROPERTY)) {
				OntProperty dtp = getTheJenaModel().getOntProperty(strSRUri);
				if (dtp == null) {
					throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri + "' not found; should have found an DatatypeProperty");
				}
				return dtp;
			}
			else {
				throw new JenaProcessorException("SadlSimpleTypeReference '" + strSRUri + "' was of a type not yet handled: " + ctype.toString());
			}
		}
		else if (sadlTypeRef instanceof SadlPrimitiveDataType) {
			return processSadlPrimitiveDataType(null, (SadlPrimitiveDataType) sadlTypeRef, null);
		}
		else if (sadlTypeRef instanceof SadlPropertyCondition) {
			return processSadlPropertyCondition((SadlPropertyCondition) sadlTypeRef);		
		}
		else if (sadlTypeRef instanceof SadlUnionType) {
			RDFNode lftNode = null; RDFNode rhtNode = null;
			SadlTypeReference lft = ((SadlUnionType)sadlTypeRef).getLeft();
			Object lftObj = sadlTypeReferenceToObject(lft);
			if (lftObj instanceof OntResource) {
				lftNode = ((OntResource)lftObj).asClass();
			}
			else {
				if (lftObj instanceof RDFNode) {
					lftNode = (RDFNode) lftObj;
				}
				else {
					throw new JenaProcessorException("Union member of unsupported type: " + lftObj.getClass().getCanonicalName());
				}
			}
			SadlTypeReference rht = ((SadlUnionType)sadlTypeRef).getRight();
			Object rhtObj = sadlTypeReferenceToObject(rht);
			if (rhtObj instanceof OntResource) {
				rhtNode = ((OntResource)rhtObj).asClass();
			}
			else {
				if (rhtObj instanceof RDFNode) {
					rhtNode = (RDFNode) rhtObj;
				}
				else {
					throw new JenaProcessorException("Union member of unsupported type: " + rhtObj != null ? rhtObj.getClass().getCanonicalName() : "null");
				}
			}
			OntClass unionCls = createUnionClass(lftNode, rhtNode);
			return unionCls;
		}
		else if (sadlTypeRef instanceof SadlIntersectionType) {
			RDFNode lftNode = null; RDFNode rhtNode = null;
			SadlTypeReference lft = ((SadlIntersectionType)sadlTypeRef).getLeft();
			Object lftObj = sadlTypeReferenceToObject(lft);
			if (lftObj instanceof OntResource) {
				lftNode = ((OntResource)lftObj).asClass();
			}
			else {
				if (lftObj instanceof RDFNode) {
					lftNode = (RDFNode) lftObj;
				}
				else {
					throw new JenaProcessorException("Intersection member of unsupported type: " + lftObj.getClass().getCanonicalName());
				}
			}
			SadlTypeReference rht = ((SadlIntersectionType)sadlTypeRef).getRight();
			if (rht == null) {
				throw new JenaProcessorException("No right-hand side to intersection");
			}
			Object rhtObj = sadlTypeReferenceToObject(rht);
			if (rhtObj instanceof OntResource) {
				rhtNode = ((OntResource)rhtObj).asClass();
			}
			else {
				if (rhtObj instanceof RDFNode) {
					rhtNode = (RDFNode) rhtObj;
				}
				else {
					throw new JenaProcessorException("Intersection member of unsupported type: " + rhtObj.getClass().getCanonicalName());
				}
			}
			OntClass intersectCls = createIntersectionClass(lftNode, rhtNode);
			return intersectCls;
		}
		return rsrc;
	}

	private com.hp.hpl.jena.rdf.model.Resource processSadlPrimitiveDataType(SadlClassOrPropertyDeclaration element, SadlPrimitiveDataType sadlTypeRef, String newDatatypeUri) throws JenaProcessorException {
		SadlDataType pt = sadlTypeRef.getPrimitiveType();
		String typeStr = pt.getLiteral();
		com.hp.hpl.jena.rdf.model.Resource onDatatype;
		if (typeStr.equals(XSD.xstring.getLocalName())) onDatatype = XSD.xstring;
		else if (typeStr.equals(XSD.anyURI.getLocalName())) onDatatype = XSD.anyURI;
		else if (typeStr.equals(XSD.base64Binary.getLocalName())) onDatatype = XSD.base64Binary;
		else if (typeStr.equals(XSD.date.getLocalName())) onDatatype = XSD.date;
		else if (typeStr.equals(XSD.dateTime.getLocalName())) onDatatype = XSD.dateTime;
		else if (typeStr.equals(XSD.decimal.getLocalName())) onDatatype = XSD.decimal;
		else if (typeStr.equals(XSD.duration.getLocalName())) onDatatype = XSD.duration;
		else if (typeStr.equals(XSD.gDay.getLocalName())) onDatatype = XSD.gDay;
		else if (typeStr.equals(XSD.gMonth.getLocalName())) onDatatype = XSD.gMonth;
		else if (typeStr.equals(XSD.gMonthDay.getLocalName())) onDatatype = XSD.gMonthDay;
		else if (typeStr.equals(XSD.gYear.getLocalName())) onDatatype = XSD.gYear;
		else if (typeStr.equals(XSD.gYearMonth.getLocalName())) onDatatype = XSD.gYearMonth;
		else if (typeStr.equals(XSD.hexBinary.getLocalName())) onDatatype = XSD.hexBinary;
		else if (typeStr.equals(XSD.integer.getLocalName())) onDatatype = XSD.integer;
		else if (typeStr.equals(XSD.time.getLocalName())) onDatatype = XSD.time;
		else if (typeStr.equals(XSD.xboolean.getLocalName())) onDatatype = XSD.xboolean;
		else if (typeStr.equals(XSD.xdouble.getLocalName())) onDatatype = XSD.xdouble;
		else if (typeStr.equals(XSD.xfloat.getLocalName())) onDatatype = XSD.xfloat;
		else if (typeStr.equals(XSD.xint.getLocalName())) onDatatype = XSD.xint;
		else if (typeStr.equals(XSD.xlong.getLocalName())) onDatatype = XSD.xlong;
		else if (typeStr.equals(XSD.anyURI.getLocalName())) onDatatype = XSD.anyURI;
		else if (typeStr.equals(XSD.anyURI.getLocalName())) onDatatype = XSD.anyURI;
		else if (typeStr.equals("data")) onDatatype = null;
		else {
			throw new JenaProcessorException("Unexpected primitive data type: " + typeStr);
		}
		if (newDatatypeUri == null) {
			return onDatatype;
		}
		OntClass datatype = getTheJenaModel().createOntResource(OntClass.class, RDFS.Datatype, newDatatypeUri);
		OntClass equivClass = getTheJenaModel().createOntResource(OntClass.class, RDFS.Datatype, null);
		equivClass.addProperty(OWL2.onDatatype, onDatatype);
		if (element.getFacet() != null) {
			com.hp.hpl.jena.rdf.model.Resource restrictions = facetsToRestrictionNode(newDatatypeUri, element.getFacet());
			// Create a list containing the restrictions
			RDFList list = getTheJenaModel().createList(new RDFNode[] {restrictions});
			equivClass.addProperty(OWL2.withRestrictions, list);
		}
		datatype.addEquivalentClass(equivClass);
		return datatype;
	}

	private com.hp.hpl.jena.rdf.model.Resource facetsToRestrictionNode(String newName, SadlDataTypeFacet facet) {
		com.hp.hpl.jena.rdf.model.Resource anon = getTheJenaModel().createResource();
		boolean minInclusive = (facet.getMinexin() != null && facet.getMinexin().equals("["));
		boolean maxInclusive = (facet.getMaxexin() != null && facet.getMaxexin().equals("]"));
		if (minInclusive) {
			anon.addProperty(xsdProperty("minInclusive"), "" + facet.getMin());
		}
		else {
			anon.addProperty(xsdProperty("minExclusive"), "" + facet.getMin());
		}
		if (maxInclusive) {
			anon.addProperty(xsdProperty("maxInclusive"), "" + facet.getMax());
		}
		else {
			anon.addProperty(xsdProperty("maxExclusive"), "" + facet.getMax());
		}
		anon.addProperty(xsdProperty("length"), "" + facet.getLen());
		anon.addProperty(xsdProperty("minLength"), "" + facet.getMinlen());
		anon.addProperty(xsdProperty("maxLength"), "" + facet.getMaxlen());
		anon.addProperty(xsdProperty("pattern"), "" + facet.getRegex());
		if (facet.getValues() != null) {
			Iterator<String> iter = facet.getValues().iterator();
			while (iter.hasNext()) {
				anon.addProperty(xsdProperty("enumeration"), iter.next());
			}
		}
		return anon;
	}

	private OntClass processSadlPropertyCondition(SadlPropertyCondition sadlPropCond) throws JenaProcessorException {
		OntClass retval = null;
		SadlResource sr = ((SadlPropertyCondition)sadlPropCond).getProperty();
		String propUri = declarationExtensions.getConceptUri(sr);
		if (propUri == null) {
			throw new JenaProcessorException("Failed to get concept URI of SadlResource in processSadlPropertyCondition");
		}
		OntConceptType propType = declarationExtensions.getOntConceptType(sr);
		OntProperty prop = getTheJenaModel().getOntProperty(propUri);
		Iterator<SadlCondition> conditer = ((SadlPropertyCondition)sadlPropCond).getCond().iterator();
		while (conditer.hasNext()) {
			SadlCondition cond = conditer.next();
			retval = processSadlCondition(cond, prop, propType);
			if (conditer.hasNext()) {
				throw new JenaProcessorException("Multiple property conditions not currently handled");
			}
		}
		return retval;
	}

	private OntClass processSadlCondition(SadlCondition cond, OntProperty prop, OntConceptType propType) throws JenaProcessorException {
		OntClass retval = null;
		if (cond instanceof SadlAllValuesCondition) {
			SadlTypeReference type = ((SadlAllValuesCondition)cond).getType();
			OntResource typersrc = sadlTypeReferenceToOntResource(type);
			AllValuesFromRestriction avf = getTheJenaModel().createAllValuesFromRestriction(null, prop, typersrc);
			logger.debug("New all values from restriction on '" + prop.getURI() + "' to values of type '" + typersrc.toString() + "'");
			retval = avf;
		}
		else if (cond instanceof SadlHasValueCondition) {
			SadlExplicitValue value = ((SadlHasValueCondition)cond).getRestriction();
			if (propType.equals(OntConceptType.CLASS_PROPERTY)) {
				if (value instanceof SadlResource) {
					OntConceptType srType = declarationExtensions.getOntConceptType((SadlResource)value);
					SadlResource srValue = (SadlResource) value;
					if (srType == null) {
						srValue = ((SadlResource)value).getName();
						srType = declarationExtensions.getOntConceptType(srValue);
					}
					if (srType == null) {
						throw new JenaProcessorException("Unable to resolve SadlResource value");
					}
					if (srType.equals(OntConceptType.INSTANCE)) {
						String valUri = declarationExtensions.getConceptUri(srValue);
						if (valUri == null) {
							throw new JenaProcessorException("Failed to find SadlResource in Xtext model");
						}
						Individual valInst = getTheJenaModel().getIndividual(valUri);
						if (valInst == null) {
							throw new JenaProcessorException("Failed to retrieve instance '" + valUri + "' from Jena model");
						}
						if (valueInObjectTypePropertyRange(prop, valInst)) {
							HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, prop, valInst);
							logger.debug("New has value restriction on '" + prop.getURI() + "' to value '" + valInst.toString() + "'");
							retval =  hvr;
						}
						else {
							throw new JenaProcessorException("Value '" + valInst.getURI() + "' not in range of object property '" + prop.getURI() + "'");
						}
					}
					else {
						throw new JenaProcessorException("A has value restriction on an object property must have an instance as the restricted value");
					}
				}
				else {
					throw new JenaProcessorException("A has value restriction on an object property has an unexpected restricted value type: " + value.getClass().getCanonicalName());
				}
			}
			else if (propType.equals(OntConceptType.DATATYPE_PROPERTY)) {
				Literal val = sadlExplicitValueToLiteral(value, prop);
				if (valueInDatatypePropertyRange(prop, val)) {
					HasValueRestriction hvr = getTheJenaModel().createHasValueRestriction(null, prop, val);
					logger.debug("New has value restriction on '" + prop.getURI() + "' to value '" + val.toString() + "'");
					retval =  hvr;
				}	
				else {
					throw new JenaProcessorException("Value '" + val.getLexicalForm() + "' not in range of datatype property '" + prop.getURI() + "'");
				}
			}
			else {
				throw new JenaProcessorException("Has value restriction on unexpected property type: " + propType.toString());
			}
		}
		else if (cond instanceof SadlCardinalityCondition) {
			// Note: SomeValuesFrom is embedded in cardinality in the SADL grammar--an "at least" cardinality with "one" instead of # 
			String cardinality = ((SadlCardinalityCondition)cond).getCardinality();
			SadlTypeReference type = ((SadlCardinalityCondition)cond).getType();
			OntResource typersrc = null;
			if (type != null) {
				typersrc = sadlTypeReferenceToOntResource(type);					
			}
			if (cardinality.equals("one")) {
				// this is interpreted as a someValuesFrom restriction
				if (type == null) {
					throw new JenaProcessorException("'one' means some value from class so a type must be given");
				}
				SomeValuesFromRestriction svf = getTheJenaModel().createSomeValuesFromRestriction(null, prop, typersrc);
				logger.debug("New some values from restriction on '" + prop.getURI() + "' to values of type '" + typersrc.toString() + "'");
				retval =  svf;
			}
			else {
				// cardinality restrictioin
				int cardNum = Integer.parseInt(cardinality);
				String op = ((SadlCardinalityCondition)cond).getOperator();
				if (op == null) {
					CardinalityRestriction cr = getTheJenaModel().createCardinalityRestriction(null, prop, cardNum);	
					logger.debug("New cardinality restriction " + cardNum + " on '" + prop.getURI() + "' created");
					if (type != null) {
						cr.removeAll(OWL.cardinality);
						cr.addLiteral(OWL2.qualifiedCardinality, cardNum);
						cr.addProperty(OWL2.onClass, typersrc);
					}
					retval =  cr;
				}
				else if (op.equals("least")) {
					MinCardinalityRestriction cr = getTheJenaModel().createMinCardinalityRestriction(null, prop, cardNum);							
					logger.debug("New min cardinality restriction " + cardNum + " on '" + prop.getURI() + "' created");
					if (type != null) {
						cr.removeAll(OWL.minCardinality);
						cr.addLiteral(OWL2.minQualifiedCardinality, cardNum);
						cr.addProperty(OWL2.onClass, typersrc);
					}
					retval =  cr;
				}
				else if (op.equals("most")) {
					logger.debug("New max cardinality restriction " + cardNum + " on '" + prop.getURI() + "' created");
					MaxCardinalityRestriction cr = getTheJenaModel().createMaxCardinalityRestriction(null, prop, cardNum);							
					if (type != null) {
						cr.removeAll(OWL.maxCardinality);
						cr.addLiteral(OWL2.maxQualifiedCardinality, cardNum);
						cr.addProperty(OWL2.onClass, typersrc);
					}
					retval =  cr;
				}
				if (logger.isDebugEnabled()) {
					if (type != null) {
						logger.debug("   cardinality is qualified; values must be of type '" + typersrc + "'");
					}	
				}
			}
		}
		else {
			throw new JenaProcessorException("Unhandled SadlCondition type: " + cond.getClass().getCanonicalName());
		}
		return retval;
	}

	private boolean valueInDatatypePropertyRange(OntProperty prop, Literal val) {
		String ptype = prop.getRange().getURI();
		if (ptype == null) {
			return true;
		}
		String dtype = val.getDatatypeURI();
		if (dtype.equals(ptype)) {
			return true;
		}
		return false;
	}

	private Literal sadlExplicitValueToLiteral(SadlExplicitValue value, OntProperty prop) throws JenaProcessorException {
		if (value instanceof SadlNumberLiteral) {
			String val = ((SadlNumberLiteral)value).getLiteralNumber();
			return UtilsForJena.getLiteralMatchingDataPropertyRange(getTheJenaModel(), prop, val);
		}
		else if (value instanceof SadlStringLiteral) {
			String val = ((SadlStringLiteral)value).getLiteralString();
			return UtilsForJena.getLiteralMatchingDataPropertyRange(getTheJenaModel(), prop, val);
		}
		else if (value instanceof SadlBooleanLiteral) {
			SadlBooleanLiteral val = ((SadlBooleanLiteral)value);
			return UtilsForJena.getLiteralMatchingDataPropertyRange(getTheJenaModel(), prop, val.toString());
		}
		else if (value instanceof SadlValueList) {
			throw new JenaProcessorException("A SADL value list cannot be converted to a Literal");
		}
		else if (value instanceof SadlConstantLiteral) {
			String val = ((SadlConstantLiteral)value).getTerm();
			return UtilsForJena.getLiteralMatchingDataPropertyRange(getTheJenaModel(), prop, val);
		}
		else {
			throw new JenaProcessorException("Unhandled sadl explicit vaue type: " + value.getClass().getCanonicalName());
		}
	}

	private boolean valueInObjectTypePropertyRange(OntProperty prop, Individual valInst) throws JenaProcessorException {
		ExtendedIterator<? extends OntResource> itr = prop.listRange();
		while (itr.hasNext()) {
			OntResource nxt = itr.next();
			if (nxt.isClass()) {
				if (instanceBelongsToClass(getTheJenaModel(), valInst, nxt)) {
					return true;
				}
			}
		}
		return false;
	}
	
	private IntersectionClass createIntersectionClass(List<RDFNode> members) throws JenaProcessorException {
		RDFNode[] array = members.toArray(new RDFNode[members.size()]);
		return createIntersectionClass(array);
	}

	private IntersectionClass createIntersectionClass(RDFNode... members) throws JenaProcessorException {
		RDFList classes = getTheJenaModel().createList(members);
		if (!classes.isEmpty()) {
			IntersectionClass intersectCls = getTheJenaModel().createIntersectionClass(null, classes);
			logger.debug("New intersection class created");
			return intersectCls;
		}
		throw new JenaProcessorException("createIntersectionClass called with empty list of classes");
	}

	private UnionClass createUnionClass(List<RDFNode> members) throws JenaProcessorException {
		RDFNode[] array = members.toArray(new RDFNode[members.size()]);
		return createUnionClass(array);
	}
	private UnionClass createUnionClass(RDFNode... members) throws JenaProcessorException {
		RDFList classes = getTheJenaModel().createList(members);
		if (!classes.isEmpty()) {
			UnionClass unionCls = getTheJenaModel().createUnionClass(null, classes);
			logger.debug("New union class created");
			return unionCls;
		}
		throw new JenaProcessorException("createUnionClass called with empty list of classes");
	}

	private OntConceptType getSadlTypeReferenceType(SadlTypeReference sadlTypeRef) throws JenaProcessorException {
		if (sadlTypeRef instanceof SadlSimpleTypeReference) {
			SadlResource sr = ((SadlSimpleTypeReference)sadlTypeRef).getType();
			return declarationExtensions.getOntConceptType(sr);
		}
		else if (sadlTypeRef instanceof SadlPrimitiveDataType) {
			return OntConceptType.DATATYPE;
		}
		else if (sadlTypeRef instanceof SadlPropertyCondition) {
			// property conditions => OntClass
			return OntConceptType.CLASS;
		}
		else if (sadlTypeRef instanceof SadlUnionType) {
			SadlTypeReference lft = ((SadlUnionType)sadlTypeRef).getLeft();
			OntConceptType lfttype = getSadlTypeReferenceType(lft);
			return lfttype;
//			SadlTypeReference rght = ((SadlUnionType)sadlTypeRef).getRight();
		}
		else if (sadlTypeRef instanceof SadlIntersectionType) {
			SadlTypeReference lft = ((SadlIntersectionType)sadlTypeRef).getLeft();
			OntConceptType lfttype = getSadlTypeReferenceType(lft);
			return lfttype;
//			SadlTypeReference rght = ((SadlIntersectionType)sadlTypeRef).getRight();
		}
		throw new JenaProcessorException("Unexpected SadlTypeReference subtype: " + sadlTypeRef.getClass().getCanonicalName());
	}

	private String assureNamespaceEndsWithHash(String name) {
		name = name.trim();
		if (!name.endsWith("#")) {
			return name + "#";
		}
		return name;
	}

	private String getModelNamespace() {
		return modelNamespace;
	}

	private void setModelNamespace(String modelNamespace) {
		this.modelNamespace = modelNamespace;
	}

	public OntDocumentManager getJenaDocumentMgr(OntModelSpec ontModelSpec) {
		if (jenaDocumentMgr == null) {
			if (getMappingModel() != null) {
				setJenaDocumentMgr(new OntDocumentManager(getMappingModel()));
				if (ontModelSpec != null) {
					ontModelSpec.setDocumentManager(jenaDocumentMgr);
				}
			}
			else {
				setJenaDocumentMgr(OntDocumentManager.getInstance());
			}
		}
		return jenaDocumentMgr;
	}

	private void setJenaDocumentMgr(OntDocumentManager ontDocumentManager) {
		jenaDocumentMgr = ontDocumentManager;
	}

	private Model getMappingModel() {
		// TODO Auto-generated method stub
		return null;
	}
	
	/**
	 * return true if the instance belongs to the class else return false
	 * 
	 * @param inst
	 * @param cls
	 * @return
	 * @throws JenaProcessorException 
	 */
	private boolean instanceBelongsToClass(OntModel m, OntResource inst, OntResource cls) throws JenaProcessorException {
		// The following cases must be considered:
		// 1) The class is a union of other classes. Check to see if the instance is a member of any of
		//		the union classes and if so return true.
		// 2) The class is an intersection of other classes. Check to see if the instance is 
		//		a member of each class in the intersection and if so return true.
		// 3) The class is neither a union nor an intersection. If the instance belongs to the class return true. Otherwise
		//		check to see if the instance belongs to a subclass of the class else
		//		return false. (Superclasses do not need to be considered because even if the instance belongs to a super
		//		class that does not tell us that it belongs to the class.)
		
		/*
		 * e.g., 	Internet is a Network.
		 * 			Network is a type of Subsystem.
		 * 			Subsystem is type of System.
		 */
		if (cls.isURIResource()) {
			cls = m.getOntClass(cls.getURI());
		}
		if (cls == null) {
			return false;
		}
		if (cls.canAs(UnionClass.class)) {
			List<OntResource> uclses = getOntResourcesInUnionClass(m, cls.as(UnionClass.class));	
			for (int i = 0; i < uclses.size(); i++) {
				OntResource ucls = uclses.get(i);
				if (instanceBelongsToClass(m, inst, ucls)) {
					return true;
				}
			}
		}
		else if (cls.canAs(IntersectionClass.class)) {
			List<OntResource> uclses = getOntResourcesInIntersectionClass(m, cls.as(IntersectionClass.class));	
			for (int i = 0; i < uclses.size(); i++) {
				OntResource ucls = uclses.get(i);
				if (!instanceBelongsToClass(m, inst, ucls)) {
					return false;
				}
			}
			return true;
		}
		else if (cls.canAs(Restriction.class)) {
			Restriction rest = cls.as(Restriction.class);
			OntProperty ontp = rest.getOnProperty();				
			if (rest.isAllValuesFromRestriction()) {
				StmtIterator siter = inst.listProperties(ontp);
				while (siter.hasNext()) {
					Statement stmt = siter.nextStatement();
					RDFNode obj = stmt.getObject();
					if (obj.canAs(Individual.class)) {
						com.hp.hpl.jena.rdf.model.Resource avfc = rest.asAllValuesFromRestriction().getAllValuesFrom();
						if (!instanceBelongsToClass(m, (Individual)obj.as(Individual.class), (OntResource)avfc.as(OntResource.class))) {
							return false;
						}
					}
				}
			}
			else if (rest.isSomeValuesFromRestriction()) {
				if (inst.hasProperty(ontp)) {
					return true;
				}
			}
			else if (rest.isHasValueRestriction()) {
				RDFNode hval = rest.as(HasValueRestriction.class).getHasValue();
				if (inst.hasProperty(ontp, hval)) {
					return true;
				}
			}
			else if (rest.isCardinalityRestriction()) {
				throw new JenaProcessorException("Unhandled cardinality restriction");
			}
			else if (rest.isMaxCardinalityRestriction()) {
				throw new JenaProcessorException("Unhandled max cardinality restriction");
			}
			else if (rest.isMinCardinalityRestriction()) {
				throw new JenaProcessorException("Unhandled min cardinality restriction");
			}
		}
		else {
			if (inst.canAs(Individual.class)) {
				ExtendedIterator<com.hp.hpl.jena.rdf.model.Resource> eitr = inst.asIndividual().listRDFTypes(false);
				while (eitr.hasNext()) {
					com.hp.hpl.jena.rdf.model.Resource r = eitr.next();				
					OntResource or = m.getOntResource(r);
					if (or.isURIResource()) {
						OntClass oc = m.getOntClass(or.getURI());
						if (classIsSubclassOf(oc, cls, true)) {
							eitr.close();
							return true;
						}
					}
					else if (or.canAs(OntClass.class)) {
						if (classIsSubclassOf(or.as(OntClass.class), cls, true)) {
							eitr.close();
							return true;
						}
					}
				}
			}
		}
		return false;
	}
	/**
	 * return true if the first argument class is a subclass of the second
	 * argument class
	 * 
	 * @param subcls
	 * @param cls
	 * @return
	 * @throws JenaProcessorException 
	 */
	private boolean classIsSubclassOf(OntClass subcls, OntResource cls, boolean rootCall) throws JenaProcessorException {
		if (subcls == null || cls == null) {
			return false;
		}
		if (cls.isURIResource() && subcls.isURIResource()
				&& cls.getURI().equals(subcls.getURI())) {
			return true;
		}
		if (cls.isAnon()) {
			if (cls.canAs(OntClass.class)) {
				OntClass ocls = cls.as(OntClass.class);
				if (ocls.isUnionClass()) {
					UnionClass ucls = cls.as(UnionClass.class);
					try {
						ExtendedIterator<? extends OntClass> eitr = ucls
								.listOperands();
						while (eitr.hasNext()) {
							OntClass uclsmember = eitr.next();
							if (classIsSubclassOf(subcls, uclsmember, false)) {
								eitr.close();
								return true;
							}
						}
					}
					catch (Exception e) {
						logger.error("Unexpected error during deep validation: apparent Union Class does not return operands.");
					}
				}
			}
		}
		try {
			if (cls.canAs(OntClass.class)) {
				ExtendedIterator<OntClass> eitr = cls.as(OntClass.class).listSubClasses();
				while (eitr.hasNext()) {
					OntClass subClsOfCls = eitr.next();
					if (subClsOfCls.equals(subcls)) {
						eitr.close();
						return true;
					}
					else {
						if (classIsSubclassOf(subcls, subClsOfCls, false)) {
							eitr.close();
							return true;
						}
					}
				}
				eitr.close();
//				if (rootCall && classIsSuperClassOf(cls.as(OntClass.class), subcls)) {
//					return true;
//				}
			}
			if (subcls.isAnon()) {
				if (subcls.isIntersectionClass()) {
					IntersectionClass icls = subcls.asIntersectionClass();
					try {
						ExtendedIterator<? extends OntClass> eitr = icls.listOperands();
						while (eitr.hasNext()) {
							OntClass iclsmember = eitr.next();
							if (classIsSubclassOf(cls.as(OntClass.class), iclsmember, false)) {
								eitr.close();
								return true;
							}
						}
					}
					catch (Exception e) {
						logger.error("Unexpected error during deep validation: apparent Intersection Class does not return operands.");
					}
				}
			}
// TODO We need to look for equivalent classes that provide a definition for a subclass, 
//			e.g. Component is equivalent to System is class, (System and connectedTo someValueFrom Network) => Component subclass of System.
			if (cls.canAs(OntClass.class)) {
				ExtendedIterator<OntClass> eqitr = cls.as(OntClass.class).listEquivalentClasses();
				while (eqitr.hasNext()) {
					OntClass eqcls = eqitr.next();
					if (classIsSubclassOf(subcls, eqcls, false)) {
						return true;
					}
				}
			}

		} catch (Throwable t) {
			t.printStackTrace();
			logger.debug("Error in classIsSubclassOf: " + t.getMessage());
			throw new JenaProcessorException(t.getMessage(), t);
		}
		return false;
	}

	private boolean classIsSuperClassOf(OntClass cls, OntClass subcls) {
		ExtendedIterator<OntClass> eitr = subcls.listSuperClasses();
		try {
			while (eitr.hasNext()) {
				OntClass sprcls = eitr.next();
				if (sprcls.equals(cls)) {
					return true;
				}
				if (classIsSuperClassOf(cls, sprcls)) {
					return true;
				}
			}
			eitr.close();
			
			eitr = cls.listSuperClasses();
			while (eitr.hasNext()) {
				OntClass equivCls = eitr.next();
				if (classIsSubclassOf(subcls, equivCls, false)) {
					eitr.close();
					return true;
				}
			}
		}
		catch (Throwable t) {
			logger.error("Error checking if class '" + cls.toString() + "' is a superclass of '" + subcls.toString() + 
					"' : " + t.getMessage());
		}
		finally {
			eitr.close();
		}
		return false;
	}

	private List<OntResource> getOntResourcesInUnionClass(OntModel m, UnionClass ucls) {
		List<OntResource> results = new ArrayList<OntResource>();
		List<RDFNode> clses = ucls.getOperands().asJavaList();
		for (int i = 0; i < clses.size(); i++) {
			RDFNode mcls = clses.get(i);
			if (mcls.canAs(OntResource.class)) {
				results.add(mcls.as(OntResource.class));
			}
		}
		return results;
	}
	
	private List<OntResource> getOntResourcesInIntersectionClass(OntModel m, IntersectionClass icls) {
		List<OntResource> results = new ArrayList<OntResource>();
		List<RDFNode> clses = icls.getOperands().asJavaList();
		for (int i = 0; i < clses.size(); i++) {
			RDFNode mcls = clses.get(i);
			if (mcls.canAs(OntResource.class)) {
				results.add(mcls.as(OntResource.class));
			}
		}
		return results;
	}

	private ValidationAcceptor getIssueAcceptor() {
		return issueAcceptor;
	}

	private void setIssueAcceptor(ValidationAcceptor issueAcceptor) {
		this.issueAcceptor = issueAcceptor;
	}

	private CancelIndicator getCancelIndicator() {
		return cancelIndicator;
	}

	private void setCancelIndicator(CancelIndicator cancelIndicator) {
		this.cancelIndicator = cancelIndicator;
	}

	protected String getModelName() {
		return modelName;
	}

	protected void setModelName(String modelName) {
		this.modelName = modelName;
	}

	@Override
	public void processExternalModels(String mappingFileFolder, List<String> fileNames) throws IOException {
		File mff = new File(mappingFileFolder);
		if (!mff.exists()) {
			mff.mkdirs();
		}
		if (!mff.isDirectory()) {
			throw new IOException("Mapping file location '" + mappingFileFolder + "' exists but is not a directory.");
		}
		System.out.println("Ready to save mappings in folder: " + mff.getCanonicalPath());
		for (int i = 0; i < fileNames.size(); i++) {
			System.out.println("   URL: " + fileNames.get(i));
		}
	}
	private String getModelAlias() {
		return modelAlias;
	}
	private void setModelAlias(String modelAlias) {
		this.modelAlias = modelAlias;
	}
	private OntModelSpec getSpec() {
		return spec;
	}
	private void setSpec(OntModelSpec spec) {
		this.spec = spec;
	}
	
	private boolean isVariable(SadlResource sr) {
		OntConceptType ct = declarationExtensions.getOntConceptType(sr);
		if (ct.equals(OntConceptType.VARIABLE)) {
			return true;
		}
		return false;
	}
	/**
	 * This method looks in the clauses of a Rule to see if there is already a triple matching the given pattern. If there is
	 * a new variable of the same name is created (to make sure the count is right) and returned. If not a rule or no match
	 * a new variable (new name) is created and returned.
	 * @param expr 
	 * @param subject
	 * @param predicate
	 * @param object
	 * @return
	 */
	protected VariableNode getVariableNode(Expression expr, Node subject, Node predicate, Node object) {
		if (target != null) {
			// Note: when we find a match we still create a new VariableNode with the same name in order to have the right reference counts for the new VariableNode
			if (target instanceof Rule) {
				VariableNode var = findVariableInTripleForReuse(((Rule)target).getGivens(), subject, predicate, object);
				if (var != null) {
					return new VariableNode(var.getName());
				}
				var = findVariableInTripleForReuse(((Rule)target).getIfs(), subject, predicate, object);
				if (var != null) {
					return new VariableNode(var.getName());
				}
				var = findVariableInTripleForReuse(((Rule)target).getThens(), subject, predicate, object);
				if (var != null) {
					return new VariableNode(var.getName());
				}
			}
		}
		return new VariableNode(getNewVar(expr));
	}
	
	protected String getNewVar(Expression expr) {
		IScopeProvider scopeProvider = ((XtextResource)expr.eResource()).getResourceServiceProvider().get(IScopeProvider.class);
		IScope scope = scopeProvider.getScope(expr, SADLPackage.Literals.SADL_RESOURCE__NAME);
		String proposedName = "v" + vNum;
		while (userDefinedVariables.contains(proposedName)
				|| 	scope.getSingleElement(QualifiedName.create(proposedName)) != null) {
			vNum++;
			proposedName = "v" + vNum;
		}
		vNum++;
		return proposedName;
	}
	
	/**
	 * Supporting method for the method above (getVariableNode(Node, Node, Node))
	 * @param gpes
	 * @param subject
	 * @param predicate
	 * @param object
	 * @return
	 */
	protected VariableNode findVariableInTripleForReuse(List<GraphPatternElement> gpes, Node subject, Node predicate, Node object) {
		if (gpes != null) {
			Iterator<GraphPatternElement> itr = gpes.iterator();
			while (itr.hasNext()) {
				GraphPatternElement gpe = itr.next();
				while (gpe != null) {
					if (gpe instanceof TripleElement) {
						TripleElement tr = (TripleElement)gpe;
						Node tsn = tr.getSubject();
						Node tpn = tr.getPredicate();
						Node ton = tr.getObject();
						if (subject == null && tsn instanceof VariableNode) {
							if (predicate != null && predicate.equals(tpn) && object != null && object.equals(ton)) {
								return (VariableNode) tsn;
							}
						}
						if (predicate == null && tpn instanceof VariableNode) {
							if (subject != null && subject.equals(tsn) && object != null && object.equals(ton)) {
								return (VariableNode) tpn;
							}
						}
						if (object == null && ton instanceof VariableNode) {
							if (subject != null && subject.equals(tsn) && predicate != null && predicate.equals(tpn)) {
								return (VariableNode) ton;
							}
						}
					}
					gpe = gpe.getNext();
				}
			}
		}
		return null;
	}
	
}
