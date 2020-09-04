package com.ge.research.sadl.ui.handlers;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CancellationException;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.resource.impl.ResourceImpl;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.xtext.findReferences.IReferenceFinder;
import org.eclipse.xtext.findReferences.TargetURIs;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.IReferenceDescription;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.ui.editor.findrefs.EditorResourceAccess;
import org.eclipse.xtext.ui.editor.findrefs.TargetURIConverter;

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.model.CircularDefinitionException;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.DeclarationExtensions;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.Name;
import com.ge.research.sadl.sADL.SADLPackage;
import com.ge.research.sadl.sADL.SadlResource;
import com.ge.research.sadl.sADL.SadlSimpleTypeReference;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.ge.research.sadl.ui.visualize.GraphGenerator;
import com.ge.research.sadl.ui.visualize.GraphGenerator.UriStrategy;
import com.ge.research.sadl.ui.visualize.GraphSegment;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.inject.Inject;
import com.google.inject.Injector;

@SuppressWarnings("restriction")
public class GraphGeneratorHandler extends SadlActionHandler {

	@Inject
	public DeclarationExtensions declarationExtensions;
	
	public static final String GRAPH_JOB = "GraphJob";

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			String[] validTargetTypes = getValidTargetFileTypes();
			Object[] target = getCommandTarget(validTargetTypes);
			project = null;
			IPath trgtFolder = null;
			IFile trgtFile = null;
			if (target != null) {
				if (target.length > 0) project = (IProject) target[0];
				if (target.length > 1) trgtFolder = (IPath) target[1];
				if (target.length > 2) trgtFile = (IFile) target[2];
			}
			String owlFileName = null;
			if (project == null || trgtFile == null) {
				console.error("Nothing is selected for graphing. Please select at least a project or click in open editor to select file or concept.\n");
				return event;
			}

//			SadlConsole.writeToConsole(MessageType.INFO, "Generating graph of '" + trgtFile.getName() + "' requested.\n");

			Map<String,String> prefMap = getPreferences(trgtFile);
			boolean derivedFN = false;
			String fmt = prefMap.get(SadlPreferences.OWL_MODEL_FORMAT.getId());
			String fileExt = SadlSerializationFormat.getFileExtension(SadlSerializationFormat.getRDFFormat(fmt));
			if (trgtFile.getName().endsWith(fileExt)) {
				owlFileName = trgtFile.getFullPath().lastSegment();
			}
			else {
				owlFileName = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(trgtFile.getFullPath().removeFileExtension().addFileExtension(fileExt).lastSegment()).toPortableString());
				if (!(new File(owlFileName).exists())) {
					String owlFileName2 = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(trgtFile.getFullPath().addFileExtension(fileExt).lastSegment()).toPortableString());
					if (!(new File(owlFileName2).exists())) {
						console.error("Selected file is '" + trgtFile.getName() + "' but corresponding OWL file (" + owlFileName + ") not found.\n");
						return event;
					}
					else {
						owlFileName = trgtFile.getFullPath().addFileExtension(fileExt).lastSegment();
						derivedFN = true;
					}
				}
				else {
					owlFileName = trgtFile.getFullPath().removeFileExtension().addFileExtension(fileExt).lastSegment();
					derivedFN = true;
				}
			}	
			
			String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
			final String format = SadlSerializationFormat.RDF_XML_ABBREV_FORMAT;
			IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format);

			IGraphVisualizer visualizer = getVisualizer(configMgr, prefMap);
			if (visualizer != null) {
				String publicUri;
				String prefix = null;
				try {
					publicUri = configMgr.getPublicUriFromActualUrl(new SadlUtils().fileNameToFileUrl(modelFolderUri + "/" + owlFileName));
					prefix = configMgr.getGlobalPrefix(publicUri);
				}
				catch (Exception e) {
					publicUri = new SadlUtils().fileNameToFileUrl(modelFolderUri + "/" + owlFileName);
				}
				
				if (target.length > 3 && target[3] != null) {
					SadlResource sr = getSadlResource(target[3]);
					if (sr != null) {
						int graphRadius = getGraphingRadius(5);	
						executeGraphingMethod(this, "graphSadlResource", new Object[]{configMgr, visualizer, sr, project, trgtFile, owlFileName, publicUri, graphRadius});
						//graphSadlResource(configMgr, visualizer, sr, project, trgtFile, owlFileName, publicUri, graphRadius);
					}
					else {
						console.info("Selected concept for graphing ('" + target[3].toString() + "') is not a SadlResource as expected.\n");
					}
				}
				else {
					int graphRadius = getGraphingRadius(10);	
					executeGraphingMethod(this, "graphAnchoredImports", new Object[]{visualizer, configMgr, trgtFile, publicUri, prefix, graphRadius, derivedFN});
					//graphAnchoredImports(visualizer, configMgr, trgtFile, publicUri, prefix, graphRadius, derivedFN);					
				}
			}
			else {
				console.error("Unable to find an instance of IGraphVisualizer to render graph.\n");
			}
		}
		catch (Exception e) {
			console.error(e.getMessage() + "\n");
		}
		finally {
			
		}
		return event;
	}

	protected void generateGraphJob(Object c, Method graphingMethod, Object[] args){
		Job graphJob = new Job("Generating Graph"){

			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					Object[] argsPlus = new Object[args.length+1];
					for(int i = 0; i < args.length; i++){
						argsPlus[i] = args[i];
					}
					argsPlus[args.length] = monitor;
					graphingMethod.invoke(c, argsPlus);
				}catch (Exception e) {
					e.printStackTrace();
					return Status.CANCEL_STATUS;
				}
				return Status.OK_STATUS;
			}
			
			@Override 
			public boolean belongsTo(Object family){
				return family.toString().equals(GRAPH_JOB) ? true : false;
			}
		};
		graphJob.schedule();
	}
	
	protected void executeGraphingMethod(Object classInstance, String methodName, Object[] args){
		try {
			Method graphingMethod = null;
			for(Method m :  classInstance.getClass().getMethods()){
				if(m.getName().equals(methodName)){
					graphingMethod = m;
					break;
				}
			}
			if(graphingMethod != null){
				generateGraphJob(classInstance, graphingMethod, args);
			}else{
				throw new NoSuchMethodException();
			}
		} catch (NoSuchMethodException | SecurityException e) {
			e.printStackTrace();
		}
	}
	
	protected void safeWriteToConsole(MessageType mType, String message){
		Display.getDefault().asyncExec(new Runnable(){
			@Override
			public void run() {
				if (console != null) {
					console.print(mType, message);
				}
				else if (mType.equals(MessageType.INFO)){
					System.out.println(message);
				}
				else {
					System.err.println(message);
				}
			}
		});
	}
	
	public void graphAnchoredImports(IGraphVisualizer visualizer, IConfigurationManagerForIDE configMgr,
			IFile trgtFile, String publicUri, String prefix, int graphRadius, boolean derivedFN, IProgressMonitor monitor)
			throws ConfigurationException, IOException, InvalidNameException, Exception {
		GraphGenerator gg = new GraphGenerator(configMgr, visualizer, project, publicUri, new ConceptName(publicUri), monitor);
		gg.setUriStrategy(UriStrategy.QNAME_WITH_URI_TOOLTIP);
		List<GraphSegment> imports = getAnchoredImports(configMgr, publicUri, prefix, trgtFile, derivedFN, graphRadius);
		ResultSet rs = null;
		if (imports == null) {
			Object[][] rsTable = new Object[1][3];
			NamedNode thisNode = new NamedNode(publicUri);
			rsTable[0][0] = ((NamedNode)thisNode).toFullyQualifiedString();
			rsTable[0][1] = null;
			rsTable[0][2] = null;
			String[] colNames = new String[]{"head", "edge", "tail"};
			rs = new ResultSet(colNames, rsTable);
		}
		else {
			rs = gg.convertDataToResultSet(imports, UriStrategy.QNAME_IF_IMPORT, prefix);
		}
		String tempDir = convertProjectRelativePathToAbsolutePath(getGraphDir(project)); 
		File tmpDirFile = new File(tempDir);
		tmpDirFile.mkdirs();
		String baseFileName = "imports_of_" + gg.getBaseFilenameFromPublicUri(publicUri);
		String graphName = prefix;
		String anchorNode = prefix;
		String description = "Imports";
		graphResultSet(visualizer, project, baseFileName, graphName, anchorNode, description, rs, null, true);
	}

	protected int getGraphingRadius(int defaultSize) {
		Shell shell = new Shell();
		InputDialog dlg = new InputDialog(
				shell,
				"Graph Radius",
				"How many edges from anchor?",
				"" + defaultSize,
				null);
		dlg.open();
		if (dlg.getReturnCode() == Window.CANCEL) {
			throw new CancellationException();
		}
		if (dlg.getReturnCode() != Window.OK) {
			return defaultSize;
		}
		String strval = dlg.getValue();
		try {
			int intVal = Integer.parseInt(strval.trim());
			return intVal;
		}
		catch (Throwable t) {
			t.printStackTrace();
		}
		return defaultSize;
	}

	public void graphSadlResource(IConfigurationManagerForIDE configMgr, IGraphVisualizer visualizer, SadlResource sr,
			IProject project, IFile trgtFile, String owlFileName, String publicUri, int graphRadius, IProgressMonitor monitor)
			throws CircularDefinitionException, ConfigurationException, IOException, InvalidNameException, URISyntaxException {
		String srnm = getSadlResourceConcreteName(sr);
		OntConceptType srType = getSadlResourceOntConceptType(sr);
		if (srType.equals(OntConceptType.CLASS)) {
			GraphGenerator gg = new GraphGenerator(configMgr, visualizer, project, publicUri, new ConceptName(getSadlResourceUri(sr)), monitor, getOntologyGraphPreferences());
			ResultSet rs = gg.generateClassNeighborhood(graphRadius); //sadlResourceToDomainRangeResultSet(configMgr, publicUri, sr);
			if (rs != null) {
				graphResultSet(visualizer, project, owlFileName+srnm+"dr", "dr", getSadlResourceAnchor(sr, gg.getUriStrategy()), "Domains and ranges", rs, null, true);
			}
			else {
				safeWriteToConsole(MessageType.INFO, "No neighborhood graph generated for this class.\n");
			}
			rs = gg.generateClassHierarchy(graphRadius); //sadlResourceToClassHierarchy(configMgr, publicUri, sr);
			if (rs != null) {
				graphResultSet(visualizer, project, owlFileName+srnm+"ch", "ch", getSadlResourceAnchor(sr, gg.getUriStrategy()), "Class hierarchy", rs, null, true);
			}
			else {
				safeWriteToConsole(MessageType.INFO, "No class hierarchy found for this class.\n");
			}
		}
		else if (srType.equals(OntConceptType.CLASS_PROPERTY) ||
				srType.equals(OntConceptType.DATATYPE_PROPERTY) ||
				srType.equals(OntConceptType.RDF_PROPERTY)) {
			GraphGenerator gg = new GraphGenerator(configMgr, visualizer, project, publicUri, new ConceptName(getSadlResourceUri(sr)), monitor);
			ResultSet rs = gg.generatePropertyNeighborhood(graphRadius);
			if (rs != null) {
				graphResultSet(visualizer, project, owlFileName+srnm+"dr", "dr", getSadlResourceAnchor(sr, gg.getUriStrategy()), "Domains and ranges", rs, null, true);
			}
			else {
				safeWriteToConsole(MessageType.INFO, "No information found for this property.\n");
			}
		}
		else if (srType.equals(OntConceptType.INSTANCE)) {
			GraphGenerator gg = new GraphGenerator(configMgr, visualizer, project, publicUri, new ConceptName(getSadlResourceUri(sr)), monitor);
			ResultSet rs = gg.generateIndividualNeighborhood(graphRadius);
			if (rs != null) {
				graphResultSet(visualizer, project, owlFileName+srnm+"dr", "dr", getSadlResourceAnchor(sr, gg.getUriStrategy()), "Instance", rs, null, true);
			}
			else {
				safeWriteToConsole(MessageType.INFO, "No information found for this instance.\n");
			}
		}
		else {
			safeWriteToConsole(MessageType.INFO, "Graphing of concepts of type '" + srType.toString() + "' not yet supported.\n");
		}
	}

	protected String getSadlResourceAnchor(SadlResource sr, UriStrategy uriStrategy) throws ConfigurationException, CircularDefinitionException, InvalidNameException, URISyntaxException {
		GraphSegment gsDummy = new GraphSegment(null, null, null, null, getConfigMgr());
		gsDummy.setUriStrategy(uriStrategy);
//		OntConceptType ct = declarationExtensions.getOntConceptType(sr);
		String sruri = declarationExtensions.getConceptUri(sr);
		ConceptName cn = new ConceptName(sruri);
		cn.setPrefix(getConfigMgr().getGlobalPrefix(cn.getNamespace()));
		String anchorNode = gsDummy.conceptNameToString(cn);  // nodeText(getSadlResourceUri(sr), getSadlResourcePrefix(sr));
		return anchorNode;
	}

//	private String getSadlResourcePrefix(SadlResource sr) {
//		return declarationExtensions.getConceptPrefix(sr);
//	}

	protected SadlResource getSadlResource(Object target3) throws IOException {
		if (target3 instanceof Name) {
			return ((Name)target3).getName();
		}
		else if (target3 instanceof SadlSimpleTypeReference) {
			return ((SadlSimpleTypeReference)target3).getType();
		}
		else if (target3 instanceof SadlResource) {
			return (SadlResource) target3;
		}
		throw new IOException("Selection '" + target3.toString() + "' is a '" + target3.getClass().getCanonicalName() + "', don't know how to get a SadlResource from it.");
	}

	protected String getSadlResourceUri(SadlResource sr) {
		return declarationExtensions.getConceptUri(sr);
	}
 
	protected OntConceptType getSadlResourceOntConceptType(SadlResource sr) throws CircularDefinitionException {
		if (sr instanceof Name && ((Name)sr).getName() != null) {
			return declarationExtensions.getOntConceptType(((Name)sr).getName());
		}
		OntConceptType srType = declarationExtensions.getOntConceptType(sr);
		return srType;
	}

	protected String getSadlResourceConcreteName(SadlResource sr) {
		String srnm = declarationExtensions.getConcreteName(sr);
		return srnm;
	}

	protected String[] getValidTargetFileTypes() {
		String[] types = {"owl", "sadl"};
		return types;
	}
	
	public void graphImportResultSet(IGraphVisualizer iGraphVisualizer, IProject project, IFile trgtFile, String publicUri, String prefix, ResultSet rs) throws IOException {
		String baseFileName = trgtFile.getFullPath().lastSegment().toString();
		String graphName = prefix;
		String anchorNode = nodeText(publicUri, prefix);
		if (prefix == null) {
			anchorNode = publicUri.substring(publicUri.lastIndexOf('/') + 1);
			if (anchorNode.endsWith(".owl")) {
				anchorNode = anchorNode.substring(0, anchorNode.length() - 4);
			}
			graphName = anchorNode.substring(0, anchorNode.indexOf('.'));
		}
		String description = "Graph of Imports";
		graphResultSet(iGraphVisualizer, project, baseFileName, graphName, anchorNode, description, rs, null, true);
	}
	
//	private ResultSet sadlResourceToDomainRangeResultSet(IConfigurationManagerForIDE configMgr, String publicUri, SadlResource sr) throws CircularDefinitionException, ConfigurationException, IOException {
//		String srUri = getSadlResourceUri(sr);
//		OntModel m = configMgr.getOntModel(publicUri, Scope.INCLUDEIMPORTS);
//		OntClass cls = m.getOntClass(srUri);
//		if (cls != null) {
//			List<String[]> domainRange = new ArrayList<String[]>();
//			domainRange = addToDomainRangeGraph(domainRange, m, cls);
//			String[] headers = null;
//			return listToResultSet(headers, domainRange);
//		}
//		return null;
//	}

//	private List<String[]> addToDomainRangeGraph(List<String[]> domainRange, OntModel m, OntClass cls) {
//		List<org.apache.jena.rdf.model.Resource> props = getPropertiesWithDomain(m, cls);
//		if (props != null) {
//			Iterator<org.apache.jena.rdf.model.Resource> propsitr = props.iterator();
//			while (propsitr.hasNext()) {
//				org.apache.jena.rdf.model.Resource prop = propsitr.next();
//				StmtIterator sitr = m.listStatements(prop, RDFS.range, (RDFNode)null);
//				while (sitr.hasNext()) {
//					String[] triple = new String[3];
//					triple[0] = cls.getLocalName();
//					triple[1] = prop.getLocalName();
//					RDFNode obj = sitr.nextStatement().getObject();
//					triple[2] = obj.isURIResource() ? obj.asResource().getLocalName() : obj.toString();
//					domainRange.add(triple);
//					if (obj.canAs(OntClass.class)){
//						domainRange = addToDomainRangeGraph(domainRange, m, obj.as(OntClass.class));
//					}
//				}
//			}
//		}
//		return domainRange;
//	}

//	private ResultSet sadlResourceToClassHierarchy(IConfigurationManagerForIDE configMgr, String publicUri,
//			SadlResource sr) throws ConfigurationException, IOException {
//		String srUri = getSadlResourceUri(sr);
//		OntModel m = configMgr.getOntModel(publicUri, Scope.INCLUDEIMPORTS);
//		OntClass cls = m.getOntClass(srUri);
//		if (cls != null) {
//			List<String[]> clsHier = new ArrayList<String[]>();
//			clsHier = addToClassHierarchy(cls, clsHier);
//			String[] headers = null;
//			return listToResultSet(headers, clsHier);
//		}
//		return null;
//	}
	
//	private List<String[]> addToClassHierarchy(OntClass cls, List<String[]> hier) {
//		ExtendedIterator<OntClass> eitr = cls.listSuperClasses(true);
//		while (eitr.hasNext()) {
//			OntClass supercls = eitr.next();
//			String[] arc = new String[3];
//			arc[2] = cls.getLocalName();
//			arc[1] = "subClass";
//			arc[0] = supercls.getLocalName();
//			hier.add(arc);
//			hier = addToClassHierarchy(supercls, hier);
//		}
//		return hier;
//	}

	protected List<GraphSegment> getAnchoredImports(IConfigurationManagerForIDE configMgr, String publicUri, String prefix, IFile trgtFile, boolean derivedFN, int graphRadius) throws ConfigurationException, IOException {
		List<GraphSegment> importList = new ArrayList<GraphSegment>();
		importList = findImports(importList, configMgr, publicUri, prefix, graphRadius);
		findIncomingImports(trgtFile, importList, graphRadius);
		if (importList != null && importList.size() > 0) {
			return importList;
		}
		return null;
	}
	
	
	
	@Inject TargetURIConverter uriConverter;
	@Inject IReferenceFinder referenceFinder;
	@Inject EditorResourceAccess editorResourceAccess;
	@Inject IResourceDescriptions indexData;
	protected IConfigurationManagerForIDE configMgr;
	protected IProject project;
	protected String modelFolderUri;

	protected void findIncomingImports(IFile trgtFile, final List<GraphSegment> importList, int graphRadius) {
		URI uri = URI.createPlatformResourceURI(trgtFile.getFullPath().toString(), true);
		findIncomingImportsByUri(importList, uri, graphRadius);
	}

	private void findIncomingImportsByUri(final List<GraphSegment> importList, URI uri, final int graphRadius) {
		final IEObjectDescription targetDesc = getModelOf(uri);
		TargetURIs targetURIs = uriConverter.fromIterable(Collections.singleton(targetDesc.getEObjectURI()));
		referenceFinder.findAllReferences(targetURIs, editorResourceAccess, indexData, new IReferenceFinder.Acceptor() {
			
			@Override
			public void accept(EObject source, URI sourceURI, EReference eReference, int index, EObject targetOrProxy,
					URI targetURI) {
				// ignore
			}
			
			@Override
			public void accept(IReferenceDescription description) {
				IEObjectDescription sourceDesc = getModelOf(description.getSourceEObjectUri());
				String targetUri = targetDesc.getQualifiedName().toString();
				String targetAlias = targetDesc.getUserData("alias");
				String subj = targetUri; //nodeText(targetUri, targetAlias);
				String pred = "importedBy";
				String sourceUri = sourceDesc.getQualifiedName().toString();
				String sourceAlias = sourceDesc.getUserData("alias");
				String obj = sourceUri; //nodeText(sourceUri, sourceAlias);
				GraphSegment gs;
				try {
					if (graphRadius > 0) {
						if (!(targetUri.equals(sourceUri))) {
							gs = new GraphSegment(null, subj, pred, obj, getConfigMgr());
							if (!importList.contains(gs)) {
								importList.add(gs);
								URI sobjuri = sourceDesc.getEObjectURI();
								findIncomingImportsByUri(importList, sobjuri, graphRadius - 1);
							}
						}
					}
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (URISyntaxException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}, null);
	}
	
	private IEObjectDescription getModelOf(URI uri) {
		IResourceDescription sourceResourceDescription = indexData.getResourceDescription(uri.trimFragment());
		for (IEObjectDescription desc: sourceResourceDescription.getExportedObjects()) {
			if (desc.getEClass() == SADLPackage.Literals.SADL_MODEL) {
				return desc;
			}
		}
		return null;
	}

//	protected ResultSet listToResultSet(String[] headers, List<GraphSegment> importList, IFile trgtFile, boolean derivedFN) {
//		String[][] data = new String[importList.size()][];
//		for (int i = 0; i < importList.size(); i++) {
//			data[i] = importList.get(i);
//			if (derivedFN && data[i][2].startsWith("file:")) {
//				data[i][2] = trgtFile.getFullPath().lastSegment();
//			}
//		}
//		if (headers != null && headers.length > 0) {
//			return new ResultSet(headers, data);
//		}
//		return new ResultSet(data);
//	}

//	private ResultSet listToResultSet(String[] headers, List<String[]> importList) {
//		if (importList.size() > 0) {
//			String[][] data = new String[importList.size()][];
//			for (int i = 0; i < importList.size(); i++) {
//				data[i] = importList.get(i);
//			}
//			return new ResultSet(headers, data);
//		}
//		return null;
//	}

//	private List<org.apache.jena.rdf.model.Resource> getPropertiesWithDomain(OntModel m, OntClass cls) {
//		StmtIterator ditr = m.listStatements(null, RDFS.domain, cls);
//		if (ditr.hasNext()) {
//			List<org.apache.jena.rdf.model.Resource> props = new ArrayList<org.apache.jena.rdf.model.Resource>();
//			while (ditr.hasNext()) {
//				org.apache.jena.rdf.model.Resource prop = ditr.nextStatement().getSubject();
//				props.add(prop);
//			}
//			return props;
//		}
//		return null;
//	}

	protected List<GraphSegment> findImports(List<GraphSegment> importList,
			IConfigurationManagerForIDE configMgr, String parentPublicUri, String parentPrefix, int graphRadius) throws ConfigurationException, IOException {
		Map<String,String> map = configMgr.getImports(parentPublicUri, Scope.LOCALONLY);
		if (map != null) {
			Iterator<String> itr = map.keySet().iterator();
			while (itr.hasNext()) {
				String key = itr.next();
				String val = map.get(key);
				if (!key.equals(SadlConstants.SADL_BASE_MODEL_URI) && !key.equals(SadlConstants.SADL_IMPLICIT_MODEL_URI) &&
						!key.equals(IReasoner.SADL_BUILTIN_FUNCTIONS_URI)) {
					String obj = parentPublicUri; //nodeText(parentPublicUri, parentPrefix);
					String pred = "importedBy";
					String subj = key;
//					if (key.equals("http://sadl.org/sadllistmodel")) {
//						subj = nodeText(key, "List");
//					}
//					else {
//						subj = nodeText(key, val);
//					}
					GraphSegment gs = new GraphSegment(null, subj, pred, obj, configMgr);
					if (!importList.contains(gs) && graphRadius > 0) {
						importList.add(gs);
						importList = findImports(importList, configMgr, key, val, graphRadius - 1);
					}
				}
			}
		}
		return importList;
	}

	private String nodeText(String publicUri, String prefix) {
		if (prefix == null) {
			return publicUri;
		}
		else if (publicUri.contains("#")) {
			return prefix + ":" + publicUri.substring(publicUri.indexOf("#") + 1);
		}
		return prefix + " (" + publicUri + ")";
	}

	protected List<String> safeGetCurrentProject() {
		List<String> returnStrings = new ArrayList<String>();
		Display.getDefault().syncExec(new Runnable(){
			@Override
			public void run() {
				IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
			    if (window != null)
			    {
			        ISelection selection = window.getSelectionService().getSelection();
			        if (selection instanceof IStructuredSelection) {
				        Object firstElement = ((IStructuredSelection) selection).getFirstElement();
				        if (firstElement instanceof IAdaptable)
				        { 
				        	String[] selarray = firstElement.toString().split("/");
				        	if (selarray != null) {
				        		for (int i = 0; i < selarray.length; i++) {
				        			returnStrings.add(selarray[i]);
				        		}
				        	}
				        }
			        }
			    }
			}
		});
		return returnStrings;
	}

	protected IConfigurationManagerForIDE getConfigMgr() throws ConfigurationException, URISyntaxException {
		if (configMgr == null || !sameProject()) {
			//create the configuration manager
			String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
			final String format = SadlSerializationFormat.RDF_XML_ABBREV_FORMAT;
			setConfigMgr(ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format));
		}
		return configMgr;
	}
	
	private boolean sameProject() throws URISyntaxException {
		String pp1 = configMgr.getProjectFolderPath();
		if (pp1.endsWith("/")) {
			pp1 = pp1.substring(0, pp1.length() - 1);
		}
		int lstslash = pp1.lastIndexOf('/');
		if (lstslash > 0) {
			pp1 = pp1.substring(lstslash + 1);
		}
		String pp2 = project.getName();
		if (pp1.equals(pp2)) {
			return true;
		}
		return false;
	}

	private void setConfigMgr(IConfigurationManagerForIDE configMgr) {
		this.configMgr = configMgr;
	}

	protected Map<String,Boolean> getOntologyGraphPreferences() {
		Injector sadlInjector = safeGetInjector(SadlActivator.COM_GE_RESEARCH_SADL_SADL);
		IPreferenceValuesProvider pvp = sadlInjector.getInstance(IPreferenceValuesProvider.class);
		org.eclipse.emf.ecore.resource.Resource resource = new ResourceImpl();
		resource.setURI(org.eclipse.emf.common.util.URI.createFileURI("/"));
	
		IPreferenceValues preferenceValues = pvp.getPreferenceValues(resource);
		if (preferenceValues != null) {
			Map<String, Boolean> map = new HashMap<String, Boolean>();
			boolean bval = Boolean.parseBoolean(preferenceValues.getPreference(SadlPreferences.GRAPH_IMPLICIT_ELEMENTS));
			if (bval) {
				map.put(SadlPreferences.GRAPH_IMPLICIT_ELEMENTS.getId(), true);
			}
			else {
				map.put(SadlPreferences.GRAPH_IMPLICIT_ELEMENTS.getId(), false);
			}
			bval = Boolean.parseBoolean(preferenceValues.getPreference(SadlPreferences.GRAPH_IMPLICIT_ELEMENT_INSTANCES));
			if (bval) {
				map.put(SadlPreferences.GRAPH_IMPLICIT_ELEMENT_INSTANCES.getId(), true);
			}
			else {
				map.put(SadlPreferences.GRAPH_IMPLICIT_ELEMENT_INSTANCES.getId(), false);
			}			
			return map;
		}
		return null;
	}

}
