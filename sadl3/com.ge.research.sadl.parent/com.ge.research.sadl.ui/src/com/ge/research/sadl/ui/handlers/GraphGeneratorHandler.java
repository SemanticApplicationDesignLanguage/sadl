package com.ge.research.sadl.ui.handlers;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.xtext.findReferences.IReferenceFinder;
import org.eclipse.xtext.findReferences.TargetURIs;
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
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.Name;
import com.ge.research.sadl.sADL.SADLPackage;
import com.ge.research.sadl.sADL.SadlResource;
import com.ge.research.sadl.sADL.SadlSimpleTypeReference;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.visualize.GraphGenerator;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.inject.Inject;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.RDFS;

@SuppressWarnings("restriction")
public class GraphGeneratorHandler extends SadlActionHandler {

	@Inject
	public DeclarationExtensions declarationExtensions;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			String[] validTargetTypes = getValidTargetFileTypes();
			Object[] target = getCommandTarget(validTargetTypes);
			IProject project = null;
			IPath trgtFolder = null;
			IFile trgtFile = null;
			if (target != null) {
				if (target.length > 0) project = (IProject) target[0];
				if (target.length > 1) trgtFolder = (IPath) target[1];
				if (target.length > 2) trgtFile = (IFile) target[2];
			}
			String owlFileName = null;

			SadlConsole.writeToConsole(MessageType.INFO, "Generating graph of '" + trgtFile.getName() + "' requested.\n");

			boolean derivedFN = false;
			if (trgtFile.getName().endsWith("owl")) {
				owlFileName = trgtFile.getFullPath().lastSegment();
			}
			else {
				owlFileName = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(trgtFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment()).toPortableString());
				if (!(new File(owlFileName).exists())) {
					owlFileName = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(trgtFile.getFullPath().addFileExtension("owl").lastSegment()).toPortableString());
					if (!(new File(owlFileName).exists())) {
						SadlConsole.writeToConsole(MessageType.ERROR, "Invalid selection for graphing\n");
						return event;
					}
					else {
						owlFileName = trgtFile.getFullPath().addFileExtension("owl").lastSegment();
						derivedFN = true;
					}
				}
				else {
					owlFileName = trgtFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment();
					derivedFN = true;
				}
			}	
			
			String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
			final String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
			IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format);

			IGraphVisualizer visualizer = getVisualizer(configMgr);
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
						graphSadlResource(configMgr, visualizer, sr, project, trgtFile, owlFileName, publicUri, graphRadius);
					}
					else {
						SadlConsole.writeToConsole(MessageType.INFO, "Selected concept for graphing ('" + target[3].toString() + "') is not a SadlResource as expected.\n");
					}
				}
				else {
					int graphRadius = getGraphingRadius(10);		
					graphSelectedResourceImports(project, trgtFile, derivedFN, configMgr, visualizer, publicUri, prefix, graphRadius);
				}
			}
			else {
				SadlConsole.writeToConsole(MessageType.ERROR, "Unable to find an instance of IGraphVisualizer to render graph.\n");
			}
		}
		catch (Exception e) {
			SadlConsole.writeToConsole(MessageType.ERROR, e.getMessage() + "\n");
		}
		finally {
			
		}
		return event;
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

	protected void graphSelectedResourceImports(IProject project, IFile trgtFile, boolean derivedFN,
			IConfigurationManagerForIDE configMgr, IGraphVisualizer visualizer, String publicUri, String prefix, int graphRadius)
			throws ConfigurationException, IOException {
		ResultSet rs = importsToResultSet(configMgr, publicUri, prefix, trgtFile, derivedFN, graphRadius);
		if (rs != null) {
			graphImportResultSet(visualizer, project, trgtFile, publicUri, prefix, rs);
		}
		else {
			SadlConsole.writeToConsole(MessageType.ERROR, "No imports found.\n");
		}
	}
	
	protected void graphSadlResource(IConfigurationManagerForIDE configMgr, IGraphVisualizer visualizer, SadlResource sr,
			IProject project, IFile trgtFile, String owlFileName, String publicUri, int graphRadius)
			throws CircularDefinitionException, ConfigurationException, IOException {
		String srnm = getSadlResourceConcreteName(sr);
		OntConceptType srType = getSadlResourceOntConceptType(sr);
		if (srType.equals(OntConceptType.CLASS)) {
			GraphGenerator gg = new GraphGenerator(configMgr, publicUri, new ConceptName(getSadlResourceUri(sr)));
			ResultSet rs = gg.generateClassNeighborhood(graphRadius); //sadlResourceToDomainRangeResultSet(configMgr, publicUri, sr);
			if (rs != null) {
				graphResultSet(visualizer, project, trgtFile, owlFileName+srnm+"dr", "dr", getSadlResourceAnchor(sr), "Domains and ranges", rs, null);
			}
			else {
				SadlConsole.writeToConsole(MessageType.INFO, "No properties found for this class.\n");
			}
			rs = gg.generateClassHierarchy(graphRadius); //sadlResourceToClassHierarchy(configMgr, publicUri, sr);
			if (rs != null) {
				graphResultSet(visualizer, project, trgtFile, owlFileName+srnm+"ch", "ch", getSadlResourceAnchor(sr), "Class hierarchy", rs, null);
			}
			else {
				SadlConsole.writeToConsole(MessageType.INFO, "No class hierarchy found for this class.\n");
			}
		}
		else if (srType.equals(OntConceptType.CLASS_PROPERTY) ||
				srType.equals(OntConceptType.DATATYPE_PROPERTY) ||
				srType.equals(OntConceptType.RDF_PROPERTY)) {
			GraphGenerator gg = new GraphGenerator(configMgr, publicUri, new ConceptName(getSadlResourceUri(sr)));
			ResultSet rs = gg.generatePropertyNeighborhood(graphRadius);
			if (rs != null) {
				graphResultSet(visualizer, project, trgtFile, owlFileName+srnm+"dr", "dr", getSadlResourceAnchor(sr), "Domains and ranges", rs, null);
			}
			else {
				SadlConsole.writeToConsole(MessageType.INFO, "No information found for this property.\n");
			}
		}
		else if (srType.equals(OntConceptType.INSTANCE)) {
			GraphGenerator gg = new GraphGenerator(configMgr, publicUri, new ConceptName(getSadlResourceUri(sr)));
			ResultSet rs = gg.generateIndividualNeighborhood(graphRadius);
			if (rs != null) {
				graphResultSet(visualizer, project, trgtFile, owlFileName+srnm+"dr", "dr", getSadlResourceAnchor(sr), "Instance", rs, null);
			}
			else {
				SadlConsole.writeToConsole(MessageType.INFO, "No information found for this instance.\n");
			}
		}
		else {
			SadlConsole.writeToConsole(MessageType.INFO, "Graphing of concepts of type '" + srType.toString() + "' not yet supported.\n");
		}
	}

	protected String getSadlResourceAnchor(SadlResource sr) {
		String anchorNode = nodeText(getSadlResourceUri(sr), getSadlResourcePrefix(sr));
		return anchorNode;
	}

	private String getSadlResourcePrefix(SadlResource sr) {
		return declarationExtensions.getConceptPrefix(sr);
	}

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
	
	protected void graphImportResultSet(IGraphVisualizer iGraphVisualizer, IProject project, IFile trgtFile, String publicUri, String prefix, ResultSet rs) throws IOException {
		String baseFileName = trgtFile.getFullPath().removeFileExtension().lastSegment().toString();
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
		graphResultSet(iGraphVisualizer, project, trgtFile, baseFileName, graphName, anchorNode, description, rs, null);
	}
	
	
	
	protected void graphOntologyResultSet(IGraphVisualizer iGraphVisualizer, IProject project, IFile trgtFile, String publicUri, String prefix, ResultSet rs) throws IOException {
		String baseFileName = trgtFile.getFullPath().removeFileExtension().lastSegment().toString();// + "_ONT";
		String graphName = prefix;
		String anchorNode = nodeText(publicUri, prefix);
		if (prefix == null) {
			anchorNode = publicUri.substring(publicUri.lastIndexOf('/') + 1);
			if (anchorNode.endsWith(".owl")) {
				anchorNode = anchorNode.substring(0, anchorNode.length() - 4);
			}
			if (anchorNode.indexOf('.') > 0) {
				graphName = anchorNode.substring(0, anchorNode.indexOf('.'));
			}
			else {
				graphName = anchorNode;
			}
		}
		String description = "Graph of Ontology File";
		createGraphFromResultSet(iGraphVisualizer, project, trgtFile, baseFileName, graphName, anchorNode, description, rs);
	}

	private ResultSet sadlResourceToDomainRangeResultSet(IConfigurationManagerForIDE configMgr, String publicUri, SadlResource sr) throws CircularDefinitionException, ConfigurationException, IOException {
		String srUri = getSadlResourceUri(sr);
		OntModel m = configMgr.getOntModel(publicUri, Scope.INCLUDEIMPORTS);
		OntClass cls = m.getOntClass(srUri);
		if (cls != null) {
			List<String[]> domainRange = new ArrayList<String[]>();
			domainRange = addToDomainRangeGraph(domainRange, m, cls);
			String[] headers = null;
			return listToResultSet(headers, domainRange);
		}
		return null;
	}

	private List<String[]> addToDomainRangeGraph(List<String[]> domainRange, OntModel m, OntClass cls) {
		List<com.hp.hpl.jena.rdf.model.Resource> props = getPropertiesWithDomain(m, cls);
		if (props != null) {
			Iterator<com.hp.hpl.jena.rdf.model.Resource> propsitr = props.iterator();
			while (propsitr.hasNext()) {
				com.hp.hpl.jena.rdf.model.Resource prop = propsitr.next();
				StmtIterator sitr = m.listStatements(prop, RDFS.range, (RDFNode)null);
				while (sitr.hasNext()) {
					String[] triple = new String[3];
					triple[0] = cls.getLocalName();
					triple[1] = prop.getLocalName();
					RDFNode obj = sitr.nextStatement().getObject();
					triple[2] = obj.isURIResource() ? obj.asResource().getLocalName() : obj.toString();
					domainRange.add(triple);
					if (obj.canAs(OntClass.class)){
						domainRange = addToDomainRangeGraph(domainRange, m, obj.as(OntClass.class));
					}
				}
			}
		}
		return domainRange;
	}

	private ResultSet sadlResourceToClassHierarchy(IConfigurationManagerForIDE configMgr, String publicUri,
			SadlResource sr) throws ConfigurationException, IOException {
		String srUri = getSadlResourceUri(sr);
		OntModel m = configMgr.getOntModel(publicUri, Scope.INCLUDEIMPORTS);
		OntClass cls = m.getOntClass(srUri);
		if (cls != null) {
			List<String[]> clsHier = new ArrayList<String[]>();
			clsHier = addToClassHierarchy(cls, clsHier);
			String[] headers = null;
			return listToResultSet(headers, clsHier);
		}
		return null;
	}
	
	private List<String[]> addToClassHierarchy(OntClass cls, List<String[]> hier) {
		ExtendedIterator<OntClass> eitr = cls.listSuperClasses(true);
		while (eitr.hasNext()) {
			OntClass supercls = eitr.next();
			String[] arc = new String[3];
			arc[2] = cls.getLocalName();
			arc[1] = "subClass";
			arc[0] = supercls.getLocalName();
			hier.add(arc);
			hier = addToClassHierarchy(supercls, hier);
		}
		return hier;
	}

	protected ResultSet importsToResultSet(IConfigurationManagerForIDE configMgr, String publicUri, String prefix, IFile trgtFile, boolean derivedFN, int graphRadius) throws ConfigurationException, IOException {
		List<String[]> importList = new ArrayList<String[]>();
		importList = findImports(importList, configMgr, publicUri, prefix, graphRadius);
		findIncomingImports(trgtFile, importList, graphRadius);
		if (importList != null && importList.size() > 0) {
			String[] headers = null;
			return listToResultSet(headers, importList, trgtFile, derivedFN);
		}
		return null;
	}
	
	
	
	@Inject TargetURIConverter uriConverter;
	@Inject IReferenceFinder referenceFinder;
	@Inject EditorResourceAccess editorResourceAccess;
	@Inject IResourceDescriptions indexData;

	protected void findIncomingImports(IFile trgtFile, final List<String[]> imports, int graphRadius) {
		URI uri = URI.createPlatformResourceURI(trgtFile.getFullPath().toString(), true);
		findIncomingImportsByUri(imports, uri, graphRadius);
	}

	private void findIncomingImportsByUri(final List<String[]> imports, URI uri, final int graphRadius) {
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
				String[] entry = new String[3];
				String targetUri = targetDesc.getQualifiedName().toString();
				String targetAlias = targetDesc.getUserData("alias");
				entry[0] = nodeText(targetUri, targetAlias);
				entry[1] = "importedBy";
				String sourceUri = sourceDesc.getQualifiedName().toString();
				String sourceAlias = sourceDesc.getUserData("alias");
				entry[2] = nodeText(sourceUri, sourceAlias);
				imports.add(entry);
				URI sobjuri = sourceDesc.getEObjectURI();
				if (graphRadius > 0) {
					findIncomingImportsByUri(imports, sobjuri, graphRadius - 1);
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

	protected ResultSet listToResultSet(String[] headers, List<String[]> importList, IFile trgtFile, boolean derivedFN) {
		String[][] data = new String[importList.size()][];
		for (int i = 0; i < importList.size(); i++) {
			data[i] = importList.get(i);
			if (derivedFN && data[i][2].startsWith("file:")) {
				data[i][2] = trgtFile.getFullPath().lastSegment();
			}
		}
		if (headers != null && headers.length > 0) {
			return new ResultSet(headers, data);
		}
		return new ResultSet(data);
	}

	private ResultSet listToResultSet(String[] headers, List<String[]> importList) {
		if (importList.size() > 0) {
			String[][] data = new String[importList.size()][];
			for (int i = 0; i < importList.size(); i++) {
				data[i] = importList.get(i);
			}
			return new ResultSet(headers, data);
		}
		return null;
	}

	private List<com.hp.hpl.jena.rdf.model.Resource> getPropertiesWithDomain(OntModel m, OntClass cls) {
		StmtIterator ditr = m.listStatements(null, RDFS.domain, cls);
		if (ditr.hasNext()) {
			List<com.hp.hpl.jena.rdf.model.Resource> props = new ArrayList<com.hp.hpl.jena.rdf.model.Resource>();
			while (ditr.hasNext()) {
				com.hp.hpl.jena.rdf.model.Resource prop = ditr.nextStatement().getSubject();
				props.add(prop);
			}
			return props;
		}
		return null;
	}

	protected List<String[]> findImports(List<String[]> importList,
			IConfigurationManagerForIDE configMgr, String parentPublicUri, String parentPrefix, int graphRadius) throws ConfigurationException, IOException {
		Map<String,String> map = configMgr.getImports(parentPublicUri, Scope.LOCALONLY);
		if (map != null) {
			Iterator<String> itr = map.keySet().iterator();
			while (itr.hasNext()) {
				String key = itr.next();
				String val = map.get(key);
				if (!key.equals("http://sadl.org/sadlbasemodel") && !key.equals("http://sadl.org/sadlimplicitmodel")) {
					String[] row = new String[3];
					row[2] = nodeText(parentPublicUri, parentPrefix);
					row[1] = "importedBy";
					if (key.equals("http://sadl.org/sadllistmodel")) {
						row[0] = nodeText(key, "List");
					}
					else {
						row[0] = nodeText(key, val);
					}
						importList.add(row);
						if (!rowAlreadyInList(importList,row) && graphRadius > 0) {
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

	private boolean rowAlreadyInList(List<String[]> importList, String[] row) {
		if (row != null) {
			for (int i = 0; importList != null && i < importList.size(); i++) {
				String[] thisrow = importList.get(i);
				if (thisrow.length == row.length) {
					boolean allEqual = true;
					for (int j = 0; j < thisrow.length; j++) {
						if (!thisrow[j].equals(row[j])) {
							allEqual = false;
							break;
						}
					}
					if (allEqual) {
						return true;
					}
				}
			}
		}
		return false;
	}

}
