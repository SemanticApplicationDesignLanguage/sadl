package com.ge.research.sadl.perspective.handlers;

import java.awt.event.HierarchyBoundsAdapter;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.XtextResource;

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.model.CircularDefinitionException;
import com.ge.research.sadl.model.OntConceptType;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.utils.ResourceManager;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.RDFS;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.sADL.SadlResource;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;

public class GraphImports extends SadlActionHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			String[] validTargetTypes = {"owl"};
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

			SadlConsole.writeToConsole(MessageType.INFO, "Graph of imports of '" + trgtFile.getName() + "' requested.\n");

			boolean derivedFN = false;
			if (trgtFile.getName().endsWith("owl")) {
				owlFileName = trgtFile.getFullPath().lastSegment();
			}
			else {
				owlFileName = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(trgtFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment()).toPortableString());
				if (!(new File(owlFileName).exists())) {
					owlFileName = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(trgtFile.getFullPath().addFileExtension("owl").lastSegment()).toPortableString());
					if (!(new File(owlFileName).exists())) {
						SadlConsole.writeToConsole(MessageType.ERROR, "Invalid selection for graphing");
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
			Map<String,String> prefMap = getPreferences();
			String renderClass = prefMap.get(SadlPreferences.GRAPH_RENDERER_CLASS.getId());
			
			String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
			final String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
			IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format);

			List<IGraphVisualizer> visualizers = configMgr.getAvailableGraphRenderers();

			if (visualizers != null && visualizers.size() > 0) {
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
					SadlResource sr = (SadlResource) target[3];
					String srnm = declarationExtensions.getConcreteName(sr);
					OntConceptType srType = declarationExtensions.getOntConceptType(sr);
					if (srType.equals(OntConceptType.CLASS)) {
						ResultSet rs = sadlResourceToDomainRangeResultSet(configMgr, publicUri, sr);
						if (rs != null) {
							graphResultSet(visualizers.get(0), project, trgtFile, owlFileName+srnm+"dr", "dr", declarationExtensions.getConceptUri(sr), "Domains and ranges", rs);
						}
						else {
							SadlConsole.writeToConsole(MessageType.INFO, "No properties found for this class.");
						}
						rs = sadlResourceToClassHierarchy(configMgr, publicUri, sr);
						if (rs != null) {
							graphResultSet(visualizers.get(0), project, trgtFile, owlFileName+srnm+"ch", "ch", declarationExtensions.getConcreteName(sr), "Class hierarchy", rs);
						}
						else {
							SadlConsole.writeToConsole(MessageType.INFO, "No class hierarchy found for this class.");
						}					}
				}
				else {
					ResultSet rs = importsToResultSet(configMgr, publicUri, prefix, trgtFile, derivedFN);
					if (rs != null) {
						graphImportResultSet(visualizers.get(0), project, trgtFile, publicUri, prefix, rs);
					}
					else {
						SadlConsole.writeToConsole(MessageType.ERROR, "No imports found.");
					}
				}
			}
			else {
				SadlConsole.writeToConsole(MessageType.ERROR, "Unable to find an instance of IGraphVisualizer to render graph.");
			}
		}
		catch (Exception e) {
			SadlConsole.writeToConsole(MessageType.ERROR, e.getMessage() + "\n");
		}
		finally {
			
		}
		return event;
	}

	private void graphImportResultSet(IGraphVisualizer iGraphVisualizer, IProject project, IFile trgtFile, String publicUri, String prefix, ResultSet rs) throws IOException {
		String baseFileName = trgtFile.getFullPath().removeFileExtension().lastSegment().toString();
		String graphName = prefix;
		String anchorNode = nodeText(publicUri, prefix);
		String description = "Graph of Imports";
		graphResultSet(iGraphVisualizer, project, trgtFile, baseFileName, graphName, anchorNode, description, rs);
	}

	private void graphResultSet(IGraphVisualizer iGraphVisualizer, IProject project, IFile trgtFile, String baseFileName,
			String graphName, String anchorNode, String description, ResultSet rs) throws IOException {
		String tempDir = convertProjectRelativePathToAbsolutePath(project.getFullPath().append("Temp").append("Graphs").toPortableString()); 
		File tmpDirFile = new File(tempDir);
		tmpDirFile.mkdirs();
		iGraphVisualizer.initialize(tempDir, baseFileName, graphName, anchorNode, IGraphVisualizer.Orientation.TD, description);
		iGraphVisualizer.graphResultSetData(rs);
	}

	private ResultSet sadlResourceToDomainRangeResultSet(IConfigurationManagerForIDE configMgr, String publicUri, SadlResource sr) throws CircularDefinitionException, ConfigurationException, IOException {
		String srUri = declarationExtensions.getConceptUri(sr);
		OntModel m = configMgr.getOntModel(publicUri, Scope.INCLUDEIMPORTS);
		OntClass cls = m.getOntClass(srUri);
		if (cls != null) {
			List<String[]> domainRange = new ArrayList<String[]>();
			domainRange = addToDomainRangeGraph(domainRange, m, cls);
			return listToResultSet(domainRange);
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
		String srUri = declarationExtensions.getConceptUri(sr);
		OntModel m = configMgr.getOntModel(publicUri, Scope.INCLUDEIMPORTS);
		OntClass cls = m.getOntClass(srUri);
		if (cls != null) {
			List<String[]> clsHier = new ArrayList<String[]>();
			clsHier = addToClassHierarchy(cls, clsHier);
			return listToResultSet(clsHier);
		}
		return null;
	}
	
	private List<String[]> addToClassHierarchy(OntClass cls, List<String[]> hier) {
		ExtendedIterator<OntClass> eitr = cls.listSuperClasses(true);
		while (eitr.hasNext()) {
			OntClass supercls = eitr.next();
			String[] arc = new String[3];
			arc[2] = cls.getLocalName();
			arc[1] = "sub-type";
			arc[0] = supercls.getLocalName();
			hier.add(arc);
			hier = addToClassHierarchy(supercls, hier);
		}
		return hier;
	}

	private ResultSet importsToResultSet(IConfigurationManagerForIDE configMgr, String publicUri, String prefix, IFile trgtFile, boolean derivedFN) throws ConfigurationException, IOException {
		List<String[]> importList = new ArrayList<String[]>();
		importList = findImports(importList, configMgr, publicUri, prefix);
		if (importList != null && importList.size() > 0) {
			return listToResultSet(importList, trgtFile, derivedFN);
		}
		return null;
	}

	private ResultSet listToResultSet(List<String[]> importList, IFile trgtFile, boolean derivedFN) {
		String[][] data = new String[importList.size()][];
		for (int i = 0; i < importList.size(); i++) {
			data[i] = importList.get(i);
			if (derivedFN && data[i][2].startsWith("file:")) {
				data[i][2] = trgtFile.getFullPath().lastSegment();
			}
		}
		return new ResultSet(data);
	}

	private ResultSet listToResultSet(List<String[]> importList) {
		String[][] data = new String[importList.size()][];
		for (int i = 0; i < importList.size(); i++) {
			data[i] = importList.get(i);
		}
		return new ResultSet(data);
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

	private List<String[]> findImports(List<String[]> importList,
			IConfigurationManagerForIDE configMgr, String parentPublicUri, String parentPrefix) throws ConfigurationException, IOException {
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
					row[0] = nodeText(key, val);
					if (!rowAlreadyInList(importList,row)) {
						importList.add(row);
						importList = findImports(importList, configMgr, key, val);
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
