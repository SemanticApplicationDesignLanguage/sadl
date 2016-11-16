package com.ge.research.sadl.ui.handlers;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.visualize.OntologyGraphGenerator;
import com.ge.research.sadl.utils.ResourceManager;

public class OntologyGraphGeneratorHandler extends GraphGeneratorHandler {


	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			String[] validTargetTypes = getValidTargetFileTypes();
			Object[] target;
			target = getCommandTarget(validTargetTypes);
			
			IProject project = null;
			IPath trgtFolder = null;
			IFile trgtFile = null;
			if (target != null) {
				if (target.length > 0) project = (IProject) target[0];
				if (target.length > 1) trgtFolder = (IPath) target[1];
				if (target.length > 2) trgtFile = (IFile) target[2];
			}
			String owlFileName = null;
	
			SadlConsole.writeToConsole(MessageType.INFO, "Generate graph of '" + trgtFile.getName() + "' requested.\n");
		
			boolean derivedFN = false;
			//Get the associated .owl file for this sadl file
			if (trgtFile.getName().endsWith("owl")) {
				owlFileName = trgtFile.getFullPath().lastSegment();
			}
			else {
				owlFileName = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(trgtFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment()).toPortableString());
				if (!(new File(owlFileName).exists())) {
					owlFileName = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).append(trgtFile.getFullPath().addFileExtension("owl").lastSegment()).toPortableString());
					if (!(new File(owlFileName).exists())) {
						SadlConsole.writeToConsole(MessageType.ERROR, "Invalid selection for graphing: '" + owlFileName + "' does not exist.\n");
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
			
			//get the configuration manager
			String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
			final String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
			IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format);
			
			//get the visualizer interface
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
				
				int graphRadius = 99;	//so radius should not play a part	
				List<String[]> ontologyResults = gatherImports(configMgr, publicUri, prefix, trgtFile, derivedFN, graphRadius);
				OntologyGraphGenerator ogg = new OntologyGraphGenerator(configMgr, publicUri, project);
				ResultSet oggResults = ogg.generateOntologyResultSet(null, publicUri);
				
				if (oggResults != null) {
					graphImportResultSet(visualizer, project, trgtFile, publicUri, prefix, oggResults);
				}
				else {
					SadlConsole.writeToConsole(MessageType.ERROR, "No imports found.\n");
				}
				
				
			}
			
			
		} catch (Exception e) {
			SadlConsole.writeToConsole(MessageType.ERROR, e.getMessage() + "\n");
		}
		finally {
			
		}
		
		return event;
	}
	
	private List<String[]> gatherImports(IConfigurationManagerForIDE configMgr, String publicUri, 
			String prefix, IFile trgtFile, boolean derivedFN, 
			int graphRadius) throws ConfigurationException, IOException{
		List<String[]> importList = new ArrayList<String[]>();
		importList = findImports(importList, configMgr, publicUri, prefix, graphRadius);
		
		return importList;
	}
	
	
	protected String[] getValidTargetFileTypes(){
		String[] types = {"owl", "sadl"};
		return types;
	}

}
