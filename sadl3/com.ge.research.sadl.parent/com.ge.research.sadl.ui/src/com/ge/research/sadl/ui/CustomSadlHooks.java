package com.ge.research.sadl.ui;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.internal.resources.Workspace;
import org.eclipse.core.resources.IBuildConfiguration;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.errorgenerator.generator.SadlErrorGenerator;
import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.IModelProcessor;
import com.ge.research.sadl.processing.ISadlImplicitModelContentProvider;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.processing.SadlModelProcessorProvider;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.ui.preferences.SadlPreferencesProvider;
import com.ge.research.sadl.utils.ResourceManager;
import com.ge.research.sadl.utils.SadlConsole;
import com.google.inject.Inject;

@SuppressWarnings("restriction")public class CustomSadlHooks implements IResourceChangeListener {

	private SadlEclipseConsole sc;
	
	private IProject project = null;

//	@Inject RequirementsPreferencesProvider requirementsPreferenceProvider;
	@Inject SadlPreferencesProvider sadlPreferenceProvider;
	
	@Inject
	protected IResourceSetProvider resourceSetProvider;

	@Inject	
	protected SadlModelProcessorProvider processorProvider;
	
	@Inject
	private ISadlImplicitModelContentProvider implicitModelContentProvider;
	
	protected IModelProcessor processor;
	
	private long startTime;
	private List<IProject> projectsSelected = null;
	private List<IProject> projectsInWorkspace = null;

	@Override
	public void resourceChanged(IResourceChangeEvent event) {
		//Check for SADLImplicitModel upon opening a project
		if(event.getType() == IResourceChangeEvent.PRE_BUILD){
			IResourceDelta rootDelta = event.getDelta();
			IResourceDelta[] childDeltas = rootDelta.getAffectedChildren(IResourceDelta.CHANGED);
			for(IResourceDelta delta : childDeltas){
				if((delta.getFlags() & IResourceDelta.OPEN) != 0){
					IResource resource = delta.getResource();
					if(((resource.getType() & IResource.PROJECT) != 0) && resource.getProject().isOpen()){
						OnOpenCheck(resource);
						break;
					}
				}
			}
		}
		
		
		if (event.getBuildKind() == IncrementalProjectBuilder.CLEAN_BUILD && 
				(event.getType() == IResourceChangeEvent.PRE_BUILD)) { // || event.getType() == IResourceChangeEvent.POST_BUILD)) {
			if (event.getSource() instanceof IProject) {
				beforeCleanBuild((IProject)event.getSource());
			}
			else if (event.getSource() instanceof Workspace) {
				IBuildConfiguration[] buildOrder = ((Workspace)event.getSource()).getBuildOrder();
				for (int i = 0; i < buildOrder.length; i++) {
					beforeCleanBuild(buildOrder[i].getProject());
				}
			}
		}
		else {
			if (event.getType() == IResourceChangeEvent.POST_BUILD && event.getBuildKind() == IncrementalProjectBuilder.AUTO_BUILD ) {
				sc = SadlEclipseConsole.getInstance();
				if (projectsSelected != null && projectsSelected.size() > 0) {
					for (int i = 0; i < projectsSelected.size(); i++) {
						setProject(projectsSelected.get(i));
//						afterCleanBuild(sc);
					}
					sc.writeToConsole(MessageType.INFO, "SADL clean/build process completed for " + projectsSelected.size() + (projectsSelected.size() > 1 ? " projects in " : " project in ") + getBuildDuration() + "\n");
					projectsSelected.clear();
				}
				else if (event.getSource() instanceof IWorkspace && (projectsInWorkspace == null || projectsInWorkspace.size() == 0)) {
					if (projectsInWorkspace == null) {
						projectsInWorkspace = new ArrayList<IProject>();
					}
					IResourceDelta rsrdelta = event.getDelta();
					IResourceDelta[] deltas = rsrdelta.getAffectedChildren();
					int cntr = 0;
					for (int i = 0; i < deltas.length; i++) {
						IProject prj = deltas[i].getResource().getProject();
						if (!projectsInWorkspace.contains(prj)) {
							setProject(prj);
//							afterCleanBuild(sc);
							projectsInWorkspace.add(prj);
							cntr++;
						}
						else {
							projectsInWorkspace.remove(prj);
						}
					}
					if (cntr > 0) {
						sc.writeToConsole(MessageType.INFO, "Build process completed for " + cntr + (cntr > 1 ? " projects.\n" : " project.\n"));
					}
				}
			}
		}
	}

	private String getBuildDuration() {
		long lElapsedTime = System.currentTimeMillis() - startTime;
		int lDays = (int) (lElapsedTime / (1000*60*60*24));
		int lHours = (int) ((lElapsedTime / (1000*60*60)) % 24);
		int lMinutes = (int) ((lElapsedTime / (1000*60)) % 60);
		int lSeconds = (int) (lElapsedTime / 1000) % 60 ;
		if(lDays > 0) {
			return String.format("%d days, %d hours, %d mins, %d seconds", lDays, lHours, lMinutes, lSeconds);
		}else if(lHours > 0) {
			return String.format("%d hours, %d mins, %d seconds", lHours, lMinutes, lSeconds);
		}else if (lMinutes > 0){
			return String.format("%d mins, %d seconds", lMinutes, lSeconds);
		}else {
			return String.format("%d seconds", lSeconds);
		}
	}
	
	private void OnOpenCheck(IResource resource) {
		IProject project = resource.getProject();
		checkForSadlImplicitModel(project);
		checkForBuiltinFunctionImplicitModel(project);
	}
	
	private void checkForBuiltinFunctionImplicitModel(IProject project) {
		try {
			IFile file = project.getFile(SadlConstants.SADL_IMPLICIT_MODEL_FOLDER + "/" + SadlConstants.SADL_BUILTIN_FUNCTIONS_FILENAME);
			if(file.exists()){
				return;
			}
			//Find root path of project
			String projectRootPath = project.getLocation().toOSString();
			//create built-in function file with implicit function references
			File builtinFunctionFile = JenaBasedSadlModelProcessor.createBuiltinFunctionImplicitModel(projectRootPath);
			//create the resource
			XtextResourceSet resourceSet = (XtextResourceSet) resourceSetProvider.get(project);
			resourceSet.createResource(URI.createFileURI(builtinFunctionFile.getAbsolutePath()));
			//refresh workspace
			project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		} catch (MalformedURLException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (ConfigurationException e) {
			e.printStackTrace();
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	private void checkForSadlImplicitModel(IProject project){
		IFile file = project.getFile(SadlConstants.SADL_IMPLICIT_MODEL_FOLDER + "/" + SadlConstants.SADL_IMPLICIT_MODEL_FILENAME);
		if(file.exists()){
			return;
		}
		File newImplicitFile = new File(project.getLocation().toOSString() + "/" +
				SadlConstants.SADL_IMPLICIT_MODEL_FOLDER + "/" +
				SadlConstants.SADL_IMPLICIT_MODEL_FILENAME);
		try {
			implicitModelContentProvider.createImplicitModel(newImplicitFile, true);
			project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		} catch (CoreException e) {
			e.printStackTrace();
			System.out.println("Unable to create SADL Implicit Model");
		}
	}
	
	protected void beforeCleanBuild(IProject project) {
		File implicitModelFolder = new File(project.getLocation().toOSString() + "/" + SadlConstants.SADL_IMPLICIT_MODEL_FOLDER );
		String modelFolderPath = project.getLocation().toOSString() + "/" + ResourceManager.OWLDIR;
		ConfigurationManagerForIDE configMgr;
		try {
			configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderPath, null);
			List<String> sources = new ArrayList<String>();
			sources.add("SADL");
			sources.add("SADL_Metrics");
			configMgr.cleanNonExisting(sources);
		} catch (ConfigurationException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}
		
		// put Graph folder cleanup hear
//		String graphsFolderPath = project.getLocation().toOSString() + "/Graphs";
//		File gff = new File(graphsFolderPath);
//		if (gff.exists()) {
//			File[] graphFiles = gff.listFiles();
//			for (File gf : graphFiles) {
//				if (gf.getName().startsWith("QueryMetadata")) {
//					if (!gf.delete()) {
//						try {
//							System.err.println("Failed to delete graph file '" + gf.getCanonicalPath() + "'");
//						} catch (IOException e) {
//							// TODO Auto-generated catch block
//							e.printStackTrace();
//						}
//					}
//				}
//			}
//		}
		implicitModelFolder.mkdir();
		checkForBuiltinFunctionImplicitModel(project);
		checkForSadlImplicitModel(project);
		if (projectsSelected == null) {
			projectsSelected = new ArrayList<IProject>();
		}
		if (projectsSelected.size() == 0) {
			System.out.println("Beginning SADL clean process...");
		}
		if (!projectsSelected.contains(project)) {
			projectsSelected.add( project);
			startTime = System.currentTimeMillis();
	//		IPreferenceValues prefValues = preferenceProvider.getPreferenceValues(context)
	//		String transFolder = prefValues.getPreference(RequirementsPreference.P_TRANSLATION_OUTPUT_FOLDER_PATH);
			IFolder transFolder = project.getFolder("Temp");
			if (transFolder.exists()) {
				IResource[] members;
				try {
					members = transFolder.members();
					for (int i = 0; members != null && i < members.length; i++) {
						IResource member = members[i];
						member.delete(true, null);
					}
				} catch (CoreException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
			}
			
			// get rid of existing metrics and Prolog files
			IFolder modelFolder = project.getFolder(ResourceManager.OWLDIR);
			if (modelFolder.exists()) {
				IResource[] members;
				try {
					members = modelFolder.members();
					for (int i = 0; members != null && i < members.length; i++) {
						IResource member = members[i];
						if (member.getName().endsWith(".metrics.owl") ||
								member.getName().endsWith(".sadl.pl")) {
							member.delete(true, null);
						}
					}
				} catch (CoreException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			//Prompt user of unsaved changes in editor before a clean
			try{
				IWorkbench workBench = PlatformUI.getWorkbench();
				Display.getDefault().syncExec(new Runnable(){
					@Override
					public void run(){
						workBench.saveAllEditors(true);
					}
				});
			}catch(Exception e){
				System.out.println("Error: " + e.getMessage() + " while saving resource during clean.  ");
			}
		}
	}
	
	protected void afterCleanBuild(SadlConsole sc) {
		if (getProject() !=null) {
			try {
				boolean enableMetricsReportGeneration = false;
				IFolder modelFolder = getProject().getFolder(ResourceManager.OWLDIR);
				if(modelFolder != null) {
					final SadlUtils su = new SadlUtils();
					final File mfFolder;
					String modelFolderUri = su.fileUrlToFileName(modelFolder.getLocationURI().toString());
					mfFolder = new File(modelFolderUri);
					//add error message files 
					writeErrorMessagesFile(mfFolder, su);
					File[] metricsFiles = obtainMetricsFiles(mfFolder);
					if (metricsFiles != null && metricsFiles.length > 0) {
						IPreferenceValues sadlPrefs = sadlPreferenceProvider.getPreferenceValues(getProject());
						String generateMetricsReport = sadlPrefs.getPreference(SadlPreferences.GENERATE_METRICS_REPORT_ON_CLEAN_BUILD);
						if (generateMetricsReport != null) {
							enableMetricsReportGeneration = Boolean.parseBoolean(generateMetricsReport);
						}
						if (enableMetricsReportGeneration) {
							String qfn = sadlPrefs.getPreference(SadlPreferences.METRICS_QUERY_FILENAME);
							if (qfn != null && qfn.length() > 0) {
//								runMetricsGeneration(mfFolder, qfn, su, metricsFiles, modelFolderUri, getProject().getName(), true);
							}
							else {
								sc.error("No SRL Metrics Query file identified. Please set SRL preference.\n");
							}
						}
						else {
							sc.info( "No SRL Metrics Report(s) generated, see Preference settings.\n");
						}
					}
				}
			}
			catch (Throwable t) {
				t.printStackTrace();
			}
		}
	}
	
	public File[] obtainMetricsFiles(File mfFolder){
		if(mfFolder == null) {
			return new File[0];
		}
		if(!mfFolder.exists()) {
			return new File[0];
		}
		
		FilenameFilter filter = new FilenameFilter() {
			public boolean accept(java.io.File dir, String name) {
				if(name != null) {
					return name.endsWith(".sreq.metrics.owl") || name.endsWith(".sadl.metrics.owl");
				}else {
					return false;
				}
			}
		};
		return mfFolder.listFiles(filter);
	}

	private boolean isCanceled = false;

	
//	private void writeToConsoleWithHyperlink(File outputfile, String projectName, boolean callFromIDE) throws IOException{
//		String linkMsg = "RCE metrics output file generated for project '" + projectName + "': ";
//		String fileName = outputfile.getCanonicalPath(); 
//		if(callFromIDE){
//			IFile file = getProject().getFile("Temp/Log/" + outputfile.getName());
//			int consoleOffset = linkMsg.length();
//			int consoleLength = fileName.length();
//			sc.writeToConsoleWithHyperlink(MessageType.INFO, linkMsg + fileName + "'\n", consoleOffset, consoleLength, file, 0, 1, 1);
//		}
//		else {
//			System.out.println(linkMsg + fileName);
//		}
//	}
//	
//	private void writeToConsole(String msg, boolean callFromIDE){
//		if(callFromIDE){
//			sc.writeToConsole(MessageType.ERROR, msg);
//		}
//		else {
//			System.err.println(msg);
//		}
//	}
//	
//	private File writeMetricsOutputFile(File mfFolder, String qname, ResultSet rs, SadlUtils su) throws IOException {
//		File outputDir = new File(mfFolder.getParentFile().getCanonicalFile() + File.separator + "Temp/Logs/");
//		outputDir.mkdirs();
//		File outputfile = new File(outputDir.getCanonicalPath() + File.separator + qname + ".csv");
//		su.stringToFile(outputfile, rs.toString(), false);
//		return outputfile;
//	}
	
	private void writeErrorMessagesFile(File mfFolder, SadlUtils su) throws IOException{
		File outputDir = new File(mfFolder.getParentFile().getCanonicalFile() + File.separator + "Temp/ErrorDocs/");
		outputDir.mkdirs();
		File outputfile1 = new File(outputDir.getCanonicalPath() + File.separator + "SadlErrors.html");
		su.stringToFile(outputfile1, SadlErrorGenerator.getSadlErrorHtmlFile() , false);
	}

//	private String getSadlMetricsMetadataModel() {
//		StringBuilder sb = new StringBuilder();
//		sb.append("<rdf:RDF\n");
//		sb.append("    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n");
//		sb.append("    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"\n");
//		sb.append("    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\n");
//		sb.append("    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\n");
//		sb.append("    xmlns:sadlmetricsmodel=\"http://com.ge.research.sadl/sadlmetricsmodel#\"\n");
//		sb.append("  xml:base=\"http://com.ge.research.sadl/sadlmetricsmodel\">\n");
//		sb.append("  <owl:Ontology rdf:about=\"\">\n");
//		sb.append("    <rdfs:comment xml:lang=\"en\">This ontology was created from a SADL file 'sadlmetricsmodel.sadl' and should not be directly edited.</rdfs:comment>\n");
//		sb.append("  </owl:Ontology>\n");
//		sb.append("  <owl:Class rdf:ID=\"Error\">\n");
//		sb.append("    <rdfs:subClassOf>\n");
//		sb.append("      <owl:Class rdf:ID=\"Marker\"/>\n");
//		sb.append("    </rdfs:subClassOf>\n");
//		sb.append("  </owl:Class>\n");
//		sb.append("  <owl:Class rdf:ID=\"Information\">\n");
//		sb.append("    <rdfs:subClassOf rdf:resource=\"#Marker\"/>\n");
//		sb.append("  </owl:Class>\n");
//		sb.append("  <owl:Class rdf:ID=\"Warning\">\n");
//		sb.append("    <rdfs:subClassOf rdf:resource=\"#Marker\"/>\n");
//		sb.append("  </owl:Class>\n");
//		sb.append("  <owl:ObjectProperty rdf:ID=\"marker\">\n");
//		sb.append("    <rdfs:range rdf:resource=\"#Marker\"/>\n");
//		sb.append("  </owl:ObjectProperty>\n");
//		sb.append("  <owl:ObjectProperty rdf:ID=\"markerType\">\n");
//		sb.append("    <rdfs:domain rdf:resource=\"#Marker\"/>\n");
//		sb.append("    <rdfs:range rdf:resource=\"#MarkerType\"/>\n");
//		sb.append("  </owl:ObjectProperty>\n");
//		sb.append("  <sadlmetricsmodel:MarkerType rdf:ID=\"UnclassifiedFailure\"/>\n");
//		sb.append("  <sadlmetricsmodel:MarkerType rdf:ID=\"TypeCheckFailure\"/>\n");
//		sb.append("  <sadlmetricsmodel:MarkerType rdf:ID=\"NestedEquation\"/>\n");
//		sb.append("  <sadlmetricsmodel:MarkerType rdf:ID=\"CircularDefinition\"/>\n");
//		sb.append("  <sadlmetricsmodel:MarkerType rdf:ID=\"CircularImport\"/>\n");
//		sb.append("  <sadlmetricsmodel:MarkerType rdf:ID=\"InvalidTableFormat\"/>\n");
//		sb.append("  <sadlmetricsmodel:MarkerType rdf:ID=\"DomainRedefinition\"/>\n");
//		sb.append("  <sadlmetricsmodel:MarkerType rdf:ID=\"InvalidExpression\"/>\n");
//		sb.append("  <sadlmetricsmodel:MarkerType rdf:ID=\"RangeRedefinition\"/>\n");
//		sb.append("  <sadlmetricsmodel:MarkerType rdf:ID=\"DuplicateName\"/>\n");
//		sb.append("</rdf:RDF>\n");
//		return sb.toString();
//	}
//	private OntModel getOntModelFromString(String serializedModel) {
//		OntModel listModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
//		InputStream stream = new ByteArrayInputStream(serializedModel.getBytes());
//		listModel.read(stream, null);
//		return listModel;
//	}
//
	protected boolean isCanceled() {
		return isCanceled;
	}

	protected void setCanceled(boolean isCanceled) {
		this.isCanceled = isCanceled;
	}

	private IProject getProject() {
		return project;
	}

	public void setProject(IProject project) {
		this.project = project;
	}

}
