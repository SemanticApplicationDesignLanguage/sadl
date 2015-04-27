/************************************************************************
 * Copyright © 2007-2010 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.builder;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtext.builder.IXtextBuilderParticipant;
import org.eclipse.xtext.resource.IResourceDescription.Delta;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.sadl.Import;
import com.ge.research.sadl.sadl.ModelName;
import com.google.inject.Inject;

public class SadlBuilder implements IXtextBuilderParticipant {
	private static final Logger logger = LoggerFactory.getLogger(SadlBuilder.class);

    private SadlModelManager visitor;
    
    private int totalErrors = 0;
    private int totalWarnings = 0;
    private int modsWithErrors = 0;
    
    private boolean showTimingInformation = false;

	@Inject
	private SadlModelManagerProvider sadlModelManagerProvider;

//    @Inject
//    public SadlBuilder(SadlModelManager visitor) {
//        this.visitor = visitor;
//    }
    
   public SadlBuilder() {
	   
   }

    /**
     * Allows us to perform additional steps in the build process such as 
     * converting the Ecore model to a Jena model and saving the Jena model.
     * 
     * @param context The build context from which to get the Ecore model.
     * @param monitor The progress monitor to use for reporting progress to the 
     * user.  It is the caller's responsibility to call done() on the given 
     * monitor.  Accepts <code>null</code>, indicating that no progress should 
     * be reported and that the operation cannot be cancelled.
     */
    @Override
    public void build(IBuildContext context, IProgressMonitor monitor)
            throws CoreException {

        // Create and save each resource's Jena model.
//        List<Resource> resources = context.getResourceSet().getResources();
    	for (Delta delta: context.getDeltas()) {
    		// Use file extension from injected language name
    		if ((delta.getNew()!=null || context.getBuildType() == BuildType.CLEAN) && delta.getUri().lastSegment().endsWith(".sadl")) {
    			context.getResourceSet().getResource(delta.getUri(), true);
    		}
    	}
        List<Resource> resources = context.getResourceSet().getResources();

        // Ensure that the OWL models folder exists.
        IProject project = context.getBuiltProject();
        IFolder folder = project.getFolder(ResourceManager.OWLDIR);
        if (!folder.exists()) {
            folder.create(IResource.NONE, true, monitor);
            logger.debug("OwlModels folder created: " + folder.toString());
        }

    	IConfigurationManagerForIDE configMgr = null;
		String modelFolder = ResourceManager.convertProjectRelativePathToAbsolutePath(folder.getFullPath().toPortableString());
       	try {
       		configMgr = getVisitor(URI.createURI(modelFolder)).getConfigurationMgr(modelFolder);
    	}
    	catch (Exception e) {
    		logger.error("Exception while getting a ConfigurationManagerForIDE: " + e.getMessage());
    		throw new CoreException(new Status(0, this.getClass().getPackage().toString(), 0, 
    				"Unable to get a ConfigurationManager: ", e));
    	}
   		if (configMgr == null) {
   			logger.error("Failed to get a ConfigurationManagerForIDE");
   			throw new CoreException(new Status(0, this.getClass().getPackage().toString(), 0, 
				"Unable to get a ConfigurationManager: ", null));
   		}
        if (context.getBuildType() == BuildType.CLEAN) {
        	// Clean up the ont-policy.rdf file if doing a clean.
        	try {
         		configMgr.cleanProject(project, folder);
    	        for (Resource resource : resources) {
    	            URI sadlFile = resource.getURI();
    	            String fext = sadlFile.fileExtension();
    	            if (ResourceManager.SADLEXT.equals(fext)) {
    	            	getVisitor(resource.getURI()).deleteMarkers(resource, true);
    	            }
    	        }
        	}
        	catch (Exception e) {
        		logger.error("Unable to do a clean because of unexpected exception: ", e);
        		throw new CoreException(new Status(0, this.getClass().getPackage().toString(), 0, 
        				"Unable to do a clean because of unexpected exception: ", e));
        	}
        }
        else if (resources != null && resources.size() > 0) {
        	boolean fullBuild = false;
        	IPreferencesService service = Platform.getPreferencesService();
        	showTimingInformation = service.getBoolean("com.ge.research.sadl.Sadl", "showTimingInformation", false, null);

        	if (context.getBuildType() == BuildType.FULL) {
        		logger.debug("Starting FULL build.");
        		fullBuild = true;
        	}
        	else {
        		logger.debug("Starting incremental build.");
        	}
        	totalErrors = 0;	// initialize these values
        	totalWarnings = 0;
        	modsWithErrors = 0;
        	
        	// The resources List will not necessarily be in the right order if the import is by sadl file. We need some way of returning dependency information 
        	//	for a failed build attempt of a file that imports other files not yet build (and therefore not yet in the policy file so that we have no way
        	//	of finding the imported file's URI. Alternatively, perhaps we could do this in two passes? On a second pass would the information be available?
	        List<Resource> sadlResources = new ArrayList<Resource>();
	        HashMap<Resource, List<Resource>> sadlDependencyList = new HashMap<Resource, List<Resource>>();
	        for (Resource resource : resources) {
	            // Process only SADL files.
	            URI sadlFile = resource.getURI();
	            String fext = sadlFile.fileExtension();
	            if (ResourceManager.SADLEXT.equals(fext)) {
	            	sadlResources.add(resource);
	            	sadlDependencyList.put(resource, null);
	            }
	        }
	        
	        int numSadlResources = sadlResources.size();
	        SubMonitor progress = SubMonitor.convert(monitor, numSadlResources + 2);	// assume determine dependencies is 2 ticks
	        long t1 = System.currentTimeMillis();
	        for (Resource resource : sadlResources) {
	            if (progress.isCanceled()) { 
	        		logger.debug("Build was cancelled.");
	                break;
	            }
	            
	            // fill in imports to handle dependencies
	        	boolean foundMnAndImp = false;
	            for (TreeIterator<EObject> iter = EcoreUtil.getAllContents(resource, true); iter.hasNext();) { 
	                EObject eObject = iter.next();
	    			if (eObject instanceof ModelName || eObject instanceof Import) {
	    				foundMnAndImp = true;
	                	if (eObject instanceof Import) {
		    				String impUri = ((Import) eObject).getImportURI();
		    				if (impUri.startsWith("http:")){
		    					try {
									impUri = getVisitor(resource.getURI()).getAltUrl(impUri, resource.getURI());
								} catch (MalformedURLException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								}
		    				}
		    				Resource dependentResource = findMatchingResource(sadlResources, impUri);
		    				if (dependentResource != null) {
			                	List<Resource> dependencies = sadlDependencyList.get(resource);
			                	if (dependencies == null) {
			                		dependencies = new ArrayList<Resource>();
			                		sadlDependencyList.put(resource, dependencies);
			                	}
		    					dependencies.add(dependentResource);
		    					logger.debug("Build added dependency '" + dependentResource.getURI().toString() + "' for resource '" + 
		    							resource.getURI().toString() + "'");
		    				}
	                	}
	    			}
	    			else if (foundMnAndImp) {
	    				break;
	    			}
	            }
	        }
            progress.worked(2);

            long t2 = 0L;
            boolean someOutput = false;
	        
	        if (showTimingInformation && fullBuild) {
	        	t2 = System.currentTimeMillis();
		        System.out.println("Build dependencies for project " + project.getName() + " determined in " + (t2 - t1) + " ms");
		        someOutput = true;
	        }
	        
	        List<Resource> processedResources = new ArrayList<Resource>();
	        List<Resource> queuedResources = new ArrayList<Resource>();
	        
	        Iterator<Resource> rsrcIter = sadlDependencyList.keySet().iterator();
	        
	        int errors = 0;
	        while (rsrcIter.hasNext()) {
	        	Resource resource = rsrcIter.next();
	        	errors += buildResource(sadlDependencyList, resource, queuedResources, processedResources, progress);
	            if (progress.isCanceled()) { 
	        		logger.debug("Build was cancelled.");
	                break;
	            }
	        }
	        if (showTimingInformation && fullBuild) {
		        long t3 = System.currentTimeMillis();
		        System.out.println("Total build time for " + processedResources.size() + " models in project " + project.getName() + ": " + (t3 - t1) + " ms");
		        someOutput = true;
	        }
	        if (totalErrors > 0) {
	        	System.err.println("   A total of " + totalErrors + " errors occurred in " + modsWithErrors + " models.");
	        	someOutput = true;
	        }
	        if (totalWarnings > 0) {
	        	if (totalWarnings > 1) {
	        		System.out.println("   There were " + totalWarnings + " warnings.");
	        		someOutput = true;
	        	}
	        	else {
	        		System.out.println("   There was 1 warning.");
	        		someOutput = true;
	        	}
	        }
	        if (someOutput) {
	        	System.out.println("\n");
	        }
	    }
    }

	private int buildResource(
			HashMap<Resource, List<Resource>> sadlDependencyList,
			Resource resource, List<Resource> queuedResources, List<Resource> processedResources, SubMonitor progress) {
        if (progress.isCanceled()) { 
    		logger.debug("Build was cancelled.");
            return 0;
        }
        if (processedResources.contains(resource)) {
    		logger.debug("Resource '" + resource.getURI().toString() + "' already processed.");
        	return 0;
        }
        
        if (sadlDependencyList.get(resource) != null && !queuedResources.contains(resource)) {
    		queuedResources.add(resource);
        	List<Resource> deps = sadlDependencyList.get(resource);
        	for (int i = 0; i < deps.size(); i++) {
                if (progress.isCanceled()) { 
            		logger.debug("Build was cancelled.");
                    return 0;
                }
        		Resource deprsrc = deps.get(i);
        		logger.debug("Building of '" + resource.getURI().toString() + "' suspended while dependency '" + deprsrc.getURI().toString() + "' is built.");
        		buildResource(sadlDependencyList, deprsrc, queuedResources, processedResources, progress);
        	}
        }
        if (!processedResources.contains(resource)) {
        	int thisModErrCnt = 0;
        	int thisModWarnCnt = 0;
	        processedResources.add(resource);
	        // Process only SADL files.
	        URI sadlFile = resource.getURI();
	        String fext = sadlFile.fileExtension();
	        if (ResourceManager.SADLEXT.equals(fext)) {
	        	synchronized(getVisitor(resource.getURI())) {
		        	try {
		        		logger.debug("Ready to build '" + resource.getURI().toString() + "': calling SMM.processModel with save true.");
		        		long t1 = System.currentTimeMillis();
		        		getVisitor(resource.getURI()).processModel(resource, true, false, progress);
		        		thisModErrCnt = getVisitor(resource.getURI()).errors(true);
		        		thisModErrCnt += getVisitor(resource.getURI()).countErrorMarkers(resource);
		        		thisModErrCnt += getVisitor(resource.getURI()).getNumTranslationErrors();
		        		if (thisModErrCnt > 0) {
		        			modsWithErrors++;
		        		}
		        		totalErrors += thisModErrCnt;
		        		thisModWarnCnt = getVisitor(resource.getURI()).warnings();
		        		totalWarnings += thisModWarnCnt;
		        		if (showTimingInformation) {
		        			long t2 = System.currentTimeMillis();
		        			String msg = "Built '" + sadlFile.lastSegment() + "': " + (t2 - t1) + " ms" + 
		        			   ((thisModErrCnt > 0 || thisModWarnCnt > 0) ? 
		        					   ("(" + thisModErrCnt + (thisModErrCnt == 1 ? " error, " : " errors, ") + 
		        							   + thisModWarnCnt + (thisModWarnCnt == 1 ? " warning)" : " warnings)")) : "");
//			    			String actualUrl = ResourceManager.convertPlatformUriToAbsoluteUri(sadlFile).toFileString();
			    			if (thisModErrCnt > 0) {
//			        			visitor.getMessageManager().error(msg + "\n",
//				    					visitor.getMessageManager().new HyperlinkInfo(actualUrl, 0, 0, 0));
			    				System.err.println(msg);
			    			}
			    			else {
//			        			visitor.getMessageManager().info(msg + "\n",
//				    					visitor.getMessageManager().new HyperlinkInfo(actualUrl, 0, 0, 0));
			    				System.out.println(msg);
			    			}
		        		}
		        		getVisitor(resource.getURI()).removeResourceModel(resource);
		        	}
		        	catch (Throwable t) {
		        		String msg = "Failed to build model for '" + sadlFile.lastSegment() + "': " + t.getLocalizedMessage();
		        		logger.error(msg);
		        		System.out.println(msg);
		        		t.printStackTrace();
		        	}
	        	}
	        }
	        
	        progress.worked(1);
	        return thisModErrCnt;
        }
        return 0;
	}

	private Resource findMatchingResource(List<Resource> sadlResources,
			String impUri) {
		if (impUri != null) {
			int lastSlash = impUri.lastIndexOf('/');
			String sadlFilename = lastSlash >= 0 ? impUri.substring(lastSlash) : impUri;
			if (sadlFilename.endsWith(ResourceManager.getOwlFileExtensionWithPrefix())) { // ".owl")) {
				sadlFilename = sadlFilename.substring(0, sadlFilename.length() - 3) + "sadl";
			}
			for (Resource resource : sadlResources) {
				if (resource.getURI().toString().endsWith(sadlFilename)) {
	        		logger.debug("Found resouce (" + resource.getURI().toString() + ") matching '" + impUri + "'");
	        		return resource;
				}
			}
		}
		return null;
	}

	private SadlModelManager getVisitor(URI uri) {
		if (visitor == null) {
			setVisitor(sadlModelManagerProvider.get(uri));
		}
		return visitor;
	}

	private void setVisitor(SadlModelManager visitor) {
		this.visitor = visitor;
	}
    
}
