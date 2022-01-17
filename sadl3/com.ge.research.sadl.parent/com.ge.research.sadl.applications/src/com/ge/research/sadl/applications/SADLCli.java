/********************************************************************************
 * Copyright (c) 2009, 2015, 2021 Broadcom Corporation, General Electric Company,
 * and others.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *******************************************************************************/

package com.ge.research.sadl.applications;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.URI;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.internal.resources.ICoreConstants;
import org.eclipse.core.internal.resources.Workspace;
import org.eclipse.core.resources.IBuildConfiguration;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.xtext.preferences.MapBasedPreferenceValues;
import org.eclipse.xtext.preferences.PreferenceValuesByLanguage;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.utils.ResourceManager;

/**
 * Builds SADL projects from the command line.
 *
 * You can use these command line arguments:
 *   - Configure SADL preferences              -config     {/path/to/file}
 *   - Import a project                        -import     {[uri:/]/path/to/project}
 *   - Import all projects in a tree           -importAll  {[uri:/]/path/to/projectTreeURI}
 *   - Clean and build all imported projects   -cleanBuild
 *   - Build all imported projects             -build
 *
 * Contains code copied from org.eclipse.cdt.managedbuilder.internal.core.HeadlessBuilder
 * https://git.eclipse.org/c/cdt/org.eclipse.cdt.git/tree/build/org.eclipse.cdt.managedbuilder.core/src/org/eclipse/cdt/managedbuilder/internal/core/HeadlessBuilder.java
 * under the Eclipse Public License, which SADL is licensed under as well.
 */
@SuppressWarnings("restriction")
public class SADLCli implements IApplication {

    /** Always print to original System.out despite any redirection */
    private static final PrintStream OUT = System.out;
    /** Always print to original System.err despite any redirection */
    private static final PrintStream ERR = System.err;

    /**
     * Prints progress of tasks and subtasks
     */
    private static class PrintingProgressMonitor extends NullProgressMonitor {
        /**
         * The progress monitor sometimes receives a subtask with the
         * same name as the main task.  In the UI that change is not
         * visible, but it is an extra line of output on the command
         * line, so suppress that case.
         */
        private String last;

        @Override
        public void beginTask(String name, int totalWork) {
            subTask(name);
        }

        @Override
        public void subTask(String name) {
            if (name != null && !name.isEmpty() && !Objects.equals(last, name)) {
                OUT.println(name);
            }
            last = name;
        }
    }

    /** OK return status */
    private static final Integer OK = IApplication.EXIT_OK;
    /** Error return status */
    private static final Integer ERROR = 1;
    /** Show usage return status */
    private static final Integer SHOW_USAGE = 2;

    /** SADL configuration file path */
    private String configFilePath = "SadlConfiguration.xml";
    /** SADL preferences */
    private PreferenceValuesByLanguage preferencesByLanguage = new PreferenceValuesByLanguage();

    /** User wants projects imported */
    private final Set<String> projectsToImport = new LinkedHashSet<>();
    /** User wants project trees imported recursively */
    private final Set<String> projectTreesToImport = new LinkedHashSet<>();
    /** User wants imported projects cleaned */
    private boolean clean = false;
    /** User wants imported projects built */
    private boolean build = false;

    /**
     * Verifies workspace instance location is not already locked or in-use.
     *
     * @return true if a valid instance location has been set, false otherwise
     */
    private boolean checkInstanceLocation() {
        // -data @none was specified but an ide requires workspace
        Location instanceLoc = Platform.getInstanceLocation();
        if (instanceLoc == null || !instanceLoc.isSet()) {
            ERR.println("Must specify a location for workspace, restart with -data switch");
            return false;
        }

        // -data "/valid/path", workspace already set
        try {
            // at this point its valid, so try to lock it to prevent concurrent use
            if (!instanceLoc.lock()) {
                ERR.println("Workspace already in use");
                return false;
            }
            return true;
        } catch (IOException e) {
            ERR.println("Couldn't obtain lock for workspace location");
        }
        return false;
    }

    /**
     * Gets all user provided arguments
     *
     * Arguments
     *   -config     {/path/to/file}
     *   -import     {[uri:/]/path/to/project}
     *   -importAll  {[uri:/]/path/to/projectTreeURI}
     *   -cleanBuild
     *   -build
     *
     * Each argument may be specified more than once
     * @param args String[] of arguments to parse
     * @return boolean indicating success
     */
    private boolean getArguments(String[] args) {
        try {
            if (args == null || args.length == 0) {
                throw new Exception("No arguments specified");
            }
            for (int i = 0; i < args.length; i++) {
                if ("-config".equals(args[i])) { //$NON-NLS-1$
                    configFilePath = args[++i];
                } else if ("-import".equals(args[i])) { //$NON-NLS-1$
                    projectsToImport.add(args[++i]);
                } else if ("-importAll".equals(args[i])) { //$NON-NLS-1$
                    projectTreesToImport.add(args[++i]);
                } else if ("-cleanBuild".equals(args[i])) { //$NON-NLS-1$
                    clean = true;
                    build = true;
                } else if ("-build".equals(args[i])) { //$NON-NLS-1$
                    build = true;
                } else {
                    throw new Exception("Unknown argument: " + args[i]);
                }
            }
        } catch (Exception e) {
            ERR.println("Invalid arguments: " + Arrays.toString(args));
            ERR.println("Error: " + e.getMessage());
            return false;
        }

        return true;
    }

    /** Configures SADL preferences from SADL configuration file **/
    private boolean loadPreferences() {
        // Check configuration file path
        File configFile = new File(configFilePath);
        if (!configFile.exists()) {
            ERR.println("Configuration file: " + configFile + " does not exist");
            return false;
        }

        // Open the configuration file and parse the XML inside it
        Map<String, String> sadlPreferences = new HashMap<>();
        try {
            DocumentBuilderFactory xmlDocFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder xmlDocBuilder = xmlDocFactory.newDocumentBuilder();
            Document xmlDoc = xmlDocBuilder.parse(configFile);
            NodeList prefNodes = xmlDoc.getElementsByTagName("preferences");

            for (int i = 0; i < prefNodes.getLength(); i++) {
                Element prefElement = (Element) prefNodes.item(i);
                String prefAttribute = prefElement.getAttribute("id");

                // Parse SADL preferences
                if (prefAttribute.equals("SADL")) {
                    NodeList sadlNodes = prefElement.getElementsByTagName("setting");
                    for (int x = 0; x < sadlNodes.getLength(); x++) {
                        Element sadlElement = (Element) sadlNodes.item(x);
                        String sadlLabel = sadlElement.getAttribute("label");
                        String sadlValue = sadlElement.getAttribute("value");

                        // If no value is set, skip and use default
                        if (sadlValue.isEmpty()) {
                            continue;
                        }
                        if (sadlLabel.equals("BaseURI")) {
                            sadlPreferences.put(SadlPreferences.SADL_BASE_URI.getId(), sadlValue);
                        } else if (sadlLabel.equals("SavedOWLModelFormat")) {
                            if (sadlValue.replace(" ", "").toLowerCase().equals("rdf/xml-abbrev")) {
                                sadlPreferences.put(
                                        SadlPreferences.OWL_MODEL_FORMAT.getId(),
                                        SadlPreferences.RDF_XML_ABBREV_FORMAT.getId());
                            } else if (sadlValue.replace(" ", "").toLowerCase().equals("rdf/xml")) {
                                sadlPreferences.put(
                                        SadlPreferences.OWL_MODEL_FORMAT.getId(),
                                        SadlPreferences.RDF_XML_FORMAT.getId());
                            } else if (sadlValue.replace(" ", "").toLowerCase().equals("n3")) {
                                sadlPreferences.put(
                                        SadlPreferences.OWL_MODEL_FORMAT.getId(),
                                        SadlPreferences.TURTLE_FORMAT.getId());
                            } else if (sadlValue
                                    .replace(" ", "")
                                    .toLowerCase()
                                    .equals("n-triple")) {
                                sadlPreferences.put(
                                        SadlPreferences.OWL_MODEL_FORMAT.getId(),
                                        SadlPreferences.N_TRIPLE_FORMAT.getId());
                            } else if (sadlValue.replace(" ", "").toLowerCase().equals("jenatbd")) {
                                sadlPreferences.put(
                                        SadlPreferences.OWL_MODEL_FORMAT.getId(),
                                        SadlPreferences.JENA_TDB.getId());
                            }
                        } else if (sadlLabel.equals("ShowImportModelListAs")) {
                            if (sadlValue
                                    .replace(" ", "")
                                    .toLowerCase()
                                    .equals("modelnamespaces")) {
                                // TODO SadlPreference Key needed
                                sadlPreferences.put(
                                        "importBy", SadlPreferences.MODEL_NAMESPACES.getId());
                            } else if (sadlValue
                                    .replace(" ", "")
                                    .toLowerCase()
                                    .equals("sadlfilenames")) {
                                // TODO SadlPreference Key needed
                                sadlPreferences.put(
                                        "importBy", SadlPreferences.SADL_FILE_NAMES.getId());
                            }
                        } else if (sadlLabel.equals(
                                "ShowPrefixesForImportedConceptsOnlyWhenNeededForDisambiguation")) {
                            sadlPreferences.put(
                                    SadlPreferences.PREFIXES_ONLY_AS_NEEDED.getId(), sadlValue);
                        } else if (sadlLabel.equals("ValidateBeforeTesting")) {
                            sadlPreferences.put(
                                    SadlPreferences.VALIDATE_BEFORE_TEST.getId(), sadlValue);
                        } else if (sadlLabel.equals("Test/QueryWithKnowledgeServer")) {
                            sadlPreferences.put(
                                    SadlPreferences.TEST_WITH_KSERVER.getId(), sadlValue);
                        } else if (sadlLabel.equals("ShowNamespacesInQueryResults")) {
                            sadlPreferences.put(
                                    SadlPreferences.NAMESPACE_IN_QUERY_RESULTS.getId(), sadlValue);
                        } else if (sadlLabel.equals("ShowTimingInformation")) {
                            sadlPreferences.put(
                                    SadlPreferences.SHOW_TIMING_INFORMATION.getId(), sadlValue);
                        } else if (sadlLabel.equals("InterpretDateAs")) {
                            if (sadlValue.replace(" ", "").toLowerCase().equals("mm/dd/yyyy")) {
                                // TODO SadlPreference Key needed
                                sadlPreferences.put(
                                        "dmyOrder", SadlPreferences.DMY_ORDER_MDY.getId());
                            } else if (sadlValue
                                    .replace(" ", "")
                                    .toLowerCase()
                                    .equals("dd/mm/yyyy")) {
                                // TODO SadlPreference Key needed
                                sadlPreferences.put(
                                        "dmyOrder", SadlPreferences.DMY_ORDER_DMY.getId());
                            }
                        } else if (sadlLabel.equals("DisableDeepValidationOfModel")) {
                            sadlPreferences.put(
                                    SadlPreferences.DEEP_VALIDATION_OFF.getId(), sadlValue);
                        } else if (sadlLabel.equals("GraphRendererPackageAndClass")) {
                            sadlPreferences.put(
                                    SadlPreferences.GRAPH_RENDERER_CLASS.getId(), sadlValue);
                        } else if (sadlLabel.equals("TabularDataImporterClass")) {
                            sadlPreferences.put(
                                    SadlPreferences.TABULAR_DATA_IMPORTER_CLASS.getId(), sadlValue);
                        } else if (sadlLabel.equals("IncludeImplicitElementsInGraph")) {
                            sadlPreferences.put(
                                    SadlPreferences.GRAPH_IMPLICIT_ELEMENTS.getId(), sadlValue);
                        } else if (sadlLabel.equals("IncludeImplicitElementInstancesInGraph")) {
                            sadlPreferences.put(
                                    SadlPreferences.GRAPH_IMPLICIT_ELEMENT_INSTANCES.getId(),
                                    sadlValue);
                        } else if (sadlLabel.equals("CheckForAmbiguousNames")) {
                            sadlPreferences.put(
                                    SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES.getId(), sadlValue);
                        } else if (sadlLabel.equals(
                                "CheckForCardinalityOfPropertyOnSpecificDomain")) {
                            sadlPreferences.put(
                                    SadlPreferences.CHECK_FOR_CARDINALITY_OF_PROPERTY_IN_DOMAIN
                                            .getId(),
                                    sadlValue);
                        } else if (sadlLabel.equals(
                                "UseIndefiniteAndDefiniteArticlesInValidationAndTranslation")) {
                            sadlPreferences.put(
                                    SadlPreferences.P_USE_ARTICLES_IN_VALIDATION.getId(),
                                    sadlValue);
                        } else if (sadlLabel.equals(
                                "FindAndExpandMissingPatternsInTranslation")) {
                            sadlPreferences.put(
                                    SadlPreferences.FIND_AND_EXPAND_MISSING_PATTERNS.getId(),
                                    sadlValue);
                        } else if (sadlLabel.equals("TypeCheckingWarningOnly")) {
                            sadlPreferences.put(
                                    SadlPreferences.TYPE_CHECKING_WARNING_ONLY.getId(), sadlValue);
                        } else if (sadlLabel.equals("TypeCheckingRangeRequired")) {
                            sadlPreferences.put(
                                    SadlPreferences.TYPE_CHECKING_RANGE_REQUIRED.getId(),
                                    sadlValue);
                        } else if (sadlLabel.equals("IgnoreUnittedQuantitiesDuringTranslation")) {
                            sadlPreferences.put(
                                    SadlPreferences.IGNORE_UNITTEDQUANTITIES.getId(), sadlValue);
                        } else if (sadlLabel.equals(
                                "TranslateMultipleClassDomainOrRangeAsUnionClass")) {
                            sadlPreferences.put(
                                    SadlPreferences.CREATE_DOMAIN_AND_RANGE_AS_UNION_CLASSES
                                            .getId(),
                                    sadlValue);
                        } else if (sadlLabel.equals(
                                "EnableMetricsCollection")) {
                            sadlPreferences.put(
                                    SadlPreferences.ENABLE_METRICS_COLLECTION.getId(),
                                    sadlValue);
                        } else if (sadlLabel.equals(
                                "GenerateMetricsReportDuringProjectCleanBuild")) {
                            sadlPreferences.put(
                                    SadlPreferences.GENERATE_METRICS_REPORT_ON_CLEAN_BUILD.getId(),
                                    sadlValue);
                        } else if (sadlLabel.equals("FileContainingMetricQueries")) {
                            sadlPreferences.put(
                                    SadlPreferences.METRICS_QUERY_FILENAME.getId(), sadlValue);
                        } else if (sadlLabel.equals("GraphImplicitElements")) {
                            sadlPreferences.put(
                                    SadlPreferences.GRAPH_IMPLICIT_ELEMENTS.getId(), sadlValue);
                        } else if (sadlLabel.equals("GraphImplicitElementInstances")) {
                            sadlPreferences.put(
                                    SadlPreferences.GRAPH_IMPLICIT_ELEMENT_INSTANCES.getId(),
                                    sadlValue);
                        }
                    }
                }
            }
        } catch (Exception e) {
            ERR.println("Error loading configuration file: " + e);
            return false;
        }

        // Prepare the SADL preferences for use later
        preferencesByLanguage.put("com.ge.research.sadl.SADL", 
                                  new MapBasedPreferenceValues(sadlPreferences));

        return true;
    }

    /**
     * Imports a project into the workspace
     * @param projURIStr base URI string
     * @param recurse Should we recurse down the URI importing all projects?
     * @return int OK / ERROR
     */
    private int importProject(IWorkspace workspace, IProgressMonitor monitor, String projURIStr, 
                              boolean recurse) throws CoreException, IOException {
        URI project_uri = null;
        try {
            project_uri = URI.create(projURIStr);
        } catch (Exception e) {
            // Will be treated as straightforward path in the case below
        }

        // Handle local paths as well
        if (project_uri == null || project_uri.getScheme() == null) {
            IPath p = new Path(projURIStr).addTrailingSeparator();
            project_uri = URIUtil.toURI(p);

            // Handle relative paths as relative to cwd
            if (project_uri.getScheme() == null) {
                String cwd = System.getProperty("user.dir"); //$NON-NLS-1$
                p = new Path(cwd).addTrailingSeparator();
                p = p.append(projURIStr);
                project_uri = URIUtil.toURI(p);
            }
            if (project_uri.getScheme() == null) {
                ERR.println("Invalid project URI: " + project_uri);
                return ERROR;
            }
        }

        if (recurse) {
            if (!EFS.getStore(project_uri).fetchInfo().exists()) {
                ERR.println("Directory: " + project_uri + " can't be found");
                return ERROR;
            }
            for (IFileStore info : EFS.getStore(project_uri).childStores(EFS.NONE, monitor)) {
                if (!info.fetchInfo().isDirectory())
                    continue;
                int status = importProject(workspace, monitor, info.toURI().toString(), recurse);
                if (status != OK)
                    return status;
            }
        }

        // Load the project description
        IFileStore fstore = EFS.getStore(project_uri).getChild(".project"); //$NON-NLS-1$
        if (!fstore.fetchInfo().exists()) {
            if (!recurse) {
                ERR.println("Project: " + project_uri + " can't be found");
                return ERROR;
            }
            // .project not found; OK if we're recursing
            return OK;
        }

        try (InputStream in = fstore.openInputStream(EFS.NONE, monitor)) {
            IProjectDescription desc = workspace.loadProjectDescription(in);

            // Check that a project with the same name doesn't already exist in the workspace
            IWorkspaceRoot root = workspace.getRoot();
            IProject project = root.getProject(desc.getName());
            if (project.exists()) {
                // It's ok if the project we're importing is the same as one already in the workspace
                if (URIUtil.equals(project.getLocationURI(), project_uri)) {
                    project.open(monitor);
                    return OK;
                }
                ERR.println("Project: " + desc.getName() + " already exists in workspace");
                return ERROR;
            }
            // Create and open the project
            // Note that if the project exists directly under the workspace root, we can't #setLocationURI(...)
            if (!URIUtil.equals(
                    org.eclipse.core.runtime.URIUtil.append(root.getLocationURI(),
                            org.eclipse.core.runtime.URIUtil.lastSegment(project_uri)),
                    project_uri))
                desc.setLocationURI(project_uri);
            else
                project_uri = null;
            // Check the URI is valid for a project in this workspace
            if (!workspace.validateProjectLocationURI(project, project_uri).equals(Status.OK_STATUS)) {
                ERR.println("URI: " + project_uri + " is not valid in workspace");
                return ERROR;
            }

            project.create(desc, monitor);
            project.open(monitor);
        }

        return OK;
    }

    /** Cleans imported projects */
    private void cleanProjects(IProject[] projects) {
        // Don't delete files ending with ".rdf", ".xml", or ".yaml" in OwlModels
        final String keepExtensions = ".*\\.(rdf|xml|yaml)";

        for (IProject project : projects) {
            // Find each project's OwlModels directory
            URI projectURI = project.getLocationURI();
            File projectDirectory = new File(projectURI.getSchemeSpecificPart());
            File owlDirectory = new File(projectDirectory, ResourceManager.OWLDIR);

            // Clean the OwlModels files since they will be regenerated automatically
            OUT.println("Cleaning " + project.getName());
            if (owlDirectory.exists() && owlDirectory.isDirectory()) {
                for (File file : owlDirectory.listFiles()) {
                    if (file.isFile() && !file.getName().matches(keepExtensions)) {
                        if (file.delete()) OUT.println("Cleaning " + file);
                    }
                }
            }
        }
    }

    /** Builds imported projects */
    private boolean buildProjects(IWorkspace iWorkspace, IProgressMonitor monitor, Set<String> allBuildErrors) {
        boolean buildSuccessful = true;

        if (iWorkspace instanceof Workspace) {
            Workspace workspace = (Workspace)iWorkspace;
            IBuildConfiguration[] buildOrder = workspace.getBuildOrder();
            int trigger = IncrementalProjectBuilder.FULL_BUILD;
    
            try {
                IBuildConfiguration[] configs = ICoreConstants.EMPTY_BUILD_CONFIG_ARRAY;
                IStatus result = workspace.getBuildManager().build(buildOrder, configs, trigger, monitor);
                if (!result.isOK()) {
                    buildSuccessful = false;
                }
            }
            finally {
                // Always send POST_BUILD if there was a PRE_BUILD
                workspace.broadcastBuildEvent(workspace, IResourceChangeEvent.POST_BUILD, trigger);
            }
    
            buildSuccessful = accumulateErrorMessages(buildOrder, buildSuccessful, allBuildErrors);
        }

        return buildSuccessful;
    }

    /** Accumulates error messages after building imported projects */
    private boolean accumulateErrorMessages(IBuildConfiguration[] buildOrder, boolean buildSuccessful, 
                                           Set<String> allBuildErrors) {
        // Get any error markers
        try {
            for (IBuildConfiguration bc : buildOrder) {
                IProject project = bc.getProject();
                IMarker[] findMarkers = project.findMarkers(null, true, IResource.DEPTH_INFINITE);
                for (IMarker marker : findMarkers) {
                    int severity = marker.getAttribute(IMarker.SEVERITY, IMarker.SEVERITY_INFO);
                    if (severity >= IMarker.SEVERITY_ERROR) {
                        buildSuccessful = false;
                        allBuildErrors.add(marker.toString());
                    }
                }
            }
        } catch (CoreException e) {
            ERR.println("Error getting error markers: " + e);
        } 

        return buildSuccessful;
    }

    // Builds SADL projects from command line
    @Override
    public Object start(IApplicationContext context) throws Exception {
        // Return whether projects were built successfully
        boolean buildSuccessful = false;
        Set<String> allBuildErrors = new LinkedHashSet<>();

        // Check whether workspace is being used in a running IDE
        if (!checkInstanceLocation())
            return ERROR;

        // Make sure workspace is saved after build
        IWorkspace workspace = ResourcesPlugin.getWorkspace();
        IWorkspaceRoot root = workspace.getRoot();
        IWorkspaceDescription desc = workspace.getDescription();
        final boolean isAutoBuilding = workspace.isAutoBuilding();
        IProgressMonitor monitor = new PrintingProgressMonitor();

        try {
            // Check whether workspace can be read and written
            if (!root.isAccessible()) {
                ERR.println("Workspace: " + root.getLocationURI().toString() + " is not accessible");
                return ERROR;
            }
            
            // Turn off workspace auto-build and refresh workspace
            desc.setAutoBuilding(false);
            workspace.setDescription(desc);
            root.refreshLocal(IResource.DEPTH_INFINITE, monitor);

            // Get user provided arguments
            final String[] args = (String[]) context.getArguments().get(IApplicationContext.APPLICATION_ARGS);
            if (!getArguments(args))
                return SHOW_USAGE;

            // Configure SADL preferences from SADL configuration file
            if (!loadPreferences()) {
                return ERROR;
            }

            // Import project trees
            for (String projURIStr : projectTreesToImport) {
                int status = importProject(workspace, monitor, projURIStr, true);
                if (status != OK)
                    return status;
            }

            // Import single projects
            for (String projURIStr : projectsToImport) {
                int status = importProject(workspace, monitor, projURIStr, false);
                if (status != OK)
                    return status;
            }

            // Clean the projects if the user wanted them cleaned
            if (clean) {
                OUT.println("Cleaning all projects...");
                IProject[] allProjects = root.getProjects();
                cleanProjects(allProjects);
            }

            // Build the projects if the user wanted them built
            if (build) {
                OUT.println("Building all projects...");
                buildSuccessful = buildProjects(workspace, monitor, allBuildErrors);
            }
        } finally {
            if (buildSuccessful) {
                OUT.println("Build completed successfully");
            } else {
                OUT.println("Build failed with " + allBuildErrors.size() + " errors");
                for (String buildError : allBuildErrors) {
                    ERR.println(buildError);
                }
                // Don't print "Eclipse:\nJVM terminated. Exit code=1" message
                System.getProperties().put("eclipse.exitdata", "");
            }
    
            // Wait for any outstanding jobs to finish
            if (!Job.getJobManager().isIdle()) {
                Thread.sleep(10);
            }

            // Reset workspace auto-build preference
            desc.setAutoBuilding(isAutoBuilding);
            workspace.setDescription(desc);

            // We sometimes hang if we save modified workspace, so just exit forcefully
            System.exit(buildSuccessful ? OK : ERROR);
        }

        return buildSuccessful ? OK : ERROR;
    }

    @Override
    public void stop() {
        // do nothing
    }
}
