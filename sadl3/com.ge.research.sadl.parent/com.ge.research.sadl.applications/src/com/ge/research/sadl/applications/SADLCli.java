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
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.inject.Injector;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.generator.GeneratorContext;
import org.eclipse.xtext.generator.GeneratorDelegate;
import org.eclipse.xtext.generator.JavaIoFileSystemAccess;
import org.eclipse.xtext.preferences.MapBasedPreferenceValues;
import org.eclipse.xtext.preferences.PreferenceValuesByLanguage;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.IResourceValidator;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * Builds SADL projects from the command line.
 *
 * You can use these command line arguments:
 *   - Configure SADL preferences              -config     {/path/to/file}
 *   - Import projects                         -import     {[uri:/]/path/to/project}
 *   - Import all projects in the tree         -importAll  {[uri:/]/path/to/projectTreeURI}
 *   - Build projects / the workspace          -build      {project_name_reg_ex | all}
 *   - Clean build projects / the workspace    -cleanBuild {project_name_reg_ex | all}
 *
 * Contains code copied from org.eclipse.cdt.managedbuilder.internal.core.HeadlessBuilder
 * https://git.eclipse.org/c/cdt/org.eclipse.cdt.git/tree/build/org.eclipse.cdt.managedbuilder.core/src/org/eclipse/cdt/managedbuilder/internal/core/HeadlessBuilder.java
 * under the Eclipse Public License, which SADL is licensed under as well.
 */
@SuppressWarnings("restriction")
public class SADLCli implements IApplication {

    /**
     * Prints progress of tasks and subtasks
     */
    protected static class PrintingProgressMonitor extends NullProgressMonitor {

        @Override
        public void beginTask(String name, int totalWork) {
            if (name != null && !name.isEmpty()) {
                System.out.println(name);
            }
        }

        @Override
        public void subTask(String name) {
            if (name != null && !name.isEmpty()) {
                System.out.println(name);
            }
        }
    }

    /** OK return status */
    public static final Integer OK = IApplication.EXIT_OK;
    /** Error return status */
    public static final Integer ERROR = 1;
    /** Show usage return status */
    public static final Integer SHOW_USAGE = 2;

    /** SADL configuration file path */
    protected String configFilePath = "SadlConfiguration.xml";
    /** SADL preferences */
    protected PreferenceValuesByLanguage preferencesByLanguage = new PreferenceValuesByLanguage();

    /** Project URIs / paths to import */
    protected final Set<String> projectsToImport = new LinkedHashSet<>();
    /** Trees of projects to recursively import */
    protected final Set<String> projectTreeToImport = new LinkedHashSet<>();
    /** Project names to clean */
    protected final Set<String> projectRegExToClean = new LinkedHashSet<>();
    /** Project names to build */
    protected final Set<String> projectRegExToBuild = new LinkedHashSet<>();

    /** Clean the workspace */
    protected boolean cleanAll = false;
    /** Build the workspace */
    protected boolean buildAll = false;

    /**
     * Verifies specified workspace is not already locked or in-use.
     *
     * @return true if a valid instance location has been set, false otherwise
     */
    protected boolean checkInstanceLocation() {
        // -data @none was specified but an ide requires workspace
        Location instanceLoc = Platform.getInstanceLocation();
        if (instanceLoc == null || !instanceLoc.isSet()) {
            System.err.println("Must specify a location for workspace, restart with -data switch");
            return false;
        }

        // -data "/valid/path", workspace already set
        try {
            // at this point its valid, so try to lock it to prevent concurrent use
            if (!instanceLoc.lock()) {
                System.err.println("Workspace already in use");
                return false;
            }
            return true;
        } catch (IOException e) {
            System.err.println("Couldn't obtain lock for workspace location");
        }
        return false;
    }

    /**
     * Gets all user provided arguments
     *
     * Arguments
     *   -config     {/path/to/file}
     *   -import     {[uri:/]/path/to/project}
     *   -importAll  {[uri:/]/path/to/projectTreeURI} Import all projects in the tree
     *   -build      {project_name_reg_ex | all}
     *   -cleanBuild {project_name_reg_ex | all}
     *
     * Each argument may be specified more than once
     * @param args String[] of arguments to parse
     * @return boolean indicating success
     */
    protected boolean getArguments(String[] args) {
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
                    projectTreeToImport.add(args[++i]);
                } else if ("-build".equals(args[i])) { //$NON-NLS-1$
                    projectRegExToBuild.add(args[++i]);
                } else if ("-cleanBuild".equals(args[i])) { //$NON-NLS-1$
                    projectRegExToClean.add(args[++i]);
                } else {
                    throw new Exception("Unknown argument: " + args[i]);
                }
            }
        } catch (Exception e) {
            System.err.println("Invalid arguments: " + Arrays.toString(args));
            System.err.println("Error: " + e.getMessage());
            return false;
        }

        if (projectRegExToClean.contains("all") || projectRegExToClean.contains("*")) { //$NON-NLS-1$ //$NON-NLS-2$
            cleanAll = true;
            buildAll = true;
            projectRegExToClean.remove("all"); //$NON-NLS-1$
            projectRegExToClean.remove("*"); //$NON-NLS-1$
        }
        if (projectRegExToBuild.contains("all") || projectRegExToBuild.contains("*")) { //$NON-NLS-1$ //$NON-NLS-2$
            buildAll = true;
            projectRegExToBuild.remove("all"); //$NON-NLS-1$
            projectRegExToBuild.remove("*"); //$NON-NLS-1$
        }

        return true;
    }

    /** Configures SADL preferences from SADL configuration file **/
    protected boolean loadPreferences() {
        // Check configuration file path
        File configFile = new File(configFilePath);
        if (!configFile.exists()) {
            System.err.println("Configuration file: " + configFile + " does not exist");
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
            System.err.println("Error loading configuration file: " + e);
            return false;
        }

        // Prepare the SADL preferences for use later
        preferencesByLanguage.put("com.ge.research.sadl.SADL", 
                                  new MapBasedPreferenceValues(sadlPreferences));

        return true;
    }

    /** Finds all projects matching specified regular expression */
    protected boolean matchProjects(String projectRegEx, IProject[] projectList, Set<IProject> projects) {
        boolean projectMatched = false;

        // Find all projects that match the regular expression
        try {
            Pattern projectPattern = Pattern.compile(projectRegEx);

            for (IProject project : projectList) {
                Matcher projectMatcher = projectPattern.matcher(project.getName());

                if (projectMatcher.matches()) {
                    projectMatched = true;
                    projects.add(project);
                }
            }
            if (!projectMatched) {
                System.err.println("WARNING: No projects matched '" + projectRegEx + "', skipping");
            }
        } catch (PatternSyntaxException e) {
            System.err.println("Regular expression syntax error: " + e.toString());
            System.err.println("Skipping '" + projectRegEx + "'");
        }

        return projectMatched;
    }

    /**
     * Imports a project into the workspace
     * @param projURIStr base URI string
     * @param recurse Should we recurse down the URI importing all projects?
     * @return int OK / ERROR
     */
    protected int importProject(String projURIStr, boolean recurse) throws CoreException, IOException {
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        IProgressMonitor monitor = new PrintingProgressMonitor();

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
                System.err.println("Invalid project URI: " + project_uri);
                return ERROR;
            }
        }

        if (recurse) {
            if (!EFS.getStore(project_uri).fetchInfo().exists()) {
                System.err.println("Directory: " + project_uri + " can't be found");
                return ERROR;
            }
            for (IFileStore info : EFS.getStore(project_uri).childStores(EFS.NONE, monitor)) {
                if (!info.fetchInfo().isDirectory())
                    continue;
                int status = importProject(info.toURI().toString(), recurse);
                if (status != OK)
                    return status;
            }
        }

        // Load the project description
        IFileStore fstore = EFS.getStore(project_uri).getChild(".project"); //$NON-NLS-1$
        if (!fstore.fetchInfo().exists()) {
            if (!recurse) {
                System.err.println("Project: " + project_uri + " can't be found");
                return ERROR;
            }
            // .project not found; OK if we're recursing
            return OK;
        }

        try (InputStream in = fstore.openInputStream(EFS.NONE, monitor)) {
            IProjectDescription desc = root.getWorkspace().loadProjectDescription(in);

            // Check that a project with the same name doesn't already exist in the workspace
            IProject project = root.getProject(desc.getName());
            if (project.exists()) {
                // It's ok if the project we're importing is the same as one already in the workspace
                if (URIUtil.equals(project.getLocationURI(), project_uri)) {
                    project.open(monitor);
                    return OK;
                }
                System.err.println("Project: " + desc.getName() + " already exists in workspace");
                return ERROR;
            }
            // Create and open the project
            // Note that if the project exists directly under the workspace root, we can't #setLocationURI(...)
            if (!URIUtil.equals(
                    org.eclipse.core.runtime.URIUtil.append(ResourcesPlugin.getWorkspace().getRoot().getLocationURI(),
                            org.eclipse.core.runtime.URIUtil.lastSegment(project_uri)),
                    project_uri))
                desc.setLocationURI(project_uri);
            else
                project_uri = null;
            // Check the URI is valid for a project in this workspace
            if (!root.getWorkspace().validateProjectLocationURI(project, project_uri).equals(Status.OK_STATUS)) {
                System.err.println("URI: " + project_uri + " is not valid in workspace");
                return ERROR;
            }

            project.create(desc, monitor);
            project.open(monitor);
        }

        return OK;
    }

    /** Builds specified projects */
    protected boolean buildProjects(Set<IProject> projects, final XtextResourceSet resourceSet,
            final IProgressMonitor monitor, final int buildType, Set<String> allBuildErrors) {
        boolean buildSuccessful = true;

        SubMonitor progress = SubMonitor.convert(monitor, 100).setWorkRemaining(projects.size());
        for (IProject project : projects) {
            buildSadlProject(project, resourceSet, progress.split(1), buildType);
            buildSuccessful = buildSuccessful && isProjectSuccesfullyBuilt(resourceSet);
            accumulateErrorMarkers(resourceSet, allBuildErrors);
        }

        return buildSuccessful;
    }

    /** Cleans implicit/owl files since they will be regenerated automatically */
    private void cleanDirectory(File directory) {
        // Keep only files ending with ".rdf"
        final String keepExtension = ".rdf";
        if (directory.exists() && directory.isDirectory()) {
            for (File file : directory.listFiles()) {
                if (file.isFile() && !file.getName().endsWith(keepExtension)) {
                    if (file.delete()) System.out.println("Cleaning " + file);
                }
            }
        }
    }

    /** Finds all files having a given file extension */
    private List<File> findFilesRecursively(File directory, String fileExtension, List<File> files) {
        for (File file : directory.listFiles()) {
            if (file.isDirectory()) {
                findFilesRecursively(file, fileExtension, files);
            } else if (file.isFile() && file.getName().endsWith(fileExtension)) {
                files.add(file);
            }
        }
        return files;
    }

    /** Writes a SADL resource as an OWL file */
    private void writeAsOwl(Resource aResource, File owlDirectory) {
        if (aResource instanceof XtextResource) {
            XtextResource resource = (XtextResource)aResource;
            System.out.println("Building " + resource.getURI().path());

            // Generate the OWL file
            GeneratorDelegate generator = resource.getResourceServiceProvider().get(GeneratorDelegate.class);
            JavaIoFileSystemAccess access = resource.getResourceServiceProvider().get(JavaIoFileSystemAccess.class);
            GeneratorContext context = new GeneratorContext();
            context.setCancelIndicator(CancelIndicator.NullImpl);
            access.setOutputPath(owlDirectory.getAbsolutePath());
            generator.generate(resource, access, context);
        }
    }

    /** Builds a SADL project */
    protected void buildSadlProject(IProject project, final XtextResourceSet resourceSet,
                                    final IProgressMonitor monitor, final int buildType) {
        URI projectURI = project.getLocationURI();
        File projectDirectory = new File(projectURI.getSchemeSpecificPart());
        File implicitModelDirectory = new File(projectDirectory, SadlConstants.SADL_IMPLICIT_MODEL_FOLDER);
        File owlDirectory = new File(projectDirectory, ResourceManager.OWLDIR);

        if (buildType == IncrementalProjectBuilder.CLEAN_BUILD) {
            // Clean implicit/owl model files since they will be regenerated automatically
            System.out.println("Cleaning " + project.getName());
            cleanDirectory(implicitModelDirectory);
            cleanDirectory(owlDirectory);
        } else if (buildType == IncrementalProjectBuilder.INCREMENTAL_BUILD) {
            // Add SADL files to resource set and resolve interdependencies during first pass
            List<File> sadlFiles = findFilesRecursively(projectDirectory, ".sadl", new ArrayList<>());
            for (File file : sadlFiles) {
                org.eclipse.emf.common.util.URI uri = 
                    org.eclipse.emf.common.util.URI.createFileURI(file.getAbsolutePath());
                Resource resource = resourceSet.getResource(uri, true);
                IResourceValidator validator = ((XtextResource)resource).getResourceServiceProvider().getResourceValidator();
                validator.validate(resource, CheckMode.NORMAL_AND_FAST, CancelIndicator.NullImpl);
            }
            EcoreUtil2.resolveAll(resourceSet);
        } else if (buildType == IncrementalProjectBuilder.FULL_BUILD) {
            // Generate OWL files from the SADL files during second pass
            System.out.println("Building " + project.getName());
            List<File> sadlFiles = findFilesRecursively(projectDirectory, ".sadl", new ArrayList<>());
            for (File file : sadlFiles) {
                org.eclipse.emf.common.util.URI uri = 
                    org.eclipse.emf.common.util.URI.createFileURI(file.getAbsolutePath());
                Resource resource = resourceSet.getResource(uri, true);
                IResourceValidator validator = ((XtextResource)resource).getResourceServiceProvider().getResourceValidator();
                validator.validate(resource, CheckMode.NORMAL_AND_FAST, CancelIndicator.NullImpl);
                writeAsOwl(resource, owlDirectory);
            }
        }
    }

    /** Checks whether project was successfully built */
    protected boolean isProjectSuccesfullyBuilt(XtextResourceSet resourceSet) {
        boolean noErrors = true;

        for (Resource resource : resourceSet.getResources()) {
            noErrors = noErrors && resource.getErrors().isEmpty();
        }

        return noErrors;
    }

    /** Accumulates error markers after building project */
    protected void accumulateErrorMarkers(XtextResourceSet resourceSet, Set<String> allBuildErrors) {
        StringBuilder sb = new StringBuilder();
        for (Resource resource : resourceSet.getResources()) {
            for (Resource.Diagnostic error : resource.getErrors()) {
                sb.setLength(0);
                sb.append(error.getLocation()).append(":");
                sb.append(error.getLine()).append(":");
                sb.append(error.getColumn()).append(": ");
                sb.append(error.getMessage());
                allBuildErrors.add(sb.toString());
            }
        }
    }

    // Builds SADL projects from command line
    @Override
    public Object start(IApplicationContext context) throws Exception {
        // Return whether projects were built successfully
        boolean buildSuccessful = true;
        Set<String> allBuildErrors = new LinkedHashSet<>();

        // Suppress log4j initialization warning and debug logs
        org.apache.log4j.BasicConfigurator.configure();
        org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.INFO);

        // Check whether workspace is being used in a running IDE
        if (!checkInstanceLocation())
            return ERROR;

        // Make sure workspace is saved after build
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        IWorkspaceDescription desc = root.getWorkspace().getDescription();
        final boolean isAutoBuilding = root.getWorkspace().isAutoBuilding();
        IProgressMonitor monitor = new PrintingProgressMonitor();
        try {
            // Check whether workspace can be read and written
            if (!root.isAccessible()) {
                System.err.println("Workspace: " + root.getLocationURI().toString() + " is not accessible");
                return ERROR;
            }

            // Turn off workspace auto-build and refresh workspace
            desc.setAutoBuilding(false);
            root.getWorkspace().setDescription(desc);
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
            for (String projURIStr : projectTreeToImport) {
                int status = importProject(projURIStr, true);
                if (status != OK)
                    return status;
            }

            // Import single projects
            for (String projURIStr : projectsToImport) {
                int status = importProject(projURIStr, false);
                if (status != OK)
                    return status;
            }

            // Create resource set to use when building projects
            Injector injector = new SADLCliAppStandaloneSetup().createInjectorAndDoEMFRegistration();
            XtextResourceSet resourceSet = injector.getInstance(XtextResourceSet.class);
            IProject[] allProjects = root.getProjects();
            Set<IProject> projectsToBuild = new LinkedHashSet<>();

            // Find projects user wanted cleaned
            if (cleanAll) {
                System.out.println("Cleaning all projects...");

                // Match all projects in workspace
                matchProjects(".*", allProjects, projectsToBuild); //$NON-NLS-1$
            } else {
                // Match regular expressions against project names
                for (String regEx : projectRegExToClean) {
                    matchProjects(regEx, allProjects, projectsToBuild);
                }
            }

            // Clean projects
            buildProjects(projectsToBuild, resourceSet, monitor, IncrementalProjectBuilder.CLEAN_BUILD, new LinkedHashSet<>());

            // Find projects user wanted built
            if (buildAll) {
                System.out.println("Building all projects...");

                // Match all projects in workspace
                matchProjects(".*", allProjects, projectsToBuild); //$NON-NLS-1$
            } else {
                // Match regular expressions against project names
                for (String regEx : projectRegExToBuild) {
                    matchProjects(regEx, allProjects, projectsToBuild);
                }
            }

            // Build projects using two passes in order to resolve dependencies between projects
            buildProjects(projectsToBuild, resourceSet, monitor, IncrementalProjectBuilder.INCREMENTAL_BUILD, new LinkedHashSet<>());
            buildSuccessful = buildProjects(projectsToBuild, resourceSet, monitor, IncrementalProjectBuilder.FULL_BUILD, allBuildErrors);
        } finally {
            // Wait for any outstanding jobs to finish
            while (!Job.getJobManager().isIdle()) {
                Thread.sleep(10);
            }

            // Reset workspace auto-build preference
            desc.setAutoBuilding(isAutoBuilding);
            root.getWorkspace().setDescription(desc);

            // Save modified workspace
            root.getWorkspace().save(true, monitor);
        }

        if (buildSuccessful) {
            System.out.println("Build completed successfully");
        } else {
            System.out.println("Build encountered " + allBuildErrors.size() + " errors");
            for (String buildError : allBuildErrors) {
                System.err.println(buildError);
            }
            // Don't print "Eclipse:\nJVM terminated. Exit code=1" message
            System.getProperties().put("eclipse.exitdata", "");
        }

        return buildSuccessful ? OK : ERROR;
    }

    @Override
    public void stop() {
        // do nothing
    }
}
