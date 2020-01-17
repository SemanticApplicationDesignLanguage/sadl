package com.ge.research.sadl.applications;

import com.ge.research.sadl.ide.handlers.SadlRunInferenceHandler;
import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.ISadlImplicitModelContentProvider;
import com.ge.research.sadl.processing.SadlConstants;
import com.google.common.base.Supplier;
import com.google.inject.Injector;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.apache.jena.atlas.logging.LogCtl;
import org.apache.log4j.PropertyConfigurator;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
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
import org.osgi.service.prefs.BackingStoreException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

// Based on com.ge.aviation.sadl.requirements.command_line.ExecuteToolChain with all ASSERT-specific
// parts removed
@SuppressWarnings("restriction")
public class ExecuteCommand implements IApplication {

    // Logger to print what we do
    private static final Logger LOGGER = LoggerFactory.getLogger(ExecuteCommand.class);

    // Default Configuration file location
    private String configFilePath = "SadlConfiguration.xml";

    // Files in OwlModels that we need to keep
    //    private final String configurationRdfFileName = "configuration.rdf";
    //    private final String ontPolicyRdfFileName = "ont-policy.rdf";

    // Essential Directories and Files for SADL
    private File importDirectory;
    private File owlModelDirectory;
    private String runInferenceFilePath;

    // Preference Mappings
    private Map<String, String> sadlPreferences;

    // Project files to load into memory
    private ArrayList<File> sadlFiles;
    private ArrayList<File> sreqFiles;
    private ArrayList<File> sverFiles;

    // Instances and resources loaded into memory
    private Injector injector;
    private XtextResourceSet resourceSet;

    // Control flow flags
    private boolean inferFlag = false;

    // Overwrite flag
    private boolean forceFlag = false;

    // Default preferences
    private boolean eclipseRefreshEnabled = false;
    private boolean eclipseRefreshLightweightEnabled = false;

    public ExecuteCommand() {
        this.sadlPreferences = new HashMap<String, String>();

        // Configure to avoid log4j warning messages on startup
        Properties log4jProperties = new Properties();
        log4jProperties.setProperty("log4j.rootLogger", "INFO, stdout");
        log4jProperties.setProperty("log4j.appender.stdout", "org.apache.log4j.ConsoleAppender");
        log4jProperties.setProperty(
                "log4j.appender.stdout.layout", "org.apache.log4j.PatternLayout");
        log4jProperties.setProperty("log4j.appender.stdout.layout.ConversionPattern", "%-5p %m%n");
        PropertyConfigurator.configure(log4jProperties);
        LogCtl.setCmdLogging();
    }

    @Override
    public void stop() {
        this.importDirectory = null;
        this.owlModelDirectory = null;
        this.runInferenceFilePath = null;
        this.sadlPreferences.clear();
        this.inferFlag = false;
        this.forceFlag = false;
    }

    @Override
    public Object start(IApplicationContext context) throws Exception {
        // Obtain command line arguments
        final Map<?, ?> argsMap = context.getArguments();
        final String[] args = (String[]) argsMap.get("application.args");

        refreshWorkspace();

        // Gets input/ted directories
        if (!obtainArguments(args)) {
            return null;
        }

        // Load and set the preferences for SADL
        if (!loadPreferences()) {
            return null;
        }

        // Set Eclipse Preferences to compatible values for SADL CLI
        setEclipsePreferences();

        // Import project if not already part of workspace
        if (!importProject()) {
            return null;
        }

        refreshWorkspace();

        // Is project being processed open
        if (!isProjectOpen()) {
            return null;
        }

        // Create Resources from sadl/sreq files and initiate Direct-Write
        if (inferFlag) {
            if (!importAndBuildSadlProject()) {
                LOGGER.error("Failure on import and build of SADL Project");
                return null;
            }
        }

        // Invoke SADL RunInference Tool
        if (inferFlag) {
            if (!runInference()) {
                LOGGER.error("Failure on inference of SADL file");
                return null;
            }
        }

        // Restore Eclipse Preferences
        restoreEclipsePreferences();
        refreshWorkspace();

        LOGGER.info("SADL Command-Line Interface complete");

        return IApplication.EXIT_OK;
    }

    private boolean importProject() {
        IWorkspace workspace = ResourcesPlugin.getWorkspace();
        IWorkspaceRoot root = workspace.getRoot();

        // Check to see if import directory is within workspace
        if (this.importDirectory
                .getAbsolutePath()
                .contains(root.getLocation().makeAbsolute().toOSString())) {
            return true;
        }

        // Check to see if project already exists within workspace
        for (IProject project : root.getProjects()) {
            String projectName = project.getName();
            if (this.importDirectory.getName().equals(projectName)) {
                if (forceFlag) {
                    try {
                        project.delete(true, true, null);
                        break;
                    } catch (CoreException e) {
                        LOGGER.error(
                                "Preexisting project could not be deleted from workspace: " + e);
                        return false;
                    }
                } else {
                    LOGGER.error(
                            "This project already exists within the workspace; use the \"-force\" flag to overwrite");
                    return false;
                }
            }
        }

        // Make a new directory in workspace for project and copy
        Path lNewProjectPath =
                Paths.get(
                        root.getLocation().makeAbsolute().toOSString(),
                        this.importDirectory.getName());
        try {
            if (!lNewProjectPath.toFile().exists()) {
                Files.createDirectory(lNewProjectPath);
            }
            copyDirectory(this.importDirectory.toPath(), lNewProjectPath);
        } catch (IOException e) {
            LOGGER.error("Unable to copy project into workspace: " + e);
            return false;
        }

        // Import project into Eclipse workspace framework
        try {
            IProjectDescription description =
                    workspace.loadProjectDescription(
                            new org.eclipse.core.runtime.Path(
                                    lNewProjectPath.toAbsolutePath() + "/.project"));
            IProject project = root.getProject(description.getName());
            project.create(description, null);
            project.open(null);
        } catch (CoreException e) {
            LOGGER.error("Unable to import project into workspace: " + e);
            return false;
        }

        // Set new import directory
        this.importDirectory = lNewProjectPath.toFile();

        return true;
    }

    private boolean copyDirectory(Path aSource, Path aDestination) throws IOException {
        Files.walk(aSource)
                .forEach(
                        lSource ->
                                copy(lSource, aDestination.resolve(aSource.relativize(lSource))));
        return true;
    }

    private void copy(Path aSource, Path aDestination) {
        try {
            Files.copy(aSource, aDestination, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException e) {
            LOGGER.error("Unable to copy file to workspace: " + aSource);
        }
    }

    private boolean isProjectOpen() {
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        for (IProject project : root.getProjects()) {
            String projectName = project.getName();
            if (this.importDirectory.getName().equals(projectName)) {
                if (project.isOpen()) {
                    return true;
                } else {
                    try {
                        project.open(null);
                    } catch (CoreException e) {
                        LOGGER.error(
                                projectName
                                        + " cannot be opened through this interface. Please open project within SADL GUI and execute again.");
                        return false;
                    }
                    return true;
                }
            }
        }

        LOGGER.error(
                "The project that is attempting to be processed is not part of the current workspace. Please set the correct workspace location within the .cmd file.");
        return false;
    }

    private boolean refreshWorkspace() {
        try {
            IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
            workspaceRoot.refreshLocal(IResource.DEPTH_INFINITE, null);
        } catch (CoreException e) {
            LOGGER.error("Error refreshing workspace: " + e);
        }
        return true;
    }

    private void setEclipsePreferences() {
        IEclipsePreferences ep = InstanceScope.INSTANCE.getNode("org.eclipse.core.resources");
        this.eclipseRefreshEnabled = ep.getBoolean(ResourcesPlugin.PREF_AUTO_REFRESH, false);
        this.eclipseRefreshLightweightEnabled =
                ep.getBoolean(ResourcesPlugin.PREF_LIGHTWEIGHT_AUTO_REFRESH, false);
        ep.putBoolean(ResourcesPlugin.PREF_AUTO_REFRESH, false);
        ep.putBoolean(ResourcesPlugin.PREF_LIGHTWEIGHT_AUTO_REFRESH, false);
        try {
            ep.flush();
        } catch (BackingStoreException e) {
            LOGGER.error("Error setting Eclipse preferences: " + e);
        }
    }

    private void restoreEclipsePreferences() {
        IEclipsePreferences ep = InstanceScope.INSTANCE.getNode("org.eclipse.core.resources");
        ep.putBoolean(ResourcesPlugin.PREF_AUTO_REFRESH, this.eclipseRefreshEnabled);
        ep.putBoolean(
                ResourcesPlugin.PREF_LIGHTWEIGHT_AUTO_REFRESH, eclipseRefreshLightweightEnabled);
        try {
            ep.flush();
        } catch (BackingStoreException e) {
            LOGGER.error("Error restoring Eclipse preferences: " + e);
        }
    }

    private boolean loadPreferences() {
        // Default location, will be update later to be passed in as argument perhaps
        File configFile = new File(this.configFilePath);
        if (!configFile.exists()) {
            LOGGER.error("Configuration file " + configFile + " does not exist");
            return false;
        }

        try {
            // load the xml
            DocumentBuilderFactory xmlDocFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder xmlDocBuilder = xmlDocFactory.newDocumentBuilder();
            Document xmlDoc = xmlDocBuilder.parse(configFile);
            NodeList prefNodes = xmlDoc.getElementsByTagName("preferences");
            for (int i = 0; i < prefNodes.getLength(); i++) {
                Element prefElement = (Element) prefNodes.item(i);
                String prefAttribute = prefElement.getAttribute("id");
                // Handle sadl preferences
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
                                        SadlPreferences.N3_FORMAT.getId());
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
            LOGGER.error("Error loading configuration file: " + e);
            return false;
        }

        return true;
    }

    private boolean obtainArguments(String[] args) {
        String importDirectory = null;
        String configFilePath = null;
        String runInferenceFilePath = null;

        for (int i = 0; i < args.length; i++) {
            if (args[i].startsWith("-import")) {
                importDirectory = args[i].split("=")[1];
            } else if (args[i].startsWith("-config")) {
                configFilePath = args[i].split("=")[1];
            } else if (args[i].startsWith("-infer")) {
                runInferenceFilePath = args[i].split("=")[1];
                this.inferFlag = true;
            } else if (args[i].equals("-force")) {
                this.forceFlag = true;
            }
        }

        if (importDirectory != null) {
            this.importDirectory = new File(importDirectory);
            if (!this.importDirectory.isDirectory()) {
                LOGGER.error("Given directory for -import argument was not valid");
                return false;
            }
        } else {
            LOGGER.error("Argument is missing: -import=[Directory of Project]");
            return false;
        }

        if (configFilePath != null) {
            this.configFilePath = configFilePath;
        }

        if (runInferenceFilePath != null) {
            this.runInferenceFilePath = runInferenceFilePath;
            if (!new File(this.importDirectory, runInferenceFilePath).canRead()) {
                LOGGER.error("Given file for -infer argument was not valid");
                return false;
            }
        } else {
            LOGGER.error("Argument is missing: -infer=[File to infer]");
            return false;
        }

        return true;
    }

    private boolean importAndBuildSadlProject() {
        // Output Directory for OwlModels
        this.owlModelDirectory = new File(this.importDirectory.getAbsolutePath(), "OwlModels");
        this.owlModelDirectory.mkdirs();

        // Clean project before build
        cleanProject();

        // Create Implicit Resources
        createImplicitResources();

        // Obtain files from import directory recursively
        sadlFiles = obtainFiles(this.importDirectory, new ArrayList<File>(), ".sadl");
        sreqFiles = obtainFiles(this.importDirectory, new ArrayList<File>(), ".sreq");
        sverFiles = obtainFiles(this.importDirectory, new ArrayList<File>(), ".sver");

        // Create and register dependency injectors
        injector = new SADLCliAppStandaloneSetup().createInjectorAndDoEMFRegistration();

        // Obtain a resource set
        resourceSet = injector.getInstance(XtextResourceSet.class);

        // Set Preferences
        Map<String, String> fullPreferenceMap = new HashMap<String, String>();
        PreferenceValuesByLanguage preferencesByLanguage = new PreferenceValuesByLanguage();
        fullPreferenceMap.putAll(this.sadlPreferences);
        preferencesByLanguage.put(
                "com.ge.research.sadl.SADL", new MapBasedPreferenceValues(fullPreferenceMap));
        preferencesByLanguage.attachToEmfObject(resourceSet);

        // build resource sets
        LOGGER.info("Creating resources in memory");
        for (File file : sadlFiles) {
            resourceSet.getResource(URI.createFileURI(file.getAbsolutePath()), true);
        }
        for (File file : sreqFiles) {
            resourceSet.getResource(URI.createFileURI(file.getAbsolutePath()), true);
        }
        for (File file : sverFiles) {
            resourceSet.getResource(URI.createFileURI(file.getAbsolutePath()), true);
        }
        LOGGER.info("Resolving interdependencies between resources");
        EcoreUtil2.resolveAll(resourceSet);

        // Direct Write
        List<File> allFiles = new ArrayList<File>();
        allFiles.addAll(sadlFiles);
        allFiles.addAll(sreqFiles);
        allFiles.addAll(sverFiles);
        Iterator<File> i = allFiles.iterator();
        while (i.hasNext()) {
            File file = i.next();
            Resource resource =
                    resourceSet.getResource(URI.createFileURI(file.getAbsolutePath()), true);
            if (resource instanceof XtextResource) {
                directWrite(resource);
            }
        }

        return true;
    }

    private void cleanProject() {
        // If we clean these files first, ExecuteCommand reports errors when building the project:
        // ERROR com.ge.research.sadl.external.ExternalEmfResource - Error occurred while loading
        // the resource. URI:platform:/resource/HawkeyeUAV/OwlModels/Components.owl
        // org.eclipse.emf.ecore.resource.Resource$IOWrappedException: Resource
        // '/HawkeyeUAV/OwlModels/Components.owl' does not exist.
        //
        // if (this.owlModelDirectory.exists()) {
        //     File[] files = this.owlModelDirectory.listFiles();
        //     for (File file : files) {
        //         if (!file.getName().equals(configurationRdfFileName) &&
        //                 !file.getName().equals(ontPolicyRdfFileName)) {
        //             file.delete();
        //         }
        //     }
        // }
    }

    private boolean runInference() {
        try {
            // Open the requested SADL resource within the newly imported project
            final File runInferenceFile = new File(this.importDirectory, this.runInferenceFilePath);
            final XtextResource sadlResource =
                    (XtextResource)
                            resourceSet.getResource(
                                    URI.createFileURI(runInferenceFile.getAbsolutePath()), true);

            // Hand off the job to the run inference handler.
            final SadlRunInferenceHandler runInferenceHandler =
                    injector.getInstance(SadlRunInferenceHandler.class);
            final Supplier<XtextResource> resourceSupplier = () -> sadlResource;
            runInferenceHandler.run(runInferenceFile.toPath(), resourceSupplier, sadlPreferences);
        } catch (Exception e) {
            LOGGER.error("Error running inference: " + e);
            return false;
        }

        return true;
    }

    private void createImplicitResources() {
        // Check for and possibly create implicit model
        String implicitModelPath =
                this.importDirectory.getAbsolutePath()
                        + "/"
                        + SadlConstants.SADL_IMPLICIT_MODEL_FOLDER
                        + "/"
                        + SadlConstants.SADL_IMPLICIT_MODEL_FILENAME;
        File implicitModel = new File(implicitModelPath);
        if (!implicitModel.exists()) {
            new ISadlImplicitModelContentProvider.Default()
                    .createImplicitModel(implicitModel, true);
        }
        // Create built-in function model
        try {
            JenaBasedSadlModelProcessor.createBuiltinFunctionImplicitModel(
                    this.importDirectory.getAbsolutePath());
        } catch (Exception e) {
            LOGGER.error("Error creating SadlBuiltinFunctions.sadl: " + e);
        }
    }

    private ArrayList<File> obtainFiles(
            File rootDirectory, ArrayList<File> fileList, String fileExtension)
            throws NullPointerException {
        for (File file : rootDirectory.listFiles()) {
            if (file.isDirectory()) {
                obtainFiles(file, fileList, fileExtension);
            } else if (file.isFile()) {
                if (file.getAbsolutePath().endsWith(fileExtension)) {
                    fileList.add(file);
                }
            }
        }

        return fileList;
    }

    private void directWrite(Resource resource) {
        URI resourceUri = ((XtextResource) resource).getURI();
        LOGGER.info("Building " + resourceUri);

        // Validation
        IResourceValidator validator =
                ((XtextResource) resource).getResourceServiceProvider().getResourceValidator();
        validator.validate(resource, CheckMode.NORMAL_AND_FAST, CancelIndicator.NullImpl);

        // Generation
        GeneratorDelegate generator = null;
        JavaIoFileSystemAccess access = null;
        GeneratorContext context = new GeneratorContext();
        context.setCancelIndicator(CancelIndicator.NullImpl);
        generator =
                ((XtextResource) resource)
                        .getResourceServiceProvider()
                        .get(GeneratorDelegate.class);
        access =
                ((XtextResource) resource)
                        .getResourceServiceProvider()
                        .get(JavaIoFileSystemAccess.class);
        access.setOutputPath(owlModelDirectory.getAbsolutePath());
        generator.generate(resource, access, context);
    }
}
