<!-- markdownlint-disable line-length -->

# SADL-Eclipse: Image for translating SADL files to OWL files

You can use this dockerized SADL-Eclipse image to translate existing
SADL projects' SADL files to OWL files on the command line.  You can
run the most recent development version of `sadl/sadl-eclipse` with
the tag `dev` or a stable release of `sadl/sadl-eclipse` with an
explicitly versioned tag, the tag `latest`, or no tag at all.

SADL-Eclipse accepts the following arguments at the end of its `docker
run` command line:

  - Import a project                        -import     {/path/to/project}
  - Import all projects in a tree           -importAll  {/path/to/projectTree}
  - Clean and build all imported projects   -cleanBuild
  - Build all imported projects             -build

A SADL-Eclipse container has its own internal (and initially empty)
workspace.  Your `docker run` command must mount a directory from your
host's filesystem inside the running container and import one or more
projects from the mounted directory into the workspace.  The import
simply records the projects' locations within the internal workspace
without copying any files.

A SADL-Eclipse container will normally run as a non-root user with
`1000` as its UID and `1000` as its GID unless you override these
default UID & GID values.  If you import projects having any other UID
& GID values into your SADL-Eclipse container, you need to run your
SADL-Eclipse container as root while passing your custom UID & GID
values in a `RUN_AS` environment variable (two integers separated by
whitespace characters, e.g., `docker run -u 0 -e RUN_AS="1001 1002"
...`) to avoid file permission errors or other problems building your
projects.

When you build your projects with your SADL-Eclipse container, it will
update your projects' files in place, that is, it will populate your
projects' `OwlModels` subdirectories with new OWL files freshly
generated from your SADL files.  Using `-build` will only add or
update OWL files in projects' `OwlModels` subdirectories, while using
`-cleanBuild` will remove existing files before generating new files
in projects' `OwlModels` subdirectory.  The only files `-cleanBuild`
will not remove from your projects' `OwlModels` subdirectories are
files with file extensions ".rdf", ".yml", and ".xml".

## Examples

Here is an example which uses `sadl/sadl-eclipse` to import, clean,
and build a single SADL project called `STEM`:

```shell
$ docker run --rm -v "/path/to/STEM:/STEM" sadl/sadl-eclipse:dev -import /STEM -cleanBuild
Refreshing workspace.
Create.
Opening 'STEM'.
Refreshing '/STEM'.
Cleaning all projects...
Cleaning STEM
Cleaning /STEM/OwlModels/BaseModel.owl
Cleaning /STEM/OwlModels/Run.owl
Cleaning /STEM/OwlModels/SadlBuiltinFunctions.owl
Cleaning /STEM/OwlModels/Queries.owl
Cleaning /STEM/OwlModels/SadlImplicitModel.owl
Cleaning /STEM/OwlModels/SadlListModel.owl
Cleaning /STEM/OwlModels/STEMRules.rules
Cleaning /STEM/OwlModels/STEMRules.owl
Cleaning /STEM/OwlModels/SadlBaseModel.owl
Building all projects...
Building
Building '/STEM'
Invoking 'Xtext Project Builder' on '/STEM'.
Building STEM: Collecting resources
Building STEM: Create resource descriptions
Building STEM: Writing new resource description BaseModel.sadl
Building STEM: Writing new resource description SadlBuiltinFunctions.sadl
Building STEM: Writing new resource description Run.sadl
Building STEM: Writing new resource description SadlImplicitModel.sadl
Building STEM: Writing new resource description ScnBusBindingsWizard.tmpl
Building STEM: Writing new resource description STEMRules.sadl
Building STEM: Writing new resource description ScnCompProps.tmpl
Building STEM: Writing new resource description ScnConnections.tmpl
Building STEM: Writing new resource description Queries.sadl
Building STEM: Writing new resource description ScnBusBindings.tmpl
Building STEM: Writing new resource description ScnCompPropsWizard.tmpl
Building STEM: Updating resource BaseModel.sadl
Building STEM: Updating resource SadlBuiltinFunctions.sadl
Building STEM: Updating resource Run.sadl
Building STEM: Updating resource SadlImplicitModel.sadl
Building STEM: Updating resource ScnBusBindingsWizard.tmpl
Building STEM: Updating resource STEMRules.sadl
Building STEM: Updating resource ScnCompProps.tmpl
Building STEM: Updating resource ScnConnections.tmpl
Building STEM: Updating resource Queries.sadl
Building STEM: Updating resource ScnBusBindings.tmpl
Building STEM: Updating resource ScnCompPropsWizard.tmpl
Building STEM: Invoking build participants
Building STEM: Refreshing '/STEM/OwlModels'.
Building STEM: Compile SadlBuiltinFunctions.sadl
Building STEM: Compile BaseModel.sadl
Building STEM: Compile Queries.sadl
Building STEM: Compile SadlImplicitModel.sadl
Building STEM: Compile Run.sadl
Building STEM: Refreshing '/STEM/OwlModels'.
Building STEM: Compile ScnBusBindings.tmpl
0 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build STEM in 4725 ms
Building '/STEM'
Invoking 'Xtext Project Builder' on '/STEM'.
Building STEM: Collecting resources
Building STEM: Create resource descriptions
Building STEM: Writing new resource description BaseModel.sadl
Building STEM: Writing new resource description SadlBuiltinFunctions.sadl
Building STEM: Writing new resource description Run.sadl
Building STEM: Writing new resource description SadlImplicitModel.sadl
Building STEM: Writing new resource description ScnBusBindingsWizard.tmpl
Building STEM: Writing new resource description STEMRules.sadl
Building STEM: Writing new resource description ScnCompProps.tmpl
Building STEM: Writing new resource description ScnConnections.tmpl
Building STEM: Writing new resource description Queries.sadl
Building STEM: Writing new resource description ScnBusBindings.tmpl
Building STEM: Writing new resource description ScnCompPropsWizard.tmpl
Building STEM: Updating resource BaseModel.sadl
Building STEM: Updating resource SadlBuiltinFunctions.sadl
Building STEM: Updating resource Run.sadl
Building STEM: Updating resource SadlImplicitModel.sadl
Building STEM: Updating resource ScnBusBindingsWizard.tmpl
Building STEM: Updating resource STEMRules.sadl
Building STEM: Updating resource ScnCompProps.tmpl
Building STEM: Updating resource ScnConnections.tmpl
Building STEM: Updating resource Queries.sadl
Building STEM: Updating resource ScnBusBindings.tmpl
Building STEM: Updating resource ScnCompPropsWizard.tmpl
Building STEM: Invoking build participants
Building STEM: Refreshing '/STEM/OwlModels'.
Building STEM: Compile BaseModel.sadl
Building STEM: Compile SadlImplicitModel.sadl
Building STEM: Compile Run.sadl
Building STEM: Compile Queries.sadl
Building STEM: Compile SadlBuiltinFunctions.sadl
Building STEM: Deleting '/STEM/OwlModels/SadlBaseModel.owl'.
Building STEM: Deleting '/STEM/OwlModels/SadlListModel.owl'.
Building STEM: Refreshing '/STEM/OwlModels'.
Building STEM: Compile ScnBusBindings.tmpl
1719 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build STEM in 1715 ms
Building '/STEM'
Build failed with 1133 errors
Marker [on: /STEM/STEMRules.sadl, id: 1281, type: com.ge.research.sadl.ui.sadl.check.normal, attributes: [CODE_KEY: processor.issue, COLUMN_KEY: 29, URI_KEY: platform:/resource/STEM/STEMRules.sadl#//@elements.9/@thens.0/@left/@right, charEnd: 2534, charStart: 2521, lineNumber: 76, location: line: 76 /STEM/STEMRules.sadl, message: The use of articles is not enabled in preferences but the content is only valid when enabled., severity: 2], created: 12/4/21, 9:38 PM]
...
Eclipse:
JVM terminated. Exit code=1
```

Here is an example which uses `sadl/sadl-eclipse` to import, clean,
and build all SADL projects inside a `RACK` directory while overriding
which UID and GID the container runs as:

```shell
$ docker run --rm -u 0 -e RUN_AS="$(id -u) $(id -g)" -v "$(pwd)/RACK:/RACK" sadl/sadl-eclipse:dev -importAll /RACK -cleanBuild
Changing UID of sadl from 1000 to 1001
Changing GID of sadl from 1000 to 1002
Refreshing workspace.
Create.
Opening 'GrammaTech-Ontology'.
Refreshing '/GrammaTech-Ontology'.
Create.
Opening 'LM-Ontology'.
Refreshing '/LM-Ontology'.
Create.
Opening 'CounterApplicationUnitTesting'.
Refreshing '/CounterApplicationUnitTesting'.
Create.
Opening 'Boeing-Ontology'.
Refreshing '/Boeing-Ontology'.
Create.
Opening 'RACK-Ontology'.
Refreshing '/RACK-Ontology'.
Create.
Opening 'GE-Ontology'.
Refreshing '/GE-Ontology'.
Create.
Opening 'SRI-Ontology'.
Refreshing '/SRI-Ontology'.
Create.
Opening 'Provenance-Example'.
Refreshing '/Provenance-Example'.
Create.
Opening 'STR-Ontology'.
Refreshing '/STR-Ontology'.
Cleaning all projects...
Cleaning Boeing-Ontology
Cleaning /RACK/Boeing-Ontology/OwlModels/Boeing.owl
Cleaning /RACK/Boeing-Ontology/OwlModels/SadlBuiltinFunctions.owl
Cleaning /RACK/Boeing-Ontology/OwlModels/SadlImplicitModel.owl
Cleaning /RACK/Boeing-Ontology/OwlModels/SadlListModel.owl
Cleaning /RACK/Boeing-Ontology/OwlModels/SadlBaseModel.owl
Cleaning CounterApplicationUnitTesting
Cleaning /RACK/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/OwlModels/CounterApplicationUnitTesting.owl
Cleaning /RACK/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/OwlModels/SadlBuiltinFunctions.owl
Cleaning /RACK/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/OwlModels/SadlImplicitModel.owl
Cleaning /RACK/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/OwlModels/SadlListModel.owl
Cleaning /RACK/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/OwlModels/SadlBaseModel.owl
Cleaning GE-Ontology
Cleaning /RACK/GE-Ontology/OwlModels/SadlBuiltinFunctions.owl
Cleaning /RACK/GE-Ontology/OwlModels/SadlImplicitModel.owl
Cleaning /RACK/GE-Ontology/OwlModels/GE.owl
Cleaning /RACK/GE-Ontology/OwlModels/SadlListModel.owl
Cleaning /RACK/GE-Ontology/OwlModels/SadlBaseModel.owl
Cleaning /RACK/GE-Ontology/OwlModels/CPS.owl
Cleaning GrammaTech-Ontology
Cleaning /RACK/GrammaTech-Ontology/OwlModels/GrammaTech.owl
Cleaning /RACK/GrammaTech-Ontology/OwlModels/SadlBuiltinFunctions.owl
Cleaning /RACK/GrammaTech-Ontology/OwlModels/SadlImplicitModel.owl
Cleaning /RACK/GrammaTech-Ontology/OwlModels/SadlListModel.owl
Cleaning /RACK/GrammaTech-Ontology/OwlModels/SadlBaseModel.owl
Cleaning LM-Ontology
Cleaning /RACK/LM-Ontology/OwlModels/SadlBuiltinFunctions.owl
Cleaning /RACK/LM-Ontology/OwlModels/SadlImplicitModel.owl
Cleaning /RACK/LM-Ontology/OwlModels/SadlListModel.owl
Cleaning /RACK/LM-Ontology/OwlModels/LM.owl
Cleaning /RACK/LM-Ontology/OwlModels/SadlBaseModel.owl
Cleaning Provenance-Example
Cleaning /RACK/Provenance-Example/OwlModels/SadlBuiltinFunctions.owl
Cleaning /RACK/Provenance-Example/OwlModels/SadlImplicitModel.owl
Cleaning /RACK/Provenance-Example/OwlModels/SadlListModel.owl
Cleaning /RACK/Provenance-Example/OwlModels/SadlBaseModel.owl
Cleaning RACK-Ontology
Cleaning /RACK/RACK-Ontology/OwlModels/DOCUMENT.owl
Cleaning /RACK/RACK-Ontology/OwlModels/MIL-STD-881D.owl
Cleaning /RACK/RACK-Ontology/OwlModels/MIL-STD-881D-AppxA.owl
Cleaning /RACK/RACK-Ontology/OwlModels/DO-330.owl
Cleaning /RACK/RACK-Ontology/OwlModels/REVIEW.owl
Cleaning /RACK/RACK-Ontology/OwlModels/DO-178C.owl
Cleaning /RACK/RACK-Ontology/OwlModels/MIL-STD-881D-AppxD.owl
Cleaning /RACK/RACK-Ontology/OwlModels/HAZARD.owl
Cleaning /RACK/RACK-Ontology/OwlModels/MIL-STD-881D-AppxB.owl
Cleaning /RACK/RACK-Ontology/OwlModels/PROCESS.owl
Cleaning /RACK/RACK-Ontology/OwlModels/SadlBuiltinFunctions.owl
Cleaning /RACK/RACK-Ontology/OwlModels/BASELINE.owl
Cleaning /RACK/RACK-Ontology/OwlModels/SadlImplicitModel.owl
Cleaning /RACK/RACK-Ontology/OwlModels/SadlListModel.owl
Cleaning /RACK/RACK-Ontology/OwlModels/MODEL.owl
Cleaning /RACK/RACK-Ontology/OwlModels/CONFIDENCE.owl
Cleaning /RACK/RACK-Ontology/OwlModels/ARP-4754A.owl
Cleaning /RACK/RACK-Ontology/OwlModels/SYSTEM.owl
Cleaning /RACK/RACK-Ontology/OwlModels/AGENTS.owl
Cleaning /RACK/RACK-Ontology/OwlModels/PROV-S.owl
Cleaning /RACK/RACK-Ontology/OwlModels/REQUIREMENTS.owl
Cleaning /RACK/RACK-Ontology/OwlModels/FILE.owl
Cleaning /RACK/RACK-Ontology/OwlModels/HARDWARE.owl
Cleaning /RACK/RACK-Ontology/OwlModels/SOFTWARE.owl
Cleaning /RACK/RACK-Ontology/OwlModels/SadlBaseModel.owl
Cleaning /RACK/RACK-Ontology/OwlModels/SECURITY.owl
Cleaning /RACK/RACK-Ontology/OwlModels/GeneratePropInfoCSV.owl
Cleaning /RACK/RACK-Ontology/OwlModels/ANALYSIS.owl
Cleaning /RACK/RACK-Ontology/OwlModels/TESTING.owl
Cleaning /RACK/RACK-Ontology/OwlModels/MIL-STD-881D-AppxC.owl
Cleaning SRI-Ontology
Cleaning /RACK/SRI-Ontology/OwlModels/SadlBuiltinFunctions.owl
Cleaning /RACK/SRI-Ontology/OwlModels/SadlImplicitModel.owl
Cleaning /RACK/SRI-Ontology/OwlModels/SadlListModel.owl
Cleaning /RACK/SRI-Ontology/OwlModels/SRI.owl
Cleaning /RACK/SRI-Ontology/OwlModels/SadlBaseModel.owl
Cleaning STR-Ontology
Cleaning /RACK/STR-Ontology/OwlModels/STR.owl
Cleaning /RACK/STR-Ontology/OwlModels/SadlBuiltinFunctions.owl
Cleaning /RACK/STR-Ontology/OwlModels/SadlImplicitModel.owl
Cleaning /RACK/STR-Ontology/OwlModels/SadlListModel.owl
Cleaning /RACK/STR-Ontology/OwlModels/SadlBaseModel.owl
Building all projects...
Building
Building '/RACK-Ontology'
Invoking 'Xtext Project Builder' on '/RACK-Ontology'.
Building RACK-Ontology: Collecting resources
SWT SessionManagerDBus: Failed to connect to org.gnome.SessionManager: Could not connect: Connection refused
SWT SessionManagerDBus: Failed to connect to org.xfce.SessionManager: Could not connect: Connection refused
Building RACK-Ontology: Create resource descriptions
Building RACK-Ontology: Writing new resource description PROCESS.sadl
Building RACK-Ontology: Writing new resource description REVIEW.sadl
Building RACK-Ontology: Writing new resource description HARDWARE.sadl
Building RACK-Ontology: Writing new resource description ARP-4754A.sadl
Building RACK-Ontology: Writing new resource description REQUIREMENTS.sadl
Building RACK-Ontology: Writing new resource description SadlImplicitModel.sadl
Building RACK-Ontology: Writing new resource description SOFTWARE.sadl
Building RACK-Ontology: Writing new resource description MODEL.sadl
Building RACK-Ontology: Writing new resource description BASELINE.sadl
Building RACK-Ontology: Writing new resource description HAZARD.sadl
Building RACK-Ontology: Writing new resource description MIL-STD-881D.sadl
Building RACK-Ontology: Writing new resource description MIL-STD-881D-AppxA.sadl
Building RACK-Ontology: Writing new resource description SadlBuiltinFunctions.sadl
Building RACK-Ontology: Writing new resource description DO-178C.sadl
Building RACK-Ontology: Writing new resource description CONFIDENCE.sadl
Building RACK-Ontology: Writing new resource description DOCUMENT.sadl
Building RACK-Ontology: Writing new resource description MIL-STD-881D-AppxD.sadl
Building RACK-Ontology: Writing new resource description SECURITY.sadl
Building RACK-Ontology: Writing new resource description ANALYSIS.sadl
Building RACK-Ontology: Writing new resource description SYSTEM.sadl
Building RACK-Ontology: Writing new resource description DO-330.sadl
Building RACK-Ontology: Writing new resource description TESTING.sadl
Building RACK-Ontology: Writing new resource description FILE.sadl
Building RACK-Ontology: Writing new resource description MIL-STD-881D-AppxC.sadl
Building RACK-Ontology: Writing new resource description GeneratePropInfoCSV.sadl
Building RACK-Ontology: Writing new resource description MIL-STD-881D-AppxB.sadl
Building RACK-Ontology: Writing new resource description AGENTS.sadl
Building RACK-Ontology: Writing new resource description PROV-S.sadl
Building RACK-Ontology: Updating resource PROCESS.sadl
Building RACK-Ontology: Updating resource REVIEW.sadl
Building RACK-Ontology: Updating resource HARDWARE.sadl
Building RACK-Ontology: Updating resource ARP-4754A.sadl
Building RACK-Ontology: Updating resource REQUIREMENTS.sadl
Building RACK-Ontology: Updating resource SadlImplicitModel.sadl
Building RACK-Ontology: Updating resource SOFTWARE.sadl
Building RACK-Ontology: Updating resource MODEL.sadl
Building RACK-Ontology: Updating resource BASELINE.sadl
Building RACK-Ontology: Updating resource HAZARD.sadl
Building RACK-Ontology: Updating resource MIL-STD-881D.sadl
Building RACK-Ontology: Updating resource MIL-STD-881D-AppxA.sadl
Building RACK-Ontology: Updating resource SadlBuiltinFunctions.sadl
Building RACK-Ontology: Updating resource DO-178C.sadl
Building RACK-Ontology: Updating resource CONFIDENCE.sadl
Building RACK-Ontology: Updating resource DOCUMENT.sadl
Building RACK-Ontology: Updating resource MIL-STD-881D-AppxD.sadl
Building RACK-Ontology: Updating resource SECURITY.sadl
Building RACK-Ontology: Updating resource ANALYSIS.sadl
Building RACK-Ontology: Updating resource SYSTEM.sadl
Building RACK-Ontology: Updating resource DO-330.sadl
Building RACK-Ontology: Updating resource TESTING.sadl
Building RACK-Ontology: Updating resource FILE.sadl
Building RACK-Ontology: Updating resource MIL-STD-881D-AppxC.sadl
Building RACK-Ontology: Updating resource GeneratePropInfoCSV.sadl
Building RACK-Ontology: Updating resource MIL-STD-881D-AppxB.sadl
Building RACK-Ontology: Updating resource AGENTS.sadl
Building RACK-Ontology: Updating resource PROV-S.sadl
Building RACK-Ontology: Invoking build participants
Building RACK-Ontology: Refreshing '/RACK-Ontology/OwlModels'.
Building RACK-Ontology: Compile PROCESS.sadl
Building RACK-Ontology: Compile GeneratePropInfoCSV.sadl
Building RACK-Ontology: Compile MIL-STD-881D-AppxB.sadl
Building RACK-Ontology: Compile DO-178C.sadl
Building RACK-Ontology: Compile CONFIDENCE.sadl
Building RACK-Ontology: Compile DOCUMENT.sadl
Building RACK-Ontology: Compile HAZARD.sadl
Building RACK-Ontology: Compile SYSTEM.sadl
Building RACK-Ontology: Compile MIL-STD-881D.sadl
Building RACK-Ontology: Compile SadlBuiltinFunctions.sadl
Building RACK-Ontology: Compile ANALYSIS.sadl
Building RACK-Ontology: Compile PROV-S.sadl
Building RACK-Ontology: Compile SECURITY.sadl
Building RACK-Ontology: Compile REVIEW.sadl
Building RACK-Ontology: Compile SadlImplicitModel.sadl
Building RACK-Ontology: Compile DO-330.sadl
Building RACK-Ontology: Compile MODEL.sadl
Building RACK-Ontology: Compile AGENTS.sadl
Building RACK-Ontology: Compile ARP-4754A.sadl
Building RACK-Ontology: Compile REQUIREMENTS.sadl
Building RACK-Ontology: Compile SOFTWARE.sadl
Building RACK-Ontology: Compile FILE.sadl
Building RACK-Ontology: Compile MIL-STD-881D-AppxA.sadl
Building RACK-Ontology: Compile HARDWARE.sadl
Building RACK-Ontology: Compile MIL-STD-881D-AppxD.sadl
Building RACK-Ontology: Compile MIL-STD-881D-AppxC.sadl
Building RACK-Ontology: Compile BASELINE.sadl
Building RACK-Ontology: Compile TESTING.sadl
0 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build RACK-Ontology in 9991 ms
Building '/Provenance-Example'
Invoking 'Xtext Project Builder' on '/Provenance-Example'.
Building Provenance-Example: Collecting resources
Building Provenance-Example: Create resource descriptions
Building Provenance-Example: Writing new resource description SadlImplicitModel.sadl
Building Provenance-Example: Writing new resource description SadlBuiltinFunctions.sadl
Building Provenance-Example: Updating resource SadlImplicitModel.sadl
Building Provenance-Example: Updating resource SadlBuiltinFunctions.sadl
Building Provenance-Example: Invoking build participants
Building Provenance-Example: Refreshing '/Provenance-Example/OwlModels'.
Building Provenance-Example: Compile SadlImplicitModel.sadl
Building Provenance-Example: Compile SadlBuiltinFunctions.sadl
133 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build Provenance-Example in 128 ms
Building '/LM-Ontology'
Invoking 'Xtext Project Builder' on '/LM-Ontology'.
Building LM-Ontology: Collecting resources
Building LM-Ontology: Create resource descriptions
Building LM-Ontology: Writing new resource description SadlBuiltinFunctions.sadl
Building LM-Ontology: Writing new resource description LM.sadl
Building LM-Ontology: Writing new resource description SadlImplicitModel.sadl
Building LM-Ontology: Updating resource SadlBuiltinFunctions.sadl
Building LM-Ontology: Updating resource LM.sadl
Building LM-Ontology: Updating resource SadlImplicitModel.sadl
Building LM-Ontology: Invoking build participants
Building LM-Ontology: Refreshing '/LM-Ontology/OwlModels'.
Building LM-Ontology: Compile LM.sadl
Building LM-Ontology: Compile SadlImplicitModel.sadl
Building LM-Ontology: Compile SadlBuiltinFunctions.sadl
362 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build LM-Ontology in 228 ms
Building '/GrammaTech-Ontology'
Invoking 'Xtext Project Builder' on '/GrammaTech-Ontology'.
Building GrammaTech-Ontology: Collecting resources
Building GrammaTech-Ontology: Create resource descriptions
Building GrammaTech-Ontology: Writing new resource description SadlImplicitModel.sadl
Building GrammaTech-Ontology: Writing new resource description SadlBuiltinFunctions.sadl
Building GrammaTech-Ontology: Writing new resource description GrammaTech.sadl
Building GrammaTech-Ontology: Updating resource SadlImplicitModel.sadl
Building GrammaTech-Ontology: Updating resource SadlBuiltinFunctions.sadl
Building GrammaTech-Ontology: Updating resource GrammaTech.sadl
Building GrammaTech-Ontology: Invoking build participants
Building GrammaTech-Ontology: Refreshing '/GrammaTech-Ontology/OwlModels'.
Building GrammaTech-Ontology: Compile GrammaTech.sadl
Building GrammaTech-Ontology: Compile SadlImplicitModel.sadl
Building GrammaTech-Ontology: Compile SadlBuiltinFunctions.sadl
650 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build GrammaTech-Ontology in 287 ms
Building '/GE-Ontology'
Invoking 'Xtext Project Builder' on '/GE-Ontology'.
Building GE-Ontology: Collecting resources
Building GE-Ontology: Create resource descriptions
Building GE-Ontology: Writing new resource description GE.sadl
Building GE-Ontology: Writing new resource description SadlBuiltinFunctions.sadl
Building GE-Ontology: Writing new resource description CPS.sadl
Building GE-Ontology: Writing new resource description SadlImplicitModel.sadl
Building GE-Ontology: Updating resource GE.sadl
Building GE-Ontology: Updating resource SadlBuiltinFunctions.sadl
Building GE-Ontology: Updating resource CPS.sadl
Building GE-Ontology: Updating resource SadlImplicitModel.sadl
Building GE-Ontology: Invoking build participants
Building GE-Ontology: Refreshing '/GE-Ontology/OwlModels'.
Building GE-Ontology: Compile CPS.sadl
Building GE-Ontology: Compile SadlBuiltinFunctions.sadl
Building GE-Ontology: Compile SadlImplicitModel.sadl
Building GE-Ontology: Compile GE.sadl
1466 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build GE-Ontology in 816 ms
Building '/CounterApplicationUnitTesting'
Invoking 'Xtext Project Builder' on '/CounterApplicationUnitTesting'.
Building CounterApplicationUnitTesting: Collecting resources
Building CounterApplicationUnitTesting: Create resource descriptions
Building CounterApplicationUnitTesting: Writing new resource description SadlBuiltinFunctions.sadl
Building CounterApplicationUnitTesting: Writing new resource description SadlImplicitModel.sadl
Building CounterApplicationUnitTesting: Writing new resource description CounterApplicationUnitTesting.sadl
Building CounterApplicationUnitTesting: Updating resource SadlBuiltinFunctions.sadl
Building CounterApplicationUnitTesting: Updating resource SadlImplicitModel.sadl
Building CounterApplicationUnitTesting: Updating resource CounterApplicationUnitTesting.sadl
Building CounterApplicationUnitTesting: Invoking build participants
Building CounterApplicationUnitTesting: Refreshing '/CounterApplicationUnitTesting/OwlModels'.
Building CounterApplicationUnitTesting: Compile SadlImplicitModel.sadl
Building CounterApplicationUnitTesting: Compile SadlBuiltinFunctions.sadl
Building CounterApplicationUnitTesting: Compile CounterApplicationUnitTesting.sadl
2539 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build CounterApplicationUnitTesting in 1073 ms
Building '/Boeing-Ontology'
Invoking 'Xtext Project Builder' on '/Boeing-Ontology'.
Building Boeing-Ontology: Collecting resources
Building Boeing-Ontology: Create resource descriptions
Building Boeing-Ontology: Writing new resource description Boeing.sadl
Building Boeing-Ontology: Writing new resource description SadlBuiltinFunctions.sadl
Building Boeing-Ontology: Writing new resource description SadlImplicitModel.sadl
Building Boeing-Ontology: Updating resource Boeing.sadl
Building Boeing-Ontology: Updating resource SadlBuiltinFunctions.sadl
Building Boeing-Ontology: Updating resource SadlImplicitModel.sadl
Building Boeing-Ontology: Invoking build participants
Building Boeing-Ontology: Refreshing '/Boeing-Ontology/OwlModels'.
Building Boeing-Ontology: Compile SadlImplicitModel.sadl
Building Boeing-Ontology: Compile SadlBuiltinFunctions.sadl
Building Boeing-Ontology: Compile Boeing.sadl
4199 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build Boeing-Ontology in 1660 ms
Building '/SRI-Ontology'
Invoking 'Xtext Project Builder' on '/SRI-Ontology'.
Building SRI-Ontology: Collecting resources
Building SRI-Ontology: Create resource descriptions
Building SRI-Ontology: Writing new resource description SadlImplicitModel.sadl
Building SRI-Ontology: Writing new resource description SadlBuiltinFunctions.sadl
Building SRI-Ontology: Writing new resource description SRI.sadl
Building SRI-Ontology: Updating resource SadlImplicitModel.sadl
Building SRI-Ontology: Updating resource SadlBuiltinFunctions.sadl
Building SRI-Ontology: Updating resource SRI.sadl
Building SRI-Ontology: Invoking build participants
Building SRI-Ontology: Refreshing '/SRI-Ontology/OwlModels'.
Building SRI-Ontology: Compile SadlImplicitModel.sadl
Building SRI-Ontology: Compile SRI.sadl
Building SRI-Ontology: Compile SadlBuiltinFunctions.sadl
13234 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build SRI-Ontology in 9035 ms
Building '/STR-Ontology'
Invoking 'Xtext Project Builder' on '/STR-Ontology'.
Building STR-Ontology: Collecting resources
Building STR-Ontology: Create resource descriptions
Building STR-Ontology: Writing new resource description STR.sadl
Building STR-Ontology: Writing new resource description SadlImplicitModel.sadl
Building STR-Ontology: Writing new resource description SadlBuiltinFunctions.sadl
Building STR-Ontology: Updating resource STR.sadl
Building STR-Ontology: Updating resource SadlImplicitModel.sadl
Building STR-Ontology: Updating resource SadlBuiltinFunctions.sadl
Building STR-Ontology: Invoking build participants
Building STR-Ontology: Refreshing '/STR-Ontology/OwlModels'.
Building STR-Ontology: Compile SadlBuiltinFunctions.sadl
Building STR-Ontology: Compile STR.sadl
Building STR-Ontology: Compile SadlImplicitModel.sadl
13427 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build STR-Ontology in 178 ms
Building '/RACK-Ontology'
Invoking 'Xtext Project Builder' on '/RACK-Ontology'.
Building RACK-Ontology: Collecting resources
Building RACK-Ontology: Create resource descriptions
Building RACK-Ontology: Writing new resource description PROCESS.sadl
Building RACK-Ontology: Writing new resource description REVIEW.sadl
Building RACK-Ontology: Writing new resource description HARDWARE.sadl
Building RACK-Ontology: Writing new resource description ARP-4754A.sadl
Building RACK-Ontology: Writing new resource description REQUIREMENTS.sadl
Building RACK-Ontology: Writing new resource description SadlImplicitModel.sadl
Building RACK-Ontology: Writing new resource description SOFTWARE.sadl
Building RACK-Ontology: Writing new resource description MODEL.sadl
Building RACK-Ontology: Writing new resource description BASELINE.sadl
Building RACK-Ontology: Writing new resource description HAZARD.sadl
Building RACK-Ontology: Writing new resource description MIL-STD-881D.sadl
Building RACK-Ontology: Writing new resource description MIL-STD-881D-AppxA.sadl
Building RACK-Ontology: Writing new resource description SadlBuiltinFunctions.sadl
Building RACK-Ontology: Writing new resource description DO-178C.sadl
Building RACK-Ontology: Writing new resource description CONFIDENCE.sadl
Building RACK-Ontology: Writing new resource description DOCUMENT.sadl
Building RACK-Ontology: Writing new resource description MIL-STD-881D-AppxD.sadl
Building RACK-Ontology: Writing new resource description SECURITY.sadl
Building RACK-Ontology: Writing new resource description ANALYSIS.sadl
Building RACK-Ontology: Writing new resource description SYSTEM.sadl
Building RACK-Ontology: Writing new resource description DO-330.sadl
Building RACK-Ontology: Writing new resource description TESTING.sadl
Building RACK-Ontology: Writing new resource description FILE.sadl
Building RACK-Ontology: Writing new resource description MIL-STD-881D-AppxC.sadl
Building RACK-Ontology: Writing new resource description GeneratePropInfoCSV.sadl
Building RACK-Ontology: Writing new resource description MIL-STD-881D-AppxB.sadl
Building RACK-Ontology: Writing new resource description AGENTS.sadl
Building RACK-Ontology: Writing new resource description PROV-S.sadl
Building RACK-Ontology: Updating resource PROCESS.sadl
Building RACK-Ontology: Updating resource REVIEW.sadl
Building RACK-Ontology: Updating resource HARDWARE.sadl
Building RACK-Ontology: Updating resource ARP-4754A.sadl
Building RACK-Ontology: Updating resource REQUIREMENTS.sadl
Building RACK-Ontology: Updating resource SadlImplicitModel.sadl
Building RACK-Ontology: Updating resource SOFTWARE.sadl
Building RACK-Ontology: Updating resource MODEL.sadl
Building RACK-Ontology: Updating resource BASELINE.sadl
Building RACK-Ontology: Updating resource HAZARD.sadl
Building RACK-Ontology: Updating resource MIL-STD-881D.sadl
Building RACK-Ontology: Updating resource MIL-STD-881D-AppxA.sadl
Building RACK-Ontology: Updating resource SadlBuiltinFunctions.sadl
Building RACK-Ontology: Updating resource DO-178C.sadl
Building RACK-Ontology: Updating resource CONFIDENCE.sadl
Building RACK-Ontology: Updating resource DOCUMENT.sadl
Building RACK-Ontology: Updating resource MIL-STD-881D-AppxD.sadl
Building RACK-Ontology: Updating resource SECURITY.sadl
Building RACK-Ontology: Updating resource ANALYSIS.sadl
Building RACK-Ontology: Updating resource SYSTEM.sadl
Building RACK-Ontology: Updating resource DO-330.sadl
Building RACK-Ontology: Updating resource TESTING.sadl
Building RACK-Ontology: Updating resource FILE.sadl
Building RACK-Ontology: Updating resource MIL-STD-881D-AppxC.sadl
Building RACK-Ontology: Updating resource GeneratePropInfoCSV.sadl
Building RACK-Ontology: Updating resource MIL-STD-881D-AppxB.sadl
Building RACK-Ontology: Updating resource AGENTS.sadl
Building RACK-Ontology: Updating resource PROV-S.sadl
Building RACK-Ontology: Invoking build participants
Building RACK-Ontology: Refreshing '/RACK-Ontology/OwlModels'.
Building RACK-Ontology: Compile CONFIDENCE.sadl
Building RACK-Ontology: Compile REQUIREMENTS.sadl
Building RACK-Ontology: Compile REVIEW.sadl
Building RACK-Ontology: Compile PROCESS.sadl
Building RACK-Ontology: Deleting '/RACK-Ontology/OwlModels/SadlBaseModel.owl'.
Building RACK-Ontology: Deleting '/RACK-Ontology/OwlModels/SadlListModel.owl'.
Building RACK-Ontology: Compile SECURITY.sadl
Building RACK-Ontology: Compile MIL-STD-881D-AppxD.sadl
Building RACK-Ontology: Compile SadlBuiltinFunctions.sadl
Building RACK-Ontology: Compile SadlImplicitModel.sadl
Building RACK-Ontology: Compile TESTING.sadl
Building RACK-Ontology: Compile MODEL.sadl
Building RACK-Ontology: Compile MIL-STD-881D-AppxB.sadl
Building RACK-Ontology: Compile HARDWARE.sadl
Building RACK-Ontology: Compile BASELINE.sadl
Building RACK-Ontology: Compile FILE.sadl
Building RACK-Ontology: Compile PROV-S.sadl
Building RACK-Ontology: Compile SYSTEM.sadl
Building RACK-Ontology: Compile GeneratePropInfoCSV.sadl
Building RACK-Ontology: Compile SOFTWARE.sadl
Building RACK-Ontology: Compile MIL-STD-881D-AppxC.sadl
Building RACK-Ontology: Compile MIL-STD-881D.sadl
Building RACK-Ontology: Compile ANALYSIS.sadl
Building RACK-Ontology: Compile DO-178C.sadl
Building RACK-Ontology: Compile DOCUMENT.sadl
Building RACK-Ontology: Compile HAZARD.sadl
Building RACK-Ontology: Compile ARP-4754A.sadl
Building RACK-Ontology: Compile DO-330.sadl
Building RACK-Ontology: Compile MIL-STD-881D-AppxA.sadl
Building RACK-Ontology: Compile AGENTS.sadl
14539 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build RACK-Ontology in 1112 ms
Building '/Provenance-Example'
Invoking 'Xtext Project Builder' on '/Provenance-Example'.
Building Provenance-Example: Collecting resources
14542 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build Provenance-Example in 1 ms
Building '/LM-Ontology'
Invoking 'Xtext Project Builder' on '/LM-Ontology'.
Building LM-Ontology: Collecting resources
14543 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build LM-Ontology in 0 ms
Building '/GrammaTech-Ontology'
Invoking 'Xtext Project Builder' on '/GrammaTech-Ontology'.
Building GrammaTech-Ontology: Collecting resources
14543 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build GrammaTech-Ontology in 0 ms
Building '/GE-Ontology'
Invoking 'Xtext Project Builder' on '/GE-Ontology'.
Building GE-Ontology: Collecting resources
14544 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build GE-Ontology in 0 ms
Building '/CounterApplicationUnitTesting'
Invoking 'Xtext Project Builder' on '/CounterApplicationUnitTesting'.
Building CounterApplicationUnitTesting: Collecting resources
14545 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build CounterApplicationUnitTesting in 0 ms
Building '/Boeing-Ontology'
Invoking 'Xtext Project Builder' on '/Boeing-Ontology'.
Building Boeing-Ontology: Collecting resources
14546 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build Boeing-Ontology in 1 ms
Building '/SRI-Ontology'
Invoking 'Xtext Project Builder' on '/SRI-Ontology'.
Building SRI-Ontology: Collecting resources
14546 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build SRI-Ontology in 0 ms
Building '/STR-Ontology'
Invoking 'Xtext Project Builder' on '/STR-Ontology'.
Building STR-Ontology: Collecting resources
14547 [main] INFO org.eclipse.xtext.builder.impl.XtextBuilder  - Build STR-Ontology in 0 ms
Building '/RACK-Ontology'
Building '/Provenance-Example'
Building '/LM-Ontology'
Building '/GrammaTech-Ontology'
Building '/GE-Ontology'
Building '/CounterApplicationUnitTesting'
Building '/Boeing-Ontology'
Building '/SRI-Ontology'
Building '/STR-Ontology'
Build completed successfully
```

When you run SADL-Eclipse, make sure you use absolute locations to
mount a host directory on a container directory and import projects
from the container directory, not the host directory.

Also make sure that your projects' directories have `.project` files
with `<name>` elements having the same names as their project
directories' names, otherwise SADL-Eclipse will complain.  Now you're
ready to run SADL-Eclipse!
