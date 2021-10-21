<!-- markdownlint-disable line-length -->

# SADL-Eclipse: Image for translating SADL files to OWL files

You can use this dockerized SADL-Eclipse image to translate an
existing SADL project's SADL files to OWL files on the command line
without needing a full Eclipse install.  You can run the most recent
development version of `sadl/sadl-eclipse` with the tag `dev` or the
latest stable version of `sadl/sadl-eclipse` with no tag or the tag
`latest` or else use an explicitly versioned tag to run that specific
stable release.

SADL-Eclipse accepts the following command line options:

  - Import projects                         -import     {/path/to/project}
  - Import all projects in a tree           -importAll  {/path/to/projectTree}
  - Build projects / the workspace          -build      {project_name_reg_ex | all}
  - Clean build projects / the workspace    -cleanBuild {project_name_reg_ex | all}

SADL-Eclipse has its own internal workspace.  You need to import a
project into SADL-Eclipse's internal workspace before you can build a
project.  A project is imported in place, that is, the import does not
copy any project files to the internal workspace and a build will
clean or build files inside the project itself (although only within
the project's `ImplicitModel` and `OwlModels` subdirectories).

The difference between a build and a clean build is that a build will
only update files in the `ImplicitModel` and `OwlModels`
subdirectories, while a clean build will delete every file in the
`ImplicitModel` and `OwlModels` subdirectories before building
completely new files.  The only files safe from deletion are files
with the file extension ".rdf".

Here is an example run of `sadl/sadl-eclipse` which imports and clean
builds a single SADL project called `STEM`:

```shell
$ docker run --rm -v /path/to/STEM:/STEM sadl/sadl-eclipse:dev -import /STEM -cleanBuild all
Refreshing workspace.
Create.
Opening 'STEM'.
Opening 'STEM'.
Refreshing '/STEM'.
Cleaning all projects...
Cleaning STEM
Cleaning /STEM/ImplicitModel/SadlBuiltinFunctions.sadl
Cleaning /STEM/ImplicitModel/SadlImplicitModel.sadl
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
log4j:WARN No appenders could be found for logger (org.eclipse.xtext.parser.antlr.AbstractInternalAntlrParser).
log4j:WARN Please initialize the log4j system properly.
Building STEM
Building file:/STEM/Run.sadl
Building file:/STEM/ImplicitModel/SadlBuiltinFunctions.sadl
Building file:/STEM/ImplicitModel/SadlImplicitModel.sadl
Building file:/STEM/BaseModel.sadl
Building file:/STEM/STEMRules.sadl
Building file:/STEM/Queries.sadl
Build completed successfully
```

Here is an example run of `sadl/sadl-eclipse` which imports a tree of
SADL projects within a directory called `RACK` and then builds the
SADL projects in a specified order taking into account a SADL
project's dependency on another SADL project that needs to be built
first:

```shell
$ docker run --rm -v /path/to/RACK:/RACK sadl/sadl-eclipse:dev -importAll /RACK -cleanBuild RACK-Ontology -cleanBuild GE-Ontology
Refreshing workspace.
Create.
Opening 'RACK-Ontology'.
Opening 'RACK-Ontology'.
Refreshing '/RACK-Ontology'.
Create.
Opening 'GE-Ontology'.
Opening 'GE-Ontology'.
Refreshing '/GE-Ontology'.
Cleaning RACK-Ontology
Cleaning /RACK/RACK-Ontology/ImplicitModel/SadlBuiltinFunctions.sadl
Cleaning /RACK/RACK-Ontology/ImplicitModel/SadlImplicitModel.sadl
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
Cleaning /RACK/RACK-Ontology/OwlModels/SOFTWARE.owl
Cleaning /RACK/RACK-Ontology/OwlModels/SadlBaseModel.owl
Cleaning /RACK/RACK-Ontology/OwlModels/GeneratePropInfoCSV.owl
Cleaning /RACK/RACK-Ontology/OwlModels/ANALYSIS.owl
Cleaning /RACK/RACK-Ontology/OwlModels/TESTING.owl
Cleaning /RACK/RACK-Ontology/OwlModels/MIL-STD-881D-AppxC.owl
Cleaning GE-Ontology
Cleaning /RACK/GE-Ontology/ImplicitModel/SadlBuiltinFunctions.sadl
Cleaning /RACK/GE-Ontology/ImplicitModel/SadlImplicitModel.sadl
Cleaning /RACK/GE-Ontology/OwlModels/SadlBuiltinFunctions.owl
Cleaning /RACK/GE-Ontology/OwlModels/SadlImplicitModel.owl
Cleaning /RACK/GE-Ontology/OwlModels/GE.owl
Cleaning /RACK/GE-Ontology/OwlModels/SadlListModel.owl
Cleaning /RACK/GE-Ontology/OwlModels/SadlBaseModel.owl
Building RACK-Ontology
Building file:/RACK/RACK-Ontology/ontology/ANALYSIS.sadl
Building file:/RACK/RACK-Ontology/ontology/PROV-S.sadl
Building file:/RACK/RACK-Ontology/ontology/REQUIREMENTS.sadl
Building file:/RACK/RACK-Ontology/ontology/BASELINE.sadl
Building file:/RACK/RACK-Ontology/ontology/SOFTWARE.sadl
Building file:/RACK/RACK-Ontology/ontology/ARP-4754A/ARP-4754A.sadl
Building file:/RACK/RACK-Ontology/ontology/MODEL.sadl
Building file:/RACK/RACK-Ontology/ontology/MIL-STD-881D/MIL-STD-881D-AppxB.sadl
Building file:/RACK/RACK-Ontology/ontology/MIL-STD-881D/MIL-STD-881D-AppxA.sadl
Building file:/RACK/RACK-Ontology/ontology/MIL-STD-881D/MIL-STD-881D.sadl
Building file:/RACK/RACK-Ontology/ontology/MIL-STD-881D/MIL-STD-881D-AppxD.sadl
Building file:/RACK/RACK-Ontology/ontology/MIL-STD-881D/MIL-STD-881D-AppxC.sadl
Building file:/RACK/RACK-Ontology/ontology/DO-330/DO-330.sadl
Building file:/RACK/RACK-Ontology/ontology/DOCUMENT.sadl
Building file:/RACK/RACK-Ontology/ontology/CONFIDENCE.sadl
Building file:/RACK/RACK-Ontology/ontology/DO-178C/DO-178C.sadl
Building file:/RACK/RACK-Ontology/ontology/SYSTEM.sadl
Building file:/RACK/RACK-Ontology/ontology/AGENTS.sadl
Building file:/RACK/RACK-Ontology/ontology/REVIEW.sadl
Building file:/RACK/RACK-Ontology/ontology/FILE.sadl
Building file:/RACK/RACK-Ontology/ontology/TESTING.sadl
Building file:/RACK/RACK-Ontology/ontology/HAZARD.sadl
Building file:/RACK/RACK-Ontology/ontology/PROCESS.sadl
Building file:/RACK/RACK-Ontology/ontology/GeneratePropInfoCSV.sadl
Building file:/RACK/RACK-Ontology/ImplicitModel/SadlBuiltinFunctions.sadl
Building file:/RACK/RACK-Ontology/ImplicitModel/SadlImplicitModel.sadl
Building GE-Ontology
Building file:/RACK/GE-Ontology/ontology/GE.sadl
Building file:/RACK/GE-Ontology/ImplicitModel/SadlBuiltinFunctions.sadl
Building file:/RACK/GE-Ontology/ImplicitModel/SadlImplicitModel.sadl
Build completed successfully
```

When you run `sadl/sadl-eclipse`, make sure you bind mount your
existing project's or tree of projects' absolute location to a
location in the Docker container's filesystem (e.g., `/STEM`) and
import that Docker filesystem's location, not the host filesystem's
location.

Also make sure that your SADL project has a .project file in its
directory and the .project's `<name>` element has the same name as the
project directory's name, otherwise SADL-Eclipse will complain.  Now
you're ready to run SADL-Eclipse!
