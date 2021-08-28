<!-- markdownlint-disable line-length -->

# SADL-Eclipse: Image for translating SADL files to OWL files

You can use this dockerized SADL-Eclipse image to translate an
existing SADL project's SADL files to OWL files on the command line
without needing a full Eclipse install.  You can run the most recent
development version of `sadl/sadl-eclipse` with the tag `dev` or the
latest stable version of `sadl/sadl-eclipse` with no tag or the tag
`latest` or else use an explicitly versioned tag to run that specific
stable release.

Here are two ways you can run `sadl/sadl-eclipse` on a SADL project.
Note the project's name is `STEM` in the examples below.

  1. Update an existing SADL project's OWL files in place:

     ```shell
     $ docker run --rm -v /existing/STEM:/STEM -v /existing/STEM:/app/workspace/STEM sadl/sadl-eclipse:dev -force -import=/STEM
     15:17:47 INFO  ExecuteCommand  :: Creating resources in memory
     15:17:47 INFO  ExecuteCommand  :: Resolving interdependencies between resources
     15:17:48 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/Run.sadl
     15:17:49 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/ImplicitModel/SadlBuiltinFunctions.sadl
     15:17:49 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/ImplicitModel/SadlImplicitModel.sadl
     15:17:49 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/BaseModel.sadl
     15:17:49 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/STEMRules.sadl
     15:17:49 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/Queries.sadl
     15:17:49 INFO  SadlPreferenceStoreAccess :: Creating preference store access: Shared SADL preference store access [1062952993].
     15:17:51 INFO  ExecuteCommand  :: SADL Command-Line Interface complete
     ```

  2. Copy an existing SADL project to a new place and update its OWL
     files in the new place instead (note that you must create the new
     place first before running this variation):

     ```shell
     $ mkdir -p /new/STEM
     $ docker run --rm -v /existing/STEM:/STEM -v /new/STEM:/app/workspace/STEM sadl/sadl-eclipse:dev -force -import=/STEM
     15:17:47 INFO  ExecuteCommand  :: Creating resources in memory
     15:17:47 INFO  ExecuteCommand  :: Resolving interdependencies between resources
     15:17:48 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/Run.sadl
     15:17:49 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/ImplicitModel/SadlBuiltinFunctions.sadl
     15:17:49 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/ImplicitModel/SadlImplicitModel.sadl
     15:17:49 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/BaseModel.sadl
     15:17:49 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/STEMRules.sadl
     15:17:49 INFO  ExecuteCommand  :: Building file:/app/workspace/STEM/Queries.sadl
     15:17:49 INFO  SadlPreferenceStoreAccess :: Creating preference store access: Shared SADL preference store access [1062952993].
     15:17:51 INFO  ExecuteCommand  :: SADL Command-Line Interface complete
     ```

When you run either command, make sure you replace all occurrences of
`STEM`, `/existing/STEM`, and `/new/STEM` with your existing/new SADL
project's actual name and absolute location(s).  Don't change
`/app/workspace/` in any way - that location is mandatory.

Also make sure that your SADL project has a .project file in its
directory and the .project's `<name>` element has the same name as the
project directory's name, otherwise Eclipse will complain.  Now you're
ready to run the command!
