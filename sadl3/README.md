#### Verify Xtext tooling features in Eclipse IDE
 - Check the current Xtext version under Eclipse About Dialog > Installation Details > Installed Software tab.
 - Both Xtext Complete SDK and Xtend IDE should have a version 2.11.0.v20170127-0250.
 - If you have an older version installed in Eclipse, install the Xtext 2.11 from [Xtext p2 update site]. 
 - The updates can be installed via Help > Install New Software...
 - When prompted, restart the Eclipse IDE.

[Xtext p2 update site]: http://download.eclipse.org/modeling/tmf/xtext/updates/milestones/head/S201701270250/

#### Verify Buildship tooling features in Eclipse IDE
 - The current version can be verified under the Eclipse About Dialog > Installation Details > Installed Software tab.
 - The Buildship: Eclipse Plug-ins for Gradle feature should have a version 2.0.0.v20170111-0958-m or older.
 - If the installed version is older, install the latest one from the [Buildship p2 update site].
 - The updates can be installed from Help > Install New Software...
 - When prompted, restart the application. 

[Buildship p2 update site]: http://download.eclipse.org/buildship/updates/e45/milestones/2.x/

#### Get the source
 - Check out the source from git.
 
 ```
 git clone https://github.com/crapo/sadlos2.git \
&& cd sadlos2 \
&& git checkout origin/master
 ```
 
#### Import the projects into the Eclipse workspace
 - Use the Import > Maven > Existing Maven Projects wizard.
 - Select the `com.ge.research.sadl.parent` project and import its children projects as well.
 
#### Configure the target platform
 - Go to Preferences... > Plug-in Development > Target Platform.
 - Select `com.ge.research.sadl.target` and click on the Edit... button.
 - This will make sure all the required dependencies are available under your Eclipse IDE.
 - Once the dependencies loaded, close the dialog and then the Preferences dialog by hitting Apply then OK.
 
#### Update Maven project dependencies
 - Select the `com.ge.research.sadl.parent` from the navigator and through the context menu select Maven > Update Project...
 - Make sure Force Update of Snapshot/Releases is checked.
 - Hit OK.
 
#### Generate the Xtext resources from the MWE2 workflow.
 - Select the workflow file from the `com.ge.research.sadl` project.
 - Right click on the `GenerateSADL.mwe2` file in the navigator and Run As > MWE2 Workflow.
 - Wait until the automatic workspace build after the generation.
 - If Eclipse complains, that the project has errors, just proceed. It has errors because the Xtex resources are not generated yet.

#### Import the Web SADL Gradle projects into the workspace
 - Import > Existing Projects into Workspace.
 - Make sure Search for nested projects is checked in the wizard.
 - Select the `com.ge.research.sadl.parent` project folder as the root directory.
 - Eclipse complains that some projects cannot be imported because they already exist in the workspace.
 - That is true. So click on Deselect All and manually select `io.typefox.lsp.endpoint` and `io.typefox.lsp.monaco` projects.
 - Click OK.
 - After the import, the Gradle project dependencies will be updated automatically.
 - If you still have errors, go to the `com.ge.research.sadl.parent` project and refresh the Gradle dependencies via the Gradle > Refresh Gradle Project context menu item.
