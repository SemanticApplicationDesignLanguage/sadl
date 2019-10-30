#### Install the Eclipse IDE for Java Developers
 - You can install the Eclipse IDE from this [Eclipse download site](https://www.eclipse.org/downloads/).
 - The latest release of the Eclipse IDE for Java Developers will work fine.
 - If you have an older release installed, check the current Xtext version under Eclipse About Dialog > Installation Details > Installed Software tab.
 - Both Xtext Complete SDK and Xtend IDE should have a version 2.11.0.v20170127-0250 or greater.

#### Get the SADL source
 - Check out the source from git.  The AugmentedTypes branch is currently the most recent branch. (TODO: Update this README when the AugmentedTypes branch is merged back into the development branch.)
 - You don't have to build the source before importing it into Eclipse, although you can if you want to check that the build works from the command line.
 - Note that the build will fail with errors when compiling the Xtend sources in `com.ge.research.sadl.ide` if you run Maven clean and install at the same time such as with "mvn clean install".  Please run "mvn install" by itself only after "mvn clean" finishes, and the build will complete successfully.

```bash
$ git clone https://github.com/crapo/sadlos2.git
$ cd sadlos2/sadl3/com.ge.research.parent
$ git checkout AugmentedTypes
$ mvn clean
$ mvn install
```

#### Import the SADL projects into the Eclipse workspace
 - Use the Import > Maven > Existing Maven Projects wizard.  Navigate to the sadlos2/sadl3 directory.
 - Import the `com.ge.research.sadl.parent` project and make sure its children projects are imported as well.
 - You will get 6948 errors (more or less) in Eclipse's Problems view after you import these projects into your Eclipse workspace for the first time.  This is normal.

#### Make the errors go away
 - Select the `com.ge.research.sadl.parent` project and execute Run As > Maven clean.  Then execute Run As > Maven install.
 - You still will get 4867 errors (more or less) in Eclipse's Problems view that won't go away even after refreshing or updating the projects.
 - To make all but one of these errors go away, you must delete the projects from your Eclipse workspace (without deleting their contents on disk) and then reimport these projects into your workspace using the Existing Maven Projects wizard again.
 - The remaining 1 error seems to be a bogus Xtend "must use override" error that you should ignore (in fact, if you run the Quick Fix that changes the def keyword to override, the Maven build will break with an Xtend error saying the override keyword is not allowed there).
 - If you run "mvn clean" and "mvn install" from the command line and refresh the projects in Eclipse, you may get 4 more errors about two projects that don't have an xtend-gen directory, but these 4 errors will go away if you delete and reimport the projects again.  Most of the other errors shouldn't return again, however.
 - In general, reimporting the projects a second time often makes errors go away.
