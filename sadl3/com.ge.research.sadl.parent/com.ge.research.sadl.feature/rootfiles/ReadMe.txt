SADL Command-Line Interface

Use the following command to execute the SADL Command-Line Interface:

SADLCli.sh [Command Flags, mandatory]

- Configure SADL preferences              -config     {/path/to/file}
- Import projects                         -import     {[uri:/]/path/to/project}
- Import all projects in the tree         -importAll  {[uri:/]/path/to/projectTreeURI}
- Build projects / the workspace          -build      {project_name_reg_ex | all}
- Clean & build projects / the workspace  -cleanBuild {project_name_reg_ex | all}

Notes:
Flags can be repeated.
The -config flag is optional. If not supplied the configuration file will default to "SadlConfiguration.xml".

Configuration File:
SADLCli will use SadlConfiguration.xml to set preferences for SADL.
This file can be placed in the directory Eclipse launches from.
These preference values mirror the preferences in the Eclipse GUI for SADL.
