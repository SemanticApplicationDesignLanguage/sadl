SADL Command-Line Interface

Use the following command to execute the SADL Command-Line Interface:

ExecuteCommand.sh [Force Overwrite Project flag, optional] [Import Directory Flag, mandatory] [SADL CLI Configuration flag, optional] [Command Flags, optional]

Force Overwrite Project Flag, Optional:
-force

Import Directory Flag, Must be used:
-import=<Path to Project Root Directory>

SADL CLI Configuration Flag, Optional:
-config=<Path to SADL Configuration XML>

Command Flags, Optional:
-infer=<Name of SADL file to run inference on>

Notes:
Ordering of flags does not matter.
The -force flag is given when the user wants to import a project into the workspace and overwrite a preexisting one with the same name.
The tool will not execute if no -import flag with a valid project directory is specified.
The -config argument is optional. If not supplied it will default to "SadlConfiguration.xml".

Configuration File:
SADL CLI will use SadlConfiguration.xml to set preferences for SADL.
This file can be placed in the directory Eclipse launches from.
These preference values mirror the preferences in the Eclipse GUI for SADL.
