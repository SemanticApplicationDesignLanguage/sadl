<!-- markdownlint-disable line-length -->

# SADL-Eclipse: Image for translating SADL files to OWL files

You can use this dockerized SADL-Eclipse image to translate existing
SADL projects' SADL files to OWL files on the command line.  You can
run the most recent development version of `sadl/sadl-eclipse` with
the tag `dev` or a stable release of `sadl/sadl-eclipse` with an
explicitly versioned tag, the tag `latest`, or no tag at all.

SADL-Eclipse accepts the following arguments at the end of its `docker
run` command line:

- -import /path/to/project // _Import a project_
- -importAll /path/to/projectTree // _Import all projects in a tree_
- -cleanBuild // _Clean and build all imported projects_
- -build // _Build all imported projects_

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

This example uses `sadl/sadl-eclipse` to clean and build a single
project called `STEM`:

```shell
docker run --rm -v "/path/to/STEM:/STEM" sadl/sadl-eclipse:dev -import /STEM -cleanBuild
```

This example uses `sadl/sadl-eclipse` to clean and build all projects
inside a `RACK` directory while changing which UID and GID the
container runs as from `"1000 1000"` to something else.

```shell
docker run --rm -u 0 -e RUN_AS="$(id -u) $(id -g)" -v "$(pwd)/RACK:/RACK" sadl/sadl-eclipse:dev -importAll /RACK -cleanBuild
```

When you run SADL-Eclipse, make sure you use absolute locations to
mount a host directory on a container directory and import projects
from the container directory, not the host directory.

Also make sure that your projects' directories have `.project` files
with `<name>` elements having the same names as their project
directories' names, otherwise SADL-Eclipse will complain.  Now you're
ready to run SADL-Eclipse!
