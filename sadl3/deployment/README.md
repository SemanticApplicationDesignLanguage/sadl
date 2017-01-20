## Preconditions

Java 8:
 - Get and install a Java 8 JDK.

Git SCM on Windows:
 - Use cygwin with additional git packages from here: http://redmine.jamoma.org/projects/1/wiki/Installing_and_setting_up_GIT.
 - Or use the git fir Windows: https://git-for-windows.github.io. Select the`Use Git for the Windows Command Promt` when installing the executable.
 - Verification: `git --version`.

Maven 3.3.9:
 - Download [Maven 3.3.9].
 - Extract the archive into `/your/path/to/the/maven/home`.
 - Set Maven environment variable and add Maven `bin` directory to the system path.
 - Verify the Maven version:
 
```
$ mvn --version
Apache Maven 3.3.9 (bb52d8502b132ec0a5a3f4c09453c07478323dc5; 2015-11-10T17:41:47+01:00)
Maven home: /usr/local/Cellar/maven/3.3.9/libexec
Java version: 1.8.0_102, vendor: Oracle Corporation
Java home: /Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre
Default locale: en_US, platform encoding: UTF-8
OS name: "mac os x", version: "10.11.6", arch: "x86_64", family: "mac"
```

Configuring Maven 3.3.9 in Eclipse IDE:
 - One has to configure Eclipse IDE to use the 3.3.9 version of Maven instead of the embedded one.
 - Open preference page at: `Preferences` > `Maven` > `Installations` and click on `Add...`.
 
 ![Add external Maven](/sadl3/deployment/media/add_external_mvn_01.png?raw=true "Add external Maven")
 
 - Point to the folder where you have extracted the Maven binaries: `/your/path/to/the/maven/home`.
 - Make sure, the installation name is `apache-maven-3.3.9`. Press `Finish` to save changes.
 
  ![Configure external Maven path](/sadl3/deployment/media/add_external_mvn_02.png?raw=true "Configure external Maven path")
 
 - Make the `apache-maven-3.3.9` installation as the default one by selecting the checkbox on the left. Do not forget to press `Apply` before selecting `OK` on the preference page.
 
   ![Apply external Maven configuration changes](/sadl3/deployment/media/add_external_mvn_03.png?raw=true "Apply external Maven configuration changes")


[Maven 3.3.9]: http://ftp-stud.hs-esslingen.de/pub/Mirrors/ftp.apache.org/dist/maven/maven-3/3.3.9/binaries/apache-maven-3.3.9-bin.zip

Install Docker toolbox:
 - Windows or OS X
   * Use binary installer: https://www.docker.com/products/docker-toolbox.
 - Linux
   * Select the specific distribution and follow instructions: https://docs.docker.com/engine/installation/linux/.
 - Verification: `docker --version`.

Installing Node.Js (6+) and npm:
 - Windows:
   * Simple way: get the binaries and install a specific version onto Windows: http://blog.teamtreehouse.com/install-node-js-npm-windows.
   * Supporting multiple Node.Js versions via nvm for Windows: https://github.com/coreybutler/nvm-windows
   * Node.
 - OS X:
   * Install Homebrew: `/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`.
   * Update Homebrew: `brew update`.
   * Install nvm: `brew install nvm`.
   * Install Node.js 6+: `nvm install 6.0.0`.
   * Use Node.js 6+: `nvm use 6.0.0`.
 - Linux
   * Install Node.Js via package manager: https://nodejs.org/en/download/package-manager/.
 - Verification: `node --version && npm --version`.

Installing Python (3+):
 - Windows
   * Installation guide for Windows: https://www.python.org/downloads/release/python-352/.
 - OS X:
   * Install from Homebrew: `brew install python3`.
 - Verification: `python --version`.
 - Linux:
   * Installation guide is available at: https://www.python.org/downloads/release/python-352/.

## All in one script to get the sources and build it locally

It is required to check out `sadlos2` and `sadl-jupyterlab` git repositories to the same folder if one would like to use below script.

```
docker ps \
&& mkdir build_sadl_docker \
&& cd build_sadl_docker/ \
&& git clone https://github.com/crapo/sadlos2.git \
&& cd sadlos2/ \
&& git checkout origin/xtext_web_prototype -b xtext_web_prototype \
&& cd .. \
&& git clone https://github.com/TypeFox/sadl-jupyterlab.git \
&& git clone https://github.com/R-Brain/jupyterlab.git \
&& cd jupyterlab/ \
&& git fetch --all --tags --prune \
&& git checkout tags/sadl_web -b sadl_web \
&& cd ../sadlos2/sadl3/com.ge.research.sadl.parent/ \
&& ./gradlew buildStandaloneTomcat \
&& cp ./io.typefox.lsp.monaco/build/tomcat.tar.gz ../deployment/ \
&& cd ../../../jupyterlab/ \
&& npm install \
&& npm run build:all \
&& cd ../sadl-jupyterlab/sadl-client/ \
&& npm install \
&& npm run update:jupyterlab \
&& npm install \
&& npm run build:all \
&& pip install -e . \
&& jupyter serverextension enable --py sadl_web \
&& python setup.py sdist \
&& cp ./dist/sadl_web-0.0.1.tar.gz ../../sadlos2/sadl3/deployment/sadl_web-0.0.1.tar.gz \
&& cd ../../sadlos2/sadl3/deployment/ \
&& docker build --rm=true -t typefox/websadl . \
&& docker run -p 8080:8080 -p 8888:8888 typefox/websadl
```

## Some useful Docker commands

List the running Docker processes:
```
$ docker ps
CONTAINER ID        IMAGE               COMMAND                  CREATED              STATUS              PORTS                                            NAMES
02676cbeb016        typefox/websadl     "tini -- /bin/sh -c '"   About a minute ago   Up About a minute   0.0.0.0:8080->8080/tcp, 0.0.0.0:8888->8888/tcp   cocky_jones
```

Closing a Docker process:
```
$ docker stop 02676cbeb016
02676cbeb016
```

When opening the web SADL, sometimes based on the Docker image, the host uses other than `localhost`, so `http://localhost:8888/sadl` cannot be used. To get the IP of the docker image one can get the process IDs of the running images and then inspect the IP of a particular image.
```
$ docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' 02676cbeb016
172.17.0.2
```


## Pull and start image

```
docker pull typefox/websadl
docker run -p 8080:8080 -p 8888:8888 typefox/websadl

```
### Open SADL in the browser

Open a browser and go to "http://docker-image-ip:8888/sadl"


## Prepare image payload

In git/sadlos2-master/sadl3/com.ge.research.sadl.parent/io.typefox.lsp.monaco
run

```
cd ~/git/sadlos2-master/sadl3/com.ge.research.sadl.parent/io.typefox.lsp.monaco

gradle buildStandaloneTomcat

```

and copy .build/tomcat.tar.gz to here


In git/sadl-jupyterlab/sadl-client
run

```
cd ~/git/sadl-jupyterlab/sadl-client

npm install
npm run clean
npm run build:all

python setup.py sdist

```
and copy ./dist/sadl_web-0.0.1.tar.gz to here



## Build and Start image:

```
docker build --rm=true -t typefox/websadl .
docker run -p 8080:8080 -p 8888:8888 typefox/websadl
```


## If everything works fine push the image to docker hub (deploy image)

```
docker login --username=<name> 
docker push typefox/websadl
```


