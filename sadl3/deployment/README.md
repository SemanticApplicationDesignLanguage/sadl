## Preconditions

Git SCM on Windows:
 - Use cygwin with additional git packages from here: http://redmine.jamoma.org/projects/1/wiki/Installing_and_setting_up_GIT.
 - Or alternatively use the git fir Windows: https://git-for-windows.github.io. Select `Use Git for the Windows Command Promt`.
 - Verification: `git --version`.

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
 -Linux
   * Install Node.Js via package manager: https://nodejs.org/en/download/package-manager/.
 - Verification: `node --version && npm --version`.

Installing python (3+):
 - Windows
   * Installation guide for Windows: https://www.python.org/downloads/release/python-352/.
 - OS X:
   * Install from Homebrew: `brew install python3`.
 - Verification: `python --version`.
 _ Linux:
   * INstallation guide is available at: https://www.python.org/downloads/release/python-352/.

## All in one script to get the sources and build it locally

```
mkdir build_sadl_docker \
&& cd build_sadl_docker/ \
&& git clone https://github.com/crapo/sadlos2.git \
&& cd sadlos2/ \
&& git checkout origin/xtext_web_prototype -b xtext_web_prototype \
&& cd .. \
&& git clone https://github.com/TypeFox/sadl-jupyterlab.git \
&& git clone https://github.com/eclipse/xtext-core.git \
&& git clone https://github.com/R-Brain/jupyterlab.git \
&& cd jupyterlab/ \
&& git fetch --all --tags --prune \
&& git checkout tags/sadl_web -b sadl_web \
&& cd ../xtext-core/ \
&& ./gradlew install \
&& cd ../sadlos2/sadl3/com.ge.research.sadl.parent/ \
&& ./gradlew buildStandaloneTomcat -PuseLocalMaven \
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


