## Pull and start image

```
docker pull typefox/websadl
docker run -p 8080:8080 -p 8888:8888 typefox/websadl

```
### Open SADL in the browser

Open a browser and go to "http://docker-image-ip:8080/sadl"


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

### sh to container 
`sudo docker exec -i -t <name> /bin/bash`


