FROM tomcat:8.0-jre8

RUN apt-get update && apt-get -y install --no-install-recommends \
 vim \
 graphviz \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*