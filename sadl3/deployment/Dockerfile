FROM jupyter/minimal-notebook:97a5071c5775

# Install packages
USER root

# install java8 and other
RUN echo 'deb http://httpredir.debian.org/debian/ jessie-backports main' >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get -y install --no-install-recommends \
  libssl-dev \
  libcurl4-openssl-dev \
  openssh-server \
  vim \
  graphviz \
  openjdk-8-jdk \
  supervisor \
 && update-java-alternatives -s java-1.8.0-openjdk-amd64 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

 # set Java_HOME - required to start a workspace agent.
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64

# prepare sshd
RUN sudo mkdir /var/run/sshd \
  && sudo sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd \
  && sudo echo "$NB_USER ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
  
USER $NB_USER
RUN mkdir -p $HOME/sadl $HOME/work $HOME/supervisor $HOME/logs/supervisor

COPY sadl_web-0.0.1.tar.gz $HOME/sadl/
ADD tomcat.tar.gz $HOME/sadl/sadl-lsp-server/

RUN sudo chown -R $NB_USER:users $HOME

# install jupyter sadl
RUN pip install $HOME/sadl/sadl_web-0.0.1.tar.gz \
  && jupyter serverextension enable --py sadl_web

EXPOSE 8080
WORKDIR $HOME/work

COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

CMD /usr/bin/supervisord -c /etc/supervisor/conf.d/supervisord.conf