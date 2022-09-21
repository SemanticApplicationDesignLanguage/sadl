# syntax=docker/dockerfile:1

# Cache metadata in our image to make subsequent builds faster

ARG BUILDKIT_INLINE_CACHE=1

# Base our image on Ubuntu 22.04 with latest Temurin JDK 17 build

FROM eclipse-temurin:17.0.4.1_1-jdk

# Install packages needed to run SADL

ARG DEBIAN_FRONTEND=noninteractive
ENV GraphVizPath=/usr/bin
RUN apt-get update -qq \
 && apt-get install -yqq dumb-init graphviz xorg xvfb \
 && rm -rf /var/lib/apt/lists/* /var/cache/apt/* \
 && adduser --disabled-password --gecos SADL sadl

# Copy SADL into our image

COPY --chown=sadl:sadl target/products/sadl.product/linux/gtk/x86_64/sadl.product/ /app/

# Run SADL as the sadl user, not as root

USER sadl
WORKDIR /app
ENTRYPOINT ["dumb-init", "--", "/app/entrypoint.sh"]
