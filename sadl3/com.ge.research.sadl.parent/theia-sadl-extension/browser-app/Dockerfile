FROM node:10-alpine
RUN apk add --no-cache make gcc g++ python
WORKDIR /home/theia
ADD package.json ./package.json
ADD yarn.lock ./yarn.lock
ARG GITHUB_TOKEN
RUN yarn && \
    yarn theia build && \
    yarn cache clean

FROM node:10-alpine
RUN apk add --no-cache git openssh bash openjdk8 unzip
WORKDIR /home/theia
COPY --from=0 /home/theia /home/theia
EXPOSE 3000
ENV SHELL /bin/bash
ENTRYPOINT [ "yarn", "theia", "start", "/home/project", "--hostname=0.0.0.0" ]