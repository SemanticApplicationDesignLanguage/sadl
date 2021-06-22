FROM node:16-alpine
RUN apk add --no-cache make gcc g++ python
WORKDIR /home/theia
ADD package.json ./package.json
ADD yarn.lock ./yarn.lock
ARG GITHUB_TOKEN

RUN npm install --global node-gyp@6.0.1 \
    && npm config set node_gyp "`npm prefix -g`/lib/node_modules/node-gyp/bin/node-gyp.js" \
    && echo "npm config get node_gyp -> `npm config get node_gyp`"

RUN rm -rf ~/.node-gyp

RUN yarn \
    && yarn theia build \
    && yarn cache clean

FROM node:16-alpine
RUN apk add --no-cache git openssh bash openjdk8 unzip
WORKDIR /home/theia
COPY --from=0 /home/theia /home/theia
EXPOSE 3000
ENV SHELL /bin/bash
ENTRYPOINT [ "yarn", "theia", "start", "/home/project", "--hostname=0.0.0.0" ]
