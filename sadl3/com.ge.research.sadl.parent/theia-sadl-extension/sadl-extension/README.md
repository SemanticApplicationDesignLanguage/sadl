# sadl-extension

Eclipse [Theia](https://github.com/theia-ide/theia) extension with a bundled language server for [SADL](http://sadl.sourceforge.net) (see [Language Server Protocol](https://github.com/Microsoft/language-server-protocol)).

## Usage

The language server `sadl-language-server` is available under the `bin` folder as a plain language server.

## Requirements

`sadl-extension` requires [Node.js](https://nodejs.org/), [`yarn`](https://yarnpkg.com), [Python 2.x](https://www.python.org/downloads/release/python-2715/), and the C++ Build Tools. More details on the prerequisites are available [here](https://github.com/theia-ide/theia/blob/master/doc/Developing.md#prerequisites).

## Install

    npm i -S sadl-extension

## Build and Run

    git clone https://github.com/crapo/sadlos2.git \
    && cd sadlos2/sadl3/com.ge.research.sadl.parent/ \
    && ./gradlew installDist --refresh-dependencies \
    && cd theia-sadl-extension \
    && yarn \
    && cd browser-app \
    && yarn run start

# Docker - SADL Web

### Pull the most recent `next` version
```
docker pull theiaide/sadl:next
```

### Build without caches
```
docker build --no-cache . -t theiaide/sadl:${VERSION}
```

### Run the most recent `next` locally on UNIX
```
docker run -it -p 3000:3000 -v "$(pwd):/home/project" theiaide/sadl:next
```

### Run the most recent `next` locally on Windows<sup>[1](#foot-note-1)</sup>
 - CMD.EXE:
   ```
   docker run -it -p 3000:3000 -v "%cd%:/home/project" theiaide/sadl:next
   ```

 - PowerShell:
   ```
   docker run -it -p 3000:3000 -v "${PWD}:/home/project" theiaide/sadl:next
   ```

 - Git Bash:
   ```
   winpty docker run -it -p 3000:3000 -v "/$(pwd -W):/home/project" theiaide/sadl:next
   ```

<a name="foot-note-1">1</a>: Running on Windows requires enabled [Hyper-V](https://en.wikipedia.org/wiki/Hyper-V) on the host environment.

If you want to use the `latest` version, just drop the `:next` tag from each command.

### Push to Docker Hub
```
docker login
docker push theiaide/sadl:${VERSION}
```

### Pull the image from Docker Hub
```
docker pull theiaide/sadl:${VERSION}
```

# Release Engineering - Manual Steps

The language server has to be manually updated inside the `bin` folder before publishing to NPM each time when the underlying Java implementation changes.

The following script will create a new version of LS and copies into the `bin` folder of the Theia `sadl-extension`. 
```
rm -rf sadl3/com.ge.research.sadl.parent/com.ge.research.sadl.ide/build/install/ \
&& rm -rf ./sadl3/com.ge.research.sadl.parent/theia-sadl-extension/sadl-extension/bin/ \
&& ./sadl3/com.ge.research.sadl.parent/gradlew -p ./sadl3/com.ge.research.sadl.parent/ build installDist --refresh-dependencies --stacktrace \
&& cp -rf ./sadl3/com.ge.research.sadl.parent/com.ge.research.sadl.ide/build/install/ ./sadl3/com.ge.research.sadl.parent/theia-sadl-extension/sadl-extension/bin/
```

If you want to use the `next` Theia, delete the `yarn.lock` file and call `yarn` from the `theia-sadl-extension`.
Publishing to NPM should be with the `next` tag: `npm publish --tag next`. Hints: `npm login`, `npm whoami`.

```
docker rm $(docker ps -a -q) \
&& docker rmi $(docker images -q) \
&& docker build --no-cache . -t theiaide/sadl:${VERSION} \
&& docker tag theiaide/sadl:${VERSION} theiaide/sadl:latest \
&& docker push theiaide/sadl:latest
```