<!-- markdownlint-disable line-length -->
<!-- markdownlint-disable no-inline-html -->

# sadl-extension

Eclipse [Theia](https://github.com/theia-ide/theia) extension with a bundled language server for [SADL](http://sadl.sourceforge.net) (see [Language Server Protocol](https://github.com/Microsoft/language-server-protocol)).

## Usage

The language server `sadl-language-server` is available under the `bin` folder as a plain language server.

## Requirements

`sadl-extension` requires [Node.js](https://nodejs.org/), [`yarn`](https://yarnpkg.com), [Python 2.x](https://www.python.org/downloads/release/python-2715/), and the C++ Build Tools. More details on the prerequisites are available [here](https://github.com/theia-ide/theia/blob/master/doc/Developing.md#prerequisites).

## Install

    npm i -S sadl-extension

## Build and Run

    git clone https://github.com/SemanticApplicationDesignLanguage/sadl.git \
    && cd sadl/sadl3/com.ge.research.sadl.parent/ \
    && ./gradlew installDist --refresh-dependencies \
    && cd theia-sadl-extension \
    && yarn \
    && cd browser-app \
    && yarn run start

## Docker - SADL Web

Pull the most recent `next` version

    docker pull theiaide/sadl:next

Build without caches

    docker build --no-cache . -t theiaide/sadl:next

Run the most recent `next` locally on UNIX

    docker run -it -p 3000:3000 -v "$(pwd):/home/project" theiaide/sadl:next

Run the most recent `next` locally on Windows<sup>[1](#foot-note-1)</sup>

- CMD.EXE:

    docker run -it -p 3000:3000 -v "%cd%:/home/project" theiaide/sadl:next

- PowerShell:

    docker run -it -p 3000:3000 -v "${PWD}:/home/project" theiaide/sadl:next

- Git Bash:

    winpty docker run -it -p 3000:3000 -v "/$(pwd -W):/home/project" theiaide/sadl:next

<a name="foot-note-1">1</a>: Running on Windows requires enabled [Hyper-V](https://en.wikipedia.org/wiki/Hyper-V) on the host environment.

Push to Docker Hub

    docker login
    docker push theiaide/sadl:next

Pull the image from Docker Hub

    docker pull theiaide/sadl:next
