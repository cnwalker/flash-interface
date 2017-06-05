
# FLASH Interface

## What it does
FLASH Interface is a graphical interface for the FLASH Computational Science codebase!

It currently supports:
- configuring and editing existing flash.par files
- generating new flash.par files by choosing among the full set of parameters
- filtering the full set of flash parameters based on categories
- searching for flash parameters, and view full descriptions

Coming soon:
- Running flash simulations directly from the Interface
- Some visualizations

## General Prerequisites (for running or building)
- [git](https://git-scm.com)
    - to clone the repo
- [npm](http://npmjs.com)
    - You will also need [Node.js](https://nodejs.org/en/) which comes with npm


## To build
FLASH Interface can be compiled to binaries for Linux (x86, x86_64, armv7l), Mac OSX, and Windows operating systems.

To build you will need a tool called electron-packager, instructions for building are [available here](https://github.com/electron-userland/electron-packager)

Some prebuilt binaries are available at [flash.uchicago.edu](http://flash.uchicago.edu/site/).

## To run (for development or testing)
```bash
# Clone this repository
git clone https://github.com/cnwalker/flash_interface
# Go into the repository
cd flash_interface
# Install dependencies and run the app
npm install && npm start
```

## Contributing
- FLASH Interface is open source and accepting pull requests!
- The interface is built on [electron](https://electron.atom.io/), a javascript framework for making desktop applications.

Learn more about Electron and its API in the [documentation](http://electron.atom.io/docs/latest).

If you have any questions, please reach me at cnwalker@flash.uchicago.edu. Thanks!

#### License [CC0 (Public Domain)](LICENSE.md)
