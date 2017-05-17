
# FLASH Interface

## What it does
FLASH Interface is a graphical interface for the FLASH Computational Science codebase!

It currently supports:
- Configuring and editing existing flash.par files
- generating new flash.par files by choosing among the full set of parameters
- Filtering the full set of flash parameters based on categories
- Search for flash parameters, and view full descriptions

Coming soon:
- Running flash simulations directly from the Interface
- Some visualizations

## General Prerequisites (for running or building)
- [Git](https://git-scm.com)
- [npm](http://npmjs.com)
    - You will also need [Node.js](https://nodejs.org/en/) which comes with npm

## To run (for development or testing)
```bash
# Clone this repository
git clone https://github.com/cnwalker/flash_interface
# Go into the repository
cd flash_interface
# Install dependencies and run the app
npm install && npm start
```

## Prerequisites for building binaries:
FLASH Interface can be compiled to binaries for Linux (x86, x86_64, armv7l), Mac OSX, and Windows operating systems. Some prebuilt binaries are available at [flash.uchicago.edu](http://flash.uchicago.edu/site/).
- [electron-packager](https://github.com/electron-userland/electron-packager)

Learn more about Electron and its API in the [documentation](http://electron.atom.io/docs/latest).

If you have any questions, please reach me at cnwalker@flash.uchicago.edu. Thanks!

#### License [CC0 (Public Domain)](LICENSE.md)
