'use strict';

const electron = require('electron');
// Manages reading and writing of .par files and other data
var file_manager = require('./js/file_manager');
// Module to control application life.
const app = electron.app;
// Module to create native browser window.
const BrowserWindow = electron.BrowserWindow;

// Miscellanious Global Variables
var ROOT_DIR = '/Users/Christopher/Desktop/Flash_Center/FLASH4.3/';


// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow;

var getDummyDirectories = function () {
    return ['One', 'Two', 'Three', 'Four'];
};

process.getDummies = getDummyDirectories;

function createWindow () {
  // Create the browser window.
  mainWindow = new BrowserWindow({width: 925, height: 675});

  /* file_manager.readParData(ROOT_DIR + 'ccsn2d/flash.par', function(parResult) {
      file_manager.writeParData(parResult.parData, parResult.writeOrder, ROOT_DIR + 'ccsn2d/flash_test.par', function() {
          console.log('Data written sucessfully!');
      });
  }); */

  // and load the index.html of the app.
  mainWindow.loadURL('file://' + __dirname + '/index.html');

  // Open the DevTools.
  mainWindow.webContents.openDevTools();

  // Emitted when the window is closed.
  mainWindow.on('closed', function() {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null;
  });
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
app.on('ready', createWindow);

// Quit when all windows are closed.
app.on('window-all-closed', function () {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('activate', function () {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) {
    createWindow();
  }
});
