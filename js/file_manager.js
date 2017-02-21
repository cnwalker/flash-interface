'use strict';

const fs = require('fs');

// Pads a string with delimeters until its length is maxPaddingLength
var addPadding = function(string, delimeter, maxPaddingLength) {
    if (maxPaddingLength - string.length > 0)
    {
        string += Array(maxPaddingLength - string.length).join(delimeter);
    }
    return string;
};

// returns true if the input is an empty string. Used to filter out empty strings with the array filter function
var isNotEmptyString = function(string) {
    if(string === '')
    {
        return false;
    }
    return true;
};

var gatherPathFiles = function(filepath, callback) {
    fs.readFile(filepath, function(err, data) {
        if (err) {
            console.log(err);
        } else {
            var paths = JSON.parse(data),
                missingPaths = true;

            if (paths.WRITE_PATH.trim() && paths.READ_PATH.trim() && paths.SETUP_PATH.trim()) {
                missingPaths = false;
            }

            callback({
                data: paths,
                pathsAreMissing: missingPaths,
            });
        }
    });
};

var writePathFiles = function(filepath, data, callback) {
    fs.writeFile(filepath, JSON.stringify(data, null, 4), function(err) {
        if (!err) {
            callback();
        } else {
            console.log(err);
            alert('Problem writing paths to config file. Consult the log for errors');
        }
    });
};

var checkExists = function(filepath_obj, failureCallback) {
    fs.access(filepath, function(err) {
        if (!err) {
            successCallback();
        } else {
            failureCallback();
        }
    });
}

// Reads in .par file and returns parData dictionary
var readParData = function(filepath, failureCallback, callback) {
    var parData = {};
    var writeOrder = [];

    fs.readFile(filepath, function(err, data) {
        var lineList, currentParameter, valAndComment;

        if (err) {
            console.error(err);
            failureCallback();
        } else {
            data.toString().split('\n').forEach(function(line) {
                // If it's not a comment or a space, it's a parameter and needs to be processed
                if (line[0] !== '#' && line.trim()) {
                     // Split based on = character to differentiate parameter and value
                     lineList = line.split('=');
                     currentParameter = lineList[0];
                     // Value is in index 0, comment (if one exists) is in index[1]
                     valAndComment = lineList[1].split('#');
                     // Create an object so that value and comment can be retrieved easily
                     parData[currentParameter.trim()] = {'value': valAndComment[0]};
                     // Adding the comment to the data entry
                     if (valAndComment[1]) {
                         parData[currentParameter.trim()].comment = '#' + valAndComment[1];
                     }
                     // Add the parameter to the write order
                     writeOrder.push(currentParameter.trim());
                } else {
                    // If it is a comment or an empty space, add it to the writeOrder
                    writeOrder.push(line + '\n');
                }
            });

            callback({
                'parData': parData,
                'writeOrder': writeOrder
            });
        }
    });
};

// Writes a .par file according to the given parData and writeOrder
var writeParData = function(parData, writeOrder, filepath, callback) {
    var body = '';

    writeOrder.forEach(function(line) {
        if (parData[line] && parData[line].value && parData[line].value.trim() !== 'null') {
            console.log(parData[line].value);
            body += addPadding(line, ' ', 35) + ' = ';
            body += parData[line].value + ' ' + (parData[line].comment || '') + '\n';
        } else {
            body += line;
        }
    });

   fs.writeFile(filepath, body, function (err) {
        if (err) {
            return console.error(err);
        }
    });
    callback();
};

var collectSetupParams = function(filepath, callback) {
    // Collects the data in the setup_params file
    var setupParams = {};
    var lineList;
    var curDir, curSubject, curVar;

    fs.readFile(filepath, function(err, data) {
        data.toString().split('\n').forEach(function(line) {
            //Directories don't have preceeding tab marks
            switch(line.search(/\S/)) {
                case 0:
                    // No preceeding whitespace, so line is a directory
                    lineList = line.split('/');
                    if (!setupParams[lineList[0]]) {
                        setupParams[lineList[0]] = {};
                    }
                    curDir = lineList.slice(1, lineList.length).join('/');
                    curSubject = lineList[0];
                    setupParams[curSubject][curDir] = {};
                    break;
                case 4:
                    // One preceeding tab, so line is a variable
                    lineList = line.replace(/\[/g, '').replace(/\]/g, '').trim().split(' ');
                    curVar = lineList[0];
                    setupParams[curSubject][curDir][curVar] = {
                        'type': lineList[1],
                        'default': lineList[lineList.length - 1],
                        'value': lineList[lineList.length - 1],
                        'description': ''
                    };

                    if (lineList.length > 3) {
                        setupParams[curSubject][curDir][curVar].flags = (
                            lineList.slice(2, lineList.length - 1).join(',')
                        );
                    }
                    break;
                case 8:
                    // Two preceeding tabs, so line is a paragraph
                    if (line.indexOf('Valid Values') > -1) {
                        setupParams[curSubject][curDir][curVar].valid_range = line.split(':')[1];
                    } else {
                        // valid_range will be set to undefined by default if not added to object
                        setupParams[curSubject][curDir][curVar].description += ' ' + line.trim();
                    }
                    break;
                default:
                    break;
            }
        });
        callback(setupParams);
    });
};

module.exports = {
    readParData: readParData,
    writeParData: writeParData,
    gatherPathFiles: gatherPathFiles,
    writePathFiles: writePathFiles,
    collectSetupParams: collectSetupParams,
    checkExists: checkExists
};
