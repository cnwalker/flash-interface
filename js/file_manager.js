'use strict';

const fs = require('fs');

// Pads a string with delimeters until it's length is maxPaddingLength
var addPadding = function(string, delimeter, maxPaddingLength) {
    if (maxPaddingLength - string.length > 0)
    {
        return string + Array(maxPaddingLength - string.length).join(delimeter);
    }
    return string;
};

// Reads in .par file and returns parData dictionary
var readParData = function(filepath, callback) {
    var parData = {};
    var writeOrder = [];

    fs.readFile(filepath, function (err, data) {
        if (err) {
            return console.error(err);
        }

        var lineList, currentParameter, valAndComment;
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
    });
};

// Writes a .par file according to the given parData and writeOrder
var writeParData = function(parData, writeOrder, filepath, callback) {
    var body = '';

    writeOrder.forEach(function(line) {
        if (parData[line]) {
            body += addPadding(line, ' ', 35) + '=';
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


module.exports = {
    readParData: readParData,
    writeParData: writeParData
};
