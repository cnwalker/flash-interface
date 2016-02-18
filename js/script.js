'use strict';

var ROOT_DIR = '/Users/Christopher/Desktop/Flash_Center/FLASH4.3/';

const fs = require('fs');

$(function(){
    fs.readdir(ROOT_DIR + '/source', function(err, files) {
        var ul = $('.display_zone ul');
        files.forEach(function(currentFile){
            //Create a paragraph object for every file
            var li = $('<li>' + currentFile + '<li>');
            li.appendTo(ul);
        });
    });
});
