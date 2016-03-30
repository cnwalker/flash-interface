'use strict'

var ROOT_DIR = '/Users/Christopher/Desktop/Flash_Center/FLASH4.3/';
var READ_PATH = ROOT_DIR + 'object/setup_params';
var WRITE_PATH = ROOT_DIR + 'ccsn2d/flash2.par';

const fs = require('fs');
var file_manager = require('./js/file_manager');

$(function(){
    var display_zone = $('.display_zone');
    var config_form = $('#config_form');

    file_manager.collectSetupParams(READ_PATH, function(setupParams){
        //console.log(Object.keys(par_obj.parData));
        var curField;
        Object.keys(setupParams).forEach(function(subject){
            // Display all the broad subject-areas
            config_form.append($('<h3>' + subject + '</h3>'));
            Object.keys(setupParams[subject]).forEach(function(directory){
                // Display all the directories
                config_form.append($('<h4>' + directory + '</h4>'));
                Object.keys(setupParams[subject][directory]).forEach(function(variable){
                    // Display all the variables
                    curField = $('<input> </input>');
                    curField.type = 'text';
                    curField.attr('name', variable);
                    curField.attr('id', subject + directory + variable);
                    curField.val(setupParams[subject][directory][variable].value);
                    config_form.append(variable + ': ');
                    config_form.append(curField);
                    config_form.append($('<br>'));
                });
            });
        });
        config_form.append($("<input type='submit' value='Write parameters'/>"));

        config_form.submit(function(event){
            console.log('Submit was pressed!');
            $("#config_form :input").each(function() {
                console.log('Another one!');
                console.log(this.id);
                //setupParams[this.subject][this.directory][this.name].value = this.value;
            });
            console.log(setupParams);
        });
    });
});
