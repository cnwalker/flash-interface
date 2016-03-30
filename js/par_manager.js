'use strict';

var ROOT_DIR = '/Users/Christopher/Desktop/Flash_Center/FLASH4.3/';
var READ_PATH = '/Users/Christopher/Desktop/Flash_Center/FLASH4.3/ccsn2d/flash.par';
var WRITE_PATH = '/Users/Christopher/Desktop/Flash_Center/FLASH4.3/ccsn2d/flash2.par';

const fs = require('fs');
var file_manager = require('./js/file_manager');

$(function(){
    var display_zone = $('.display_zone');
    var par_form = $('#par_form');

    file_manager.readParData(READ_PATH, function(par_obj){
        //console.log(Object.keys(par_obj.parData));
        var curField;
        Object.keys(par_obj.parData).forEach(function(parameter){
            curField = $('<input> </input>');
            /* if par_obj.parData[parameter][0] === '.':
                curField.type = "checkbox" */
            curField.type = "text";
            curField.attr('name', parameter.toString());
            curField.val(par_obj.parData[parameter].value);
            par_form.append(parameter.toString() + ': ');
            par_form.append(curField);
            par_form.append($('<br>'));
        });
        par_form.append($("<input type='submit' value='write to .par file'/>"));

        par_form.submit(function(event){
            console.log('Submit was pressed!');
            $("#par_form :input").each(function() {
                if (this.name.toString() in par_obj.parData)
                {
                    console.log('Found parData');
                    par_obj.parData[this.name.toString().trim()].value = this.value;
                }
            });
            console.log(par_obj.parData);
            file_manager.writeParData(par_obj.parData, par_obj.writeOrder, WRITE_PATH, function(){
                console.log('Success');
            });
        });
    });
});
