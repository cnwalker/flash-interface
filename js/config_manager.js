'use strict'

var ROOT_DIR = '/Users/Christopher/Desktop/Flash_Center/FLASH4.3/';
var READ_PATH = ROOT_DIR + 'object/setup_params';
var SIM_PATH = ROOT_DIR + 'ccsn2d/';

const fs = require('fs');
var file_manager = require('./js/file_manager');

$(function(){
    var subject_zone = $('.subject_zone');
    var display_zone = $('.display_zone');
    var config_form = $('#config_form');

    var restrictToSubject = function (setupParams, referenceSubject) {
        //console.log(referenceSubject);
        Object.keys(setupParams).forEach(function(subject) {
            if (subject !== referenceSubject && referenceSubject != "All") {
                $('#' + 'subject_' + subject).addClass('inactive');
            } else {
                $('#' + 'subject_' + subject).removeClass('inactive');
            }
            Object.keys(setupParams[subject]).forEach(function(directory) {
                if (subject !== referenceSubject && referenceSubject != "All"){
                    $('#' + subject + '_' + directory.replace(/\//g, '')).addClass('inactive');
                } else {
                    $('#' + subject + '_' + directory.replace(/\//g, '')).removeClass('inactive');
                }
                Object.keys(setupParams[subject][directory]).forEach(function(variable) {
                    if (subject !== referenceSubject && referenceSubject !== "All") {
                        $('#' + (subject + directory + variable).replace(/\//g, '')).addClass('inactive');
                        $('#' + (subject + directory + variable + '_label').replace(/\//g, '')).addClass('inactive');
                    } else {
                        $('#' + (subject + directory + variable).replace(/\//g, '')).removeClass('inactive');
                        $('#' + (subject + directory + variable + '_label').replace(/\//g, '')).removeClass('inactive');
                    }
                });
            });
        });
    };

    file_manager.collectSetupParams(READ_PATH, function(setupParams) {
        //console.log(Object.keys(par_obj.parData));
        var curField;
        var curLabel;
        var write_button;
        var advanced_button;
        var cur_element;


        // Get parData to determine which params are advanced and which aren't
        file_manager.readParData(SIM_PATH + 'flash.par', function(parData) {
            subject_zone.append($('<a id="All_button" href="#"><h3>' + 'All' + '</h3></a>'));
            $('#All_button').click(function() {
                restrictToSubject(setupParams, 'All');
            });
            Object.keys(setupParams).forEach(function(subject) {
                // Display all the broad subject-areas
                subject_zone.append($('<a id=' + subject + '_button ' + 'href="#"><h3>' + subject + '</h3></a>'));
                $('#' + subject + '_button').click(function() {
                    restrictToSubject(setupParams, subject);
                });
                config_form.append($('<h2 id="subject_' + subject + '">' + subject + '</h2>'));
                Object.keys(setupParams[subject]).forEach(function(directory) {
                    // Display all the directories
                    config_form.append($('<h4 id=' + subject + '_' + directory.replace(/\//g, '') + '>' + directory + '</h4>'));
                    Object.keys(setupParams[subject][directory]).forEach(function(variable) {
                            // Display all the variables
                            var cur_val = setupParams[subject][directory][variable].value;
                            if (cur_val === "TRUE" || cur_val === "FALSE") {
                                curField = $('<select><option value="TRUE">TRUE</option><option value="FALSE">FALSE</option></select>');
                            } else if (cur_val === "true" || cur_val == "false"){
                                curField = $('<select><option value="TRUE">true</option><option value="FALSE">false</option></select>');
                            } else {
                                curField = $('<input type="text"> </input>');
                            }
                            curField.attr('name', variable);

                            // Give each field a unique identifier related to their position in the file tree
                            curField.attr('id', (subject + directory + variable).replace(/\//g, ''));
                            curField.val(setupParams[subject][directory][variable].value)
                            curLabel = $('<p id=' + (subject + directory + variable + '_label').replace(/\//g, '') + '> '  + variable + ':<p>');
                            curLabel.attr('id', (subject + directory + variable + '_label').replace(/\//g, ''));
                            if (parData.writeOrder.indexOf(variable) > -1) {
                                curField.addClass('advanced_param');
                                curField.addClass('inactive');
                                curLabel.addClass('inactive');
                            }
                            //curLabel.addClass('parameter');
                            //curField.append($('<div class="popup"> ' + setupParams[subject][directory][variable].description + ' </div>'));
                            config_form.append(curLabel);
                            config_form.append(curField);

                    });
                });
            });
            // Write parameters button
            write_button = $("<input type='submit' value='Write parameters'/>");
            write_button.click(function(event){
                // In order to update all of the values in setupParams in O(n), select values using their ids
                Object.keys(setupParams).forEach(function(subject) {
                    Object.keys(setupParams[subject]).forEach(function(directory) {
                        Object.keys(setupParams[subject][directory]).forEach(function(variable) {
                            setupParams[subject][directory][variable] = $('#' + (subject + directory + variable).replace(/\//g, '')).val();
                        });
                    });
                });
            });

            // Hide/Show advanced parameters button
            advanced_button = $("<input type='submit' value='Show advanced parameters'/>");
            advanced_button.click(function(event){
                Object.keys(setupParams).forEach(function(subject) {
                    Object.keys(setupParams[subject]).forEach(function(directory) {
                        Object.keys(setupParams[subject][directory]).forEach(function(variable) {
                            cur_element = $('#' + (subject + directory + variable).replace(/\//g, ''));
                            curLabel = $('#' + (subject + directory + variable + '_label').replace(/\//g, ''));
                            if (cur_element.hasClass('advanced_param')) {
                                if (advanced_button.val() === "Show advanced parameters"){
                                    cur_element.removeClass('advanced_inactive');
                                    curLabel.removeClass('advanced_inactive');
                                    advanced_button.val('Hide advanced parameters');
                                } else {
                                    advanced_button.val('Show advanced parameters');
                                    cur_element.addClass('advanced_inactive');
                                    curLabel.addClass('advanced_inactive');
                                }
                            }
                        });
                    });
                });
            });

            subject_zone.append(advanced_button);
            subject_zone.append(write_button);

        });
    });
});
