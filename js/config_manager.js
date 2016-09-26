'use strict';

/* global $ */

var restrictToSubject = function (setupParams, referenceSubject) {
    Object.keys(setupParams).forEach(function(subject) {
        if (subject !== referenceSubject && referenceSubject !== 'All') {
            $('#' + 'subject_' + subject).addClass('inactive');
        } else {
            $('#' + 'subject_' + subject).removeClass('inactive');
        }
        Object.keys(setupParams[subject]).forEach(function(directory) {
            if (subject !== referenceSubject && referenceSubject !== 'All'){
                $('#' + subject + '_' + directory.replace(/\//g, '')).addClass('inactive');
            } else {
                $('#' + subject + '_' + directory.replace(/\//g, '')).removeClass('inactive');
            }
            Object.keys(setupParams[subject][directory]).forEach(function(variable) {
                if (subject !== referenceSubject && referenceSubject !== 'All') {
                    $('#' + (subject + directory + variable).replace(/\//g, '')).addClass('inactive');
                    $('#' + (subject + directory + variable).replace(/\//g, '')).attr('style', 'display: none;');
                    $('#' + (subject + directory + variable + '_label').replace(/\//g, '')).addClass('inactive');
                } else {
                    $('#' + (subject + directory + variable).replace(/\//g, '')).removeClass('inactive');
                    $('#' + (subject + directory + variable + '_label').replace(/\//g, '')).removeClass('inactive');
                    $('#' + (subject + directory + variable).replace(/\//g, '')).attr('style', '');
                }
            });
        });
    });
};

var updateDescription = function(subject, directory, variable, setupParams) {
    var active_variable_name = $('#active_variable_name');
    var active_variable_description = $('#active_variable_description');
    var error_msg = 'No description avaliable';

    active_variable_name.text(variable);
    active_variable_description.text(setupParams[subject][directory][variable].description || error_msg);
};

var file_manager = require('./js/file_manager');

$(function() {
    var subject_zone = $('#subject_zone');
    var display_zone = $('.display_zone');
    var config_form = $('#config_form');
    var action_zone = $('#action_zone');

    file_manager.gatherPathFiles(__dirname + '/config.json', function(result) {
        if (result.pathsAreMissing) {
            alert('Some config paths are missing. You must pick which files to read and write from.');
            $('#go-to-settings')[0].click();
        } else {
            var paths = result.data;

            file_manager.collectSetupParams(paths.SETUP_PATH, function(setupParams) {
                var curField;
                var curLabel;
                var write_button;
                var advanced_button;
                var cur_element;
                var curRow;
                var curVal;
                var trueSelected;
                var falseSelected;

                // Get parData to determine which params are advanced and which aren't
                file_manager.readParData(paths.READ_PATH, function(parObj) {
                    subject_zone.append($('<li><a id="all_button" href="#">All</a></li>'));
                    subject_zone.append('<li class="divider"></li>');

                    $('#all_button').click(function() {
                        restrictToSubject(setupParams, 'All');
                        advanced_button.click();
                        advanced_button.click();
                    });

                    Object.keys(setupParams).forEach(function(subject) {
                        // Display all the broad subject-areas
                        subject_zone.append($('<li><a id=' + subject + '_button ' + 'href="#">' + subject.charAt(0).toUpperCase() + subject.slice(1) + '</a></li>'));
                        subject_zone.append('<li class="divider"></li>');
                        $('#' + subject + '_button').click(function() {
                            restrictToSubject(setupParams, subject);
                            advanced_button.click();
                            advanced_button.click();
                        });
                        config_form.append($('<h2 id="subject_' + subject + '">' + subject.charAt(0).toUpperCase() + subject.slice(1) + '</h2>'));
                        Object.keys(setupParams[subject]).forEach(function(directory) {
                            // Display all the directories
                            config_form.append($('<h4 id=' + subject + '_' + directory.replace(/\//g, '') + '>' + directory + '</h4>'));
                            Object.keys(setupParams[subject][directory]).forEach(function(variable) {
                                    // Display all the variables
                                    trueSelected = ' ';
                                    falseSelected = ' ';
                                    curVal = setupParams[subject][directory][variable].value;

                                    if (curVal.toLowerCase().trim() === 'true') {
                                        trueSelected = ' selected="selected" ';
                                    } else {
                                        falseSelected = ' selected="selected" ';
                                    }

                                    if (curVal === 'TRUE' || curVal === 'FALSE') {
                                        curField = $('<select><option' + trueSelected + 'value="TRUE">TRUE</option>' +
                                        ' <option' + falseSelected + 'value="FALSE">FALSE</option> </select>');
                                    } else if (curVal === 'true' || curVal === 'false'){
                                        curField = $('<select> <option' + trueSelected + 'value="true">true</option>' +
                                        ' <option' + falseSelected + 'value="false">false</option> </select>');
                                    } else {
                                        curField = $('<input id=' + (subject + directory + variable).replace(/\//g, '') + ' type="text"> </input>');
                                    }
                                    curField.attr('name', variable);

                                    // Give each field a unique identifier related to their position in the file tree
                                    curField.attr('id', (subject + directory + variable).replace(/\//g, ''));
                                    curField.val(setupParams[subject][directory][variable].value);
                                    curLabel = $('<label id=' + (subject + directory + variable + '_label').replace(/\//g, '') + '> '  + variable + '<label>');
                                    curLabel.click(function(){
                                        updateDescription(subject, directory, variable, setupParams);
                                    });
                                    curLabel.attr('id', (subject + directory + variable + '_label').replace(/\//g, ''));
                                    if (parObj.writeOrder.indexOf(variable) > -1) {
                                        curField.addClass('advanced_param');
                                        curField.addClass('advanced_inactive');
                                        curField.attr('style', 'display: none;');
                                        curLabel.addClass('advanced_inactive');
                                    }

                                    config_form.append(curLabel);
                                    config_form.append(curField);

                            });
                        });
                    });

                    // Add settings section
                    var settings_section = $('<li><a href="./settings.html">Settings</a></li>');
                    subject_zone.append(settings_section);

                    // Write parameters button
                    write_button = $('<div class="button radius expand">Write parameters</div>');
                    write_button.click(function(event) {
                        var curSetupVal;
                        var checkVal;

                        if (parObj.writeOrder.indexOf('# Advanced Parameters\n') === -1) {
                            parObj.writeOrder.push('# Advanced Parameters\n');
                        }

                        // In order to update all of the values in setupParams in O(n), select values using their ids
                        Object.keys(setupParams).forEach(function(subject) {
                            Object.keys(setupParams[subject]).forEach(function(directory) {
                                Object.keys(setupParams[subject][directory]).forEach(function(variable) {
                                    curSetupVal = $('#' + (subject + directory + variable).replace(/\//g, '')).val();
                                    checkVal = setupParams[subject][directory][variable].value.toString();
                                    if (checkVal !== curSetupVal) {
                                        if (parObj.writeOrder.indexOf(variable) === -1) {
                                            parObj.writeOrder.push(variable);
                                        }
                                        parObj.parData[variable] = {'value': curSetupVal};
                                    }
                                });
                            });
                        });

                        file_manager.writeParData(parObj.parData, parObj.writeOrder, paths.WRITE_PATH,  function () {
                            alert('Parameters successfully written to ' + paths.WRITE_PATH);
                        });
                    });

                    // Hide/Show advanced parameters button
                    advanced_button = $('<div class="button radius expand">Show advanced parameters</div>');
                    advanced_button.click(function(event) {
                        var nextState = 'show';

                        Object.keys(setupParams).forEach(function(subject) {
                            Object.keys(setupParams[subject]).forEach(function(directory) {
                                Object.keys(setupParams[subject][directory]).forEach(function(variable) {
                                    cur_element = $('#' + (subject + directory + variable).replace(/\//g, ''));

                                    curLabel = $('#' + (subject + directory + variable + '_label').replace(/\//g, ''));

                                    if (cur_element.hasClass('advanced_param')) {
                                        if (advanced_button.text() === 'Show advanced parameters') {
                                            nextState = 'hide';
                                            cur_element.removeClass('advanced_inactive');
                                            if (!cur_element.hasClass('inactive')) {
                                                cur_element.attr('style', '');
                                            } else {
                                                cur_element.attr('style', 'display: none;');
                                            }
                                            curLabel.removeClass('advanced_inactive');
                                        } else {
                                            nextState = 'show';
                                            cur_element.addClass('advanced_inactive');
                                            cur_element.attr('style', 'display: none;');
                                            curLabel.addClass('advanced_inactive');
                                        }
                                    }
                                });
                            });
                        });

                        if (nextState === 'hide') {
                            advanced_button.text('Hide advanced parameters');
                        } else {
                            advanced_button.text('Show advanced parameters');
                        }
                    });
                    action_zone.append(advanced_button);
                    action_zone.append(write_button);
                });
            });
        }
    });
});
