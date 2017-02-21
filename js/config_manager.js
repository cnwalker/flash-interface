'use strict';

/* global $ */

var file_manager = require('./js/file_manager');
var utils = require('./js/utils');

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
                var directorySelector;
                var allSubjects;

                $('#searchbox').keyup(function() {
                    utils.searchVariables(setupParams, $(this).val(), 'results_zone');
                });

                // Get parData to determine which params are advanced and which aren't
                file_manager.readParData(paths.READ_PATH, function() {

                    alert('There was an error reading some of the config files.\n' +
                    'Please make sure the files in parameter path and setup path exist');
                    $('#go-to-settings')[0].click();
                },
                function(parObj) {
                    Object.keys(setupParams).forEach(function(subject) {
                        // Display all the broad subject-areas
                        subject_zone.append($('<li><a id=' + subject + '_button '
                        + 'href="#">' + subject.charAt(0).toUpperCase() +
                        subject.slice(1) + '</a></li>'));
                        subject_zone.append('<li class="divider"></li>');
                        $('#' + subject + '_button').click(function() {
                            utils.restrictTo(setupParams, {'subject': subject});
                            advanced_button.click();
                            advanced_button.click();
                        });
                        config_form.append($('<h2 id="subject_' + subject + '">' +
                        subject.charAt(0).toUpperCase() + subject.slice(1) + '</h2>'));

                        directorySelector = $('<select required id=' + subject.toLowerCase() + '_directory_selector> </select>');
                        directorySelector.append($('<option value="" disabled selected>Select directory</option>'));

                        directorySelector.change(function() {
                            utils.restrictTo(setupParams, {
                                'subject': subject,
                                'directory': $(this).val()
                            });
                            advanced_button.click();
                            advanced_button.click();
                            $(this).removeClass('inactive');
                            $('#subject_' + subject).removeClass('inactive');
                        });

                        allSubjects = Object.keys(setupParams[subject]);

                        if (allSubjects.length > 1) {
                            config_form.append(directorySelector);
                        }

                        allSubjects.forEach(function(directory) {
                            if (directory.trim()) {
                                $('#' + subject.toLowerCase() + '_directory_selector').append($('<option> ' + directory + ' </option>'));
                            }

                            // Display all the directories
                            config_form.append($('<h4 id=' + subject + '_' +
                            directory.replace(/\//g, '') + '>' + directory + '</h4>'));
                            Object.keys(setupParams[subject][directory]).forEach(function(variable) {
                                    // Display all the variables
                                    if (parObj.parData[variable]) {
                                      curVal = parObj.parData[variable].value;
                                    } else {
                                      curVal = setupParams[subject][directory][variable].value.trim();
                                    }

                                    if (typeof curVal === 'string' || curVal instanceof String) {
                                        curVal = curVal.trim();
                                        if (curVal.toLowerCase() === 'true') {
                                            trueSelected = ' selected="selected" ';
                                            falseSelected = ' ';
                                        } else {
                                            falseSelected = ' selected="selected" ';
                                            trueSelected = ' ';
                                        }
                                    }

                                    if (curVal === 'TRUE' || curVal === 'FALSE') {
                                        curField = $('<select><option' + trueSelected + 'value="TRUE">.TRUE.</option>' +
                                        ' <option' + falseSelected + 'value="FALSE">.FALSE.</option> </select>');
                                    } else if (curVal === 'true' || curVal === 'false'){
                                        curField = $('<select> <option' + trueSelected + 'value="true">.true.</option>' +
                                        ' <option' + falseSelected + 'value="false">.false.</option> </select>');
                                    } else {
                                        curField = $('<input id=' + (subject + directory + variable).replace(/\//g, '')
                                        + ' type="text"> </input>');
                                    }

                                    curField.attr('name', variable);

                                    // Give each field a unique identifier related to their position in the file tree
                                    curField.attr('id', (subject + directory + variable).replace(/\//g, ''));
                                    curField.attr('subject', subject);
                                    curField.attr('directory', directory);
                                    curField.val(curVal);
                                    curLabel = $('<label id=' + (subject + directory + variable + '_label').replace(/\//g, '') +
                                    '> '  + variable + '<label>');
                                    curLabel.click(function(){
                                        utils.updateDescription(subject, directory, variable, setupParams);
                                    });
                                    curLabel.attr('id', (subject + directory +
                                      variable + '_label').replace(/\//g, ''));

                                    var writeSet = new Set(parObj.writeOrder);
                                    if (!writeSet.has(variable)) {
                                        curField.addClass('advanced_param');
                                        curField.addClass('advanced_inactive');
                                        curField.attr('style', 'display: none;');
                                        curLabel.addClass('advanced_inactive');
                                        curLabel.attr('style', 'display: none;');
                                    }

                                    config_form.append(curLabel);
                                    config_form.append(curField);
                            });
                            utils.checkChildrenAndDisplayDirectory(subject, directory);
                        });
                    });

                    // Add all button and settings section
                    subject_zone.append($('<li><a id="all_button" href="#">All</a></li>'));
                    subject_zone.append($('<li class="divider"></li>'));

                    var settings_section = $('<li><a href="./settings.html">Settings</a></li>');
                    subject_zone.append(settings_section);

                    $('#all_button').click(function() {
                        utils.restrictTo(setupParams, {});
                        advanced_button.click();
                        advanced_button.click();
                    });

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

                                    if (parObj.parData[variable] && parObj.parData[variable] !== curSetupVal) {
                                        parObj.parData[variable] = {'value': curSetupVal};
                                    }
                                });
                            });
                        });

                        file_manager.writeParData(parObj.parData, parObj.writeOrder, paths.WRITE_PATH,  function () {
                            alert('Parameters successfully written to ' + paths.WRITE_PATH);
                        });
                    });

                    // Hide/Show all FLASH parameters button
                    advanced_button = $('<div id="advanced_button" class="button radius expand">Show all FLASH parameters</div>');
                    advanced_button.click(function(event) {
                        var nextState = 'show';

                        Object.keys(setupParams).forEach(function(subject) {
                            Object.keys(setupParams[subject]).forEach(function(directory) {
                                Object.keys(setupParams[subject][directory]).forEach(function(variable) {
                                    cur_element = $('#' + (subject + directory + variable).replace(/\//g, ''));

                                    curLabel = $('#' + (subject + directory + variable + '_label').replace(/\//g, ''));

                                    if (cur_element.hasClass('advanced_param')) {
                                        if (advanced_button.text() === 'Show all FLASH parameters') {
                                            nextState = 'hide';
                                            cur_element.removeClass('advanced_inactive');
                                            if (!cur_element.hasClass('inactive')) {
                                                cur_element.attr('style', '');
                                                curLabel.attr('style', '');
                                            } else {
                                                cur_element.attr('style', 'display: none;');
                                                curLabel.attr('style', 'display: none');
                                            }
                                            curLabel.removeClass('advanced_inactive');
                                        } else {
                                            nextState = 'show';
                                            cur_element.addClass('advanced_inactive');
                                            cur_element.attr('style', 'display: none;');
                                            curLabel.addClass('advanced_inactive');
                                            curLabel.attr('style', 'display: none;');
                                        }
                                    }
                                });
                                utils.checkChildrenAndDisplayDirectory(subject, directory);
                            });
                        });

                        if (nextState === 'hide') {
                            advanced_button.text('Restrict to existing parameters');
                        } else {
                            advanced_button.text('Show all FLASH parameters');
                        }
                    });
                    action_zone.append(advanced_button);
                    action_zone.append(write_button);
                });
            });
        }
    });
});
