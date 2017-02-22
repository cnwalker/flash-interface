'use strict'

/* global $ */

var matchesAttributes = function(attributes, referenceAttributes) {
    var attr_keys = Object.keys(referenceAttributes);

    for (var i = 0; i < attr_keys.length; i++) {
        if (attributes[attr_keys[i]] !== referenceAttributes[attr_keys[i]]) {
            return false;
        }
    }
    return true;
};

var restrictTo = function(setupParams, referenceObj) {
    var subject_label;
    var directory_label;
    var variable_input;
    var variable_label;
    var directory_selector;

    var currentParams = {};

    var selectorAdded = false;

    Object.keys(setupParams).forEach(function(subject) {
        subject_label = $('#' + 'subject_' + subject);
        directory_selector = $('#' + subject.toLowerCase() + '_directory_selector');
        currentParams['subject'] = subject;

        if (matchesAttributes(currentParams, referenceObj)) {
            if (!referenceObj.directory) {
                directory_selector.val('All Directories');
            }
            subject_label.removeClass('inactive');
            directory_selector.removeClass('inactive');
        } else {
            subject_label.addClass('inactive');
            directory_selector.addClass('inactive');
        }

        Object.keys(setupParams[subject]).forEach(function(directory) {
            directory_label = $('#' + subject + '_' + directory.replace(/\//g, ''));

            currentParams['directory'] = directory;

            if (matchesAttributes(currentParams, referenceObj)) {
                directory_label.removeClass('inactive');
            } else {
                directory_label.addClass('inactive');
            }

            Object.keys(setupParams[subject][directory]).forEach(function(variable) {
                variable_input = $('#' + (subject + directory + variable).replace(/\//g, ''));
                variable_label = $('#' + (subject + directory + variable + '_label').replace(/\//g, ''));

                currentParams['variable'] = variable;

                if (matchesAttributes(currentParams, referenceObj)) {
                    variable_input.removeClass('inactive');
                    variable_input.attr('style', '');
                    variable_label.removeClass('inactive');
                } else {
                    variable_input.addClass('inactive');
                    variable_input.attr('style', 'display: none;');
                    variable_label.addClass('inactive');
                }
            });

            if (matchesAttributes(currentParams, referenceObj)) {
                checkChildrenAndDisplayDirectory(subject, directory);
            }
        });
    });
}

var checkChildrenAndDisplayDirectory = function(subject, directory) {
    var active_vars = $('[id^=' +
                    (subject + directory).replace(/\//g, '') + ']'
                ).not('.inactive')
                .not('.advanced_inactive');

    var directory_label = $('#' + subject + '_' + directory.replace(/\//g, ''));

    if (active_vars.length === 0) {
        directory_label.addClass('inactive');
    } else {
        directory_label.removeClass('inactive');
    }
}

var updateDescription = function(subject, directory, variable, setupParams) {
    var active_variable_name = $('#active_variable_name');
    var active_variable_description = $('#active_variable_description');
    var error_msg = 'No description avaliable';

    active_variable_name.text(variable);
    active_variable_description.text(setupParams[subject][directory][variable].description || error_msg);
};

var removeChildren = function(elementID) {
    var results_zone = document.getElementById(elementID);

    while (results_zone.firstChild) {
        results_zone.removeChild(results_zone.firstChild);
    }
};

var searchVariables = function(setupParams, searchTerm, resultsID) {
    if (searchTerm.trim().length) {
        var hits = $('[name^="' + searchTerm.trim() + '" i]');

        removeChildren(resultsID);

        for (var i = 0; i < hits.length; i++) {
            var result = $('<p> ' + $(hits[i]).attr('name') + ' (' + $(hits[i]).attr('directory') + ')' + '</p>');
            result.attr('subject', $(hits[i]).attr('subject'));
            result.attr('directory', $(hits[i]).attr('directory'));
            result.attr('hitId', $(hits[i]).attr('id'));
            result.attr('advancedAndHidden', $(hits[i]).hasClass('advanced_inactive'));

            result.click(function() {
                var isHidden =  $(this).attr('advancedAndHidden') === 'true';

                if (isHidden && ($('#advanced_button').text().trim() === 'Show all FLASH parameters')) {
                    alert('This variable is currently hidden. ' +
                    'Click "Show all FLASH parameters" on the right hand' +
                    ' side of the screen to make it searchable');
                } else {
                    restrictTo(setupParams, {
                        'subject': $(this).attr('subject'),
                        'directory': $(this).attr('directory')
                    });

                    $('#advanced_button').click();
                    $('#advanced_button').click();

                    var classesAdded = false;
                    var maxIterations = 12;
                    var iterations = 0;
                    var hitLabel = $('#' + $(this).attr('hitId') + '_label');

                    $('html, body').animate({
                        scrollTop: hitLabel.offset().top - 82
                    }, 700);

                    var searchMatchInterval = setInterval(function(){
                        iterations += 1
                        if (!classesAdded) {
                            hitLabel.addClass('search_match');
                            classesAdded = true;
                        } else {
                            hitLabel.removeClass('search_match');
                            classesAdded = false;
                        }

                        if (iterations === maxIterations) {
                            clearInterval(searchMatchInterval);
                            hitLabel.removeClass('search_match');
                        }
                    }, 100);
                }
            });
            $('#' + resultsID).append(result);
        }
    } else {
        removeChildren(resultsID);
    }
};

module.exports = {
    restrictTo: restrictTo,
    updateDescription: updateDescription,
    checkChildrenAndDisplayDirectory: checkChildrenAndDisplayDirectory,
    searchVariables: searchVariables
}
