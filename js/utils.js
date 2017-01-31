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

module.exports = {
    restrictTo: restrictTo,
    updateDescription: updateDescription,
    checkChildrenAndDisplayDirectory: checkChildrenAndDisplayDirectory
}
