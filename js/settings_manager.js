'use strict';

/* global $ */

var file_manager = require('./js/file_manager');

$(function() {
    var display_zone = $('.display_zone'),
        settings_form = $('#settings_form');

    var passPath = function(event){
        console.log(this.files[0].path);
        console.log(this.id.replace('_file', '_text'));
        $('#' + this.id.replace('_file', '_text')).val(this.files[0].path);
    };

    file_manager.gatherPathFiles('./config.json', function(result) {
        var all_keys = Object.keys(result.data),
            curLabel,
            curFileInput,
            curTextInput;

        for (var i = 0; i < all_keys.length; ++i) {
            curLabel = all_keys[i].replace('_', ' ');

            console.log('#' + all_keys[i].toLowerCase() + '_file');

            curFileInput = $('#' + all_keys[i].toLowerCase() + '_file');
            curFileInput.change(passPath);

            curTextInput = $('#' + all_keys[i].toLowerCase() + '_text');
            curTextInput.val(result.data[all_keys[i]]);
        }
    });
});
