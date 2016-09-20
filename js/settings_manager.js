'use strict';

/* global $ */

var file_manager = require('./js/file_manager');

$(function() {
    var display_zone = $('.display_zone'),
        settings_form = $('#settings_form');

    var passPath = function(event){
        var writePath = $('#write_path_text').val();

        $('#' + this.id.replace('_file', '_text')).val(this.files[0].path);
        if (this.id === 'read_path_file' && (!writePath.trim() || writePath === this.files[0].path)) {
            $('#write_path_text').val(this.files[0].path);
        }
    };

    file_manager.gatherPathFiles('./config.json', function(result) {
        var all_keys = Object.keys(result.data),
            curLabel,
            curFileInput,
            curTextInput;

        for (var i = 0; i < all_keys.length; ++i) {
            curLabel = all_keys[i].replace('_', ' ');

            curFileInput = $('#' + all_keys[i].toLowerCase() + '_file');
            curFileInput.change(passPath);

            curTextInput = $('#' + all_keys[i].toLowerCase() + '_text');
            curTextInput.val(result.data[all_keys[i]]);
            curTextInput.click(function(event) {
                this.select();
            });
        }

        $('#save_settings').click(function(event) {
            for (var j = 0; j < all_keys.length; ++j) {
                result.data[all_keys[j]] = $('#' + all_keys[j].toLowerCase() + '_text').val();
            }
            alert('Files written successfully');
            file_manager.writePathFiles('./config.json', result.data);
        });
    });
});
