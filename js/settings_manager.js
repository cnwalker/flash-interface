'use strict';

/* global $ */

var file_manager = require('./js/file_manager');

$(function() {
    var display_zone = $('.display_zone'),
        settings_form = $('#settings_form'),

        read_path_file = $('#read_path_file'),
        write_path_file = $('#write_path_file'),
        setup_path_file = $('#setup_path_file'),

        read_path_text_field = $('#read_path_text'),
        write_path_text_field = $('#write_path_text'),
        setup_path_text_field = $('#setup_path_text');

    var passPath = function(event){
        var writePathText = write_path_text_field.val();

        $('#' + this.id.replace('_file', '_text')).val(this.files[0].path);
        console.log($('#' + this.id.replace('_file', '_text')));
        $(this).removeClass('filepath_changed');
        if (this.id === 'read_path_file') {
            write_path_text_field.val(this.files[0].path);
        }
    };

    file_manager.gatherPathFiles(__dirname + '/config.json', function(result) {
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

            file_manager.writePathFiles(__dirname + '/config.json', result.data, function() {
                alert('Settings saved successfully!');
            });

        });
    });


});
