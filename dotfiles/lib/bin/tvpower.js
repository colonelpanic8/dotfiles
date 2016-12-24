#!/usr/bin/env node

var lgtv = require("lgtv2")({
    url: 'ws://10.0.0.136:3000'
});

lgtv.on('connect', function () {
    console.log('connected');
    lgtv.request('ssap://system/turnOff', function (err, res) {
        lgtv.disconnect();
    });
});
