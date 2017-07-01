var express = require('express');
var router = express.Router();
var path = require('path');

var apidefault = require('../api/default');


function adduser(req, res) {
  console.log ("Hello!");
}

module.exports = function (app) {
  app.use('/api/default', apidefault);

  app.get('/adduser', adduser);
  app.use(express.static(path.join(__dirname, '../../dist')));

  app.route('/*').get(function(req, res, next) {
    res.sendFile(path.join(__dirname, '../../dist/index.html'));
  });
}
