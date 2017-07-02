var express = require('express');
var router = express.Router();
var path = require('path');

var apidefault = require('../api/default/index');
var mongoose = require('mongoose');
var Schema = mongoose.Schema;
var Users = require('../api/default/users');


// User setup
mongoose.connect('mongodb://localhost:27017/test');
var db = mongoose.connection;

db.on('error', function (err) {
    console.log('connection error:'+ err.message);
});
db.once('open', function callback () {
    console.log('Connected to database');
});

function adduser(req, res) {
  console.log ("Hello!");
  var u = {
    username   : 'Test',
    oauthcode  : 'SomeMore'
  }
  console.log(db)

/*
 *  db.collection('users').insert(u, function (err, createdUser) {
 *
 *  })
 */
  //Load main site
  res.sendFile(path.join(__dirname, '../../dist/index.html'));
}


module.exports = function (app) {
  app.use('/api/default', apidefault);

  app.use(express.static(path.join(__dirname, '../../dist')));
  app.get('/adduser', adduser);

  app.route('/*').get(function(req, res, next) {
    res.sendFile(path.join(__dirname, '../../dist/index.html'));
  });
}
