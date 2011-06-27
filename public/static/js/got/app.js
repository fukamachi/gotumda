// Copyright 2011 Eitarow Fukamachi. All rights reserved.

/**
 * @fileoverview Main package for talking with backend.
 *
 * @author e.arrows@gmail.com (Eitarow Fukamachi)
 */

goog.provide('got.App');

goog.require('got.app.Task');
goog.require('goog.array');
goog.require('goog.net.XhrIo');
goog.require('goog.net.EventType');
goog.require('goog.events');

/**
 * @param {String} baseUrl
 * @constructor
 */
got.App = function(baseUrl) {
  this.baseUrl = baseUrl;
};

/**
 * Request to 'api/all-tasks.json'.
 * @param {Function(Array.<Object>)} callback
 */
got.App.prototype.allTasks = function(callback) {
  var xhr = new goog.net.XhrIo();
  goog.events.listen(
    xhr, goog.net.EventType.COMPLETE,
    function(e) {
      var res = e.target.getResponseJson();
      var tasks = [];
      goog.array.forEach(res, function (data) {
        var task = new got.app.Task(data['id'], data['body']);
        tasks.push(task);
      });
      callback(tasks);
    });
  xhr.send(
    this.baseUrl + 'api/all-tasks.json', 'GET'
  );
};

got.App.prototype.update = function(body) {
};
