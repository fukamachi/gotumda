// Copyright 2011 Eitarow Fukamachi. All rights reserved.

/**
 * @fileoverview Main package for talking with backend.
 *
 * @author e.arrows@gmail.com (Eitarow Fukamachi)
 */

goog.provide('got.Api');

goog.require('goog.array');
goog.require('goog.net.XhrIo');
goog.require('goog.net.EventType');
goog.require('goog.events');
goog.require('goog.uri.utils');

/**
 * @param {String} baseUrl
 * @constructor
 */
got.Api = function(baseUrl) {
  this.baseUrl = baseUrl;
};

/**
 * Request to 'api/all-tasks.json'.
 * @param {Function(Array.<Object>)} callback
 */
got.Api.prototype.allTasks = function(callback) {
  var xhr = new goog.net.XhrIo();
  goog.events.listen(
    xhr, goog.net.EventType.COMPLETE,
    function(e) {
      var res = e.target.getResponseJson();
      callback(res);
    });
  xhr.send(
    this.baseUrl + 'api/all-tasks.json', 'GET'
  );
};

got.Api.prototype.update = function(id, opt_body, opt_url, opt_isDone) {
  var query = {'id': id};
  if (!(opt_body === null || opt_body === false)) {
    query['body'] = opt_body;
  }
  if (!(opt_url === null || opt_url === false)) {
    query['url'] = opt_url;
  }
  if (arguments.length > 3) {
    query['isDone'] = opt_isDone;
  }
  var xhr = new goog.net.XhrIo();
  xhr.send(this.baseUrl + 'api/update.json', 'POST',
           goog.uri.utils.buildQueryDataFromMap(query));
};

/**
 * @param {Integer} id
 */
got.Api.prototype.destroy = function(id) {
  var xhr = new goog.net.XhrIo();
  xhr.send(this.baseUrl + 'api/destroy.json', 'POST', 'id='+id);
};

/**
 * @param {Array.<Integer>} order
 */
got.Api.prototype.sortTasks = function(order) {
  var xhr = new goog.net.XhrIo();
  xhr.send(this.baseUrl + 'api/sort-tasks.json', 'POST',
           "order="+order.join(','));
};
