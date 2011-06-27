// Copyright 2011 Eitarow Fukamachi. All rights reserved.

/**
 * @fileoverview Task.
 *
 * @author e.arrows@gmail.com (Eitarow Fukamachi)
 */

goog.provide('got.Task');

goog.require('goog.dom');
goog.require('goog.json');

/**
 * Class for Task.
 * @param {Object} params
 * @constructor
 */
got.Task = function(params) {
  /**
   * @type {Integer}
   */
  this.id = params['id'];

  /**
   * @type {String}
   */
  this.body = params['body'];

  /**
   * @type {Boolean}
   */
  this.isDone = goog.json.parse(params['isDone']);
};

/**
 * @param {Element=} element Where to render.
 */
got.Task.prototype.render = function(element) {
  /**
   * @type {Element}
   * @protected
   */
  this.element_ = goog.dom.getElement(element);

  var taskEl =  goog.dom.createDom('div', 'got-taskitem');
  var doneCheckEl = goog.dom.createDom(
    'input',
    {'class': 'got-taskitem-done',
     'type': 'checkbox',
     'name': 'id',
     'value': this.id}
  );
  if (this.isDone) {
    doneCheckEl.checked = true;
  }
  taskEl.appendChild(doneCheckEl);
  var taskBodyEl = goog.dom.createDom('span', 'got-taskitem-body');
  taskBodyEl.innerHTML = this.body;
  taskEl.appendChild(taskBodyEl);
  this.element_.appendChild(taskEl);
};
