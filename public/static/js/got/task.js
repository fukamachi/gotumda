// Copyright 2011 Eitarow Fukamachi. All rights reserved.

/**
 * @fileoverview Task.
 *
 * @author e.arrows@gmail.com (Eitarow Fukamachi)
 */

goog.provide('got.Task');

goog.require('goog.dom');

/**
 * @constructor
 */
got.Task = function(id, body) {
  this.id_ = id;
  this.body_ = body;
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
     'value': this.id_}
  );
  taskEl.appendChild(doneCheckEl);
  var taskBodyEl = goog.dom.createDom('span', 'got-taskitem-body');
  taskBodyEl.innerHTML = this.body_;
  taskEl.appendChild(taskBodyEl);
  this.element_.appendChild(taskEl);
};
