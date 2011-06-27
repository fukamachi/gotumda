// Copyright 2011 Eitarow Fukamachi. All rights reserved.

/**
 * @fileoverview Task.
 *
 * @author e.arrows@gmail.com (Eitarow Fukamachi)
 */

goog.provide('got.app.Task');

goog.require('goog.dom');

/**
 * @constructor
 */
got.app.Task = function(id, body) {
  this.id_ = id;
  this.body_ = body;
};

/**
 * CSS class name for each Task items.
 * @type {String}
 */
got.app.Task.CSS_NAME = 'got-taskitem';

/**
 * @param {Element=} element Where to render.
 */
got.app.Task.prototype.render = function(element) {
  /**
   * @type {Element}
   * @protected
   */
  this.element_ = goog.dom.getElement(element);
  var taskEl =  goog.dom.createDom('div', got.app.Task.CSS_NAME);
  taskEl.innerHTML = this.body_;
  this.element_.appendChild(taskEl);
};
