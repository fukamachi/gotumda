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
   * @protected
   */
  this.id_ = params['id'];

  /**
   * @type {String}
   * @protected
   */
  this.body_ = params['body'];

  /**
   * @type {Boolean}
   * @protected
   */
  this.isDone_ = goog.json.parse(params['isDone']);
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
  if (this.isDone_) {
    doneCheckEl.checked = true;
  }
  taskEl.appendChild(doneCheckEl);
  var taskBodyEl = goog.dom.createDom('span', 'got-taskitem-body');
  taskBodyEl.innerHTML = this.body_;
  taskEl.appendChild(taskBodyEl);
  this.element_.appendChild(taskEl);
};
