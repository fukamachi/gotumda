// Copyright 2011 Eitarow Fukamachi. All rights reserved.

/**
 * @fileoverview Class for Task.
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
   * @type {String}
   */
  this.url = params['url'];

  /**
   * @type {Boolean}
   */
  this.isDone = goog.json.parse(params['isDone']);
};

/**
 * Render this task into the specified element.
 * @param {Element|String} element Where to render.
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
  var taskBodyEl = goog.dom.createDom('div', 'got-taskitem-body');
  if (this.url === "") {
    taskBodyEl.innerHTML = '<div>' + this.body + '</div>';
  } else {
    taskBodyEl.appendChild(
      goog.dom.createDom('div', null,
                         goog.dom.createDom('a', {'href': this.url}, this.body))
    );
  }
  taskEl.appendChild(taskBodyEl);
  var editEl = goog.dom.createDom('a', null, 'Edit');
  var deleteEl = goog.dom.createDom('a', null, 'Delete');
  var taskActionEl = goog.dom.createDom(
    'div', {'class': 'got-taskitem-action',
            'style': 'display: none;'},
    editEl, deleteEl
  );
  taskEl.appendChild(taskActionEl);

  this.element_.appendChild(taskEl);
};
