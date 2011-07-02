// Copyright 2011 Eitarow Fukamachi. All rights reserved.

/**
 * @fileoverview Class for Task.
 *
 * @author e.arrows@gmail.com (Eitarow Fukamachi)
 */

goog.provide('got.task');

goog.require('goog.dom');
goog.require('goog.json');

/**
 * Render this task into the specified element.
 * @param {Object} task
 * @param {Element|String} element Where to render.
 */
got.task.render = function(task, element) {
  /**
   * @type {Element}
   * @protected
   */
  element = goog.dom.getElement(element);

  var taskEl =  goog.dom.createDom('div', 'got-taskitem');
  taskEl.appendChild(goog.dom.createDom('img', {'src': task['user']['imageUrl']}));
  var taskBodyEl = goog.dom.createDom('div', 'got-taskitem-body');
  taskBodyEl.innerHTML = '<div>' + task['body'] + '</div>';
  taskEl.appendChild(taskBodyEl);
  var copyEl = goog.dom.createDom('a', null, 'Copy');
  var moveEl = goog.dom.createDom('a', null, 'Move');
  var taskActionEl = goog.dom.createDom(
    'div', {'class': 'got-taskitem-action'},
    copyEl, moveEl
  );
  taskEl.appendChild(taskActionEl);

  taskEl['task-id'] = task['oid'];

  element.appendChild(taskEl);
};
