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

  taskEl.appendChild(goog.dom.createDom('img',
                                        {'src': task['owner']['imageUrl'],
                                         'class': 'got-taskitem-owner'}));
  if (task['user']['name'] !== task['owner']['name']) {
    taskEl.appendChild(
      goog.dom.createDom('img', {'src': task['user']['thumbnailUrl'],
                                 'class': 'got-taskitem-user'})
    );
  }

  var taskBodyEl = goog.dom.createDom('div', 'got-taskitem-body');
  taskBodyEl.innerHTML = task['body'];
  taskEl.appendChild(taskBodyEl);

  var taskDataEl = goog.dom.createDom('div', 'got-taskitem-data');
  if (task['originTask']) {
    goog.dom.append(taskDataEl, 'from');
    taskDataEl.appendChild(
      goog.dom.createDom('img', {'src': task['originTask']['user']['thumbnailUrl']})
    );
    taskDataEl.appendChild(
      goog.dom.createDom('span', null, task['originTask']['body'])
    );
  }
  taskEl.appendChild(taskDataEl);

  var copyEl = goog.dom.createDom('a', null, 'Copy');
  var passEl = goog.dom.createDom('a', null, 'PassMe');
  var taskActionEl = goog.dom.createDom(
    'div', {'class': 'got-taskitem-action'},
    copyEl, passEl
  );
  taskEl.appendChild(taskActionEl);

  taskEl['taskId'] = task['taskId'];

  goog.dom.insertChildAt(element, taskEl, 0);
};
