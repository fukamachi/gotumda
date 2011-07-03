// Copyright 2011 Eitarow Fukamachi. All rights reserved.

/**
 * @fileoverview Class for Task.
 *
 * @author e.arrows@gmail.com (Eitarow Fukamachi)
 */

goog.provide('got.task');

goog.require('goog.dom');
goog.require('goog.json');
goog.require('goog.array');

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

  taskEl.appendChild(
    goog.dom.createDom('div', 'profile-image',
                       goog.dom.createDom('img',
                                          {'src': task['owner']['imageUrl'],
                                           'class': 'got-taskitem-owner'})));
  if (task['user']['name'] !== task['owner']['name']) {
    taskEl.appendChild(
      goog.dom.createDom('img', {'src': task['user']['thumbnailUrl'],
                                 'class': 'got-taskitem-user'})
    );
  }

  var taskBodyEl = goog.dom.createDom('div', 'got-taskitem-body');
  if (task['projects'] && !goog.array.isEmpty(task['projects'])) {
    goog.array.forEach(task['projects'], function(project) {
      var a = goog.dom.createDom('a',
                                 {'href': '/project/'+project},
                                 '#'+project);
      task['body'] = task['body'].replace(
        '#'+project, goog.dom.getOuterHtml(a));
    });
  }
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
    'div', 'got-taskitem-action',
    copyEl, passEl
  );
  taskEl.appendChild(taskActionEl);

  taskEl['taskId'] = task['taskId'];

  goog.dom.insertChildAt(element, taskEl, 0);
};

got.task.renderLine = function(task, element) {
  element = goog.dom.getElement(element);

  var taskEl = goog.dom.createDom(
    'div', 'got-taskitemline',
    goog.dom.createDom(
      'input',
      {'class': 'got-taskitem-done', 'type': 'checkbox'}
    ),
    goog.dom.createDom(
      'div', 'got-taskitem-body',
      goog.dom.createDom(
        'img', {'src': task['user']['thumbnailUrl']}),
      task['body']
    )
  );

  goog.dom.insertChildAt(element, taskEl, 0);
};
