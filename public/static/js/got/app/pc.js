// Copyright 2011 Eitarow Fukamachi. All rights reserved.

/**
 * @fileoverview JavaScript used in PC browser.
 *
 * @author e.arrows@gmail.com (Eitarow Fukamachi)
 */

goog.provide('got.app.PC');

goog.require('got.Api');
goog.require('goog.dom');
goog.require('goog.fx.DragListGroup');
goog.require('goog.fx.DragListDirection');

/**
 * @param {String} baseUrl
 * @constructor
 */
got.app.PC = function(baseUrl) {
  this.api_ = new got.Api(baseUrl);
};

/**
 * Load all tasks and render them in the specified element.
 * @param {Element|String} element Element where to render tasks.
 */
got.app.PC.prototype.loadAllTasks = function(element) {
  this.api_.allTasks(
    function(tasks) {
      element = goog.dom.getElement(element);
      element.innerHTML = '';
      goog.array.forEach(tasks, function(task) {
        task.render(element);
      });
      var dlg = new goog.fx.DragListGroup();
      dlg.addDragList(element,
                      goog.fx.DragListDirection.DOWN);
      dlg.setDragItemHoverClass('cursor-move');
      dlg.setDraggerElClass('cursor-move');
      dlg.setFunctionToGetHandleForDragItem(
        function(item) {
          return goog.dom.getElementByClass('got-taskitem-body', item);
        }
      );
      dlg.init();
    });
};
