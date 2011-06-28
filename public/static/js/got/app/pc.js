// Copyright 2011 Eitarow Fukamachi. All rights reserved.

/**
 * @fileoverview JavaScript used in PC browser.
 *
 * @author e.arrows@gmail.com (Eitarow Fukamachi)
 */

goog.provide('got.app.PC');

goog.require('got.Api');
goog.require('got.Task');
goog.require('goog.dom');
goog.require('goog.style');
goog.require('goog.events');
goog.require('goog.array');
goog.require('goog.fx.DragListGroup');
goog.require('goog.fx.DragListDirection');

/**
 * @param {String} baseUrl
 * @constructor
 */
got.app.PC = function(baseUrl) {
  /**
   * @type {got.App}
   * @protected
   */
  this.api_ = new got.Api(baseUrl);

  /**
   * Is the cursor on a link.
   * @type {Boolean}
   */
  this.isOnLink_ = false;

  this.load();
};

/**
 * Load all tasks and render them in the specified element.
 */
got.app.PC.prototype.load = function() {
  this.api_.allTasks(
    goog.bind(function(tasks) {
      var allTaskListEl = document.getElementById('got-all-tasks');
      var curTaskListEl = document.getElementById('got-current-tasks');
      var doneTaskListEl = document.getElementById('got-done-tasks');
      curTaskListEl.innerHTML = '';
      goog.array.forEach(tasks, function(taskObj) {
        var task = new got.Task(taskObj);
        task.render(
          task.isDone ? doneTaskListEl : curTaskListEl
        );
      });

      goog.array.forEach(curTaskListEl.childNodes, function(node) {
        node = goog.dom.getElementsByTagNameAndClass('a', null, node);
        goog.events.listen(node[0], goog.events.EventType.MOUSEOVER,
                           function(e) {
                             this.isOnLink_ = true;
                           }, false, this);
        goog.events.listen(node[0], goog.events.EventType.MOUSEOUT,
                           function(e) {
                             this.isOnLink_ = false;
                           }, false, this);
      }, this);
      this.listenDragEvents_(curTaskListEl);
      this.listenCheckEvents_(allTaskListEl);
      this.listenMouseEvents_(curTaskListEl);
    }, this));
};

/**
 * Event handler fired on end of dragging.
 * @param {goog.events.BrowserEvent} e
 * @protected
 */
got.app.PC.prototype.onDragEnd_ = function(e) {
  var checkboxes = goog.dom.getElementsByClass('got-taskitem-done', this.taskListEl_);
  this.api_.sortTasks(goog.array.map(checkboxes, function(box) {
    return box.value;
  }));
};

/**
 * Specify which element is draggable.
 * @param {Element} item
 * @return {Element}
 * @protected
 */
got.app.PC.prototype.handlerForDragItem_ = function(item) {
  return goog.dom.getElementByClass('got-taskitem-body', item);
};

/**
 * Listen drag events of tasks.
 * @param {Element|String} element
 * @protected
 */
got.app.PC.prototype.listenDragEvents_ = function(element) {
  element = goog.dom.getElement(element);

  /**
   * @type {goog.fx.DragListGroup}
   * @protected
   */
  var dlg = this.dlg_ = new goog.fx.DragListGroup();

  dlg.addDragList(element,
                  goog.fx.DragListDirection.DOWN);
  dlg.setDragItemHoverClass('cursor-move');
  dlg.setDraggerElClass('cursor-move dragging');
  dlg.setFunctionToGetHandleForDragItem(
    this.handlerForDragItem_
  );
  goog.events.listen(dlg, goog.fx.DragListGroup.EventType.DRAGEND,
                     this.onDragEnd_ , false, this);

  // don't drag if the cursor is on a link.
  goog.events.listen(
    this.dlg_, goog.fx.DragListGroup.EventType.BEFOREDRAGSTART,
    function(e) {
      if (this.isOnLink_) {
        e.preventDefault();
      }
    }, false, this);

  dlg.init();
};

/**
 * Event handler fired on check of checkboxes.
 * @param {goog.events.BrowserEvent} e
 * @protected
 */
got.app.PC.prototype.onCheck_ = function(e) {
  var checkEl = e.target;
  var taskEl = goog.dom.getAncestorByClass(checkEl, 'got-taskitem');
  goog.dom.removeNode(taskEl);
  if (checkEl.checked) {
    var doneTaskListEl = document.getElementById('got-done-tasks');
    doneTaskListEl.appendChild(taskEl);
  } else {
    var curTaskListEl = document.getElementById('got-current-tasks');
    curTaskListEl.appendChild(taskEl);
  }
  this.api_.update(
    checkEl.value, null, null, checkEl.checked
  );
  goog.style.showElement(goog.dom.getElementByClass('got-taskitem-action', taskEl), false);
};

/**
 * Listen click events of checkboxes.
 * @param {Element|String} element
 * @protected
 */
got.app.PC.prototype.listenCheckEvents_ = function(element) {
  element = goog.dom.getElement(element);
  var checkboxes = goog.dom.getElementsByClass('got-taskitem-done', element);
  goog.array.forEach(checkboxes, function(checkEl) {
    goog.events.listen(checkEl, goog.events.EventType.CLICK,
                       this.onCheck_ , false, this);
  }, this);
};

/**
 * Event handler fired when the mouse is over tasks.
 * @param {goog.events.BrowserEvent} e
 * @protected
 */
got.app.PC.prototype.onMouseOver_ = function(e) {
  var taskEl = goog.dom.getAncestorByClass(e.target, 'got-taskitem');
  var actionEl = goog.dom.getElementByClass('got-taskitem-action', taskEl);
  if (actionEl) {
    goog.style.showElement(actionEl, true);
  }
};

/**
 * Event handler fired when the mouse goes out of tasks.
 * @param {goog.events.BrowserEvent} e
 * @protected
 */
got.app.PC.prototype.onMouseOut_ = function(e) {
  var taskEl = goog.dom.getAncestorByClass(e.target, 'got-taskitem');
  var actionEl = goog.dom.getElementByClass('got-taskitem-action', taskEl);
  if (actionEl) {
    goog.style.showElement(actionEl, false);
  }
};

/**
 * Listen mouse events of tasks.
 * @param {Element|String} element
 * @protected
 */
got.app.PC.prototype.listenMouseEvents_ = function(element) {
  element = goog.dom.getElement(element);
  goog.events.listen(element, goog.events.EventType.MOUSEOVER,
                     this.onMouseOver_, false, this);
  goog.events.listen(element, goog.events.EventType.MOUSEOUT,
                     this.onMouseOut_, false, this);
};
