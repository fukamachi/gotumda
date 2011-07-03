// Copyright 2011 Eitarow Fukamachi. All rights reserved.

/**
 * @fileoverview JavaScript used in PC browser.
 *
 * @author e.arrows@gmail.com (Eitarow Fukamachi)
 */

goog.provide('got.app.PC');

goog.require('got.Api');
goog.require('got.task');
goog.require('goog.dom');
goog.require('goog.style');
goog.require('goog.events');
goog.require('goog.array');
goog.require('goog.uri.utils');
goog.require('goog.fx.DragListGroup');
goog.require('goog.fx.DragListDirection');

/**
 * Class for PC frontend.
 * @param {String} baseUri
 * @constructor
 */
got.app.PC = function(baseUri) {
  /**
   * @type {got.App}
   * @protected
   */
  this.api_ = new got.Api(baseUri);

  /**
   * Is the cursor on a link.
   * @type {Boolean}
   * @protected
   */
  this.isOnLink_ = false;

  if (goog.dom.getElement('got-public-tasks')) {
    this.loadPublicTasks();
  }
  if (goog.dom.getElement('got-my-tasks')) {
    this.loadMyTasks();
  }
  if (goog.dom.getElement('got-project-tasks')) {
    this.loadProjectTasks();
  }

  this.listenPostButton_();
};

/**
 * Load all tasks and render them in the specified element.
 */
got.app.PC.prototype.loadPublicTasks = function() {
  this.api_.allTasks(
    goog.bind(function(tasks) {
      var element = goog.dom.getElement('got-public-tasks');
      element.innerHTML = '';
      goog.array.forEach(tasks, function(task) {
        got.task.render(task, element);
      });
      var form = goog.dom.getElement('got-post-task');
      goog.events.listen(form, goog.events.EventType.SUBMIT,
                         this.onSubmit_, false, this);
      this.listenTaskAction_(element);
    }, this));
};

got.app.PC.prototype.loadMyTasks = function() {
  this.api_.myTasks(
    goog.bind(function(tasks) {
      var element = goog.dom.getElement('got-my-tasks');
      element.innerHTML = '';
      goog.array.forEach(tasks, function(task) {
        got.task.renderLine(task, element);
      });
      this.listenDragEvents_(element);
    }, this)
  );
};

got.app.PC.prototype.loadProjectTasks = function() {
  this.api_.projectTasks(
    goog.uri.utils.getPath(location.href).replace(/^\/project\//, ''),
    goog.bind(function(tasks) {
      var element = goog.dom.getElement('got-project-tasks');
      element.innerHTML = '';
      goog.array.forEach(tasks, function(task) {
        got.task.render(task, element);
      });
    })
  );
};

got.app.PC.prototype.onSubmit_ = function(e) {
  var form = e.target;
  var textarea = goog.dom.getElementsByTagNameAndClass('textarea', null, form)[0];
  this.api_.update(null, textarea.value, null, function(res) {
    got.task.render(res, 'got-public-tasks');
  });
  textarea.value = '';
};

/**
 * Event handler fired on end of dragging.
 * @param {goog.events.BrowserEvent} e
 * @protected
 */
got.app.PC.prototype.onDragEnd_ = function(e) {
  var checkboxes
      = goog.dom.getElementsByClass('got-taskitem-done', this.taskListEl_);
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
got.app.PC.prototype.getHandlerForDragItem_ = function(item) {
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
    this.getHandlerForDragItem_
  );
  goog.events.listen(dlg, goog.fx.DragListGroup.EventType.DRAGEND,
                     this.onDragEnd_ , false, this);

  // don't drag if the cursor is on a link.
  goog.events.listen(
    dlg, goog.fx.DragListGroup.EventType.BEFOREDRAGSTART,
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
    goog.events.unlisten(taskEl, goog.events.EventType.MOUSEOVER,
                         this.dlg_.handleDragItemMouseover_, false, this.dlg_);
    var doneTaskListEl = goog.dom.getElement('got-done-tasks');
    goog.dom.insertChildAt(doneTaskListEl, taskEl, 0);
  } else {
    goog.events.listen(taskEl, goog.events.EventType.MOUSEOVER,
                       this.dlg_.handleDragItemMouseover_, false, this.dlg_);
    var curTaskListEl = goog.dom.getElement('got-current-tasks');
    curTaskListEl.appendChild(taskEl);
  }
  this.api_.update(
    checkEl.value, null, null, checkEl.checked
  );
  goog.style.showElement(
    goog.dom.getElementByClass('got-taskitem-action', taskEl), false);
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

/**
 * Listen click events on task action (Edit/Delete).
 * @param {Element|String} element
 * @protected
 */
got.app.PC.prototype.listenTaskAction_ = function(element) {
  element = goog.dom.getElement(element);
  var tasks = goog.dom.getElementsByClass('got-taskitem', element);
  goog.array.forEach(tasks, function(task) {
    var actionEl = goog.dom.getElementByClass('got-taskitem-action', task);
    goog.events.listen(
      actionEl.childNodes[0], goog.events.EventType.CLICK,
      function(e) {
        this.api_.copy(task['taskId'], function(res) {
          got.task.render(res, 'got-public-tasks');
        });
      }, false, this);
    goog.events.listen(
      actionEl.childNodes[1], goog.events.EventType.CLICK,
      function(e) {
        this.api_.move(task['taskId'], function(res) {
          goog.dom.removeNode(task);
          got.task.render(res, 'got-public-tasks');
        });
      }, false, this);
  }, this);
};

got.app.PC.prototype.listenPostButton_ = function() {
  var button = goog.dom.getElement('got-post-button');
  var form = goog.dom.getElement('got-post-form');
  var textarea = goog.dom.getElementsByTagNameAndClass('textarea', null, form)[0];
  goog.events.listen(button, goog.events.EventType.CLICK, function(e) {
    goog.style.showElement(button, false);
    goog.style.showElement(form, true);
    textarea.focus();
    goog.events.listen(document.body, goog.events.EventType.CLICK,
                       this.blurPostForm_, false, this);
  }, false, this);
};

/**
 * @private
 */
got.app.PC.prototype.blurPostForm_ = function(e) {
  var button = goog.dom.getElement('got-post-button');
  var form = goog.dom.getElement('got-post-form');
  var textarea = goog.dom.getElementsByTagNameAndClass('textarea', null, form)[0];
  if (textarea.value === '' &&
      e.target !== button &&
      !goog.dom.getAncestor(e.target, function(el) {
        return el.id === 'got-post-form'; })) {
    goog.style.showElement(form, false);
    goog.style.showElement(button, true);
    goog.events.unlisten(document.body, goog.events.EventType.CLICK,
                         this.blurPostForm_, false, this);
  }
};
