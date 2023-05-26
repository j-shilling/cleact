(in-package :cleact.reconciler)

(defclass renderer ()
  ((should-set-text-content-p :reader renderer-should-set-text-content-p
                              :initarg :should-set-text-content-p
                              :initform (error "should-set-text-content-p is required")
                              :type (function (fiber-type association-list) boolean)
                              :documentation
                              "A function that takes a fiber type and it's props to check whether the inner
text of this node should be updated.")
   (prepare-update :reader renderer-prepare-update
                   :initarg :prepare-update
                   :initform (error "prepare-update is required")
                   :type (function (t fiber-type association-list association-list) association-list)
                   :documentation
                   "A function that takes an element, its type, its old props and its new props to
return a collection of props that have changed.")))

(declaim (ftype (function (renderer fiber-type association-list) boolean) should-set-text-content-p))
(defun should-set-text-content-p (renderer type props)
  (let ((fn (renderer-should-set-text-content-p renderer)))
    (declare (type (function (fiber-type t) boolean) fn))
    (funcall fn type props)))

(declaim (ftype (function (renderer t fiber-type association-list association-list) association-list) prepare-update))
(defun prepare-update (renderer host-element type old-props new-props)
  (let ((fn (renderer-prepare-update renderer)))
    (declare (type (function (t fiber-type association-list association-list) association-list) fn))
    (funcall fn host-element type old-props new-props)))
