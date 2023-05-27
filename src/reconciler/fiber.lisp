(in-package :cleact.reconciler)

(defstruct fiber
  (tag (error "tag is required") :type fiber-tag)
  (type nil :type (nullable fiber-type))
  (flags (make-hash-table) :type hash-table)
  (subtree-flags (make-hash-table) :type hash-table)
  key
  (index 0 :type index)
  state-node
  (update-queue nil :type (nullable update-queue))
  (memoized-props nil :type association-list)
  (memoized-state nil :type association-list)
  (pending-props nil :type association-list)
  (alternate nil :type nullable-fiber)
  (deletions nil :type list) ; list of fibers
  ref
  (parent nil :type nullable-fiber)
  (child nil :type nullable-fiber)
  (sibling nil :type nullable-fiber))

(declaim (ftype (function (fiber fiber-flag) t) fiber-set-flag))
(defun fiber-set-flag (fiber flag)
  (setf (gethash flag (fiber-flags fiber)) t))

(declaim (ftype (function (fiber fiber-flag) t) fiber-check-flag))
(defun fiber-check-flag (fiber flag)
  (gethash flag (fiber-flags fiber)))

(declaim (ftype (function (fiber fiber-flag) t) fiber-remove-flag))
(defun fiber-remove-flag (fiber flag)
  (remhash flag (fiber-flags fiber)))

(declaim (ftype (function (fiber fiber-flag) t) fiber-set-subtree-flag))
(defun fiber-set-subtree-flag (fiber subtree-flag)
  (setf (gethash subtree-flag (fiber-subtree-flags fiber)) t))

(declaim (ftype (function (fiber fiber-flag) t) fiber-check-subtree-flag))
(defun fiber-check-subtree-flag (fiber subtree-flag)
  (gethash subtree-flag (fiber-subtree-flags fiber)))

(declaim (ftype (function (fiber fiber-flag) t) fiber-remove-subtree-flag))
(defun fiber-remove-subtree-flag (fiber subtree-flag)
  (remhash subtree-flag (fiber-subtree-flags fiber)))

(declaim (ftype (function (fiber) t) fiber-clear-flags))
(defun fiber-clear-flags (fiber)
  (setf (fiber-flags fiber) (make-hash-table)))

(declaim (ftype (function (fiber) t) fiber-clear-subtree-flags))
(defun fiber-clear-subtree-flags (fiber)
  (setf (fiber-subtree-flags fiber) (make-hash-table)))

(declaim (ftype (function (fiber t) fiber) create-work-in-progress))
(defun create-work-in-progress (current pending-props)
  (declare (ignore current))
  (declare (ignore pending-props))
  (error "not implemented"))

(declaim (ftype (function (fiber-tag association-list (nullable string)) fiber) create-fiber))
(defun create-fiber (tag pending-props key)
  (make-fiber
   :tag tag
   :pending-props pending-props
   :key key))

(declaim (ftype (function (string) fiber) create-fiber-from-text) )
(defun create-fiber-from-text (text-content)
  (declare (ignore text-content))
  (error "not implemented"))

(declaim (ftype (function (element) fiber) create-fiber-from-element))
(defun create-fiber-from-element (element)
  (declare (ignore element))
  (error "not implemented"))
