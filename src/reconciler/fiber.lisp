(in-package :cleact.reconsiler)

(eval-when (:compile-toplevel)
  (unless (boundp '+fiber-tags+)
    (defconstant +fiber-tags+
      '(functional-component
        host-component
        host-text)))

  (unless (boundp '+fiber-flags+)
    (defconstant +fiber-flags+
      '(content-reset
        ref
        ref-static)))

  (defun fiber-tag-p (tag)
    (member tag +fiber-tags+))

  (defun fiber-flag-p (flag)
    (member flag +fiber-flags+))

  (deftype fiber-tag ()
    '(satisfies fiber-tag-p))

  (deftype fiber-flag ()
    '(satisfies fiber-flag-p))

  (deftype fiber-type ()
    'symbol))

(defclass fiber ()
  ((tag :accessor fiber-tag
        :initarg :tag
        :initform (error "tag is required")
        :type fiber-tag)
   (type :accessor fiber-type
         :initarg :type
         :initform (error "type is required")
         :type fiber-type)
   (flags :accessor fiber-flags
          :initarg :flags
          :initform (make-hash-table)
          :type hash-table)
   (memoized-props :accessor fiber-memoized-props
                   :initarg :memoized-props
                   :initform nil
                   :type (or null association-list))
   (pending-props :accessor fiber-pending-props
                  :initarg :pending-props
                  :initform nil
                  :type (or null association-list))
   (alternate :accessor fiber-alternate
              :initarg :alternate
              :initform nil
              :type (or null fiber))
   (ref :accessor fiber-ref
        :initarg :ref
        :initform nil
        :type t)
   (parent :accessor fiber-parent
           :initarg :parent
           :initform nil
           :type (or null fiber))
   (child :accessor fiber-child
           :initarg :child
           :initform nil
           :type (or null fiber))
   (sibling :accessor fiber-sibling
            :initarg :sibling
            :initform nil
            :type (or null fiber))))

(declaim (ftype (function (fiber fiber-flag) t) fiber-set-flag))
(defun fiber-set-flag (fiber flag)
  (setf (gethash flag (fiber-flags fiber)) t))

(declaim (ftype (function (fiber fiber-flag) t) fiber-check-flag))
(defun fiber-check-flag (fiber flag)
  (gethash flag (fiber-flags fiber)))

(declaim (ftype (function (fiber fiber-flag) t) fiber-remove-flag))
(defun fiber-remove-flag (fiber flag)
  (remhash flag (fiber-flags fiber)))

(declaim (ftype (function (fiber) t) fiber-clear-flags))
(defun fiber-clear-flags (fiber)
  (setf (fiber-flags fiber) (make-hash-table)))
