(in-package :cleact.core)

(unless (boundp '+reserved-props+)
  (defconstant +reserved-props+
    '(:key :ref :children)))

(unless (boundp '+element-types+)
  (defconstant +element-types+
    '(:h1 :p)))

(defun element-type-p (obj)
  (member obj +element-types+))

(deftype element-type ()
  '(satisfies element-type-p))

(deftype element-props ()
  '(association-list symbol t))

(defstruct element
  (type nil :type element-type)
  (props nil :type element-props)
  (key nil :type (or null symbol))
  (ref nil :type t))

(deftype element-children ()
  '(or null string element (array element)))

(declaim (ftype (function (element) element-children) element-children))
(defun element-children (element)
  (let* ((props (element-props element))
         (children (access props 'children)))
    (declare (type element-children children))
    children))

(declaim (ftype (function (element-type element-props &optional element-children) element) create-element))
(defun create-element (type props &optional (children nil))
  (let* ((key (access props 'key))
         (ref (access props 'ref))
         (children (typecase children
                     (list (make-array (length children)
                                       :element-type
                                       'element
                                       :initial-contents
                                       children))
                     (element-children children)
                     (t (signal 'type-error
                                :expected-type
                                'element-children))))
         (normal-props (remove-if (lambda (x) (member (car x) +reserved-props+)) props))
         (final-props (cons
                       `(children . ,children)
                       normal-props)))
    (make-element :type type
                  :props final-props
                  :key key
                  :ref ref)))
