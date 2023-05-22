(in-package :cleact.core)

(eval-when (:compile-toplevel)
  (unless (boundp '+reserved-props+)
    (defconstant +reserved-props+
      '(key ref children)))
  (unless (boundp '+element-types+)
    (defconstant +element-types+
      '())

    (defun element-type-p (obj)
      (member obj +element-types+))

    (deftype element-type ()
      '(satisfies element-type-p))))

(deftype cleact-element-children ()
  '(or null string (array element)))

(defclass element ()
  ((type :reader element-type
         :initarg :type
         :initform (error "Type is required.")
         :type element-type)
   (props :reader element-props
          :initarg :props
          :initform '()
          :type association-list)
   (key :reader element-key
        :initarg :key
        :initform nil
        :type (or null symbol))
   (ref  :reader element-ref
         :initarg :ref
         :initform nil)))

(defun elementp (obj)
  (typep obj 'element))

(declaim (ftype (function (cleact-element) cleact-element-children) cleact-element-children))
(defun cleact-element-children (element)
  (let* ((props (cleact-element-props element))
         (children (access props 'children)))
    (declare (type cleact-element-children children))
    children))

(declaim (ftype (function (symbol association-list &optional (or null string sequence)) cleact-element) create-element))
(defun create-element (type props &optional (children nil))
  (let* ((key (access props 'key))
         (ref (access props 'ref))
         (children (typecase children
                     (null children)
                     (array children)
                     (string children)
                     (sequence (make-array (length children)
                                           :element-type
                                           'cleact-element
                                           :initial-contents
                                           children))
                     (t (signal 'type-error
                                :expected-type
                                '(or null string sequence)))))
         (normal-props (remove-if (lambda (x) (member (car x) +reserved-props+)) props))
         (final-props (if children
                          (cons
                           `(children . ,children)
                           normal-props)
                          normal-props))
         (inst
           (make-instance 'cleact-element
                          :type type
                          :props final-props
                          :key key
                          :ref ref)))
    (declare (type cleact-element inst))
    inst))
