(in-package :cleact.reconsiler)

(defvar *work-in-progress-root* nil)

(declaim (type (or fiber null) *work-in-progress*))
(defvar *work-in-progress* nil)

(defvar *did-receive-update* nil)
(defvar *renderer* nil)

(declaim (ftype (function ((or fiber null) fiber) t) mark-ref))
(defun mark-ref (current work-in-progress)
  (let ((ref (fiber-ref work-in-progress)))
    (when (or
           (and (not current) ref)
           (and current (not (equal ref (fiber-ref current)))))
      (fiber-set-flag work-in-progress 'ref)
      (fiber-set-flag work-in-progress 'ref-static))))

(declaim (ftype (function (fiber t) (or fiber null)) mount-child-fibers))
(defun mount-child-fibers (parent next-children)
  (declare (ignore parent))
  (declare (ignore next-children))
  (error "not implemented"))

(declaim (ftype (function (fiber (or fiber null) t) (or fiber null)) reconcile-child-fibers))
(defun reconcile-child-fibers (parent current-first-child next-children)
  (declare (ignore parent))
  (declare (ignore current-first-child))
  (declare (ignore next-children))
  (error "not implemented"))

(declaim (ftype (function ((or fiber null) fiber t) t) reconcile-children))
(defun reconcile-children (current work-in-progress next-children)
  (if current
      (mount-child-fibers work-in-progress next-children)
      (reconcile-child-fibers work-in-progress (fiber-child current) next-children)))

(declaim (ftype (function ((or fiber null) fiber function association-list) (or fiber null)) update-function-component))
(defun update-function-component (current work-in-progress component next-props)
  (declare (ignore current))
  (declare (ignore work-in-progress))
  (declare (ignore component))
  (declare (ignore next-props))
  (error "not implemented"))

(declaim (ftype (function ((or fiber null) fiber) (or fiber null)) update-host-component))
(defun update-host-component (current work-in-progress)
  (let* ((type (fiber-type work-in-progress))
         (next-props (fiber-pending-props work-in-progress))
         (prev-props (if current
                         (fiber-memoized-props current)
                         nil))
         (next-children (access 'children next-props)))
    (cond
      ((should-set-text-content-p *renderer* type next-props)
       (setf next-children nil))
      ((should-set-text-content-p *renderer* type prev-props)
       (fiber-set-flag work-in-progress 'content-reset)))
    (mark-ref current work-in-progress)
    (reconcile-children current work-in-progress next-children)
    (fiber-child work-in-progress)))

(declaim (ftype (function ((or fiber null) fiber) (or fiber null)) update-host-text))
(defun update-host-text (current work-in-progress)
  (declare (ignore current))
  (declare (ignore work-in-progress))
  ;; do nothing
  nil)

(declaim (ftype (function (association-list association-list) t) props-equal-p))
(defun props-equal-p (alist-a alist-b)
  (and (= (length alist-a) (length alist-b))
       (every (lambda (pair)
                (destructuring-bind (k v) pair
                  (equal v (access k alist-b))))
              alist-a)))

(declaim (ftype (function ((or fiber null) fiber) (or fiber null)) begin-work))
(defun begin-work (current work-in-progress)
  (let ((new-props (fiber-pending-props work-in-progress)))
    (if current
        (let ((old-props (fiber-memoized-props current))
              (new-props (fiber-pending-props work-in-progress)))
          (setf *did-receive-update* (not (props-equal-p old-props new-props))))
        (setf *did-receive-update* nil))
    (ecase (fiber-tag work-in-progress)
      (functional-component
       (update-function-component current work-in-progress
                                  (fiber-type work-in-progress)
                                  new-props))
      (host-component
       (update-host-component current work-in-progress))
      (host-text
       (update-host-text current work-in-progress)))))

(declaim (ftype (function ((or fiber null) fiber) (or fiber null)) complete-work))
(defun complete-work (current completed-work)
  (declare (ignore current))
  (declare (ignore completed-work))
  (error "not implemented"))

(declaim (ftype (function (fiber) t) complete-unit-of-unit))
(defun complete-unit-of-work (unit-of-work)
  (let ((incomplete-work unit-of-work))
    (loop while incomplete-work
          do (let* ((current (fiber-alternate incomplete-work))
                    (parent (fiber-parent incomplete-work))
                    (sibling (fiber-sibling incomplete-work))
                    (next (complete-work current incomplete-work)))
               (cond
                 (next (setf *work-in-progress* next))
                 (sibling (setf *work-in-progress* sibling))
                 (parent
                  (progn
                    (setf *work-in-progress* parent)
                    (setf incomplete-work parent))))))))

(declaim (ftype (function (fiber) t) perform-unit-of-work))
(defun perform-unit-of-work (unit-of-work)
  (let* ((current (fiber-alternate unit-of-work))
         (next (begin-work current unit-of-work)))
    (setf (fiber-memoized-props unit-of-work)
          (fiber-pending-props unit-of-work))
    (if next
        (setf *work-in-progress* next)
        (complete-unit-of-work unit-of-work))))

(defun work-loop ()
  (loop while *work-in-progress*
        do (perform-unit-of-work *work-in-progress*)))
