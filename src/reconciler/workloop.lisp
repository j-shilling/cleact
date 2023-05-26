(in-package :cleact.reconciler)

(require :types "types.lisp")

;;
;; Dynamic Environment
;;

(declaim (type (nullable fiber) *work-in-progress*))
(defvar *work-in-progress* nil)

(declaim (type boolean *did-receive-update*))
(defvar *did-receive-update-p* nil)

(declaim (type (nullable renderer) *renderer*))
(defvar *renderer* nil)

;;
;; Utilities
;;

(declaim (ftype (function (association-list association-list) boolean) props-equal-p))
(defun props-equal-p (alist-a alist-b)
  (and (= (length alist-a) (length alist-b))
       (every (lambda (pair)
                (destructuring-bind (k v) pair
                  (equal v (access k alist-b))))
              alist-a)))

;;
;; Tag fibers
;;

(declaim (ftype (function (fiber) t) mark-update))
(defun mark-update (work-in-progress)
  (fiber-set-flag work-in-progress 'update))

(declaim (ftype (function (fiber) t) mark-ref))
(defun mark-ref (work-in-progress)
  (fiber-set-flag work-in-progress 'ref)
  (fiber-set-flag work-in-progress 'ref-static))

(declaim (ftype (function (nullable-fiber fiber) t) mark-ref-maybe))
(defun mark-ref-maybe (current work-in-progress)
  (let ((ref (fiber-ref work-in-progress)))
    (when (or
           (and (not current) ref)
           (and current (not (equal ref (fiber-ref current)))))
      (mark-ref work-in-progress))))

;;
;; Reconcile Children: See child-reconciler.lisp
;;

(declaim (ftype (function (nullable-fiber fiber t) t) reconcile-children))
(defun reconcile-children (current work-in-progress next-children)
  (if current
      (mount-child-fibers work-in-progress next-children)
      (reconcile-child-fibers work-in-progress (fiber-child current) next-children)))

;;
;; Begin Update By Fiber Type
;;

(declaim (ftype (function (nullable-fiber fiber function association-list) nullable-fiber) begin-update-function-component))
(defun begin-update-function-component (current work-in-progress component next-props)
  (let ((result)
        (next-children (render-with-hooks current work-in-progress component next-props)))
    (declare (type nullable-fiber result))
    (if (and current (not *did-receive-update-p*))
        (progn
          (bailout-hooks current work-in-progress)
          (setf result (bailout-on-already-finished-work current work-in-progress)))
        (progn
          (fiber-set-flag work-in-progress 'performed-work)
          (reconcile-children current work-in-progress next-children)
          (setf result (fiber-child work-in-progress))))
    result))

(declaim (ftype (function (nullable-fiber fiber) nullable-fiber) begin-update-host-component))
(defun begin-update-host-component (current work-in-progress)
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
    (mark-ref-maybe current work-in-progress)
    (reconcile-children current work-in-progress next-children)
    (fiber-child work-in-progress)))

(declaim (ftype (function (nullable-fiber fiber) nullable-fiber) begin-update-host-text))
(defun begin-update-host-text (current work-in-progress)
  (declare (ignore current))
  (declare (ignore work-in-progress))
  ;; do nothing
  nil)

;;
;; Complete Update By Fiber Tag
;;

(declaim (ftype (function (fiber fiber string string) t) complete-update-host-text))
(defun complete-update-host-text (current work-in-progress old-text new-text)
  (declare (ignore current))
  (unless (equal old-text new-text)
    (mark-update work-in-progress)))

(declaim (ftype (function (fiber fiber fiber-type association-list) t) complete-update-host-component))
(defun complete-update-host-component (current work-in-progress type new-props)
  (let ((old-props (fiber-memoized-props current)))
   (when (not (props-equal-p old-props new-props))
     (let ((update-payload (prepare-update *renderer*
                                           (fiber-state-node work-in-progress)
                                           type
                                           old-props
                                           new-props)))
       (setf (fiber-update-queue work-in-progress) update-payload)
       (when update-payload
         (mark-update work-in-progress))))))

(declaim (ftype (function (fiber) boolean) bubble-properties))
(defun bubble-properties (completed-work)
  (let ((did-bailout-p (and (fiber-alternate completed-work)
                            (fiber-child (fiber-alternate completed-work))))
        (child (fiber-child completed-work)))
    (loop while child
          do (flet ((add-flag (flag)
                      (declare (type fiber-flag flag))
                      (when (or (fiber-static-flag-p flag) (not did-bailout-p))
                       (fiber-set-subtree-flag completed-work flag))))
               (map nil #'add-flag (hash-table-keys (fiber-flags child)))
               (map nil #'add-flag (hash-table-keys (fiber-subtree-flags child)))
               (setf (fiber-parent child) completed-work)
               (setf child (fiber-sibling child))))
    did-bailout-p))

;;
;; High Level Work Loop Stages
;;

(declaim (ftype (function (nullable-fiber fiber) nullable-fiber) begin-work))
(defun begin-work (current work-in-progress)
  (let ((new-props (fiber-pending-props work-in-progress)))
    (if current
        (let ((old-props (fiber-memoized-props current))
              (new-props (fiber-pending-props work-in-progress)))
          (setf *did-receive-update-p* (not (props-equal-p old-props new-props))))
        (setf *did-receive-update-p* nil))
    (ecase (fiber-tag work-in-progress)
      (functional-component
       (begin-update-function-component current work-in-progress
                                        (fiber-type work-in-progress)
                                        new-props))
      (host-component
       (begin-update-host-component current work-in-progress))
      (host-text
       (begin-update-host-text current work-in-progress)))))

(declaim (ftype (function (nullable-fiber fiber) nullable-fiber) complete-work))
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

(declaim (ftype (function (nullable-fiber fiber) nullable-fiber) bailout-on-already-finished-work))
(defun bailout-on-already-finished-work (current work-in-progress)
  (declare (ignore current))
  (declare (ignore work-in-progress))
  (error "not implemented"))

(defun work-loop ()
  (loop while *work-in-progress*
        do (perform-unit-of-work *work-in-progress*)))
