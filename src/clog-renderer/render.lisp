(in-package :cleact.clog-renderer)

(declaim (type renderer *renderer*))
(defvar *renderer*
  (make-instance 'renderer
                 :should-set-text-content-p #'should-set-text-content-p
                 :prepare-update #'prepare-update))

(declaim (ftype (function (clog-element) t) remove-all-children))
(defun remove-all-children (clog-element)
  "Remove any children from CLOG-ELEMENT."
  (let ((child (first-child clog-element)))
    (loop while (not (equal (html-id child) "undefined"))
          do (let ((next (next-sibling child)))
               (destroy child)
               (setf child next)))))

(declaim (ftype (function (element clog-element) t) render))
(defun render (element container)
  ;; Clear any existing contents from container
  (remove-all-children container)
  ;; create a reconciler-container
  (create-container *renderer* container)
  ;; updatecontainer
  (update-container element container))
