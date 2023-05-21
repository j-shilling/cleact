(in-package :cleact.reconsiler)

(defvar *should-track-side-effects-p* nil)

(declaim (ftype (function (fiber fiber) t) delete-child))
(defun delete-child (parent child-to-delete)
  (when *should-track-side-effects-p*
    (let ((deletions (fiber-deletions parent)))
      (fiber-set-flag parent 'child-deletion)
      (setf (fiber-deletions parent)
            (cons child-to-delete deletions)))))

(declaim (ftype (function (fiber (or fiber null)) t) delete-remaining-children))
(defun delete-remaining-children (parent current-first-child)
  (when *should-track-side-effects-p*
    (let ((child-to-delete current-first-child))
      (loop while child-to-delete
            do (progn
                 (delete-child parent child-to-delete)
                 (setf child-to-delete (fiber-sibling child-to-delete)))))))

(declaim (ftype (function (fiber fiber) hash-table) map-remaining-children))
(defun map-remaining-children (parent current-first-child)
  (declare (ignore parent))
  (let ((map (make-hash-table))
        (existing-child current-first-child))
    (loop while existing-child
          do (let ((key (fiber-key existing-child))
                   (index (fiber-index existing-child))
                   (sibling (fiber-sibling existing-child)))
               (if key
                   (setf (gethash key map) existing-child)
                   (setf (gethash index map) existing-child))
               (setf existing-child sibling)))
    map))

(declaim (ftype (function (fiber t) fiber) use-fiber))
(defun use-fiber (fiber pending-props)
  (let ((clone (create-work-in-progress fiber pending-props)))
    (setf (fiber-index clone) 0)
    (setf (fiber-sibling clone) nil)
    clone))

(declaim (ftype (function (fiber (integer 0 *) (integer 0 *)) (integer 0 *)) place-child))
(defun place-child (new-fiber last-placed-index new-index)
  (let ((new-last-placed-index last-placed-index))
    (setf (fiber-index new-fiber) new-index)
    (when *should-track-side-effects-p*
      (let* ((current (fiber-alternate new-fiber))
             (old-index (if current (fiber-index current) nil)))
        (if (and current old-index (>= old-index last-placed-index))
            (setf new-last-placed-index old-index)
            (fiber-set-flag new-fiber 'placement))))
    new-last-placed-index))

(declaim (ftype (function (fiber) fiber) place-single-child))
(defun place-single-child (new-fiber)
  (when (and *should-track-side-effects-p*
             (not (fiber-alternate new-fiber)))
    (fiber-set-flag new-fiber 'placement))
  new-fiber)

(declaim (ftype (function (fiber (or fiber null) string) fiber) update-text-node))
(defun update-text-node (parent current text-content)
  (let ((new-fiber (if (or (not current) (not (eq (fiber-tag current) 'host-text)))
                       (create-fiber-from-text text-content)
                       (use-fiber current text-content))))
    (setf (fiber-parent new-fiber) parent)
    new-fiber))

(declaim (ftype (function (fiber (or fiber null) element) fiber) update-element))
(defun update-element (parent new-child element)
  (declare (ignore parent))
  (declare (ignore new-child))
  (declare (ignore element))
  (error "not implemented"))

(declaim (ftype (function (fiber t) (or fiber null)) create-child))
(defun create-child (parent new-child)
  (declare (ignore parent))
  (declare (ignore new-child))
  (error "not implemented"))

(declaim (ftype (function (fiber (or fiber null) t) (or fiber null)) update-slot))
(defun update-slot (parent current-first-child new-child)
  (declare (ignore parent))
  (declare (ignore current-first-child))
  (declare (ignore new-child))
  (error "not implemented"))

(declaim (ftype (function (hash-table fiber (integer 0 *) t)) update-from-map))
(defun update-from-map (existing-children parent new-index new-child)
  (declare (ignore existing-children))
  (declare (ignore parent))
  (declare (ignore new-index))
  (declare (ignore new-child))
  (error "not implemented"))

(declaim (ftype (function (fiber (or fiber null) array)) reconcile-children-array))
(defun reconcile-children-array (parent current-first-child new-children)
  (declare (ignore parent))
  (declare (ignore current-first-child))
  (declare (ignore new-children))
  (error "not implemented"))

(declaim (ftype (function (fiber (or fiber null) sequence)) reconcile-children-sequence)) ;; reconcileChildrenIterator
(defun reconcile-children-sequence (parent current-first-child new-children)
  (declare (ignore parent))
  (declare (ignore current-first-child))
  (declare (ignore new-children))
  (error "not implemented"))

(declaim (ftype (function (fiber (or fiber null) string) fiber) reconcile-single-text-node))
(defun reconcile-single-text-node (parent current-first-child text-content)
  (declare (ignore parent))
  (declare (ignore current-first-child))
  (declare (ignore text-content))
  (error "not implemented"))

(declaim (ftype (function (fiber (or fiber null) element) fiber) reconcile-single-element))
(defun reconcile-single-element (parent current-first-child element)
  (declare (ignore parent))
  (declare (ignore current-first-child))
  (declare (ignore element))
  (error "not implemented"))

(declaim (ftype (function (fiber (or fiber null) t) (or fiber null)) reconcile-child-fibers-impl))
(defun reconcile-child-fibers-impl (parent current-first-child new-child)
  (cond
    ((elementp new-child)
     (place-single-child
      (reconcile-single-element parent current-first-child new-child)))
    ((arrayp new-child)
     (reconcile-children-array
      parent current-first-child new-child))
    ((and (typep new-child 'sequence)
          (not (stringp new-child)))
     (reconcile-children-sequence
      parent current-first-child new-child))
    ((or (and (stringp new-child) (> (length new-child) 0))
         (numberp new-child))
     (place-single-child
      (reconcile-single-text-node parent current-first-child (princ-to-string new-child))))))

(declaim (ftype (function (fiber t) (or fiber null)) mount-child-fibers))
(defun mount-child-fibers (parent new-child)
  (let ((*should-track-side-effects-p* nil))
    (reconcile-child-fibers-impl parent nil new-child)))

(declaim (ftype (function (fiber (or fiber null) t) (or fiber null)) reconcile-child-fibers))
(defun reconcile-child-fibers (parent current-first-child new-child)
  (let ((*should-track-side-effects-p* t))
    (reconcile-child-fibers-impl parent current-first-child new-child)))
