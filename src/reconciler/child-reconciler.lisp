(in-package :cleact.reconsiler)

(defvar *should-track-side-effects-p* nil)

(declaim (ftype (function (fiber nullable-fiber element) t) coerce-ref))
(defun coerce-ref (parent current element)
  (declare (ignore parent))
  (declare (ignore current))
  (element-ref element))

(declaim (ftype (function (fiber fiber) t) delete-child))
(defun delete-child (parent child-to-delete)
  (when *should-track-side-effects-p*
    (let ((deletions (fiber-deletions parent)))
      (fiber-set-flag parent 'child-deletion)
      (setf (fiber-deletions parent)
            (cons child-to-delete deletions)))))

(declaim (ftype (function (fiber nullable-fiber) t) delete-remaining-children))
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

(declaim (ftype (function (fiber index index) index) place-child))
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

(declaim (ftype (function (fiber nullable-fiber string) fiber) update-text-node))
(defun update-text-node (parent current text-content)
  (let ((new-fiber (if (or (not current) (not (eq (fiber-tag current) 'host-text)))
                       (create-fiber-from-text text-content)
                       (use-fiber current text-content))))
    (setf (fiber-parent new-fiber) parent)
    new-fiber))

(declaim (ftype (function (fiber nullable-fiber element) fiber) update-element))
(defun update-element (parent current element)
  (let* ((type (element-type element))
         (result (if (and current (eq type (element-type current)))
                     (use-fiber current (element-props element))
                     (create-fiber-from-element element))))
    (setf (fiber-ref result) (coerce-ref parent current element))
    (setf (fiber-parent result) parent)
    result))

(declaim (ftype (function (fiber text-content) nullable-fiber) create-child))
(defun create-child (parent new-child)
  (let ((result))
    (cond
      ((text-content-p new-child)
       (setf result (create-fiber-from-text (princ-to-string new-child))))
      ((elementp new-child)
       (setf result (create-fiber-from-element new-child))))
    (when result
      (setf (fiber-parent result) parent))
    result))

(declaim (ftype (function (fiber nullable-fiber t) nullable-fiber) update-slot))
(defun update-slot (parent old-fiber new-child)
  (let ((result)
        (key (if old-fiber (fiber-key old-fiber) nil)))
    (cond
      ((and (not key) (text-content-p new-child))
       (setf result (update-text-node parent old-fiber (coerce-text-content new-child))))
      ((and (elementp new-child) (eq key (element-key new-child)))
       (setf result (update-element parent old-fiber new-child))))
    result))

(declaim (ftype (function (hash-table fiber index t) nullable-fiber) update-from-map))
(defun update-from-map (existing-children parent new-index new-child)
  (let ((result))
    (catch 'no-update
      (destructuring-bind (key content update-fn)
          (cond
            ((text-content-p new-child)
             `(,new-index ,(princ-to-string new-child) ,#'update-text-node))
            ((elementp new-child)
             `(,(or (element-key new-child) new-index) ,new-child ,#'update-element))
            (t (throw 'no-update nil)))
        (setf result
              (funcall update-fn parent (gethash key existing-children) content))))
    result))

(declaim (ftype (function (fiber nullable-fiber (or simple-vector list)) nullable-fiber) reconcile-children-sequence)) ;; reconcileChildrenIterator
(defun reconcile-children-sequence (parent current-first-child new-children)
  (let ((resulting-new-first-child)
        (previous-new-fiber)
        (new-fiber)
        (old-fiber current-first-child)
        (next-old-fiber)
        (last-placed-index 0)
        (new-index 0)
        (iter (typecase new-children
                (simple-vector -1)
                (list (cons nil new-children)))))
    (declare (type nullable-fiber resulting-new-first-child))
    (declare (type nullable-fiber previous-new-fiber))
    (declare (type nullable-fiber old-fiber))
    (declare (type nullable-fiber new-fiber))
    (declare (type nullable-fiber next-old-fiber))
    (declare (type index last-placed-index))
    (declare (type index new-index))
    ;; Deal with the overlap between the current children list and new-children
    (labels ((more-new-children-p ()
               (typecase new-children
                 (simple-vector (> (length new-children) (+ 1 iter)))
                 (cons (cdr iter))))
             (next-new-child ()
               (typecase new-children
                 (simple-vector
                  (progn
                    (incf iter)
                    (svref new-children iter)))
                 (list
                  (progn
                    (setf iter (cdr iter))
                    (car iter)))))
             (place-new-fiber (new-fiber)
               (declare (type fiber new-fiber))
               (setf last-placed-index (place-child new-fiber last-placed-index new-index))
               (if previous-new-fiber
                   (setf resulting-new-first-child new-fiber)
                   (setf (fiber-sibling previous-new-fiber) new-fiber))
               (incf new-index)
               (setf previous-new-fiber new-fiber))
             (process-overlapping-child (current-new-child)
               (declare (type fiber current-new-child))
               (when (not old-fiber)
                 (throw 'exit-loop t))
               (if (> (fiber-index old-fiber) new-index)
                   (progn
                     ;; old-fiber comes after new-child, so put off old-fiber til
                     ;; the next iteration.
                     (setf next-old-fiber old-fiber)
                     (setf old-fiber nil))
                   (setf new-old-fiber (fiber-sibling old-fiber)))
               (setf new-fiber (update-slot parent old-fiber current-new-child))
               (unless new-fiber
                 (unless old-fiber
                   ;; Restore old-fiber if it was just put off
                   (setf old-fiber next-old-fiber))
                 (throw 'exit-loop t))
               (when (and *should-track-side-effects-p*
                          old-fiber
                          (not (fiber-alternate new-fiber)))
                 ;; Matched slot, but not reusing old fiber
                 (delete-child parent old-fiber))
               (place-new-fiber new-fiber)
               (setf old-fiber next-old-fiber)))
      ;; Iterate over overlapping children
      (catch 'exit-loop
        (loop while (more-new-children-p)
              do (process-overlapping-child (next-new-child))))
      (cond
        ((not (more-new-children-p))
         ;; no more new-children, any remaining old ones should be deleted
         (delete-remaining-children parent old-fiber))
        ((not old-fiber)
         ;; no more old children, insert any remaining new ones
         (loop while (more-new-children-p)
               do (let* ((new-child (next-new-child))
                         (new-fiber (create-child parent new-child)))
                    (when new-fiber
                      (place-new-fiber new-fiber)))))
        (t
         ;; still have both old and new children
         (let ((existing-children (map-remaining-children parent old-fiber)))
           (loop while (more-new-children-p)
                 do (let* ((new-child (next-new-child))
                           (new-fiber (update-from-map existing-children parent new-index new-child)))
                      (when new-fiber
                        (when (and *should-track-side-effects-p*
                                   (fiber-alternate new-fiber))
                          ;; reusing old fiber instance, remove it from the map
                          (remhash (or (fiber-key new-fiber) new-index) existing-children))
                        (place-new-fiber new-fiber))))
           (when *should-track-side-effects-p*
             (loop for v being the hash-value of existing-children
                   do (delete-child parent v)))))))
    resulting-new-first-child))

(declaim (ftype (function (fiber nullable-fiber string) fiber) reconcile-single-text-node))
(defun reconcile-single-text-node (parent current-first-child text-content)
  (flet ((host-text-p (fiber)
           (declare (type fiber fiber))
           (and fiber (eq 'host-text (fiber-tag fiber)))))
    (destructuring-bind (return-fiber first-child-to-delete)
        (if (host-text-p current-first-child)
            `(,(use-fiber current-first-child text-content)
              ,(fiber-sibling current-first-child))
            `(,(create-fiber-from-text text-content)
              ,current-first-child))
      (delete-remaining-children parent first-child-to-delete)
      (setf (fiber-parent return-fiber) parent)
      return-fiber)))

(declaim (ftype (function (fiber nullable-fiber element) fiber) reconcile-single-element))
(defun reconcile-single-element (parent current-first-child element)
  (let ((key (element-key element))
        (type (element-type element))
        (child current-first-child)
        (result nil))
    (declare (type nullable-fiber result))
    (loop while (and child (not result))
          do (if (and (eq key (fiber-key child))
                      (eq type (fiber-type child)))
                 (progn
                   (delete-remaining-children parent (fiber-sibling child))
                   (setf result (use-fiber child (element-props element))))
                 (progn
                   (delete-child parent child)
                   (setf child (fiber-sibling child)))))
    (unless result
      (setf result (create-fiber-from-element element)))
    (setf (fiber-ref result) (coerce-ref parent child element))
    (setf (fiber-parent result) parent)
    result))

(declaim (ftype (function (fiber nullable-fiber t) nullable-fiber) reconcile-child-fibers-impl))
(defun reconcile-child-fibers-impl (parent current-first-child new-child)
  (cond
    ((elementp new-child)
     (place-single-child
      (reconcile-single-element parent current-first-child new-child)))
    ((and (typep new-child 'sequence)
          (not (stringp new-child)))
     (reconcile-children-sequence
      parent current-first-child new-child))
    ((or (and (stringp new-child) (> (length new-child) 0))
         (numberp new-child))
     (place-single-child
      (reconcile-single-text-node parent current-first-child (princ-to-string new-child))))))

(declaim (ftype (function (fiber t) nullable-fiber) mount-child-fibers))
(defun mount-child-fibers (parent new-child)
  (let ((*should-track-side-effects-p* nil))
    (reconcile-child-fibers-impl parent nil new-child)))

(declaim (ftype (function (fiber nullable-fiber t) nullable-fiber) reconcile-child-fibers))
(defun reconcile-child-fibers (parent current-first-child new-child)
  (let ((*should-track-side-effects-p* t))
    (reconcile-child-fibers-impl parent current-first-child new-child)))
