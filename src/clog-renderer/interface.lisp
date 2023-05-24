(in-package :cleact.clog-renderer)

(declaim (ftype (function (fiber-type association-list) boolean) should-set-text-content-p))
(defun should-set-text-content-p (type props)
  "Return `t' if an element should have its text content set. This should happen if
PROPS is a string or a number, or if TYPE is a text-only element type.'"
  (or (eq type 'textarea)
      (eq type 'noscript)
      (typep (access 'children props) '(or string number))))

(declaim (ftype (function (t fiber-type association-list association-list) association-list) prepare-update))
(defun prepare-update (instance type old-props new-props)
  (declare (ignore instance))
  (declare (ignore type))
  (let ((style-updates))
    (labels ((key-deleted-p (prop-key)
               ;; `t' if PROP-KEY has been removed.
               (and
                ;;  If key not in new-props. Use `assoc' so it returns truthy event
                ;;  when the value is `nil'.
                (not (assoc prop-key new-props))
                ;;  If key is not in old-props, or its value is `nil'.
                (access old-props prop-key)))
             (deleted-keys-reducer (update-payload old-kv)
               (let ((prop-key (car old-kv))
                     (old-value (cdr old-kv)))
                 ;; prop-key is a key from old-props
                 (cond
                   ((not (key-deleted-p prop-key))
                    ;; key wasn't deleted, no change
                    update-payload)
                   ((eq prop-key 'style)
                    ;; style is special because it is also an alist
                    (setf style-updates
                          (reduce (lambda (acc kv)
                                    (cons (cons (car kv) "") acc))
                                  old-value)))
                   (t
                    (cons (cons prop-key nil) update-payload))))))
      (reduce #'deleted-keys-reducer old-props
              :initial-value '()))))

(prepare-update nil nil '((key1 . val1) (key2 . val2)) '((key3 . val3)))
