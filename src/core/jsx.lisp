(in-package :cleact.core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *readtable-stack* nil)

  (define-condition jsx-error ()
    ((message :initarg :message :reader jsx-error-message)))

  (defun plump-node-to-lisp (obj)
    (log:debug "Converting DOM element to Lisp form" obj)
    (typecase obj
      (plump:text-node
       (plump:text obj))
      (plump:element
       (let ((type (read-from-string (concatenate 'string ":" (plump:tag-name obj))))
             (props (alexandria:hash-table-alist (plump:attributes obj)))
             (children (map 'list #'plump-node-to-lisp (plump:children obj))))
         `(create-element ,type ',props
           ,(cond
              ((= 0 (length children))
               nil)
              ((= 1 (length children))
               (car children))
              (t children)))))
      (t
       (log:warn "Not a valid plump type" obj)
       obj)))

  (defun parse-jsx-dom (dom)
    (declare (type plump:root dom))
    (let ((children (plump:children dom)))
      (if (= (length children) 1)
          (progn
            (let ((result (plump-node-to-lisp (aref children 0))))
              (log:debug "Created lisp from JSX" result)
              result))
          (signal 'jsx-error
                  :message
                  (format nil "Wrong number of children for a JSX document: ~A"
                          (length children))))))

  (defun read-jsx (stream char)
    (log:debug "Starting to read JSX")
    (unread-char char stream)
    (parse-jsx-dom (plump:parse stream)))

  (defmacro enable-jsx ()
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (push *readtable* *readtable-stack*)
       (setf *readtable* (copy-readtable))
       (set-macro-character #\< 'read-jsx)))

  (defmacro disable-jsx ()
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((rt (pop *readtable-stack*)))
         (when rt
           (setf *readtable* rt))))))
