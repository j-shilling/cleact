(in-package :cleact.core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *saved-readtable* nil
    "Saves the previous readtable when the JSX enabled one us activated.")

  (define-condition jsx-error ()
    ((message :initarg :message :reader jsx-error-message))
    (:documentation
     "Signals an error during JSX parsing."))

  (define-condition not-jsx () ()
    (:documentation
     "Signals that something not JSX was read when JSX was expected."))

  (defun string-to-keyword (str)
    "Prefix STR with a `:' and read the result as a lisp object."
    (read-from-string (concatenate 'string ":" str)))

  (defun expand-jsx-string (str)
    (labels ((emptyp (obj)
               (and (typep obj 'sequence)
                    (= 0 (length obj))))
             (empty-string-p (obj)
               (and (stringp obj)
                    (emptyp obj)))
             (expand-string (str)
               (declare (type string str))
               (let ((result str))
                 (cl-ppcre:register-groups-bind (expr)
                     ("\\{([^\\}]+)\\}" str)
                   (declare (type string expr))
                   (setf result (read-from-string expr)))
                 result)))
      (let* ((parts (cl-ppcre:split "(\\{[^\\}]*\\})" str :with-registers-p t))
             (expanded (map 'list #'expand-string parts))
             (filtered (remove-if #'empty-string-p expanded)))
        (if (cdr filtered) ; if more than 1 element
            `(concatenate 'string ,@filtered)
            (car filtered)))))

  (defun plump-node-to-props (obj)
    (loop for key being the hash-keys of obj
            using (hash-value value)
          collect `(,(string-to-keyword key) . ,value)))

  (defun plump-node-to-lisp (obj)
    (log:debug "Converting DOM element to Lisp form" obj)
    (typecase obj
      (plump:text-node
       (plump:text obj))
      (plump:element
       (let ((type (string-to-keyword (plump:tag-name obj)))
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
    (unread-char char stream)
    (with-open-stream (out (make-string-output-stream))
      (with-open-stream (echo (make-echo-stream stream out))
        (handler-case
            (let ((root (plump:parse echo)))
              (unless (and (plump:root-p root)
                           (> (length (plump:children root)) 0)
                           (not (plump:text-node-p (aref (plump:children root) 0))))
                (error 'not-jsx))
              (parse-jsx-dom root))
          (not-jsx ()
            (let ((*readtable* (or *saved-readtable* *readtable*))
                  (backed-up-chars (get-output-stream-string out)))
              (log:debug "Some non-jsx was intercepted by the JSX reader"
                         backed-up-chars)
              (read (make-concatenated-stream
                     (make-string-input-stream backed-up-chars)
                     stream))))))))

  (let ((cache))
    (defun make-jsx-readtable ()
      (unless cache
        (setf cache (copy-readtable))
        (set-macro-character #\< 'read-jsx))
      cache))

  (defmacro enable-jsx ()
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (unless *saved-readtable*
         (setf *saved-readtable* *readtable*)
         (setf *readtable* (make-jsx-readtable))
         (log:debug "JSX reading enabled"))))

  (defmacro disable-jsx ()
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (when *saved-readtable*
         (setf *readtable* *saved-readtable*)
         (setf *saved-readtable* nil)
         (log:debug "JSX reading disabled")))))
