(in-package :cleact.clog-renderer)

(declaim (ftype (function (fiber-type association-list) boolean) should-set-text-content-p))
(defun should-set-text-content-p (type props)
  (or (eq type 'textarea)
      (eq type 'noscript)
      (typep (access 'children props) '(or string number))))

(declaim (ftype (function (t fiber-type association-list association-list) association-list) prepare-update))
(defun prepare-update (instance type old-props new-props)
  (declare (ignore instance))
  (declare (ignore type))
  (declare (ignore old-props))
  (declare (ignore new-props)))
