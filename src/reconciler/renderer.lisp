(in-package :cleact.reconsiler)

(defclass renderer ()
  ((should-set-text-content-p :reader renderer-should-set-text-content-p
                              :initarg :should-set-text-content-p
                              :initform (error "should-set-text-content-p is required")
                              :type (function (fiber-type t) t)
                              :documentation
                              "A function that takes a fiber type and it's props to check whether the inner
text of this node should be updated.")))

(declaim (ftype (function (renderer fiber-type t) t) should-set-text-content-p))
(defun should-set-text-content-p (renderer type props)
  (funcall (renderer-should-set-text-content-p renderer) type props))
