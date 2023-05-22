(in-package :cleact.reconsiler)

(defclass renderer ()
  ((should-set-text-content-p :reader renderer-should-set-text-content-p
                              :initarg :should-set-text-content-p
                              :initform (error "should-set-text-content-p is required")
                              :type (function (fiber-type t) boolean)
                              :documentation
                              "A function that takes a fiber type and it's props to check whether the inner
text of this node should be updated.")))

(declaim (ftype (function (renderer fiber-type t) boolean) should-set-text-content-p))
(defun should-set-text-content-p (renderer type props)
  (let ((fn (renderer-should-set-text-content-p renderer)))
    (declare (type (function (fiber-type t) boolean) fn))
    (funcall fn type props)))
