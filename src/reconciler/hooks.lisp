(in-package :cleact.reconciler)

(deftype functional-component (&optional (props t))
  `(function (,props) t))

(declaim (ftype (function (nullable-fiber fiber functional-component t) t) render-with-hooks))
(defun render-with-hooks (current work-in-progress component props)
  (declare (ignore current))
  (declare (ignore work-in-progress))
  (declare (ignore component))
  (declare (ignore props))
  (error "not implemented"))

(declaim (ftype (function () t) did-render-id-hook-p))
(defun did-render-id-hook-p ()
  (error "not implemented"))

(declaim (ftype (function (fiber fiber) t) bailout-hooks))
(defun bailout-hooks (current work-in-progress)
  (declare (ignore current))
  (declare (ignore work-in-progress))
  (error "not implemented"))
