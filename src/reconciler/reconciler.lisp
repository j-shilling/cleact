(in-package :cleact.reconciler)

(declaim (ftype (function (renderer t) t) create-container))
(defun create-container (renderer host-container)
  "Initialize the reconciler using RENDERER to interact with the host environment.
HOST-CONTAINER is an object past in that may be relevant to the renderer."
  (declare (ignore host-container))
  (setf *renderer* renderer)
  (error "not implemented"))

(declaim (ftype (function (element t) t) update-container))
(defun update-container (element container)
  "Process a request from the renderer to attach ELEMENT as a child to CONTAINER.
CONTAINER should have already been initialized via CREATE-CONTAINER."
  (declare (ignore element))
  (declare (ignore container))
  (error "not implemented"))
