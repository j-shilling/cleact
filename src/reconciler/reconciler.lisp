(in-package :cleact.reconciler)

(defstruct fiber-root-node
  container-info
  pending-children
  (current nil :type nullable-fiber))

(declaim (ftype (function (renderer t) fiber-root-node) create-container))
(defun create-container (renderer host-container)
  "Initialize the reconciler using RENDERER to interact with the host environment.
HOST-CONTAINER is an object past in that may be relevant to the renderer."
  (setf *renderer* renderer)
  (let ((root (make-fiber-root-node :container-info host-container))
        (uninitialized-fiber (create-fiber 'host-root nil nil)))
    (setf (fiber-state-node uninitialized-fiber) root)
    (setf (fiber-root-node-current root) uninitialized-fiber)
    root))

(declaim (ftype (function (element fiber-root-node) t) update-container))
(defun update-container (element container)
  "Process a request from the renderer to attach ELEMENT as a child to CONTAINER.
CONTAINER should have already been initialized via CREATE-CONTAINER."
  (let* ((current (fiber-root-node-current container ))
         (update (make-update :payload `((element . ,element))))
         (root (enqueue-update current update)))
    (when root
      (schedule-update-on-fiber root current))
    nil))
