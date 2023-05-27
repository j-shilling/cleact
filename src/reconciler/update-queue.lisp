(in-package :cleact.reconciler)

(deftype callback-fn ()
  '(function () t))

(unless (boundp '+update-tags+)
  (defconstant +update-tags+
    '(update-state
      replace-state
      force-update
      capture-update)))

(defun update-tag-p (obj)
  (member obj +update-tags+))

(deftype update-tag ()
  '(satisfies update-tag-p))

(defstruct update
  payload
  (tag 'update-state :type update-tag)
  (callback nil :type (nullable callback-fn))
  (next nil :type (nullable update)))

(defstruct shared-queue
  (pending nil :type (nullable update))
  (hidden-callbacks nil :type (nullable (array callback-fn 1))))

(defstruct update-queue
  base-state
  (first-base-update nil :type (nullable update))
  (last-base-update nil :type (nullable update))
  (shared (make-shared-queue) :type shared-queue)
  (callbacks nil :type (nullable (array callback-fn 1))))

(defvar *concurrent-queues*
  (make-instance 'metabang.cl-containers:basic-queue))

(declaim (ftype (function (fiber) t) initialize-update-queue))
(defun initialize-update-queue (fiber)
  (setf (fiber-update-queue fiber)
        (make-update-queue :base-state
                           (fiber-memoized-state fiber))))

(declaim (ftype (function (fiber update) (nullable fiber-root-node)) enqueue-update))
(defun enqueue-update (fiber update)
  (let ((update-queue (fiber-update-queue fiber)))
    (when update-queue
      (let ((shared-queue (update-queue-shared update-queue))
            (node fiber))
        (loop while (fiber-parent node)
              do (setf node (fiber-parent node)))
        (enqueue *concurrent-queues* fiber)
        (enqueue *concurrent-queues* shared-queue)
        (enqueue *concurrent-queues* update)
        (when (eq 'host-root (fiber-tag node))
          (fiber-state-node node))))))
