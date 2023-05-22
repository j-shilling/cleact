(in-package :cleact.reconsiler)

(eval-when (:compile-toplevel)
  (defun non-empty-string-p (obj)
    (and (stringp obj)
         (< 0 (length obj))))

  (deftype non-empty-string ()
    '(satisfies non-empty-string-p))

  (deftype text-content ()
    '(or non-empty-string number))

  (defun text-content-p (obj)
    (typep obj 'text-content))

  (deftype text-or-element ()
    '(or element text-content))

  (deftype nullable (subtype)
    `(or null ,subtype))

  (defun coerce-text-content (obj)
    (typecase obj
      (non-empty-string obj)
      (number (write-to-string obj :base 10))
      (t nil)))

  (deftype index ()
    '(integer 0 *))

  (unless (boundp '+fiber-tags+)
    (defconstant +fiber-tags+
      '(functional-component
        host-component
        host-text)))

  (unless (boundp '+fiber-flags+)
    (defconstant +fiber-flags+
      '(content-reset
        ref
        ref-static
        child-deletion
        placement
        performed-work)))

  (defun fiber-tag-p (tag)
    (member tag +fiber-tags+))

  (defun fiber-flag-p (flag)
    (member flag +fiber-flags+))

  (deftype fiber-tag ()
    '(satisfies fiber-tag-p))

  (deftype fiber-flag ()
    '(satisfies fiber-flag-p))

  (deftype fiber-type ()
    'symbol)

  (deftype nullable-fiber ()
    '(nullable fiber)))
