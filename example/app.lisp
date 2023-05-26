(defpackage :cleact.example
  (:use #:cl #:clog #:clog-gui #:cleact)
  (:export start-app
           on-new-window))

(in-package :cleact.example)

(defun app ()
  (cleact.core:create-element :h1 nil "Hello, World"))

(defun on-new-window (body)
  (render (app) body))

(defun start-app ()
  (initialize
   'on-new-window
   :static-root
   (merge-pathnames
    "./www/"
    (asdf:system-source-directory
     :cleact-example)))
  (open-browser))
