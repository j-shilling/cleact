(asdf:defsystem "cleact-reconciler"
  :version "0.1.0"
  :author "Jake Shilling"
  :license "Public Domain"
  :depends-on (:cleact-core
               :alexandria
               :log4cl
               :access
               :cl-containers)
  :components ((:module "src/reconciler/"
                :components
                ((:file "package")
                 (:file "types")
                 (:file "fiber")
                 (:file "renderer")
                 (:file "child-reconciler")
                 (:file "hooks")
                 (:file "reconciler")
                 (:file "workloop"))))
  :description "")
