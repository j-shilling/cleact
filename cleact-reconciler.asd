(asdf:defsystem "cleact-reconciler"
  :version "0.1.0"
  :author "Jake Shilling"
  :license "Public Domain"
  :depends-on (:cleact-core)
  :components ((:module "src/reconciler/"
                :components
                ((:file "package")
                 (:file "fiber")
                 (:file "renderer")
                 (:file "workloop"))))
  :description "")
