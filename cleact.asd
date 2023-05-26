(asdf:defsystem "cleact"
  :version "0.1.0"
  :author "Jake Shilling"
  :license "Public Domain"
  :depends-on (:cleact-core
               :cleact-reconciler
               :cleact-clog-renderer)
  :components ((:module "src/"
                :components
                ((:file "package"))))
  :description "")
