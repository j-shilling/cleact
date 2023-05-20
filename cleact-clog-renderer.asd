(asdf:defsystem "cleact-clog-renderer"
  :version "0.1.0"
  :author "Jake Shilling"
  :license "Public Domain"
  :depends-on (:cleact-core
               :clog)
  :components ((:module "src/clog-renderer"
                :components
                ((:file "package"))))
  :description "")
