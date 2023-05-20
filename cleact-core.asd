(asdf:defsystem "cleact-core"
  :version "0.1.0"
  :author "Jake Shilling"
  :license "Public Domain"
  :depends-on (:access
               :trivial-types)
  :components ((:module "src/core"
                :components
                ((:file "package")
                 (:file "element"))))
  :description "")
