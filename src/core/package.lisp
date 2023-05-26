(defpackage :cleact.core
  (:use :cl)
  (:import-from #:access
                #:access)
  (:import-from #:trivial-types
                #:association-list)
  (:export element
           elementp
           element-type
           element-key
           element-props
           element-ref
           create-element
           enable-jsx
           disable-jsx))
