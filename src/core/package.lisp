(defpackage :cleact.core
  (:use :cl)
  (:import-from #:access
                #:access)
  (:import-from #:trivial-types
                #:association-list)
  (:export element
           elementp
           element-type))
