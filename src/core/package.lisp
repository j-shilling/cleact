(defpackage :cleact.core
  (:use :cl)
  (:import-from #:access
                #:access)
  (:import-from #:trivial-types
                #:association-list)
  (:export cleact-element
           cleact-element-type
           cleact-element-props
           cleact-element-key
           cleact-element-ref))
