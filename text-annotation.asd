(cl:in-package #:asdf-user)

(defsystem text-annotation
  :depends-on (:mcclim-truetype :split-sequence)
  :serial t
  :components
  ((:file "packages")
   (:file "model")
   (:file "gui")))

  
