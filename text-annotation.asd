(cl:in-package #:asdf-user)

(defsystem text-annotation
  :depends-on (:mcclim-truetype :split-sequence :clueless)
  :serial t
  :components
  ((:file "packages")
   (:file "model")
   (:file "gui")))

  
