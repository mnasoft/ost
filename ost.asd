;;;; ost.asd

(asdf:defsystem #:ost
  :description "Describe ost here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lst-arr)
  :serial t
  :components ((:file "package")
               (:file "ost")
	       (:file "gost_7805-70")))

