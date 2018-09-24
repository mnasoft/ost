;;;; ost.asd

(defsystem #:ost
  :description "Describe ost here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on (#:lst-arr)
  :serial t
  :components ((:file "package")
               (:file "ost")
	       (:file "gost_7805-70")))

