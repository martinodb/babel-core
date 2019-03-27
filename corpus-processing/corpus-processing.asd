(in-package :asdf)

(defsystem :corpus-processing
  :depends-on (:utils :cl-fad :cl-json :trivial-timeout)
  :components 
  (#+LISPWORKS7+(:file "corpus-processing")
   #+LISPWORKS7+(:file "json-stream-processing")))
	

