(in-package :asdf)

(defsystem :type-hierarchies
  :description "General support for using type-hierarchies in
  FCG."
  :depends-on (:fcg :graph-utils)
  :components 
  ((:file "package")
   (:file "graph-utils-additions")
   (:file "type-hierarchy")
   (:file "type-hierarchy-matcher")
   (:file "html")))


